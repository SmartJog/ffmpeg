/*
 * Copyright (c) 2012 Clément Bœsch <clement.boesch@smartjog.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file Adobe HTTP Dynamic Streaming for Flash fragmenter
 */

//#include <stdio.h>
#include <ctype.h>
#include <expat.h>

#include "avformat.h"
#include "internal.h"
#include "libavutil/base64.h"
#include "libavutil/eval.h"

/* the following ifdef-ing is directly imported from the libexpat examples */
#if defined(__amigaos__) && defined(__USE_INLINE__)
#include <proto/expat.h>
#endif

#ifdef XML_LARGE_SIZE
#if defined(XML_USE_MSC_EXTENSIONS) && _MSC_VER < 1400
#define XML_FMT_INT_MOD "I64"
#else
#define XML_FMT_INT_MOD "ll"
#endif
#else
#define XML_FMT_INT_MOD "l"
#endif

struct content_block {
    char *str;      ///< zero terminated string of read data in the current chunk
    int len;        ///< length of str, not accounting the '\0' end character
    uint8_t *dec;   ///< the decoded version of str (base64 or just stripped string), zero terminated
    int dec_len;    ///< length of dec, not accounting the '\0' end character
};

struct media {
    int bootstrap_id; // XXX: fill me
    //char url[MAX_URL_SIZE];
    AVFormatContext *ctx;
    AVPacket pkt;
    //AVIOContext pb;

    /* from xml */
    char *stream_id;
    char *url;
    int bitrate;
    char *bootstrap_info_id;
    struct content_block metadata;
};

struct bootstrap {
    int media_id; // XXX: fill me

    /* from xml */
    char *id;
    char *profile;
    struct content_block data;
};

enum {
    DATA_TYPE_NONE,
    DATA_TYPE_ID,
    DATA_TYPE_STREAM_TYPE,
    DATA_TYPE_DURATION,
    DATA_TYPE_MEDIA_METADATA,
    DATA_TYPE_BOOTSTRAP_INFO,
};

typedef struct HDSContext {
    AVIOInterruptCB *interrupt_callback;

    /* xml info */
    struct content_block id;            ///< "id" root field
    struct content_block stream_type;   ///< "streamType" root field
    struct content_block duration;      ///< "duration" float root field

    struct media **medias;              ///< array of media
    int nb_medias;                      ///< number of element in the medias array

    struct bootstrap **bs_info;         ///< array of bootstrap info
    int nb_bs_info;                     ///< number of element in the bootstrap info array

    /* xml parsing */
    int parse_ret;                      ///< an error occurred while parsing XML
    int data_type;                      ///< next data chunk will be of this style
} HDSContext;

static int hds_probe(AVProbeData *p)
{
    char *ptr = p->buf;

    while (isspace(*ptr))
        ptr++;
    if (!strncmp(ptr, "<?xml", 5) &&
        strstr(ptr, "<manifest") && strstr(ptr, "ns.adobe.com/f4m"))
        return AVPROBE_SCORE_MAX;
    return 0;
}

static void XMLCALL xml_start(void *data, const char *el, const char **attr)
{
    int i;
    HDSContext *hds = data;

    hds->data_type = DATA_TYPE_NONE;
    if      (!strcmp(el, "id"))         hds->data_type = DATA_TYPE_ID;
    else if (!strcmp(el, "streamType")) hds->data_type = DATA_TYPE_STREAM_TYPE;
    else if (!strcmp(el, "duration"))   hds->data_type = DATA_TYPE_DURATION;
    else if (!strcmp(el, "media")) {
        struct media *m = av_mallocz(sizeof(*m));
        if (!m) {
            hds->parse_ret = AVERROR(ENOMEM);
            return;
        }
        dynarray_add(&hds->medias, &hds->nb_medias, m);

        for (i = 0; attr[i]; i += 2) {
            if      (!strcmp(attr[i], "streamId"))        m->stream_id         = av_strdup(attr[i + 1]);
            else if (!strcmp(attr[i], "url"))             m->url               = av_strdup(attr[i + 1]);
            else if (!strcmp(attr[i], "bitrate"))         m->bitrate           = strtol(attr[i + 1], NULL, 10);
            else if (!strcmp(attr[i], "bootstrapInfoId")) m->bootstrap_info_id = av_strdup(attr[i + 1]);
        }
    } else if (!strcmp(el, "metadata")) {
        hds->data_type = DATA_TYPE_MEDIA_METADATA;
    } else if (!strcmp(el, "bootstrapInfo")) {
        struct bootstrap *b = av_mallocz(sizeof(*b));
        if (!b) {
            hds->parse_ret = AVERROR(ENOMEM);
            return;
        }
        dynarray_add(&hds->bs_info, &hds->nb_bs_info, b);

        for (i = 0; attr[i]; i += 2) {
            if      (!strcmp(attr[i], "profile")) b->profile = av_strdup(attr[i + 1]);
            else if (!strcmp(attr[i], "id"))      b->id      = av_strdup(attr[i + 1]);
        }
        hds->data_type = DATA_TYPE_BOOTSTRAP_INFO;
    }
}

static struct content_block *get_content_block(HDSContext *hds, int data_type)
{
    switch (hds->data_type) {
    case DATA_TYPE_ID:              return &hds->id;
    case DATA_TYPE_STREAM_TYPE:     return &hds->stream_type;
    case DATA_TYPE_DURATION:        return &hds->duration;
    case DATA_TYPE_MEDIA_METADATA:  return hds->nb_medias  ? &hds->medias [hds->nb_medias  - 1]->metadata : NULL;
    case DATA_TYPE_BOOTSTRAP_INFO:  return hds->nb_bs_info ? &hds->bs_info[hds->nb_bs_info - 1]->data     : NULL;
    }
    return NULL;
}

static void XMLCALL xml_data(void *data, const char *s, int len)
{
    HDSContext *hds = data;
    struct content_block *block = get_content_block(hds, hds->data_type);

    if (!block || !len || block->len >= INT_MAX - len - 1)
        return;
    block->str = av_realloc(block->str, block->len + len + 1);
    if (!block->str) {
        hds->parse_ret = AVERROR(ENOMEM);
        return;
    }
    memcpy(block->str + block->len, s, len);
    block->len += len;
    block->str[block->len] = 0;
}

static void XMLCALL xml_end(void *data, const char *el)
{
    HDSContext *hds = data;
    struct content_block *block = get_content_block(hds, hds->data_type);

    if (block->len && !block->dec) {
        /* [rl]strip the string */
        int i;
        char *s = block->str;
        for (i = block->len - 1; i >= 0 && isspace(s[i]); i--)
            s[i] = 0;
        while (isspace(*s))
            s++;

        if (hds->data_type == DATA_TYPE_MEDIA_METADATA ||
            hds->data_type == DATA_TYPE_BOOTSTRAP_INFO) {
            /* decode base64 fields */
            block->dec = av_malloc(block->len);
            if (!block->dec) {
                hds->parse_ret = AVERROR(ENOMEM);
                return;
            }
            block->dec_len = av_base64_decode(block->dec, s, block->len);
        } else if (hds->data_type == DATA_TYPE_ID ||
                   hds->data_type == DATA_TYPE_STREAM_TYPE ||
                   hds->data_type == DATA_TYPE_DURATION) {
            /* duplicated cleaned string */
            block->dec = av_strdup(s);
        }
    }
    av_freep(&block->str);
    block->len = 0;
}

static int parse_manifest(AVFormatContext *s)
{
    int done = 0;
    char buf[4096];
    XML_Parser xmlp;
    HDSContext *hds = s->priv_data;

    xmlp = XML_ParserCreate(NULL);
    if (!xmlp) {
        av_log(s, AV_LOG_ERROR, "Unable to allocate memory for the libexpat XML parser\n");
        return AVERROR(ENOMEM);
    }
    XML_SetUserData(xmlp, hds);
    XML_SetElementHandler(xmlp, xml_start, xml_end);
    XML_SetCharacterDataHandler(xmlp, xml_data);

    while (!done && !hds->parse_ret) {
        int len;

        len = avio_read(s->pb, buf, sizeof(buf));
        if (len < 0) {
            done = len == AVERROR_EOF;
            len = 0;
        }

        if (XML_Parse(xmlp, buf, len, done) == XML_STATUS_ERROR) {
            av_log(s, AV_LOG_ERROR, "Parse error at line %" XML_FMT_INT_MOD "u:\n%s\n",
                   XML_GetCurrentLineNumber(xmlp),
                   XML_ErrorString(XML_GetErrorCode(xmlp)));
            return AVERROR_INVALIDDATA;
        }
    }

    // TODO: make links between bootstraps and medias

    XML_ParserFree(xmlp);
    return hds->parse_ret;
}

static int hds_read_header(AVFormatContext *s)
{
    int ret;
    HDSContext *hds = s->priv_data;

    hds->interrupt_callback = &s->interrupt_callback;

    if ((ret = parse_manifest(s)) < 0)
        return ret;

    return 0;
}

static void destroy_block(struct content_block *block)
{
    av_freep(&block->str);
    av_freep(&block->dec);
    block->len = block->dec_len = 0;
}

static int hds_read_close(AVFormatContext *s)
{
    int i;
    HDSContext *hds = s->priv_data;

#if 1
    av_log(0,0,"ID=[%s]\n", hds->id.dec);
    av_log(0,0,"StreamType=[%s]\n", hds->stream_type.dec);
    av_log(0,0,"Duration=[%s]\n", hds->duration.dec);

    for (i = 0; i < hds->nb_medias; i++) {
        const struct media *m = hds->medias[i];
        av_log(0,0," media #%d\n", i);
        av_log(0,0,"  stream_id=[%s] url=[%s] bitrate=%d bs=[%s]\n",
               m->stream_id, m->url, m->bitrate, m->bootstrap_info_id);
        av_log(0,0,"  metadata (len=%d):\n", m->metadata.dec_len);
        av_hex_dump_log(0,0, m->metadata.dec, m->metadata.dec_len);
    }

    for (i = 0; i < hds->nb_bs_info; i++) {
        const struct bootstrap *b = hds->bs_info[i];
        av_log(0,0," bs #%d\n", i);
        av_log(0,0,"  id=[%s] profile=[%s]\n", b->id, b->profile);
        av_log(0,0,"  data (len=%d):\n", b->data.dec_len);
        av_hex_dump_log(0,0, b->data.dec, b->data.dec_len);
    }
#endif

    destroy_block(&hds->id);
    destroy_block(&hds->stream_type);
    destroy_block(&hds->duration);

    for (i = 0; i < hds->nb_medias; i++) {
        av_freep(&hds->medias[i]->stream_id);
        av_freep(&hds->medias[i]->url);
        av_freep(&hds->medias[i]->bootstrap_info_id);
        destroy_block(&hds->medias[i]->metadata);
        av_freep(&hds->medias[i]);
    }
    av_freep(&hds->medias);

    for (i = 0; i < hds->nb_bs_info; i++) {
        av_freep(&hds->bs_info[i]->id);
        av_freep(&hds->bs_info[i]->profile);
        destroy_block(&hds->bs_info[i]->data);
        av_freep(&hds->bs_info[i]);
    }
    av_freep(&hds->bs_info);

    return 0;
}

static int hds_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    //HDSContext *hds = s->priv_data;
    pkt->size = 0;
    pkt->data = NULL;
    return AVERROR_EOF;
}

AVInputFormat ff_hds_demuxer = {
    .name           = "hds",
    .long_name      = NULL_IF_CONFIG_SMALL("Adobe HTTP Dynamic Streaming"),
    .priv_data_size = sizeof(HDSContext),
    .read_probe     = hds_probe,
    .read_header    = hds_read_header,
    .read_packet    = hds_read_packet,
    .read_close     = hds_read_close,
    //.read_seek      = hds_read_seek,
};
