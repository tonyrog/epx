/***************************************************************************
 *
 * Copyright (C) 2007 - 2023, <tony@rogvall.se>
 *
 * This software is licensed as described in the file COPYRIGHT, which
 * you should have received as part of this distribution. The terms
 * are also available at http://www.rogvall.se/docs/copyright.txt.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYRIGHT file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ***************************************************************************/
/*
 *  EPX SFT functions
 */
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wchar.h>
#include <errno.h>

#include "../include/epx_pixmap.h"
#include "../include/epx_gc.h"
#include "../include/epx_sft.h"

void epx_sft_init(epx_sft_t* sft)
{
    EPX_OBJECT_INIT(sft, EPX_SFT_TYPE);

    memset(&sft->sft, 0, sizeof(SFT));
    memset(&sft->lmetrics, 0, sizeof(SFT_LMetrics));
    sft->font_family_name = NULL;
    sft->font_family_name_len = 0;
    sft->font_subfamily_name = NULL;
    sft->font_subfamily_name_len = 0;
}

epx_sft_t* epx_sft_create()
{
    epx_sft_t* sft;

    if ((sft = (epx_sft_t*) malloc(sizeof(epx_sft_t))) == 0)
	return 0;
    epx_sft_init(sft);
    sft->on_heap = 1;
    sft->refc = 1;
    return sft;
}

void EPX_SFT_TYPE_RELEASE(void* arg)
{
    epx_sft_t* esft = (epx_sft_t*) arg;
    
    EPX_DBGFMT_MEM("EPX_SFT_TYPE_RELEASE: %p", arg);

    sft_freefont(esft->sft.font);

    if (esft->on_heap)
	free(esft);
}

void epx_sft_destroy(epx_sft_t* esft)
{
    epx_object_unref(esft);
}
