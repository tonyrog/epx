/***************************************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
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
//
// EPX Sft managment
//
#ifndef __EPX_SFT_H__
#define __EPX_SFT_H__

#include "epx_object.h"
#include "epx_efnt.h"
#include "schrift.h"

/* The pixels are normally stored as a EPIXEL_FMT_ALPHA (8 bits)
 * then we will render with
 *     fully opaque a=255  as foreground_color
 *     
 * and optionally render a=0 as background
 */
typedef struct _epx_sft_t {
    EPX_OBJECT_MEMBERS(struct _epx_sft_t);
    // The schrift object
    SFT sft;
    SFT_LMetrics lmetrics;
    int font_family_name_len;    
    char* font_family_name;    // point into shared memory!
    int font_subfamily_name_len;
    char* font_subfamily_name; // point into shared memory!
} epx_sft_t;

extern void        epx_sft_init(epx_sft_t* font);
extern epx_sft_t* epx_sft_create(void);
extern void        epx_sft_destroy(epx_sft_t* font);

#endif
