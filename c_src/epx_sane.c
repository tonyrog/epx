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
//
//
#include <stdlib.h>
#include <stddef.h>
#include "epx.h"


// size and offset print out

void print_sizes(void)
{
    printf("sizeof(epx_pixel_t) = %zd\n", sizeof(epx_pixel_t));
    printf("sizeof(epx_pixmap_t) = %zd\n", sizeof(epx_pixmap_t));
    // offsets
    printf("offsetof(epx_pixel_t.a) = %zd\n", offsetof(epx_pixel_t, a));
    printf("offsetof(epx_pixel_t.r) = %zd\n", offsetof(epx_pixel_t, r));
    printf("offsetof(epx_pixel_t.g) = %zd\n", offsetof(epx_pixel_t, g));
    printf("offsetof(epx_pixel_t.b) = %zd\n", offsetof(epx_pixel_t, b));
}


main()
{
    print_sizes();
    exit(0);
}

