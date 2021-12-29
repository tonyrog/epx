/***************************************************************************
 *
 * Copyright (C) 2007 - 2021, Rogvall Invest AB, <tony@rogvall.se>
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

#include "../include/epx_t2d.h"
#include <math.h>
#include <memory.h>

const epx_t2d_t epx_identity_ctm =
{
    .sx = 1.0f, .ry = 0.0f, .tx = 0.0f,
    .rx = 0.0f, .sy = 1.0f, .ty = 0.0f,
    .version = 0
};

void epx_t2d_transform_xy(epx_t2d_t* t, float* xp, float* yp)
{
    float x = *xp;
    float y = *yp;
    *xp = x*t->sx + y*t->ry + t->tx;
    *yp = x*t->rx + y*t->sy + t->ty;
}

void epx_t2d_transform_wh(epx_t2d_t* t, float* wp, float* hp)
{
    *wp = fabs((*wp)*t->sx);
    *hp = fabs((*hp)*t->sy);
}

void epx_t2d_transform_vec(epx_t2d_t* t, float* vec, size_t n)
{
    while(n--) {
	epx_t2d_transform_xy(t, vec, vec+1);
	vec += 2;
    }
}

void epx_t2d_identity(epx_t2d_t* dst)
{
    memcpy(dst, &epx_identity_ctm, sizeof(epx_t2d_t));
}

void epx_t2d_set(float elem[6], epx_t2d_t* dst)
{
    dst->version++;
    dst->sx = elem[T_SX];
    dst->ry = elem[T_RY];
    dst->tx = elem[T_TX];
    dst->ry = elem[T_RY];
    dst->sy = elem[T_SY];
    dst->ty = elem[T_TY];
}

void epx_t2d_get(epx_t2d_t* t, float elem[6])
{
    elem[T_SX] = t->sx;
    elem[T_RY] = t->ry;
    elem[T_TX] = t->tx;
    elem[T_RX] = t->rx;    
    elem[T_SY] = t->sy;
    elem[T_TY] = t->ty;
}

//
// Compose - Transform "matrix"
//  | sx  ry  tx |   | a b e |     | sx*a+ry*c  sx*b+ry*d sx*e+ry*f+tx |
//  | rx  sy  ty | * | c d f |  =  | rx*a+sy*c  rx*b+sy*d rx*e+sy*f+ty |
//  | 0   0   1  |   | 0 0 1 |     | 0          0         1            |
//
void epx_t2d_compose(epx_t2d_t* t, epx_t2d_t* s, epx_t2d_t* dst)
{
    float tsx = t->sx;
    float try = t->ry;
    float trx = t->rx;
    float tsy = t->sy;
    float sx = tsx*s->sx + try*s->rx;
    float ry = tsx*s->ry + try*s->sy;
    float rx = trx*s->sx + tsy*s->rx;
    float sy = trx*s->ry + tsy*s->sy;
    float tx = tsx*s->tx + try*s->ty + t->tx;
    float ty = trx*s->tx + tsy*s->ty + t->ty;
    dst->version++;
    dst->sx = sx;
    dst->ry = ry;
    dst->sy = sy;
    dst->rx = rx;
    dst->tx = tx;
    dst->ty = ty;
}

//
// Translate - Transform "matrix"
//  | sx  ry  tx |   | 1 0 x |     | sx ry sx*x+ry*y+tx |
//  | rx  sy  ty | * | 0 1 y |  =  | rx sy rx*x+sy*y+ty |
//  | 0   0   1  |   | 0 0 1 |     | 0  0  1            |
//
void epx_t2d_translate(epx_t2d_t* t, float tx, float ty, epx_t2d_t* dst)
{
    dst->version++;
    dst->tx = t->tx + t->sx*tx + t->ry*ty;
    dst->ty = t->ty + t->rx*tx + t->sy*ty;
    dst->sx = t->sx;
    dst->ry = t->ry;
    dst->sy = t->sy;
    dst->rx = t->rx;
}

//
// Scale - Transform "matrix"
//  | sx  ry  tx |   | x   0   0 |    | sx*x ry*y tx |
//  | rx  sy  ty | * | 0   y   0 | =  | rx*x sy*y ty |
//  | 0   0   1  |   | 0   0   1 |    | 0    0    1  |
//
void epx_t2d_scale(epx_t2d_t* t, float sx, float sy, epx_t2d_t* dst)
{
    dst->version++;    
    dst->sx = t->sx*sx;
    dst->ry = t->ry*sy;
    dst->rx = t->rx*sx;    
    dst->sy = t->sy*sy;
    dst->tx = t->tx;
    dst->ty = t->ty;
}

//
// Rotate - Transform "matrix"
//  | sx  ry  tx |   | c  -s   0 |     | sx*c+ry*s  -sx*s+ry*c tx |
//  | rx  sy  ty | * | s   c   0 |  =  | rx*c+sy*s  -rx*s+sy*c ty |
//  | 0   0   1  |   | 0   0   1 |     | 0          0          1  |
//

void epx_t2d_rotate(epx_t2d_t* t, float a, epx_t2d_t* dst)
{
    float c = cosf(a);
    float s = sinf(a);
    float ax, ay;
    
    ax =  t->sx*c + t->ry*s;
    ay = -t->sx*s + t->ry*c;

    dst->version++;
    dst->sx = ax;
    dst->ry = ay;
    dst->tx = t->tx;
    
    ax = t->rx*c + t->sy*s;
    ay = -t->rx*s + t->sy*c;

    dst->rx = ax;
    dst->sy = ay;
    dst->tx = t->tx;
}
