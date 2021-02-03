//
// CTM
//

#ifndef __EPX_CTM_H__
#define __EPX_CTM_H__

#include <stdlib.h>

#define T_SX  0
#define T_RY  1
#define T_TX  2
#define T_RX  3
#define T_SY  4
#define T_TY  5

typedef struct _epx_t2d_t
{
    float sx; float ry; float tx;
    float rx; float sy; float ty;
    //     0;        0;        1;
} epx_t2d_t;

extern const epx_t2d_t epx_identity_ctm;

extern void epx_t2d_transform_xy(epx_t2d_t* t, float* xp, float* yp);
extern void epx_t2d_transform_wh(epx_t2d_t* t, float* wp, float* hp);
extern void epx_t2d_transform_vec(epx_t2d_t* t, float* vec, size_t n);
extern void epx_t2d_identity(epx_t2d_t* t);
extern void epx_t2d_set(float elem[6], epx_t2d_t* dst);
extern void epx_t2d_get(epx_t2d_t* t, float elem[6]);
extern void epx_t2d_compose(epx_t2d_t* t, epx_t2d_t* s, epx_t2d_t* dst);
extern void epx_t2d_translate(epx_t2d_t* t, float tx, float ty, epx_t2d_t* dst);
extern void epx_t2d_scale(epx_t2d_t* t, float sx, float sy, epx_t2d_t* dst);
extern void epx_t2d_rotate(epx_t2d_t* t, float a, epx_t2d_t* dst);

#endif



    
