/*
 * EPIC Animation frame drawing
 *
 */

#include "epic.h"

/* Copy animation frame to background */
void EAnimCopyFrame(EGc* gc, EPixmap* pic, int x, int y,
		    int width, int height, int src_pt,
		    EAnimPixels* base, EAnimPixels* current)
{
    (void) gc;
    u_int8_t *src = (u_int8_t*) current;
    u_int8_t* src_save = NULL;
    int n_blocks = 0;
    unsigned char *dst0 = EPIXEL_ADDR(pic, x, y);
    int dst_wb = pic->bytesPerRow;
    unsigned int srcPixelType = src_pt;
    int srcPixelSize = EPIXEL_SIZE(src_pt);
    unsigned int dstPixelType = pic->pixelType;
    int dstPixelSize = pic->bytesPerPixel;

    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    EAnimPixels* hdr = (EAnimPixels*) src;
	    u_int32_t pcount;

	    if (hdr->type == EANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(EAnimPixels); // continue point
		src = isrc;
		hdr = (EAnimPixels*) src; // switch to indirect header
	    }

	    src += sizeof(EAnimPixels);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EANIM_SKIP:
		break;

	    case EANIM_SHADOW:
		EDirectCopyRow(src, EPIXEL_TYPE_A8,
			       dst, dstPixelType, pcount);
		src  += pcount;
		break;

	    case EANIM_RGBA:
		EDirectCopyRow(src,EPIXEL_TYPE_RGBA,
			       dst, dstPixelType, pcount);
		src += pcount*4;
		break;

	    case EANIM_BGRA:
		EDirectCopyRow(src,EPIXEL_TYPE_BGRA,
			       dst, dstPixelType, pcount);
		src += pcount*4;
		break;
		
	    case EANIM_COPY: // BGRA values!!!
		EDirectCopyRow(src, srcPixelType,
			       dst, dstPixelType,
			       pcount);
		src += pcount*srcPixelSize;
		break;

	    case EANIM_FILL:
		EDirectFillRow(dst, dstPixelType, pcount,
			       epixel_argb(src[0],src[1],src[2],src[3]));
		src += 4;
		break;
	    default: 
		break;
	    }
	    dst += pcount*dstPixelSize;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
}

/* Draw & Blend animation frame with background */
void EAnimDrawFrame(EGc* gc, EPixmap* pic, int x, int y,
		    int width, int height, int src_pt,
		    EAnimPixels* base, EAnimPixels* current)
{
    u_int8_t fader = gc->fader_value;
    u_int8_t *src = (u_int8_t*) current;
    u_int8_t* src_save = NULL;
    int n_blocks = 0;
    unsigned char *dst0 = EPIXEL_ADDR(pic, x, y);
    int dst_wb = pic->bytesPerRow;
    unsigned int srcPixelType = src_pt;
    int srcPixelSize = EPIXEL_SIZE(src_pt);
    unsigned int dstPixelType = pic->pixelType;
    int dstPixelSize = pic->bytesPerPixel;
    ERect_t r = {{x,y}, {width,height}};
    int x0, yi;
    int clip_y0, clip_y1;
    int clip_x0, clip_x1, clip_len;
    int need_x_clip;
    
    if (ERectIsSubRect(&r, &pic->clip)) {
	if (srcPixelType == dstPixelType) {
	    if (dstPixelType == EPIXEL_TYPE_BGRA)
		goto bgra_noclip;
	    else if (dstPixelType == EPIXEL_TYPE_RGBA)
		goto rgba_noclip;
	    goto no_clip;
	}
    }
    goto clip;

    /* if we must clip the image we do the general stuff */

clip:
    clip_y0  = ERectTop(&pic->clip);
    clip_y1  = ERectBottom(&pic->clip);
    clip_x0  = ERectLeft(&pic->clip);
    clip_x1  = ERectRight(&pic->clip);
    clip_len = ERectWidth(&pic->clip);

    yi = y;
    x0 = x;
    need_x_clip = !( (x0 >= clip_x0) && ((x0+width-1) <= clip_x1) );

    while(height--) {
	int xi    = x0;
	int w = width;

	if (!EInRange(yi, clip_y0, clip_y1)) {
	    // The line is clipped but we must skip the source data
	    while(w) {
		EAnimPixels *hdr = (EAnimPixels *) src;
		u_int32_t pcount;

		if (hdr->type == EANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(EAnimPixels); // continue point
		    src = isrc;
		    hdr = (EAnimPixels*) src; // switch to indirect header
		}
		src += sizeof(EAnimPixels);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EANIM_SKIP:
		    break;
		case EANIM_SHADOW:
		    src  += pcount;
		    break;
		case EANIM_BGRA:
		case EANIM_RGBA:
		    src += pcount*4;
		    break;
		case EANIM_COPY:
		    src += pcount*srcPixelSize;
		    break;
		case EANIM_FILL:
		    src += 4;
		    break;
		default:
		    break;
		}
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	else if (!need_x_clip) {  // No horizontal clipping needed
	    unsigned char *dst = EPIXEL_ADDR(pic, xi, yi);
	    while(w) {
		EAnimPixels *hdr = (EAnimPixels *) src;
		u_int32_t pcount;
		u_int8_t a;

		if (hdr->type == EANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(EAnimPixels); // continue point
		    src = isrc;
		    hdr = (EAnimPixels*) src; // switch to indirect header
		}

		src += sizeof(EAnimPixels);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EANIM_SKIP:
		    break;

		case EANIM_SHADOW:
		    EDirectAddColorRow(src, EPIXEL_TYPE_A8,
				       dst, dstPixelType,
				       fader, epixel_transparent(),
				       pcount, EFLAG_BLEND);
		    src  += pcount;
		    break;

		case EANIM_BGRA:
		    EDirectFadeRow(src,EPIXEL_TYPE_BGRA,
				   dst, dstPixelType, fader, pcount);
		    src += pcount*4;
		    break;

		case EANIM_RGBA:
		    EDirectFadeRow(src,EPIXEL_TYPE_RGBA,
				   dst, dstPixelType, fader, pcount);
		    src += pcount*4;
		    break;
		    
		case EANIM_COPY:
		    if (fader == ALPHA_FACTOR_1)
			EDirectCopyRow(src, srcPixelType,
				       dst, dstPixelType,
				       pcount);
		    else if (fader != ALPHA_FACTOR_0)
			EDirectAlphaRow(src, srcPixelType,
					dst, dstPixelType, 
					fader, pcount);
		    src += pcount*srcPixelSize;
		    break;

		case EANIM_FILL:
		    a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		    if (a == EALPHA_TRANSPARENT)
			;
		    else if (a == EALPHA_OPAQUE)
			EDirectFillRow(dst, dstPixelType, pcount,
				       epixel_argb(a,src[1],src[2],src[3]));
		    else
			EDirectFillRowBlend(dst, dstPixelType, pcount,
					    epixel_argb(a,src[1],
							src[2],src[3]));
		    src += 4;
		    break;
		    
		default:
		    break;
		}

		dst += pcount*dstPixelSize;
		
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	else {  // we need to clip dds
	    unsigned char *dst = EPIXEL_ADDR(pic, xi, yi);

	    while(w) {
		EAnimPixels *hdr = (EAnimPixels *) src;
		u_int32_t pcount;
		u_int8_t a;

		if (hdr->type == EANIM_INDIRECT) {
		    u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		    n_blocks = hdr->nblocks;   // number of blocks to run 
		    src_save = src + sizeof(EAnimPixels); // continue point
		    src = isrc;
		    hdr = (EAnimPixels*) src; // switch to indirect header
		}

		src += sizeof(EAnimPixels);
		pcount = hdr->count;
		w -= pcount;

		switch(hdr->type) {
		case EANIM_SKIP:
		    dst  += pcount*dstPixelSize;
		    xi   += pcount;
		    break;

		case EANIM_SHADOW: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
			
		    n = EIntersectSegments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs;
			    dst1 += offs*dstPixelSize;
			}
			EDirectAddColorRow(src1, EPIXEL_TYPE_A8,
					   dst1, dstPixelType,
					   fader, epixel_transparent(),
					   n, EFLAG_BLEND);
		    }
		    dst  += pcount*dstPixelSize;
		    src  += pcount;
		    xi   += pcount;
		    break;
		}

		case EANIM_RGBA: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = EIntersectSegments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*4;
			    dst1 += offs*dstPixelSize;
			}
			EDirectFadeRow(src1,EPIXEL_TYPE_RGBA,
				       dst1, dstPixelType, fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*4;
		    xi  += pcount;
		    break;
		}

		case EANIM_BGRA: {
		    u_int8_t      *src1 = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = EIntersectSegments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*4;
			    dst1 += offs*dstPixelSize;
			}
			EDirectFadeRow(src1,EPIXEL_TYPE_BGRA,
				       dst1, dstPixelType, fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*4;
		    xi  += pcount;
		    break;
		}
		    
		case EANIM_COPY: {
		    u_int8_t *src1      = src;
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = EIntersectSegments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    src1 += offs*srcPixelSize;
			    dst1 += offs*dstPixelSize;
			}
			if (fader == ALPHA_FACTOR_1)
			    EDirectCopyRow(src1, srcPixelType,
					   dst1, dstPixelType,
					   n);
			else if (fader != ALPHA_FACTOR_0)
			    EDirectAlphaRow(src1, srcPixelType,
					    dst1, dstPixelType, 
					    fader, n);
		    }
		    dst += pcount*dstPixelSize;
		    src += pcount*srcPixelSize;
		    xi  += pcount;
		    break;
		}

		case EANIM_FILL: {
		    unsigned char *dst1 = dst;
		    int n, bx;
		    n = EIntersectSegments(xi,pcount,clip_x0,clip_len,&bx);
		    if (n > 0) {
			int offs;
			if ((offs = (bx - xi))) {
			    dst1 += offs*dstPixelSize;
			}
			a= (fader==ALPHA_FACTOR_1)?src[0]:((src[0]*fader)>>8);
			if (a == EALPHA_TRANSPARENT)
			    ;
			else if (a == EALPHA_OPAQUE)
			    EDirectFillRow(dst1, dstPixelType, n,
					   epixel_argb(a,src[1],src[2],src[3]));
			else
			    EDirectFillRowBlend(dst1, dstPixelType, n,
						epixel_argb(a,src[1],src[2],src[3]));
		    }
		    dst += pcount*dstPixelSize;
		    src += 4;
		    xi  += pcount;
		    break;
		}

		    
		default:
		    break;
		}
		
		if (n_blocks) {  // are we running indirect blocks?
		    if (n_blocks == 1)
			src = src_save;   // restore & continue
		    n_blocks--;
		}
	    }
	}
	yi++;
    }
    return;

/*
 *  General anim block draw when no clipping is needed 
 *  
 */
no_clip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    EAnimPixels* hdr = (EAnimPixels*) src;
	    u_int32_t pcount;
	    u_int8_t a;

	    if (hdr->type == EANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(EAnimPixels); // continue point
		src = isrc;
		hdr = (EAnimPixels*) src; // switch to indirect header
	    }

	    src += sizeof(EAnimPixels);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EANIM_SKIP:
		break;

	    case EANIM_SHADOW:
		EDirectAddColorRow(src, EPIXEL_TYPE_A8,
				   dst, dstPixelType,
				   fader, epixel_transparent(),
				   pcount, EFLAG_BLEND);
		src  += pcount;
		break;

	    case EANIM_RGBA:
		EDirectFadeRow(src,EPIXEL_TYPE_BGRA,
			       dst, dstPixelType, fader, pcount);
		src += pcount*4;
		break;

	    case EANIM_BGRA:
		EDirectFadeRow(src,EPIXEL_TYPE_BGRA,
			       dst, dstPixelType, fader, pcount);
		src += pcount*4;
		break;
		
	    case EANIM_COPY:
		if (fader == ALPHA_FACTOR_1)
		    EDirectCopyRow(src, srcPixelType,
				   dst, dstPixelType, pcount);
		else if (fader != ALPHA_FACTOR_0)
		    EDirectAlphaRow(src, srcPixelType,
				    dst, dstPixelType, fader, pcount);
		src += pcount*srcPixelSize;
		break;

	    case EANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EALPHA_TRANSPARENT)
		    ;
		else if (a == EALPHA_OPAQUE)
		    EDirectFillRow(dst, dstPixelType, pcount,
				   epixel_argb(a,src[1],src[2],src[3]));
		else
		    EDirectFillRowBlend(dst, dstPixelType, pcount,
					epixel_argb(a,src[1],src[2],src[3]));
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*dstPixelSize;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
    return;
/*
 * BGRA anim block draw when no clipping is needed 
 *   both srcPixelType and dstPixelType is BGRA
 */
bgra_noclip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    EAnimPixels* hdr = (EAnimPixels*) src;
	    u_int32_t pcount;
	    u_int8_t  a;

	    if (hdr->type == EANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(EAnimPixels); // continue point
		src = isrc;
		hdr = (EAnimPixels*) src; // switch to indirect header
	    }

	    src += sizeof(EAnimPixels);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EANIM_SKIP:
		break;

	    case EANIM_SHADOW:
		EDirectAddColorRow(src, EPIXEL_TYPE_A8,
				   dst, EPIXEL_TYPE_BGRA,
				   fader, epixel_transparent(),
				   pcount, EFLAG_BLEND);
		src  += pcount;
		break;

	    case EANIM_BGRA:
		if (fader == ALPHA_FACTOR_0)
		    ;
		else if (fader == ALPHA_FACTOR_1)
		    ESimdBlendAreaRGBA32(src, 0, dst, 0, pcount, 1);
		else
		    ESimdFadeAreaRGBA32(src, 0, dst, 0, fader, pcount, 1);
		src += pcount*4;
		break;

	    case EANIM_RGBA:
		EDirectFadeRow(src,EPIXEL_TYPE_RGBA,
			       dst, EPIXEL_TYPE_BGRA, fader, pcount);
		src += pcount*4;
		break;
		
	    case EANIM_COPY: // BGRA => BGRA 
		if (fader == ALPHA_FACTOR_1)
		    ESimdCopy(src, dst, pcount*4);
		else if (fader != ALPHA_FACTOR_0)
		    ESimdAlphaAreaRGBA32(src,0,dst,0,fader,pcount,1);
		src += pcount*4;
		break;

	    case EANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EALPHA_TRANSPARENT)
		    ;
		else if (a == EALPHA_OPAQUE) {
		    // FIXME this can be done by calling wmemset directly
		    EDirectFillRow(dst, EPIXEL_TYPE_BGRA, pcount,
				   epixel_argb(a,src[1],src[2],src[3]));
		}
		else /* note! swapping rgb into bgr */
		    ESimdFillAreaBlendRGBA32(dst, 0, pcount, 1,
					     epixel_argb(a,src[3],
							 src[2],src[1]));
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*4;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
    return;
/*
 * RGBA anim block draw when no clipping is needed 
 *   both srcPixelType and dstPixelType is RGBA
 */
rgba_noclip:
    while(height--) {
	unsigned char* dst = dst0;
	int w = width;

	dst0 += dst_wb;

	while(w) {
	    EAnimPixels* hdr = (EAnimPixels*) src;
	    u_int32_t pcount;
	    u_int8_t  a;

	    if (hdr->type == EANIM_INDIRECT) {
		u_int8_t *isrc = ((u_int8_t*)base) + hdr->count;
		n_blocks = hdr->nblocks;   // number of blocks to run 
		src_save = src + sizeof(EAnimPixels); // continue point
		src = isrc;
		hdr = (EAnimPixels*) src; // switch to indirect header
	    }

	    src += sizeof(EAnimPixels);
	    pcount = hdr->count;
	    w -= pcount;

	    switch(hdr->type) {
	    case EANIM_SKIP:
		break;

	    case EANIM_SHADOW:
		EDirectAddColorRow(src, EPIXEL_TYPE_A8,
				   dst, EPIXEL_TYPE_RGBA,
				   fader, epixel_transparent(),
				   pcount, EFLAG_BLEND);
		src  += pcount;
		break;

	    case EANIM_RGBA:
		if (fader == ALPHA_FACTOR_0)
		    ;
		else if (fader == ALPHA_FACTOR_1)
		    ESimdBlendAreaRGBA32(src, 0, dst, 0, pcount, 1);
		else
		    ESimdFadeAreaRGBA32(src, 0, dst, 0, fader, pcount, 1);
		src += pcount*4;
		break;

	    case EANIM_BGRA:
		EDirectFadeRow(src,EPIXEL_TYPE_BGRA,
			       dst, EPIXEL_TYPE_RGBA, fader, pcount);
		src += pcount*4;
		break;
		
	    case EANIM_COPY: // RGBA => RBGA 
		if (fader == ALPHA_FACTOR_1)
		    ESimdCopy(src, dst, pcount*4);
		else if (fader != ALPHA_FACTOR_0)
		    ESimdAlphaAreaRGBA32(src,0,dst,0,fader,pcount,1);
		src += pcount*4;
		break;

	    case EANIM_FILL:
		a= (fader==ALPHA_FACTOR_1) ? src[0] : ((src[0]*fader) >> 8);
		if (a == EALPHA_TRANSPARENT)
		    ;
		else if (a == EALPHA_OPAQUE) {
		    // FIXME this can be done by calling wmemset directly
		    EDirectFillRow(dst, EPIXEL_TYPE_RGBA, pcount,
				   epixel_argb(a,src[1],src[2],src[3]));
		}
		else
		    ESimdFillAreaBlendRGBA32(dst, 0, pcount, 1, 
					     epixel_argb(a,src[1],
							 src[2],src[3]));
		src += 4;
		break;

	    default: 
		break;
	    }
	    dst += pcount*4;
		
	    if (n_blocks) {  // are we running indirect blocks?
		if (n_blocks == 1)
		    src = src_save;   // restore & continue
		n_blocks--;
	    }
	}
    }
}

