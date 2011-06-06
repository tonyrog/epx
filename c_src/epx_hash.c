/*
** Linear hash of Epic objects
*/
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "epx_hash.h"

#define EPX_HASH_SZEXP   8
#define EPX_HASH_SEGSZ   (1 << EPX_HASH_SZEXP)
#define EPX_HASH_SZMASK  ((1 << EPX_HASH_SZEXP)-1)

#define EPX_HASH_SEG(i)  ((i)>>EPX_HASH_SZEXP)
#define EPX_HASH_POS(i)  ((i)&EPX_HASH_SZMASK)

#define EPX_HASH_SEG_LEN         256   /* When growing init segs */
#define EPX_HASH_SEG_INCREAMENT  128   /* Number of segments to grow */

#define EPX_HASH_BUCKET(lh, i) (lh)->seg[EPX_HASH_SEG(i)][EPX_HASH_POS(i)]

#define EPX_HASH_IX(lh, hval) \
    (((((hval) & (lh)->szm)) < (lh)->p) ? \
       ((hval) & (((lh)->szm << 1) | 1)) : \
       (((hval) & (lh)->szm)))
      

static epx_bucket_t** _epx_hash__alloc_seg(int seg_sz)
{
    epx_bucket_t** bp;
    int sz = sizeof(epx_bucket_t*)*seg_sz;

    bp = (epx_bucket_t**) malloc(sz);
    memset(bp, 0, sz);
    return bp;
}

inline static epx_bucket_t** _epx_hash__hlookup(epx_hash_t* lh,
					      epx_hash_value_t hval,
					      void* key)
{
    int ix = EPX_HASH_IX(lh, hval);
    epx_bucket_t** bpp = &EPX_HASH_BUCKET(lh, ix);
    epx_bucket_t* b = *bpp;

    while(b) {
	if ((b->hvalue == hval) && (lh->cmp(key, (void*) b) == 0))
	    return bpp;
	bpp = &b->next;
	b = b->next;
    }
    return bpp;
}

/* scan bucket for key return bucket */
inline static epx_bucket_t** _epx_hash__lookup(epx_hash_t* lh, void* key)
{
    return _epx_hash__hlookup(lh, lh->hash(key), key);
}


epx_hash_t* epx_hash_init(epx_hash_t* lh, char* name, int thres,
			  epx_hash_value_t (*hash)(void*),
			  int (*cmp)(void*, void*),
			  void (*release)(void*),
			  void* (*copy)(void*))
{
    epx_bucket_t*** bp;

    if ((bp = (epx_bucket_t***) malloc(sizeof(epx_bucket_t**))) == NULL)
	return NULL;
    lh->hash    = hash;
    lh->cmp     = cmp;
    lh->release = release;
    lh->copy    = copy;
    lh->is_allocated = 0;
    lh->name = name;
    lh->thres = thres;
    lh->szm = EPX_HASH_SZMASK;
    lh->nactive = EPX_HASH_SEGSZ;
    lh->nitems = 0;
    lh->p = 0;
    lh->nsegs = 1;
    lh->seg = bp;
    lh->seg[0] = _epx_hash__alloc_seg(EPX_HASH_SEGSZ);
    lh->nslots = EPX_HASH_SEGSZ;
    lh->n_seg_alloc = 1;
    lh->n_seg_free  = 0;
    lh->n_resize    = 0;
    return lh;
}


static void epx_hash_grow(epx_hash_t* lh)
{
    epx_bucket_t** bp;
    epx_bucket_t** bps;
    epx_bucket_t* b;
    unsigned int ix;
    unsigned int nszm = (lh->szm << 1) | 1;

    if (lh->nactive >= lh->nslots) {
	// Time to get a new array 
	if (EPX_HASH_POS(lh->nactive) == 0) {
	    unsigned int six = EPX_HASH_SEG(lh->nactive);
	    if (six == lh->nsegs) {
		int i, sz;

		if (lh->nsegs == 1)
		    sz = EPX_HASH_SEG_LEN;
		else
		    sz = lh->nsegs + EPX_HASH_SEG_INCREAMENT;
		lh->seg = (epx_bucket_t***) realloc(lh->seg,
						      sizeof(epx_bucket_t**)*sz);
		lh->nsegs = sz;
		lh->n_resize++;
		for (i = six+1; i < sz; i++)
		    lh->seg[i] = 0;
	    }
	    lh->seg[six] = _epx_hash__alloc_seg(EPX_HASH_SEGSZ);
	    lh->nslots += EPX_HASH_SEGSZ;
	    lh->n_seg_alloc++;
	}
    }

    ix = lh->p;
    bp = &EPX_HASH_BUCKET(lh, ix);
    ix += (lh->szm+1);
    bps = &EPX_HASH_BUCKET(lh, ix);
    b = *bp;

    while (b != 0) {
	ix = b->hvalue & nszm;

	if (ix == lh->p)
	    bp = &b->next;          /* object stay */
	else {
	    *bp = b->next;  	    /* unlink */
	    b->next = *bps;         /* link */
	    *bps = b;
	}
	b = *bp;
    }

    lh->nactive++;
    if (lh->p == lh->szm) {
	lh->p = 0;
	lh->szm = nszm;
    }
    else
	lh->p++;
}

/*
** Shrink the hash table
** Remove segments if they are empty
** but do not reallocate the segment index table !!!
*/
static void epx_hash_shrink(epx_hash_t* lh)
{
    epx_bucket_t** bp;

    if (lh->nactive == EPX_HASH_SEGSZ)
	return;

    lh->nactive--;
    if (lh->p == 0) {
	lh->szm >>= 1;
	lh->p = lh->szm;
    }
    else
	lh->p--;

    bp = &EPX_HASH_BUCKET(lh, lh->p);
    while(*bp != 0) 
	bp = &(*bp)->next;

    *bp = EPX_HASH_BUCKET(lh, lh->nactive);
    EPX_HASH_BUCKET(lh, lh->nactive) = 0;

    if ((lh->nactive & EPX_HASH_SZMASK) == EPX_HASH_SZMASK) {
	int six = EPX_HASH_SEG(lh->nactive)+1;
	free(lh->seg[six]);
	lh->seg[six] = NULL;
	lh->nslots -= EPX_HASH_SEGSZ;
	lh->n_seg_free++;
    }
}

epx_hash_t* epx_hash_new(char* name, int thres,
		epx_hash_value_t (*hash)(void*),
		int (*cmp)(void*, void*),
		void (*release)(void*),
		void* (*copy)(void*))
{
    epx_hash_t* tp;

    if ((tp = (epx_hash_t*) malloc(sizeof(epx_hash_t))) == NULL)
	return NULL;
    
    if (epx_hash_init(tp, name, thres, hash, cmp, release, copy) == NULL) {
	free(tp);
	return NULL;
    }
    tp->is_allocated = 1;
    return tp;
}


void epx_hash_delete(epx_hash_t* lh)
{
    epx_bucket_t*** sp = lh->seg;
    int n = lh->nsegs;

    while(n--) {
	epx_bucket_t** bp = *sp;
	if (bp != 0) {
	    int m = EPX_HASH_SEGSZ;
	    while(m--) {
		epx_bucket_t* p = *bp++;
		while(p != 0) {
		    epx_bucket_t* next = p->next;
		    if (lh->release)
			(*lh->release)((void*) p);
		    p = next;
		}
	    }
	    free(*sp);
	}
	sp++;
    }
    free(lh->seg);

    if (lh->is_allocated)
	free(lh);
}

void* epx_hash_insert_new(epx_hash_t* lh, void* key, void* data)
{
    epx_hash_value_t hval = lh->hash(key);
    epx_bucket_t** bpp = _epx_hash__hlookup(lh, hval, key);
    epx_bucket_t* b = *bpp;

    if (b != NULL) {
	// release data if copy function is not defined 
	if (lh->copy==NULL) {
	    if (lh->release) lh->release(data);
	}
	return NULL;
    }
    b = (epx_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
    b->hvalue = hval;
    b->next = *bpp;
    *bpp = b;
    lh->nitems++;

    if ((lh->nitems / lh->nactive) >= lh->thres)
	epx_hash_grow(lh);
    return (void*) b;
}

void* epx_hash_Insert(epx_hash_t* lh, void* key, void* data)
{
    epx_hash_value_t hval = lh->hash(key);
    epx_bucket_t** bpp = _epx_hash__hlookup(lh, hval, key);
    epx_bucket_t* b = *bpp;

    if (b != NULL) {
	epx_bucket_t* b_next = b->next;
	if (lh->release != NULL) lh->release(b);
	b = (epx_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next = b_next;
	*bpp = b;
    }
    else {
	b = (epx_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next   = NULL;
	*bpp = b;
	lh->nitems++;

	if ((lh->nitems / lh->nactive) >= lh->thres)
	    epx_hash_grow(lh);
    }
    return (void*) b;

}


void* epx_hash_Lookup(epx_hash_t* lh, void* key)
{
    epx_bucket_t** bpp = _epx_hash__lookup(lh, key);
    return *bpp;
}

/*
** Erase an item
*/
void* epx_hash_Erase(epx_hash_t* lh, void* key)
{
    epx_bucket_t** bpp = _epx_hash__lookup(lh, key);
    epx_bucket_t* b = *bpp;

    if (b != NULL) {
	*bpp = b->next;  /* unlink */
	if (lh->release) lh->release((void*) b);
	lh->nitems--;
	if ((lh->nitems / lh->nactive) < lh->thres)
	    epx_hash_shrink(lh);
    }
    return (void*)b;
}

void epx_hash_each(epx_hash_t* lh, void (elem)(epx_hash_t* lh, void* elem, void* arg),
	       void* arg)
{
    int i;
    int nslots = lh->nslots;

    for (i = 0; i < nslots; i++) {
	epx_bucket_t* list = EPX_HASH_BUCKET(lh, i);
	while(list) {
	    epx_bucket_t* next = list->next;
	    elem(lh, (void*) list, arg);
	    list = next;
	}
    }
}

