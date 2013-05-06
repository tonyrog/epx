/*
**
*/
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "lhash.h"

#ifdef DEBUG
#define DBG(args...) fprintf(stderr, args)
#else
#define DBG(args...)
#endif


static dds_lhash_bucket_t** dds_lhash_alloc_seg(int seg_sz)
{
    dds_lhash_bucket_t** bp;
    int sz = sizeof(dds_lhash_bucket_t*)*seg_sz;

    bp = (dds_lhash_bucket_t**) malloc(sz);
    memset(bp, 0, sz);
    return bp;
}

inline static dds_lhash_bucket_t** dds_lhash_HLOOKUP(dds_lhash_t* lh,
					   hash_value_t hval,
					   void* key)
{
    int ix = DDS_LHASH_IX(lh, hval);
    dds_lhash_bucket_t** bpp = &DDS_LHASH_BUCKET(lh, ix);
    dds_lhash_bucket_t* b = *bpp;

    while(b != (dds_lhash_bucket_t*) 0) {
	if ((b->hvalue == hval) && (lh->cmp(key, (void*) b) == 0))
	    return bpp;
	bpp = &b->next;
	b = b->next;
    }
    return bpp;
}

/* scan bucket for key return bucket */
inline static dds_lhash_bucket_t** dds_lhash_LOOKUP(dds_lhash_t* lh, void* key)
{
    return dds_lhash_HLOOKUP(lh, lh->hash(key), key);
}


dds_lhash_t* dds_lhash_init(dds_lhash_t* lh, char* name, int thres,
		    hash_value_t (*hash)(void*),
		    int (*cmp)(void*, void*),
		    void (*release)(void*),
		    void* (*copy)(void*))
{
    dds_lhash_bucket_t*** bp;

    if ((bp = (dds_lhash_bucket_t***) malloc(sizeof(dds_lhash_bucket_t**))) == NULL)
	return NULL;
    lh->hash    = hash;
    lh->cmp     = cmp;
    lh->release = release;
    lh->copy    = copy;
    lh->is_allocated = 0;
    lh->name = name;
    lh->thres = thres;
    lh->szm = DDS_LHASH_SZMASK;
    lh->nactive = DDS_LHASH_SEGSZ;
    lh->nitems = 0;
    lh->p = 0;
    lh->nsegs = 1;
    lh->seg = bp;
    lh->seg[0] = dds_lhash_alloc_seg(DDS_LHASH_SEGSZ);
    lh->nslots = DDS_LHASH_SEGSZ;
    lh->n_seg_alloc = 1;
    lh->n_seg_free  = 0;
    lh->n_resize    = 0;
    return lh;
}


static void dds_lhash_grow(dds_lhash_t* lh)
{
    dds_lhash_bucket_t** bp;
    dds_lhash_bucket_t** bps;
    dds_lhash_bucket_t* b;
    unsigned int ix;
    unsigned int nszm = (lh->szm << 1) | 1;

    DBG("dds_lhash: grow\n");

    if (lh->nactive >= lh->nslots) {
	DBG("dds_lhash: grow, adding %d new slots\n", DDS_LHASH_SEGSZ);
	/* Time to get a new array */
	if (DDS_LHASH_POS(lh->nactive) == 0) {
	    unsigned int six = DDS_LHASH_SEG(lh->nactive);
	    if (six == lh->nsegs) {
		int i, sz;

		if (lh->nsegs == 1)
		    sz = DDS_LHASH_SEG_LEN;
		else
		    sz = lh->nsegs + DDS_LHASH_SEG_INCREAMENT;
		lh->seg = (dds_lhash_bucket_t***) realloc(lh->seg,
						      sizeof(dds_lhash_bucket_t**)*sz);
		lh->nsegs = sz;
		lh->n_resize++;
		for (i = six+1; i < sz; i++)
		    lh->seg[i] = 0;
	    }
	    lh->seg[six] = dds_lhash_alloc_seg(DDS_LHASH_SEGSZ);
	    lh->nslots += DDS_LHASH_SEGSZ;
	    lh->n_seg_alloc++;
	}
    }

    ix = lh->p;
    bp = &DDS_LHASH_BUCKET(lh, ix);
    ix += (lh->szm+1);
    bps = &DDS_LHASH_BUCKET(lh, ix);
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
static void dds_lhash_shrink(dds_lhash_t* lh)
{
    dds_lhash_bucket_t** bp;

    DBG("dds_lhash: shrink\n");

    if (lh->nactive == DDS_LHASH_SEGSZ)
	return;

    lh->nactive--;
    if (lh->p == 0) {
	lh->szm >>= 1;
	lh->p = lh->szm;
    }
    else
	lh->p--;

    bp = &DDS_LHASH_BUCKET(lh, lh->p);
    while(*bp != 0) 
	bp = &(*bp)->next;

    *bp = DDS_LHASH_BUCKET(lh, lh->nactive);
    DDS_LHASH_BUCKET(lh, lh->nactive) = 0;

    if ((lh->nactive & DDS_LHASH_SZMASK) == DDS_LHASH_SZMASK) {
	int six = DDS_LHASH_SEG(lh->nactive)+1;

	DBG("dds_lhash: shrink, removing %d slots\n", DDS_LHASH_SEGSZ);
	free(lh->seg[six]);
	lh->seg[six] = NULL;
	lh->nslots -= DDS_LHASH_SEGSZ;
	lh->n_seg_free++;
    }
}

dds_lhash_t* dds_lhash_new(char* name, int thres,
		   hash_value_t (*hash)(void*),
		   int (*cmp)(void*, void*),
		   void (*release)(void*),
		   void* (*copy)(void*))
{
    dds_lhash_t* tp;

    if ((tp = (dds_lhash_t*) malloc(sizeof(dds_lhash_t))) == NULL)
	return NULL;
    
    if (dds_lhash_init(tp, name, thres, hash, cmp, release, copy) == NULL) {
	free(tp);
	return NULL;
    }
    tp->is_allocated = 1;
    return tp;
}


void dds_lhash_delete(dds_lhash_t* lh)
{
    dds_lhash_bucket_t*** sp = lh->seg;
    int n = lh->nsegs;

    while(n--) {
	dds_lhash_bucket_t** bp = *sp;
	if (bp != 0) {
	    int m = DDS_LHASH_SEGSZ;
	    while(m--) {
		dds_lhash_bucket_t* p = *bp++;
		while(p != 0) {
		    dds_lhash_bucket_t* next = p->next;
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

void* dds_lhash_insert_new(dds_lhash_t* lh, void* key, void* data)
{
    hash_value_t hval = lh->hash(key);
    dds_lhash_bucket_t** bpp = dds_lhash_HLOOKUP(lh, hval, key);
    dds_lhash_bucket_t* b = *bpp;

    if (b != NULL) {
	DBG("dds_lhash: insert_new exists\n");
	/* release data if copy function is not defined */
	if (lh->copy==NULL) {
	    if (lh->release) lh->release(data);
	}
	return NULL;
    }
    DBG("dds_lhash: insert_new\n");
    b = (dds_lhash_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
    b->hvalue = hval;
    b->next = *bpp;
    *bpp = b;
    lh->nitems++;

    if ((lh->nitems / lh->nactive) >= lh->thres)
	dds_lhash_grow(lh);
    return (void*) b;
}

void* dds_lhash_insert(dds_lhash_t* lh, void* key, void* data)
{
    hash_value_t hval = lh->hash(key);
    dds_lhash_bucket_t** bpp = dds_lhash_HLOOKUP(lh, hval, key);
    dds_lhash_bucket_t* b = *bpp;

    DBG("dds_lhash: insert\n");

    if (b != NULL) {
	dds_lhash_bucket_t* b_next = b->next;
	if (lh->release != NULL) lh->release(b);
	/* printf("REPLACE %s\n", (char*) key); */
	b = (dds_lhash_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next = b_next;
	*bpp = b;
    }
    else {
	/* printf("INSERT %s\n", (char*) key); */
	b = (dds_lhash_bucket_t*) ((lh->copy != NULL) ? lh->copy(data) : data);
	b->hvalue = hval;
	b->next   = NULL;
	*bpp = b;
	lh->nitems++;

	if ((lh->nitems / lh->nactive) >= lh->thres)
	    dds_lhash_grow(lh);
    }
    return (void*) b;

}


void* dds_lhash_lookup(dds_lhash_t* lh, void* key)
{
    dds_lhash_bucket_t** bpp = dds_lhash_LOOKUP(lh, key);

    DBG("dds_lhash: lookup %x\n", key);

    return *bpp;
}

/*
** Erase an item
*/
void* dds_lhash_erase(dds_lhash_t* lh, void* key)
{
    dds_lhash_bucket_t** bpp = dds_lhash_LOOKUP(lh, key);
    dds_lhash_bucket_t* b = *bpp;

    /* printf("ERASE %s\n", (char*) key); */

    DBG("dds_lhash: erase %x\n", b);

    if (b != NULL) {
	*bpp = b->next;  /* unlink */
	if (lh->release) lh->release((void*) b);
	lh->nitems--;
	if ((lh->nitems / lh->nactive) < lh->thres)
	    dds_lhash_shrink(lh);
    }
    return (void*)b;
}


void dds_lhash_info(dds_lhash_t* lh)
{
    unsigned int i;
    int depth = 0;

    for (i = 0; i < lh->nslots; i++) {
	dds_lhash_bucket_t* list = DDS_LHASH_BUCKET(lh, i);
	int d = 0;

	while(list) {
 	    list = list->next;
	    d++;
	}
	if (d > depth)
	    depth = d;
    }
    printf("  Name: %s\n", lh->name);
    printf("  Size: %d\n", lh->szm+1);
    printf("Active: %d\n", lh->nactive);    
    printf(" Split: %d\n", lh->p);
    printf(" Items: %d\n", lh->nitems);
    printf(" Slots: %d\n", lh->nslots);
    printf("  Segs: %d\n", lh->nsegs);
    printf(" Thres: %d\n", lh->thres);
    printf(" Ratio: %e\n", (float) lh->nitems / (float) lh->nactive);
    printf("   Max: %d\n", depth);
    printf("Resize: %d\n", lh->n_resize);
    printf(" Alloc: %d\n", lh->n_seg_alloc);
    printf("  Free: %d\n", lh->n_seg_free);
    
}

void dds_lhash_each(dds_lhash_t* lh, void (elem)(dds_lhash_t* lh, void* elem, void* arg),
		void* arg)
{
    int i;
    int nslots = lh->nslots;

    for (i = 0; i < nslots; i++) {
	dds_lhash_bucket_t* list = DDS_LHASH_BUCKET(lh, i);
	while(list) {
	    dds_lhash_bucket_t* next = list->next;
	    elem(lh, (void*) list, arg);
	    list = next;
	}
    }
}
