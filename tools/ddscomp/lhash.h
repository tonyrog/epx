/*
** Linear hashing
**
*/

#ifndef __DDS_LHASH_H__
#define __DDS_LHASH_H__

#define DDS_LHASH_SZEXP   8
#define DDS_LHASH_SEGSZ   (1 << DDS_LHASH_SZEXP)
#define DDS_LHASH_SZMASK  ((1 << DDS_LHASH_SZEXP)-1)

#define DDS_LHASH_SEG(i)  ((i)>>DDS_LHASH_SZEXP)
#define DDS_LHASH_POS(i)  ((i)&DDS_LHASH_SZMASK)

#define DDS_LHASH_SEG_LEN         256   /* When growing init segs */
#define DDS_LHASH_SEG_INCREAMENT  128   /* Number of segments to grow */

#define DDS_LHASH_BUCKET(lh, i) (lh)->seg[DDS_LHASH_SEG(i)][DDS_LHASH_POS(i)]

#define DDS_LHASH_IX(lh, hval) \
    (((((hval) & (lh)->szm)) < (lh)->p) ? \
       ((hval) & (((lh)->szm << 1) | 1)) : \
       (((hval) & (lh)->szm)))
      

typedef unsigned long hash_value_t;

typedef struct _dds_lhash_bucket_t {
    struct _dds_lhash_bucket_t* next;
    hash_value_t hvalue;
} dds_lhash_bucket_t;

typedef struct {
    hash_value_t (*hash)(void*);  /* calculate hash */
    int (*cmp)(void*, void*);     /* compare data items */
    void (*release)(void*);       /* data release (free) */
    void* (*copy)(void*);         /* copy (may be used with insert) */

    int is_allocated;
    char* name;

    unsigned int thres;        /* Medium bucket chain len, for grow */
    unsigned int szm;          /* current size mask */
    unsigned int nactive;      /* Number of "active" slots */
    unsigned int nslots;       /* Total number of slots */
    unsigned int nitems;       /* Total number of items */
    unsigned int p;            /* Split position */
    unsigned int nsegs;        /* Number of segments */
    unsigned int n_resize;     /* Number of index realloc calls */
    unsigned int n_seg_alloc;  /* Number of segment allocations */
    unsigned int n_seg_free;   /* Number of segment destroy */
    dds_lhash_bucket_t*** seg;
} dds_lhash_t;

dds_lhash_t* dds_lhash_new(char* name, int thres,
		   hash_value_t (*hash)(void*),
		   int (*cmp)(void*, void*),
		   void (*release)(void*),
		   void* (*copy)(void*));


dds_lhash_t* dds_lhash_init(dds_lhash_t* lh, char* name, int thres, 
		    hash_value_t (*hash)(void*),
		    int (*cmp)(void*, void*),
		    void (*release)(void*),
		    void* (*copy)(void*));

void dds_lhash_delete(dds_lhash_t* lh);


void* dds_lhash_lookup(dds_lhash_t* lh, void* key);
void* dds_lhash_insert(dds_lhash_t* lh, void* key, void* data);
void* dds_lhash_insert_new(dds_lhash_t* lh, void* key, void* data);
void* dds_lhash_erase(dds_lhash_t* lh, void* key);

void dds_lhash_info(dds_lhash_t* lh);
void dds_lhash_each(dds_lhash_t* lh, 
		void (elem)(dds_lhash_t* lh, void* elem, void* arg),
		void* arg);

#endif
