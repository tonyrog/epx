#ifndef __EPX_HASH_H__
#define __EPX_HASH_H__

#include <stdint.h>

typedef uintptr_t epx_hash_value_t;

typedef struct _epx_bucket_t {
    struct _epx_bucket_t* next;
    epx_hash_value_t hvalue;
} epx_bucket_t;

typedef struct {
    epx_hash_value_t (*hash)(void*);  // calculate hash
    int (*cmp)(void*, void*);         // compare data items
    void (*release)(void*);           // data release (free)
    void* (*copy)(void*);             // copy (may be used with insert)
    
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
    epx_bucket_t*** seg;
} epx_hash_t;

extern epx_hash_t* epx_hash_new(char* name, int thres,
				epx_hash_value_t (*hash)(void*),
				int (*cmp)(void*, void*),
				void (*release)(void*),
				void* (*copy)(void*));
extern epx_hash_t* epx_hash_init(epx_hash_t* lh, char* name, int thres, 
				 epx_hash_value_t (*hash)(void*),
				 int (*cmp)(void*, void*),
				 void (*release)(void*),
				 void* (*copy)(void*));
extern void epx_hash_delete(epx_hash_t* lh);
extern void* epx_hash_lookup(epx_hash_t* lh, void* key);
extern void* epx_hash_insert(epx_hash_t* lh, void* key, void* data);
extern void* epx_hash_insert_new(epx_hash_t* lh, void* key, void* data);
extern void* epx_hash_erase(epx_hash_t* lh, void* key);
extern void epx_hash_each(epx_hash_t* lh, 
			  void (elem)(epx_hash_t* lh, void* elem, void* arg),
			  void* arg);

#endif
