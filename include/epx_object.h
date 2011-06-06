//
// epx_object.h
//
//   EPX object internal api
//
#ifndef __EPX_OBJECT_H__
#define __EPX_OBJECT_H__

#include "epx_hash.h"
#include "epx_debug.h"



typedef enum
{
    EPX_BACKEND_TYPE,
    EPX_WINDOW_TYPE,
    EPX_PIXMAP_TYPE,
    EPX_BITMAP_TYPE,
    EPX_GC_TYPE,
    EPX_DICT_TYPE,
    EPX_FONT_TYPE
} epx_object_type_t;

extern void EPX_BACKEND_TYPE_RELEASE(void*);
extern void EPX_WINDOW_TYPE_RELEASE(void*);
extern void EPX_PIXMAP_TYPE_RELEASE(void*);
extern void EPX_BITMAP_TYPE_RELEASE(void*);
extern void EPX_GC_TYPE_RELEASE(void*);
extern void EPX_DICT_TYPE_RELEASE(void*);
extern void EPX_FONT_TYPE_RELEASE(void*);

#define EPX_OBJECT_INIT(obj,Type) do {		\
    (obj)->on_heap = 0;				\
    (obj)->refc    = 0;				\
    (obj)->release = Type##_RELEASE;		\
    (obj)->opaque = 0;				\
    (obj)->next   = 0;			\
    (obj)->type   = (Type);		\
    } while(0)

#define EPX_OBJECT_MEMBERS(Type_t) \
    epx_bucket_t   hbucket;			\
    int            on_heap;			\
    unsigned int   refc;			\
    void           (*release)(void*);		\
    void*          opaque;			\
    epx_object_type_t  type;			\
    Type_t*        next

/* generic object */
typedef struct _epx_object_t {
    EPX_OBJECT_MEMBERS(struct _epx_object_t);
} epx_object_t;

static inline void epx_object_ref(void* arg)
{
    epx_object_t* obj = (epx_object_t*) arg;
    if (obj)
	obj->refc++;
}

static inline void epx_object_unref(void* arg)
{
    epx_object_t* obj = (epx_object_t*) arg;
    if (obj) {
	if (obj->refc <= 1) {
	    if (obj->release)
		obj->release(obj);
	}
	else
	    obj->refc--;
    }
}

static inline void epx_object_link(void* list, void* obj)
{
    ((epx_object_t*)obj)->next = (epx_object_t*)*((void**)list);
    *((void**)list) = obj;
}

static inline void epx_object_unlink(void* list, void* obj)
{
    epx_object_t** pp = (epx_object_t**)list;
    while((*pp) && (*pp) != (epx_object_t*)obj) {
	pp = &(*pp)->next;
    }
    if ((*pp) == (epx_object_t*)obj)
	*pp = ((epx_object_t*)obj)->next;
}

#endif
