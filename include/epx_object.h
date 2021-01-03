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
// epx_object.h
//
//   EPX object internal api
//
#ifndef __EPX_OBJECT_H__
#define __EPX_OBJECT_H__

#include "epx_hash.h"
#include "epx_debug.h"
#include "epx_lock.h"

typedef enum
{
    EPX_BACKEND_TYPE,
    EPX_WINDOW_TYPE,
    EPX_PIXMAP_TYPE,
    EPX_BITMAP_TYPE,
    EPX_GC_TYPE,
    EPX_DICT_TYPE,
    EPX_FONT_TYPE,
    EPX_ANIM_TYPE,
    EPX_CANVAS_TYPE
} epx_object_type_t;

extern void EPX_BACKEND_TYPE_RELEASE(void*);
extern void EPX_WINDOW_TYPE_RELEASE(void*);
extern void EPX_PIXMAP_TYPE_RELEASE(void*);
extern void EPX_BITMAP_TYPE_RELEASE(void*);
extern void EPX_GC_TYPE_RELEASE(void*);
extern void EPX_DICT_TYPE_RELEASE(void*);
extern void EPX_FONT_TYPE_RELEASE(void*);
extern void EPX_ANIM_TYPE_RELEASE(void*);
extern void EPX_CANVAS_TYPE_RELEASE(void*);

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

typedef struct _epx_object_list_t {
    epx_lock_t    lock;
    unsigned      length;
    epx_object_t* first;
} epx_object_list_t;

static inline void epx_object_ref(void* arg)
{
    epx_object_t* obj = (epx_object_t*) arg;
    // FIXME: make atomic
    if (obj) obj->refc++;
}

static inline void epx_object_unref(void* arg)
{
    epx_object_t* obj = (epx_object_t*) arg;
    if (obj) {
	// FIXME: make atomic
	if (obj->refc <= 1) {
	    if (obj->release)
		obj->release(obj);
	}
	else
	    obj->refc--;
    }
}

static inline void epx_object_list_init(epx_object_list_t* list)
{
    list->lock = epx_lock_create();
    list->first = (epx_object_t*) 0;
    list->length = 0;
}

static inline void epx_object_link_(epx_object_list_t* list, epx_object_t* obj)
{
    epx_lock_lock(list->lock);
    obj->next = list->first;
    list->first = obj;
    list->length++;
    epx_lock_unlock(list->lock);
}

#define epx_object_link(list, obj) epx_object_link_(list, (epx_object_t*) (obj))


static inline void epx_object_unlink_(epx_object_list_t* list, epx_object_t* obj)
{
    epx_object_t** pp = &list->first;

    epx_lock_lock(list->lock);
    while((*pp) && ((*pp) != obj)) {
	pp = &(*pp)->next;
    }
    if ((*pp) == obj) {
	list->length--;
	*pp = obj->next;
	obj->next = (epx_object_t*) 0;
    }
    epx_lock_unlock(list->lock);
}

#define epx_object_unlink(list, obj) epx_object_unlink_(list, (epx_object_t*) (obj))

#endif
