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
// epx_dict.h
//
//   EPX dictionary api
//
#ifndef __EPX_DICT_H__
#define __EPX_DICT_H__

#include <stdlib.h>
#include "epx_object.h"

typedef enum {
    EPX_DICT_NONE     = 0,  // no value
    EPX_DICT_BOOLEAN  = 1,  // boolean values
    EPX_DICT_INTEGER  = 2,  // integer value
    EPX_DICT_FLOAT    = 3,  // float value
    EPX_DICT_STRING   = 4,  // string value
    EPX_DICT_BINARY   = 5,  // binary value
    EPX_DICT_SEQUENCE = 6   // sequence of (EDictData)
} epx_dict_type_t;

struct _epx_dict_t;

typedef struct _epx_dict_data_t {
    epx_dict_type_t type;
    union {
	int     v_boolean;  // bool
	int     v_integer;  // integer
	double  v_float;    // float
	struct {
	    char*  ptr;     // string (with terminating 0)
	    size_t len;     // terminator is not included in length!
	} v_string;
	struct {
	    void*  ptr;
	    size_t len;
	} v_binary;
	struct {
	    struct _epx_dict_data_t* ptr;
	    size_t len;
	} v_sequence;
    } u;
} epx_dict_data_t;

// dictionary entry
typedef struct _epx_dict_entry_t {
    epx_dict_data_t key;
    epx_dict_data_t data;
    /*! Total size in mem area. */
    size_t    mem_size;
    /*! Data storage area. */
    uint8_t mem[0];
} epx_dict_entry_t;
   
typedef struct _epx_dict_t {
    EPX_OBJECT_MEMBERS(struct _epx_dict_t);
    /*! Number allocated stuff in dict. */
    uint32_t    entries;
    /*! Number used stuff in dict. */
    uint32_t    used;
    /*! Bsearch possible predicate. */
    int          is_sorted;
    /*! Dictionary entries. */
    epx_dict_entry_t** entry;      
} epx_dict_t;

// epx_dict_t interface

extern int         epx_dict_init(epx_dict_t* dict);
extern epx_dict_t* epx_dict_create(void);
extern int         epx_dict_init_copy(epx_dict_t* src, epx_dict_t* dst);
extern void        epx_dict_destroy(epx_dict_t* dict);
extern epx_dict_t* epx_dict_copy(epx_dict_t* dict);
extern void        epx_dict_sort(epx_dict_t* dict);

extern int epx_dict_lookup_ix(epx_dict_t* dict, epx_dict_data_t* key);

extern epx_dict_entry_t* epx_dict_lookup_ent(epx_dict_t* dict,
					     epx_dict_data_t* key);

extern int epx_dict_set_ent(epx_dict_t* dict, 
			    epx_dict_data_t* key, 
			    epx_dict_data_t* data);

extern int epx_dict_unset_ent(epx_dict_t* dict, epx_dict_data_t* key);

// String key interface
extern int epx_dict_unset(epx_dict_t* dict, char* key);
extern int epx_dict_lookup_boolean(epx_dict_t* dict, char* key, 
				   int* value);
extern int epx_dict_lookup_integer(epx_dict_t* dict, char* key, 
				   int* value);
extern int epx_dict_lookup_float(epx_dict_t* dict, char* key,
				 double* value);
extern int epx_dict_lookup_string(epx_dict_t* dict, char* key, 
				  char** value, size_t* len);
extern int epx_dict_lookup_binary(epx_dict_t* dict, char* key,
				  void** value,size_t* len);
extern int epx_dict_lookup_dict(epx_dict_t* dict, char* key,
				epx_dict_t** value);

extern int epx_dict_set_none(epx_dict_t* dict, char* key);
extern int epx_dict_set_boolean(epx_dict_t* dict, char* key, int value);
extern int epx_dict_set_integer(epx_dict_t* dict, char* key, int value);
extern int epx_dict_set_float(epx_dict_t* dict, char* key, double value);
extern int epx_dict_set_string(epx_dict_t* dict, char* key, char* value);
extern int epx_dict_set_binary(epx_dict_t* dict, char* key, void* value, 
			       size_t len);
extern int epx_dict_set_sequence(epx_dict_t* dict, char* key, 
				 epx_dict_data_t* value, size_t len);
extern int epx_dict_set_dict(epx_dict_t*, char* key, epx_dict_t* value);

extern size_t epx_dict_size(epx_dict_t* dict);
extern int epx_dict_is_key(epx_dict_t* dict, char* key);

extern int epx_dict_first(epx_dict_t* dict, char** key, size_t* len);
extern int epx_dict_next(epx_dict_t* dict, char** key, size_t* len);

#endif
