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
/*
 * EPX Dictionary
 *
 *   Used to store key value pairs:
 *
 */
#include <string.h>

#include "../include/epx_dict.h"

#define EPX_DICT_ENTRY_CHUNK 32

static int edata_cmp(const epx_dict_data_t* a, const epx_dict_data_t* b)
{
    int i;

    if ((i = ((int) a->type - (int) b->type)) != 0)
	return i;
    switch(a->type) {
    case EPX_DICT_NONE:
	return 0;
    case EPX_DICT_BOOLEAN:
	return (a->u.v_boolean - b->u.v_boolean);
    case EPX_DICT_INTEGER:
	return (a->u.v_integer - b->u.v_integer);
    case EPX_DICT_FLOAT:
	if (a->u.v_float < b->u.v_float) return -1;
	if (a->u.v_float > b->u.v_float) return  1;
	return 0;
    case EPX_DICT_STRING:
	if ((i = (a->u.v_string.len - b->u.v_string.len)) != 0)
	    return i;
	return strncmp(a->u.v_string.ptr, b->u.v_string.ptr,
		       a->u.v_string.len);
    case EPX_DICT_BINARY:
	if ((i = (a->u.v_binary.len - b->u.v_binary.len)) != 0)
	    return i;
	return memcmp(a->u.v_binary.ptr, b->u.v_binary.ptr,
		      a->u.v_binary.len);
    case EPX_DICT_SEQUENCE:
	if ((i = (a->u.v_sequence.len - b->u.v_sequence.len)) != 0)
	    return i;
	for (i = 0; i < (int)a->u.v_sequence.len; i++) {
	    int j;
	    j = edata_cmp(&a->u.v_sequence.ptr[i],&b->u.v_sequence.ptr[i]);
	    if (j != 0)
		return j;
	}
	return 0;
    default:  // should not occure
	return 0;
    }
}

static void edata_offset(epx_dict_data_t* a, long offset)
{
    int i;

    switch(a->type) {
    case EPX_DICT_STRING:
	a->u.v_string.ptr = a->u.v_string.ptr + offset;
	break;
    case EPX_DICT_BINARY:
	a->u.v_binary.ptr = (void*) (((u_int8_t*)a->u.v_binary.ptr) + offset);
	break;
    case EPX_DICT_SEQUENCE:
	for (i = 0; i < (int) a->u.v_sequence.len; i++)
	    edata_offset(&a->u.v_sequence.ptr[i], offset);
	break;
    case EPX_DICT_NONE:
    case EPX_DICT_BOOLEAN:
    case EPX_DICT_INTEGER:
    case EPX_DICT_FLOAT:
    default:
	break;
    }
}

// Calculate external data size
static size_t edata_size(epx_dict_data_t* a)
{
    switch(a->type) {
    case EPX_DICT_STRING:
	return a->u.v_string.len+1; // add terminator
    case EPX_DICT_BINARY:
	return a->u.v_binary.len;
    case EPX_DICT_SEQUENCE: {
	int i;
	size_t sz = a->u.v_sequence.len*sizeof(epx_dict_data_t);
	for (i = 0; i < (int)a->u.v_sequence.len; i++)
	    sz += edata_size(&a->u.v_sequence.ptr[i]);
	return sz;
    }
    case EPX_DICT_NONE:
    case EPX_DICT_BOOLEAN:
    case EPX_DICT_INTEGER:
    case EPX_DICT_FLOAT:
    default:
	return 0;
    }
}

// Copy external data from a to b and store data in ptr
static u_int8_t* edata_copy(epx_dict_data_t* b, epx_dict_data_t* a, u_int8_t* ptr)
{
    switch(a->type) {
    case EPX_DICT_STRING:
	memcpy(ptr, a->u.v_string.ptr, a->u.v_string.len);
	ptr[a->u.v_string.len] = '\0';
	b->type = a->type;
	b->u.v_string.len = a->u.v_string.len;
	b->u.v_string.ptr = (char*) ptr;
	return ptr + b->u.v_string.len + 1;

    case EPX_DICT_BINARY:
	memcpy(ptr, a->u.v_binary.ptr, a->u.v_binary.len);
	b->type = a->type;
	b->u.v_binary.len = a->u.v_binary.len;
	b->u.v_binary.ptr = (void*) ptr;
	return ptr + b->u.v_binary.len;

    case EPX_DICT_SEQUENCE: {
	int i;
	b->type = a->type; 
	// FIXME: align
	b->u.v_sequence.ptr = (epx_dict_data_t*) ptr;
	b->u.v_sequence.len = a->u.v_sequence.len;
	ptr += a->u.v_sequence.len*sizeof(epx_dict_data_t);
	for (i = 0; i < (int) a->u.v_sequence.len; i++) {
	    ptr = edata_copy(&b->u.v_sequence.ptr[i],
			     &a->u.v_sequence.ptr[i], ptr);
	}
	return ptr;
    }
    case EPX_DICT_NONE:
    case EPX_DICT_BOOLEAN:
    case EPX_DICT_INTEGER:
    case EPX_DICT_FLOAT:
    default:
	*b = *a;
	return ptr;
    }
}

static int edict_kcmp(const epx_dict_data_t* key, const epx_dict_entry_t* b)
{
    return edata_cmp(key, &b->key);
}

static int edict_cmp(const epx_dict_entry_t* a, const epx_dict_entry_t* b) 
{
    return edata_cmp(&a->key, &b->key);
}

// search for match with bsearch in sorted dictionary
static int edict_bsearch(epx_dict_t* dict, epx_dict_data_t* key, int* ix)
{
    int l = 0;
    int h = dict->used-1;

    while(l <= h) {
	int m = (h + l) >> 1;
	int cmp = edict_kcmp(key, dict->entry[m]);
	if (cmp < 0)
	    h = m-1;
	else if (cmp > 0)
	    l = m+1;
	else {
	    *ix = m;
	    return 1;
	}
    }
    *ix = l;
    return 0;
}

static int edict_qsort(epx_dict_entry_t** v, size_t n)
{
    while(n >= 2) {
	int n1 = (n >> 2);
	epx_dict_entry_t* p = v[n1];
	epx_dict_entry_t** l = v;
	epx_dict_entry_t** r = v + (n-1);

	while(l <= r) {
	    if (edict_cmp(*l, p) < 0)
		l++;
	    else if (edict_cmp(*r, p) > 0)
		r--;
	    else {
		epx_dict_entry_t* t = *l;
		*l++ = *r;
		*r-- = t;
	    }
	}
	edict_qsort(v+n1, n-n1-1); // sort the top half recursive
	n = n1;
    }
    return 0;
}


static void edata_set_none(epx_dict_data_t* d)
{
    d->type = EPX_DICT_NONE;
}

static void edata_set_boolean(epx_dict_data_t* d, int bool)
{
    d->type = EPX_DICT_BOOLEAN;
    d->u.v_boolean = bool;
}

static void edata_set_integer(epx_dict_data_t* d, int val)
{
    d->type = EPX_DICT_INTEGER;
    d->u.v_integer = val;
}

static void edata_set_float(epx_dict_data_t* d, double val)
{
    d->type = EPX_DICT_FLOAT;
    d->u.v_float = val;
}

static void edata_set_string(epx_dict_data_t* d, char* val)
{
    d->type = EPX_DICT_STRING;
    d->u.v_string.ptr = val;
    d->u.v_string.len = strlen(val);
}

static void edata_set_binary(epx_dict_data_t* d, void* val, size_t len)
{
    d->type = EPX_DICT_BINARY;
    d->u.v_binary.ptr = val;
    d->u.v_binary.len = len;
}

static void edata_set_sequence(epx_dict_data_t* d, epx_dict_data_t* val, size_t len)
{
    d->type = EPX_DICT_SEQUENCE;
    d->u.v_sequence.ptr = val;
    d->u.v_sequence.len = len;
}


int epx_dict_init(epx_dict_t* dict)
{
    EPX_OBJECT_INIT(dict, EPX_DICT_TYPE);
    dict->entries    = 0;
    dict->used       = 0;
    dict->is_sorted  = 1;
    dict->entry      = NULL;
    return 0;
}

epx_dict_t* epx_dict_create()
{
    epx_dict_t* dict;

    if ((dict = (epx_dict_t*) malloc(sizeof(epx_dict_t))) == NULL)
	return NULL;
    epx_dict_init(dict);
    dict->on_heap = 1;
    dict->refc = 1;
    return dict;
}

void EPX_DICT_TYPE_RELEASE(void* arg)
{
    epx_dict_t* dict = (epx_dict_t*) arg;

    if (!dict) {
	EPX_DBGFMT("EPX_DICT_TYPE_RELEASE: NULL POINTER: %p", arg);
	return;
    }
    
    EPX_DBGFMT("EPX_DICT_TYPE_RELEASE: %p", arg);
    if (dict->entry) {
	size_t i;
	for (i = 0; i < dict->used; i++) {
	    if (dict->entry[i])
		free(dict->entry[i]);
	}
	free(dict->entry);
    }
    if (dict->on_heap)
	free(dict);
}

void epx_dict_destroy(epx_dict_t* dict)
{
    epx_object_unref(dict);
}

int epx_dict_init_copy(epx_dict_t* src, epx_dict_t* dst)
{
    size_t sz;
    size_t i;

    sz = sizeof(epx_dict_entry_t*)*(src->used+EPX_DICT_ENTRY_CHUNK);
    if (!(dst->entry = (epx_dict_entry_t**)malloc(sz)))
	goto mem_error;
    memset(dst->entry, 0, sz);
    dst->entries = src->used+EPX_DICT_ENTRY_CHUNK;
    dst->used    = src->used;
    dst->is_sorted = src->is_sorted;

    for (i = 0; i < src->used; i++) {
	epx_dict_entry_t* ent;
	long offset;

	sz = sizeof(epx_dict_entry_t)+src->entry[i]->mem_size;
	ent = (epx_dict_entry_t*)malloc(sz);
	if (!ent)
	    goto mem_error;
	memcpy(ent, src->entry[i],sz);
	offset = ((u_int8_t*) ent - (u_int8_t*) src->entry[i]);
	edata_offset(&ent->key, offset);
	edata_offset(&ent->data, offset);
	dst->entry[i] = ent;
    }
    return 0;
mem_error:
    EPX_DICT_TYPE_RELEASE(dst);
    return -1;
}


epx_dict_t* epx_dict_copy(epx_dict_t* dict)
{
    epx_dict_t* copy;

    if (!(copy = epx_dict_create()))
	return 0;
    if (epx_dict_init_copy(dict, copy) < 0)
	return 0;
    return copy;
}


void epx_dict_sort(epx_dict_t* dict)
{
    if (!dict->is_sorted) {
	edict_qsort(dict->entry, dict->used);
	dict->is_sorted = 1;
    }
}
	
// Copy dictionary data into dictionary entry
static void edict_update_ent(epx_dict_entry_t* ent, epx_dict_data_t* key, epx_dict_data_t* data)
{
    u_int8_t* ptr = ent->mem;

    if (!ent)
	return;
    ptr = edata_copy(&ent->key, key, ptr);
    edata_copy(&ent->data, data, ptr);
}


static int edict_add_ent(epx_dict_t* dict, epx_dict_entry_t* ent)
{
    size_t sz;

    if (dict->used < dict->entries) {
	int i = dict->used++;
	dict->entry[i] = ent;
	if (dict->is_sorted && (dict->used > 1) &&
	    (edict_cmp(dict->entry[i], dict->entry[i-1]) < 0))
	    dict->is_sorted = 0;
    }
    else if (dict->entry == NULL) {
	sz = sizeof(epx_dict_entry_t*)*EPX_DICT_ENTRY_CHUNK;
	if ((dict->entry = (epx_dict_entry_t**) malloc(sz)) == NULL) {
	    free(ent);
	    return -1;
	}
	dict->entry[0] = ent;
	dict->entries = EPX_DICT_ENTRY_CHUNK;
	dict->used    = 1;
	dict->is_sorted  = 1;
    }
    else {
	epx_dict_entry_t** entry;
	sz = sizeof(epx_dict_entry_t*)*(dict->entries+EPX_DICT_ENTRY_CHUNK);
	if ((entry = (epx_dict_entry_t**) realloc(dict->entry,sz)) == NULL) {
	    free(ent);
	    return -1;
	}
	dict->entry = entry;
	dict->entry[dict->used++] = ent;
	dict->entries += EPX_DICT_ENTRY_CHUNK;
	dict->is_sorted  = 0;
    }
    return 0;
}


int epx_dict_lookup_ix(epx_dict_t* dict, epx_dict_data_t* key)
{
    if (!dict)
	return -1;
    if (dict->is_sorted) {
	int ix;
	if (dict->used && edict_bsearch(dict, key, &ix))
	    return ix;
    }
    else {
	int i;
	for (i = 0; i < (int) dict->used; i++) {
	    if (edict_kcmp(key, dict->entry[i]) == 0)
		return i;
	}
    }
    return -1;
}

/* remove index ix and compact */
int epx_dict_del_ix(epx_dict_t* dict, int ix)
{
    if ((ix < 0) || (ix >= (int) dict->used))
	return -1;
    free(dict->entry[ix]);
    dict->entry[ix] = 0;
    dict->used--;
    if (ix < (int) dict->used) {
	if (dict->is_sorted) {
	    while(ix < (int) dict->used) {
		dict->entry[ix] = dict->entry[ix+1];
		ix++;
	    }
	}
	else {
	    dict->entry[ix] = dict->entry[dict->used];
	}
	dict->entry[dict->used] = 0;
    }
    return 0;
}

epx_dict_entry_t* epx_dict_lookup_ent(epx_dict_t* dict,epx_dict_data_t* key)
{
    int ix;
    if ((ix = epx_dict_lookup_ix(dict, key)) >= 0)
	return dict->entry[ix];
    return 0;
}


int epx_dict_set_ent(epx_dict_t* dict, epx_dict_data_t* key, epx_dict_data_t* data)
{
    size_t mem_size;
    int ix;

    mem_size = edata_size(key);
    mem_size += edata_size(data);

    if ((ix = epx_dict_lookup_ix(dict, key)) >= 0) {
	epx_dict_entry_t* ent = dict->entry[ix];
	if (mem_size <= ent->mem_size) 
	    edict_update_ent(ent, key, data);
	else {
	    if (!(ent = (epx_dict_entry_t*) realloc(ent,sizeof(epx_dict_entry_t)+mem_size)))
		return -1;
	    ent->mem_size = mem_size;
	    dict->entry[ix] = ent;
	    edict_update_ent(ent, key, data);
	}
	return 0;
    }
    else {
	epx_dict_entry_t* ent;
	if (!(ent = (epx_dict_entry_t*) malloc(sizeof(epx_dict_entry_t)+mem_size)))
	    return -1;
	ent->mem_size = mem_size;
	edict_update_ent(ent, key, data);
	return edict_add_ent(dict, ent);
    }
}

int epx_dict_unset_ent(epx_dict_t* dict, epx_dict_data_t* key)
{
    int ix;

    if ((ix = epx_dict_lookup_ix(dict, key)) >= 0)
	epx_dict_del_ix(dict, ix);
    return 0;
}

int epx_dict_unset(epx_dict_t* dict, char* key)
{
    epx_dict_data_t k;

    edata_set_string(&k, key);
    return epx_dict_unset_ent(dict, &k);
}

int epx_dict_lookup_boolean(epx_dict_t* dict, char* key, int* value)
{
    epx_dict_data_t k;    
    int ix;

    edata_set_string(&k, key);
    if ((ix = epx_dict_lookup_ix(dict, &k)) < 0)
	return -1;
    else if (dict->entry[ix]->data.type == EPX_DICT_INTEGER) {
	*value = dict->entry[ix]->data.u.v_integer;
	return 0;
    }
    else if (dict->entry[ix]->data.type == EPX_DICT_BOOLEAN) {
	*value = dict->entry[ix]->data.u.v_boolean;
	return 0;
    }
    return -1;
}

int epx_dict_lookup_integer(epx_dict_t* dict, char* key, int* value)
{
    epx_dict_data_t k;
    int ix;

    edata_set_string(&k, key);
    if ((ix = epx_dict_lookup_ix(dict, &k)) < 0)
	return -1;
    else if (dict->entry[ix]->data.type == EPX_DICT_INTEGER) {
	*value = dict->entry[ix]->data.u.v_integer;
	return 0;
    }
    else if (dict->entry[ix]->data.type == EPX_DICT_BOOLEAN) {
	*value = dict->entry[ix]->data.u.v_boolean;
	return 0;
    }
    return -1;
}

int epx_dict_lookup_float(epx_dict_t* dict, char* key, double* value)
{
    epx_dict_data_t k;    
    int ix;

    edata_set_string(&k, key);
    if ((ix = epx_dict_lookup_ix(dict, &k)) < 0)
	return -1;
    if (dict->entry[ix]->data.type == EPX_DICT_FLOAT) {
	*value = dict->entry[ix]->data.u.v_float;
	return 0;
    }
    else if (dict->entry[ix]->data.type == EPX_DICT_INTEGER) {
	*value = (double) dict->entry[ix]->data.u.v_integer;
	return 0;
    }
    else if (dict->entry[ix]->data.type == EPX_DICT_BOOLEAN) {
	*value = (double) dict->entry[ix]->data.u.v_boolean;
	return 0;
    }
    return -1;
}

int epx_dict_lookup_string(epx_dict_t* dict, char* key, char** value, size_t* len)
{
    epx_dict_data_t k;    
    int ix;

    edata_set_string(&k, key);
    if ((ix = epx_dict_lookup_ix(dict, &k)) < 0)
	return -1;
    if (dict->entry[ix]->data.type == EPX_DICT_STRING) {
	if (value)
	    *value = (char*) dict->entry[ix]->data.u.v_string.ptr;
	if (len)
	    *len   = dict->entry[ix]->data.u.v_string.len;
	return 0;
    }
    return -1;
}

int epx_dict_lookup_binary(epx_dict_t* dict, char* key, void** value, size_t* len)
{
    epx_dict_data_t k;
    int ix;

    edata_set_string(&k, key);
    if ((ix = epx_dict_lookup_ix(dict, &k)) < 0)
	return -1;
    if (dict->entry[ix]->data.type == EPX_DICT_BINARY) {
	if (value)
	    *value = (void*) dict->entry[ix]->data.u.v_binary.ptr;
	if (len)
	    *len   = dict->entry[ix]->data.u.v_binary.len;
	return 0;
    }
    else if (dict->entry[ix]->data.type == EPX_DICT_STRING) {
	if (value)
	    *value = (void*) dict->entry[ix]->data.u.v_string.ptr;
	if (len)
	    *len   = dict->entry[ix]->data.u.v_string.len;
	return 0;
    }
    return -1;
}

int epx_dict_set_none(epx_dict_t* dict, char* key)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_none(&d);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_float(epx_dict_t* dict, char* key, double value)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_float(&d, value);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_integer(epx_dict_t* dict, char* key, int value)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_integer(&d, value);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_boolean(epx_dict_t* dict, char* key, int value)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_boolean(&d, value);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_string(epx_dict_t* dict, char* key, char* value)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_string(&d, value);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_binary(epx_dict_t* dict, char* key, void* value, size_t len)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_binary(&d, value, len);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_set_sequence(epx_dict_t* dict, char* key, epx_dict_data_t* value, size_t len)
{
    epx_dict_data_t k, d;
    edata_set_string(&k, key);
    edata_set_sequence(&d, value, len);
    return epx_dict_set_ent(dict, &k, &d);
}

int epx_dict_is_key(epx_dict_t* dict, char* key)
{
    epx_dict_data_t k;

    edata_set_string(&k, key);
    if (epx_dict_lookup_ix(dict, &k) < 0)
	return 0;
    return 1;
}

size_t epx_dict_size(epx_dict_t* dict)
{
    return dict->used;
}

int epx_dict_first(epx_dict_t* dict, char** key, size_t* len)
{
    if (dict->used == 0)
	return -1;
    if (dict->entry[0]->key.type == EPX_DICT_STRING) {
	if (key)
	    *key = (char*) dict->entry[0]->key.u.v_string.ptr;
	if (len)
	    *len = dict->entry[0]->key.u.v_string.len;
	return 0;
    }
    return -1;
}

int epx_dict_next(epx_dict_t* dict, char** key, size_t* len)
{
    epx_dict_data_t k;
    int ix, r;

    if (dict->used == 0)
	return -1;
    edata_set_string(&k, *key);
    if (dict->is_sorted) {
	r = edict_bsearch(dict, &k, &ix);
	if (r) ix++;
    }
    else {
	for (ix = 0; ix < (int) dict->used; ix++) {
	    if (edict_kcmp(&k, dict->entry[ix]) == 0)
		break;
	}
	ix++;
    }
    if (ix >= (int)dict->used)
	return -1;
    if (dict->entry[ix]->key.type == EPX_DICT_STRING) {
	if (key)
	    *key = (char*) dict->entry[ix]->key.u.v_string.ptr;
	if (len)
	    *len = dict->entry[ix]->key.u.v_string.len;
	return 0;
    }
    return -1;
}
