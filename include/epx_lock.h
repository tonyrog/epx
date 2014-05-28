/***************************************************************************
 *
 * Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
#ifndef __EPX_LOCK_H__
#define __EPX_LOCK_H__

typedef enum {
    EPX_LOCK_CREATE,
    EPX_LOCK_DESTROY,
    EPX_LOCK_LOCK,
    EPX_LOCK_UNLOCK
} epx_lock_command_t;

typedef void* epx_lock_t;

extern void epx_lock_init(void* (*cb)(epx_lock_command_t cmd, epx_lock_t lock));
extern epx_lock_t epx_lock_create(void);
extern void epx_lock_destroy(epx_lock_t lock);
extern int epx_lock_lock(epx_lock_t lock);
extern int epx_lock_unlock(epx_lock_t lock);

#endif
