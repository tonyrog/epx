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
// Lock API functions
//
#include "../include/epx_lock.h"

#define NO_LOCK ((epx_lock_t) 0)

static void* (*lockf)(epx_lock_command_t cmd, void* lock) = 0;

void epx_lock_init(void* (*cb)(epx_lock_command_t cmd, epx_lock_t lock))
{
    lockf = cb;
}

void* epx_lock_create()
{
    if (lockf != 0)
	return (*lockf)(EPX_LOCK_CREATE, NO_LOCK);
    return NO_LOCK;
}

void epx_lock_destroy(epx_lock_t lock)
{
    if ((lockf != 0) && (lock != NO_LOCK))
	(*lockf)(EPX_LOCK_DESTROY, lock);
}

int epx_lock_lock(epx_lock_t lock)
{
    if ((lockf != 0) && (lock != NO_LOCK)) {
	long r = (long) (*lockf)(EPX_LOCK_LOCK, lock);
	return (int) r;
    }
    return 0;
}

int epx_lock_unlock(epx_lock_t lock)
{
    if ((lockf != 0) && (lock != NO_LOCK)) {
	long r = (long) (*lockf)(EPX_LOCK_UNLOCK, lock);
	return (int) r;
    }
    return 0;
}
