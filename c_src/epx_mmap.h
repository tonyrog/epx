// map and unmap
#ifndef EPX_MMAP_H
#define EPX_MMAP_H

#include <stdlib.h>

extern void* map_file_ro(int fd, size_t size);
extern int   unmap_file_ro(void* ptr, size_t size);

#endif
