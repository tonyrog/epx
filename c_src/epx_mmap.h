// map and unmap
#ifndef EPX_MMAP_H
#define EPX_MMAP_H

#include <stdlib.h>
#include <fcntl.h>

// files are opened as binary (windows opens text files otherwise)
#ifndef O_BINARY
#define O_BINARY 0
#endif

extern void* map_file_ro(int fd, size_t size);
extern int   unmap_file_ro(void* ptr, size_t size);

#endif
