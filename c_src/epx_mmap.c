
#ifdef WIN32

#include <windows.h>
#include <io.h>

void* map_file_ro(int fd, size_t size)
{
	HANDLE fh;
	HANDLE mh;
	void *p;

	// Get the OS handle from the CRT descriptor
	fh = (HANDLE)_get_osfhandle(fd);
	if (fh == INVALID_HANDLE_VALUE)
		return NULL;

	mh = CreateFileMapping(fh, NULL, PAGE_READONLY,
			       (DWORD)((unsigned long long)size >> 32),
			       (DWORD)(size & 0xffffffff), NULL);
	if (mh == NULL)
		return NULL;

	p = MapViewOfFile(mh, FILE_MAP_READ, 0, 0, size);

	// The view keeps the mapping object alive
	CloseHandle(mh);
	return p;
}

int unmap_file_ro(void* ptr, size_t size)
{
    (void) size;
    UnmapViewOfFile(ptr);
    return 0;
}

#else

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>

void* map_file_ro(int fd, size_t size)
{
    return mmap(0, size, PROT_READ, MAP_SHARED, fd, 0);
}

int unmap_file_ro(void* ptr, size_t size)
{
    return munmap(ptr, size);
}
 
#endif
