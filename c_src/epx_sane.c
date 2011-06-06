//
//
//
#include <stdlib.h>
#include <stddef.h>
#include "epx.h"


// size and offset print out

void print_sizes(void)
{
    printf("sizeof(epx_pixel_t) = %zd\n", sizeof(epx_pixel_t));
    printf("sizeof(epx_pixmap_t) = %zd\n", sizeof(epx_pixmap_t));
    // offsets
    printf("offsetof(epx_pixel_t.a) = %zd\n", offsetof(epx_pixel_t, a));
    printf("offsetof(epx_pixel_t.r) = %zd\n", offsetof(epx_pixel_t, r));
    printf("offsetof(epx_pixel_t.g) = %zd\n", offsetof(epx_pixel_t, g));
    printf("offsetof(epx_pixel_t.b) = %zd\n", offsetof(epx_pixel_t, b));
}


main()
{
    print_sizes();
    exit(0);
}

