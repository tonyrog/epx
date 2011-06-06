//
// debug logging support
//

#include <stdio.h>
#include <stdarg.h>

int epx_debug_mask = 0;

void epx_emit_error(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}

void epx_debug(int debug_mask)
{
    epx_debug_mask = debug_mask;
}
