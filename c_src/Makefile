#
# We assume that dthread has been checked out as well into an
# overlaid directory structure by
# meta-exosense/recipes-exodev/can/erlang-can.bb
#

PRIVDIR := ../priv

all : $(PRIVDIR)/epx_nif.so

override CFLAGS += -DDLOG_DEFAULT=DLOG_NONE -msse2 -DFB -DHAVE_MTTR -fpic -shared


$(PRIVDIR)/epx_nif.so : epx_debug.c epx_hash.c epx_dict.c \
		epx_pixel.c epx_pixmap.c epx_anim.c \
		epx_line.c  epx_ellipse.c epx_draw.c \
		epx_backend.c \
		epx_backend_fb.c epx_backend_none.c \
		epx_window.c epx_font.c \
		epx_gl.c epx_gc.c epx_simd.c \
		epx_simd_emu.c 	epx_simd_mmx.c \
		epx_simd_sse2.c epx.c \
		epx_nif.c \
	$(CC) -shared -fpic $(LDFLAGS) 	\
	-o  $@ $^

clean:
	$(RM) -f $(PRIVDIR)/epx_nif.so *.o

