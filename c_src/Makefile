#
# We assume that dthread has been checked out as well into an
# overlaid directory structure by
# meta-exosense/recipes-exodev/can/erlang-can.bb
#

PRIVDIR := ../priv

all : $(PRIVDIR)/epx_nif.so

override CFLAGS += -DDLOG_DEFAULT=DLOG_NONE -msse2 -DFB -DHAVE_MTTR -fpic -shared


$(PRIVDIR)/epx_nif.so : epx_debug.o epx_hash.o epx_dict.o \
		epx_pixel.o epx_pixmap.o epx_anim.o \
		epx_line.o  epx_ellipse.o epx_draw.o \
		epx_backend.o \
		epx_backend_fb.o epx_backend_none.o \
		epx_window.o epx_font.o \
		epx_gl.o epx_gc.o epx_simd.o \
		epx_simd_emu.o 	epx_simd_mmx.o \
		epx_simd_sse2.o epx.o \
		epx_nif.o
	$(CC) -shared -fpic $(LDFLAGS) 	\
	-o  $@ $^

clean:
	$(RM) -f $(PRIVDIR)/epx_nif.so *.o

