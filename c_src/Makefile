#
# Make the epx library
#
OSNAME := $(shell uname -s)
MACHINE := $(shell uname -m)
RELEASE := $(shell uname -r)
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
CC = gcc
RANLIB = ranlib

EPX_DIR := $(shell cd ..; pwd)

include ../vsn.mk

MAC_OS_X  = No
WIN32     = No
LINUX     = No
X11       = No
XCB       = No
FB        = No
GLX       = No
AGL       = No
FBGL      = No
ALTIVEC   = No
MMX       = No
SSE       = No
SSE2      = No
SSE3      = No
SSE4      = No
AVX2      = No
NEON      = No

OBJDIR=../obj
LIBDIR=../priv

ifeq ($(TYPE), debug)
CFG_LIB=$(EPX_DIR)/priv/epx.a -lm 
DFLAGS += -DDEBUG -D_THREAD_SAFE -D_REENTRANT -DDLOG_DEFAULT=DLOG_DEBUG
CFLAGS += -g -Wall -Wextra -Wswitch-default -Wswitch-enum -fno-common 
endif

ifeq ($(TYPE), release)
CFG_LIB=$(EPX_DIR)/priv/epx.a -lm 
DFLAGS += -D_THREAD_SAFE -D_REENTRANT
CFLAGS += -O3 -Wall -Wextra -Wswitch-default -Wswitch-enum -fno-common
endif

nif_target:	CFLAGS += -I$(ERL_TOP)/usr/include

CFG_CFLAGS=-I$(EPX_DIR)/include

ifeq ($(OSNAME), Linux)
OSNAME = Linux
LINUX = Yes
ifeq ($(TARGET_SYS),)
FB  = Yes
X11 = Yes
XCB = Yes
CFLAGS += -DHAVE_MTTR
else
FB  = Yes
CC = $(TARGET_SYS)-gcc
NEON = Yes
endif
LD_SHARED	:= $(CC) -shared
DLL_LIBS=
endif

ifeq ($(OSNAME), Darwin)
MAC_OS_X = Yes
AGL      = Yes
X11      = Yes
GLX      = Yes
LD_SHARED := $(CC) -bundle -flat_namespace -undefined suppress
ARCH_FLAGS  += -arch i386 -arch x86_64 
DLL_LIBS=
endif

ifeq ($(WITH_X11), 1)
X11 = Yes
GLX = Yes
endif


ifeq ($(TARGET_SYS),)

ifeq ($(MACHINE), "Power Macintosh")
ALTIVEC   = Yes
endif

# bug in gcc 4.2.1 (Apple build 5664)
ifeq ($(MACHINE), i386)
MMX       = Yes
SSE       = No
SSE2      = Yes
SSE3      = No
endif

ifeq ($(MACHINE), i686)
MMX       = Yes
SSE       = No
SSE2      = Yes
SSE3      = No
endif

ifeq ($(MACHINE), x86_64)
MMX       = Yes
SSE       = No
SSE2      = Yes
SSE3      = No
endif

endif


BE_OBJS     =  epx_backend_none.o
BE_LD_FLAGS = 

#
# LINUX X11 with optional GLX support
#
ifeq ($(LINUX), Yes)
X11_PATH=/usr
BE_LD_FLAGS += -L$(X11_PATH)/lib
CFG_LDFLAGS += -L$(X11_PATH)/lib

ifeq ($(XCB), Yes)
	DFLAGS += -DXCB
	CFLAGS += -I$(X11_PATH)/include
	BE_OBJS += epx_backend_xcb.o
	BE_LD_FLAGS += -lxcb
	CFG_LDFLAGS += -lxcb
endif

ifeq ($(X11), Yes)
	DFLAGS += -DX11
	CFLAGS += -I$(X11_PATH)/include
	BE_OBJS += epx_backend_x11.o
	BE_LD_FLAGS += -lX11
	CFG_LDFLAGS += -lX11
endif

ifeq ($(GLX), Yes)
	DFLAGS += -DX11_GLX -DHAVE_OPENGL
	CFLAGS +=
	BE_LD_FLAGS += -lGL -lGLU
	CFG_LDFLAGS += -lGL -lGLU
endif

endif

#
# MacOSX with optional X11,GLX,AGL support
#
ifeq ($(MAC_OS_X),Yes)
	DFLAGS += -DMAC_OS_X
	CFLAGS += -ObjC
	# BE_OBJS += epx_backend_cocoa.o
	BE_OBJS += epx_backend_macos.o
	BE_LD_FLAGS += -framework Carbon -framework CoreFoundation -framework Foundation -framework AppKit
	CFG_LDFLAGS += -framework Carbon -framework CoreFoundation -framework Foundation -framework AppKit
ifeq ($(AGL),Yes)
	DFLAGS += -DMAC_OS_AGL -DHAVE_OPENGL
	CFLAGS +=
	BE_LD_FLAGS += -framework OpenGL -framework AGL -lGL -lGLU
	CFG_LDFLAGS += -framework AGL -framework OpenGL
	CFG_LDFLAGS += -Wl,-dylib_file,/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib
endif

ifeq ($(X11), Yes)
ifeq ($(RELEASE), 16.7.0)
	DFLAGS += -DX11 -I/usr/X11/include
	BE_LD_FLAGS += -L/usr/X11/lib -lX11
	CFG_LDFLAGS += -L/usr/X11/lib -lX11
else
	DFLAGS += -DX11 -I/usr/X11R6/include
	BE_LD_FLAGS += -L/usr/X11R6/lib -lX11
	CFG_LDFLAGS += -L/usr/X11R6/lib -lX11
endif
	CFLAGS +=
	BE_OBJS += epx_backend_x11.o
ifeq ($(GLX),Yes)
	DFLAGS += -DX11_GLX -DHAVE_OPENGL
	CFLAGS += 
	BE_LD_FLAGS += -framework OpenGL
	CFG_LDFLAGS += -framework OpenGL -lGL
endif
endif
endif

# Should be optional ?
ifeq ($(MAC_OS_X),Yes)
	CFG_LDFLAGS += -sectcreate __TEXT __info_plist epx.plist
endif


ifeq ($(WIN32),Yes)
	DFLAGS += -DWIN32
	BE_OBJS += epx_backend_win32.o
endif

ifeq ($(FB),Yes)
	DFLAGS += -DFB
	BE_OBJS += epx_backend_fb.o
endif

CFG_LDFLAGS += $(CFG_LIB)

SIMD_OBJS = epx_simd_emu.o

ifeq ($(ALTIVEC),Yes)
	DFLAGS +=  -DUSE_ALTIVEC
	ARCH_FLAGS += -maltivec
	SIMD_OBJS += epx_simd_altivec.o
endif

ifeq ($(SSE),Yes)
	ARCH_FLAGS += -msse
endif

ifeq ($(SSE2),Yes)
	DFLAGS += -DUSE_SSE2
	ARCH_FLAGS += -msse2
	SIMD_OBJS += epx_simd_sse2.o
endif

ifeq ($(SSE3),Yes)
	ARCH_FLAGS += -msse3
endif

ifeq ($(SSE4),Yes)
	ARCH_FLAGS += -msse4.1
endif

ifeq ($(AVX2),Yes)
	DFLAGS += -DUSE_AVX2
	ARCH_FLAGS += -mavx2
	SIMD_OBJS += epx_simd_avx2.o
endif

ifeq ($(MMX),Yes)
	DFLAGS += -DUSE_MMX
	ARCH_FLAGS += -mmmx
	SIMD_OBJS += epx_simd_mmx.o
endif

LIB_OBJS = \
	epx_debug.o \
	epx_hash.o \
	epx_dict.o \
	epx_t2d.o \
	epx_pixel.o \
	epx_pixmap.o \
	epx_bitmap.o \
	epx_canvas.o \
	epx_poly.o \
	epx_anim.o \
	epx_line.o \
	epx_ellipse.o \
	epx_draw.o \
	epx_lock.o \
	epx_backend.o \
	epx_window.o \
	epx_font.o \
	schrift.o \
	epx_sft.o \
	epx_gl.o \
	epx_gc.o \
	epx_simd.o \
	epx.o

EPX_LIB = $(LIBDIR)/epx.a
EPX_NIF = $(LIBDIR)/epx_nif.so

EPX_LIB_OBJS  = $(LIB_OBJS:%=$(OBJDIR)/%)
EPX_SIMD_OBJS = $(SIMD_OBJS:%=$(OBJDIR)/%)
EPX_BE_OBJS   = $(BE_OBJS:%=$(OBJDIR)/%)

SRC = $(LIB_OBJS:%.o=%.c) $(SIMD_OBJS:%.o=%.c) $(BE_OBJS:%.o=%.c)

WARN = -Wall -Wextra -Wswitch-default -Wswitch-enum 

DFLAGS += -I ../include 
CFLAGS += $(WARN) -g -fPIC

nif:
	$(MAKE) nif_target TYPE=release
	$(MAKE) epx-config TYPE=release

nif_debug:
	$(MAKE) nif_target TYPE=debug
	$(MAKE) epx-config TYPE=debug

clean:
	$(MAKE) clean_target TYPE=release
	$(MAKE) clean_target TYPE=debug

debug:
	$(MAKE) lib_target TYPE=debug
	$(MAKE) epx-config TYPE=debug

release:
	$(MAKE) lib_target TYPE=release
	$(MAKE) epx-config TYPE=release

lib_target: $(EPX_LIB)

nif_target: $(EPX_LIB) $(EPX_NIF)

epx-config: $(OBJDIR)/epx-config
	chmod +x $(OBJDIR)/epx-config

clean_target:
	-rm -f $(EPX_LIB) $(OBJDIR)/*.o $(EPX_NIF)

$(EPX_LIB): $(EPX_LIB_OBJS) $(EPX_SIMD_OBJS) $(EPX_BE_OBJS)
	rm -f $@
	$(AR) rcv $@ $(EPX_LIB_OBJS) $(EPX_SIMD_OBJS) $(EPX_BE_OBJS)
ifdef RANLIB
	$(RANLIB) $@
endif

$(EPX_NIF): $(EPX_LIB) $(OBJDIR)/epx_nif.o
	$(LD_SHARED) -o $@ $(OBJDIR)/epx_nif.o $(DLL_LIBS)  $(EPX_LIB) $(LD_FLAGS) $(ARCH_FLAGS) $(BE_LD_FLAGS)

$(OBJDIR)/epx_nif.o: epx_nif.c
	$(CC) -c -o $@ -MMD -MF .epx_nif.d $(DFLAGS) $(CFLAGS) $(ARCH_FLAGS) epx_nif.c

$(OBJDIR)/epx-config: epx-config.src ../vsn.mk
	sed -e 's;%EPX_VSN%;$(EPX_VSN);' -e 's;%CFLAGS%;$(CFG_CFLAGS);' -e 's;%LDFLAGS%;$(CFG_LDFLAGS);' $< > $@

# we should probably generate dependcies dependent on (at least) debug flags!
$(OBJDIR)/%.o:	%.c
	$(CC) -c -o $@ -MMD -MF .$<.d $(CFLAGS) $(ARCH_FLAGS) $(DFLAGS) $<

-include .*.d
