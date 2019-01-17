OSNAME := $(shell uname -s)
EPX_DIR=$(HOME)/erlang/epx
EPX_REL=$(EPX_DIR)/priv

ifeq ($(OSNAME), Darwin)
LDFLAGS += -arch i386
CFLAGS += -arch i386 -DDARWIN
EPX_CFLAGS  += $(shell $(EPX_REL)/epx-config --cflags)
EPX_LDFLAGS += $(shell $(EPX_REL)/epx-config --libs)
else
ifeq ($(OSNAME), Linux)
#LDFLAGS += -m32
#CFLAGS += -m32
## 32bit path
EPX_CFLAGS  += $(shell $(EPX_REL)/epx-config --cflags)
EPX_LDFLAGS += $(shell $(EPX_REL)/epx-config --libs)
#EPX_LDFLAGS += -L/usr/lib/i386-linux-gnu -lX11
endif
endif

CFLAGS = -O3 $(EPX_CFLAGS)

epx_bench:	epx_bench.o
	$(CC) -o$@ epx_bench.o ../priv/epx.a -lm

bary:	bary.o
	$(CC) $(LDFLAGS) -o $@ bary.o $(EPX_LDFLAGS)



