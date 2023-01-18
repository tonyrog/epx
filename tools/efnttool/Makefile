#
# Make file efnttool
#
BINDIR = ../../priv/bin
TARGET := $(BINDIR)/ttfinfo # $(BINDIR)/efnttool 

FT_CFLAGS  = $(shell freetype-config --cflags)
FT_LIBS    = $(shell freetype-config --libs)
FT_VERSION = $(shell freetype-config --version)

EPX_CFLAGS = $(shell ../../obj/release/epx-config --cflags)
EPX_LIBS   = $(shell ../../obj/release/epx-config --libs)

CFLAGS = -g -Wall $(FT_CFLAGS) $(EPX_CFLAGS)
LIBS = $(EPX_LIBS) $(FT_LIBS) -lm -ldl

.PHONY: all efnttool clean

all:	$(TARGET)

efnttool.o:	efnttool.c
	$(CC) -c $(CFLAGS) efnttool.c

$(BINDIR)/efnttool:	efnttool.o $(BINDIR)
	$(CC) -o $@ efnttool.o $(LIBS)

$(BINDIR)/ttfinfo:	ttfinfo.o schrift.o $(BINDIR)
	$(CC) -o $@ ttfinfo.o schrift.o -lm ../../priv/epx.a

ttfinfo.o:	ttfinfo.c ../../c_src/schrift.h
	$(CC) -O -c ttfinfo.c

schrift.o:	../../c_src/schrift.c ../../c_src/schrift.h
	$(CC) -O3 -O -c -I../../c_src/ ../../c_src/schrift.c

clean:
	rm -f efnttool.o $(TARGET)

$(BINDIR):
	mkdir -p $(BINDIR)
