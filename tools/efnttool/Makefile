#
# Make file efnttool
#
BINDIR = ../../priv/bin
TARGET := $(BINDIR)/efnttool


FT_CFLAGS  = $(shell freetype-config --cflags)
FT_LIBS    = $(shell freetype-config --libs)
FT_VERSION = $(shell freetype-config --version)

EPX_CFLAGS = $(shell ../../obj/release/epx-config --cflags)
EPX_LIBS   = $(shell ../../obj/release/epx-config --libs)


CFLAGS = -g -Wall $(FT_CFLAGS) $(EPX_CFLAGS) 
LIBS = $(EPX_LIBS) $(FT_LIBS) -lm -ldl

.PHONY: all efnttool

all:	$(TARGET)

efnttool.o:	efnttool.c
	$(CC) -c $(CFLAGS) efnttool.c

$(TARGET):	efnttool.o
	$(CC) -o $(TARGET) efnttool.o $(LIBS)