#
# Simple makefile for efnttool and the ddscomp compiler
#
# You will probably need libfreetype6-dev libavcodec-dev
#  libswscale-dev and libavformat-dev to build dds compiler
#

DDSCOMP=ddscomp

EFNTTOOL=efnttool

.PHONY: all ddscomp efnttool clean

all: efnttool ddscomp

efnttool:
	(cd $(EFNTTOOL); make)

ddscomp: $(DDSCOMP)/Makefile
	(cd $(DDSCOMP); make)

$(DDSCOMP)/Makefile: $(DDSCOMP)/configure
	(cd $(DDSCOMP); ./configure)

$(DDSCOMP)/configure:
	(cd $(DDSCOMP); autoconf)

clean:
	(cd $(EFNTTOOL); make clean)
	(cd $(DDSCOMP); make clean)

spotless:
	(cd $(EFNTTOOL); make clean)
	(cd $(DDSCOMP); make spotless)
