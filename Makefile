#
#
#
release:
	(cd c_src; $(MAKE) release)
	(cd src; $(MAKE) release)
	(cd ddscomp; autoconf; ./configure; make)

debug:
	(cd c_src; $(MAKE) debug)
	(cd src; $(MAKE) debug)
