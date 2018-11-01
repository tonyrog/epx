#
#
#
release:
	(cd c_src; $(MAKE) -f Makefile.clib release)
	(cd tools; $(MAKE))

debug:
	(cd c_src; $(MAKE) -f Makefile.clib debug)
	(cd tools; $(MAKE))

clean:
	(cd c_src; $(MAKE) -f Makefile.clib clean)
	(cd tools; $(MAKE) clean)

spotless:
	(cd c_src; $(MAKE) -f Makefile.clib clean)
	(cd tools; $(MAKE) clean)
