#
#
#
release:
	(cd c_src; $(MAKE) release)
	(cd src; $(MAKE) release)
	(cd tools; $(MAKE))

debug:
	(cd c_src; $(MAKE) debug)
	(cd src; $(MAKE) debug)
	(cd tools; $(MAKE))
