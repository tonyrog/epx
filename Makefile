#
#
#
release:
	(cd c_src; $(MAKE) release)
	(cd tools; $(MAKE))

debug:
	(cd c_src; $(MAKE) debug)
	(cd tools; $(MAKE))

clean:
	(cd c_src; $(MAKE) clean)
	(cd tools; $(MAKE) clean)

spotless:
	(cd c_src; $(MAKE) clean)
	(cd tools; $(MAKE) clean)
