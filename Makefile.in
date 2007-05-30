PHONY: config test

test: 
	$(MAKE) -C src $@



superclean clean:
	if test "$@" = clean ; then $(MAKE) -C config $@ ; fi
	$(MAKE) -C src $@
	rm -rf *.log  *~ autom4te.cache
	find data/. -name "*~" -exec rm -f {} \;



# Part for the auto configuration

config: configure

configure: config/configure.ac
	cd $(<D) && autoconf && mv configure .. & cd ..

