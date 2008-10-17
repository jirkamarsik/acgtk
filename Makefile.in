PHONY: config test

BINARIES=acgc acgc.opt acg acg.opt

test opt: 
	$(MAKE) -C src $@
	for file in $(BINARIES); do find . -name "$$file" -exec cp {} . \; ; done


superclean clean:
	if test "$@" = clean ; then $(MAKE) -C config $@ ; fi
	$(MAKE) -C src $@
	rm -rf *.log  *~ autom4te.cache
	find data/. -name "*~" -exec rm -f {} \;
	-for file in $(BINARIES); do rm $$file ; done



# Part for the auto configuration

config: configure

configure: config/configure.ac
	cd $(<D) && autoconf && mv configure .. & cd ..

