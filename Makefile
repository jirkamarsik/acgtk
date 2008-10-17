PHONY: config byte opt clean superclean install

BINARIES=acgc acgc.opt acg acg.opt


prefix = /usr/local
exec_prefix = ${prefix}

byte opt: 
	$(MAKE) -C src $@
	for file in $(BINARIES); do find . -name "$$file" -exec cp {} . \; ; done

all: byte opt

superclean clean:
	if test "$@" = clean ; then $(MAKE) -C config $@ ; fi
	$(MAKE) -C src $@
	rm -rf *.log  *~ autom4te.cache
	find . -name "*~" -exec rm -f {} \;
	-for file in $(BINARIES); do rm $$file ; done

install:
	for file in $(BINARIES); do if test -x $$file ; then cp $$file ${exec_prefix}/bin/. ; fi ; done

uninstall:
	for file in $(BINARIES); do if test -x ${exec_prefix}/bin/$$file ; then rm ${exec_prefix}/bin/$$file ; fi ; done


# Part for the auto configuration

config: configure

configure: config/configure.ac
	cd $(<D) && autoconf && mv configure .. & cd ..

