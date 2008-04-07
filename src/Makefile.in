PHONY: test

SUBDIRS= utils grammars lambdas

test:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) byte;)



superclean clean:
	rm -rf *~
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) $@;)

