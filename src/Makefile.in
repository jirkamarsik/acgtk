PHONY: test

SUBDIRS= utils logic grammars lambda

test:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) byte;)



superclean clean:
	rm -rf *~
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) $@;)

