PHONY: test

SUBDIRS= utils logic grammars acg-data scripting lambda

test:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) byte;)



superclean clean:
	rm -rf *~
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) $@;)

