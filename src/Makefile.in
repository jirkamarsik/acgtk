PHONY: test

SUBDIRS= utils logic grammars acg-data scripting lambda

test:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) byte;)

opt:
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) opt;)



superclean clean:
	rm -rf *~ *.log
	$(foreach dir,$(SUBDIRS),$(MAKE) -r -S -C $(dir) $@;)

