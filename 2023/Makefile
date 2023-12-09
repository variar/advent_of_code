TOPTARGETS := all clean

SUBDIRS := $(wildcard */.)

$(TOPTARGETS): $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TOPTARGETS) $(SUBDIRS)

next:
	mkdir $(day)
	cp template/Makefile $(day)
	cp template/00.hs $(day)/$(day)_1.hs
	cp template/00.hs $(day)/$(day)_2.hs
	touch $(day)/$(day)_test
	touch $(day)/$(day)_input