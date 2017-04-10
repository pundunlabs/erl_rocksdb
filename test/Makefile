include ../vsn.mk
VSN=$(LEVELDB_VSN)

EBIN = ../ebin

RM ?= rm -f

MODULES = \
	phofs \
	leveldb_test \
	leveldb_utils_test \

ERL_FILES = $(MODULES:%=%.erl)
ERL_BEAMS = $(MODULES:%=$(EBIN)/%.beam)


INCLUDE_DIRS = \
	../include \

ERL_INCLUDE = $(INCLUDE_DIRS:%=-I %)

.PHONY: all clean beams

all: $(EBIN) beams

beams: $(ERL_BEAMS)

$(EBIN)/%.beam: %.erl
	erlc $(ERL_INCLUDE) -o $(EBIN) $<

$(EBIN):
	mkdir $@

$(APP_TARGET): $(APP_SRC) 
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	$(RM) ../ebin/*.beam
