#@BEGIN-APP-DEFAULT-RULES@
APP=$(shell basename `cd ..; pwd`)
ERLC=$(shell which erlc)
ERLC_FLAGS=-MMD -MF .$<.d -I ../.. +debug_info
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=../ebin/%.beam)
ALL_OBJECTS=$(ERL_OBJECTS)
ERL_MODULES=$(ERL_SOURCES:%.erl=%)
comma=,
empty=
space = $(empty) $(empty)
MODULES=$(subst $(space),$(comma),$(ERL_MODULES))
VERSION=$(shell git describe --always --tags)
APP_SRC=$(APP).app.src
APP_TARGET=../ebin/$(APP).app

all: $(APP_TARGET) $(ALL_OBJECTS)

clean:
	rm -f $(ALL_OBJECTS) *.core .*.d

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

$(APP_TARGET): $(APP_SRC)
	sed -e 's;{vsn,.*git};{vsn,"$(VERSION)"};' -e 's;"@@MODULES@@";$(MODULES);' $(APP_SRC) > $(APP_TARGET)

-include .*.d
#@END-APP-DEFAULT-RULES@
