ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := scrumjet

all: deps/webmachine erl ebin/$(APP).app

deps/webmachine:
	@mkdir -p deps
	@hg clone http://bitbucket.org/justin/webmachine/ deps/webmachine
	@(cd deps/webmachine;$(MAKE))

erl:
	@mkdir -p ebin
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app:
	@cp -v src/$(APP).app $@

run:
	./start-dev.sh