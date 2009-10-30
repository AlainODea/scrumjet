ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := scrumjet

all: deps/webmachine erl

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
	@(cd ebin;find . -type f ! -name ${APP}.app -execdir rm -v {} +)

run:
	./start-dev.sh