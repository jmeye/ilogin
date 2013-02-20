
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

run:
	erl +K true +A 16 +a 2048 -name ilogin@127.0.0.1 -pa apps/*/ebin -pa ebin -pa edit -pa deps/*/ebin -s ilogin -s reloader -boot start_sasl 

shell:
	erl -name debug@127.0.0.1 -remsh ilogin@127.0.0.1

