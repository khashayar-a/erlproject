.PHONY: all doc \
	compile quickcompile xcompile \
	rel  quickrel \
	clean distclean relclean \
	test ct eunit \
	build_overview \
	start start_wait ping attach stop hard_stop generate \
	analyze xref

all: deps compile

deps: rebar.config
	./rebar get-deps
	touch deps

doc:
	./rebar doc skip_deps=true

compile:
	./rebar compile

quickcompile:
	./rebar compile skip_deps=true

xcompile:
	./rebar compile xref skip_deps=true

clean:
	./rebar clean

distclean: clean relclean
	./rebar delete-deps

eunit:
	./rebar eunit skip_deps=true

xref:
	r=0; for i in $$(ls apps); do ./rebar xref apps=$$i skip_deps=true; res=$$?; if [ "$$res" != 0 ]; then r=$$res; fi; done; exit $$r

start: 