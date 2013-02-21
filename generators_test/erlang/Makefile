REBAR=../../rebar
GDT=../../gdt

all: tests

tests: compile
	$(REBAR) eunit skip_deps=true

generate:
	$(GDT) -o ./src ../test_types.gdt erlang

deps: $(REBAR)
	$(REBAR) get-deps

compile: deps generate
	$(REBAR) compile

clean:
	$(REBAR) clean