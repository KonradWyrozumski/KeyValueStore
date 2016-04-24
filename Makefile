all:
	rebar get-deps compile
run:
	erl -pa ebin deps/*/ebin -s key_value_store
clean:
	rebar clean
