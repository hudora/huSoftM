all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

check:
	dialyzer -I include --src -c src
