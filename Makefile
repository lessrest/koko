build:
	docker build --rm -t lessrest/koko .
repl:
	docker run -it --rm -v `pwd`:/usr/src/koko lessrest/koko \
	  sh -c "cabal configure --enable-tests && cabal repl tests"
shell:
	docker run -it --rm lessrest/koko bash
