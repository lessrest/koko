build:
	docker build --rm -t lessrest/koko .
repl:
	docker run -it --rm \
	  -v `pwd`/src:/usr/src/koko/src \
	  -v `pwd`/test:/usr/src/koko/test lessrest/koko \
	  sh -c "cabal configure --enable-tests && cabal repl tests"
shell:
	docker run -it --rm \
	  -v `pwd`/src:/usr/src/koko/src \
	  -v `pwd`/test:/usr/src/koko/test lessrest/koko \
	   bash
