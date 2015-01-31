FROM lessrest/docker-haskell:latest
WORKDIR /usr/src/koko
COPY koko.cabal /usr/src/koko/koko.cabal
RUN cabal install --enable-tests --only-dependencies
COPY . /usr/src/koko
RUN cabal configure --enable-tests
RUN cabal test && cabal install
ENV PATH /root/.cabal/bin:$PATH