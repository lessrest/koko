FROM haskell:7.8
RUN cabal update
WORKDIR /usr/src/koko
COPY . /usr/src/koko
RUN cabal configure && cabal install
WORKDIR /root
ENV PATH /root/.cabal/bin:$PATH