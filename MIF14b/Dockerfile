FROM haskell:7.10

WORKDIR /opt/server

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./MIF14.cabal /opt/server/MIF14.cabal

RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/server
RUN cabal install
RUN MIF14
CMD ["MIF14"]