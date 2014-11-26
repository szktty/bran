VERSION=1.2.0
set -x
set -e
if [ ! -e /home/ubuntu/opam/$VERSION/bin/opam ]; then
    curl -L -O https://github.com/ocaml/opam/releases/download/$VERSION/opam-full-$VERSION.tar.gz
    tar xvfz opam-full-$VERSION.tar.gz
    cd opam-full-$VERSION
    ./configure --prefix=/home/ubuntu/opam/$VERSION --bindir=/home/ubuntu/bin
    make lib-ext
    make
    make install

    /home/ubuntu/opam/$VERSION/bin/opam init -a
    /home/ubuntu/opam/$VERSION/bin/opam install menhir -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install omake -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install spotlib -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install ounit -y -v
fi
