VERSION=1.2.0
set -x
set -e
if [ ! -e /home/ubuntu/opam/$VERSION/bin/opam ]; then
    curl -O http://www.ocamlpro.com/pub/opam-full-$VERSION.tar.gz
    tar xvfz opam-full-$VERSION.tar.gz
    cd opam-full-$VERSION
    ./configure --prefix=/home/ubuntu/opam/$VERSION --bindir=/home/ubuntu/bin
    make
    make install

    /home/ubuntu/opam/$VERSION/bin/opam init -a
    /home/ubuntu/opam/$VERSION/bin/opam install menhir -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install omake -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install spotlib -y -v
    /home/ubuntu/opam/$VERSION/bin/opam install ounit -y -v
fi
