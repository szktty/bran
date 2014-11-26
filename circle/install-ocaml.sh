VERSION=$VERSION
DISTDIR=ocaml-4.02
set -x
set -e
if [ ! -e /home/ubuntu/ocaml/$VERSION/bin/ocaml ]; then
    curl -O http://caml.inria.fr/pub/distrib/$DISTDIR/ocaml-$VERSION.tar.gz
    tar xvfz ocaml-$VERSION.tar.gz
    cd ocaml-$VERSION
    sed -i -e "s/^prefix=\/usr\/local/prefix=\/home\/ubuntu\/ocaml\/$VERSION/g" configure
    ./configure
    make world.opt
    make install
fi
