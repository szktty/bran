#!/bin/sh

ZIP=bran-develop-`uname -s`-`uname -m`.tar.gz

#opam switch 4.02.1
#eval `opam config env`
rm -rf bran $ZIP
git clone https://github.com/szktty/bran.git
cd bran
omake --project
omake --project test && rm -rf .omakedb .omakedb.lock devbuild.sh circle.yml circle src test OMake* lib/OMake* liberl/OMake* && tar czf ../$ZIP .
cd ..
