#!/bin/sh

#opam switch 4.02.1
#eval `opam config env`
rm -rf bran bran-develop.tar.gz
git clone https://github.com/szktty/bran.git
cd bran
omake --project
omake --project test && rm -rf .git .gitignore devbuild.sh circle.yml circle src test OMake* lib/OMake* liberl/OMake* && tar czf ../bran-develop.tar.gz .
cd ..
