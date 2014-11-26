VERSION=17.3
set -x
set -e
if [ ! -e otp_src_$VERSION/bin/erl ]; then
    curl -O http://www.erlang.org/download/otp_src_$VERSION.tar.gz
    tar xzf otp_src_$VERSION.tar.gz
    cd otp_src_$VERSION
    ./configure \
        --prefix=/home/ubuntu/otp/$VERSION \
        --bindir=/home/ubuntu/bin \
        --enable-smp-support \
        --enable-m64-build \
        --disable-native-libs \
        --disable-sctp \
        --enable-threads \
        --enable-kernel-poll \
        --disable-hipe \
        --without-javac
    make
    make install
fi
