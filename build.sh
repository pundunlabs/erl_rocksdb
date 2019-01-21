#!/bin/sh
ROCKSDB_DIR=c_src/rocksdb
ROCKSDB_TAG=v5.17.2

if [ ! -d ${ROCKSDB_DIR} ]; then
    # Control will enter here if rocksdb doesn't exist.
    (cd c_src && git clone https://github.com/pundunlabs/rocksdb.git && cd rocksdb && git checkout tags/$ROCKSDB_TAG -b $ROCKSDB_TAG)
fi
export INSTALL_PATH="."
export CXXFLAGS=-fPIC

# building libraries under rocksdb checks sha256 checksum using openssl
# set working opnessl first in path
if [ -z "$(echo "test text" | openssl sha256 2>/dev/null)" ]; then
    for p in ${PATH//:/ }; do
	if [ -e $p/openssl ]; then
	    if [ ! -z "$(echo "test text" | $p/openssl sha256 2>/dev/null)" ]; then
		export PATH=$p:$PATH;
	    fi
	fi
    done
fi

# disable BZIP since building the static lib for support got harder
# when bzip.org got discontinued
export ROCKSDB_DISABLE_BZIP=1

(cd ${ROCKSDB_DIR} && \
    make -j 4 libzstd.a && \
    make -j 4 libsnappy.a && \
    make -j 4 libz.a && \
    make -j 4 liblz4.a && \
    make -j 4 static_lib)
