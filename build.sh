#!/bin/sh
ROCKSDB_DIR=c_src/rocksdb
ROCKSDB_TAG=v5.6.1

if [ ! -d ${ROCKSDB_DIR} ]; then
    # Control will enter here if rocksdb doesn't exist.
    (cd c_src && git clone https://github.com/pundunlabs/rocksdb.git && cd rocksdb && git checkout tags/$ROCKSDB_TAG -b $ROCKSDB_TAG)
fi
export INSTALL_PATH="."
export CXXFLAGS=-fPIC
(cd ${ROCKSDB_DIR} && \
    make libzstd.a && \
    make libsnappy.a && \
    make libbz2.a && \
    make libz.a && \
    make liblz4.a && \
    make -j 4 static_lib)
