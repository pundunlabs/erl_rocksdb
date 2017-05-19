#!/bin/sh
ROCKSDB_DIR=c_src/rocksdb
ROCKSDB_TAG=v5.3.5

if [ ! -d ${ROCKSDB_DIR} ]; then
    # Control will enter here if rocksdb doesn't exist.
    #git clone https://github.com/google/rocksdb.git
    (cd c_src && git clone https://github.com/pundunlabs/rocksdb.git && cd rocksdb && git checkout tags/$ROCKSDB_TAG -b $ROCKSDB_TAG)
fi
export INSTALL_PATH="."
export CXXFLAGS=-fPIC
(cd ${ROCKSDB_DIR} && V=1 make -j 4 static_lib)
