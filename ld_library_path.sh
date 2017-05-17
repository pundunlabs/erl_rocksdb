#!/bin/bash

OS=`uname -s`
SCRIPT=$(readlink -f "$0")
BASEDIR=$(dirname "$SCRIPT")
if [ "$OS" == "Darwin" ] ; then
  export DYLD_LIBRARY_PATH=$BASEDIR/c_src/rocksdb:$DYLD_LIBRARY_PATH
else
  export LD_LIBRARY_PATH=$BASEDIR/c_src/rocksdb:$LD_LIBRARY_PATH
fi
echo $LD_LIBRARY_PATH
$*
