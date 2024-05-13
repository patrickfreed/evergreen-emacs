#!/bin/bash

set -o errexit

if [ -z "${1+x}" ]; then
    echo "version string must be specified as command line argument"
    exit 1
fi

VERSION=${1}
SOURCE_DIR="evg-${VERSION}"

if [ -d "$SOURCE_DIR" ]; then
    rm -r ${SOURCE_DIR};
fi
cp -r src ${SOURCE_DIR}

tar -cf "evg-${VERSION}.tar" ${SOURCE_DIR}

rm -r ${SOURCE_DIR}
