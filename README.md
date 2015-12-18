# llvm-pgas
LLVM optimizations for PGAS programs

This repository enables an LLVM based compiler to optimize communication
for PGAS programs. PGAS is Partitioned Global Address Space. PGAS
languages and libraries typically support GET and PUT operations on
remote memory. These GET and PUT operations might be supported by
hardware and are conceptually similar to load and store - but for memory
not local to a machine. Using the software in this repository, the GET
and PUT operations will be represented as a kind of load or store so that
LLVM optimizations can apply to remove communication.

# Build and Test Instructions

## Prerequisites

* cmake
* ninja
* make
* LLVM dependencies (e.g. Python, C++11 compiler, etc)

## Prerequisites

```bash

# Download LLVM 3.7
git clone http://llvm.org/git/llvm.git
pushd llvm

git checkout release_37
mkdir -p build/Release
mkdir -p install

cd build/Release/

cmake ../.. -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_INSTALL_PREFIX=`pwd`/../../install -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_INSTALL_UTILS=ON

ninja
ninja install
popd

# Back in the llvm-pgas directory
mkdir -p build
cd build
cmake .. -DLLVM_ROOT=`pwd`/../llvm/install -DLLVM_SRC=`pwd`/../llvm
make
make check
cd ..

```
