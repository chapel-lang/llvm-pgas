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

## Quick Commands

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

# Description of LLVM optimizations

Communication optimization within LLVM uses the address space feature of LLVM
in order to create a conceptual global address space. In particular, instead of
generating a call to the runtime functions to 'put' or 'get', when PGAS LLVM
optimizations are enabled, a PGAS language compiler will generate a load,
store, or memcpy using an address space 100 pointer. Address space 100 pointers
represent global memory - and address space 0 pointers continue to represent
local memory. The existing LLVM optimization passes will operate normally on
these address space 100 operations. The LLVM documentation describes these
optimizations and which are normally run.

Because it may be necessary to build a global pointer or to gather information
from it - for example when constructing a global pointer from a node number and
a local address, or extracting the node number or the address - the LLVM code
generated for PGAS LLVM optimizations can include calls to nonexistent
functions to mark these operations:

* .gf.addr extracts an address from a global pointer
* .gf.loc extracts a locale from a global pointer
* .gf.node extracts a node number from a global pointer
* .gf.make constructs a global pointer from a locale and an address
* .gf.g2w converts a global pointer to a wide pointer
* .gf.w2g converts a wide pointer to a global pointer

These functions will be replaced with the usual runtime functions once all
global pointers are lowered into wide pointers by the global-to-wide pass.

After the usual LLVM optimization passes run, two PGAS LLVM passes run:

* aggregate-global-ops bundles together sequences of loads or sequences of
  stores on adjacent global memory locations into a single memcpy. That way,
  adjacent loads will generate a single 'get' instead of several 'get' calls.
  Without this pass, other LLVM passes might change memcpy into several
  sepaarte loads and stores, for example.

* global-to-wide converts operations on address space 100 pointers, notably
  including load, store, memcpy, and memset operations, into calls to the PGAS
  language runtime. It converts address space 100 pointers into packed pointers
  and any of the special function calls (e.g. .gf.addr to extract the local
  address portion of a global pointer) into the usual operations on a packed
  pointer. In the future, we would like to support converting address space 100
  pointers into 128-bit structure-based wide pointers.

