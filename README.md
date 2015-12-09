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

