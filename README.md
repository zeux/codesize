codesize
--------

codesize is a tool that shows the memory impact of your code using a hierarchical display adapted to work well in large C++ codebases. It works by parsing debug information from PDB/ELF/Mach-O files. The purpose of the tool is to let the developer quickly find areas in the codebase that can be improved to gain memory by reducing code size, which is particularly important on memory-constrained platforms.

codesize runs on Windows and requires .NET Framework 4.0.

License
-------

codesize depends on GNU binutils; because of the dependency it is distributed under the terms of the GNU General Public License.
