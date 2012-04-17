@echo off
setlocal
set PATH=C:\MinGW64\bin;%PATH%
g++ binutils.cpp -static-libgcc -shared -m32 -lbfd -liberty -std=c++0x -o binutils.dll -Wl,--kill-at -s
