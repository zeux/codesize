#pragma once

#include <stdint.h>

// DLL interface
#define API extern "C" __declspec(dllexport) __stdcall

struct BuFile;

API BuFile* buOpen(const char* path);
API void buClose(BuFile* file);

struct BuSymtab;

API BuSymtab* buSymtabOpen(BuFile* file);
API void buSymtabClose(BuSymtab* symtab);

struct BuSymbol;

API unsigned int buSymtabGetData(BuSymtab* symtab, BuSymbol* buffer, unsigned int bufferSize);

struct BuSymbol
{
    uint64_t address;
    uint64_t size;
    int type;
    char* name;
};
