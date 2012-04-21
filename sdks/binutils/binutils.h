#pragma once

#include <stdint.h>

// DLL interface
#define API extern "C" __declspec(dllexport) __stdcall

struct BuFile;

API BuFile* buFileOpen(const char* path, int offset);
API void buFileClose(BuFile* file);

struct BuSymtab;

API BuSymtab* buSymtabOpen(BuFile* file);
API void buSymtabClose(BuSymtab* symtab);

struct BuSymbol
{
    uint64_t address;
    uint64_t size;
    int type;
    const char* name;
};

API unsigned int buSymtabGetData(BuSymtab* symtab, BuSymbol* buffer, unsigned int bufferSize);

struct BuLinetab;

API BuLinetab* buLinetabOpen(BuFile* file);
API void buLinetabClose(BuLinetab* linetab);

struct BuLine
{
    uint64_t address;
    unsigned int file;
    unsigned int line;
};

API unsigned int buLinetabGetFiles(BuLinetab* linetab, const char** buffer, unsigned int bufferSize);
API unsigned int buLinetabGetLines(BuLinetab* linetab, BuLine* buffer, unsigned int bufferSize);

API bool buGetFileLine(BuFile* file, uint64_t address, const char** filename, unsigned int* line);
