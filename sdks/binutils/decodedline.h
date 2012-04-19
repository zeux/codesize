#pragma once

#include <stdint.h>

struct DecodedLineVM
{
    virtual ~DecodedLineVM() {}

    virtual void resetTables() = 0;
    virtual void addDirectory(const char* path) = 0;
    virtual void addFile(unsigned int directory, const char* name) = 0;

    virtual void addLine(unsigned int file, unsigned int line, uint64_t address) = 0;
};

bool decodedLineProcess(struct bfd* abfd, struct bfd_symbol** symtab, DecodedLineVM* vm);
