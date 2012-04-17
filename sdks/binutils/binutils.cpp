#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <new>
#include <memory>
#include <vector>

#include <bfd.h>
#include "bfdext.h"

#include "binutils.h"

// Symbol extraction
bool keepSymbol(bfd* f, asymbol* sym)
{
    if (sym->flags & BSF_DEBUGGING) return false;
	if (bfd_is_target_special_symbol(f, sym)) return false;

    return true;
}

struct BuSymbolOwn: BuSymbol
{
    BuSymbolOwn(const BuSymbolOwn&) = delete;
    BuSymbolOwn& operator=(const BuSymbolOwn&) = delete;

    BuSymbolOwn()
    {
        name = 0;
    }
    
    ~BuSymbolOwn()
    {
        free(name);
    }

    BuSymbolOwn(BuSymbolOwn&& other)
    {
        name = 0;
        *this = std::move(other);
    }

    BuSymbolOwn& operator=(BuSymbolOwn&& other)
    {
        free(name);

        address = other.address;
        size = other.size;
        type = other.type;
        name = other.name;
        other.name = 0;
    }
};

char* getDemangledNameRaw(bfd* f, const char* name)
{
    char* result = bfd_demangle(f, name, DMGL_PARAMS | DMGL_ANSI | DMGL_RET_POSTFIX);

    return result ? result : strdup(name);
}

char* getDemangledName(bfd* f, const char* name)
{
    // strip ./$ prefix
    if (name[0] == '.' || name[0] == '$') name++;

    // strip $rodata suffix
    size_t length = strlen(name);

    if (length > 7 && strcmp(name + length - 7, "$rodata") == 0)
    {
        std::unique_ptr<char, void (*)(void*)> temp(strdup(name), free);

        temp.get()[length - 7] = 0;

        return getDemangledNameRaw(f, temp.get());
    }
    else
        return getDemangledNameRaw(f, name);
}

std::vector<BuSymbolOwn> getSymbols(bfd* f)
{
    std::vector<BuSymbolOwn> result;

    void* minisyms;
    unsigned int size;

    long symcount = bfd_read_minisymbols(f, 0, &minisyms, &size);

    if (symcount > 0)
    {
        bool elf = bfd_get_flavour(f) == bfd_target_elf_flavour;
        asymbol* store = bfd_make_empty_symbol(f);

        for (long i = 0; i < symcount; ++i)
        {
            asymbol* sym = bfd_minisymbol_to_symbol(f, 0, static_cast<char*>(minisyms) + i * size, store);
            if (!sym) continue;
            if (!keepSymbol(f, sym)) continue;

            symbol_info info;
            bfd_get_symbol_info(f, sym, &info);

            BuSymbolOwn s;
            s.address = info.value;
            s.size = elf ? reinterpret_cast<elf_symbol_type*>(sym)->internal_elf_sym.st_size : 0;

            s.type = info.type;
            s.name = getDemangledName(f, info.name);

            result.push_back(std::move(s));
        }
    }

    return std::move(result);
}

// DLL implementation
struct BuFile
{
    std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> file;

    BuFile(std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> file): file(std::move(file))
    {
    }
};

struct BuSymtab
{
    std::vector<BuSymbolOwn> symbols;

    BuSymtab(std::vector<BuSymbolOwn> symbols): symbols(std::move(symbols))
    {
    }
};

struct iovecStreamParams
{
    const char* filename;
    int offset;
};

struct iovecStream
{
    FILE* fd;
    int offset;
    struct stat fstat;

    iovecStream(FILE* fd, int offset, struct stat fstat): fd(fd), offset(offset), fstat(fstat)
    {
    }
};

void* iovecOpen(struct bfd *nbfd, void *open_closure)
{
    iovecStreamParams* p = static_cast<iovecStreamParams*>(open_closure);

    struct stat fstat;
    if (stat(p->filename, &fstat)) return 0;

    if (fstat.st_size < p->offset) return 0;
    fstat.st_size -= p->offset;

    FILE* fd = fopen(p->filename, "rb");
    if (!fd) return 0;

    return new iovecStream(fd, p->offset, fstat);
}

file_ptr iovecRead(struct bfd *nbfd, void *stream, void *buf, file_ptr nbytes, file_ptr offset)
{
    iovecStream* s = static_cast<iovecStream*>(stream);

    fseeko64(s->fd, offset + s->offset, SEEK_SET);

    return fread(buf, 1, nbytes, s->fd);
}

int iovecClose(struct bfd *nbfd, void *stream)
{
    iovecStream* s = static_cast<iovecStream*>(stream);

    fclose(s->fd);
    delete s;
}

int iovecStat(struct bfd *abfd, void *stream, struct stat *sb)
{
    iovecStream* s = static_cast<iovecStream*>(stream);

    *sb = s->fstat;
    return 0;
}

BuFile* buOpen(const char* path, int offset)
{
    iovecStreamParams p = {path, offset};

    std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> file(bfd_openr_iovec(path, 0, iovecOpen, &p, iovecRead, iovecClose, iovecStat), bfd_close);
    if (!file) return 0;
    if (!bfd_check_format(file.get(), bfd_object)) return 0;

    return new BuFile(std::move(file));
}

void buClose(BuFile* file)
{
    delete file;
}

BuSymtab* buSymtabOpen(BuFile* file)
{
    std::vector<BuSymbolOwn> symbols = getSymbols(file->file.get());

    return new BuSymtab(std::move(symbols));
}

unsigned int buSymtabGetData(BuSymtab* symtab, BuSymbol* buffer, unsigned int bufferSize)
{
    memset(buffer, 0, sizeof(BuSymbol) * bufferSize);

    if (!symtab) return 0;

    for (unsigned int i = 0; i < bufferSize && i < symtab->symbols.size(); ++i)
        buffer[i] = symtab->symbols[i];

    return symtab->symbols.size();
}

void buSymtabClose(BuSymtab* symtab)
{
    delete symtab;
}

