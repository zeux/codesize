#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>

#include <new>
#include <memory>
#include <vector>
#include <string>
#include <map>

#include <bfd.h>
#include "bfdext.h"

#include "binutils.h"
#include "decodedline.h"

// Symbol extraction
bool keepSymbol(bfd* f, asymbol* sym)
{
    if (sym->flags & BSF_DEBUGGING) return false;
	if (bfd_is_target_special_symbol(f, sym)) return false;

    return true;
}

struct BuSymbolOwn: BuSymbol
{
    std::unique_ptr<char, void (*)(void*)> namestorage;

    BuSymbolOwn(): namestorage(0, free)
    {
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

            char* name = getDemangledName(f, info.name);

            BuSymbolOwn s;
            s.address = info.value;
            s.size = elf ? reinterpret_cast<elf_symbol_type*>(sym)->internal_elf_sym.st_size : 0;

            s.type = info.type;
            s.name = name;

            s.namestorage.reset(name);

            result.push_back(std::move(s));
        }
    }

    return std::move(result);
}

// File/line extraction
struct DecodedLineVMExtractor: DecodedLineVM
{
    std::vector<std::string> dirtab;
    std::vector<unsigned int> filetab;

    std::map<std::string, unsigned int> pathcache;

    std::vector<BuLine> lines;

    virtual void resetTables()
    {
        dirtab.clear();
        filetab.clear();
    }

    virtual void addDirectory(const char* path)
    {
        dirtab.push_back(path);
    }

    virtual void addFile(unsigned int directory, const char* name)
    {
        assert(directory < dirtab.size());

        std::string path = dirtab[directory] + "/" + name;
        unsigned int& pci = pathcache[path];

        if (pci == 0) pci = pathcache.size();

        filetab.push_back(pci);
    }

    virtual void addLine(unsigned int file, unsigned int line, uint64_t address)
    {
        assert(file < filetab.size());

        BuLine l;
        l.address = address;
        l.file = filetab[file];
        l.line = line;

        lines.push_back(l);
    }
};

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

struct BuLinetab
{
    std::vector<std::string> files;
    std::vector<BuLine> lines;

    BuLinetab(std::vector<std::string> files, std::vector<BuLine> lines): files(std::move(files)), lines(std::move(lines))
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

    return 0;
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
    for (unsigned int i = 0; i < bufferSize && i < symtab->symbols.size(); ++i)
        buffer[i] = symtab->symbols[i];

    return symtab->symbols.size();
}

void buSymtabClose(BuSymtab* symtab)
{
    delete symtab;
}

BuLinetab* buLinetabOpen(BuFile* file)
{
    DecodedLineVMExtractor vm;
    if (!decodedLineProcess(file->file.get(), &vm)) return 0;

    std::vector<std::string> files(vm.pathcache.size());

    for (auto& p: vm.pathcache)
    {
        assert(p.second > 0);

        // not sure why moving the map key compiles, but it's for the best since it saves reallocations
        files[p.second - 1] = std::move(p.first);
    }

    return new BuLinetab(std::move(files), std::move(vm.lines));
}

void buLinetabClose(BuLinetab* linetab)
{
    delete linetab;
}

API unsigned int buLinetabGetFiles(BuLinetab* linetab, const char** buffer, unsigned int bufferSize)
{
    for (unsigned int i = 0; i < bufferSize && i < linetab->files.size(); ++i)
        buffer[i] = linetab->files[i].c_str();

    return linetab->files.size();
}

API unsigned int buLinetabGetLines(BuLinetab* linetab, BuLine* buffer, unsigned int bufferSize)
{
    for (unsigned int i = 0; i < bufferSize && i < linetab->lines.size(); ++i)
        buffer[i] = linetab->lines[i];

    return linetab->lines.size();
}
