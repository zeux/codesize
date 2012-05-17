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
#include <algorithm>

#include <objbase.h>

#include <bfd.h>
#include "bfdext.h"

#include "binutils.h"
#include "decodedline.h"

// Helper functions
template <typename T> T* pbegin(std::vector<T>& v)
{
    return v.empty() ? nullptr : &v[0];
}

// Symbol extraction
std::vector<asymbol*> slurpSymtab(bfd* abfd)
{
    long storage_needed = bfd_get_symtab_upper_bound (abfd);

    std::vector<asymbol*> res(storage_needed);

    if (storage_needed)
    {
        long symcount = bfd_canonicalize_symtab (abfd, &res[0]);
        res.resize(symcount);
    }

    return std::move(res);
}

struct BuSymbolOwn: BuSymbol
{
    std::unique_ptr<char, void (*)(void*)> namestorage;

    BuSymbolOwn(): namestorage(0, free)
    {
    }
};

char* getDemangledNameRaw(bfd* abfd, const char* name)
{
    char* result = bfd_demangle(abfd, name, DMGL_PARAMS | DMGL_ANSI | DMGL_RET_POSTFIX);

    return result ? result : strdup(name);
}

char* concatStrings(const char* lhs, const char* rhs)
{
    size_t ll = strlen(lhs);
    size_t rl = strlen(rhs);

    char* result = static_cast<char*>(malloc(ll + rl + 1));
    if (!result) return 0;

    memcpy(result, lhs, ll);
    memcpy(result + ll, rhs, rl);
    result[ll + rl] = 0;

    return result;
}

bool isDigit(char ch)
{
    return ch >= '0' && ch <= '9';
}

const char* findRodataSuffix(const char* name)
{
    size_t length = strlen(name);

    // $rodata suffix
    if (length > 7 && strcmp(name + length - 7, "$rodata") == 0)
        return name + length - 7;

    // .\d+ suffix
    size_t dotpos = length;

    for (; dotpos > 0; --dotpos)
    {
        char ch = name[dotpos - 1];

        if (ch == '.')
            return name + dotpos - 1;
        else if (!isDigit(ch))
            break;
    }

    return 0;
}

// prefix characters that have special meaning:
// * - \d*
// + - \d+
const char* stripPrefixWild(const char* name, const char* prefix)
{
    const char* s = name;

    for (; *prefix; prefix++)
    {
        if (*prefix == '*')
        {
            while (isDigit(*s)) s++;
        }
        else if (*prefix == '+')
        {
            if (!isDigit(*s)) return name;
            do s++;
            while (isDigit(*s));
        }
        else
        {
            if (*s++ != *prefix) return name;
        }
    }

    return s;
}

const char* getStripPrefix(const char* name)
{
    if (name[0] == '.' && name[1] == 'L')
    {
        switch (name[2])
        {
        case 'N': return ".LNst*.";
        case 'f': return ".Lfi*.";
        case 'v': return ".Lvi*.";
        case 'l': return ".Llli*.";
        }
    }

    return 0;
}

char* getDemangledName(bfd* abfd, const char* name)
{
    // strip ./$ prefix
    if (name[0] == '.' || name[0] == '$') name++;

    // strip various known symbol prefixes
    if (const char* strip = getStripPrefix(name))
    {
        name = stripPrefixWild(name, strip);
    }

    // strip /... prefix
    if (name[0] == '/')
    {
        const char* dot = strchr(name, '.');
        if (dot) name = dot + 1;
    }

    // strip $rodata suffix before demangling
    if (const char* rodata = findRodataSuffix(name))
    {
        std::string temp(name, rodata);

        char* rawname = getDemangledNameRaw(abfd, temp.c_str());
        char* result = rawname ? concatStrings(rawname, " rodata") : 0;

        free(rawname);

        return result;
    }
    else
        return getDemangledNameRaw(abfd, name);
}

bool keepSymbol(bfd* abfd, asymbol* sym)
{
    if (sym->flags & BSF_DEBUGGING) return false;
	if (bfd_is_target_special_symbol(abfd, sym)) return false;
    if (strncmp(bfd_asymbol_name(sym), ".LANCHOR", 8) == 0) return false;

    return true;
}

bfd_vma getSymbolSize(bfd* abfd, asymbol* sym)
{
    return (bfd_get_flavour(abfd) == bfd_target_elf_flavour) ? reinterpret_cast<elf_symbol_type*>(sym)->internal_elf_sym.st_size : 0;
}

bfd_vma getSymbolPaddedSize(bfd* abfd, asymbol* sym, asymbol* next)
{
    bfd_section* sec = bfd_get_section(sym);

    // If next symbol is from the same section, size is considered to be the address difference
    // Otherwise, symbol is assumed to occupy the rest of the section
    // Exception to the rule is when symbol VMA is not in the section (this can happen for TLS symbols)
    // Then just fall back to the ELF-specified size
    return
        (next && sec == bfd_get_section(next) && next->value < sec->size) ?
            bfd_asymbol_value(next) - bfd_asymbol_value(sym) :
            sym->value < sec->size ?
                sec->size - sym->value :
                getSymbolSize(abfd, sym);
}

std::vector<asymbol*> getSymbolsSortedFiltered(bfd* abfd, asymbol** symtab, size_t symcount)
{
    std::vector<asymbol*> result;
    result.reserve(symcount);

    for (size_t i = 0; i < symcount; ++i)
    {
        asymbol* sym = symtab[i];
        assert(sym);

        if (keepSymbol(abfd, sym))
            result.push_back(sym);
    }

    // sort by section, then by offset within section, then by size (reverse) - that way larger symbol survives after unique()
    std::sort(result.begin(), result.end(), [=](asymbol* lhs, asymbol* rhs) {
        return lhs->section != rhs->section ?
            lhs->section < rhs->section :
                lhs->value != rhs->value ?
                    lhs->value < rhs->value :
                    getSymbolSize(abfd, lhs) > getSymbolSize(abfd, rhs); });

    // remove duplicate symbols
    result.erase(
        std::unique(result.begin(), result.end(), [](asymbol* lhs, asymbol* rhs) { return lhs->section == rhs->section && lhs->value == rhs->value; }),
        result.end());

    return std::move(result);
}

std::vector<BuSymbolOwn> getSymbols(bfd* abfd, asymbol** symtab, size_t symcount)
{
    std::vector<asymbol*> symbols = getSymbolsSortedFiltered(abfd, symtab, symcount);

    std::vector<BuSymbolOwn> result(symbols.size());

    for (size_t i = 0; i < symbols.size(); ++i)
    {
        asymbol* sym = symbols[i];

        symbol_info info;
        bfd_get_symbol_info(abfd, sym, &info);

        char* name = getDemangledName(abfd, info.name);

        BuSymbolOwn s;
        s.address = info.value;
        s.size = getSymbolPaddedSize(abfd, sym, (i + 1 < symbols.size()) ? symbols[i + 1] : nullptr);

        s.type = info.type;
        s.name = name;

        s.namestorage.reset(name);

        result[i] = std::move(s);
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

        auto it = pathcache.insert(std::make_pair(std::move(path), pathcache.size()));

        filetab.push_back(it.first->second);
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
    std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> abfd;
    std::vector<asymbol*> symtab;

    BuFile(std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> abfd, std::vector<asymbol*> symtab): abfd(std::move(abfd)), symtab(std::move(symtab))
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

BuFile* buFileOpen(const char* path, bool preload, int offset)
{
    iovecStreamParams p = {path, offset};

    // open file
    std::unique_ptr<bfd, bfd_boolean (*)(bfd*)> abfd(bfd_openr_iovec(path, 0, iovecOpen, &p, iovecRead, iovecClose, iovecStat), bfd_close);
    if (!abfd) return 0;
    if (!bfd_check_format(abfd.get(), bfd_object)) return 0;

    // decompress sections (we don't know if we'll need it)
    abfd->flags |= BFD_DECOMPRESS;

    // slurp symtab since all operations need it anyway
    std::vector<asymbol*> symtab = slurpSymtab(abfd.get());

    return new BuFile(std::move(abfd), std::move(symtab));
}

void buFileClose(BuFile* file)
{
    delete file;
}

BuSymtab* buSymtabOpen(BuFile* file)
{
    std::vector<BuSymbolOwn> symbols = getSymbols(file->abfd.get(), pbegin(file->symtab), file->symtab.size());

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
    if (!decodedLineProcess(file->abfd.get(), pbegin(file->symtab), &vm)) return 0;

    std::vector<std::string> files(vm.pathcache.size());

    for (auto& p: vm.pathcache)
    {
        // not sure why moving the map key compiles, but it's for the best since it saves reallocations
        files[p.second] = std::move(p.first);
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

struct GetFileLineContext
{
    bool found;
    uint64_t address;
    const char* filename;
    const char* functionname;
    unsigned int line;

    asymbol** syms;

    static void find_address_in_section (bfd *abfd, asection *section, void *data)
    {
        GetFileLineContext* ctx = static_cast<GetFileLineContext*>(data);

        if (ctx->found) return;

        uint64_t pc = ctx->address;
        
        if ((bfd_get_section_flags (abfd, section) & SEC_ALLOC) == 0)
            return;

        bfd_vma vma = bfd_get_section_vma (abfd, section);
        if (pc < vma)
            return;

        bfd_size_type size = bfd_get_section_size (section);
        if (pc >= vma + size)
            return;

        ctx->found = bfd_find_nearest_line (abfd, section, ctx->syms, pc - vma, &ctx->filename, &ctx->functionname, &ctx->line);
    }
};

char* CoTaskStrDup(const char* str)
{
    if (!str) return 0;

    size_t size = strlen(str) + 1;
    void* memory = CoTaskMemAlloc(size);

    if (memory) memcpy(memory, str, size);

    return static_cast<char*>(memory);
}

API bool buGetFileLine(BuFile* file, uint64_t address, const char** filename, unsigned int* line)
{
    GetFileLineContext context = {false, address, nullptr, nullptr, 0, pbegin(file->symtab)};

    bfd_map_over_sections(file->abfd.get(), GetFileLineContext::find_address_in_section, &context);

    *filename = CoTaskStrDup(context.filename);
    *line = context.line;

    return context.found;
}
