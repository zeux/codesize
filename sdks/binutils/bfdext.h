#pragma once

// Demangle options, from demangle.h
#define DMGL_PARAMS	 (1 << 0)
#define DMGL_ANSI	 (1 << 1)
#define DMGL_RET_POSTFIX (1 << 5)
#define DMGL_RET_DROP	 (1 << 6) // binutils 2.22 required

// Elf_Internal_Sym from elf/internal.h
struct elf_internal_sym {
  bfd_vma	st_value;		/* Value of the symbol */
  bfd_vma	st_size;		/* Associated symbol size */
  unsigned long	st_name;		/* Symbol name, index in string tbl */
  unsigned char	st_info;		/* Type and binding attributes */
  unsigned char	st_other;		/* Visibilty, and target specific */
  unsigned char st_target_internal;	/* Internal-only information */
  unsigned int  st_shndx;		/* Associated section index */
};

typedef struct elf_internal_sym Elf_Internal_Sym;

// elf_symbol_type from elf-bfd.h
typedef struct
{
  /* The BFD symbol.  */
  asymbol symbol;
  /* ELF symbol information.  */
  Elf_Internal_Sym internal_elf_sym;
  /* Backend specific information.  */
  union
    {
      unsigned int hppa_arg_reloc;
      void *mips_extr;
      void *any;
    }
  tc_data;

  /* Version information.  This is from an Elf_Internal_Versym
     structure in a SHT_GNU_versym section.  It is zero if there is no
     version information.  */
  unsigned short version;

} elf_symbol_type;
