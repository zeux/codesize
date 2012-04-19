#include "decodedline.h"

#include <stdlib.h>
#include <string.h>
#include <bfd.h>

#define HOST_WIDEST_INT	long long
#include "dwarf.h"
#include "dwarf2.h"

typedef unsigned HOST_WIDEST_INT elf_vma;

static elf_vma
byte_get_little_endian (unsigned char *field, int size)
{
  switch (size)
    {
    case 1:
      return *field;

    case 2:
      return  ((unsigned int) (field[0]))
	|    (((unsigned int) (field[1])) << 8);

    case 3:
      return  ((unsigned long) (field[0]))
	|    (((unsigned long) (field[1])) << 8)
	|    (((unsigned long) (field[2])) << 16);

    case 4:
      return  ((unsigned long) (field[0]))
	|    (((unsigned long) (field[1])) << 8)
	|    (((unsigned long) (field[2])) << 16)
	|    (((unsigned long) (field[3])) << 24);

    case 8:
      if (sizeof (elf_vma) == 8)
	return  ((elf_vma) (field[0]))
	  |    (((elf_vma) (field[1])) << 8)
	  |    (((elf_vma) (field[2])) << 16)
	  |    (((elf_vma) (field[3])) << 24)
	  |    (((elf_vma) (field[4])) << 32)
	  |    (((elf_vma) (field[5])) << 40)
	  |    (((elf_vma) (field[6])) << 48)
	  |    (((elf_vma) (field[7])) << 56);
      else if (sizeof (elf_vma) == 4)
	/* We want to extract data from an 8 byte wide field and
	   place it into a 4 byte wide field.  Since this is a little
	   endian source we can just use the 4 byte extraction code.  */
	return  ((unsigned long) (field[0]))
	  |    (((unsigned long) (field[1])) << 8)
	  |    (((unsigned long) (field[2])) << 16)
	  |    (((unsigned long) (field[3])) << 24);

    default:
      abort();
    }
}

static elf_vma
byte_get_big_endian (unsigned char *field, int size)
{
  switch (size)
    {
    case 1:
      return *field;

    case 2:
      return ((unsigned int) (field[1])) | (((int) (field[0])) << 8);

    case 3:
      return ((unsigned long) (field[2]))
	|   (((unsigned long) (field[1])) << 8)
	|   (((unsigned long) (field[0])) << 16);

    case 4:
      return ((unsigned long) (field[3]))
	|   (((unsigned long) (field[2])) << 8)
	|   (((unsigned long) (field[1])) << 16)
	|   (((unsigned long) (field[0])) << 24);

    case 8:
      if (sizeof (elf_vma) == 8)
	return ((elf_vma) (field[7]))
	  |   (((elf_vma) (field[6])) << 8)
	  |   (((elf_vma) (field[5])) << 16)
	  |   (((elf_vma) (field[4])) << 24)
	  |   (((elf_vma) (field[3])) << 32)
	  |   (((elf_vma) (field[2])) << 40)
	  |   (((elf_vma) (field[1])) << 48)
	  |   (((elf_vma) (field[0])) << 56);
      else if (sizeof (elf_vma) == 4)
	{
	  /* Although we are extracing data from an 8 byte wide field,
	     we are returning only 4 bytes of data.  */
	  field += 4;
	  return ((unsigned long) (field[3]))
	    |   (((unsigned long) (field[2])) << 8)
	    |   (((unsigned long) (field[1])) << 16)
	    |   (((unsigned long) (field[0])) << 24);
	}

    default:
      abort ();
    }
}

dwarf_vma
read_leb128 (unsigned char *data, unsigned int *length_return, int sign)
{
  dwarf_vma result = 0;
  unsigned int num_read = 0;
  unsigned int shift = 0;
  unsigned char byte;

  do
    {
      byte = *data++;
      num_read++;

      result |= ((dwarf_vma) (byte & 0x7f)) << shift;

      shift += 7;

    }
  while (byte & 0x80);

  if (length_return != NULL)
    *length_return = num_read;

  if (sign && (shift < 8 * sizeof (result)) && (byte & 0x40))
    result |= -1L << shift;

  return result;
}

/* Create a signed version to avoid painful typecasts.  */
static dwarf_signed_vma
read_sleb128 (unsigned char *data, unsigned int *length_return)
{
  return (dwarf_signed_vma) read_leb128 (data, length_return, 1);
}

typedef struct State_Machine_Registers
{
  dwarf_vma address;
  unsigned int file;
  unsigned int line;
  unsigned int column;
  int is_stmt;
  int basic_block;
  unsigned char op_index;
  unsigned char end_sequence;
} SMR;

static void
reset_state_machine (SMR& state_machine_regs, int is_stmt)
{
  state_machine_regs.address = 0;
  state_machine_regs.op_index = 0;
  state_machine_regs.file = 1;
  state_machine_regs.line = 1;
  state_machine_regs.column = 0;
  state_machine_regs.is_stmt = is_stmt;
  state_machine_regs.basic_block = 0;
  state_machine_regs.end_sequence = 0;
}

static bool display_debug_lines_decoded (bfd* abfd, unsigned char *data, bfd_size_type size, DecodedLineVM* vm)
{
    elf_vma (*byte_get) (unsigned char *, int) = bfd_little_endian(abfd) ? byte_get_little_endian : byte_get_big_endian;

    unsigned char* end = data + size;

    while (data < end)
    {
        SMR state_machine_regs;

        /* This loop amounts to one iteration per compilation unit.  */
        DWARF2_Internal_LineInfo linfo;
        unsigned char *standard_opcodes;
        unsigned char *end_of_sequence;
        unsigned char *hdrptr;
        int initial_length_size;
        int offset_size;

        hdrptr = data;

        /* Extract information from the Line Number Program Header.
           (section 6.2.4 in the Dwarf3 doc).  */

        /* Get the length of this CU's line number information block.  */
        linfo.li_length = byte_get (hdrptr, 4);
        hdrptr += 4;

        if (linfo.li_length == 0xffffffff)
        {
            /* This section is 64-bit DWARF 3.  */
            linfo.li_length = byte_get (hdrptr, 8);
            hdrptr += 8;
            offset_size = 8;
            initial_length_size = 12;
        }
        else
        {
            offset_size = 4;
            initial_length_size = 4;
        }

        if (linfo.li_length + initial_length_size > size)
        {
            // The line info appears to be corrupt - the section is too small.
            return false;
        }

        /* Get this CU's Line Number Block version number.  */
        linfo.li_version = byte_get (hdrptr, 2);
        hdrptr += 2;
        if (linfo.li_version != 2
                && linfo.li_version != 3
                && linfo.li_version != 4)
        {
            // Only DWARF version 2, 3 and 4 line info is currently supported.
            return false;
        }

        linfo.li_prologue_length = byte_get (hdrptr, offset_size);
        hdrptr += offset_size;
        linfo.li_min_insn_length = byte_get (hdrptr, 1);
        hdrptr++;
        if (linfo.li_version >= 4)
        {
            linfo.li_max_ops_per_insn = byte_get (hdrptr, 1);
            hdrptr++;
            if (linfo.li_max_ops_per_insn == 0)
            {
                // Invalid maximum operations per insn.
                return false;
            }
        }
        else
            linfo.li_max_ops_per_insn = 1;
        linfo.li_default_is_stmt = byte_get (hdrptr, 1);
        hdrptr++;
        linfo.li_line_base = byte_get (hdrptr, 1);
        hdrptr++;
        linfo.li_line_range = byte_get (hdrptr, 1);
        hdrptr++;
        linfo.li_opcode_base = byte_get (hdrptr, 1);
        hdrptr++;

        /* Sign extend the line base field.  */
        linfo.li_line_base <<= 24;
        linfo.li_line_base >>= 24;

        /* Find the end of this CU's Line Number Information Block.  */
        end_of_sequence = data + linfo.li_length + initial_length_size;

        reset_state_machine (state_machine_regs, linfo.li_default_is_stmt);

        /* Save a pointer to the contents of the Opcodes table.  */
        standard_opcodes = hdrptr;

        // VM: reset tables
        vm->resetTables();
        vm->addDirectory(".");

        /* Traverse the Directory table to save the directories.  */
        data = standard_opcodes + linfo.li_opcode_base - 1;

        while (*data != 0)
        {
            vm->addDirectory((char *)data);
            data += strlen ((char *) data) + 1;
        }
        
        /* Skip the NUL at the end of the table.  */
        data++;

        /* Traverse the File Name table to save the strings. */
        while (*data != 0)
        {
            unsigned int bytes_read;

            char* name = (char*) data;
            data += strlen ((char *) data) + 1;
            unsigned int directory_index = read_leb128 (data, & bytes_read, 0);
            data += bytes_read;
            read_leb128 (data, & bytes_read, 0);
            data += bytes_read;
            read_leb128 (data, & bytes_read, 0);
            data += bytes_read;

            vm->addFile(directory_index, name);
        }

        /* Skip the NUL at the end of the table.  */
        data++;

        /* This loop iterates through the Dwarf Line Number Program.  */
        while (data < end_of_sequence)
        {
            unsigned char op_code;
            int adv;
            unsigned long int uladv;
            unsigned int bytes_read;
            int is_special_opcode = 0;

            op_code = *data++;

            if (op_code >= linfo.li_opcode_base)
            {
                op_code -= linfo.li_opcode_base;
                uladv = (op_code / linfo.li_line_range);
                if (linfo.li_max_ops_per_insn == 1)
                {
                    uladv *= linfo.li_min_insn_length;
                    state_machine_regs.address += uladv;
                }
                else
                {
                    state_machine_regs.address
                        += ((state_machine_regs.op_index + uladv)
                                / linfo.li_max_ops_per_insn)
                        * linfo.li_min_insn_length;
                    state_machine_regs.op_index
                        = (state_machine_regs.op_index + uladv)
                        % linfo.li_max_ops_per_insn;
                }

                adv = (op_code % linfo.li_line_range) + linfo.li_line_base;
                state_machine_regs.line += adv;
                is_special_opcode = 1;
            }
            else switch (op_code)
            {
                case DW_LNS_extended_op:
                    {
                        unsigned int ext_op_code_len;
                        unsigned char ext_op_code;
                        unsigned char *op_code_data = data;

                        ext_op_code_len = read_leb128 (op_code_data, &bytes_read, 0);
                        op_code_data += bytes_read;

                        if (ext_op_code_len == 0)
                        {
                            // warn: Badly formed extended line op encountered!
                            break;
                        }
                        ext_op_code_len += bytes_read;
                        ext_op_code = *op_code_data++;

                        switch (ext_op_code)
                        {
                            case DW_LNE_end_sequence:
                                reset_state_machine (state_machine_regs, linfo.li_default_is_stmt);
                                break;
                            case DW_LNE_set_address:
                                state_machine_regs.address =
                                    byte_get (op_code_data, ext_op_code_len - bytes_read - 1);
                                state_machine_regs.op_index = 0;
                                break;
                            case DW_LNE_define_file:
                                {
                                    unsigned int dir_index = 0;
                                    char* name = (char *) op_code_data;

                                    op_code_data += strlen ((char *) op_code_data) + 1;
                                    dir_index = read_leb128 (op_code_data, & bytes_read, 0);
                                    op_code_data += bytes_read;
                                    read_leb128 (op_code_data, & bytes_read, 0);
                                    op_code_data += bytes_read;
                                    read_leb128 (op_code_data, & bytes_read, 0);

                                    vm->addFile(dir_index, name);
                                    break;
                                }
                            default:
                                // UNKNOWN: length ext_op_code_len - bytes_read
                                break;
                        }
                        data += ext_op_code_len;
                        break;
                    }
                case DW_LNS_copy:
                    break;

                case DW_LNS_advance_pc:
                    uladv = read_leb128 (data, & bytes_read, 0);
                    data += bytes_read;
                    if (linfo.li_max_ops_per_insn == 1)
                    {
                        uladv *= linfo.li_min_insn_length;
                        state_machine_regs.address += uladv;
                    }
                    else
                    {
                        state_machine_regs.address
                            += ((state_machine_regs.op_index + uladv)
                                    / linfo.li_max_ops_per_insn)
                            * linfo.li_min_insn_length;
                        state_machine_regs.op_index
                            = (state_machine_regs.op_index + uladv)
                            % linfo.li_max_ops_per_insn;
                    }
                    break;

                case DW_LNS_advance_line:
                    adv = read_sleb128 (data, & bytes_read);
                    data += bytes_read;
                    state_machine_regs.line += adv;
                    break;

                case DW_LNS_set_file:
                    adv = read_leb128 (data, & bytes_read, 0);
                    data += bytes_read;
                    state_machine_regs.file = adv;
                    break;

                case DW_LNS_set_column:
                    uladv = read_leb128 (data, & bytes_read, 0);
                    data += bytes_read;
                    state_machine_regs.column = uladv;
                    break;

                case DW_LNS_negate_stmt:
                    adv = state_machine_regs.is_stmt;
                    adv = ! adv;
                    state_machine_regs.is_stmt = adv;
                    break;

                case DW_LNS_set_basic_block:
                    state_machine_regs.basic_block = 1;
                    break;

                case DW_LNS_const_add_pc:
                    uladv = ((255 - linfo.li_opcode_base) / linfo.li_line_range);
                    if (linfo.li_max_ops_per_insn == 1)
                    {
                        uladv *= linfo.li_min_insn_length;
                        state_machine_regs.address += uladv;
                    }
                    else
                    {
                        state_machine_regs.address
                            += ((state_machine_regs.op_index + uladv)
                                    / linfo.li_max_ops_per_insn)
                            * linfo.li_min_insn_length;
                        state_machine_regs.op_index
                            = (state_machine_regs.op_index + uladv)
                            % linfo.li_max_ops_per_insn;
                    }
                    break;

                case DW_LNS_fixed_advance_pc:
                    uladv = byte_get (data, 2);
                    data += 2;
                    state_machine_regs.address += uladv;
                    state_machine_regs.op_index = 0;
                    break;

                case DW_LNS_set_prologue_end:
                    break;

                case DW_LNS_set_epilogue_begin:
                    break;

                case DW_LNS_set_isa:
                    uladv = read_leb128 (data, & bytes_read, 0);
                    data += bytes_read;
                    break;

                default:
                    // Unknown opcode
                    break;
            }

            /* Only Special opcodes, DW_LNS_copy and DW_LNE_end_sequence adds a row
               to the DWARF address/line matrix.  */
            if ((is_special_opcode) || (op_code == DW_LNE_end_sequence)
                    || (op_code == DW_LNS_copy))
            {
                vm->addLine(state_machine_regs.file - 1, state_machine_regs.line, state_machine_regs.address);
            }
        }
    }

    return 0;
}

struct DecodedLineProcessContext
{
    DecodedLineVM* vm;
    asymbol** symtab;

    static void dump_dwarf_section (bfd *abfd, asection *sec, void *arg)
    {
        DecodedLineProcessContext* ctx = static_cast<DecodedLineProcessContext*>(arg);

        const char *name = bfd_get_section_name (abfd, sec);

        if (strcmp(name, ".debug_line") == 0 || strcmp(name, ".zdebug_line") == 0)
        {
            bfd_size_type size = bfd_get_section_size (sec);

            unsigned char* start = bfd_simple_get_relocated_section_contents(abfd, sec, 0, ctx->symtab);

            if (start)
            {
                display_debug_lines_decoded (abfd, start, size, ctx->vm);

                free(start);
            }
        }
    }
};

bool decodedLineProcess(bfd* abfd, asymbol** symtab, DecodedLineVM* vm)
{
    DecodedLineProcessContext context = {vm, symtab};
    bfd_map_over_sections(abfd, DecodedLineProcessContext::dump_dwarf_section, &context);

    return true;
}
