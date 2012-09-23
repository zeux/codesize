MINGW=C:\MinGW64\bin
BUVER=binutils-2.22.90
BUILD=$(BUVER)-build-$(PLATFORM)

CFLAGS=-O3 -Wall -Wextra -I$(BUVER)/include -I$(BUVER)/bfd -I$(BUILD) -DDEBUGDIR=\"\"
LDFLAGS=-static-libgcc -shared -liberty -lole32 -Wl,--kill-at -s

ifeq ($(PLATFORM),x86)
CFLAGS+=-m32
else
ifeq ($(PLATFORM),x64)
CFLAGS+=-m64
else
$(error Unknown platform)
endif
endif

HEADERS=
HEADERS+=$(BUILD)/bfd.h
HEADERS+=$(BUILD)/bfdver.h
HEADERS+=$(BUILD)/bfd_stdint.h
HEADERS+=$(BUILD)/config.h
HEADERS+=$(BUILD)/elf32-target.h
HEADERS+=$(BUILD)/elf64-target.h
HEADERS+=$(BUILD)/targmatch.h

SOURCES=
# SOURCES+=$(BUVER)/bfd/aix386-core.c
SOURCES+=$(BUVER)/bfd/aix5ppc-core.c
SOURCES+=$(BUVER)/bfd/aout-adobe.c
SOURCES+=$(BUVER)/bfd/aout-arm.c
SOURCES+=$(BUVER)/bfd/aout-cris.c
SOURCES+=$(BUVER)/bfd/aout-ns32k.c
SOURCES+=$(BUVER)/bfd/aout-sparcle.c
SOURCES+=$(BUVER)/bfd/aout-tic30.c
SOURCES+=$(BUVER)/bfd/aout0.c
SOURCES+=$(BUVER)/bfd/aout32.c
SOURCES+=$(BUVER)/bfd/aout64.c
SOURCES+=$(BUVER)/bfd/archive.c
SOURCES+=$(BUVER)/bfd/archive64.c
SOURCES+=$(BUVER)/bfd/archures.c
SOURCES+=$(BUVER)/bfd/armnetbsd.c
SOURCES+=$(BUVER)/bfd/bfd.c
SOURCES+=$(BUVER)/bfd/bfdio.c
SOURCES+=$(BUVER)/bfd/bfdwin.c
SOURCES+=$(BUVER)/bfd/binary.c
SOURCES+=$(BUVER)/bfd/bout.c
SOURCES+=$(BUVER)/bfd/cache.c
SOURCES+=$(BUVER)/bfd/cf-i386lynx.c
SOURCES+=$(BUVER)/bfd/cf-sparclynx.c
SOURCES+=$(BUVER)/bfd/cisco-core.c
SOURCES+=$(BUVER)/bfd/coff-alpha.c
SOURCES+=$(BUVER)/bfd/coff-apollo.c
SOURCES+=$(BUVER)/bfd/coff-arm.c
SOURCES+=$(BUVER)/bfd/coff-aux.c
SOURCES+=$(BUVER)/bfd/coff-go32.c
SOURCES+=$(BUVER)/bfd/coff-h8300.c
SOURCES+=$(BUVER)/bfd/coff-h8500.c
SOURCES+=$(BUVER)/bfd/coff-i386.c
SOURCES+=$(BUVER)/bfd/coff-i860.c
SOURCES+=$(BUVER)/bfd/coff-i960.c
# SOURCES+=$(BUVER)/bfd/coff-ia64.c
SOURCES+=$(BUVER)/bfd/coff-m68k.c
SOURCES+=$(BUVER)/bfd/coff-m88k.c
# SOURCES+=$(BUVER)/bfd/coff-mcore.c
SOURCES+=$(BUVER)/bfd/coff-mips.c
SOURCES+=$(BUVER)/bfd/coff-or32.c
# SOURCES+=$(BUVER)/bfd/coff-ppc.c
SOURCES+=$(BUVER)/bfd/coff-rs6000.c
SOURCES+=$(BUVER)/bfd/coff-sh.c
SOURCES+=$(BUVER)/bfd/coff-sparc.c
SOURCES+=$(BUVER)/bfd/coff-stgo32.c
SOURCES+=$(BUVER)/bfd/coff-svm68k.c
SOURCES+=$(BUVER)/bfd/coff-tic30.c
SOURCES+=$(BUVER)/bfd/coff-tic4x.c
SOURCES+=$(BUVER)/bfd/coff-tic54x.c
SOURCES+=$(BUVER)/bfd/coff-tic80.c
SOURCES+=$(BUVER)/bfd/coff-u68k.c
SOURCES+=$(BUVER)/bfd/coff-w65.c
SOURCES+=$(BUVER)/bfd/coff-we32k.c
SOURCES+=$(BUVER)/bfd/coff-x86_64.c
SOURCES+=$(BUVER)/bfd/coff-z80.c
SOURCES+=$(BUVER)/bfd/coff-z8k.c
SOURCES+=$(BUVER)/bfd/coff64-rs6000.c
SOURCES+=$(BUVER)/bfd/coffgen.c
SOURCES+=$(BUVER)/bfd/cofflink.c
SOURCES+=$(BUVER)/bfd/compress.c
SOURCES+=$(BUVER)/bfd/corefile.c
SOURCES+=$(BUVER)/bfd/cpu-alpha.c
SOURCES+=$(BUVER)/bfd/cpu-arc.c
SOURCES+=$(BUVER)/bfd/cpu-arm.c
SOURCES+=$(BUVER)/bfd/cpu-avr.c
SOURCES+=$(BUVER)/bfd/cpu-bfin.c
SOURCES+=$(BUVER)/bfd/cpu-cr16.c
SOURCES+=$(BUVER)/bfd/cpu-cr16c.c
SOURCES+=$(BUVER)/bfd/cpu-cris.c
SOURCES+=$(BUVER)/bfd/cpu-crx.c
SOURCES+=$(BUVER)/bfd/cpu-d10v.c
SOURCES+=$(BUVER)/bfd/cpu-d30v.c
SOURCES+=$(BUVER)/bfd/cpu-dlx.c
SOURCES+=$(BUVER)/bfd/cpu-epiphany.c
SOURCES+=$(BUVER)/bfd/cpu-fr30.c
SOURCES+=$(BUVER)/bfd/cpu-frv.c
SOURCES+=$(BUVER)/bfd/cpu-h8300.c
SOURCES+=$(BUVER)/bfd/cpu-h8500.c
SOURCES+=$(BUVER)/bfd/cpu-hppa.c
SOURCES+=$(BUVER)/bfd/cpu-i370.c
SOURCES+=$(BUVER)/bfd/cpu-i386.c
SOURCES+=$(BUVER)/bfd/cpu-i860.c
SOURCES+=$(BUVER)/bfd/cpu-i960.c
SOURCES+=$(BUVER)/bfd/cpu-ia64.c
SOURCES+=$(BUVER)/bfd/cpu-ia64-opc.c
SOURCES+=$(BUVER)/bfd/cpu-ip2k.c
SOURCES+=$(BUVER)/bfd/cpu-iq2000.c
SOURCES+=$(BUVER)/bfd/cpu-k1om.c
SOURCES+=$(BUVER)/bfd/cpu-l1om.c
SOURCES+=$(BUVER)/bfd/cpu-lm32.c
SOURCES+=$(BUVER)/bfd/cpu-m10200.c
SOURCES+=$(BUVER)/bfd/cpu-m10300.c
SOURCES+=$(BUVER)/bfd/cpu-m32c.c
SOURCES+=$(BUVER)/bfd/cpu-m32r.c
SOURCES+=$(BUVER)/bfd/cpu-m68hc11.c
SOURCES+=$(BUVER)/bfd/cpu-m68hc12.c
SOURCES+=$(BUVER)/bfd/cpu-m68k.c
SOURCES+=$(BUVER)/bfd/cpu-m88k.c
SOURCES+=$(BUVER)/bfd/cpu-m9s12x.c
SOURCES+=$(BUVER)/bfd/cpu-m9s12xg.c
SOURCES+=$(BUVER)/bfd/cpu-mcore.c
SOURCES+=$(BUVER)/bfd/cpu-mep.c
SOURCES+=$(BUVER)/bfd/cpu-microblaze.c
SOURCES+=$(BUVER)/bfd/cpu-mips.c
SOURCES+=$(BUVER)/bfd/cpu-mmix.c
SOURCES+=$(BUVER)/bfd/cpu-moxie.c
SOURCES+=$(BUVER)/bfd/cpu-msp430.c
SOURCES+=$(BUVER)/bfd/cpu-mt.c
SOURCES+=$(BUVER)/bfd/cpu-ns32k.c
SOURCES+=$(BUVER)/bfd/cpu-openrisc.c
SOURCES+=$(BUVER)/bfd/cpu-or32.c
SOURCES+=$(BUVER)/bfd/cpu-pdp11.c
SOURCES+=$(BUVER)/bfd/cpu-pj.c
SOURCES+=$(BUVER)/bfd/cpu-plugin.c
SOURCES+=$(BUVER)/bfd/cpu-powerpc.c
SOURCES+=$(BUVER)/bfd/cpu-rl78.c
SOURCES+=$(BUVER)/bfd/cpu-rs6000.c
SOURCES+=$(BUVER)/bfd/cpu-rx.c
SOURCES+=$(BUVER)/bfd/cpu-s390.c
SOURCES+=$(BUVER)/bfd/cpu-score.c
SOURCES+=$(BUVER)/bfd/cpu-sh.c
SOURCES+=$(BUVER)/bfd/cpu-sparc.c
SOURCES+=$(BUVER)/bfd/cpu-spu.c
SOURCES+=$(BUVER)/bfd/cpu-tic30.c
SOURCES+=$(BUVER)/bfd/cpu-tic4x.c
SOURCES+=$(BUVER)/bfd/cpu-tic54x.c
SOURCES+=$(BUVER)/bfd/cpu-tic6x.c
SOURCES+=$(BUVER)/bfd/cpu-tic80.c
SOURCES+=$(BUVER)/bfd/cpu-tilegx.c
SOURCES+=$(BUVER)/bfd/cpu-tilepro.c
SOURCES+=$(BUVER)/bfd/cpu-v850.c
SOURCES+=$(BUVER)/bfd/cpu-vax.c
SOURCES+=$(BUVER)/bfd/cpu-w65.c
SOURCES+=$(BUVER)/bfd/cpu-we32k.c
SOURCES+=$(BUVER)/bfd/cpu-xc16x.c
SOURCES+=$(BUVER)/bfd/cpu-xgate.c
SOURCES+=$(BUVER)/bfd/cpu-xstormy16.c
SOURCES+=$(BUVER)/bfd/cpu-xtensa.c
SOURCES+=$(BUVER)/bfd/cpu-z80.c
SOURCES+=$(BUVER)/bfd/cpu-z8k.c
SOURCES+=$(BUVER)/bfd/demo64.c
SOURCES+=$(BUVER)/bfd/dwarf1.c
SOURCES+=$(BUVER)/bfd/dwarf2.c
SOURCES+=$(BUVER)/bfd/ecoff.c
SOURCES+=$(BUVER)/bfd/ecofflink.c
SOURCES+=$(BUVER)/bfd/elf.c
SOURCES+=$(BUVER)/bfd/elf-attrs.c
SOURCES+=$(BUVER)/bfd/elf-eh-frame.c
SOURCES+=$(BUVER)/bfd/elf-ifunc.c
SOURCES+=$(BUVER)/bfd/elf-m10200.c
SOURCES+=$(BUVER)/bfd/elf-m10300.c
SOURCES+=$(BUVER)/bfd/elf-nacl.c
SOURCES+=$(BUVER)/bfd/elf-strtab.c
SOURCES+=$(BUVER)/bfd/elf-vxworks.c
SOURCES+=$(BUVER)/bfd/elf32.c
SOURCES+=$(BUVER)/bfd/elf32-am33lin.c
SOURCES+=$(BUVER)/bfd/elf32-arc.c
SOURCES+=$(BUVER)/bfd/elf32-arm.c
SOURCES+=$(BUVER)/bfd/elf32-avr.c
SOURCES+=$(BUVER)/bfd/elf32-bfin.c
SOURCES+=$(BUVER)/bfd/elf32-cr16.c
SOURCES+=$(BUVER)/bfd/elf32-cr16c.c
SOURCES+=$(BUVER)/bfd/elf32-cris.c
SOURCES+=$(BUVER)/bfd/elf32-crx.c
SOURCES+=$(BUVER)/bfd/elf32-d10v.c
SOURCES+=$(BUVER)/bfd/elf32-d30v.c
SOURCES+=$(BUVER)/bfd/elf32-dlx.c
SOURCES+=$(BUVER)/bfd/elf32-epiphany.c
SOURCES+=$(BUVER)/bfd/elf32-fr30.c
SOURCES+=$(BUVER)/bfd/elf32-frv.c
SOURCES+=$(BUVER)/bfd/elf32-gen.c
SOURCES+=$(BUVER)/bfd/elf32-h8300.c
SOURCES+=$(BUVER)/bfd/elf32-hppa.c
SOURCES+=$(BUVER)/bfd/elf32-i370.c
SOURCES+=$(BUVER)/bfd/elf32-i386.c
SOURCES+=$(BUVER)/bfd/elf32-i860.c
SOURCES+=$(BUVER)/bfd/elf32-i960.c
SOURCES+=$(BUVER)/bfd/elf32-ip2k.c
SOURCES+=$(BUVER)/bfd/elf32-iq2000.c
SOURCES+=$(BUVER)/bfd/elf32-lm32.c
SOURCES+=$(BUVER)/bfd/elf32-m32c.c
SOURCES+=$(BUVER)/bfd/elf32-m32r.c
SOURCES+=$(BUVER)/bfd/elf32-m68hc11.c
SOURCES+=$(BUVER)/bfd/elf32-m68hc12.c
SOURCES+=$(BUVER)/bfd/elf32-m68hc1x.c
SOURCES+=$(BUVER)/bfd/elf32-m68k.c
SOURCES+=$(BUVER)/bfd/elf32-m88k.c
SOURCES+=$(BUVER)/bfd/elf32-mcore.c
SOURCES+=$(BUVER)/bfd/elf32-mep.c
SOURCES+=$(BUVER)/bfd/elf32-microblaze.c
SOURCES+=$(BUVER)/bfd/elf32-mips.c
SOURCES+=$(BUVER)/bfd/elf32-moxie.c
SOURCES+=$(BUVER)/bfd/elf32-msp430.c
SOURCES+=$(BUVER)/bfd/elf32-mt.c
SOURCES+=$(BUVER)/bfd/elf32-openrisc.c
SOURCES+=$(BUVER)/bfd/elf32-or32.c
SOURCES+=$(BUVER)/bfd/elf32-pj.c
SOURCES+=$(BUVER)/bfd/elf32-ppc.c
SOURCES+=$(BUVER)/bfd/elf32-rl78.c
SOURCES+=$(BUVER)/bfd/elf32-rx.c
SOURCES+=$(BUVER)/bfd/elf32-s390.c
SOURCES+=$(BUVER)/bfd/elf32-score.c
SOURCES+=$(BUVER)/bfd/elf32-score7.c
SOURCES+=$(BUVER)/bfd/elf32-sh.c
SOURCES+=$(BUVER)/bfd/elf32-sh-symbian.c
SOURCES+=$(BUVER)/bfd/elf32-sh64.c
SOURCES+=$(BUVER)/bfd/elf32-sh64-com.c
SOURCES+=$(BUVER)/bfd/elf32-sparc.c
SOURCES+=$(BUVER)/bfd/elf32-spu.c
SOURCES+=$(BUVER)/bfd/elf32-tic6x.c
SOURCES+=$(BUVER)/bfd/elf32-tilegx.c
SOURCES+=$(BUVER)/bfd/elf32-tilepro.c
SOURCES+=$(BUVER)/bfd/elf32-v850.c
SOURCES+=$(BUVER)/bfd/elf32-vax.c
SOURCES+=$(BUVER)/bfd/elf32-xc16x.c
SOURCES+=$(BUVER)/bfd/elf32-xgate.c
SOURCES+=$(BUVER)/bfd/elf32-xstormy16.c
SOURCES+=$(BUVER)/bfd/elf32-xtensa.c
SOURCES+=$(BUVER)/bfd/elf64.c
SOURCES+=$(BUVER)/bfd/elf64-alpha.c
SOURCES+=$(BUVER)/bfd/elf64-gen.c
SOURCES+=$(BUVER)/bfd/elf64-hppa.c
SOURCES+=$(BUVER)/bfd/elf64-ia64-vms.c
SOURCES+=$(BUVER)/bfd/elf64-mips.c
SOURCES+=$(BUVER)/bfd/elf64-mmix.c
SOURCES+=$(BUVER)/bfd/elf64-ppc.c
SOURCES+=$(BUVER)/bfd/elf64-s390.c
SOURCES+=$(BUVER)/bfd/elf64-sh64.c
SOURCES+=$(BUVER)/bfd/elf64-sparc.c
SOURCES+=$(BUVER)/bfd/elf64-tilegx.c
SOURCES+=$(BUVER)/bfd/elf64-x86-64.c
SOURCES+=$(BUVER)/bfd/elflink.c
SOURCES+=$(BUVER)/bfd/elfn32-mips.c
SOURCES+=$(BUILD)/elf32-ia64.c
SOURCES+=$(BUILD)/elf64-ia64.c
SOURCES+=$(BUVER)/bfd/elfxx-ia64.c
SOURCES+=$(BUVER)/bfd/elfxx-mips.c
SOURCES+=$(BUVER)/bfd/elfxx-sparc.c
SOURCES+=$(BUVER)/bfd/elfxx-tilegx.c
SOURCES+=$(BUVER)/bfd/epoc-pe-arm.c
SOURCES+=$(BUVER)/bfd/epoc-pei-arm.c
SOURCES+=$(BUVER)/bfd/format.c
# SOURCES+=$(BUVER)/bfd/gen-aout.c
SOURCES+=$(BUVER)/bfd/hash.c
# SOURCES+=$(BUVER)/bfd/host-aout.c
SOURCES+=$(BUVER)/bfd/hp300bsd.c
SOURCES+=$(BUVER)/bfd/hp300hpux.c
SOURCES+=$(BUVER)/bfd/hppabsd-core.c
# SOURCES+=$(BUVER)/bfd/hpux-core.c
SOURCES+=$(BUVER)/bfd/i386aout.c
SOURCES+=$(BUVER)/bfd/i386bsd.c
SOURCES+=$(BUVER)/bfd/i386dynix.c
SOURCES+=$(BUVER)/bfd/i386freebsd.c
SOURCES+=$(BUVER)/bfd/i386linux.c
SOURCES+=$(BUVER)/bfd/i386lynx.c
SOURCES+=$(BUVER)/bfd/i386mach3.c
SOURCES+=$(BUVER)/bfd/i386msdos.c
SOURCES+=$(BUVER)/bfd/i386netbsd.c
SOURCES+=$(BUVER)/bfd/i386os9k.c
SOURCES+=$(BUVER)/bfd/ieee.c
SOURCES+=$(BUVER)/bfd/ihex.c
SOURCES+=$(BUVER)/bfd/init.c
SOURCES+=$(BUVER)/bfd/irix-core.c
SOURCES+=$(BUVER)/bfd/libbfd.c
SOURCES+=$(BUVER)/bfd/linker.c
SOURCES+=$(BUVER)/bfd/lynx-core.c
SOURCES+=$(BUVER)/bfd/m68k4knetbsd.c
SOURCES+=$(BUVER)/bfd/m68klinux.c
SOURCES+=$(BUVER)/bfd/m68knetbsd.c
SOURCES+=$(BUVER)/bfd/m88kmach3.c
SOURCES+=$(BUVER)/bfd/m88kopenbsd.c
SOURCES+=$(BUVER)/bfd/mach-o.c
SOURCES+=$(BUVER)/bfd/mach-o-i386.c
# SOURCES+=$(BUVER)/bfd/mach-o-target.c
SOURCES+=$(BUVER)/bfd/mach-o-x86-64.c
SOURCES+=$(BUVER)/bfd/merge.c
SOURCES+=$(BUVER)/bfd/mipsbsd.c
SOURCES+=$(BUVER)/bfd/mmo.c
# SOURCES+=$(BUVER)/bfd/netbsd-core.c
SOURCES+=$(BUVER)/bfd/newsos3.c
SOURCES+=$(BUVER)/bfd/nlm.c
SOURCES+=$(BUVER)/bfd/nlm32.c
SOURCES+=$(BUVER)/bfd/nlm32-alpha.c
SOURCES+=$(BUVER)/bfd/nlm32-i386.c
SOURCES+=$(BUVER)/bfd/nlm32-ppc.c
SOURCES+=$(BUVER)/bfd/nlm32-sparc.c
SOURCES+=$(BUVER)/bfd/nlm64.c
SOURCES+=$(BUVER)/bfd/ns32knetbsd.c
SOURCES+=$(BUVER)/bfd/oasys.c
SOURCES+=$(BUVER)/bfd/opncls.c
# SOURCES+=$(BUVER)/bfd/osf-core.c
SOURCES+=$(BUVER)/bfd/pc532-mach.c
SOURCES+=$(BUVER)/bfd/pdp11.c
SOURCES+=$(BUVER)/bfd/pe-arm.c
SOURCES+=$(BUVER)/bfd/pe-arm-wince.c
SOURCES+=$(BUVER)/bfd/pe-i386.c
SOURCES+=$(BUVER)/bfd/pe-mcore.c
SOURCES+=$(BUVER)/bfd/pe-mips.c
SOURCES+=$(BUVER)/bfd/pe-ppc.c
SOURCES+=$(BUVER)/bfd/pe-sh.c
SOURCES+=$(BUVER)/bfd/pe-x86_64.c
SOURCES+=$(BUVER)/bfd/pef.c
SOURCES+=$(BUVER)/bfd/pei-arm.c
SOURCES+=$(BUVER)/bfd/pei-arm-wince.c
SOURCES+=$(BUVER)/bfd/pei-i386.c
SOURCES+=$(BUVER)/bfd/pei-ia64.c
SOURCES+=$(BUVER)/bfd/pei-mcore.c
SOURCES+=$(BUVER)/bfd/pei-mips.c
SOURCES+=$(BUVER)/bfd/pei-ppc.c
SOURCES+=$(BUVER)/bfd/pei-sh.c
SOURCES+=$(BUVER)/bfd/pei-x86_64.c
SOURCES+=$(BUILD)/peigen.c
SOURCES+=$(BUILD)/pepigen.c
SOURCES+=$(BUILD)/pex64igen.c
SOURCES+=$(BUVER)/bfd/plugin.c
SOURCES+=$(BUVER)/bfd/ppcboot.c
SOURCES+=$(BUVER)/bfd/ptrace-core.c
SOURCES+=$(BUVER)/bfd/reloc.c
SOURCES+=$(BUVER)/bfd/reloc16.c
SOURCES+=$(BUVER)/bfd/riscix.c
SOURCES+=$(BUVER)/bfd/rs6000-core.c
# SOURCES+=$(BUVER)/bfd/sco5-core.c
SOURCES+=$(BUVER)/bfd/section.c
SOURCES+=$(BUVER)/bfd/simple.c
SOURCES+=$(BUVER)/bfd/som.c
SOURCES+=$(BUVER)/bfd/sparclinux.c
SOURCES+=$(BUVER)/bfd/sparclynx.c
SOURCES+=$(BUVER)/bfd/sparcnetbsd.c
SOURCES+=$(BUVER)/bfd/srec.c
SOURCES+=$(BUVER)/bfd/stab-syms.c
SOURCES+=$(BUVER)/bfd/stabs.c
SOURCES+=$(BUVER)/bfd/sunos.c
SOURCES+=$(BUVER)/bfd/syms.c
SOURCES+=$(BUVER)/bfd/targets.c
SOURCES+=$(BUVER)/bfd/tekhex.c
# SOURCES+=$(BUVER)/bfd/trad-core.c
SOURCES+=$(BUVER)/bfd/vax1knetbsd.c
SOURCES+=$(BUVER)/bfd/vaxbsd.c
SOURCES+=$(BUVER)/bfd/vaxnetbsd.c
SOURCES+=$(BUVER)/bfd/verilog.c
SOURCES+=$(BUVER)/bfd/versados.c
SOURCES+=$(BUVER)/bfd/vms-alpha.c
SOURCES+=$(BUVER)/bfd/vms-lib.c
SOURCES+=$(BUVER)/bfd/vms-misc.c
SOURCES+=$(BUVER)/bfd/xcofflink.c
SOURCES+=$(BUVER)/bfd/xsym.c
SOURCES+=$(BUVER)/bfd/xtensa-isa.c
SOURCES+=$(BUVER)/bfd/xtensa-modules.c

OBJECTS=$(SOURCES:%=$(BUILD)/%.o)

TARGET=binutils_$(PLATFORM).dll

all: $(TARGET)

$(TARGET): binutils.cpp decodedline.cpp $(BUILD)/libbfd.a
	$(MINGW)\g++ $(CFLAGS) -I$(BUVER)/binutils -std=c++0x $^ $(LDFLAGS) -o $@

$(BUILD)/libbfd.a: $(OBJECTS)
	del /q $(subst /,\,$@)
	$(MINGW)\ar rc $@ $^

$(BUILD)/bfd.h: $(BUVER)/bfd/bfd-in2.h
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	echo #include "config.h" >$@
	sed \
	-e s/@supports_plugins@/0/g \
	-e s/@wordsize@/64/g \
	-e s/@bfd_default_target_size@/32/g \
	-e s/@BFD_HOST_64BIT_LONG@/0/g \
	-e s/@BFD_HOST_64BIT_LONG_LONG@/1/g \
	-e s/@BFD_HOST_64_BIT_DEFINED@/1/g \
	-e "s/@BFD_HOST_64_BIT@/long long/g" \
	-e "s/@BFD_HOST_U_64_BIT@/unsigned long long/g" \
	-e "s/@BFD_HOSTPTR_T@/unsigned long/g" \
	-e s/@bfd_file_ptr@/long/g \
	<$< >>$@

$(BUILD)/bfdver.h:
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	echo #define BFD_VERSION_DATE 20120730 >$@
	echo #define BFD_VERSION 222900000 >>$@
	echo #define BFD_VERSION_STRING  "(GNU Binutils) " "2.22.90" >>$@
	echo #define REPORT_BUGS_TO "<http://www.sourceware.org/bugzilla/>" >>$@

$(BUILD)/bfd_stdint.h:
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	echo #include "stdint.h" >$@

$(BUILD)/config.h: config.in
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	copy /y $(subst /,\,$<) $(subst /,\,$@)

$(BUILD)/elf32-target.h: $(BUVER)/bfd/elfxx-target.h
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/NN/32/g <$< >$@

$(BUILD)/elf64-target.h: $(BUVER)/bfd/elfxx-target.h
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/NN/64/g <$< >$@

$(BUILD)/targmatch.h: $(BUVER)/bfd/config.bfd $(BUVER)/bfd/targmatch.sed
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -f $(BUVER)/bfd/targmatch.sed <$< >$@

$(BUILD)/peigen.c: $(BUVER)/bfd/peXXigen.c
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/XX/pe/g <$< >$@

$(BUILD)/pepigen.c: $(BUVER)/bfd/peXXigen.c
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/XX/pep/g <$< >$@

$(BUILD)/pex64igen.c: $(BUVER)/bfd/peXXigen.c
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/XX/pex64/g <$< >$@

$(BUILD)/elf32-ia64.c: $(BUVER)/bfd/elfnn-ia64.c
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/NN/32/g <$< >$@

$(BUILD)/elf64-ia64.c: $(BUVER)/bfd/elfnn-ia64.c
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	sed -e s/NN/64/g <$< >$@

$(BUILD)/%.c.o: %.c $(HEADERS)
	@mkdir $(subst /,\,$(dir $@)) 2>nul &
	$(MINGW)\gcc $(CFLAGS) $< -c -o $@

clean:
	del /s /q $(BUILD)

.PHONY: all clean print
