########################################################################
### py430.py
###

__doc__ = """
This module implements a profiling MSP430F1611 simulator.
"""

import _py430, _soft430
from _py430 import __version__

from _soft430 import *
del new

def _g():
    "internal"
    for i in xrange(_py430.ARY_SIZE):
        yield 0

def _ary():
    "internal: return array suitable for _py430.bind()"
    from array import array
    return array("L", _g())

class Subarray:
    """
    acts like a list of length self.SIZE whose
    first element lives at index self.OFS of
    the object passed to the constructor.
    """

    def __init__(self, obj):
        "create subarray in obj"
        d = self.__dict__
        d["SIZE"] = self.SIZE
        d["OFS"]  = self.OFS
        d["_obj"] = obj

    def __len__(self):
        "return our length"
        return self.SIZE

    def __getitem__(self, item):
        "fetch item"
        if not (0 <= item < self.SIZE):
            raise IndexError, "Index out of range"
        return self._obj[item + self.OFS]

    def __setitem__(self, item, val):
        "store item"
        if not (0 <= item < self.SIZE):
            raise IndexError, "Index out of range"
        self._obj[item + self.OFS] = val

    def clear(self):
        "zero ourself out"
        for i in xrange(len(self)):
            self[i] = 0

class _Regs(Subarray):
    "internal: direct register access"
    OFS  = _py430.REG_OFS
    SIZE = _py430.REG_SIZE

class _Rom(Subarray):
    "internal: program rom"
    OFS  = _py430.ROM_OFS
    SIZE = _py430.ROM_SIZE

class _Ram(Subarray):
    "internal: ram"
    OFS  = _py430.RAM_OFS
    SIZE = _py430.RAM_SIZE

class PCCount(Subarray):
    "pc counts by (address >> 1)"
    OFS  = _py430.PCC_OFS
    SIZE = _py430.PCC_SIZE

class SPSet(Subarray):
    "has sp (r1) range been initialized?"
    OFS  = _py430.SP_OFS
    SIZE = 1

class SPRange(Subarray):
    "sp range [low, high]"
    OFS  = _py430.SP_OFS + 1
    SIZE = 2

class MemCount(Subarray):
    "memory access counts by address"
    OFS  = _py430.MEM_OFS
    SIZE = _py430.MEM_SIZE

class JumpCount(Subarray):
    "jump counts by (target addr >> 1)"
    OFS  = _py430.JMP_OFS
    SIZE = _py430.JMP_SIZE

class CallCount(Subarray):
    "call counts by (target addr >> 1)"
    OFS  = _py430.CAL_OFS
    SIZE = _py430.CAL_SIZE

class Breakpoint(Subarray):
    """
    breakpoints by address. if an element of
    this array is nonzero, any memory access
    to that address raises MSP430_EXC_DEBUG.

    note that recovery can be tricky; e.g.,
    trapping memory accesses during instruction
    decode can require fancy recovery.
    """
    OFS  = _py430.BRK_OFS
    SIZE = _py430.BRK_SIZE

class PCTicks(Subarray):
    "cumulative ticks by (pc >> 1)"
    OFS  = _py430.PCT_OFS
    SIZE = _py430.PCT_SIZE

########################################
### helper base class

class VBase:
    "abstract helper base class"

    def __init__(self, cpu):
        self.__dict__["_cpu"] = cpu

########################################
### register helper

class Regs(VBase):
    "cpu registers"

    def __len__(self):
        "return 16 (number of msp430 registers)"
        return MSP430_NREGS

    def __getitem__(self, reg):
        "fetch register by number"
        if not (0 <= reg < MSP430_NREGS):
            raise IndexError, reg
        return self._cpu.get_reg(reg)

    def __setitem__(self, reg, val):
        "store register by number"
        if not (0 <= reg < MSP430_NREGS):
            raise IndexError, reg
        self._cpu.set_reg(reg, val)

    _attrs = {
        "pc" : 0,
        "sp" : 1,
        "sr" : 2,
        "cg" : 3,
        "r0" : 0,
        "r1" : 1,
        "r2" : 2,
        "r3" : 3,
        "r4" : 4,
        "r5" : 5,
        "r6" : 6,
        "r7" : 7,
        "r8" : 8,
        "r9" : 9,
        "r10": 10,
        "r11": 11,
        "r12": 12,
        "r13": 13,
        "r14": 14,
        "r15": 15,
    }.get

    def __getattr__(self, attr):
        "fetch register by name"
        if attr[:2] == "__":
            raise AttributeError, attr
        v = self._attrs(attr)
        if v is None:
            raise AttributeError, attr
        return self[v]

    def __setattr__(self, attr, val):
        "store register by name"
        v = self._attrs(attr)
        if v is None:
            raise AttributeError, attr
        self[v] = val

    def clear(self):
        "zero out all registers"
        self._cpu.clr_reg()

########################################
### memory access helpers

class ByteMem(VBase):
    "access cpu address space as bytes"

    def __len__(self):
        "return 64k"
        return 0x10000

    def __getitem__(self, addr):
        "fetch memory"
        if not (0 <= addr <= 0xffff):
            raise IndexError, addr
        return self._cpu.bfetch(addr)

    def __setitem__(self, addr, val):
        "store memory"
        if not (0 <= addr <= 0xffff):
            raise IndexError, addr
        self._cpu.bstore(addr, val)

class WordMem(VBase):
    "access cpu address space as words"

    def __len__(self):
        "return 64k"
        return 0x10000

    def __getitem__(self, addr):
        "fetch memory"
        if not (0 <= addr <= 0xffff):
            raise IndexError, addr
        if addr & 1:
            raise ValueError, addr
        return self._cpu.wfetch(addr)

    def __setitem__(self, addr, val):
        "store memory"
        if not (0 <= addr <= 0xffff):
            raise IndexError, addr
        if addr & 1:
            raise ValueError, addr
        self._cpu.wstore(addr, val)

########################################
### profiling helpers

class InsnBase:
    "abstract base for per-instruction profile details"

    ### map instruction name to number
    _imap = { }
    _ilst = [ ]

    def __iter__(self):
        """
        iterator that returns (name, quan) in MSP430_INSN_*
        order (the first name will be "all"). the interpretation
        of quan is left to the subclass.
        """
        for k in self._ilst:
            yield (k, getattr(self, k))

    def __getattr__(self, attr):
        "fetch data by core insn mnemonic"
        v = self._imap.get(attr.lower())
        if v is None:
            raise AttributeError, attr
        return self[v]

    def __setattr__(self, attr, val):
        "store data by core insn mnemonic"
        v = self._imap.get(attr.lower())
        if v is None:
            raise AttributeError, attr
        self[v] = val

def _boot():
    "internal: init various data"
    import _soft430
    d = { }
    for k in dir(_soft430):
        v = getattr(_soft430, k)
        ### support accessing profile via instruction name
        if k.startswith("MSP430_INSN_TYPE"):
            continue
        if k.startswith("MSP430_INSN_"):
            n = k[12:].lower()
            InsnBase._imap[n] = v
            d[v] = n
    for i in xrange(len(d)):
        InsnBase._ilst.append(d[i])
_boot()
del _boot

class InsnCount(Subarray, InsnBase):
    "per-core-instruction counts by instruction"
    OFS  = _py430.INS_OFS
    SIZE = _py430.INS_SIZE

class InsnTicks(Subarray, InsnBase):
    "per-core-instruction ticks by instruction"
    OFS  = _py430.TCK_OFS
    SIZE = _py430.TCK_SIZE

class Profile:
    """
    Profiling class. Interesting ivars include:

    pc_count   -- a  PCCount
    insn_count -- an InsnCount
    insn_ticks -- an InsnTicks
    mem_count  -- a  MemCount
    sp_set     -- an SPSet
    sp_range   -- an SPRange
    jump_count -- a  JumpCount
    call_count -- a  CallCount
    pc_ticks   -- a  PCTicks
    """
    
    def __init__(self, cpu, ary):
        "create profiler bound to cpu and data array"
        self._cpu       = cpu
        self.pc_count   = PCCount(ary)
        self.insn_count = InsnCount(ary)
        self.insn_ticks = InsnTicks(ary)
        self.mem_count  = MemCount(ary)
        self.sp_set     = SPSet(ary)
        self.sp_range   = SPRange(ary)
        self.jump_count = JumpCount(ary)
        self.call_count = CallCount(ary)
        self.pc_ticks   = PCTicks(ary)

    def clear(self):
        "zero out profiling data"
        self._cpu.clr_prf()

    def set(self, val="now"):
        """
        set profiling state. val must be an integer or one
        of the strings "off", "on", "now", "next". the
        profiling state is set as follows:

        str   int   description
        ===   ===   ===========
        off     0   profiling is suspended
        on     >0   profiling enabled after current instruction completes
        next   >0   same as "on"
        now    <0   profiling enabled immediately
        """
        
        if not val:
            val = "off"
        if isinstance(val, str):
            val = val.lower()
            if val not in ("off", "on", "now", "next"):
                raise ValueError, val
        elif not isinstance(val, int):
            raise TypeError, "value must be string or int"
        if (val == "off") or (not val):
            val = MSP430_PROFILE_OFF
        elif (val in ("on", "next")) or (val > 0):
            val = MSP430_PROFILE_NEXT
        else:
            val = MSP430_PROFILE_NOW
        self._cpu.ena_prf(val)

########################################
### sfr handler mixin

class MSP430SFR:
    "implements some MSP430F1611 peripherals"

    def __init__(self):
        "get ready"
        self.sfrs = [0x00] * MSP430_NSFR
        self._cpu.set_sfr(self._do_sfr)
        self._hwmul_state = 0

    def _do_sfr(self, addr, val, isWrite, isWord):
        "process an sfr request"
        if not (addr & 0x0100):
            if isWord:
                raise MSP430Error, MSP430_EXC_BAD_SFR
        else:
            ### let hwmul handle itself
            if ((addr < 0x0130) or (addr > 0x013f)) and (not isWord):
                raise MSP430Error, MSP430_EXC_BAD_SFR

        ### make sure it's mapped
        n = self._cpu.sfr_nam(addr)
        if not n:
            raise MSP430Error, MSP430_EXC_ENXIO

        ### wdt rule check
        if (addr == 0x0121):
            if isWrite:
                if val != 0x5a:
                    raise MSP430Error, MSP430_EXC_WDT_FAULT
            else:
                val = 0x69

        ### flash controller rule checks
        if (addr in (0x0129, 0x012b, 0x012d)):
            if isWrite:
                if val != 0xa5:
                    raise MSP430Error, MSP430_EXC_WDT_FAULT
            else:
                val = 0x96

        ### hardware multiplier
        if ((addr >= 0x0130) and (addr < 0x0140)):
            val = self._hwmul(addr, val, isWrite, isWord)

        ### treat rest like ram
        if isWrite:
            self.sfrs[addr] = val
        else:
            val = self.sfrs[addr]

        print "[%s] %cSFR 0x%03x = 0x%02x insn=%d tick=%d" % \
              (n, (isWrite and "W" or "R"), addr, val,
               self.insns, self.ticks)

        return val

    def _hwmul(self, addr, val, isWrite, isWord):
        "implements the hardware multiplier"
        if (not isWord) and (addr & 1):
            raise MSP430Error, MSP430_EXC_BAD_ALIGN
        if not isWrite:
            return val
        if addr >= 0x013e:
            raise MSP430Error, MSP430_EXC_BAD_STORE
        if addr >= 0x013a:
            return val
        if not isWord:
            self.sfrs[addr + 1] = 0
        if addr < 0x0134:
            for i in xrange(6):
                self.sfrs[0x013a + i] = 0
        if addr & 1:
            return val
        if addr < 0x0138:
            self._hwmul_state = addr - 0x0130
        else:
            self._do_hwmul()
        return val

    def _do_hwmul(self):
        "internal: do a multiply"
        ofs = self._hwmul_state
        op1 = self.sfrs[0x0130 + ofs] | (self.sfrs[0x0131 + ofs] << 8)
        op2 = self.sfrs[0x0138]       | (self.sfrs[0x0139]       << 8)
        op1 = long((op1 ^ 0x8000) - 0x8000)
        op2 = long((op2 ^ 0x8000) - 0x8000)
        prd = op1 * op2
        for i in xrange(6):
            b  = self.sfrs[0x013a + ofs]
            b += prd & 0xff
            c  = b >> 8
            self.sfrs[0x013a + ofs] = b & 0xff
            prd += c

########################################
### mixin to load various file formats

class MSP430Loader:
    """
    This mixin teaches the cpu to load
    C (.c) files, assembler (.s, .S,
    and .asm) files, and executable (.exe)
    files. dot-s and dot-S files are run
    through msp430-gcc; dot-asm files are
    run through msp430-as.

    This mixin also attempts to load
    symbol table and debugging information
    from a .exe file with the same base
    name.
    """

    def __init__(self):
        "internal"
        self.symtab = None

    def _execute(self, *args):
        "internal"
        from subprocess import Popen, PIPE, STDOUT

        p = text = code = None
        try:
            try:
                p    = Popen(args, stdin=PIPE, stdout=PIPE, stderr=STDOUT)
                text = p.communicate()[0]
                code = p.returncode
            except:
                pass
        finally:
            del p
        if code is None:
            raise OSError, "subprocess exception!"
        if code:
            raise RuntimeError, "subprocess exited with code " + \
                  str(code) + ": " + text
        return text

    def exe_to_hex(self, exe, hex):
        "internal: run msp430-objcopy on a .exe"
        self._execute("msp430-objcopy",
                      "-O", "ihex",
                      exe, hex)

    def _rungcc(self, exe, *opts):
        "internal: run msp430-gcc"
        self._execute("msp430-gcc",
                      "-DSOFTMSP",
                      "-mmcu=msp430x1611",
                      "-Wall",
                      "-g", "-O",
                      "-o", exe,
                      *opts)

    def c_to_exe(self, c, exe, *opts):
        "internal: compile .c to .exe"
        import os
        lst = os.path.splitext(exe)[0] + ".lst"
        self._rungcc(exe,
                     "-fverbose-asm",
                     "-save-temps",
                     "-Wa,-a=" + lst,
                     c, *opts)

    def s_to_exe(self, s, exe, *opts):
        "internal: compile .s and .S to .exe"
        import os
        lst = os.path.splitext(exe)[0] + ".lst"
        self._rungcc(exe,
                     "-Wa,-a=" + lst,
                     s, *opts)

    S_to_exe = s_to_exe

    def asm_to_exe(self, asm, exe, *opts):
        "internal: compile .asm to .exe"
        import os
        lst = os.path.splitext(exe)[0] + ".lst"
        obj = os.path.splitext(exe)[0] + ".o"
        self._execute("msp430-as",
                      "-mmcu=msp430x1611",
                      "-a=" + lst,
                      "--gstabs",
                      "-o", obj, asm)
        self._rungcc(exe,
                     obj,
                     *opts)
        os.unlink(obj)

    def load(self, fname, *opts):
        "load a file into the cpu"
        import os
        base, ext = os.path.splitext(fname)
        if not ext:
            raise ValueError, "cannot determine file type"
        ext = ext[1:]
        if ext == "hex":
            if opts:
                raise ValueError, "Options not supported for hex files"
            hex = fname
        else:
            if ext == "exe":
                if opts:
                    raise ValueError, "Options not supported for exe files"
                exe = fname
            else:
                exe = base + ".exe"
                tox = getattr(self, ext + "_to_exe", None)
                if not tox:
                    raise ValueError, "cannot load " + repr(fname)
                tox(fname, exe, *opts)
            hex = base + ".hex"
            self.exe_to_hex(exe, hex)
        self.load_syms(base + ".exe")
        self.load_ihex(hex)

    def load_syms(self, exe):
        "attempt to load symbol/debugging information from exe"
        try:
            symtab = self._execute("msp430-objdump",
                                   "-t", exe)
            finfo  = self._execute("msp430-objdump",
                                   "-g", exe)
        except RuntimeError:
            self.symtab = None
        else:
            self.symtab = self.parse_syms(symtab, finfo)

    def parse_syms(self, symtab, finfo):
        """
        parse symbol/debugging information into the following
        data structure:

        {
            addr: {
                "addr": addr,
                "base": <first addr for object>,
                "size": <size of object>,
                "name": <name of object>,
                "type": "function" or "object",
                "file": <filename>,
                "line": <line#>,
            },
            ...
        }

        The "addr" key will always be filled in. If the
        entry appears in the symbol table, the "base",
        "size", "name", and "type" slots will be filled
        in. If the entry appears in the debugging info,
        the "file" and "line" slots will be filled in.

        Only type "F" and "O" objects are considered in
        the symbol table (functions and data [in C]).

        Only entries corresponding to addresses less than
        65504 (the start of the interrupt vector table)
        are considered.

        Consider using the bisect module to locate
        entries in the returned object; i.e., not all
        addresses need be valid.
        """
        nt = { }
        st = { None: nt }
        ot = { "F": "function", "O": "object" }
        symtab = symtab.replace("\r\n", "\n").replace("\r", "\n")
        for line in symtab.split("\n"):
            if len(line) < 16:
                continue
            type = line[15]
            if type not in "FO":
                continue
            line = line.split()
            if len(line) < 3:
                continue
            addr = int(line[0], 16)
            name = line[-1]
            size = int(line[-2], 16)
            nt[name] = addr
            if addr >= 65536 - 32:
                continue
            for i in xrange(size):
                st[addr + i] = {
                    "name": name,
                    "addr": addr + i,
                    "base": addr,
                    "size": size,
                    "type": ot[type],
                }
        finfo = finfo.replace("\r\n", "\n").replace("\r", "\n")
        fdict = { }
        tmp   = [ ]
        for line in finfo.split("\n"):
            if "/* file " not in line:
                continue
            line = line.split()
            if len(line) < 7:
                continue
            file = line[2]
            addr = int(line[-2], 0)
            line = int(line[-4])
            if addr >= 65536 - 32:
                continue
            if file not in fdict:
                try:
                    f = open(file, "r")
                    try:
                        lines = f.readlines()
                        lines.insert(0, "")
                    finally:
                        f.close()
                except:
                    lines = None
                fdict[file] = lines
            lines = fdict[file]
            try:
                code = lines[line]
            except:
                code = "<source code not available>"
            tmp.append( (addr, file, line, code) )
        tmp.sort(lambda a,b: cmp(b[0], a[0]))
        while tmp:
            addr, file, line, code = tmp.pop()
            if tmp:
                end = tmp[-1][0]
            else:
                end = addr + 2
            while addr < end:
                info = st.setdefault(addr, { })
                info["file"] = file
                info["line"] = line
                info["code"] = code
                info.setdefault("addr", addr)
                addr += 1
        return st

########################################################################
### cpu class

class MSP430F1611(MSP430SFR, MSP430Loader):
    """
    This class implements the MSP430F1611 CPU core.
    """

    def __init__(self):
        "create a new CPU instance"
        from _soft430 import new
        self._ary       = _ary()
        self._cpu       = new()
        self._bnd       = _py430.bind(self._cpu, self._ary)
        self._regs      = _Regs(self._ary)
        self.regs       = Regs(self._cpu)
        self.byte_mem   = ByteMem(self._cpu)
        self.word_mem   = WordMem(self._cpu)
        self._rom       = _Rom(self._ary)
        self._ram       = _Ram(self._ary)
        self.profile    = Profile(self._cpu, self._ary)
        self.brkpts     = Breakpoint(self._ary)
        self._cpu.set_trp(self._do_trap)
        MSP430SFR.__init__(self)
        MSP430Loader.__init__(self)

    def __del__(self):
        "destroy ourself"
        self.__dict__.clear()

    def __getattr__(self, attr):
        "pass attribute requests on the underlying C CPU implementation"
        if attr[:2] == "__":
            raise AttributeError, attr
        ret = getattr(self._cpu, attr, None)
        if callable(ret):
            ret = None
        if ret is None:
            raise AttributeError, attr
        return ret

    def load_ihex(self, filename):
        """
        load an Intel hex file and initialize
        the cpu.
        """
        _py430.load(self._bnd, filename)
        self.initialize()

    def initialize(self):
        """Clear all profiling data, enable
        profiling, and load the reset vector
        into the pc (r0).
        """
        self.profile.clear()
        self.brkpts.clear()
        self.profile.sp_set.clear()
        self.profile.sp_range.clear()
        self.profile.set()
        self.reset()

    def reset(self):
        "load the pc (r0) with the word contents of 0xfffe"
        self._cpu.reset()

    def run(self, maxi=0, maxc=0):
        """
        run code on the CPU. if maxi is positive, run
        at most maxi instructions. if maxc is positive,
        run for at most maxc clock cycles. if both maxi
        and maxc are zero, run until an exception is
        raised or the CPU halts.

        returns zero on CPU halt; otherwise, raises an
        exception.
        """
        if (maxi < 0) or (maxc < 0):
            raise ValueError, "Bad maxi/maxc"
        if maxi or maxc:
            return self._cpu.run(maxi, maxc)
        while self._cpu.run(0, 8192) == MSP430_EXC_CPU_TIME:
            pass

    def sfr_name(self, addr):
        "return name of SFR corresponding to address"
        return self._cpu.sfr_nam(addr)

    def strerror(self, code):
        "return error description for MSP430_EXC_* code"
        return self._cpu.strerr(code)

    def restore(self, filename):
        "restore the cpu state from a file"
        import array
        a = array.array("L")
        f = open(filename, "rb")
        try:
            a.fromfile(f, _py430.ARY_SIZE)
        finally:
            f.close()
        self._ary[:] = a

    def save(self, filename):
        "dump the cpu state to a file"
        f = open(filename, "wb")
        try:
            self._ary.tofile(f)
        finally:
            f.close()

    def _do_trap(self, val):
        "internal: trap implementation"
        if val == 1:
            self.profile.set("off")
        elif val == 2:
            self.profile.clear()
        elif val == 4:
            self.profile.set("next")
        else:
            raise MSP430Error, MSP430_EXC_BAD_TRAP

def test():
    x = MSP430F1611()

    x.load("sjrefa.hex")
    import time
    t = time.time()

    #x.brkpts[0x4b28] = 1
    try:
        x.run()
        print "CPU halt"
    finally:
        print time.time() - t
        print hex(x.last_pc), x.last_pc

    print "Regs", [hex(w) for w in x.regs]
    print "Glbl insns/ticks", x.insns, x.ticks
    print "Prof insns/ticks", x.profile.insn_count[0], x.profile.insn_ticks[0]
    print "Stack", [w for w in x.profile.sp_range]
    print

    z = x.profile.call_count
    w = [(hex(i << 1), i << 1, z[i]) for i in xrange(len(z)) if z[i]]
    print "Calls", w
    print

    print "Insn ticks", [w for w in x.profile.insn_ticks]
    print

    z = x.profile.pc_ticks
    #z = x.profile.pc_count
    z = [(hex(i << 1), i << 1, z[i]) for i in xrange(len(z)) if z[i]]
    print "PC ticks", z
    print
    z.sort(lambda a, b: cmp(b[2], a[2]))
    print "Worst", z[:8]
    print

if __name__ == "__main__":
    test()

### EOF py430.py

