#!/bin/sh
########################################################################
### mdb.py -- msp430 debugger
###

'''exec' python $0 ${1+"$@"}
'''

import glob, sys
sys.path.insert(0, glob.glob("build/lib.*")[0])

########################################################################
try:
    import readline
except ImportError:
    pass

import cmd

class MDB(cmd.Cmd):
    intro = """\
MDB MSP430F1611 debugger version 1.0

Type 'help' for a list of topics
"""

    prompt = "MDB> "
    
    def __init__(self, *rest, **kw):
        cmd.Cmd.__init__(self, *rest, **kw)
        from py430 import MSP430F1611
        self.cpu = MSP430F1611()

    def precmd(self, line):
        l = line.lstrip()
        if l.startswith("."):
            return line[:len(line)-len(l)] + "dot_" + l[1:]
        return line

    def __call__(self, func, *rest, **kw):
        try:
            func(*rest, **kw)
        except:
            import sys, traceback
            t = "".join(traceback.format_exception(*(sys.exc_info())))
            print >>self.stdout, "ERROR:\n", t

    def _int(self, arg):
        if self.cpu.symtab:
            val = self.cpu.symtab[None].get(arg)
            if val is not None:
                return val
        if arg.lower().startswith("0b"):
            d   = { "0": 0, "1": 1 }
            ok  = False
            ret = 0
            for c in arg:
                if c in "-,":
                    continue
                if c not in d:
                    return None
                ret = (ret << 1) | d[c]
                ok  = True
            if not ok:
                return None
            return ret
        try:
            return int(arg, 0)
        except:
            return None

    def _bin(self, x):
        r  = "0b"
        x &= 0xffff
        for i in xrange(16):
            if i in (4, 8, 12):
                r += "-"
            r  += (x & (1 << (15 - i))) and "1" or "0"
            x >>= 1
        return r

    def _go(self, addr):
        self.cpu.regs[0] = addr

    def do_go(self, arg):
        "set pc (r0)"
        arg = self._int(arg)
        if arg is None:
            print >>self.stdout, "usage: go address"
        else:
            self(self._go, arg)

    def do_regs(self, arg):
        "dump registers"
        if arg:
            print >>self.stdout, "usage: regs"
        for r in xrange(len(self.cpu.regs)):
            if r == 3:
                continue
            v = self.cpu.regs[r]
            n = "r%d" % r
            print "%3s 0x%04x %5d %s   " % (n, v, v, self._bin(v)),
            if r == 2:
                s  = ""
                s += (v & 0x100) and "v" or "-"
                s += (v & 0x008) and "i" or "-"
                s += (v & 0x004) and "n" or "-"
                s += (v & 0x002) and "z" or "-"
                s += (v & 0x001) and "c" or "-"
                print "flags:", s,
            print

    def do_next(self, arg):
        "execute n instructions (default 1)"
        if arg:
            arg = self._int(arg) or 0
            if arg < 0:
                print >>self.stdout, "usage: step [n]"
                return
        else:
            arg = 1
        print >>self.stdout, "Step", arg
        self(self.cpu.run, arg)

    def do_dot_(self, arg):
        "print info about current pc location"
        addr = self.cpu.regs.r0
        print >>self.stdout, "r0: 0x%04x %d" % (addr, addr)
        if not self.cpu.symtab:
            return
        info = self.cpu.symtab.get(addr)
        if not info:
            print >>self.stdout, "  no debug info available"
            return
        if "type" not in info:
            return
        if info["type"] != "function":
            print >>self.stdout, "  no debug info available"
        print >>self.stdout, "  func ", info["name"]
        print >>self.stdout, "  start 0x%04x %d" % (info["base"], info["base"])
        print >>self.stdout, "  ofs  ", addr - info["base"]
        if "file" in info:
            print >>self.stdout, "  file ", info["file"]
            print >>self.stdout, "  line ", info["line"]
            print >>self.stdout, "  code ", info["code"].strip()

    def do_sym(self, arg):
        "dump address for symbol or all symbols and addresses"
        if not self.cpu.symtab:
            print >>self.stdout, "no symbol info available"
        nt = self.cpu.symtab[None]
        if arg:
            if arg not in nt:
                print >>self.stdout, "symbol " + repr(arg) + " not defined"
                return
            l = [(arg, nt[arg])]
        else:
            l = nt.items()
        l.sort(lambda a,b: cmp(a[0], b[0]))
        for n, a in l:
            print "0x%04x %5d %s" % (a, a, n)

    def do_Load(self, arg):
        "load a file into the CPU"
        if arg:
            self(self.cpu.load, arg)
        else:
            print >>self.stdout, "usage: load filename"

    def do_Restore(self, arg):
        "restore the cpu state from a file"
        if arg:
            self(self.cpu.restore, arg)
        else:
            print >>self.stdout, "usage: restore filename"

    def do_Save(self, arg):
        "dump the cpu state to a file"
        if arg:
            self(self.cpu.save, arg)
        else:
            print >>self.stdout, "usage: save filename"

    def do_Quit(self, arg):
        "terminate the shell"
        if arg:
            print >>self.stdout, "usage: quit"
        else:
            print >>self.stdout, "bye"
            return True

    do_Exit = do_EOF = do_Quit

    def do_shell(self, arg):
        "execute command in a subshell"
        import os, pwd
        arg = arg or \
              os.environ.get("SHELL") or \
              pwd.getpwuid(os.getuid()).pw_shell or \
              "/bin/sh"
        os.system(arg)

def go(filename=None):
    ci = MDB()
    if filename:
        ci.do_Load(filename)
    ci.cmdloop()

########################################################################

__doc__ = """
usage: mdb.py
"""

def usage():
    raise SystemExit, __doc__

def main(args):
    len(args) > 1 and usage()
    go(*(args or [None]))

if __name__ == "__main__":
    import sys
    main(sys.argv[1:])

### EOF mdb.py

