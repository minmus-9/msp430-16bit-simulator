/***********************************************************************
 * py430.c -- python profiling module for soft430 module
 */

#include <Python.h>

#define SOFT430_LIB
#define PYTHON_API
#include "soft430.h"

static PyObject *array = NULL;

#define SP_SET  0
#define SP_LOW  1
#define SP_HIGH 2

#define REG_SIZE (MSP430_NREGS)
#define ROM_SIZE (MSP430_NPROG)
#define RAM_SIZE (MSP430_NRAM)
#define PCC_SIZE (0x8000)
#define SP_SIZE  (3)
#define INS_SIZE ((MSP430_INSN_CNT) + 1)
#define TCK_SIZE ((MSP430_INSN_CNT) + 1)
#define MEM_SIZE (0x10000)
#define JMP_SIZE (0x8000)
#define CAL_SIZE (0x8000)
#define BRK_SIZE (0x10000)
#define PCT_SIZE (0x8000)

#define ARY_SIZE (REG_SIZE + ROM_SIZE + RAM_SIZE + PCC_SIZE + SP_SIZE + INS_SIZE + TCK_SIZE + MEM_SIZE + JMP_SIZE + CAL_SIZE + BRK_SIZE + PCT_SIZE)

static PyTypeObject Py430type;

typedef struct py430 {
  unsigned long *regs;     /* 16  */
  unsigned long *rom;      /* 48k */
  unsigned long *ram;      /* 10k */
  unsigned long *pccnt;    /* 32k */
  unsigned long *spinfo;   /*  3  */
  unsigned long *insns;    /* 28  */
  unsigned long *ticks;    /* 28  */
  unsigned long *memcnt;   /* 64k */
  unsigned long *jumpcnt;  /* 32k */
  unsigned long *callcnt;  /* 32k */
  unsigned long *breakpt;  /* 64k */
  unsigned long *pcticks;  /* 32k */
} py430_t;

typedef struct py430object {
  PyObject_HEAD
  py430_t       p;
  msp430_ctx_t *ctx;     /* cpu context */
  PyObject     *pyary;   /* array containing array data */
  PyObject     *softmsp; /* object containing ctx */
} py430object;

#define ctx(obj) ((obj)->ctx)
#define pyo(obj) (&((obj)->p))

static int regs(msp430_ctx_t  *ctx,
		int            reg,
		msp430_word_t *val,
		int            isWrite,
		void          *xtra) {
  py430_t *p = (py430_t *) xtra;

  if (isWrite) {
    msp430_word_t v = *val;

    p->regs[reg] = v;
    if (ctx->profile) {
      switch (reg) {
        case 0: {
	  p->pccnt[v >> 1]++;
	  break;
	}
        case 1: {
	  if (p->spinfo[0]) {
	    if (v < p->spinfo[1])
	      p->spinfo[1] = v;
	    if (v > p->spinfo[2])
	      p->spinfo[2] = v;
	  } else {
	    p->spinfo[0] = 1;
	    p->spinfo[1] = v;
	    p->spinfo[2] = v;
	  }
	  break;
	}
      }
    }
  } else {
    *val = p->regs[reg];
  }
  return MSP430_SUCCESS;
}

static int mem(msp430_ctx_t  *ctx,
	       msp430_word_t  addr,
	       msp430_byte_t *val,
	       int            isWrite,
	       int            isWord,
	       void          *xtra) {
  py430_t *p = (py430_t *) xtra;

  if (p->breakpt[addr])
    return MSP430_EXC_DEBUG;

  if (ctx->profile)
    p->memcnt[addr]++;

  if (addr & 0xc000) {
    /*** program flash */
    if (isWrite)
      return MSP430_EXC_BAD_STORE;
    addr -= MSP430_ROM_BASE;
    *val  = p->rom[addr];
    return MSP430_SUCCESS;
  }
  if (addr & 0x3000) {
    if (addr >= 0x3900)
      return MSP430_EXC_ENXIO;

    if (addr < MSP430_RAM_BASE) {
      /*** user flash */
      if (isWrite)
	return MSP430_EXC_BAD_STORE;
      /*** XXX assume erased */
      *val = 0xff;
      return MSP430_SUCCESS;
    }

    /*** ram */
    addr -= MSP430_RAM_BASE;
    if (isWrite)
      p->ram[addr] = *val;
    else
      *val = p->ram[addr];
    return MSP430_SUCCESS;
  }
  if (addr >= MSP430_BSL_BASE) {
    /*** boot prom */
    if (isWrite)
      return MSP430_EXC_BAD_STORE;
    /*** XXX assume empty */
    *val = 0xff;
    return MSP430_SUCCESS;
  }
  if (addr >= 0x0a00)
    return MSP430_EXC_ENXIO;
  if (addr >= 0x0200) {
    /*** ram */
    addr -= 0x0200;
    if (isWrite)
      p->ram[addr] = *val;
    else
      *val = p->ram[addr];
    return MSP430_SUCCESS;
  }
  /*** sfrs */
  if (ctx->in_exec)
    return MSP430_EXC_BAD_EXEC;
  return ctx->sfr(ctx, addr, val, isWrite, isWord, ctx->sfr_xtra);
}

static void prof(msp430_ctx_t *ctx,
		 int           insn,
		 int           ticks,
		 void         *xtra) {
  py430_t *p = (py430_t *) xtra;

  if (insn && ticks) {
    p->insns[0]++;
    p->insns[insn]++;
    p->ticks[0] += ticks;
    p->ticks[insn] += ticks;
    /*** count jump/call targets */
    if (insn == MSP430_INSN_CALL) {
      p->callcnt[soft430_get_reg(ctx, 0) >> 1]++;
    } else {
      if (insn >= MSP430_INSN_JNZ) {
	p->jumpcnt[soft430_get_reg(ctx, 0) >> 1]++;
      }
    }
    /*** add ticks to this insn's pc */
    p->pcticks[ctx->insn_pc >> 1] += ticks;
  } else {
    memset(p->pccnt,   0, PCC_SIZE * sizeof(unsigned long));
    memset(p->memcnt,  0, MEM_SIZE * sizeof(unsigned long));
    memset(p->insns,   0, INS_SIZE * sizeof(unsigned long));
    memset(p->ticks,   0, TCK_SIZE * sizeof(unsigned long));
    memset(p->jumpcnt, 0, JMP_SIZE * sizeof(unsigned long));
    memset(p->callcnt, 0, CAL_SIZE * sizeof(unsigned long));
    memset(p->pcticks, 0, PCT_SIZE * sizeof(unsigned long));
  }
}

static int do_bind(py430_t *rec, msp430_ctx_t *ctx) {
  if (soft430_set_reg(ctx, regs, rec) != MSP430_SUCCESS)
    goto bad;
  if (soft430_set_mem(ctx, mem, rec) != MSP430_SUCCESS)
    goto bad;
  if (soft430_set_prf(ctx, prof, rec) != MSP430_SUCCESS)
    goto bad;
  return 0;

 bad:
  soft430_set_reg(ctx, NULL, NULL);
  soft430_set_mem(ctx, NULL, NULL);
  soft430_set_prf(ctx, NULL, NULL);
  return -1;
}

static py430object *new(void) {
  py430object *ret;

  if ((ret = PyObject_GC_New(py430object, &Py430type)) == NULL)
    return NULL;
  memset(pyo(ret), 0, sizeof(py430_t));
  ret->ctx     = NULL;
  ret->pyary   = NULL;
  ret->softmsp = NULL;
  return ret;
}

static int clear(py430object *p) {
  msp430_ctx_t *c = p->ctx;
  PyObject     *tmp;

  if (c == NULL)
    return 0;
  p->ctx = NULL;
  soft430_set_reg(c, NULL, NULL);
  soft430_set_mem(c, NULL, NULL);
  soft430_set_prf(c, NULL, NULL);
  tmp = p->pyary;
  p->pyary = NULL;
  Py_XDECREF(tmp);
  tmp = p->softmsp;
  p->softmsp = NULL;
  Py_XDECREF(tmp);
  return 0;
}

static void del(py430object *p) {
  PyObject_GC_UnTrack(p);
  clear(p);
  PyObject_GC_Del(p);
}

static int traverse(py430object *self, visitproc visit, void *arg) {
  PyObject *tmp;
  int       rc;

  if ((tmp = self->pyary) != NULL) {
    if ((rc = visit(tmp, arg)) != 0) return rc;
  }
  if ((tmp = self->softmsp) != NULL) {
    if ((rc = visit(tmp, arg)) != 0) return rc;
  }
  return 0;
}

static PyObject *pybind(PyObject *self, PyObject *args) {
  PyObject        *oary, *omsp, *tmp;
  msp430_ctx_t    *msp;
  py430object     *ret;
  py430_t         *rec;
  char            *s;
  unsigned long   *bp;
  Py_ssize_t       bl;

  if (!PyArg_ParseTuple(args, "OO", &omsp, &oary))
    return NULL;
  if ((msp = soft430_get_ctx(omsp)) == NULL)
    return NULL;
  if (PyObject_Type(oary) != array) {
    PyErr_SetString(PyExc_TypeError, "array expected");
    return NULL;
  }
  if (PyObject_Size(oary) < ARY_SIZE) {
    PyErr_SetString(PyExc_ValueError, "array too short");
    return NULL;
  }
  if ((tmp = PyObject_GetAttrString(oary, "typecode")) == NULL)
    return NULL;
  if (!PyString_Check(tmp)) {
    PyErr_SetString(PyExc_TypeError, "array.typecode didn't return string");
    return NULL;
  }
  if ((s = PyString_AsString(tmp)) == NULL)
    return NULL;
  Py_DECREF(tmp);
  if (strcmp(s, "L")) {
    PyErr_SetString(PyExc_ValueError, "expected type L array");
    return NULL;
  }
  if (PyObject_AsWriteBuffer(oary, (void **) &bp, &bl))
    return NULL;
  if (bl < ARY_SIZE) {
    PyErr_SetString(PyExc_ValueError, "array too short");
    return NULL;
  }

  if ((ret = new()) == NULL)
    return NULL;
  rec = pyo(ret);

  rec->regs = bp;
  bp += REG_SIZE;
  rec->rom = bp;
  bp += ROM_SIZE;
  rec->ram = bp;
  bp += RAM_SIZE;
  rec->pccnt = bp;
  bp += PCC_SIZE;
  rec->spinfo = bp;
  bp += SP_SIZE;
  rec->insns = bp;
  bp += INS_SIZE;
  rec->ticks = bp;
  bp += TCK_SIZE;
  rec->memcnt = bp;
  bp += MEM_SIZE;
  rec->jumpcnt = bp;
  bp += JMP_SIZE;
  rec->callcnt = bp;
  bp += CAL_SIZE;
  rec->breakpt = bp;
  bp += BRK_SIZE;
  rec->pcticks = bp;
  bp += PCT_SIZE;

  ret->ctx     = msp;
  ret->pyary   = oary;
  ret->softmsp = omsp;
  Py_INCREF(oary);
  Py_INCREF(omsp);
  if (do_bind(rec, msp)) {
    Py_DECREF(ret);
    PyErr_SetString(PyExc_RuntimeError, "bind() failed");
    return NULL;
  }
  PyObject_GC_Track((PyObject *) ret);
  return (PyObject *) ret;
}

PyDoc_STRVAR(bind_doc,
"bind(msp430, ary) -> obj\n\
\n\
Bind the MSP430 object as returned by _soft430.new\n\
to the type-L array object ary. CPU execution will\n\
update the profiling data in ary. The array map in\n\
ary is as follows:\n\
\n\
    start    length    contents\n\
   ======    ======    ==================================\n\
        0        16    register values\n\
       16     49152    program rom\n\
    49168     10240    ram\n\
    59408     32768    pc counts by word address\n\
    92176         3    sp info\n\
    92179        28    counts by instruction\n\
    92207        28    ticks by instruction\n\
    92235     65536    address space accesses\n\
   157771     32768    jumps to address\n\
   190539     32768    calls to address\n\
   223307     65536    breakpoints by address\n\
   288843     32768    total ticks per pc address\n\
\n\
the sp (r1) info consists of the following three slots:\n\
\n\
   0 = nonzero if sp has been set\n\
   1 = lowest sp value\n\
   2 = highest sp value\n\
\n\
NOTE: The CPU register, memory, and profiling handlers are\n\
      replaced after the call to bind().\n\
\n\
NOTE: If bind() fails, either none or all of these handlers\n\
      will be reset to the default C handler function.\n\
\n\
NOTE: When the returned object is destroyed, the CPU will\n\
      be rebound to the default C handler functions.");

static PyObject *pyihex(PyObject *self, PyObject *args) {
  PyObject      *po;
  py430_t       *p;
  char          *fn;
  msp430_byte_t *wp, *wp0;
  unsigned long *rom;
  int            i;

  if (!PyArg_ParseTuple(args, "Os", &po, &fn))
    return NULL;
  if (PyObject_Type(po) != (PyObject *) &Py430type) {
    PyErr_SetString(PyExc_TypeError, "py430object expected");
    return NULL;
  }
  p   = pyo((py430object *) po);
  rom = p->rom;
  if ((wp = (msp430_byte_t *) PyMem_Malloc(ROM_SIZE * sizeof(msp430_byte_t))) == NULL)
    return PyErr_NoMemory();
  memset(wp, 0, ROM_SIZE * sizeof(msp430_byte_t));
  if (soft430_ld_ihex(wp, fn) != MSP430_SUCCESS) {
    PyMem_Free(wp);
    PyErr_SetString(PyExc_RuntimeError, "ihex load failed");
    return NULL;
  }
  wp0 = wp;
  for (i = 0; i < ROM_SIZE; i++)
    *rom++ = *wp++;
  PyMem_Free(wp0);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(ihex_doc,
"load(obj, filename)\n\
\n\
Load the Intel hex file specified by filename\n\
into the obj returned by bind().");

static PyMethodDef funcs[] = {
  { "bind", (PyCFunction) pybind, METH_VARARGS, bind_doc },
  { "load", (PyCFunction) pyihex, METH_VARARGS, ihex_doc },
  { NULL, NULL },
};

PyDoc_STRVAR(module_doc,
"This module defines a high-performance MSP430F1611\n\
profiling object; it is intended to be used in conjunction\n\
with the _soft430 module.");

PyDoc_STRVAR(type_doc,
"See the module docs.");

static PyTypeObject Py430type = {
  PyObject_HEAD_INIT(NULL)
  0,
  "_py430.py430",
  sizeof(py430object),
  0,
  (destructor) del,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
  type_doc,
  (traverseproc) traverse,
  (inquiry) clear,
};

static char *rcsid = "$Id: py430.c,v 1.8 2007/07/26 22:37:37 DEV Exp $";

PyMODINIT_FUNC init_py430(void) {
  PyObject *m;

  /*** pull in the _soft430 module and its API */
  if (import_soft430() < 0)
    return;

  /*** init ourself */
  Py430type.ob_type = &PyType_Type;
  if (PyType_Ready(&Py430type) < 0)
    return;
  if ((m = Py_InitModule3("_py430", funcs, module_doc)) == NULL)
    return;
  if (PyModule_AddObject(m, "Py430Type", (PyObject *) &Py430type))
    return;
  if (PyModule_AddStringConstant(m, "__version__", rcsid))
    return;
#define DC(x) if (PyModule_AddIntConstant(m, #x, x)) return
#define REG_OFS 0
  DC(REG_OFS);
  DC(REG_SIZE);
#define ROM_OFS (REG_OFS + REG_SIZE)
  DC(ROM_OFS);
  DC(ROM_SIZE);
#define RAM_OFS (ROM_OFS + ROM_SIZE)
  DC(RAM_OFS);
  DC(RAM_SIZE);
#define PCC_OFS (RAM_OFS + RAM_SIZE)
  DC(PCC_OFS);
  DC(PCC_SIZE);
#define SP_OFS (PCC_OFS + PCC_SIZE)
  DC(SP_OFS);
  DC(SP_SIZE);
#define INS_OFS (SP_OFS + SP_SIZE)
  DC(INS_OFS);
  DC(INS_SIZE);
#define TCK_OFS (INS_OFS + INS_SIZE)
  DC(TCK_OFS);
  DC(TCK_SIZE);
#define MEM_OFS (TCK_OFS + TCK_SIZE)
  DC(MEM_OFS);
  DC(MEM_SIZE);
#define JMP_OFS (MEM_OFS + MEM_SIZE)
  DC(JMP_OFS);
  DC(JMP_SIZE);
#define CAL_OFS (JMP_OFS + JMP_SIZE)
  DC(CAL_OFS);
  DC(CAL_SIZE);
#define BRK_OFS (CAL_OFS + CAL_SIZE)
  DC(BRK_OFS);
  DC(BRK_SIZE);
#define PCT_OFS (BRK_OFS + BRK_SIZE)
  DC(PCT_OFS);
  DC(PCT_SIZE);
  DC(ARY_SIZE);

  /*** pull in the array module and ArrayType */
  if ((m = PyImport_ImportModule("array")) == NULL)
    return;
  if ((array = PyObject_GetAttrString(m, "ArrayType")) == NULL)
    return;
  Py_DECREF(m);
}

/*** EOF py430.c */

