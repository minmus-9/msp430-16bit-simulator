/***********************************************************************
 * soft430.h
 */

#ifndef __soft430_h__
#define __soft430_h__

#ifdef SOFT430_LIB
#include <sys/types.h>
#include <setjmp.h>
#include <stdio.h>

typedef unsigned char  msp430_byte_t;
typedef unsigned short msp430_word_t;
typedef unsigned long  msp430_host_long_t;

typedef struct msp430_ctx msp430_ctx_t;

typedef int (*msp430_reg_func)(msp430_ctx_t  *ctx,
			       int            reg,
			       msp430_word_t *val,
			       int            isWrite,
			       void          *xtra);

typedef int (*msp430_mem_func)(msp430_ctx_t  *ctx,
			       msp430_word_t  addr,
			       msp430_byte_t *val,
			       int            isWrite,
			       int            isWord,
			       void          *xtra);

typedef int (*msp430_sfr_func)(msp430_ctx_t  *ctx,
			       msp430_word_t  addr,
			       msp430_byte_t *val,
			       int            isWrite,
			       int            isWord,
			       void          *xtra);

typedef int (*msp430_trap_func)(msp430_ctx_t  *ctx,
				msp430_word_t  val,
				void          *xtra);

#define MSP430_PROFILE_CLEAR 0x0000

typedef void (*msp430_prof_func)(msp430_ctx_t *ctx,
				 int           insn,
				 int           ticks,
				 void         *xtra);

#define MSP430_NREGS 16
#define MSP430_NSFR  512
#define MSP430_NRAM  10240
#define MSP430_NPROG 49152

#define MSP430_BSL_BASE 0x0c00
#define MSP430_UF_BASE  0x1000
#define MSP430_RAM_BASE 0x1100
#define MSP430_ROM_BASE 0x4000

#define MSP430_INSN_TYPE1 1
#define MSP430_INSN_TYPE2 2
#define MSP430_INSN_TYPE3 3

typedef struct msp430_insn_info {
  msp430_word_t op;
  msp430_word_t type;
  msp430_word_t sreg;
  msp430_word_t sa;
  msp430_word_t dreg;
  msp430_word_t da;
  msp430_word_t daddr;
  msp430_word_t res;
  msp430_word_t bw;
  msp430_word_t mask;
  msp430_word_t sign;
  msp430_word_t ticks;
} msp430_insn_info_t;

struct msp430_ctx {
  msp430_reg_func        reg;
  void                  *reg_xtra;

  msp430_mem_func        mem;
  void                  *mem_xtra;

  msp430_word_t          insn_pc;
  msp430_word_t          op_ndx;
  msp430_word_t          opcode[3];
  msp430_insn_info_t     info;

  msp430_host_long_t     insns;
  msp430_host_long_t     ticks;

  int                    profile;
  int                    penable;
  int                    in_exec;

  msp430_prof_func       prof;
  void                  *prof_xtra;

  msp430_sfr_func        sfr;
  void                  *sfr_xtra;

  msp430_trap_func       trap;
  void                  *trap_xtra;

  jmp_buf                env;
  msp430_word_t          exc_info[4];
};

/*** profile instruction indices */
#define MSP430_INSN_ALL   0
#define MSP430_INSN_MOV   1
#define MSP430_INSN_ADD   2
#define MSP430_INSN_ADDC  3
#define MSP430_INSN_SUBC  4
#define MSP430_INSN_SUB   5
#define MSP430_INSN_CMP   6
#define MSP430_INSN_DADD  7
#define MSP430_INSN_BIT   8
#define MSP430_INSN_BIC   9
#define MSP430_INSN_BIS  10
#define MSP430_INSN_XOR  11
#define MSP430_INSN_AND  12
#define MSP430_INSN_RRC  13
#define MSP430_INSN_SWPB 14
#define MSP430_INSN_RRA  15
#define MSP430_INSN_SXT  16
#define MSP430_INSN_PUSH 17
#define MSP430_INSN_CALL 18
#define MSP430_INSN_RETI 19
/*** these MUST be contiguous -- and last */
#define MSP430_INSN_JNZ  20
#define MSP430_INSN_JZ   21
#define MSP430_INSN_JNC  22
#define MSP430_INSN_JC   23
#define MSP430_INSN_JN   24
#define MSP430_INSN_JGE  25
#define MSP430_INSN_JL   26
#define MSP430_INSN_JMP  27

#define MSP430_INSN_CNT  27

/*** exceptions */
#define MSP430_SUCCESS          0x00
#define MSP430_EXC_BAD_ALIGN    0x01
#define MSP430_EXC_BAD_SR       0x02
#define MSP430_EXC_ENOSYS       0x03
#define MSP430_EXC_BAD_EXC      0x04
#define MSP430_EXC_ENXIO        0x05
#define MSP430_EXC_BAD_ADDRMODE 0x06
#define MSP430_EXC_NULL_CTX     0x07
#define MSP430_EXC_BAD_REG      0x08
#define MSP430_EXC_BAD_WORD     0x09
#define MSP430_EXC_BAD_STORE    0x0a
#define MSP430_EXC_BAD_EXEC     0x0b
#define MSP430_EXC_EINVAL       0x0c
#define MSP430_EXC_OS_ERROR     0x0d
#define MSP430_EXC_CPU_TIME     0x0e
#define MSP430_EXC_BAD_BYTE     0x0f
#define MSP430_EXC_BAD_TRAP     0x10
#define MSP430_EXC_BAD_SFR      0x11
#define MSP430_EXC_WDT_FAULT    0x12
#define MSP430_EXC_FC_FAULT     0x13
#define MSP430_EXC_BAD_IHEX     0x14
#define MSP430_EXC_PYTHON_FAULT 0x15
#define MSP430_EXC_PM_CHANGE    0x16
#define MSP430_EXC_DEBUG        0x17

#define MSP430_EXC_CNT          24

/*** sr bits */
#define _msp430_sr(c)        (msp430_getreg(c, 2))
#define _msp430_getsr(c,m)   ((_msp430_sr(c) & m) ? 1 : 0)
#define _msp430_setsr(c,m,v) (msp430_setreg(c, 2, (v) ? \
                                (_msp430_sr(c) | (m)) : \
                                (_msp430_sr(c) & (0xffff ^ (m)))))

#define MSP430_C_MASK 0x001
#define MSP430_Z_MASK 0x002
#define MSP430_N_MASK 0x004
#define MSP430_I_MASK 0x008
#define MSP430_V_MASK 0x100

#define msp430_setc(ctx,v) _msp430_setsr(ctx,MSP430_C_MASK,v)
#define msp430_setz(ctx,v) _msp430_setsr(ctx,MSP430_Z_MASK,v)
#define msp430_setn(ctx,v) _msp430_setsr(ctx,MSP430_N_MASK,v)
#define msp430_seti(ctx,v) _msp430_setsr(ctx,MSP430_I_MASK,v)
#define msp430_setv(ctx,v) _msp430_setsr(ctx,MSP430_V_MASK,v)
#define msp430_getc(ctx)   _msp430_getsr(ctx,MSP430_C_MASK)
#define msp430_getz(ctx)   _msp430_getsr(ctx,MSP430_Z_MASK)
#define msp430_getn(ctx)   _msp430_getsr(ctx,MSP430_N_MASK)
#define msp430_geti(ctx)   _msp430_getsr(ctx,MSP430_I_MASK)
#define msp430_getv(ctx)   _msp430_getsr(ctx,MSP430_V_MASK)

extern int msp430_get_regf(msp430_ctx_t     *ctx,
			   msp430_reg_func  *rf,
			   void            **rx);

extern int msp430_set_regf(msp430_ctx_t    *ctx,
			   msp430_reg_func  rf,
			   void            *rx);

extern int msp430_get_reg(msp430_ctx_t  *ctx,
                          int            reg,
                          msp430_word_t *val);

extern int msp430_set_reg(msp430_ctx_t *ctx,
                          int           reg,
                          msp430_word_t val);

extern int msp430_clear_regs(msp430_ctx_t *ctx);

extern int msp430_wfetch(msp430_ctx_t  *ctx,
			 msp430_word_t  addr,
			 msp430_word_t *res);

extern int msp430_wstore(msp430_ctx_t  *ctx,
			 msp430_word_t  addr,
			 msp430_word_t  val);

extern int msp430_bfetch(msp430_ctx_t  *ctx,
			 msp430_word_t  addr,
			 msp430_byte_t *res);

extern int msp430_bstore(msp430_ctx_t  *ctx,
			 msp430_word_t  addr,
			 msp430_byte_t  val);

extern int msp430_get_mem(msp430_ctx_t     *ctx,
			  msp430_mem_func  *mem,
			  void            **mem_xtra);

extern int msp430_set_mem(msp430_ctx_t    *ctx,
			  msp430_mem_func  mem,
			  void            *mem_xtra);

extern msp430_byte_t *msp430_get_ram(msp430_ctx_t *ctx);

extern msp430_byte_t *msp430_get_rom(msp430_ctx_t *ctx);

extern int msp430_get_sfr(msp430_ctx_t     *ctx,
			  msp430_sfr_func  *sfr,
			  void            **sfr_xtra);

extern int msp430_set_sfr(msp430_ctx_t    *ctx,
			  msp430_sfr_func  sfr,
			  void            *sfr_xtra);

int msp430_sfr_byte(msp430_ctx_t  *ctx,
		    msp430_word_t  addr,
		    msp430_byte_t *val,
		    int            isWrite);

int msp430_sfr_word(msp430_ctx_t  *ctx,
		    msp430_word_t  addr,
		    msp430_word_t *val,
		    int            isWrite);

extern const char *msp430_sfr_name(msp430_word_t addr);

extern int msp430_get_prof(msp430_ctx_t      *ctx,
			   msp430_prof_func  *pf,
			   void             **px);

extern int msp430_set_prof(msp430_ctx_t     *ctx,
			   msp430_prof_func  pf,
			   void             *px);

extern int msp430_clear_prof(msp430_ctx_t *ctx);

#define MSP430_PROFILE_OFF   0
#define MSP430_PROFILE_NOW  -1
#define MSP430_PROFILE_NEXT  1

extern void msp430_prof_enable(msp430_ctx_t *ctx,
			       int           on);

extern int msp430_get_trap(msp430_ctx_t      *ctx,
			   msp430_trap_func  *trap,
			   void             **trap_xtra);

extern int msp430_set_trap(msp430_ctx_t     *ctx,
			   msp430_trap_func  trap,
			   void             *trap_xtra);

extern int msp430_init(msp430_ctx_t *ctx);

extern int msp430_fini(msp430_ctx_t *ctx);

extern int msp430_reset(msp430_ctx_t *ctx);

extern int msp430_load_ihex(msp430_byte_t *rom, char *ifile);

extern int msp430_dump(msp430_ctx_t *ctx, FILE *fp);

extern int msp430_run(msp430_ctx_t       *ctx,   /*** ctx ptr */
		      msp430_host_long_t  maxi,  /*** max #insns to execute */
		      msp430_host_long_t  maxc); /*** max #clocks to execute */

extern const char *msp430_strerror(int errcode);

extern int msp430_main(int argc, char *argv[]);

typedef struct msp430api {
  msp430_ctx_t *(*get_ctx)(void *);
  int           (*get_sfr)(msp430_ctx_t *, msp430_sfr_func *, void **);
  int           (*set_reg)(msp430_ctx_t *, msp430_reg_func, void *);
  int           (*set_mem)(msp430_ctx_t *, msp430_mem_func, void *);
  int           (*set_prf)(msp430_ctx_t *, msp430_prof_func, void *);
  int           (*ld_ihex)(msp430_byte_t *, char *);
  msp430_word_t (*get_reg)(msp430_ctx_t *, int);
} msp430api_t;

#ifdef PYTHON_API
static msp430api_t *__msp430api = NULL;

#define soft430_get_ctx(o)     (__msp430api->get_ctx((PyObject *) (o)))
#define soft430_get_sfr(o,f,x) (__msp430api->get_sfr(o, f, x))
#define soft430_set_reg(o,f,x) (__msp430api->set_reg(o, f, x))
#define soft430_set_mem(o,f,x) (__msp430api->set_mem(o, f, x))
#define soft430_set_prf(o,f,x) (__msp430api->set_prf(o, f, x))
#define soft430_ld_ihex(p,n)   (__msp430api->ld_ihex(p, n))
#define soft430_get_reg(c,n)   (__msp430api->get_reg(c, n))

static int import_soft430(void) {
  PyObject *m, *o;

  if ((m = PyImport_ImportModule("_soft430")) == NULL)
    return -1;
  o = PyObject_GetAttrString(m, "__C_API");
  Py_DECREF(m);
  if (o == NULL)
    return -1;
  if (PyCObject_Check(o))
    __msp430api = (msp430api_t *) PyCObject_AsVoidPtr(o);
  Py_DECREF(o);
  return 0;
}
#endif

#else
/*********************************************************************
 * !SOFT430_LIB -- for c programs to use
 */

#if defined(SOFTMSP) && !defined(NO_PROFILE)
#warning "Including profiling instructions"

/*** turn profiling on */
#define MSP430_PROF_ENABLE  __asm__ __volatile__("mov #4, r3")

/*** clear profiler stats */
#define MSP430_PROF_CLEAR   __asm__ __volatile__("mov #2, r3")

/*** turn profiling off */
#define MSP430_PROF_DISABLE __asm__ __volatile__("mov #1, r3")

/*** save x into r15 and ~x into r14 */
#define MSP430_SAVE_R15(x) { \
  uint16_t __r15 = x; \
  __asm__ __volatile__("mov %0,r15\n\t": : "r" (__r15)); \
  __asm__ __volatile__("mov r15,r14\n\t"); \
  __asm__ __volatile__("xor #-1,r14\n\t"); \
}

/*** halt the simulation by setting SR.CPUOFF */
#define MSP430_TRAP_HALT    __asm__ __volatile__("bis #0x0010, r2")

/*** halt the simulation with an error */
#define MSP430_TRAP_ERROR   __asm__ __volatile__("mov #-1, r3")

#else

#warning "Not including profiling instructions"

/*** stub everything out to noop */
#include <stdio.h>
#define MSP430_PROF_ENABLE
#define MSP430_PROF_CLEAR
#define MSP430_PROF_DISABLE
#ifdef  NO_PROFILE
#define MSP430_SAVE_R15(x)
#warning "Save to r15 disabled"
#else
#define MSP430_SAVE_R15(x) printf("SAVE_R15: result=%d\n", x)
#endif
#define MSP430_TRAP_HALT
#define MSP430_TRAP_ERROR
#endif

#endif
/*** SOFTMSP_LIB */
/* !SOFT430_LIB */

#endif

/*** EOF soft430.h */

