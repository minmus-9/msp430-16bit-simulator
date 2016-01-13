/***********************************************************************
 * soft430.c
 */

#define SOFT430_LIB
#include "soft430.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/*** bad sr bits */
#define MSP430_BAD_SR 0xfe00

/*** exception handling */
#define MSP430_EXCEPTION(ctx,n,v1,v2) { \
  ctx->exc_info[0] = n; \
  ctx->exc_info[1] = v1; \
  ctx->exc_info[2] = v2; \
  ctx->exc_info[3] = __LINE__; \
  longjmp(ctx->env, \
          ctx->exc_info[0] ? ctx->exc_info[0] : MSP430_EXC_BAD_EXC); \
}

#define MSP430_EXC0(n)       MSP430_EXCEPTION(ctx,n,0,0)
#define MSP430_EXC1(n,v1)    MSP430_EXCEPTION(ctx,n,v1,0)
#define MSP430_EXC2(n,v1,v2) MSP430_EXCEPTION(ctx,n,v1,v2)

/***************************************
 * memory access
 */

static int msp430_mem(msp430_ctx_t  *ctx,
		      msp430_word_t  addr,
		      msp430_byte_t *val,
		      int            isWrite,
		      int            isWord) {
  return ctx->mem(ctx, addr, val, isWrite, isWord, ctx->mem_xtra);
}

static msp430_word_t msp430_byte_fetch(msp430_ctx_t  *ctx,
                                       msp430_word_t  addr) {
  msp430_byte_t val;
  int           rc;

  if ((rc = msp430_mem(ctx, addr, &val, 0, 0)) != MSP430_SUCCESS)
    MSP430_EXC1(rc, addr);
  return val;
}

static msp430_word_t msp430_word_fetch(msp430_ctx_t  *ctx,
                                       msp430_word_t  addr) {
  msp430_byte_t val;
  msp430_word_t ret;
  int           rc;

  if (addr & 1)
    MSP430_EXC1(MSP430_EXC_BAD_ALIGN, addr);
  /*** do high byte first for sfrs */
  if ((rc = msp430_mem(ctx, addr + 1, &val, 0, 1)) != MSP430_SUCCESS)
    MSP430_EXC1(rc, addr);
  ret = val << 8;
  if ((rc = msp430_mem(ctx, addr, &val, 0, 1)) != MSP430_SUCCESS)
    MSP430_EXC1(rc, addr);
  ret |= val;
  return ret;
}

static void msp430_byte_store(msp430_ctx_t  *ctx,
                              msp430_word_t  addr,
                              msp430_word_t  val) {
  msp430_byte_t b = val;
  int           rc;

  if ((rc = msp430_mem(ctx, addr, &b, 1, 0)) != MSP430_SUCCESS)
    MSP430_EXC2(rc, addr, val);
}

static void msp430_word_store(msp430_ctx_t  *ctx,
                              msp430_word_t  addr,
                              msp430_word_t  val) {
  msp430_byte_t b;
  int           rc;

  if (addr & 1)
    MSP430_EXC1(MSP430_EXC_BAD_ALIGN, addr);
  /*** do high byte first for sfrs */
  b = (val >> 8) & 0xff;
  if ((rc = msp430_mem(ctx, addr + 1, &b, 1, 1)) != MSP430_SUCCESS)
    MSP430_EXC2(rc, addr, val);
  b = val & 0xff;
  if ((rc = msp430_mem(ctx, addr, &b, 1, 1)) != MSP430_SUCCESS)
    MSP430_EXC2(rc, addr, val);
}

/***************************************
 * register access
 */

static int msp430_reg(msp430_ctx_t  *ctx,
		      int            reg,
		      msp430_word_t *val,
		      int            isWrite) {
  msp430_word_t pm = 0;
  int           rc;

  if ((reg == 3) || (reg < 0) || (reg >= MSP430_NREGS))
    return MSP430_EXC_BAD_REG;
  if (isWrite) {
    if (reg == 2) {
      if (*val & MSP430_BAD_SR)
	return MSP430_EXC_BAD_SR;
      pm = *val & 0x00f0;
    } else {
      if ((reg < 2) && (*val & 0x1))
	return MSP430_EXC_BAD_ALIGN;
    }
  }
  if ((rc = ctx->reg(ctx, reg, val, isWrite, ctx->reg_xtra)) != MSP430_SUCCESS)
    return rc;
  return pm ? MSP430_EXC_PM_CHANGE : MSP430_SUCCESS;
}

static msp430_word_t msp430_getreg(msp430_ctx_t *ctx, int reg) {
  msp430_word_t val;
  int           rc;

  if ((rc = msp430_reg(ctx, reg, &val, 0)) != MSP430_SUCCESS)
    MSP430_EXC0(rc);
  return val;
}

static void msp430_setreg(msp430_ctx_t  *ctx,
			  int            reg,
			  msp430_word_t  val) {
  int rc;

  if ((rc = msp430_reg(ctx, reg, &val, 1)) != MSP430_SUCCESS)
    MSP430_EXC0(rc);
}

/***************************************
 * instruction fetch/decode
 */

static msp430_word_t msp430_next_word(msp430_ctx_t *ctx) {
  msp430_word_t ret, pc;

  ctx->in_exec = 1; /* mem() should inspect this and raise exc */
  ret = msp430_word_fetch(ctx, pc = msp430_getreg(ctx, 0));
  ctx->in_exec = 0;
  ctx->opcode[ctx->op_ndx++] = ret;
  
  msp430_setreg(ctx, 0, (pc + 2) & 0xffff);
  return ret;
}

static void msp430_exec_prep(msp430_ctx_t *ctx) {
  msp430_word_t       op, it, ticks=0, sa=0, sreg=0, da=0, dreg=0, bw=0;
  msp430_insn_info_t *info = &ctx->info;
  int                 cg, imm, immed;

  ctx->insn_pc  = msp430_getreg(ctx, 0);
  ctx->op_ndx   = 0;
  op            = msp430_next_word(ctx);

  for (;;) {
    /*** dual-operand */
    if (op & 0xc000) {
      it   = MSP430_INSN_TYPE1;
      dreg = op & 0xf;
      op   = op >> 4;
      sa   = op & 0x3;
      op   = op >> 2;
      bw   = op & 0x1;
      op   = op >> 1;
      da   = op & 0x1;
      op   = op >> 1;
      sreg = op & 0xf;
      op   = op >> 4;
      op   = (op & 0xf) + MSP430_INSN_MOV - 4;

      cg   = (sreg == 3) || ((sreg == 2) && (sa > 1));
      imm  = (sreg == 0) && (sa == 3);

      /*** no byte ops on r2 */
      if (bw && (((sa == 0) && (sreg == 2)) || ((da == 0) && (dreg == 2))))
	MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);

      /*** do ticks */
      for (;;) {
	if (cg) {
	  ticks = 1;
	  break;
	}
	if (imm) {
	  ticks = 2;
	  break;
	}
	switch (sa) {
  	  case 0: { ticks = 1; break; }
  	  case 1: { ticks = 3; break; }
  	  case 2: { ticks = 2; break; }
  	  case 3: { ticks = 2; break; }
	}
	break;
      }
      if ((da == 1) && (dreg != 2) && (dreg != 3)) {
	ticks += 3;
      } else {
	if ((dreg == 0) && ((sa == 0) || (sa == 3)))
	  ticks += 1;
      }

      break;
    }

    /*** jumps */
    if (op & 0x2000) {
      msp430_word_t ofs = (op & 0x3ff) << 1;

      it    = MSP430_INSN_TYPE3;
      ofs   = ((ofs ^ 0x0400) + 0xfc00) & 0xffff;
      op    = op >> 10;
      op    = (op & 0x7) + MSP430_INSN_JNZ;
      sreg  = ofs;
      ticks = 2;
      break;
    }

    /*** single-operand */
    if (op & 0x1000) {
      /*** we advanced r0 one word -- barf if unimplemented */
      if (op & 0x0c00)
	MSP430_EXC1(MSP430_EXC_ENOSYS, op);
      if ((op & 0x0380) == 0x0380)
	MSP430_EXC1(MSP430_EXC_ENOSYS, op);
      it    = MSP430_INSN_TYPE2;
      dreg  = op & 0xf;
      op    = op >> 4;
      da    = op & 0x3;
      op    = op >> 2;
      bw    = op & 0x1;
      op    = op >> 1;
      op    = (op & 0x7) + MSP430_INSN_RRC;

      cg    = (dreg == 3) || ((dreg == 2) && (da > 1));
      imm   = (dreg == 0) && (da == 3);
      immed = cg || imm;

      /*** no byte ops on r2 */
      if (bw && (da == 0) && (dreg == 2))
	MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);
      /*** no byte mode for swpb or sxt */
      if (bw && ((op == MSP430_INSN_SWPB) ||
		 (op == MSP430_INSN_SXT)))
	MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);

      /*** scope it out */
      switch (op) {
        case MSP430_INSN_RETI: {
	  /*** asm generates "reti r0" -- anything
	   *** else => cpu in the weeds
	   ***/
	  if (bw || da || dreg)
	    MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);
	  ticks = 5;
	  break;
	}

        case MSP430_INSN_CALL: {
	  /*** jump into sfrs makes no sense => cpu in weeds */
	  if (bw)
	    MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);
	  sreg = dreg; dreg = 0;
	  sa   = da;   da   = 0;
	  if (cg) {
	    ticks = 4;
	  } else {
	    switch (da) {
	      case 0: { ticks = 4; break; }
	      case 1: { ticks = 5; break; }
	      case 2: { ticks = 4; break; }
	      case 3: { ticks = 5; break; }
	    }
	  }
	  break;
	}

        case MSP430_INSN_PUSH: {
	  sreg = dreg; dreg = 0;
	  sa   = da;   da   = 0;
	  if (cg) {
	    /*** from msp430 errata: push #4 and push #8 don't work */
	    if ((dreg == 2) && (da > 1))
	      MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);
	    ticks = 3;
	  } else {
	    if (imm) {
	      ticks = 4;
	    } else {
	      switch (da) {
	        case 0: { ticks = 3; break; }
	        case 1: { ticks = 5; break; }
	        case 2: { ticks = 4; break; }
	        case 3: { ticks = 5; break; }
	      }
	    }
	  }
	  break;
	}

	/*** rrc, swpb, rra, sxt */
        default: {
	  if (immed)
	    MSP430_EXC1(MSP430_EXC_BAD_ADDRMODE, ctx->opcode[0]);
	  switch (da) {
  	    case 0: { ticks = 1; break; }
  	    case 1: { ticks = 4; break; }
  	    case 2: { ticks = 3; break; }
  	    case 3: { ticks = 3; break; }
	  }
	  break;
	}
      }
      break;
    }

    /*** everything else is illegal */
    MSP430_EXC1(MSP430_EXC_ENOSYS, ctx->opcode[0]);
    break;
  }
  /*** it'll work as far as we know -- save info */
  info->op    = op;
  info->type  = it;
  info->sreg  = sreg;
  info->sa    = sa;
  info->dreg  = dreg;
  info->da    = da;
  info->daddr = 0;
  info->res   = 0;
  info->bw    = bw;
  info->mask  = bw ? 0xff : 0xffff;
  info->sign  = bw ? 0x80 : 0x8000;
  info->ticks = ticks;
}

static void msp430_execute(msp430_ctx_t *ctx);

static void msp430_exec_one(msp430_ctx_t *ctx) {
  /*** fetch and decode */
  msp430_exec_prep(ctx);

  /*** execute */
  msp430_execute(ctx);

  /*** update cum info */
  ctx->insns++;
  ctx->ticks += ctx->info.ticks;

  /*** do profiling */
  if (ctx->profile) {
    ctx->prof(ctx, ctx->info.op, ctx->info.ticks, ctx->prof_xtra);
  } else {
    if (ctx->penable) {
      ctx->profile = 1;
    }
  }
  ctx->penable = 0;
}

/***************************************
 * helpers
 */

static void msp430__setzn(msp430_ctx_t  *ctx,
                          msp430_word_t  val,
                          msp430_word_t  mask,
                          msp430_word_t  sign) {
  msp430_setz(ctx, (val & mask) ? 0 : 1);
  msp430_setn(ctx, (val & sign) ? 1 : 0);
}

static void msp430_setzn(msp430_ctx_t  *ctx,
                         msp430_word_t  val) {
  msp430__setzn(ctx,
		val,
                ctx->info.mask,
                ctx->info.sign);
}

static msp430_word_t msp430_b2d(msp430_word_t v) {
  msp430_word_t r = 0, m = 1, n;
  int           i;

  for (i = 0; i < 4; i++) {
    n  = v & 0xf;
    v  = v >> 4;
    r += n * m;
    m *= 10;
  }
  return r;
}

static msp430_word_t msp430_d2b(msp430_host_long_t v,
				int bw,
				msp430_host_long_t *xtra) {
  msp430_word_t r = 0, m = 0, n;
  int           i;

  for (i = 0; i < (bw ? 2 : 4); i++) {
    n  = v % 10;
    v /= 10;
    r |= n << m;
    m += 4;
  }
  *xtra = v;
  return r;
}

static msp430_word_t msp430_getsrc(msp430_ctx_t *ctx) {
  msp430_word_t v, a, r;
  msp430_word_t sa = ctx->info.sa, sreg = ctx->info.sreg, bw = ctx->info.bw;

  if (sreg == 3) {
    switch (sa) {
      case 0: { return 0; }
      case 1: { return 1; }
      case 2: { return 2; }
      case 3: { return ctx->info.mask; }
    }
  }
  v = msp430_getreg(ctx, sreg);
  if (sa == 0)
    return v & ctx->info.mask;
  if (sreg == 2) {
    if (sa > 1) {
      return (sa & 1) ? 8 : 4;
    }
    a = 0;
  } else {
    a = v;
  }
  if ((sa == 3) && (sreg == 0)) {
    return msp430_next_word(ctx);
  }
  if (sa == 1) {
    a = (a + msp430_next_word(ctx)) & 0xffff;
  }
  if (bw) {
    r = msp430_byte_fetch(ctx, a);
  } else {
    r = msp430_word_fetch(ctx, a);
  }
  if (sa == 3) {
    if (sreg < 2) bw = 0;
    msp430_setreg(ctx, sreg, (v + (bw ? 1 : 2)) & 0xffff);
  }
  return r;
}

static msp430_word_t msp430_getdst(msp430_ctx_t *ctx) {
  msp430_word_t v, a, r;
  msp430_word_t da = ctx->info.da, dreg = ctx->info.dreg, bw = ctx->info.bw;

  if (dreg == 3) {
    switch (da) {
      case 0: { return 0; }
      case 1: { return 1; }
      case 2: { return 2; }
      case 3: { return ctx->info.mask; }
    }
  }
  v = msp430_getreg(ctx, dreg);
  if (da == 0)
    return v & ctx->info.mask;
  if (dreg == 2) {
    if (da > 1) {
      return (da & 1) ? 8 : 4;
    }
    a = 0;
  } else {
    a = v;
  }
  if (da == 1) {
    a = (a + msp430_next_word(ctx)) & 0xffff;
  }
  ctx->info.daddr = a;
  if (bw) {
    r = msp430_byte_fetch(ctx, a);
  } else {
    r = msp430_word_fetch(ctx, a);
  }
  if (da == 3) {
    if (dreg < 2) bw = 0;
    msp430_setreg(ctx, dreg, (v + (bw ? 1 : 2)) & 0xffff);
  }
  return r;
}

static void msp430_setdst(msp430_ctx_t *ctx, msp430_word_t v) {
  msp430_word_t da = ctx->info.da, dreg = ctx->info.dreg, bw = ctx->info.bw;

  v &= ctx->info.mask;
  ctx->info.res = v;
  if ((dreg == 3) || ((dreg == 2) && (da > 1)))
    return;
  if (da == 0) {
    msp430_setreg(ctx, dreg, v);
  } else {
    if (bw) {
      msp430_byte_store(ctx, ctx->info.daddr, v);
    } else {
      msp430_word_store(ctx, ctx->info.daddr, v);
    }
  }
}

static void msp430_get2(msp430_ctx_t *ctx,
			msp430_word_t *a,
			msp430_word_t *b) {
  *a = msp430_getsrc(ctx);
  *b = msp430_getdst(ctx);
}

static msp430_word_t msp430_add(msp430_ctx_t  *ctx,
                                msp430_word_t  x,
                                msp430_word_t  y,
                                int            isAdd,
				int            doStore) {
  msp430_word_t      res;
  msp430_word_t      m = ctx->info.mask;
  msp430_word_t      s = ctx->info.sign;
  msp430_host_long_t rres;

  if (!isAdd)
    x ^= m;
  rres = y + x + msp430_getc(ctx);
  res  = rres & m;
  if (doStore) {
    /*** do this first -- might raise exception */
    msp430_setdst(ctx, res);
    /*** the rest of this never raises an exception */
  }
  msp430__setzn(ctx, res, m, s);
  msp430_setc(ctx, ((rres >> 1) & s) ? 1 : 0);
  rres ^= x ^ y;
  msp430_setv(ctx, ((rres ^ (rres >> 1)) & s) ? 1 : 0);
  return res;
}

/***************************************
 * instruction implementations
 */

static void msp430_op_ill(msp430_ctx_t *ctx) {
  MSP430_EXC0(MSP430_EXC_ENOSYS);
}

static void msp430_op_mov(msp430_ctx_t *ctx) {
  msp430_insn_info_t *i = &ctx->info;
  msp430_word_t       x, y;

  msp430_get2(ctx, &x, &y);
  if (!x ||
      (i->dreg != 3) ||
      (i->da != 0) ||
      !(((i->sreg == 0) && (i->sa == 3)) ||
	((i->sreg == 3) && (i->sa != 0)) ||
	((i->sreg == 2) && (i->sa  > 1)))) {
      msp430_setdst(ctx, x);
  } else {
    ctx->trap(ctx, x, ctx->trap_xtra);
  }
}

static void msp430_op_addc(msp430_ctx_t *ctx) {
  msp430_word_t x, y;

  msp430_get2(ctx, &x, &y);
  msp430_add(ctx, x, y, 1, 1);
}

static void msp430_op_add(msp430_ctx_t *ctx) {
  msp430_setc(ctx, 0);
  msp430_op_addc(ctx);
}

static void msp430_op_subc(msp430_ctx_t *ctx) {
  msp430_word_t x, y;

  msp430_get2(ctx, &x, &y);
  msp430_add(ctx, x, y, 0, 1);
}

static void msp430_op_sub(msp430_ctx_t *ctx) {
  msp430_setc(ctx, 1);
  msp430_op_subc(ctx);
}

static void msp430_op_cmp(msp430_ctx_t *ctx) {
  msp430_word_t x, y;

  msp430_get2(ctx, &x, &y);
  msp430_setc(ctx, 1);
  msp430_add(ctx, x, y, 0, 0);
}

static void msp430_op_dadd(msp430_ctx_t *ctx) {
  msp430_host_long_t rres;
  msp430_word_t      x, y, res;

  msp430_get2(ctx, &x, &y);
  x    = msp430_b2d(x);
  y    = msp430_b2d(y);
  rres = x + y + msp430_getc(ctx);
  res  = msp430_d2b(rres, ctx->info.bw, &rres) & ctx->info.mask;

  msp430_setdst(ctx, res);
  msp430_setzn(ctx, res);
  msp430_setc(ctx, rres ? 1 : 0);
}

static void msp430_op_bit(msp430_ctx_t *ctx) {
  msp430_word_t x, y, res;

  msp430_get2(ctx, &x, &y);
  res = x & y & ctx->info.mask;
  msp430_setzn(ctx, res);
  msp430_setc(ctx, msp430_getz(ctx) ^ 1);
  msp430_setv(ctx, 0);
}

static void msp430_op_bic(msp430_ctx_t *ctx) {
  msp430_word_t x, y;

  msp430_get2(ctx, &x, &y);
  msp430_setdst(ctx, (x ^ 0xffff) & y & ctx->info.mask);
}

static void msp430_op_bis(msp430_ctx_t *ctx) {
  msp430_word_t x, y;

  msp430_get2(ctx, &x, &y);
  msp430_setdst(ctx, (x | y) & ctx->info.mask);
}

static void msp430_op_xor(msp430_ctx_t *ctx) {
  msp430_word_t x, y, res;

  msp430_get2(ctx, &x, &y);
  res = (x ^ y) & ctx->info.mask;
  msp430_setdst(ctx, res);
  msp430_setzn(ctx, res);
  msp430_setc(ctx, msp430_getz(ctx) ^ 1);
  msp430_setv(ctx, (x & y & ctx->info.sign) ? 1 : 0);
}

static void msp430_op_and(msp430_ctx_t *ctx) {
  msp430_word_t x, y, res;

  msp430_get2(ctx, &x, &y);
  res = x & y & ctx->info.mask;
  msp430_setdst(ctx, res);
  msp430_setzn(ctx, res);
  msp430_setc(ctx, msp430_getz(ctx) ^ 1);
  msp430_setv(ctx, 0);
}

static void msp430_op_rrc(msp430_ctx_t *ctx) {
  msp430_word_t val;
  msp430_byte_t c;

  val = msp430_getdst(ctx);
  c   = val & 0x1;
  val = (val >> 1) | (msp430_getc(ctx) ? ctx->info.sign : 0);
  msp430_setdst(ctx, val);
  msp430_setzn(ctx, val);
  msp430_setc(ctx, c);
  msp430_setv(ctx, 0);
}

static void msp430_op_swpb(msp430_ctx_t *ctx) {
  msp430_word_t val;

  val = msp430_getdst(ctx);
  val = ((val >> 8) | (val << 8)) & 0xffff;
  msp430_setdst(ctx, val);
}

static void msp430_op_rra(msp430_ctx_t *ctx) {
  msp430_word_t val, s = ctx->info.sign;
  msp430_byte_t c;

  val = msp430_getdst(ctx);
  c   = val & 0x1;
  val = (((val ^ s) >> 1) + (s | (s >> 1))) & ctx->info.mask;
  msp430_setdst(ctx, val);
  msp430_setzn(ctx, val);
  msp430_setc(ctx, c);
  msp430_setv(ctx, 0);
}

static void msp430_op_sxt(msp430_ctx_t *ctx) {
  msp430_word_t val;

  val = msp430_getdst(ctx);
  val = (((val & 0xff) ^ 0x80) + 0xff80) & 0xffff;
  msp430_setdst(ctx, val);
  msp430_setzn(ctx, val);
  msp430_setc(ctx, msp430_getz(ctx) ^ 1);
  msp430_setv(ctx, 0);
}

static void msp430_op_push(msp430_ctx_t *ctx) {
  msp430_word_t val;

  val = msp430_getsrc(ctx);
  msp430_setreg(ctx, 1, (msp430_getreg(ctx, 1) - 2) & 0xffff);
  /*** XXX special case for "push sp"
   *** XXX try "push @sp+" on HW and see what really happens
   ***/
  if ((ctx->info.sreg == 1) && (ctx->info.sa == 0))
    val = msp430_getreg(ctx, 1);
  msp430_wstore(ctx, msp430_getreg(ctx, 1), val);
}

static void msp430_op_call(msp430_ctx_t *ctx) {
  msp430_word_t addr, val;

  val = msp430_getsrc(ctx);
  if (val & 0x1)
    MSP430_EXC1(MSP430_EXC_BAD_ALIGN, val);
  msp430_setreg(ctx, 1, addr = ((msp430_getreg(ctx, 1) - 2) & 0xffff));
  msp430_wstore(ctx, addr, msp430_getreg(ctx, 0));
  msp430_setreg(ctx, 0, val);
}

static void msp430_op_reti(msp430_ctx_t *ctx) {
  msp430_word_t sp, sr;

  sp = msp430_getreg(ctx, 1);
  sr = msp430_word_fetch(ctx, sp);
  sp = (sp + 2) & 0xffff;
  msp430_setreg(ctx, 0, msp430_word_fetch(ctx, sp));
  sp = (sp + 2) & 0xffff;
  msp430_setreg(ctx, 1, sp);
  /*** do this last -- for pwr mgt */
  msp430_setreg(ctx, 2, sr);
}

static void msp430_op_jmp(msp430_ctx_t *ctx) {
  msp430_setreg(ctx, 0, (msp430_getreg(ctx, 0) + ctx->info.sreg) & 0xffff);
}

static void msp430_op_jnz(msp430_ctx_t *ctx) {
  if (!msp430_getz(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jz(msp430_ctx_t *ctx) {
  if (msp430_getz(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jnc(msp430_ctx_t *ctx) {
  if (!msp430_getc(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jc(msp430_ctx_t *ctx) {
  if (msp430_getc(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jn(msp430_ctx_t *ctx) {
  if (msp430_getn(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jge(msp430_ctx_t *ctx) {
  if (msp430_getn(ctx) == msp430_getv(ctx)) msp430_op_jmp(ctx);
}

static void msp430_op_jl(msp430_ctx_t *ctx) {
  if (msp430_getn(ctx) != msp430_getv(ctx)) msp430_op_jmp(ctx);
}

static void (*optab[28])(msp430_ctx_t *) = {
  msp430_op_ill,
  msp430_op_mov,
  msp430_op_add,
  msp430_op_addc,
  msp430_op_subc,
  msp430_op_sub,
  msp430_op_cmp,
  msp430_op_dadd,
  msp430_op_bit,
  msp430_op_bic,
  msp430_op_bis,
  msp430_op_xor,
  msp430_op_and,
  msp430_op_rrc,
  msp430_op_swpb,
  msp430_op_rra,
  msp430_op_sxt,
  msp430_op_push,
  msp430_op_call,
  msp430_op_reti,
  msp430_op_jnz,
  msp430_op_jz,
  msp430_op_jnc,
  msp430_op_jc,
  msp430_op_jn,
  msp430_op_jge,
  msp430_op_jl,
  msp430_op_jmp,
};

static void msp430_execute(msp430_ctx_t *ctx) {
  optab[ctx->info.op](ctx);
}

/***********************************************************************
 * public sfr access
 */

int msp430_sfr_byte(msp430_ctx_t  *ctx,
		    msp430_word_t  addr,
		    msp430_byte_t *val,
		    int            isWrite) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (val == NULL)
    return MSP430_EXC_EINVAL;
  return ctx->sfr(ctx, addr, val, isWrite, 0, ctx->sfr_xtra);
}

int msp430_sfr_word(msp430_ctx_t  *ctx,
		    msp430_word_t  addr,
		    msp430_word_t *val,
		    int            isWrite) {
  msp430_byte_t b;
  msp430_word_t w;
  int           rc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (val == NULL)
    return MSP430_EXC_EINVAL;
  b = (*val >> 8) & 0xff;
  if ((rc = ctx->sfr(ctx, addr + 1, &b,
		     isWrite, 1, ctx->sfr_xtra)) != MSP430_SUCCESS)
    return rc;
  w = b << 8;
  b = *val & 0xff;
  if ((rc = ctx->sfr(ctx, addr + 1, &b,
		     isWrite, 1, ctx->sfr_xtra)) != MSP430_SUCCESS)
    return rc;
  w |= b;
  return MSP430_SUCCESS;
}

/***********************************************************************
 * default register accessor
 */

static int msp430_dfl_reg(msp430_ctx_t  *ctx,
			  int            reg,
			  msp430_word_t *val,
			  int            isWrite,
			  void          *xtra) {
  msp430_word_t *regs = (msp430_word_t *) xtra;

  if (isWrite)
    regs[reg] = *val;
  else
    *val = regs[reg];
  return MSP430_SUCCESS;
}

/***********************************************************************
 * default memory accessor
 */

typedef struct msp430_mem {
  msp430_byte_t rom[MSP430_NPROG];
  msp430_byte_t ram[MSP430_NRAM];
} msp430_mem_t;

static int msp430_dfl_mem(msp430_ctx_t  *ctx,
			  msp430_word_t  addr,
			  msp430_byte_t *val,
			  int            isWrite,
			  int            isWord,
			  void          *xtra) {
  msp430_mem_t *m = (msp430_mem_t *) xtra;

  if (addr & 0xc000) {
    /*** program flash */
    if (isWrite)
      return MSP430_EXC_BAD_STORE;
    addr -= MSP430_ROM_BASE;
    *val = m->rom[addr];
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
      m->ram[addr] = *val;
    else
      *val = m->ram[addr];
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
      m->ram[addr] = *val;
    else
      *val = m->ram[addr];
    return MSP430_SUCCESS;
  }
  /*** sfrs */
  if (ctx->in_exec)
    return MSP430_EXC_BAD_EXEC;
  return ctx->sfr(ctx, addr, val, isWrite, isWord, ctx->sfr_xtra);
}

/***********************************************************************
 * default profiler
 */

typedef struct msp430_dfl_prof {
  msp430_host_long_t insns[MSP430_INSN_CNT + 1];
  msp430_host_long_t ticks[MSP430_INSN_CNT + 1];
} msp430_dfl_prof_t;

static void msp430_dfl_prof(msp430_ctx_t *ctx,
			    int           op,
			    int           ticks,
			    void         *xtra) {
  msp430_dfl_prof_t *p = (msp430_dfl_prof_t *) xtra;

  if (op) {
    p->insns[MSP430_INSN_ALL]++;
    p->insns[op]++;
    p->ticks[MSP430_INSN_ALL] += ticks;
    p->ticks[op] += ticks;
  } else {
    memset(p, 0, sizeof(msp430_dfl_prof_t));
  }
}

/***********************************************************************
 * default trap handler
 */

static int msp430_dfl_trap(msp430_ctx_t  *ctx,
			   msp430_word_t  val,
			   void          *unused) {
  switch (val) {
    case 1: {
      msp430_prof_enable(ctx, MSP430_PROFILE_OFF);
      break;
    }
    case 2: {
      msp430_clear_prof(ctx);
      break;
    }
    case 4: {
      msp430_prof_enable(ctx, MSP430_PROFILE_NEXT);
      break;
    }
    default: {
      return MSP430_EXC_BAD_TRAP;
    }
  }
  return MSP430_SUCCESS;
}

/***********************************************************************
 * default sfr handler
 */

typedef struct msp430_sfr {
  msp430_byte_t sfr[MSP430_NSFR];
} msp430_sfr_t;

static const char *sfrn[MSP430_NSFR] = {
  /* 0x000 */
  "IE1", "IE2", "IFG1", "IFG2", "ME1", "ME2", NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x010 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  "P3IN", "P3OUT", "P3DIR", "P3SEL", "P4IN", "P4OUT", "P4DIR", "P4SEL",
  /* 0x020 */
  "P1IN", "P1OUT", "P1DIR", "P1IFG", "P1IES", "P1IE", "P1SEL", NULL,
  "P2IN", "P2OUT", "P2DIR", "P2IFG", "P2IES", "P2IE", "P2SEL", NULL,
  /* 0x030 */
  "P5IN", "P5OUT", "P5DIR", "P5SEL", "P6IN", "P6OUT", "P6DIR", "P6SEL",
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x040 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x050 */
  "I2CIE", "I2CIFG", "I2CNDAT", NULL, NULL, "SVSCTL", "DCOCTL", "BCSCTL1",
  "BCSCTL2", "CACTL1", "CACTL2", "CAPD", NULL, NULL, NULL, NULL,
  /* 0x060 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x070 */
  "U0CTL", "U0TCTL", "U0RCTL", "U0MCTL",
  "U0BR0", "U0BR1", "U0RXBUF", "U0TXBUF",
  "U1CTL", "U1TCTL", "U1RCTL", "U1MCTL",
  "U1BR0", "U1BR1", "U1RXBUF", "U1TXBUF",
  /* 0x080 */
  "ADC12MCTL0", "ADC12MCTL1", "ADC12MCTL2", "ADC12MCTL3", 
  "ADC12MCTL4", "ADC12MCTL5", "ADC12MCTL6", "ADC12MCTL7", 
  "ADC12MCTL8", "ADC12MCTL9", "ADC12MCTL10", "ADC12MCTL11", 
  "ADC12MCTL12", "ADC12MCTL13", "ADC12MCTL14", "ADC12MCTL15", 
  /* 0x090 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0a0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0b0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0c0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0d0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0e0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x0f0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x100 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x110 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, "TBIV-L", "TBIV-H",
  /* 0x120 */
  "WDTCTL-L", "WDTCTL-H", "DMACTL0-L", "DMACTL0-H",
  "DMACTL1-L", "DMACTL1-H", NULL, NULL,
  "FCTL1-L", "FCTL1-H", "FCTL2-L", "FCTL2-H",
  "FCTL3-L", "FCTL3-H", "TAIV-L", "TAIV-H",
  /* 0x130 */
  "MPY-L", "MPY-H", "MPYS-L", "MPYS-H",
  "MAC-L", "MAC-H", "MACS-L", "MACS-H",
  "OP2-L", "OP2-H", "RESLO-L", "RESLO-H",
  "RESHI-L", "RESHI-H", "SUMEXT-L", "SUMEXT-H",
  /* 0x140 */
  "ADC12MEM0-L", "ADC12MEM0-H",
  "ADC12MEM1-L", "ADC12MEM1-H",
  "ADC12MEM2-L", "ADC12MEM2-H",
  "ADC12MEM3-L", "ADC12MEM3-H",
  "ADC12MEM4-L", "ADC12MEM4-H",
  "ADC12MEM5-L", "ADC12MEM5-H",
  "ADC12MEM6-L", "ADC12MEM6-H",
  "ADC12MEM7-L", "ADC12MEM7-H",
  /* 0x150 */
  "ADC12MEM8-L", "ADC12MEM8-H",
  "ADC12MEM9-L", "ADC12MEM9-H",
  "ADC12MEM10-L", "ADC12MEM10-H",
  "ADC12MEM11-L", "ADC12MEM11-H",
  "ADC12MEM12-L", "ADC12MEM12-H",
  "ADC12MEM13-L", "ADC12MEM13-H",
  "ADC12MEM14-L", "ADC12MEM14-H",
  "ADC12MEM15-L", "ADC12MEM15-H",
  /* 0x160 */
  "TACTL-L", "TACTL-H", "TACCTL0-L", "TACCTL0-H",
  "TACCTL1-L", "TACCTL1-H", "TACCTL2-L", "TACCTL2-H",
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x170 */
  "TAR-L", "TAR-H", "TACCR0-L", "TACCR0-H",
  "TACCR1-L", "TACCR1-H", "TACCR2-L", "TACCR2-H",
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x180 */
  "TBCTL-L", "TBCTL-H", "TBCCTL0-L", "TBCCTL0-H",
  "TBCCTL1-L", "TBCCTL1-H", "TBCCTL2-L", "TBCCTL2-H",
  "TBCCTL3-L", "TBCCTL3-H", "TBCCTL4-L", "TBCCTL4-H",
  "TBCCTL5-L", "TBCCTL5-H", "TBCCTL6-L", "TBCCTL6-H",
  /* 0x190 */
  "TBR-L", "TBR-H", "TBCCR0-L", "TBCCR0-H",
  "TBCCR1-L", "TBCCR1-H", "TBCCR2-L", "TBCCR2-H",
  "TBCCR3-L", "TBCCR3-H", "TBCCR4-L", "TBCCR4-H",
  "TBCCR5-L", "TBCCR5-H", "TBCCR6-L", "TBCCR6-H",
  /* 0x1a0 */
  "ADC12CTL0-L", "ADC12CTL0-H", "ADC12CTL1-L", "ADC12CTL1-H",
  "ADC12IFG-L", "ADC12IFG-H", "ADC12IE-L", "ADC12IE-H",
  "ADC12IV-L", "ADC12IV-H", NULL, NULL,
  NULL, NULL, NULL, NULL,
  /* 0x1b0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x1c0 */
  "DAC12_0CTL-L", "DAC12_0CTL-H", "DAC12_1CTL-L", "DAC12_1CTL-H",
  NULL, NULL, NULL, NULL,
  "DAC12_0DAT-L", "DAC12_0DAT-H", "DAC12_1DAT-L", "DAC12_1DAT-H",
  NULL, NULL, NULL, NULL,
  /* 0x1d0 */
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  /* 0x1e0 */
  "DMA0CTL-L", "DMA0CTL-H", "DMA0SA-L", "DMA0SA-H",
  "DMA0DA-L", "DMA0DA-H", "DMA0SZ-L", "DMA0SZ-H",
  "DMA1CTL-L", "DMA1CTL-H", "DMA1SA-L", "DMA1SA-H",
  "DMA1DA-L", "DMA1DA-H", "DMA1SZ-L", "DMA1SZ-H",
  /* 0x1f0 */
  "DMA2CTL-L", "DMA2CTL-H", "DMA2SA-L", "DMA2SA-H",
  "DMA2DA-L", "DMA2DA-H", "DMA2SZ-L", "DMA2SZ-H",
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
};

const char *msp430_sfr_name(msp430_word_t addr) {
  if (addr >= MSP430_NSFR)
    return NULL;
  return sfrn[addr];
}

#define HWMUL_STATE 0x1ff

static void _do_hwmul(msp430_byte_t *sfrs) {
  int i    = sfrs[HWMUL_STATE];
  int rsgn = 0;
  msp430_host_long_t op1  = sfrs[i+0x130] | (sfrs[i+0x131] << 8);
  msp430_host_long_t op2  = sfrs[0x138]   | (sfrs[0x139]   << 8);
  msp430_host_long_t isgn = 0x8000;
  msp430_host_long_t osgn = isgn << 16;
  msp430_host_long_t prod;

  if ((i == 2) || (i == 6)) {
    int op1s = (op1 & isgn) ? 1 : 0;
    int op2s = (op2 & isgn) ? 1 : 0;

    rsgn = op1s ^ op2s;
    if (op1s)
      op1 = (op1 ^ 0xffff) + 1;
    if (op2s)
      op2 = (op2 ^ 0xffff) + 1;
  }
  prod = op1 * op2;
  if (rsgn)
    prod = (prod ^ (osgn | (osgn - 1))) + 1;
  fprintf(stderr, "HWMUL i=%d op1=0x%04lx op2=0x%04lx prod=0x%08lx\n",
	  i, op1, op2, prod);
  for (i = 0; i < 6; i++) {
    msp430_host_long_t b = sfrs[i + 0x13a], c;
    
    b += prod & 0xff;
    c  = b >> 8;
    sfrs[i + 0x13a] = b & 0xff;
    prod >>= 8;
    prod  += c;
  }
  fprintf(stderr, "  RES =");
  for (i = 0; i < 6; i++)
    fprintf(stderr, " %02x", sfrs[i + 0x13a]);
  fprintf(stderr, "\n");
}

static int msp430_dfl_sfr(msp430_ctx_t  *ctx,
			  msp430_word_t  addr,
			  msp430_byte_t *valp,
			  int            isWrite,
			  int            isWord,
			  void          *xtra) {
  msp430_sfr_t  *s = (msp430_sfr_t *) xtra;
  msp430_byte_t *sfrs = s->sfr, val = *valp;
  const char *n;

  /*** check rules */
  if (!(addr & 0x0100)) {
    /*** 8-bit sfr */
    if (isWord) return MSP430_EXC_BAD_SFR;
  } else {
    /*** 16-bit sfr */

    /*** let hwmul handle itself */
    if ((addr < 0x130) || (addr > 0x13f))
      if (!isWord) return MSP430_EXC_BAD_SFR;
  }

  /*** make sure something's mapped here */
  if ((n = msp430_sfr_name(addr)) == NULL)
    return MSP430_EXC_ENXIO;

  /*** WDT rule-check */
  if (addr == 0x0121) {
    if (isWrite) {
      if (val != 0x5a) {
	return MSP430_EXC_WDT_FAULT;
      }
      goto out;
    } else {
      val = 0x69;
      goto out;
    }
  }

  /*** flash controller rule-checks */
  if ((addr == 0x0129) || (addr == 0x012B) || (addr == 0x012D)) {
    if (isWrite) {
      if (val != 0xa5) {
	return MSP430_EXC_FC_FAULT;
      }
    } else {
      val = 0x96;
      goto out;
    }
  }

  /*** hardware multiplier */
  if ((addr >= 0x130) && (addr < 0x140)) {
    /* can't directly access high byte of registers */
    if (!isWord && (addr & 0x1)) {
      return MSP430_EXC_BAD_ALIGN;
    }
    /* treat 0x130-0x13f like ram for reads */
    if (isWrite) {
      /* SumExt is r/o */
      if (addr >= 0x13e) {
	return MSP430_EXC_BAD_STORE;
      }
      /* treat 0x13a-0x13d like ram for writes */
      if (addr < 0x13a) {
	/*** one of: MPY, MPYS, MAC, MACS, OP2 */

	/*** byte write clears high byte of register */
	if (!isWord)
	  sfrs[addr + 1] = 0;

	/*** clear result if not a mac op */
	if (addr < 0x134) {
	  sfrs[0x13a] = sfrs[0x13b] = 0;
	  sfrs[0x13c] = sfrs[0x13d] = 0;
	  sfrs[0x13e] = sfrs[0x13f] = 0;
	}

	if (addr < 0x138) {
	  /*** MPY, MPYS, MAC, MACS */
	  if (!(addr & 0x1))
	    sfrs[HWMUL_STATE] = addr - 0x130; /* match power-up-to-zero */
	} else {
	  /*** OP2 */
	  if (!(addr & 0x1)) {
	    sfrs[addr] = val; /* gotta set this first... */
	    _do_hwmul(sfrs);  /* compute result */
	  }
	}
      }
    }
  }

  /*** XXX treat the rest like ram */
  if (isWrite) {
    sfrs[addr] = val;
  } else {
    val = sfrs[addr];
  }
  fprintf(stderr, "<%s> %cSFR 0x%03x = 0x%02x insn=%ld tick=%ld\n",
	  n, (isWrite ? 'W' : 'R'),
	  addr, val, ctx->insns, ctx->ticks);

 out:
  *valp = val;
  return MSP430_SUCCESS;
}

/***********************************************************************
 * public api
 */

int msp430_get_regf(msp430_ctx_t     *ctx,
		    msp430_reg_func  *rf,
		    void            **rx) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (rf != NULL)
    *rf = ctx->reg;
  if (rx != NULL)
    *rx = ctx->reg_xtra;
  return MSP430_SUCCESS;
}

int msp430_set_regf(msp430_ctx_t    *ctx,
		    msp430_reg_func  rf,
		    void            *rx) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (rf == NULL) {
    if (rx != NULL)
      return MSP430_EXC_EINVAL;
    rf = msp430_dfl_reg;
    if ((rx = (void *) malloc(MSP430_NREGS * sizeof(msp430_word_t))) == NULL)
      return MSP430_EXC_OS_ERROR;
    memset(rx, 0, MSP430_NREGS * sizeof(msp430_word_t));
  }
  if (ctx->reg == msp430_dfl_reg) {
    if ((ctx->reg_xtra != NULL) && (ctx->reg_xtra != rx)) {
      free(ctx->reg_xtra);
      ctx->reg_xtra = NULL;
    }
  }
  ctx->reg      = rf;
  ctx->reg_xtra = rx;
  return MSP430_SUCCESS;
}

int msp430_get_reg(msp430_ctx_t  *ctx,
		   int            reg,
		   msp430_word_t *val) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if ((reg < 0) || (reg >= MSP430_NREGS))
    return MSP430_EXC_BAD_REG;
  if (reg == 3) {
    *val = 0;
  } else {
    jmp_buf save;
    int     exc;

    memcpy(&save, &(ctx->env), sizeof(save));
    if (!(exc = setjmp(ctx->env))) {
      *val = msp430_getreg(ctx, reg);
      exc = MSP430_SUCCESS;
    }
    memcpy(&(ctx->env), &save, sizeof(save));
    return exc;
  }
  return MSP430_SUCCESS;
}

int msp430_set_reg(msp430_ctx_t *ctx,
		   int           reg,
		   msp430_word_t val) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (val ^ (val & 0xffff))
    return MSP430_EXC_BAD_WORD;
  if ((reg < 0) || (reg >= MSP430_NREGS) || (reg == 3))
    return MSP430_EXC_BAD_REG;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    msp430_setreg(ctx, reg, val);
    exc = MSP430_SUCCESS;
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_clear_regs(msp430_ctx_t *ctx) {
  msp430_word_t zero = 0;
  int           i, rc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  for (i = 0; i < MSP430_NREGS; i++)
    if ((i != 3) && (rc = msp430_reg(ctx, i, &zero, 1)) != MSP430_SUCCESS)
      return rc;
  return MSP430_SUCCESS;
}

int msp430_wfetch(msp430_ctx_t *ctx, msp430_word_t addr, msp430_word_t *res) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (res == NULL)
    return MSP430_EXC_EINVAL;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    *res = msp430_word_fetch(ctx, addr);
    exc = MSP430_SUCCESS;
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_wstore(msp430_ctx_t *ctx, msp430_word_t addr, msp430_word_t val) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (val ^ (val & 0xffff))
    return MSP430_EXC_BAD_WORD;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    msp430_word_store(ctx, addr, val);
    exc = MSP430_SUCCESS;
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_bfetch(msp430_ctx_t *ctx, msp430_word_t addr, msp430_byte_t *res) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (res == NULL)
    return MSP430_EXC_EINVAL;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    *res = msp430_byte_fetch(ctx, addr);
    exc = MSP430_SUCCESS;
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_bstore(msp430_ctx_t *ctx, msp430_word_t addr, msp430_byte_t val) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (val ^ (val & 0xff))
    return MSP430_EXC_BAD_BYTE;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    msp430_byte_store(ctx, addr, val);
    exc = MSP430_SUCCESS;
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_get_mem(msp430_ctx_t     *ctx,
		   msp430_mem_func  *mem,
		   void            **mem_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (mem != NULL)
    *mem = ctx->mem;
  if (mem_xtra != NULL)
    *mem_xtra = ctx->mem_xtra;
  return MSP430_SUCCESS;
}

int msp430_set_mem(msp430_ctx_t    *ctx,
		   msp430_mem_func  mem,
		   void            *mem_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (mem == NULL) {
    if (mem_xtra != NULL)
      return MSP430_EXC_EINVAL;
    mem = msp430_dfl_mem;
    if ((mem_xtra = (void *) malloc(sizeof(msp430_mem_t))) == NULL)
      return MSP430_EXC_OS_ERROR;
    memset(mem_xtra, 0, sizeof(msp430_mem_t));
  }
  if (ctx->mem == msp430_dfl_mem) {
    if ((ctx->mem_xtra != NULL) && (ctx->mem_xtra != mem_xtra)) {
      free(ctx->mem_xtra);
      ctx->mem_xtra = NULL;
    }
  }
  ctx->mem      = mem;
  ctx->mem_xtra = mem_xtra;
  return MSP430_SUCCESS;
}

msp430_byte_t *msp430_get_ram(msp430_ctx_t *ctx) {
  if (ctx == NULL)
    return NULL;
  if (ctx->mem != msp430_dfl_mem)
    return NULL;
  return ((msp430_mem_t *) (ctx->mem_xtra))->ram;
}

msp430_byte_t *msp430_get_rom(msp430_ctx_t *ctx) {
  if (ctx == NULL)
    return NULL;
  if (ctx->mem != msp430_dfl_mem)
    return NULL;
  return ((msp430_mem_t *) (ctx->mem_xtra))->rom;
}

int msp430_get_sfr(msp430_ctx_t     *ctx,
		   msp430_sfr_func  *sfr,
		   void            **sfr_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (sfr != NULL)
    *sfr = ctx->sfr;
  if (sfr_xtra != NULL)
    *sfr_xtra = ctx->sfr_xtra;
  return MSP430_SUCCESS;
}

int msp430_set_sfr(msp430_ctx_t    *ctx,
		   msp430_sfr_func  sfr,
		   void            *sfr_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (sfr == NULL) {
    if (sfr_xtra != NULL)
      return MSP430_EXC_EINVAL;
    sfr = msp430_dfl_sfr;
    if ((sfr_xtra = (void *) malloc(sizeof(msp430_sfr_t))) == NULL)
      return MSP430_EXC_OS_ERROR;
    memset(sfr_xtra, 0, sizeof(msp430_sfr_t));
  }
  if (ctx->sfr == msp430_dfl_sfr) {
    if ((ctx->sfr_xtra != NULL) && (ctx->sfr_xtra != sfr_xtra)) {
      free(ctx->sfr_xtra);
      ctx->sfr_xtra = NULL;
    }
  }
  ctx->sfr      = sfr;
  ctx->sfr_xtra = sfr_xtra;
  return MSP430_SUCCESS;
}

int msp430_get_prof(msp430_ctx_t      *ctx,
		    msp430_prof_func  *pf,
		    void             **px) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (pf != NULL)
    *pf = ctx->prof;
  if (px != NULL)
    *px = ctx->prof_xtra;
  return MSP430_SUCCESS;
}

int msp430_set_prof(msp430_ctx_t     *ctx,
		    msp430_prof_func  pf,
		    void             *px) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (pf == NULL) {
    if (px != NULL)
      return MSP430_EXC_EINVAL;
    pf = msp430_dfl_prof;
    if ((px = (void *) malloc(sizeof(msp430_dfl_prof_t))) == NULL)
      return MSP430_EXC_OS_ERROR;
    memset(px, 0, sizeof(msp430_dfl_prof_t));
  }
  if (ctx->prof == msp430_dfl_prof) {
    if ((ctx->prof_xtra != NULL) && (ctx->prof_xtra != px)) {
      free(ctx->prof_xtra);
      ctx->prof_xtra = NULL;
    }
  }
  ctx->prof      = pf;
  ctx->prof_xtra = px;
  return MSP430_SUCCESS;
}

int msp430_clear_prof(msp430_ctx_t *ctx) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  ctx->prof(ctx, 0, 0, ctx->prof_xtra);
  return MSP430_SUCCESS;
}

void msp430_prof_enable(msp430_ctx_t *ctx,
			int           on) {
  if (!on) {
    ctx->profile = ctx->penable = 0;
  } else {
    if (on < 0) {
      ctx->profile = 1;
      ctx->penable = 0;
    } else {
      ctx->penable = 1;
    }
  }
}

int msp430_get_trap(msp430_ctx_t      *ctx,
		    msp430_trap_func  *trap,
		    void             **trap_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (trap != NULL)
    *trap = ctx->trap;
  if (trap_xtra != NULL)
    *trap_xtra = ctx->trap_xtra;
  return MSP430_SUCCESS;
}

int msp430_set_trap(msp430_ctx_t     *ctx,
		    msp430_trap_func  trap,
		    void             *trap_xtra) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (trap == NULL) {
    if (trap_xtra != NULL)
      return MSP430_EXC_EINVAL;
    trap = msp430_dfl_trap;
  }
  ctx->trap      = trap;
  ctx->trap_xtra = trap_xtra;
  return MSP430_SUCCESS;
}

int msp430_init(msp430_ctx_t *ctx) {
  int rc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  memset(ctx, 0, sizeof(msp430_ctx_t));
  if ((rc = msp430_set_regf(ctx, NULL, NULL)) != MSP430_SUCCESS)
    return rc;
  if ((rc = msp430_clear_regs(ctx)) != MSP430_SUCCESS)
    return rc;
  if ((rc = msp430_set_mem(ctx, NULL, NULL)) != MSP430_SUCCESS)
    return rc;
  if ((rc = msp430_set_sfr(ctx, NULL, NULL)) != MSP430_SUCCESS)
    return rc;
  if ((rc = msp430_set_prof(ctx, NULL, NULL)) != MSP430_SUCCESS)
    return rc;
  if ((rc = msp430_clear_prof(ctx)) != MSP430_SUCCESS)
    return rc;
  msp430_prof_enable(ctx, MSP430_PROFILE_NOW);
  if ((rc = msp430_set_trap(ctx, NULL, NULL)) != MSP430_SUCCESS)
    return rc;
  return MSP430_SUCCESS;
}

int msp430_fini(msp430_ctx_t *ctx) {
  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  if (ctx->reg == msp430_dfl_reg) {
    if (ctx->reg_xtra != NULL) {
      free(ctx->reg_xtra);
      ctx->reg_xtra = NULL;
    }
  }
  if (ctx->mem == msp430_dfl_mem) {
    if (ctx->mem_xtra != NULL) {
      free(ctx->mem_xtra);
      ctx->mem_xtra = NULL;
    }
  }
  if (ctx->prof == msp430_dfl_prof) {
    if (ctx->prof_xtra != NULL) {
      free(ctx->prof_xtra);
      ctx->prof_xtra = NULL;
    }
  }
  if (ctx->sfr == msp430_dfl_sfr) {
    if (ctx->sfr_xtra != NULL) {
      free(ctx->sfr_xtra);
      ctx->sfr_xtra = NULL;
    }
  }
  return MSP430_SUCCESS;
}

extern int msp430_reset(msp430_ctx_t *ctx) {
  jmp_buf save;
  int     exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  memcpy(&save, &(ctx->env), sizeof(save));
  if (!(exc = setjmp(ctx->env))) {
    msp430_word_t v = msp430_word_fetch(ctx, 0xfffe);

    if (v & 1) {
      exc = MSP430_EXC_BAD_ALIGN;
    } else {
      /*** reset vector should always point into rom */
      if (v < MSP430_ROM_BASE) {
	exc = MSP430_EXC_BAD_EXEC;
      } else {
	msp430_setreg(ctx, 0, v);
	exc = MSP430_SUCCESS;
      }
    }
  }
  memcpy(&(ctx->env), &save, sizeof(save));
  return exc;
}

int msp430_run(msp430_ctx_t       *ctx,
	       msp430_host_long_t  maxi,
	       msp430_host_long_t  maxc) {
  msp430_host_long_t oi, ot;
  int                exc;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;
  ctx->op_ndx      = 0;
  ctx->opcode[0]   = 0x0000;
  ctx->opcode[1]   = 0x0000;
  ctx->opcode[2]   = 0x0000;
  ctx->exc_info[0] = MSP430_SUCCESS;
  ctx->exc_info[1] = 0;
  ctx->exc_info[2] = 0;
  ctx->exc_info[3] = 0;

  oi = ctx->insns; ctx->insns = 0;
  ot = ctx->ticks; ctx->ticks = 0;
  if (!(exc = setjmp(ctx->env))) {
    while (1) {
      if ((maxi && (ctx->insns >= maxi)) ||
	  (maxc && (ctx->ticks >= maxc))) {
	exc = MSP430_EXC_CPU_TIME;
	break;
      }
      msp430_exec_one(ctx);
    }
  }
  /*** turn cpu halt into success */
  if (exc == MSP430_EXC_PM_CHANGE) {
    msp430_word_t sr;

    if ((msp430_reg(ctx, 2, &sr, 0) == MSP430_SUCCESS) &&
	((sr & 0xf0) == 0x10))
      exc = 0;
  }
  if (!exc)
    exc = MSP430_SUCCESS;
  ctx->insns += oi;
  ctx->ticks += ot;

  return exc;
}

const char *msp430_strerror(int errcode) {
  static const char *errs[MSP430_EXC_CNT+1] = {
    "Success",
    "Bad address alignment",
    "Attempt to modify reserved SR bits",
    "Function not implemented",
    "Invalid exception",
    "No such device or address",
    "Illegal addressing mode",
    "NULL context pointer",
    "Illegal register number",
    "Word constant out of range",
    "Store to read-only location",
    "Attempt to execute from illegal location",
    "Invalid argument",
    "Operating system error",
    "Excessive CPU time",
    "Byte constant out of range",
    "Bad trap",
    "Bad SFR access",
    "WDT access violation",
    "Flash controller access violation",
    "Invalid Intel hex file",
    "Python wrapper fault",
    "Power management change",
    "Debug trap",

    "Unknown exception",
  };
  if ((errcode < 0) || (errcode > MSP430_EXC_CNT))
    errcode = MSP430_EXC_CNT;
  return errs[errcode];
}

int msp430_load_ihex(msp430_byte_t *rom, char *ifile) {
  int   rc = MSP430_EXC_BAD_IHEX, cnt, addr, type, cksum, xchk, i, done = 0;
  FILE *fp;
  char  _buf[1024], *bp;

  if ((rom == NULL) || (ifile == NULL))
    return MSP430_EXC_EINVAL;
  if (!strcmp(ifile, "-"))
    fp = stdin;
  else
    if ((fp = fopen(ifile, "r")) == NULL) {
      return MSP430_EXC_OS_ERROR;
    }
  while (fgets(_buf, 1024, fp) != NULL) {
    int ll = strlen(_buf);

    bp = _buf;
    if (done) {
      goto out;
    }
    if (ll && (bp[--ll] != '\n')) {
      goto out;
    }
    if (ll && (bp[ll-1] == '\r'))
      ll--;
    bp[ll] = '\0';

    if (ll < 1) goto badl;
    if (*bp++ != ':') goto badl;
    if (--ll & 0x1) goto badl;

    if (ll < 2) goto badl;
    if (sscanf(bp, "%02x", &cnt) != 1) goto badl;
    bp += 2; ll -= 2;

    if (ll != ((cnt + 4) << 1)) {
      goto out;
    }

    if (sscanf(bp, "%04x", &addr) != 1) goto badl;
    bp += 4;

    if (sscanf(bp, "%02x", &type) != 1) goto badl;
    bp += 2;

    if (type == 0x03)
      continue;

    if (type == 0x01) {
      if (strcasecmp(_buf, ":00000001FF")) {
	goto out;
      }
      done = 1;
      continue;
    }
    if (type) goto badl;

    if ((addr < MSP430_ROM_BASE) || ((0x10000 - cnt) < addr)) {
      goto out;
    }

    xchk  = cnt + (addr & 0xff) + (addr >> 8) + type;
    addr -= MSP430_ROM_BASE;
    for (i = 0; i < cnt; i++) {
      int b;

      if (sscanf(bp, "%02x", &b) != 1) goto badl;
      bp += 2;
      rom[addr++] = b;
      xchk += b;
    }
    xchk  = ((0xff ^ xchk) + 1) & 0xff;

    if (sscanf(bp, "%02x", &cksum) != 1) goto badl;

    if (xchk != cksum) {
      goto out;
    }
    
  }
  if (!done) {
    goto out;
  }
  rc = 0;
  goto out;

 badl:

 out:
  if ((fp != NULL) && (fp != stdin))
    fclose(fp);
  return rc;
}

static char *op_names[MSP430_INSN_CNT + 1] = {
  "???",
  "mov",
  "add",
  "addc",
  "subc",
  "sub",
  "cmp",
  "dadd",
  "bit",
  "bic",
  "bis",
  "xor",
  "and",
  "rrc",
  "swpb",
  "rra",
  "sxt",
  "push",
  "call",
  "reti",
  "jnz",
  "jz",
  "jnc",
  "jc",
  "jn",
  "jge",
  "jl",
  "jmp",
};

static int diss(msp430_word_t *opcode,
		int            reg,
		int            am,
		int            lim,
		char          *ret) {
  if (reg == 3) {
    static char *c1[4] = { "#0", "#1", "#2", "#-1" };
    strcpy(ret, c1[am]);
    return 1;
  }
  if ((reg == 2) && (am > 1)) {
    strcpy(ret, (am == 2) ? "#4" : "#8");
    return 1;
  }
  if ((reg == 0) && (am == 3)) {
    if (lim < 2)
      return -1;
    sprintf(ret, "#0x%04x", opcode[1]);
    return 2;
  }
  if ((reg == 2) && (am == 1)) {
    if (lim < 2)
      return -1;
    sprintf(ret, "&0x%04x", opcode[1]);
    return 2;
  }
  if (am == 0) {
    sprintf(ret, "r%d", reg);
    return 1;
  }
  if (am == 1) {
    if (lim < 2)
      return -1;
    sprintf(ret, "#0x%04x(r%d)", opcode[1], reg);
    return 2;
  }
  sprintf(ret, "@r%d%s", reg, (am == 3) ? "+" : "");
  return 1;
}

static int disd(msp430_word_t *opcode,
		int            reg,
		int            am,
		int            pos,
		int            lim,
		char          *ret) {
  if (reg == 3) {
    strcpy(ret, am ? "#1" : "#0");
    return pos;
  }
  if ((reg == 2) && am) {
    if (lim < pos)
      return -1;
    sprintf(ret, "&0x%04x", opcode[pos]);
    return pos + 1;
  }
  if (am) {
    if (lim < pos)
      return -1;
    sprintf(ret, "#0x%04x(r%d)", opcode[pos], reg);
    return pos + 1;
  }
  sprintf(ret, "r%d", reg);
  return pos;
}

static char *msp430_disassemble(msp430_word_t *opcodes, int *nwords) {
  static char   ret[100], bufn[16], buf1[16], buf2[16];
  int           am, reg, pos, lim;
  msp430_word_t opcode;

  if ((opcodes == NULL) || nwords == NULL)
    return NULL;
  if (!(lim = *nwords))
    return NULL;
  opcode  = opcodes[0];
  *nwords = 1;

  if ((opcode & 0xe000) == 0x2000) {
    /*** jump */
    char *name;
    int   ofs = opcode & 0x03ff;

    name = op_names[MSP430_INSN_JNZ + ((opcode >> 10) & 0x7)];
    if (ofs & 0x0200)
      ofs = ((ofs ^ 0x0200) - 0x0200);
    sprintf(ret, "%-6s #%d", name, ofs);
    return ret;
  }
  if (opcode & 0xc000) {
    /*** dual-opnd */
    strcpy(bufn, op_names[MSP430_INSN_MOV + ((opcode >> 12) & 0xf) - 4]);
    strcat(bufn, (opcode & 0x40) ? ".b" : "");

    reg = (opcode >> 8) & 0xf;
    am  = (opcode >> 4) & 0x3;
    if ((pos = diss(opcodes, reg, am, lim, buf1)) < 0)
      return NULL;
    
    reg = opcode & 0xf;
    am  = (opcode >> 7) & 0x1;
    if ((pos = disd(opcodes, reg, am, pos, lim, buf2)) < 0)
      return NULL;

    sprintf(ret, "%-6s %s, %s", bufn, buf1, buf2);
    *nwords = pos;
    return ret;
  }
  if ((opcode & 0xfc00) != 0x1000) {
    sprintf(ret, ".short 0x%04x", opcode);
    return ret;
  }

  /*** sngl-opnd */
  if ((opcode & 0x0380) == 0x0380) {
    sprintf(ret, ".short 0x%04x", opcode);
    return ret;
  }

  strcpy(bufn, op_names[MSP430_INSN_RRC + ((opcode >> 7) & 0x7)]);
  strcat(bufn, (opcode & 0x40) ? ".b" : "");

  reg = opcode & 0xf;
  am  = (opcode >> 4) & 0x3;

  if ((pos = diss(opcodes, reg, am, lim, buf1)) < 0)
    return NULL;

  sprintf(ret, "%-6s %s", bufn, buf1);
  *nwords = pos;
  return ret;
}

int msp430_dump(msp430_ctx_t *ctx, FILE *fp) {
  int   i;
  float fac;

  if (ctx == NULL)
    return MSP430_EXC_NULL_CTX;

  fprintf(fp, "[Status]\n");
  fprintf(fp, "Instructions: %ld\n", ctx->insns);
  fprintf(fp, "Ticks       : %ld\n", ctx->ticks);
  fprintf(fp, "Profile     : %s <- %s\n",
	  ctx->profile ? "on" : "off",
	  ctx->penable ? "on" : "n/a");
  if (ctx->exc_info[0] != MSP430_SUCCESS) {
    fprintf(fp, "Exception   : 0x%04x 0x%04x 0x%04x %s%s line %d\n",
	    ctx->exc_info[0], ctx->exc_info[1], ctx->exc_info[2],
	    ctx->in_exec ? "[in_exec] " : " ",
	    msp430_strerror(ctx->exc_info[0]),
	    ctx->exc_info[3]);
  }
  fprintf(fp, "Last PC     : 0x%04x\n", ctx->insn_pc);
  fprintf(fp, "Opcode      :");
  for (i = 0; i < ctx->op_ndx; i++)
    fprintf(fp, " 0x%04x", ctx->opcode[i]);
  fprintf(fp, "\n");
  fprintf(fp, "Insn Info   : op=%d [%s] class=%d ticks=%d\n",
	  ctx->info.op, op_names[ctx->info.op],
	  ctx->info.type, ctx->info.ticks);
  fprintf(fp, "Source      : sreg=%d sa=%d\n",
	  ctx->info.sreg, ctx->info.sa);
  fprintf(fp, "Destination : dreg=%d da=%d daddr=0x%04x\n",
	  ctx->info.dreg, ctx->info.da, ctx->info.daddr);
  fprintf(fp, "Byte/Word   : bw=%d mask=0x%04x sign=0x%04x\n",
	  ctx->info.bw, ctx->info.mask, ctx->info.sign);
  fprintf(fp, "Result      : 0x%04x\n", ctx->info.res);
  i = 3;
  fprintf(fp, "Mnemonic    : %s\n", msp430_disassemble(ctx->opcode, &i));
  fprintf(fp, "\n");

  fprintf(fp, "[Registers]\n");
  for (i = 0; i < MSP430_NREGS; i++) {
    msp430_word_t v = (i == 3) ? 0 : msp430_getreg(ctx, i);

    fprintf(fp, "r%d%s %5d 0x%04x\n",
	    i,
	    (i < 10) ? " " : "",
	    v, v);
  }
  fprintf(fp, "\n");

  if (ctx->prof == msp430_dfl_prof) {
    msp430_dfl_prof_t  *p = (msp430_dfl_prof_t *) ctx->prof_xtra;
    msp430_host_long_t *I = p->insns, *T = p->ticks;

    fprintf(fp, "[Counts]\n");
    fac = 100.0 / (I[0] ? I[0] : 1.0);
    for (i = 1; i <= MSP430_INSN_CNT; i++)
      if (I[i])
	fprintf(fp, "%-5s: %9ld %5.1f%%\n",
		op_names[i], I[i], fac * I[i]);
    fprintf(fp, "Total: %9ld\n", I[0]);
    fprintf(fp, "\n");
    
    fprintf(fp, "[Ticks]\n");
    fac = 100.0 / (T[0] ? T[0] : 1.0);
    for (i = 1; i <= MSP430_INSN_CNT; i++)
      if (T[i])
	fprintf(fp, "%-5s: %9ld %5.1f%%\n",
		op_names[i], T[i], fac * T[i]);
    fprintf(fp, "Total: %9ld\n", T[0]);
    fprintf(fp, "\n");
  }

  return MSP430_SUCCESS;
}

int msp430_main(int argc, char *argv[]) {
  int rc;
  msp430_ctx_t _c, *c = &_c;

  if (argc != 2) {
    fprintf(stderr, "usage: %s ihex_file\n", *argv);
    return 1;
  }
  argc--; argv++;

  if ((rc = msp430_init(c))) {
    printf("msp430_init: %s [%d]\n", msp430_strerror(rc), rc);
    return 2;
  }

  if (msp430_load_ihex(msp430_get_rom(c), *argv))
    return 3;

  if ((rc = msp430_reset(c))) {
    printf("msp430_reset: %s\n", msp430_strerror(rc));
    return 4;
  }

  rc = msp430_run(c, 0, 1024 * 1048576UL);

  msp430_dump(c, stdout);

  if (rc) {
    printf("msp430_run: %s [%d]: 0x%04x 0x%04x line %d\n",
	   msp430_strerror(rc), rc,
	   c->exc_info[1], c->exc_info[2], c->exc_info[3]);
  } else {
    printf("msp430_run: ok [%d]\n", rc);
  }

  msp430_fini(c);

  return rc;
}

#ifndef WITH_PYTHON
#ifndef NO_SOFT430_MAIN

int main(int argc, char *argv[]) {
  return msp430_main(argc, argv);
}

#endif
#else

#include <Python.h>

typedef struct {
  PyObject_HEAD
  msp430_ctx_t ctx;
} msp430object;

static PyTypeObject MSP430type;

static PyObject *MSP430Error;

#define ctx(v) (&((v)->ctx))

static void *exc(char *func, int code) {
  const char *msg = msp430_strerror(code);
  PyObject   *obj, *tmp;

  if (msg == NULL) msg = "Unknown error";
  if ((obj = PyTuple_New(3)) == NULL)
    return NULL;
  if ((tmp = PyString_FromString(func)) == NULL)
    goto bad;
  PyTuple_SET_ITEM(obj, 0, tmp);
  if ((tmp = PyString_FromString(msg)) == NULL)
    goto bad;
  PyTuple_SET_ITEM(obj, 1, tmp);
  if ((tmp = PyInt_FromLong(code)) == NULL)
    goto bad;
  PyTuple_SET_ITEM(obj, 2, tmp);

  PyErr_SetObject(MSP430Error, obj);
  return NULL;

 bad:
  Py_DECREF(obj);
  return NULL;
}

static msp430object *_new(void) {
  msp430object *ret;
  int           rc;

  if ((ret = PyObject_GC_New(msp430object, &MSP430type)) == NULL)
    return NULL;
  memset(ctx(ret), 0, sizeof(msp430_ctx_t));
  if ((rc = msp430_init(ctx(ret))) != MSP430_SUCCESS) {
    Py_DECREF(ret);
    return exc("msp430_init", rc);
  }
  PyObject_GC_Track((PyObject *) ret);
  return ret;
}

static int pysfr(msp430_ctx_t  *ctx,
		 msp430_word_t  addr,
		 msp430_byte_t *val,
		 int            isWrite,
		 int            isWord,
		 void          *xtra);

static int pytrap(msp430_ctx_t  *ctx,
		  msp430_word_t  val,
		  void          *xtra);

static int py_mem(msp430_ctx_t  *ctx,
		  msp430_word_t  addr,
		  msp430_byte_t *val,
		  int            isWrite,
		  int            isWord,
		  void          *xtra);

static int py_reg(msp430_ctx_t  *ctx,
		  int            reg,
		  msp430_word_t *val,
		  int            isWrite,
		  void          *xtra);

static void pyprof(msp430_ctx_t *ctx,
		   int           op,
		   int           ticks,
		   void         *xtra);

static int clear(msp430object *obj) {
  msp430_reg_func   reg;
  msp430_mem_func   mem;
  msp430_sfr_func   sfr;
  msp430_trap_func  trap;
  msp430_prof_func  prof;
  void             *xtra;

  if ((msp430_get_regf(ctx(obj), &reg, &xtra) == MSP430_SUCCESS) &&
      (reg == py_reg)) {
    msp430_set_regf(ctx(obj), NULL, NULL);
    Py_XDECREF((PyObject *) xtra);
  }

  if ((msp430_get_mem(ctx(obj), &mem, &xtra) == MSP430_SUCCESS) &&
      (mem == py_mem)) {
    msp430_set_mem(ctx(obj), NULL, NULL);
    Py_XDECREF((PyObject *) xtra);
  }

  if ((msp430_get_sfr(ctx(obj), &sfr, &xtra) == MSP430_SUCCESS) &&
      (sfr == pysfr)) {
    msp430_set_sfr(ctx(obj), NULL, NULL);
    Py_XDECREF((PyObject *) xtra);
  }

  if ((msp430_get_trap(ctx(obj), &trap, &xtra) == MSP430_SUCCESS) &&
      (trap == pytrap)) {
    msp430_set_trap(ctx(obj), NULL, NULL);
    Py_XDECREF((PyObject *) xtra);
  }

  if ((msp430_get_prof(ctx(obj), &prof, &xtra) == MSP430_SUCCESS) &&
      (prof == pyprof)) {
    msp430_set_prof(ctx(obj), NULL, NULL);
    Py_XDECREF((PyObject *) xtra);
  }

  msp430_fini(ctx(obj));
  return 0;
}

static void del(msp430object *obj) {
  PyObject_GC_UnTrack((PyObject *) obj);
  clear(obj);
  PyObject_GC_Del(obj);
}

static int traverse(msp430object *obj, visitproc visit, void *arg) {
  msp430_reg_func   reg;
  msp430_mem_func   mem;
  msp430_sfr_func   sfr;
  msp430_trap_func  trap;
  msp430_prof_func  prof;
  PyObject         *po;
  int               rc;

  if ((msp430_get_regf(ctx(obj), &reg, (void **) &po) == MSP430_SUCCESS) &&
      (reg == py_reg) &&
      (po != NULL)) {
    if ((rc = visit(po, arg)) != 0) return rc;
  }
  if ((msp430_get_mem(ctx(obj), &mem, (void **) &po) == MSP430_SUCCESS) &&
      (mem == py_mem) &&
      (po != NULL)) {
    if ((rc = visit(po, arg)) != 0) return rc;
  }
  if ((msp430_get_sfr(ctx(obj), &sfr, (void **) &po) == MSP430_SUCCESS) &&
      (sfr == pysfr) &&
      (po != NULL)) {
    if ((rc = visit(po, arg)) != 0) return rc;
  }
  if ((msp430_get_trap(ctx(obj), &trap, (void **) &po) == MSP430_SUCCESS) &&
      (trap == pytrap) &&
      (po != NULL)) {
    if ((rc = visit(po, arg)) != 0) return rc;
  }
  if ((msp430_get_prof(ctx(obj), &prof, (void **) &po) == MSP430_SUCCESS) &&
      (prof == pyprof) &&
      (po != NULL)) {
    if ((rc = visit(po, arg)) != 0) return rc;
  }
  return 0;
}

static int print(msp430object *self, FILE *fp, int flags) {
  msp430_dump(ctx(self), fp);
  return 0;
}

/***********************************************************************
 * methods
 */

static PyObject *sfrnam(msp430object *self, PyObject *args) {
  PyObject   *ret = NULL;
  const char *nam;
  int         addr;

  if (!PyArg_ParseTuple(args, "i", &addr))
    return NULL;
  if ((nam = msp430_sfr_name(addr)) != NULL) {
    ret = PyString_FromString(nam);
  } else {
    ret = Py_None;
    Py_INCREF(ret);
  }
  return ret;
}

PyDoc_STRVAR(sfrnam_doc,
"sfr_nam(addr) -> string|None\n\
\n\
Returns the name for an SFR address. Returns\n\
None is addr does not map to a defined SFR.");

static PyObject *getreg(msp430object *self, PyObject *args) {
  msp430_word_t ret;
  int           rn, rc;

  if (!PyArg_ParseTuple(args, "i", &rn))
    return NULL;
  if ((rc = msp430_get_reg(ctx(self), rn, &ret)) != MSP430_SUCCESS)
    return exc("msp430_get_reg", rc);
  return PyInt_FromLong(ret);
}

PyDoc_STRVAR(getreg_doc,
"getreg(regnum)\n\
\n\
fetch the value in a register.");

static PyObject *setreg(msp430object *self, PyObject *args) {
  msp430_word_t val;
  int           rn, rv, rc;

  if (!PyArg_ParseTuple(args, "ii", &rn, &rv))
    return NULL;
  val = rv;
  if ((rc = msp430_set_reg(ctx(self), rn, val)) != MSP430_SUCCESS)
    return exc("msp430_set_reg", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setreg_doc,
"setreg(regnum, val)\n\
\n\
store val into a register.");

static PyObject *clrreg(msp430object *self, PyObject *args) {
  int rc;

  if ((rc = msp430_clear_regs(ctx(self))) != MSP430_SUCCESS)
    return exc("msp430_clear_regs", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(clrreg_doc,
"clr_reg()\n\
\n\
Clear all registers to zero.");

static PyObject *wfetch(msp430object *self, PyObject *args) {
  msp430_word_t ret;
  int           addr, rc;

  if (!PyArg_ParseTuple(args, "i", &addr))
    return NULL;
  if ((rc = msp430_wfetch(ctx(self), addr, &ret)) != MSP430_SUCCESS)
    return exc("msp430_wfetch", rc);
  return PyInt_FromLong(ret);
}

PyDoc_STRVAR(wfetch_doc,
"wfetch(addr) -> value\n\
\n\
Return the word contents of addr.");

static PyObject *bfetch(msp430object *self, PyObject *args) {
  msp430_byte_t ret;
  int           addr, rc;

  if (!PyArg_ParseTuple(args, "i", &addr))
    return NULL;
  if ((rc = msp430_bfetch(ctx(self), addr, &ret)) != MSP430_SUCCESS)
    return exc("msp430_bfetch", rc);
  return PyInt_FromLong(ret);
}

PyDoc_STRVAR(bfetch_doc,
"bfetch(addr) -> value\n\
\n\
Return the byte contents of addr.");

static PyObject *wstore(msp430object *self, PyObject *args) {
  int addr, val, rc;

  if (!PyArg_ParseTuple(args, "ii", &addr, &val))
    return NULL;
  if ((rc = msp430_wstore(ctx(self), addr, val)) != MSP430_SUCCESS)
    return exc("msp430_wstore", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(wstore_doc,
"wstore(addr, value)\n\
\n\
Store the word value into addr.");

static PyObject *bstore(msp430object *self, PyObject *args) {
  int addr, val, rc;

  if (!PyArg_ParseTuple(args, "ii", &addr, &val))
    return NULL;
  if ((rc = msp430_bstore(ctx(self), addr, val)) != MSP430_SUCCESS)
    return exc("msp430_bstore", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(bstore_doc,
"bstore(addr, value)\n\
\n\
Store the word value into addr.");

static PyObject *progm(msp430object *self, PyObject *args) {
  int            addr, val;
  msp430_byte_t *romp = msp430_get_rom(ctx(self));

  if (!PyArg_ParseTuple(args, "ii", &addr, &val))
    return NULL;
  if (addr & 0x1)
    return exc("program", MSP430_EXC_BAD_ALIGN);
  if (romp == NULL) {
    PyErr_SetString(PyExc_ValueError, "CPU ROM not internally defined");
    return NULL;
  }
  if ((addr < MSP430_ROM_BASE) || (addr > (0x10000 - 2))) {
    PyErr_SetString(PyExc_ValueError, "address out of range");
    return NULL;
  }
  addr          -= MSP430_ROM_BASE;
  romp[addr]     = val & 0xff;
  romp[addr + 1] = (val >> 8) & 0xff;

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(progm_doc,
"program(addr, value)\n\
\n\
Store the word value into program flash/ROM at addr.\n\
Only works with the default memory handler.");

static PyObject *clrprf(msp430object *self, PyObject *args) {
  int rc;

  if ((rc = msp430_clear_prof(ctx(self))) != MSP430_SUCCESS)
    return exc("msp430_clear_prof", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(clrprf_doc,
"clr_prf()\n\
\n\
Clear all profiling registers.");

static PyObject *enaprf(msp430object *self, PyObject *args) {
  int val;

  if (!PyArg_ParseTuple(args, "i", &val))
    return NULL;
  msp430_prof_enable(ctx(self), val);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(enaprf_doc,
"ena_prf(flag)\n\
\n\
Configure profiling as follows:\n\
    MSP430_PROF_OFF  -- disable profiling\n\
    MSP430_PROF_NEXT -- start profiling on next instruction\n\
    MSP430_PROF_NOW  -- start profiling on current instruction");

static PyObject *reset(msp430object *self, PyObject *args) {
  int rc;

  if ((rc = msp430_reset(ctx(self))) != MSP430_SUCCESS)
    return exc("msp430_reset", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(reset_doc,
"reset()\n\
\n\
Load r0 with the contents of 0xfffe.");

static PyObject *ldihex(msp430object *self, PyObject *args) {
  msp430_byte_t *romp = msp430_get_rom(ctx(self));
  char          *fname;
  int            rc;

  if (!PyArg_ParseTuple(args, "s", &fname))
    return NULL;
  if (romp == NULL) {
    PyErr_SetString(PyExc_ValueError, "CPU ROM not internally defined");
    return NULL;
  }
  if ((rc = msp430_load_ihex(romp, fname)) != MSP430_SUCCESS)
    return exc("msp430_load_ihex", rc);
  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(ldihex_doc,
"ld_ihex(filename)\n\
\n\
Load the specified Intel hex file into the CPU.");

static PyObject *run(msp430object *self, PyObject *args) {
  int rc, maxi = 0, maxc = 0;

  if (!PyArg_ParseTuple(args, "|ii", &maxi, &maxc))
    return NULL;
  if (maxi < 0) maxi = 0;
  if (maxc < 0) maxc = 0;
  if (((rc = msp430_run(ctx(self), maxi, maxc)) != MSP430_SUCCESS) &&
      (rc != MSP430_EXC_CPU_TIME)) {
      return exc("msp430_run", rc);
  }
  return PyInt_FromLong(rc);
}

PyDoc_STRVAR(run_doc,
"run(max_insns=0, max_ticks=0)\n\
\n\
Execute code on the CPU. If max_insns is greater\n\
than zero, run no more than that many instructions.\n\
If max_ticks is greater than zero, run for no more\n\
than that many clock cycles.");

static PyObject *strerr(msp430object *self, PyObject *args) {
  const char *msg;
  int         code;

  if (!PyArg_ParseTuple(args, "i", &code))
    return NULL;
  if ((msg = msp430_strerror(code)) == NULL)
    msg = "Unknown exception";
  return PyString_FromString(msg);
}

PyDoc_STRVAR(strerr_doc,
"strerr(code) -> string\n\
\n\
Return a string description of an exception code.");

static int pysfr(msp430_ctx_t  *ctx,
		 msp430_word_t  addr,
		 msp430_byte_t *val,
		 int            isWrite,
		 int            isWord,
		 void          *xtra) {
  PyObject *func = (PyObject *) xtra;
  PyObject *ret;

  if ((ret = PyObject_CallFunction(func, "iiii",
				   addr, *val, isWrite, isWord)) == NULL) {
    if (PyErr_ExceptionMatches(MSP430Error)) {
      PyObject *t, *v, *tb;
      int       rc = MSP430_EXC_PYTHON_FAULT;

      PyErr_Fetch(&t, &v, &tb);
      Py_XDECREF(t);
      Py_XDECREF(tb);
      if (v == NULL) {
	return MSP430_EXC_PYTHON_FAULT; /* this cannot happen */
      }
      if ((t = PyObject_GetAttrString(v, "args")) == NULL)
	goto out;
      Py_DECREF(v); v = NULL;
      if (!PyTuple_Check(t))
	goto out;
      switch (PyTuple_Size(t)) {
        case 1: {
	  if ((v = PyTuple_GET_ITEM(t, 0)) == NULL)
	    goto out;
	  break;
	}
        case 3: {
	  if ((v = PyTuple_GET_ITEM(t, 2)) == NULL)
	    goto out;
	  break;
	}
        default: {
	  goto out;
	}
      }
      if (!PyInt_Check(v))
	goto out;
      rc = PyInt_AsLong(v);

    out:
      Py_XDECREF(t);
      Py_XDECREF(v);
      return rc;

    } else {
      PyErr_Clear();
      return MSP430_EXC_PYTHON_FAULT;
    }
  }
  if (!PyInt_Check(ret)) {
    Py_DECREF(ret);
    return MSP430_EXC_PYTHON_FAULT;
  }
  *val = PyInt_AsLong(ret);
  Py_DECREF(ret);
  return MSP430_SUCCESS;
}

static PyObject *setsfr(msp430object *self, PyObject *args) {
  msp430_sfr_func  old, new;
  void            *oldx;
  PyObject        *func;
  int              rc;

  if (!PyArg_ParseTuple(args, "O", &func))
    return NULL;
  if (func != Py_None) {
    if (!PyCallable_Check(func)) {
      PyErr_SetString(PyExc_TypeError, "func must be callable");
      return NULL;
    }
    new = pysfr;
  } else {
    new  = NULL;
    func = NULL;
  }
  if ((rc = msp430_get_sfr(ctx(self), &old, &oldx)) != MSP430_SUCCESS)
    return exc("msp430_get_sfr", rc);
  if ((rc = msp430_set_sfr(ctx(self), new, func)) != MSP430_SUCCESS)
    return exc("msp430_set_sfr", rc);
  if (old == pysfr) {
    Py_XDECREF((PyObject *) oldx);
  }
  if (func != NULL)
    Py_INCREF(func);

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setsfr_doc,
"setsfr(func)\n\
\n\
Set the SFR handler to func. When any SFR is\n\
accessed, func will be called as:\n\
\n\
    val <- func(addr, val, isWrite, isWord)\n\
\n\
and must return an integer or raise an exception.\n\
addr is the sfr address to be accessed. val is a\n\
byte value to be stored if isWrite is nonzero.\n\
isWrite is nonzero if a write is being requested;\n\
it is zero for read requests. isWord is zero if a\n\
byte access is requested and nonzero for a word access.\n\
\n\
For word accesses, the high byte (at the odd address)\n\
will be requested first, followed immediately by a\n\
request for the low byte (at the even address). this\n\
second request does not occur if an exception is\n\
raised during the first request.");

static PyObject *sfrbyt(msp430object *self, PyObject *args) {
  int            addr, isWrite, rc;
  PyObject      *tmp = NULL;
  msp430_byte_t  val;

  if (!PyArg_ParseTuple(args, "i|O", &addr, &tmp))
    return NULL;
  if (tmp == NULL) {
    isWrite = 0;
    val     = 0;
  } else {
    if (!PyInt_Check(tmp)) {
      PyErr_SetString(PyExc_TypeError, "int expected");
      return NULL;
    }
    isWrite = 1;
    val     = PyInt_AsLong(tmp);
  }
  if ((rc = msp430_sfr_byte(ctx(self), addr, &val, isWrite)) != MSP430_SUCCESS)
    return exc("msp430_sfr_byte", rc);
  return PyInt_FromLong(val);
}

PyDoc_STRVAR(sfrbyt_doc,
"sfr_byt(addr [, val]) -> val\n\
\n\
If val is omitted, return the current byte value of\n\
the SFR addr; otherwise, store val into the SFR.");

static PyObject *sfrwrd(msp430object *self, PyObject *args) {
  int            addr, isWrite, rc;
  PyObject      *tmp = NULL;
  msp430_word_t  val;

  if (!PyArg_ParseTuple(args, "i|O", &addr, &tmp))
    return NULL;
  if (tmp == NULL) {
    isWrite = 0;
    val     = 0;
  } else {
    if (!PyInt_Check(tmp)) {
      PyErr_SetString(PyExc_TypeError, "int expected");
      return NULL;
    }
    isWrite = 1;
    val     = PyInt_AsLong(tmp);
  }
  if ((rc = msp430_sfr_word(ctx(self), addr, &val, isWrite)) != MSP430_SUCCESS)
    return exc("msp430_sfr_word", rc);
  return PyInt_FromLong(val);
}

PyDoc_STRVAR(sfrwrd_doc,
"sfr_byt(addr [, val]) -> val\n\
\n\
If val is omitted, return the current word value of\n\
the SFR addr; otherwise, store val into the SFR.");

static int pytrap(msp430_ctx_t  *ctx,
		  msp430_word_t  val,
		  void          *xtra) {
  PyObject *func = (PyObject *) xtra;
  PyObject *ret;

  if ((ret = PyObject_CallFunction(func, "i", val)) == NULL) {
    if (PyErr_ExceptionMatches(MSP430Error)) {
      PyObject *t, *v, *tb;
      int       rc = MSP430_EXC_PYTHON_FAULT;

      PyErr_Fetch(&t, &v, &tb);
      Py_XDECREF(t);
      Py_XDECREF(tb);
      if (v == NULL) {
	return MSP430_EXC_PYTHON_FAULT; /* this cannot happen */
      }
      if ((t = PyObject_GetAttrString(v, "args")) == NULL)
	goto out;
      Py_DECREF(v); v = NULL;
      if (!PyTuple_Check(t))
	goto out;
      switch (PyTuple_Size(t)) {
        case 1: {
	  if ((v = PyTuple_GET_ITEM(t, 0)) == NULL)
	    goto out;
	  break;
	}
        case 3: {
	  if ((v = PyTuple_GET_ITEM(t, 2)) == NULL)
	    goto out;
	  break;
	}
        default: {
	  goto out;
	}
      }
      if (!PyInt_Check(v))
	goto out;
      rc = PyInt_AsLong(v);

    out:
      Py_XDECREF(t);
      Py_XDECREF(v);
      return rc;

    } else {
      PyErr_Clear();
      return MSP430_EXC_PYTHON_FAULT;
    }
  }
  Py_DECREF(ret);
  return MSP430_SUCCESS;
}

static PyObject *setrap(msp430object *self, PyObject *args) {
  msp430_trap_func  old, new;
  void             *oldx;
  PyObject         *func;
  int               rc;

  if (!PyArg_ParseTuple(args, "O", &func))
    return NULL;
  if (func != Py_None) {
    if (!PyCallable_Check(func)) {
      PyErr_SetString(PyExc_TypeError, "func must be callable");
      return NULL;
    }
    new = pytrap;
  } else {
    new  = NULL;
    func = NULL;
  }
  if ((rc = msp430_get_trap(ctx(self), &old, &oldx)) != MSP430_SUCCESS)
    return exc("msp430_get_trap", rc);
  if ((rc = msp430_set_trap(ctx(self), new, func)) != MSP430_SUCCESS)
    return exc("msp430_set_trap", rc);
  if (old == pytrap) {
    Py_XDECREF((PyObject *) oldx);
  }
  if (func != NULL)
    Py_INCREF(func);

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setrap_doc,
"setrap(func)\n\
\n\
Set the trap handler to func. When a trap is\n\
generated, func will be called as:\n\
\n\
    func(val)\n\
\n\
The return value of func is ignored. Func\n\
may raise an exception.\n\
\n\
Traps are generated by storing an immediate\n\
word value to r3.");

static int py_mem(msp430_ctx_t  *ctx,
		  msp430_word_t  addr,
		  msp430_byte_t *val,
		  int            isWrite,
		  int            isWord,
		  void          *xtra) {
  PyObject *func = (PyObject *) xtra;
  PyObject *ret;
  int       rc = MSP430_EXC_PYTHON_FAULT;

  if ((ret = PyObject_CallFunction(func, "iiii",
				   addr, *val, isWrite, isWord)) == NULL) {
    if (PyErr_ExceptionMatches(MSP430Error)) {
      PyObject *t, *v, *tb;

      PyErr_Fetch(&t, &v, &tb);
      Py_XDECREF(t);
      Py_XDECREF(tb);
      if (v == NULL)
	return rc; /* this cannot happen */
      if ((t = PyObject_GetAttrString(v, "args")) == NULL)
	goto out;
      Py_DECREF(v); v = NULL;
      if (!PyTuple_Check(t))
	goto out;
      switch (PyTuple_Size(t)) {
        case 1: {
	  if ((v = PyTuple_GET_ITEM(t, 0)) == NULL)
	    goto out;
	  break;
	}
        case 3: {
	  if ((v = PyTuple_GET_ITEM(t, 2)) == NULL)
	    goto out;
	  break;
	}
        default: {
	  goto out;
	}
      }
      if (!PyInt_Check(v))
	goto out;
      if (!(rc = PyInt_AsLong(v)))
	rc = MSP430_EXC_PYTHON_FAULT;

    out:
      Py_XDECREF(t);
      Py_XDECREF(v);
      return rc;

    } else {
      PyErr_Clear();
      return rc;
    }
  }
  if (!PyInt_Check(ret)) {
    Py_DECREF(ret);
    return rc;
  }
  *val = PyInt_AsLong(ret);
  Py_DECREF(ret);
  return MSP430_SUCCESS;
}

static PyObject *setmem(msp430object *self, PyObject *args) {
  msp430_mem_func  old, new;
  void            *oldx;
  PyObject        *func;
  int              rc;

  if (!PyArg_ParseTuple(args, "O", &func))
    return NULL;
  if (func != Py_None) {
    if (!PyCallable_Check(func)) {
      PyErr_SetString(PyExc_TypeError, "func must be callable");
      return NULL;
    }
    new = py_mem;
  } else {
    new  = NULL;
    func = NULL;
  }
  if ((rc = msp430_get_mem(ctx(self), &old, &oldx)) != MSP430_SUCCESS)
    return exc("msp430_get_mem", rc);
  if ((rc = msp430_set_mem(ctx(self), new, func)) != MSP430_SUCCESS)
    return exc("msp430_set_mem", rc);
  if (old == py_mem) {
    Py_XDECREF((PyObject *) oldx);
  }
  if (func != NULL)
    Py_INCREF(func);

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setmem_doc,
"set_mem(func)\n\
\n\
Set the memory handler to func. When memory is\n\
accessed, func will be called as:\n\
\n\
    val <- func(addr, val, isWrite, isWord)\n\
\n\
and must return an integer or raise an exception.\n\
addr is the mem address to be accessed. val is a\n\
byte value to be stored if isWrite is nonzero.\n\
isWrite is nonzero if a write is being requested;\n\
it is zero for read requests. isWord is zero if a\n\
byte access is requested and nonzero for a word access.\n\
\n\
For word accesses, the high byte (at the odd address)\n\
will be requested first, followed immediately by a\n\
request for the low byte (at the even address). this\n\
second request does not occur if an exception is\n\
raised during the first request.");

static int py_reg(msp430_ctx_t  *ctx,
		  int            reg,
		  msp430_word_t *val,
		  int            isWrite,
		  void          *xtra) {
  PyObject *func = (PyObject *) xtra;
  PyObject *ret;
  int       rc = MSP430_EXC_PYTHON_FAULT;

  if ((ret = PyObject_CallFunction(func, "iii",
				   reg, *val, isWrite)) == NULL) {
    if (PyErr_ExceptionMatches(MSP430Error)) {
      PyObject *t, *v, *tb;

      PyErr_Fetch(&t, &v, &tb);
      Py_XDECREF(t);
      Py_XDECREF(tb);
      if (v == NULL)
	return rc; /* this cannot happen */
      if ((t = PyObject_GetAttrString(v, "args")) == NULL)
	goto out;
      Py_DECREF(v); v = NULL;
      if (!PyTuple_Check(t))
	goto out;
      switch (PyTuple_Size(t)) {
        case 1: {
	  if ((v = PyTuple_GET_ITEM(t, 0)) == NULL)
	    goto out;
	  break;
	}
        case 3: {
	  if ((v = PyTuple_GET_ITEM(t, 2)) == NULL)
	    goto out;
	  break;
	}
        default: {
	  goto out;
	}
      }
      if (!PyInt_Check(v))
	goto out;
      if (!(rc = PyInt_AsLong(v)))
	rc = MSP430_EXC_PYTHON_FAULT;

    out:
      Py_XDECREF(t);
      Py_XDECREF(v);
      return rc;

    } else {
      PyErr_Clear();
      return rc;
    }
  }
  if (!PyInt_Check(ret)) {
    Py_DECREF(ret);
    return rc;
  }
  *val = PyInt_AsLong(ret);
  Py_DECREF(ret);
  return MSP430_SUCCESS;
}

static PyObject *setrgf(msp430object *self, PyObject *args) {
  msp430_reg_func  old, new;
  void            *oldx;
  PyObject        *func;
  int              rc;

  if (!PyArg_ParseTuple(args, "O", &func))
    return NULL;
  if (func != Py_None) {
    if (!PyCallable_Check(func)) {
      PyErr_SetString(PyExc_TypeError, "func must be callable");
      return NULL;
    }
    new = py_reg;
  } else {
    new  = NULL;
    func = NULL;
  }
  if ((rc = msp430_get_regf(ctx(self), &old, &oldx)) != MSP430_SUCCESS)
    return exc("msp430_get_regf", rc);
  if ((rc = msp430_set_regf(ctx(self), new, func)) != MSP430_SUCCESS)
    return exc("msp430_set_regf", rc);
  if (old == py_reg) {
    Py_XDECREF((PyObject *) oldx);
  }
  if (func != NULL)
    Py_INCREF(func);

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setrgf_doc,
"set_rgf(func)\n\
\n\
Set the register handler to func. When a register is\n\
accessed, func will be called as:\n\
\n\
    val <- func(reg, val, isWrite)\n\
\n\
and must return an integer or raise an exception.\n\
reg is the number of the register to be accessed.\n\
val is a word value to be stored if isWrite is nonzero.\n\
isWrite is nonzero if a write is being requested;\n\
it is zero for read requests.");

static void pyprof(msp430_ctx_t  *ctx,
		   int            op,
		   int            ticks,
		   void          *xtra) {
  PyObject *func = (PyObject *) xtra;
  PyObject *ret;

  if ((ret = PyObject_CallFunction(func, "ii", op, ticks)) == NULL) {
    PyErr_Clear();
  }
  Py_XDECREF(ret);
}

static PyObject *setprf(msp430object *self, PyObject *args) {
  msp430_prof_func  old, new;
  void             *oldx;
  PyObject         *func;
  int               rc;

  if (!PyArg_ParseTuple(args, "O", &func))
    return NULL;
  if (func != Py_None) {
    if (!PyCallable_Check(func)) {
      PyErr_SetString(PyExc_TypeError, "func must be callable");
      return NULL;
    }
    new = pyprof;
  } else {
    new  = NULL;
    func = NULL;
  }
  if ((rc = msp430_get_prof(ctx(self), &old, &oldx)) != MSP430_SUCCESS)
    return exc("msp430_get_prof", rc);
  if ((rc = msp430_set_prof(ctx(self), new, func)) != MSP430_SUCCESS)
    return exc("msp430_set_prof", rc);
  if (old == pyprof) {
    Py_XDECREF((PyObject *) oldx);
  }
  if (func != NULL)
    Py_INCREF(func);

  Py_INCREF(Py_None);
  return Py_None;
}

PyDoc_STRVAR(setprf_doc,
"set_prf(func)\n\
\n\
Set the profile handler to func. After each instruction\n\
is executed, func will be called as:\n\
\n\
    func(op, ticks)\n\
\n\
The return value of func is ignored. op will be one\n\
of the MSP430_INSN_xyz constants and ticks will be\n\
the number of clocks used by this instruction.");

static PyMethodDef methods[] = {
  { "sfr_nam", (PyCFunction) sfrnam, METH_VARARGS, sfrnam_doc },
  { "get_reg", (PyCFunction) getreg, METH_VARARGS, getreg_doc },
  { "set_reg", (PyCFunction) setreg, METH_VARARGS, setreg_doc },
  { "clr_reg", (PyCFunction) clrreg, METH_NOARGS,  clrreg_doc },
  { "wfetch",  (PyCFunction) wfetch, METH_VARARGS, wfetch_doc },
  { "bfetch",  (PyCFunction) bfetch, METH_VARARGS, bfetch_doc },
  { "wstore",  (PyCFunction) wstore, METH_VARARGS, wstore_doc },
  { "bstore",  (PyCFunction) bstore, METH_VARARGS, bstore_doc },
  { "program", (PyCFunction) progm,  METH_VARARGS,  progm_doc },
  { "clr_prf", (PyCFunction) clrprf, METH_NOARGS,  clrprf_doc },
  { "ena_prf", (PyCFunction) enaprf, METH_VARARGS, enaprf_doc },
  { "reset",   (PyCFunction) reset,  METH_NOARGS,   reset_doc },
  { "ld_ihex", (PyCFunction) ldihex, METH_VARARGS, ldihex_doc },
  { "run",     (PyCFunction) run,    METH_VARARGS,    run_doc },
  { "strerr",  (PyCFunction) strerr, METH_VARARGS, strerr_doc },
  { "set_sfr", (PyCFunction) setsfr, METH_VARARGS, setsfr_doc },
  { "sfr_byt", (PyCFunction) sfrbyt, METH_VARARGS, sfrbyt_doc },
  { "sfr_wrd", (PyCFunction) sfrwrd, METH_VARARGS, sfrwrd_doc },
  { "set_trp", (PyCFunction) setrap, METH_VARARGS, setrap_doc },
  { "set_mem", (PyCFunction) setmem, METH_VARARGS, setmem_doc },
  { "set_rgf", (PyCFunction) setrgf, METH_VARARGS, setrgf_doc },
  { "set_prf", (PyCFunction) setprf, METH_VARARGS, setprf_doc },
  { NULL, NULL },
};

/***********************************************************************
 * attributes
 */

static PyObject *get_insns(msp430object *self) {
  return PyLong_FromUnsignedLong(ctx(self)->insns);
}

PyDoc_STRVAR(ainsns_doc,
	     "Cumulative number of instructions executed");

static PyObject *get_ticks(msp430object *self) {
  return PyLong_FromUnsignedLong(ctx(self)->ticks);
}

PyDoc_STRVAR(aticks_doc,
	     "Cumulative number of CPU clocks");

static PyObject *get_prof(msp430object *self) {
  return PyInt_FromLong(ctx(self)->profile ? 1 : 0);
}

PyDoc_STRVAR(aprofile_doc,
	     "Profiler enable status");

static PyObject *get_pena(msp430object *self) {
  return PyInt_FromLong(ctx(self)->penable ? 1 : 0);
}

PyDoc_STRVAR(apenable_doc,
	     "Profiler deferred-enable status");

static PyObject *get_exc(msp430object *self) {
  PyObject *ret, *tmp;

  if ((ret = PyTuple_New(4)) == NULL)
    return NULL;
  if ((tmp = PyInt_FromLong(ctx(self)->exc_info[0])) == NULL)
    goto bad;
  PyTuple_SET_ITEM(ret, 0, tmp);
  if ((tmp = PyInt_FromLong(ctx(self)->exc_info[1])) == NULL)
    goto bad;
  PyTuple_SET_ITEM(ret, 1, tmp);
  if ((tmp = PyInt_FromLong(ctx(self)->exc_info[2])) == NULL)
    goto bad;
  PyTuple_SET_ITEM(ret, 2, tmp);
  if ((tmp = PyInt_FromLong(ctx(self)->exc_info[3])) == NULL)
    goto bad;
  PyTuple_SET_ITEM(ret, 3, tmp);
  return ret;

 bad:
  Py_DECREF(ret);
  return NULL;
}

PyDoc_STRVAR(aexc_info_doc,
"Return last MSP430 exception data as a 4-tuple\n\
\n\
    (exc#, arg1, arg2, line#)");

static PyObject *get_in_exec(msp430object *self) {
  return PyInt_FromLong(ctx(self)->in_exec ? 1 : 0);
}

PyDoc_STRVAR(ain_exec_doc,
	     "Nonzero if and only if CPU is fetching an instruction");

static PyObject *get_last_pc(msp430object *self) {
  return PyInt_FromLong(ctx(self)->insn_pc);
}

PyDoc_STRVAR(alast_pc_doc,
	     "PC for last successfully decoded instruction");

static PyObject *get_op_ndx(msp430object *self) {
  return PyInt_FromLong(ctx(self)->op_ndx);
}

PyDoc_STRVAR(aop_ndx_doc,
	     "Number of words in current/last instruction");

static PyObject *get_opcode(msp430object *self) {
  msp430_ctx_t *c = ctx(self);
  PyObject     *ret, *tmp;
  int           i;

  if (c->op_ndx > 3) {
    PyErr_SetString(PyExc_ValueError, "op_ndx out of range!");
    return NULL;
  }
  if ((ret = PyTuple_New(c->op_ndx)) == NULL)
    return NULL;
  for (i = 0; i < c->op_ndx; i++) {
    if ((tmp = PyInt_FromLong(c->opcode[i])) == NULL)
      goto bad;
    PyTuple_SET_ITEM(ret, i, tmp);
  }
  return ret;

 bad:
  Py_DECREF(ret);
  return NULL;
}

PyDoc_STRVAR(aopcode_doc,
	     "Opcode words in current/last instruction");

static PyObject *get_sreg(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.sreg);
}

PyDoc_STRVAR(asreg_doc, "Source register");

static PyObject *get_sa(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.sa);
}

PyDoc_STRVAR(asa_doc, "Source address mode");

static PyObject *get_dreg(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.dreg);
}

PyDoc_STRVAR(adreg_doc, "Destination register");

static PyObject *get_da(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.da);
}

PyDoc_STRVAR(ada_doc, "Destination address mode");

static PyObject *get_op(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.op);
}

PyDoc_STRVAR(aop_doc, "Last instruction number");

static PyObject *get_type(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.type);
}

PyDoc_STRVAR(atype_doc, "Last instruction type");

static PyObject *get_iticks(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.ticks);
}

PyDoc_STRVAR(aiticks_doc, "Last instruction clock count");

static PyObject *get_daddr(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.daddr);
}

PyDoc_STRVAR(adaddr_doc, "Last instruction destination address");

static PyObject *get_res(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.res);
}

PyDoc_STRVAR(ares_doc, "Last instruction result");

static PyObject *get_bw(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.bw);
}

PyDoc_STRVAR(abw_doc, "Last instruction byte flag");

static PyObject *get_mask(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.mask);
}

PyDoc_STRVAR(amask_doc, "Last instruction mask");

static PyObject *get_sign(msp430object *self) {
  return PyInt_FromLong(ctx(self)->info.sign);
}

PyDoc_STRVAR(asign_doc, "Last instruction sign");

static PyGetSetDef getsets[] = {
  { "insns",    (getter) get_insns,   NULL, ainsns_doc },
  { "ticks",    (getter) get_ticks,   NULL, aticks_doc },
  { "profile",  (getter) get_prof,    NULL, aprofile_doc },
  { "penable",  (getter) get_pena,    NULL, apenable_doc },
  { "exc_info", (getter) get_exc,     NULL, aexc_info_doc },
  { "in_exec",  (getter) get_in_exec, NULL, ain_exec_doc },
  { "last_pc",  (getter) get_last_pc, NULL, alast_pc_doc },
  { "op_ndx",   (getter) get_op_ndx,  NULL, aop_ndx_doc },
  { "opcode",   (getter) get_opcode,  NULL, aopcode_doc },
  { "sreg",     (getter) get_sreg,    NULL, asreg_doc },
  { "sa",       (getter) get_sa,      NULL, asa_doc },
  { "dreg",     (getter) get_dreg,    NULL, adreg_doc },
  { "da",       (getter) get_da,      NULL, ada_doc },
  { "op",       (getter) get_op,      NULL, aop_doc },
  { "type",     (getter) get_type,    NULL, atype_doc },
  { "iticks",   (getter) get_iticks,  NULL, aiticks_doc },
  { "daddr",    (getter) get_daddr,   NULL, adaddr_doc },
  { "res",      (getter) get_res,     NULL, ares_doc },
  { "bw",       (getter) get_bw,      NULL, abw_doc },
  { "mask",     (getter) get_mask,    NULL, amask_doc },
  { "sign",     (getter) get_sign,    NULL, asign_doc },
  { NULL },
};

/***********************************************************************
 * type decl
 */

PyDoc_STRVAR(type_doc,
"See the module docs.");

static PyTypeObject MSP430type = {
  PyObject_HEAD_INIT(NULL)
  0,
  "_soft430.soft430",
  sizeof(msp430object),
  0,
  (destructor) del,
  (printfunc) print,
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
  0,
  0,
  0,
  0,
  methods,
  0,
  getsets,
};

/***********************************************************************
 * module stuff
 */

static PyObject *new(PyObject *self, PyObject *args) {
  return (PyObject *) _new();
}

static msp430_ctx_t *get_ctx(void *o) {
  if (o == NULL)
    return NULL;
  if (PyObject_Type((PyObject *) o) != (PyObject *) &MSP430type) {
    PyErr_SetString(PyExc_TypeError, "Bad type");
    return NULL;
  }
  return ctx((msp430object *) o);
}

static msp430_word_t get_reg(msp430_ctx_t *ctx, int reg) {
  msp430_word_t val = 0;

  msp430_reg(ctx, reg, &val, 0);
  return val;
}

static msp430api_t api;

PyDoc_STRVAR(new_doc,
"new() -> cpu object\n\
\n\
Returns a new MSP430F1611 CPU object.");

static PyMethodDef funcs[] = {
  { "new", (PyCFunction) new, METH_NOARGS, new_doc },
  { NULL, NULL },
};

PyDoc_STRVAR(module_doc,
"This module implements a Texas Instruments MSP430F1611\n\
microcontroller in software. See the documentation for\n\
MSP430Type and new() for details.\n\
\n\
This module defines an exception named MSP430Error and\n\
an alias named error.\n\
\n\
_soft430.error is raised when an error condition\n\
occurs in the simulator. Errors generated at the C\n\
level will raise this exception with three args:\n\
\n\
    (func_name, error_desc, error_code)\n\
\n\
where func_name is the name of the API function that\n\
raised the error, error_code is one of the MSP430_EXC_*\n\
constants, and error_desc is a string description of the\n\
error.\n\
\n\
Python callbacks may raise this exception in one of two\n\
ways: with a single error_code argument, or with three\n\
arguments. In the latter case, the third argument is\n\
returned to the C API as the exception (the first two\n\
arguments are ignored). If this protocol is not followed\n\
or if python callbacks raise some other exception, the C\n\
API gets a generic MSP430_EXC_PYTHON_FAULT.\n\
\n\
The CPU object contains a 4-tuple describing the last\n\
error:\n\
\n\
    (error_code, detail_1, detail_2, line#)\n\
\n\
where error_code is an MSP430_EXC_* code, detail_* are\n\
word values describing the context (see the C source), and\n\
line# is the line number in soft430.c where the error was\n\
raised.");

static char *rcsid = "$Id: soft430.c,v 1.32 2007/07/27 15:22:22 DEV Exp $";

PyMODINIT_FUNC init_soft430(void) {
  PyObject *m, *a;

  if ((MSP430Error = PyErr_NewException("_soft430.error",
					PyExc_RuntimeError,
					NULL)) == NULL)
    return;
  MSP430type.ob_type = &PyType_Type;
  if (PyType_Ready(&MSP430type) < 0)
    return;
  if ((m = Py_InitModule3("_soft430", funcs, module_doc)) == NULL)
    return;
  api.get_ctx = get_ctx;
  api.get_sfr = msp430_get_sfr;
  api.set_reg = msp430_set_regf;
  api.set_mem = msp430_set_mem;
  api.set_prf = msp430_set_prof;
  api.ld_ihex = msp430_load_ihex;
  api.get_reg = get_reg;
  if ((a = PyCObject_FromVoidPtr(&api, NULL)) == NULL)
    return;
  Py_INCREF(a);
  PyModule_AddObject(m,  "__C_API", a);
  PyModule_AddObject(m, "MSP430Type", (PyObject *) &MSP430type);
  PyModule_AddObject(m, "MSP430Error", MSP430Error);
  Py_INCREF(MSP430Error);
  PyModule_AddObject(m, "error", MSP430Error);
  PyModule_AddStringConstant(m, "__version__", rcsid);
#define DC(x) if (PyModule_AddIntConstant(m, #x, x)) return

  DC(MSP430_NREGS);
  DC(MSP430_NSFR);
  DC(MSP430_NRAM);
  DC(MSP430_NPROG);
  DC(MSP430_BSL_BASE);
  DC(MSP430_UF_BASE);
  DC(MSP430_RAM_BASE);
  DC(MSP430_ROM_BASE);
  DC(MSP430_INSN_TYPE1);
  DC(MSP430_INSN_TYPE2);
  DC(MSP430_INSN_TYPE3);
  DC(MSP430_INSN_ALL);
  DC(MSP430_INSN_MOV);
  DC(MSP430_INSN_ADD);
  DC(MSP430_INSN_ADDC);
  DC(MSP430_INSN_SUBC);
  DC(MSP430_INSN_SUB);
  DC(MSP430_INSN_CMP);
  DC(MSP430_INSN_DADD);
  DC(MSP430_INSN_BIT);
  DC(MSP430_INSN_BIC);
  DC(MSP430_INSN_BIS);
  DC(MSP430_INSN_XOR);
  DC(MSP430_INSN_AND);
  DC(MSP430_INSN_RRC);
  DC(MSP430_INSN_SWPB);
  DC(MSP430_INSN_RRA);
  DC(MSP430_INSN_SXT);
  DC(MSP430_INSN_PUSH);
  DC(MSP430_INSN_CALL);
  DC(MSP430_INSN_RETI);
  DC(MSP430_INSN_JNZ);
  DC(MSP430_INSN_JZ);
  DC(MSP430_INSN_JNC);
  DC(MSP430_INSN_JC);
  DC(MSP430_INSN_JN);
  DC(MSP430_INSN_JGE);
  DC(MSP430_INSN_JL);
  DC(MSP430_INSN_JMP);
  DC(MSP430_INSN_CNT);
  DC(MSP430_PROFILE_CLEAR);
  DC(MSP430_SUCCESS);
  DC(MSP430_EXC_BAD_ALIGN);
  DC(MSP430_EXC_BAD_SR);
  DC(MSP430_EXC_ENOSYS);
  DC(MSP430_EXC_BAD_EXC);
  DC(MSP430_EXC_ENXIO);
  DC(MSP430_EXC_BAD_ADDRMODE);
  DC(MSP430_EXC_NULL_CTX);
  DC(MSP430_EXC_BAD_REG);
  DC(MSP430_EXC_BAD_WORD);
  DC(MSP430_EXC_BAD_STORE);
  DC(MSP430_EXC_BAD_EXEC);
  DC(MSP430_EXC_EINVAL);
  DC(MSP430_EXC_OS_ERROR);
  DC(MSP430_EXC_CPU_TIME);
  DC(MSP430_EXC_BAD_BYTE);
  DC(MSP430_EXC_BAD_TRAP);
  DC(MSP430_EXC_BAD_SFR);
  DC(MSP430_EXC_WDT_FAULT);
  DC(MSP430_EXC_FC_FAULT);
  DC(MSP430_EXC_BAD_IHEX);
  DC(MSP430_EXC_PYTHON_FAULT);
  DC(MSP430_EXC_CNT);
  DC(MSP430_C_MASK);
  DC(MSP430_Z_MASK);
  DC(MSP430_N_MASK);
  DC(MSP430_I_MASK);
  DC(MSP430_V_MASK);
  DC(MSP430_PROFILE_OFF);
  DC(MSP430_PROFILE_NOW);
  DC(MSP430_PROFILE_NEXT);
}

#endif

/*** EOF soft430.c */

