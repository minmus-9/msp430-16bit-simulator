/***********************************************************************
 * sjrefb.c
 */

#define USE_ASM
#define NO_REF_MAIN

#include "sjref.c"

/*** NB ctx *must* be word-aligned! */
static uint16_t ctx[22];

static uint8_t key[10] = {
  0x52, 0x95, 0xd9, 0x1e, 0x4e, 0x38, 0x44, 0x28, 0x0a, 0xdf,
};

static uint8_t  packet[56] = {
  0xa3, 0xd7, 0x09, 0x83, 0xf8, 0x48, 0xf6, 0xf4, /*** header */
  0xb3, 0x21, 0x15, 0x78, 0x99, 0xb1, 0xaf, 0xf9, /*** nonce */
  0xe7, 0x2d, 0x4d, 0x8a, 0xce, 0x4c, 0xca, 0x2e, /*** payload */
  0x52, 0x95, 0xd9, 0x1e, 0x4e, 0x38, 0x44, 0x28,
  0x0a, 0xdf, 0x02, 0xa0, 0x17, 0xf1, 0x60, 0x68,
  0x52, 0x95, 0xd9, 0x1e, 0x4e, 0x38, 0x44, 0x28,
  0x0a, 0xdf, 0x02, 0xa0, 0x17, 0xf1, 0x60, 0x68, /*** mac */
};

/* 4n clk */
static void __inline__ brief_pause(register uint16_t n) {
  __asm__ __volatile__ (
    "1: \n"
    " dec %[n] \n"
    " nop \n"
    " jne 1b \n"
    : [n] "+r"(n) : : "r13"
  );
}

int main(void) {
  uint16_t i;

  /* configure CPU */
  WRITE_SR(0);     /* make sure all clk-related bits are clear */
  dint();          /* disable interrupts */

  /* configure DCO for ~7.9715 MHz */
#warning "Running off 7.9715 MHz DCO"
  DCOCTL  = 0xc0; /* DCO=6, MOD=0 */
  BCSCTL1 = 0x87; /* XT2 off, LFXT1 watch xtal, ACLK is /1, RSELx=7 */
  BCSCTL2 = 0x01; /* MCLK=DCO/1, SMCLK=DCO/1, DCOR=P2.5 */

  /* configure leds */
  P1SEL &= 0xf0;
  P1OUT |= 0x0f; /* on */
  P1DIR |= 0x0f;

  sjcs_ksched(ctx, key);

  MSP430_PROF_DISABLE;
  MSP430_PROF_CLEAR;

#if 0
  for (i = 0; i < 100; i++)
    brief_pause(20000); /* 80k/8M = 10ms */
#endif

  P1OUT &= 0xf0; /* off */

  for (i = 0; i < 10000; i++) {
    MSP430_PROF_ENABLE;
    sjcs_auth(ctx,
	      packet + 8,       /*** nonce:8 */
	      packet, 8,        /*** header:8 */
	      packet + 16, 32,  /*** payload:32 */
	      packet + 48);     /*** mac:8 */
    MSP430_PROF_DISABLE;
  }

  P1OUT |= 0xf; /* on */

  MSP430_TRAP_HALT;

  return 0;
}

/*** EOF sjrefb.c */

