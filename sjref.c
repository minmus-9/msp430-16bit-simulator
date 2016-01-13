/***********************************************************************
 * sjref.c -- sj/sjcs c ref impl and test
 */

#include <string.h>

#ifdef __MSP430__
#warning "Loaded MSP430 defs"
#include <io.h>
#include <signal.h>
#endif

#if 0
#include "softmsp.h"
#else
#include "soft430.h"
#endif

#define MAX_SECURITY

#ifdef MAX_SECURITY
#warning "max sec"
#else
#warning "min sec"
#endif

#if 0
typedef unsigned char uint8_t;
typedef unsigned int  uint16_t;
#endif

#ifdef USE_ASM

#ifndef MAX_SECURITY
#define MAX_SECURITY
#endif

extern void sjcs_ksched(uint16_t *ctx, uint8_t *key);

extern void sjcs_auth(uint16_t *ctx, uint8_t *nonce,
		      uint8_t  *aad, uint8_t aadlen,
		      uint8_t  *ptd, uint8_t ptdlen,
		      uint8_t  *mac);

#else

/*** sj sbox */
static const uint8_t SJ_F[256] = {
  0xa3, 0xd7, 0x09, 0x83, 0xf8, 0x48, 0xf6, 0xf4,
  0xb3, 0x21, 0x15, 0x78, 0x99, 0xb1, 0xaf, 0xf9,
  0xe7, 0x2d, 0x4d, 0x8a, 0xce, 0x4c, 0xca, 0x2e,
  0x52, 0x95, 0xd9, 0x1e, 0x4e, 0x38, 0x44, 0x28,
  0x0a, 0xdf, 0x02, 0xa0, 0x17, 0xf1, 0x60, 0x68,
  0x12, 0xb7, 0x7a, 0xc3, 0xe9, 0xfa, 0x3d, 0x53,
  0x96, 0x84, 0x6b, 0xba, 0xf2, 0x63, 0x9a, 0x19,
  0x7c, 0xae, 0xe5, 0xf5, 0xf7, 0x16, 0x6a, 0xa2,
  0x39, 0xb6, 0x7b, 0x0f, 0xc1, 0x93, 0x81, 0x1b,
  0xee, 0xb4, 0x1a, 0xea, 0xd0, 0x91, 0x2f, 0xb8,
  0x55, 0xb9, 0xda, 0x85, 0x3f, 0x41, 0xbf, 0xe0,
  0x5a, 0x58, 0x80, 0x5f, 0x66, 0x0b, 0xd8, 0x90,
  0x35, 0xd5, 0xc0, 0xa7, 0x33, 0x06, 0x65, 0x69,
  0x45, 0x00, 0x94, 0x56, 0x6d, 0x98, 0x9b, 0x76,
  0x97, 0xfc, 0xb2, 0xc2, 0xb0, 0xfe, 0xdb, 0x20,
  0xe1, 0xeb, 0xd6, 0xe4, 0xdd, 0x47, 0x4a, 0x1d,
  0x42, 0xed, 0x9e, 0x6e, 0x49, 0x3c, 0xcd, 0x43,
  0x27, 0xd2, 0x07, 0xd4, 0xde, 0xc7, 0x67, 0x18,
  0x89, 0xcb, 0x30, 0x1f, 0x8d, 0xc6, 0x8f, 0xaa,
  0xc8, 0x74, 0xdc, 0xc9, 0x5d, 0x5c, 0x31, 0xa4,
  0x70, 0x88, 0x61, 0x2c, 0x9f, 0x0d, 0x2b, 0x87,
  0x50, 0x82, 0x54, 0x64, 0x26, 0x7d, 0x03, 0x40,
  0x34, 0x4b, 0x1c, 0x73, 0xd1, 0xc4, 0xfd, 0x3b,
  0xcc, 0xfb, 0x7f, 0xab, 0xe6, 0x3e, 0x5b, 0xa5,
  0xad, 0x04, 0x23, 0x9c, 0x14, 0x51, 0x22, 0xf0,
  0x29, 0x79, 0x71, 0x7e, 0xff, 0x8c, 0x0e, 0xe2,
  0x0c, 0xef, 0xbc, 0x72, 0x75, 0x6f, 0x37, 0xa1,
  0xec, 0xd3, 0x8e, 0x62, 0x8b, 0x86, 0x10, 0xe8,
  0x08, 0x77, 0x11, 0xbe, 0x92, 0x4f, 0x24, 0xc5,
  0x32, 0x36, 0x9d, 0xcf, 0xf3, 0xa6, 0xbb, 0xac,
  0x5e, 0x6c, 0xa9, 0x13, 0x57, 0x25, 0xb5, 0xe3,
  0xbd, 0xa8, 0x3a, 0x01, 0x05, 0x59, 0x2a, 0x46,
};

/*** quarter of G */
#define G4(x) \
tmp = SJ_F[(uint8_t) x ^ *skey++]; \
x   = (x << 8) | (x >> 8); \
x  ^= (uint16_t) tmp

/*** half of G */
#define G2(x) G4(x); G4(x)

/*** G itself */
#define G(x) G2(x); G2(x)

/*** A-rule */
#define A(x, y) \
ctr++; \
G(x); \
y ^= x ^ ctr

/*** B-rule */
#define B(x, y) \
ctr++; \
y ^= x ^ ctr; \
G(x)

/*** for scheduled key resync */
#define RK skey -= 20;

/*** half an encryption */
static void TB(uint8_t  **skeyp,
	       uint16_t  *ctrp,
	       uint8_t   *bp) {
  /* load 'em up */
  uint8_t  *skey = *skeyp, tmp;
  uint16_t w1, w2, w3, w4, ctr = *ctrp;

  w1 = (bp[0] << 8) | bp[1];
  w2 = (bp[2] << 8) | bp[3];
  w3 = (bp[4] << 8) | bp[5];
  w4 = (bp[6] << 8) | bp[7];

  /* do it */
  A(w1, w4);
  A(w4, w3);
  A(w3, w2);
  A(w2, w1);

  if (ctr == 20) RK;
  A(w1, w4);
  if (ctr == 5) RK;
  A(w4, w3);
  A(w3, w2);
  A(w2, w1);

  B(w1, w2);
  if (ctr == 25) RK;
  B(w4, w1);
  if (ctr == 10) RK;
  B(w3, w4);
  B(w2, w3);

  B(w1, w2);
  B(w4, w1);
  if (ctr == 30) RK;
  B(w3, w4);
  if (ctr == 15) RK;
  B(w2, w3);

  /* put 'em back */
  *ctrp  = ctr;
  *skeyp = skey;

  *bp++  = w1 >> 8;
  *bp++  = w1;
  *bp++  = w2 >> 8;
  *bp++  = w2;
  *bp++  = w3 >> 8;
  *bp++  = w3;
  *bp++  = w4 >> 8;
  *bp++  = w4;
}

/*** sj block encrypt */
static void enc(uint8_t *skey,
		uint8_t *bp) {
  uint16_t ctr = 0;

  TB(&skey, &ctr, bp);
  TB(&skey, &ctr, bp);
}

static void sjks(uint8_t *ctx,
		 uint8_t *key) {
  memcpy(ctx, key, 10);
  memcpy(ctx+10, key, 10);
}

#ifndef NO_REF_MAIN
static uint16_t testsj(void) {
  uint8_t p[8]  = { 0x33, 0x22, 0x11, 0x00,
		    0xdd, 0xcc, 0xbb, 0xaa };
  uint8_t c[8]  = { 0x25, 0x87, 0xca, 0xe2,
		    0x7a, 0x12, 0xd3, 0x00 };
  uint8_t k[10] = { 0x00, 0x99, 0x88, 0x77,
		    0x66, 0x55, 0x44, 0x33,
		    0x22, 0x11 };
  uint8_t sk[20];

  /*** schedule key */
  sjks(sk, k);

  /*** encrypt */
  enc(sk, p);
  
  /*** verify test vector */
  if (memcmp(p, c, 8))
    return 0x8000;

#ifndef SOFTMSP
  printf("sj pass\n");
#endif

  return 0;
}
#endif

/**********************************************************************/

void sjcs_ksched(uint16_t *_ctx,
		 uint8_t  *key) {
  uint8_t  buf[8], *ctx = (uint8_t *) _ctx, *h = ctx + 36;
  uint16_t ctr = 0;

  sjks(ctx, key);
  memset(buf, 0, 8);
  TB(&ctx, &ctr, buf);
#ifdef MAX_SECURITY
  TB(&ctx, &ctr, buf);
#endif
  memcpy(h, buf, 8);
}

static void sjcsns(uint8_t *ctx,
		   uint8_t *nonce) {
  uint8_t  i, *r = ctx + 20, *a = r + 8, *h = a + 8;
  uint16_t ctr = 0;

  for (i = 0; i < 8; i++)
    r[i] = h[i] ^ nonce[i];
  TB(&ctx, &ctr, r);
#ifdef MAX_SECURITY
  TB(&ctx, &ctr, r);
#endif
  for (i = 0; i < 8; i++)
    r[i] ^= h[i];
  memset(a, 0, 8);
}

static void sjcsmac(uint8_t *ctx,
		    uint8_t *mac) {
  uint8_t  i, *r = ctx + 20, *a = r + 8;
  uint16_t ctr = 0;

  for (i = 0; i < 8; i++)
    mac[i] = r[i] ^ a[i];
  TB(&ctx, &ctr, mac);
  TB(&ctx, &ctr, mac);
  for (i = 0; i < 8; i++)
    mac[i] ^= a[i];
}

static void gf(uint8_t *x) {
  uint8_t b, c, *y = x;

  b  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1);
  x++;
  c  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | b;
  x++;
  b  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | c;
  x++;
  c  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | b;
  x++;
  b  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | c;
  x++;
  c  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | b;
  x++;
  b  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | c;
  x++;
  c  = (uint8_t) (*x & 0x01) ? 0x80 : 0x00;
  *x = (uint8_t) (*x >> 1) | b;
  if (c)
    *y ^= 0xd8;
}

static void sjcsadd(uint8_t *ctx,
		    uint8_t *data) {
  uint16_t ctr = 0;
  uint8_t  i, buf[8], *r = ctx + 20, *a = r + 8;

  for (i = 0; i < 8; i++)
    buf[i] = data[i] ^ r[i];
  TB(&ctx, &ctr, buf);
  gf(r);
  gf(a);
  for (i = 0; i < 8; i++)
    a[i] ^= buf[i];
}

static void sjcsproc(uint8_t *ctx,
		     uint8_t *data,
		     uint8_t  dlen) {
  uint8_t  buf[8], *a = ctx + 28;

  a[a[0] & 7] ^= dlen & 0xff;

  while (dlen >= 8) {
    sjcsadd(ctx, data);
    data += 8;
    dlen -= 8;
  }
  if (dlen) {
    memset(buf, 0, 8);
    memcpy(buf, data, dlen);
    sjcsadd(ctx, buf);
  }
}

void sjcs_auth(uint16_t *_ctx, uint8_t *nonce,
	       uint8_t  *aad,  uint8_t aadlen,
	       uint8_t  *ptd,  uint8_t ptdlen,
	       uint8_t  *mac) {
  uint8_t *ctx = (uint8_t *) _ctx;

  sjcsns(ctx, nonce);
  if (aadlen)
    sjcsproc(ctx, aad, aadlen);
  if (ptdlen)
    sjcsproc(ctx, ptd, ptdlen);
  sjcsmac(ctx, mac);
}

/*** !USE_ASM */
#endif

#ifndef NO_REF_MAIN
/*** NB ctx MUST be word-aligned */
static uint16_t _ctx[22];
static uint8_t  mac[8];

static uint16_t testcs(void) {
  uint16_t  rc = 0, _rc = 1;
  uint8_t *ctx = (uint8_t *) _ctx;

  uint8_t k[10] = { 0x00, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33,
		    0x22, 0x11 };
  uint8_t n[8]  = { 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x80, 0xff };
  uint8_t p[16] = { 0x33, 0x22, 0x11, 0x00, 0xdd, 0xcc, 0xbb, 0xaa,
		    0x33, 0x22, 0x11, 0x00, 0xdd, 0xcc, 0xbb, 0xaa };

#ifdef MAX_SECURITY
  /*** full crypt during ksched and nsched */
  uint8_t hx[8] = { 0xa5, 0xa4, 0x59, 0xaf, 0x7e, 0xba, 0x7e, 0x8c };
  uint8_t rx[8] = { 0xc2, 0xb8, 0xa9, 0x00, 0x12, 0x13, 0x9e, 0x7b };
  uint8_t ax[8] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
  uint8_t mx[8] = { 0xd5, 0x1f, 0x3e, 0x56, 0x57, 0x49, 0x7a, 0x8c };

  uint8_t rx8[8]  = { 0xb9, 0x5c, 0x54, 0x80, 0x09, 0x09, 0xcf, 0x3d };
  uint8_t ax8[8]  = { 0xdf, 0xaa, 0x73, 0x37, 0x9d, 0xb4, 0x0f, 0x4b };
  uint8_t mx8[8]  = { 0x38, 0x6c, 0x18, 0x36, 0x3b, 0x94, 0x7f, 0x75 };

  uint8_t rx12[8] = { 0x84, 0xae, 0x2a, 0x40, 0x04, 0x84, 0xe7, 0x9e };
  uint8_t ax12[8] = { 0x70, 0x8e, 0xf6, 0x49, 0xc3, 0x89, 0x55, 0x3f };
  uint8_t mx12[8] = { 0x92, 0x4a, 0x25, 0x17, 0x91, 0xc4, 0x3f, 0x48 };

  uint8_t rx16[8] = { 0x84, 0xae, 0x2a, 0x40, 0x04, 0x84, 0xe7, 0x9e };
  uint8_t ax16[8] = { 0x5d, 0x14, 0xef, 0x6f, 0x06, 0x1e, 0x57, 0xab };
  uint8_t mx16[8] = { 0x51, 0x01, 0xb9, 0xab, 0xf8, 0xbf, 0xea, 0x48 };

  uint8_t rx13[8] = { 0x84, 0xae, 0x2a, 0x40, 0x04, 0x84, 0xe7, 0x9e };
  uint8_t ax13[8] = { 0x67, 0xc9, 0x86, 0x78, 0xd5, 0x70, 0x22, 0x58 };
  uint8_t mx13[8] = { 0x42, 0x1c, 0x1f, 0xd1, 0x3a, 0x6d, 0x94, 0xec };

#else
  /*** half crypt during ksched and nsched */
  uint8_t hx[8] = { 0x71, 0x13, 0x1c, 0x50, 0xd3, 0x20, 0xdc, 0x4a };

  uint8_t rx[8] = { 0x8b, 0xbe, 0x1d, 0xf4, 0xc8, 0xe9, 0x23, 0x28 };
  uint8_t ax[8] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
  uint8_t mx[8] = { 0xa6, 0xda, 0xc1, 0x99, 0xe5, 0xfa, 0x87, 0x28 };

  uint8_t rx8[8]  = { 0x45, 0xdf, 0x0e, 0xfa, 0x64, 0x74, 0x91, 0x94 };
  uint8_t ax8[8]  = { 0xe1, 0x84, 0x7f, 0xdf, 0xc7, 0x2a, 0x4b, 0x0e };
  uint8_t mx8[8]  = { 0x9e, 0x1d, 0x90, 0xf4, 0x82, 0x20, 0x16, 0x24 };

  uint8_t rx12[8] = { 0x22, 0xef, 0x87, 0x7d, 0x32, 0x3a, 0x48, 0xca };
  uint8_t ax12[8] = { 0x06, 0x5a, 0x03, 0x64, 0xdd, 0xaa, 0x45, 0x5b };
  uint8_t mx12[8] = { 0xb1, 0x23, 0xd8, 0x00, 0x03, 0x4d, 0x78, 0x4c };

  uint8_t rx16[8] = { 0x22, 0xef, 0x87, 0x7d, 0x32, 0x3a, 0x48, 0xca };
  uint8_t ax16[8] = { 0xc7, 0x19, 0x6d, 0xd8, 0x52, 0x5b, 0x17, 0x6f };
  uint8_t mx16[8] = { 0xf4, 0x71, 0x3c, 0xc1, 0x5b, 0xb6, 0xfd, 0xda };

  uint8_t rx13[8] = { 0x22, 0xef, 0x87, 0x7d, 0x32, 0x3a, 0x48, 0xca };
  uint8_t ax13[8] = { 0x95, 0x31, 0x3b, 0xd4, 0xc2, 0xaf, 0x53, 0x15 };
  uint8_t mx13[8] = { 0x1a, 0xc3, 0x76, 0x9a, 0xb3, 0x75, 0xc2, 0x15 };
#endif

#define BONK { rc = _rc; goto dump; } else { ++_rc; }

  /* 1-4 */
  sjcs_ksched(_ctx, k);
  if (memcmp(ctx+36, hx, 8))
    BONK;

  sjcs_auth(_ctx, n, NULL, 0, NULL, 0, mac);
  if (memcmp(ctx+20, rx, 8))
    BONK;
  if (memcmp(ctx+28, ax, 8))
    BONK;
  if (memcmp(mac, mx, 8))
    BONK;

  /* 5-7 */
  sjcs_auth(_ctx, n, p, 8, NULL, 0, mac);
  if (memcmp(ctx+20, rx8, 8))
    BONK;
  if (memcmp(ctx+28, ax8, 8))
    BONK;
  if (memcmp(mac, mx8, 8))
    BONK;

  /* 8-10 */
  /*** mac of 12 */
  sjcs_auth(_ctx, n, NULL, 0, NULL, 0, mac);
  if (memcmp(ctx+20, rx, 8))
    BONK;
  if (memcmp(ctx+28, ax, 8))
    BONK;
  if (memcmp(mac, mx, 8))
    BONK;

  /* 11-13 */
  sjcs_auth(_ctx, n, p, 12, NULL, 0, mac);
  if (memcmp(ctx+20, rx12, 8))
    BONK;
  if (memcmp(ctx+28, ax12, 8))
    BONK;
  if (memcmp(mac, mx12, 8))
    BONK;

  /* 14-16 */
  /*** mac of 16 */
  sjcs_auth(_ctx, n, NULL, 0, NULL, 0, mac);
  if (memcmp(ctx+20, rx, 8))
    BONK;
  if (memcmp(ctx+28, ax, 8))
    BONK;
  if (memcmp(mac, mx, 8))
    BONK;

  /* 17-19 */
  sjcs_auth(_ctx, n,  p, 16, NULL, 0, mac);
  if (memcmp(ctx+20, rx16, 8))
    BONK;
  if (memcmp(ctx+28, ax16, 8))
    BONK;
  if (memcmp(mac, mx16, 8))
    BONK;

  /* 20-22 */
  /*** mac of 13 */
  sjcs_auth(_ctx, n, NULL, 0, NULL, 0, mac);
  if (memcmp(ctx+20, rx, 8))
    BONK;
  if (memcmp(ctx+28, ax, 8))
    BONK;
  if (memcmp(mac, mx, 8))
    BONK;

  /* 23-25 */
  sjcs_auth(_ctx, n, p, 13, NULL, 0, mac);
  if (memcmp(ctx+20, rx13, 8))
    BONK;
  if (memcmp(ctx+28, ax13, 8))
    BONK;
  if (memcmp(mac, mx13, 8))
    BONK;

 dump:
#ifndef SOFTMSP
  if (rc) {
   uint8_t i, j;

   for (i = 0; i < 4; i++) {
     printf("%c: ", "rahm"[i]);
     for (j = 0; j < 8; j++)
       printf("0x%02x, ", ctx[20 + (i << 3) + j]);
     printf("\n");
   }
  } else {
    printf("cs pass\n");
  }
#endif

  return rc;
}

/**********************************************************************/

uint16_t myprog(void) {
  uint16_t rc;

#ifndef USE_ASM
  if ((rc = testsj()))
    return rc;
#endif
  if ((rc = testcs()))
    return rc;
  return 0;
}

int main(void) {
  uint16_t rc;

#ifdef SOFTMSP
  P1SEL &= 0xf0;
  P1DIR |= 0x0f;
  P1OUT &= 0xf0;

  /*** prep profiler */
  MSP430_PROF_DISABLE;
  MSP430_PROF_CLEAR;

  /*** run code */
  MSP430_PROF_ENABLE;
#endif
  rc = myprog();
#ifdef SOFTMSP
  MSP430_PROF_DISABLE;

  /*** get results */
  MSP430_SAVE_R15(rc);

  /*** done */
  MSP430_TRAP_HALT;

  if (rc)
    P1OUT |= 0x1;
  else
    P1OUT |= 0xf;
#endif

  return rc;
}
#endif

/*** EOF sjref.c */

