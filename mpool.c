/***********************************************************************
 * mpool.c -- linked list message pool thing
 */

#include "softmsp.h"

#include <io.h>
#include <sys/types.h>

/*** dummy tos message (right size for tmote) */
typedef struct message {
  uint8_t junk[46];
} message_t;


/***************************************
 * these two delimit what would be an
 * atomic section in nesc...
 *
 * i.e.,
 *
 *   __lock()
 *   <stuff>
 *   __unlock()
 *
 * is exactly equivalent to
 *
 *   atomic {
 *     <stuff>
 *   }
 *
 * though you'd better not do "return" in <stuff> !!!
 */
#define __lock() \
__asm__ __volatile__("push r2\n\t" \
    "dint\n\t" \
    "nop"::)

#define __unlock() \
__asm__ __volatile__("pop r2"::)

#if 0
/***********************************************************************
 * linked list implementation
 */

/*** linked list of messages */
typedef struct message_item message_item_t;

struct message_item {
  message_item_t *next; /* ptr to next item in list */
  message_t       msg;  /* what tos is interested in */
};

/*** a fancy way of saying "two" :D */
#define msg_ofs \
  (((char *) &(((message_item_t *) NULL)->msg)) - ((char *) NULL))

/***************************************
 * message pool stuff follows
 */

/*** size of pool */
#define N_MPOOL 16

/*** the pool and a ptr to its head */
static message_item_t mpool[N_MPOOL], *mpool_head = mpool;

/*** one-time init needed at system startup */
static void init_mpool(void) {
  int16_t i = N_MPOOL - 1;

  /*** last item points at nothing */
  mpool[i].next = NULL;
  /*** other items point to next item */
  for (i--; i >= 0; i--)
    mpool[i].next = &mpool[i+1];
}

/*** grab msg from pool -- returns NULL if none are available */
static message_t *get_from_pool(void) {
  message_item_t *rec;
  message_t      *ret = NULL;

  __lock();
  if ((rec = mpool_head) != NULL) {
    mpool_head = rec->next;
    ret        = &(rec->msg);
  }
  __unlock();

  return ret;
}

/*** return msg to pool (you'd better have gotten it from get_from_pool :-) */
static void return_to_pool(message_t *msg) {
  message_item_t *mi;

  __lock();
  mi         = (message_item_t *) (((char *) msg) - msg_ofs);
  mi->next   = mpool_head;
  mpool_head = mi;
  __unlock();
}

#else

/***********************************************************************
 * array implementation (no hwmul)
 */

/*** cannot be bigger than 16 */
#define N_MPOOL 16

message_t mpool[N_MPOOL];
uint16_t  bmask = 0;

static void init_mpool(void) {
  bmask = -1;
}

static message_t *get_from_pool(void) {
  uint16_t   item;
  message_t *ret = NULL;

  __lock();
  item   = bmask & (~bmask + 1); /* grab mask for lowest 1-bit in bmask */
  bmask ^= item;                 /* and turn it off in bmask */
  __unlock();
  if (item) {
    ret = mpool;                 /* linear search for result */
    while (item != 1) {
      item >>= 1;
      ret++;
    }
  }
  return ret;
}

static const uint16_t _bmaps[16] = {
  0x0001,
  0x0002,
  0x0004,
  0x0008,
  0x0010,
  0x0020,
  0x0040,
  0x0080,
  0x0100,
  0x0200,
  0x0400,
  0x0800,
  0x1000,
  0x2000,
  0x4000,
  0x8000,
};

static void return_to_pool(message_t *msg) {
  /*** NB: there's an implicit s-l-o-w SW multiply in msg-mpool !!! */
  uint16_t mask = _bmaps[msg - mpool];

  __lock();
  bmask |= mask;
  __unlock();
}

#endif

/***************************************
 * test code
 */

#define SUCCESS  0
#define FAIL    -1

int main(void) {
  uint16_t   i;
  message_t *m;

  /*** do one-time pool init for app */
  init_mpool();

  /*** use the pool */
  for (i = 0; i < 1000; i++) {
    /* grab a message from the pool
     *   linked list: 31 clocks, <16 us on tmote
     *   array impl:  40 clocks, 10 us on tmote
     */
    if ((m = get_from_pool()) == NULL)
      return FAIL;

    /* ...use it... */

    /* put it back when done
     *   linked list: 26 clocks, <7 us on tmote
     *   array impl:  44 clocks, 11 us on tmote
     */
    return_to_pool(m);
  }

  /*** linked list is almost 50% faster 
   ***
   *** this is largely due to the s-l-o-w multiply-by-46
   ***
   ***/

  return SUCCESS;
}

/*** EOF mpool.c */

