
#include <sys/types.h>
#include "sgc.h"

void *x;
void *y;

void work()
{
  char *v, *w;

  x = (char *)GC_malloc(20);
  y = (char *)GC_malloc(2000);

  v = (char *)GC_malloc(2);
  GC_gcollect();
  v[0] = 0;
  v = (char *)GC_malloc(10);
  GC_gcollect();
  v[0] = 0;
  v = (char *)GC_malloc(10000);
  w = (char *)GC_malloc(11000);
  GC_gcollect();
  v[0] = 0;
  v = (char *)GC_malloc(10);
  GC_gcollect();
  v[0] = 0;
  w[0] = 0;
}

typedef struct Chained
{
  struct Chained *next;
  int data[1];
} Chained;

#define MAX_C_SIZE 70
#define CHAIN_DEPTH 100
#define NUM_REPEATS 10

#define CHAINS_AS_LOCAL 1

#if !CHAINS_AS_LOCAL
Chained *chains[MAX_C_SIZE];
#endif

work2()
{
  int broken = 15;
  int i, j, k, l;
#if CHAINS_AS_LOCAL
  Chained *chains[MAX_C_SIZE];
#endif

#if !CHAINS_AS_LOCAL
  GC_add_roots((void *)chains, ((char *)chains) + sizeof(chains) + 1);
#endif

  printf("chains at %lx\n", (long)chains);

  for (l = NUM_REPEATS; l--; ) {
    printf("cycle: %d\n", NUM_REPEATS - l);

    for (i = 0; i < MAX_C_SIZE; i++)
      chains[i] = 0L;

    if (l & 1) {
      /* Same size together: */
      for (i = 0; i < MAX_C_SIZE; i++) {
	for (k = CHAIN_DEPTH; k--; ) {
	  Chained *c;
	  
	  c = (Chained *)GC_malloc(sizeof(Chained) + (i - 1) * sizeof(int));
	  for (j = 0; j < i; j++) {
	    c->data[j] = i;
	  }
	  
	  c->next = chains[i];
	  chains[i] = c;
	}
      }
    } else {
      /* Sizes shuffled: */
      for (k = CHAIN_DEPTH; k--; ) {
	for (i = 0; i < MAX_C_SIZE; i++) {
	  Chained *c;
	  
	  c = (Chained *)GC_malloc(sizeof(Chained) + (i - 1) * sizeof(int));
	  for (j = 0; j < i; j++) {
	    c->data[j] = i;
	  }
	  
	  c->next = chains[i];
	  chains[i] = c;
	}
      }      
    }

    for (i = 0; i < MAX_C_SIZE; i++) {
      Chained *c;
      
      c = chains[i];
      for (k = CHAIN_DEPTH; k--; c = c->next) {
	for (j = 0; j < i; j++)
	  if (c->data[j] != i) {
	    printf("broken: %d[%d][%d] = %d\n", i, (CHAIN_DEPTH - k), j, c->data[j]);
	    if (!(broken--))
	      return;
	  }
      }
    }
  }
}

main()
{
  int dummy;

  GC_set_stack_base((void *)&dummy);

  GC_add_roots((void *)&y, ((char *)&y) + sizeof(y) + 1);
  GC_add_roots((void *)&x, ((char *)&x) + sizeof(x) + 1);

  work();

  work2();
}
