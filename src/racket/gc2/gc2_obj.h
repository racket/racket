#ifndef GC2_OBJHEAD_H
#define GC2_OBJHEAD_H

#if defined(MZ_PRECISE_GC) && !defined(USE_COMPACT_3M_GC)

#ifdef _WIN32
# define LOG_APAGE_SIZE 16
#else
# define LOG_APAGE_SIZE 14
#endif
typedef struct objhead {
  /* the type and size of the object */
  uintptr_t type      : 3;
  /* these are the various mark bits we use */
  uintptr_t mark      : 1;
  uintptr_t btc_mark  : 1;
  /* these are used for compaction et al*/
  uintptr_t moved     : 1;
  uintptr_t dead      : 1;
  uintptr_t size      : LOG_APAGE_SIZE;
  /* leftover bits are used for hashing: */
  uintptr_t hash      : ((8 * sizeof(intptr_t)) - (4+3+LOG_APAGE_SIZE) );
} objhead;

#define OBJHEAD_SIZE (sizeof(objhead))
#define OBJPTR_TO_OBJHEAD(p) ((objhead *) (((char *)(p)) - OBJHEAD_SIZE))
#define OBJHEAD_TO_OBJPTR(p) ((void *) (((char *)(p)) + OBJHEAD_SIZE))


XFORM_NONGCING extern int GC_is_allocated(void *p);

#define OBJHEAD_HAS_HASH_BITS
#define OBJHEAD_HASH_BITS(p) (OBJPTR_TO_OBJHEAD(p)->hash)

#endif

#endif
