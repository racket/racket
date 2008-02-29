
#if defined(MZ_PRECISE_GC) && !defined(USE_COMPACT_3M_GC)

#ifdef _WIN32
# define LOG_APAGE_SIZE 16
#else
# define LOG_APAGE_SIZE 14
#endif

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define OBJH_WORD_SIZE 8
#else
# define OBJH_WORD_SIZE 4
#endif

struct objhead {
  unsigned long hash : ((8*OBJH_WORD_SIZE) - (4+3+LOG_APAGE_SIZE));
  /* the type and size of the object */
  unsigned long type : 3;
  /* these are the various mark bits we use */
  unsigned long mark : 1;
  unsigned long btc_mark : 1;
  /* these are used for compaction et al*/
  unsigned long moved : 1;
  unsigned long dead : 1;
  unsigned long size : LOG_APAGE_SIZE;
};

XFORM_NONGCING extern int GC_is_allocated(void *p);

#define OBJHEAD_HAS_HASH_BITS
#define OBJHEAD_HASH_BITS(p) ((struct objhead *)((void **)p - 1))->hash

#endif
