
/******************************************************************************/
/*                     OS-specific low-level allocator                        */
/******************************************************************************/

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif

static inline size_t vm_round_up_to_page_size(size_t len, size_t page_size) {
  len += (page_size -1) - (len & (page_size - 1));
  return len;
};

#if !( defined(_WIN32) || defined(OSKIT) )
typedef struct {
  char *start;
  long len;
  short age;
  short zeroed;
} FreeBlock;
#endif

typedef struct VM {
#if !( defined(_WIN32) || defined(OSKIT) )
  FreeBlock *freeblocks;
#endif
  size_t memory_allocated;
} VM;

static VM *vm_create() {
  VM *vm = ofm_malloc(sizeof(VM));
  memset(vm, 0, sizeof(VM));
#if !( defined(_WIN32) || defined(OSKIT) )
  #define BLOCKFREE_CACHE_SIZE 96
  vm->freeblocks = ofm_malloc(sizeof(FreeBlock) * BLOCKFREE_CACHE_SIZE);
  memset(vm->freeblocks, 0, sizeof(FreeBlock) * BLOCKFREE_CACHE_SIZE);
#endif
  return vm;
}

static void vm_free(VM *vm) {
#if !( defined(_WIN32) || defined(OSKIT) )
  free(vm->freeblocks);
#endif
  free(vm);
}

static size_t vm_memory_allocated(VM *vm) {
  return vm->memory_allocated;
}

static size_t vm_memory_allocated_inc(VM *vm, size_t len) {
  vm->memory_allocated += len;
  return vm->memory_allocated;
}

static size_t vm_memory_allocated_dec(VM *vm, size_t len) {
  vm->memory_allocated -= len;
  return vm->memory_allocated;
}


#if _WIN32            /* Windows */
# include "vm_win.c"
#elif defined(OSKIT)  /* OSKit */
# include "vm_osk.c"
#elif defined(OS_X)   /* OS X */
# include "vm_osx.c"
#else                 /* Default: mmap */
# include "vm_mmap.c"
#endif
