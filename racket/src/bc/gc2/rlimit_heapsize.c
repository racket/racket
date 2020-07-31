#ifndef DONT_NEED_MAX_HEAP_SIZE

# include <sys/time.h>
# include <sys/resource.h>
# include <unistd.h>

typedef uintptr_t size_type;

static size_type determine_max_heap_size(void) 
{
  struct rlimit rlim;

#if defined(RLIMIT_AS)
  getrlimit(RLIMIT_AS, &rlim);
#else
  getrlimit(RLIMIT_DATA, &rlim);
#endif

  return (rlim.rlim_cur == RLIM_INFINITY) ? (uintptr_t)-1 : rlim.rlim_cur;
}

#endif
