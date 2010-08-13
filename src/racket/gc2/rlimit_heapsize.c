#ifndef DONT_NEED_MAX_HEAP_SIZE

# include <sys/time.h>
# include <sys/resource.h>
# include <unistd.h>

typedef unsigned long size_type;

static size_type determine_max_heap_size(void) 
{
  struct rlimit rlim;

  getrlimit(RLIMIT_AS, &rlim);

  return (rlim.rlim_cur == RLIM_INFINITY) ? (unsigned long)-1 : rlim.rlim_cur;
}

#endif
