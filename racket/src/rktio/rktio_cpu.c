#include "rktio.h"
#include "rktio_private.h"

#if defined(__linux__) || defined(__QNX__)
# include <unistd.h>
#elif defined(OS_X) || defined(__FreeBSD__) || defined(__OpenBSD__)
# include <sys/param.h>
# include <sys/sysctl.h>
#elif defined(RKTIO_SYSTEM_WINDOWS)
# include <windows.h>
#endif

void rktio_init_cpu(rktio_t *rktio)
{
  int processor_count;

#if defined(__linux__) || defined(__QNX__)
  processor_count = sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(OS_X) || defined(__FreeBSD__)
  size_t size = sizeof(processor_count);

  if (sysctlbyname("hw.ncpu", &processor_count, &size, NULL, 0))
    processor_count = 2;
#elif defined(__OpenBSD__)
  size_t size = sizeof(processor_count);
  int mib[2];
  mib[0] = CTL_HW;
  mib[1] = HW_NCPU;
  if (sysctl(mib, 2, &processor_count, &size, NULL, 0) == -1)
    processor_count = 2;
#elif defined(RKTIO_SYSTEM_WINDOWS)
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  processor_count = sysinfo.dwNumberOfProcessors;
#else
  /* Conservative guess! */
  /* A result of 1 is not conservative, because that's claiming a
     uniprocessor. */
  processor_count = 2;
#endif

  rktio->processor_count = processor_count;
}

int rktio_processor_count(rktio_t *rktio)
{
  return rktio->processor_count;
}
