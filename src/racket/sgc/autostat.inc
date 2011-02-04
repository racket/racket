
#if defined(_IBMR2)
  extern int end;
# define DATASTART ((void *)0x20000000)
# define DATAEND ((void *)(&end))
# define USE_DATASTARTEND 1
#endif

#if defined(__FreeBSD__) && defined(i386)
  extern char etext;
  extern int end;
# define DATASTART ((void *)(&etext))
# define DATAEND ((void *)(&end))
# define USE_DATASTARTEND 1
#endif

#if defined(linux) && defined(i386) && defined(__ELF__)
# include <linux/version.h>
# include <features.h>
# if LINUX_VERSION_CODE >= 0x20000 && defined(__GLIBC__) && __GLIBC__ >= 2
  extern int __data_start;
#  define DATASTART ((void *)(&__data_start))
# else
   extern int _etext;
#  define DATASTART ((void *)((((unsigned long) (&_etext)) + 0xfff) & ~0xfff))
# endif
  extern int _end;
# define DATAEND (&_end)
# define USE_DATASTARTEND 1
#endif

#if defined(sun)
# include <errno.h>
# ifdef ECHRNG
/* Solaris */
  extern char _etext;
  extern int _end;
#  define DATASTART sysv_GetDataStart(0x10000, (int)&_etext)
#  define DATAEND (void *)(&_end)
#  define NEED_SYSV_GET_START
# else
#  define TEXTSTART 0x2000
#  define DATASTART ((ptr_t)(*(int *)(TEXTSTART+0x4)+TEXTSTART))
# endif
# define USE_DATASTARTEND 1
#endif

#ifndef USE_DATASTARTEND
# define USE_DATASTARTEND 0
#endif

#ifdef WIN32
/* Mostly borrowed from conservative GC, Boehm et al. */
static void cond_add_roots(char *base, char * limit, long allocation_granularity)
{
  char dummy;
  char * stack_top;
  
  if (base == limit) return;
  
  stack_top = (char *) ((long)(&dummy) & ~(allocation_granularity-1));
  
  if (limit > stack_top && base < (char *)GC_stackbottom) {
    /* Part of the stack; ignore it. */
    return;
  }
  GC_add_roots(base, limit);
}
  
void register_static_variables()
{
  MEMORY_BASIC_INFORMATION buf;
  SYSTEM_INFO sysinfo;
  DWORD result;
  DWORD protect;
  LPVOID p;
  char * base;
  char * limit, * new_limit;
  long allocation_granularity;
  
  GetSystemInfo(&sysinfo);
  base = limit = p = sysinfo.lpMinimumApplicationAddress;
  allocation_granularity = sysinfo.dwAllocationGranularity;
  while (p < sysinfo.lpMaximumApplicationAddress) {
    result = VirtualQuery(p, &buf, sizeof(buf));
    new_limit = (char *)p + buf.RegionSize;
    protect = buf.Protect;
    if (buf.State == MEM_COMMIT
	&& (protect == PAGE_EXECUTE_READWRITE
	    || protect == PAGE_READWRITE
	    || protect == PAGE_WRITECOPY
	    || protect == PAGE_EXECUTE_WRITECOPY)
	&& !is_sector_segment(buf.AllocationBase)) {
      if ((char *)p == limit) {
	limit = new_limit;
      } else {
	cond_add_roots(base, limit, allocation_granularity);
	base = p;
	limit = new_limit;
      }
    }
    if (p > (LPVOID)new_limit /* overflow */) break;
    p = (LPVOID)new_limit;
  }
  cond_add_roots(base, limit, allocation_granularity);
}

long total_memory_use()
{
  /* Try to count total used bytes in the heap. */
  MEMORY_BASIC_INFORMATION buf;
  SYSTEM_INFO sysinfo;
  LPVOID p;
  char * new_limit;
  long allocation_granularity;
  long total = 0;
    
  GetSystemInfo(&sysinfo);
  p = sysinfo.lpMinimumApplicationAddress;
  allocation_granularity = sysinfo.dwAllocationGranularity;
  while (p < sysinfo.lpMaximumApplicationAddress) {
    VirtualQuery(p, &buf, sizeof(buf));
    new_limit = (char *)p + buf.RegionSize;
    if (buf.State != MEM_FREE)
      total += buf.RegionSize;
    if (p > (LPVOID)new_limit /* overflow */) break;
    p = (LPVOID)new_limit;
  }
  
  return total;
}
#endif /* Win32 */

#ifdef NEED_SYSV_GET_START
/* Also borrowed conservative GC, Boehm et al. */
#include <signal.h>
# define MIN_PAGE_SIZE 256	/* Smallest conceivable page size, bytes */
static jmp_buf sysv_jb;
    
void sysv_fault_handler(int sig)
{
  longjmp(sysv_jb, 1);
}

typedef void (*handler)(int);
# ifdef sun
static struct sigaction oldact;
# else
static handler old_segv_handler, old_bus_handler;
# endif

static void sysv_setup_temporary_fault_handler()
{
# ifdef sun
  struct sigaction act;

  act.sa_handler = sysv_fault_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO | SA_NODEFER;
  /* The presence of SA_NODEFER represents yet another gross    */
  /* hack.  Under Solaris 2.3, siglongjmp doesn't appear to     */
  /* interact correctly with -lthread.  We hide the confusion   */
  /* by making sure that signal handling doesn't affect the     */
  /* signal mask.                                               */

  (void) sigemptyset(&act.sa_mask);
  (void) sigaction(SIGSEGV, &act, &oldact);
# else
  old_segv_handler = signal(SIGSEGV, sysv_fault_handler);
# ifdef SIGBUS
 old_bus_handler = signal(SIGBUS, sysv_fault_handler);
# endif
# endif
}
    
void sysv_reset_fault_handler()
{
# ifdef sun
  (void) sigaction(SIGSEGV, &oldact, 0);
# else
  (void) signal(SIGSEGV, old_segv_handler);
# ifdef SIGBUS
  (void) signal(SIGBUS, old_bus_handler);
# endif
# endif
}

/* Return the first nonaddressible location > p (up) or 	*/
/* the smallest location q s.t. [q,p] is addressible (!up).	*/
void *sysv_find_limit(void *p, int up)
{
  static void *result;
  static char dummy;
  /* Needs to be static, since otherwise it may not be	*/
  /* preserved across the longjmp.  Can safely be 	*/
  /* static since it's only called once, with the       */
  /* allocation lock held.				*/
  
  sysv_setup_temporary_fault_handler();
  if (setjmp(sysv_jb) == 0) {
    result = (void *)(((unsigned long)(p)) & ~(MIN_PAGE_SIZE-1));
    while(1) {
      if (up)
	result += MIN_PAGE_SIZE;
      else
	result -= MIN_PAGE_SIZE;

      dummy = *(char *)result;
    }
  }
  sysv_reset_fault_handler();
  if (!up)
    result += MIN_PAGE_SIZE;
  return result;
}

void *sysv_GetDataStart(int max_page_size, int etext_addr)
{
  unsigned long text_end = (((unsigned long)(etext_addr) + sizeof(unsigned long) - 1)
			    & ~(sizeof(unsigned long) - 1));
  /* etext rounded to word boundary	*/
  unsigned long next_page = ((text_end + (unsigned long)max_page_size - 1)
			     & ~((unsigned long)max_page_size - 1));
  unsigned long page_offset = (text_end & ((unsigned long)max_page_size - 1));
  char * result = (char *)(next_page + page_offset);
  /* Note that this isn't equivalent to just adding		*/
  /* max_page_size to &etext if &etext is at a page boundary	*/
  
  sysv_setup_temporary_fault_handler();
  if (setjmp(sysv_jb) == 0) {
    /* Try writing to the address.	*/
    *result = *result;
  } else {
    /* We got here via a longjmp.  The address is not readable.	*/
    /* This is known to happen under Solaris 2.4 + gcc, which place	*/
    /* string constants in the text segment, but after etext.	*/
    /* Use plan B.  Note that we now know there is a gap between	*/
    /* text and data segments, so plan A bought us something.	*/
    result = (char *)sysv_find_limit((void *)(DATAEND) - MIN_PAGE_SIZE, 0);
  }
  sysv_reset_fault_handler();
  return (void *)result;
}
#endif /* SysV */
