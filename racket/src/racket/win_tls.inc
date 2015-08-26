#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
extern intptr_t _tls_index;
# ifdef __MINGW32__
static __thread void *tls_space;
# else
static __declspec(thread) void *tls_space;
# endif
#endif

# ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
static void register_win_tls()
{
#  ifdef __MINGW32__
  {
    /* gcc declares space for the thread-local variable in a way that
       the OS can set up, but its doesn't actually map variables
       through the OS-supplied mechanism. Just assume that the first
       thread-local variable is ours. */
    void **base;
#  ifdef _WIN64
    asm("mov %%gs:(0x58), %0;" :"=r"(base));
#  else
    asm("mov %%fs:(0x2C), %0;" :"=r"(base));
#  endif
    scheme_register_tls_space(*base, _tls_index);
  }
#  else
  scheme_register_tls_space(&tls_space, _tls_index);
#  endif
}
# else
static void register_win_tls() {}
# endif
