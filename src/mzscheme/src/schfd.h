
#ifdef USE_FAR_MZ_FDCALLS
# define DECL_FDSET(n, c) static fd_set *n
# define INIT_DECL_FDSET(n, c) (n = (n ? (fd_set *)scheme_init_fdset_array(n, c) : (fd_set *)scheme_alloc_fdset_array(c, 1)))
#else
# define DECL_FDSET(n, c) fd_set n[c]
# define INIT_DECL_FDSET(n, c) /* empty */
#endif
