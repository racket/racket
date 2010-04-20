
#ifdef USE_FAR_MZ_FDCALLS
struct mz_fd_set { fd_set fd; };
THREAD_LOCAL_DECL(extern struct mz_fd_set *scheme_fd_set);
# define DECL_FDSET(n, c) fd_set *n
# define INIT_DECL_FDSET(r, w, e) { \
    r = MZ_GET_FDSET(&scheme_fd_set->fd, 0 ); \
    w = MZ_GET_FDSET(&scheme_fd_set->fd, 1 ); \
    e = MZ_GET_FDSET(&scheme_fd_set->fd, 2 ); \
  }
# define INIT_DECL_RD_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->fd, 0 )
# define INIT_DECL_WR_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->fd, 1 )
# define INIT_DECL_ER_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->fd, 2 )
#else
# define DECL_FDSET(n, c) fd_set n[c]
# define INIT_DECL_FDSET(r, w, e) /* empty */
# define INIT_DECL_RD_FDSET(r) /* empty */
# define INIT_DECL_WR_FDSET(r) /* empty */
# define INIT_DECL_ER_FDSET(r) /* empty */
#endif
