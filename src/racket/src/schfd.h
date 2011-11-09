
#ifdef USE_FAR_MZ_FDCALLS
THREAD_LOCAL_DECL(extern struct mz_fd_set *scheme_fd_set);
# ifdef HAVE_POLL_SYSCALL
struct mz_fd_set {
  struct mz_fd_set_data *data;
  struct mz_fd_set *w;
  struct mz_fd_set *e;
  Scheme_Object *flags;
};
struct mz_fd_set_data {
  struct pollfd *pfd;
  Scheme_Object *size, *count;
};
# else
struct mz_fd_set { fd_set data; };
# endif
#define DECL_FDSET(n, c) fd_set *n
#define INIT_DECL_FDSET(r, w, e) { \
   r = MZ_GET_FDSET(&scheme_fd_set->data, 0 ); \
   w = MZ_GET_FDSET(&scheme_fd_set->data, 1 ); \
   e = MZ_GET_FDSET(&scheme_fd_set->data, 2 ); \
 }
# define INIT_DECL_RD_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->data, 0 )
# define INIT_DECL_WR_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->data, 1 )
# define INIT_DECL_ER_FDSET(r) r = MZ_GET_FDSET(&scheme_fd_set->data, 2 )
#else
# define DECL_FDSET(n, c) fd_set n[c]
# define INIT_DECL_FDSET(r, w, e) /* empty */
# define INIT_DECL_RD_FDSET(r) /* empty */
# define INIT_DECL_WR_FDSET(r) /* empty */
# define INIT_DECL_ER_FDSET(r) /* empty */
#endif

void *scheme_merge_fd_sets(void *fds, void *src_fds);
void scheme_clean_fd_set(void *fds);
int scheme_get_fd_limit(void *fds);
