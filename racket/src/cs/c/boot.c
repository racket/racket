#ifndef WIN32
# include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "scheme.h"
#include "rktio.h"

#ifdef WIN32
# define RACKET_API_EXTERN __declspec(dllexport)
#else
# define RACKET_API_EXTERN extern
#endif
#define BOOT_EXTERN RACKET_API_EXTERN
#include "boot.h"
#include "api.h"

#define RACKET_AS_BOOT

#if defined(_MSC_VER) || defined(__MINGW32__)
# define BOOT_O_BINARY O_BINARY
#endif

#ifndef BOOT_O_BINARY
# define BOOT_O_BINARY 0
#endif

static ptr Sbytevector(char *s)
{
  iptr len = strlen(s);
  ptr bv;
  bv = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bv), s, len);
  return bv;
}

static ptr parse_coldirs(char *s)
{
  iptr len = strlen(s);

  if (!len || !s[len+1]) {
    /* empty string or only one string */
    return Sbytevector(s);
  }

  /* multiple collects paths; put into a reversed list */
  {
    ptr rev = Snil;
    iptr delta = 0;

    while (s[delta]) {
      len = strlen(s + delta);
      rev = Scons(Sbytevector(s+delta), rev);
      delta += len + 1;
    }

    return rev;
  }
}

static void run_cross_server(char **argv)
{
  ptr c, a;
  const char *target_machine = argv[1];
  const char *cross_server_patch_file = argv[2];
  const char *cross_server_library_file = argv[3];

  c = Stop_level_value(Sstring_to_symbol("load")); /* original `load` */
  a = Sstring(cross_server_patch_file);
  (void)Scall1(c, a);

  c = Stop_level_value(Sstring_to_symbol("load")); /* this is the patched `load` */
  a = Sstring(cross_server_library_file);
  (void)Scall1(c, a);
  c = Stop_level_value(Sstring_to_symbol("serve-cross-compile"));

  a = Sstring(target_machine);
  (void)Scall1(c, a);
}

static void racket_exit(int v)
{
  exit(v);
}

static int racket_errno()
{
  return errno;
}

static void init_foreign()
{
# include "rktio.inc"
  Sforeign_symbol("racket_exit", (void *)racket_exit);
  Sforeign_symbol("racket_errno", (void *)racket_errno);
}

void racket_boot(racket_boot_arguments_t *ba)
{
  int cross_server = 0;

#ifdef WIN32
  if (ba->dll_dir)
    rktio_set_dll_path((wchar_t *)ba->dll_dir);
  if (ba->dll_open)
    rktio_set_dll_procs(ba->dll_open, ba->dll_find_object, ba->dll_close);
#endif

  Sscheme_init(NULL);

  if ((ba->argc == 4) && !strcmp(ba->argv[0], "--cross-server"))
    cross_server = 1;

  /* Open boot files, but reuse file descriptors when possible */
  {
    int fd1, fd2, close_fd1 = 0, close_fd2 = 0;

    if ((ba->boot2_offset == 0)
        || ((ba->boot1_path != ba->boot2_path)
            && strcmp(ba->boot1_path, ba->boot2_path)))
      close_fd1 = 1;
# ifdef RACKET_AS_BOOT
    if ((ba->boot3_offset == 0)
        || ((ba->boot2_path != ba->boot3_path)
            && strcmp(ba->boot2_path, ba->boot3_path)))
      close_fd2 = 1;
#else
    close_fd2 = 1;
#endif

    fd1 = open(ba->boot1_path, O_RDONLY | BOOT_O_BINARY);
    Sregister_boot_file_fd_region("petite", fd1, ba->boot1_offset, ba->boot1_len, close_fd1);

    if (!close_fd1)
      fd2 = fd1;
    else
      fd2 = open(ba->boot2_path, O_RDONLY | BOOT_O_BINARY);
    Sregister_boot_file_fd_region("scheme", fd2, ba->boot2_offset, ba->boot2_len, close_fd2);

# ifdef RACKET_AS_BOOT
    if (!cross_server) {
      int fd3;

      if (!close_fd2)
        fd3 = fd2;
      else
        fd3 = open(ba->boot3_path, O_RDONLY | BOOT_O_BINARY);
      Sregister_boot_file_fd_region("racket", fd3, ba->boot3_offset, ba->boot3_len, 1);
    }
# endif
  }

  Sbuild_heap(NULL, init_foreign);

  if (cross_server) {
    /* Don't run Racket as usual. Instead, load the patch
       file and run `serve-cross-compile` */
    run_cross_server(ba->argv);
    racket_exit(0);
  }

  {
    ptr l = Snil;
    int i;
    char segment_offset_s[32], wm_is_gracket_s[32];

    if (ba->argv) {
      for (i = ba->argc; i--; ) {
        l = Scons(Sbytevector(ba->argv[i]), l);
      }
    } else {
      l = Scons(Sbytevector("-n"), l);
    }
    l = Scons(Sbytevector(ba->gracket_guid_or_x11_args ? ba->gracket_guid_or_x11_args : ""), l);
    sprintf(wm_is_gracket_s, "%d", ba->wm_is_gracket_or_x11_arg_count);
    l = Scons(Sbytevector(wm_is_gracket_s), l);
    l = Scons(Sbytevector(ba->is_gui ? "true" : "false"), l);
    l = Scons(Sbytevector(ba->cs_compiled_subdir ? "true" : "false"), l);
    sprintf(segment_offset_s, "%ld", ba->segment_offset);
    l = Scons(Sbytevector(segment_offset_s), l);
    l = Scons(Sbytevector(ba->config_dir ? (char *)ba->config_dir : "etc"), l);
    l = Scons(parse_coldirs(ba->collects_dir ? (char *)ba->collects_dir : ""), l);
    l = Scons(Sbytevector(ba->run_file ? (char *)ba->run_file : (char *)ba->exec_file ), l);
    l = Scons(Sbytevector((char *)ba->exec_file), l);
    l = Scons(Sbytevector(ba->exit_after ? "false" : "true"), l);

#ifdef RACKET_AS_BOOT
    {
      ptr c, start, apply;
      c = Stop_level_value(Sstring_to_symbol("scheme-start"));
      start = Scall0(c);
      apply = Stop_level_value(Sstring_to_symbol("apply"));
      Scall2(apply, start, l);
    }
#else
    Sset_top_level_value(Sstring_to_symbol("bytes-command-line-arguments"), l);
#endif
  }

#ifndef RACKET_AS_BOOT
  {
    ptr c, p;
    int f3;

    fd3 = open(ba->boot3_path, O_RDONLY | BOOT_O_BINARY);
    if (boot3_offset) lseek(fd3, ba->boot3_offset, SEEK_SET);
    c = Stop_level_value(Sstring_to_symbol("open-fd-input-port"));
    p = Scall1(c, Sfixnum(fd3));
    Slock_object(p);
    c = Stop_level_value(Sstring_to_symbol("port-file-compressed!"));
    Scall1(c, p);
    Sunlock_object(p);
    c = Stop_level_value(Sstring_to_symbol("load-compiled-from-port"));
    Scall1(c, p);
  }
#endif
}

/* **************************************** */

enum {
  EMBEDDED_ENTRY_APPLY,
  EMBEDDED_ENTRY_PRIMITIVE_LOOKUP,
  EMBEDDED_ENTRY_EVAL,
  EMBEDDED_ENTRY_DYNAMIC_REQUIRE,
  EMBEDDED_ENTRY_NAMESPACE_REQUIRE,
  EMBEDDED_ENTRY_EMBEDDED_LOAD
};

static ptr get_embedded_entry(int index)
{
  ptr vec;
  
  vec = Stop_level_value(Sstring_to_symbol("embedded-racket-entry-info"));
  return Svector_ref(vec, index);
}

ptr racket_apply(ptr proc, ptr arg_list)
{
  ptr app = get_embedded_entry(EMBEDDED_ENTRY_APPLY);

  return Scall2(app, proc, arg_list);
}

ptr racket_primitive(const char *name)
{
  ptr prim_lookup = get_embedded_entry(EMBEDDED_ENTRY_PRIMITIVE_LOOKUP);

  return Scall1(prim_lookup, Sstring_to_symbol(name));
}

ptr racket_eval(ptr s_expr)
{
  ptr eval = get_embedded_entry(EMBEDDED_ENTRY_EVAL);

  return racket_apply(eval, Scons(s_expr, Snil));
}

ptr racket_dynamic_require(ptr module_path, ptr sym_or_false)
{
  ptr dy_req = get_embedded_entry(EMBEDDED_ENTRY_DYNAMIC_REQUIRE);

  return racket_apply(dy_req, Scons(module_path, Scons(sym_or_false, Snil)));
}

void racket_namespace_require(ptr module_path)
{
  ptr ns_req = get_embedded_entry(EMBEDDED_ENTRY_NAMESPACE_REQUIRE);

  (void)racket_apply(ns_req, Scons(module_path, Snil));
}

static void embedded_load(ptr path, ptr start, ptr end, ptr bstr, int as_predefined)
{
  ptr load = get_embedded_entry(EMBEDDED_ENTRY_EMBEDDED_LOAD);
  ptr pre = (as_predefined ? Strue : Sfalse);

  (void)racket_apply(load, Scons(path, Scons(start, Scons(end, Scons(bstr, Scons(pre, Snil))))));
}

void racket_embedded_load_bytes(const char *code, uptr len, int as_predefined)
{
  ptr bstr = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bstr), code, len);

  embedded_load(Sfalse, Sfalse, Sfalse, bstr, as_predefined);
}

void racket_embedded_load_file(const char *path, int as_predefined)
{
  embedded_load(Sbytevector((char *)path), Sfixnum(0), Sfalse, Sfalse, as_predefined);
}

void racket_embedded_load_file_region(const char *path, uptr start, uptr end, int as_predefined)
{
  embedded_load(Sbytevector((char *)path), Sfixnum(start), Sfixnum(end), Sfalse, as_predefined);
}

void *racket_cpointer_address(ptr cptr) {
  void *p;
  iptr offset;
  p = racket_cpointer_base_address(cptr);
  offset = racket_cpointer_offset(cptr);
  return (char *)p + offset;
}

void *racket_cpointer_base_address(ptr cptr) {
  if (Srecordp(cptr)) {
    cptr = Srecord_uniform_ref(cptr, 0);

    if (Sbytevectorp(cptr))
      return &Sbytevector_u8_ref(cptr, 0);
    else if (Svectorp(cptr))
      return &Svector_ref(cptr, 0);
    else if (Sfixnump(cptr) || Sbignump(cptr))
      return TO_VOIDP(Sinteger_value(cptr));
  }

  return NULL;
}

iptr racket_cpointer_offset(ptr cptr) {
  if (Srecordp(cptr)) {
    if (Srecord_type_parent(Srecord_type(cptr)) != Sfalse) {
      /* assume that it's a cpointer+offset */
      return Sinteger_value(Srecord_uniform_ref(cptr, 2));
    }
  }

  return 0;
}
