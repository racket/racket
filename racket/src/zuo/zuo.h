/* This header is used only when embedding Zuo in a larger
   application, and this file defines the embedding interface. 

   To use an embedded Zuo, it must be initialized through the three
   startup steps below. The space between those steps offer two
   interposition opportunities: adding primitives before an image
   (that might rely on the primitives) is loaded, and configuring
   runtime information that is reported by `runtime-env`. */

#ifndef ZUO_EMBEDDED_H
#define ZUO_EMBEDDED_H

#ifndef ZUO_EXPORT
# define ZUO_EXPORT extern
#endif

/* The type `zuo_ext_t*` represents a Zuo value. All values are
   subject to garbage collection or relocation during
   `zuo_eval_module` or a `zuo_ext_apply` of a non-primitive to a
   primitive that evaluates (`kernel-eval` or `module->hash`). Use
   `zuo_ext_stash_push` and `zuo_ext_stash_pop` to save something
   across a potential collection. */
typedef struct zuo_t zuo_ext_t;

/* ======================================================================== */
/*
   Startup step 1: initialize primitives, and maybe add your own.

   Any added primitives will appear in `kernel-env`, as well as being
   propagated as `zuo/kernel`, `zuo`, etc., initial imports.

   To ensure that images will work, primitives must be added in the
   same order, always. Images will only work in an environment with
   the same set of primitives, and using an iage without the
   primitives will effectively remove them by using the image's
   `kernel-env`.
 */
ZUO_EXPORT void zuo_ext_primitive_init();

/* Add more primitives only after calling `zuo_ext_primitive_init`: */
typedef zuo_ext_t *(*zuo_ext_primitive_t)(zuo_ext_t *args_list);
ZUO_EXPORT void zuo_ext_add_primitive(zuo_ext_primitive_t proc, int arity_mask, const char *name);

/* ======================================================================== */
/*
   Startup step 2: load a boot image, or initialize with the default
   or embedded image if `boot_image_file` is NULL.
*/
ZUO_EXPORT void zuo_ext_image_init(char *boot_image_file);

/* After calling `zuo_ext_image_init`, the following functions are available: */

/* Functions that get a constant: */
ZUO_EXPORT zuo_ext_t *zuo_ext_false();
ZUO_EXPORT zuo_ext_t *zuo_ext_true();
ZUO_EXPORT zuo_ext_t *zuo_ext_null();
ZUO_EXPORT zuo_ext_t *zuo_ext_void();
ZUO_EXPORT zuo_ext_t *zuo_ext_eof();
ZUO_EXPORT zuo_ext_t *zuo_ext_empty_hash();

/* Other data constructors and accessors: */
ZUO_EXPORT zuo_ext_t *zuo_ext_integer(long long i);
ZUO_EXPORT long long zuo_ext_integer_value(zuo_ext_t *v);
ZUO_EXPORT zuo_ext_t *zuo_ext_cons(zuo_ext_t *car, zuo_ext_t *cdr);
ZUO_EXPORT zuo_ext_t *zuo_ext_car(zuo_ext_t *obj);
ZUO_EXPORT zuo_ext_t *zuo_ext_cdr(zuo_ext_t *obj);
ZUO_EXPORT zuo_ext_t *zuo_ext_string(const char *str, long long len);
ZUO_EXPORT long long zuo_ext_string_length(zuo_ext_t *str);
ZUO_EXPORT char *zuo_ext_string_ptr(zuo_ext_t *str);
ZUO_EXPORT zuo_ext_t *zuo_ext_symbol(const char *str);
ZUO_EXPORT zuo_ext_t *zuo_ext_hash_ref(zuo_ext_t *ht, zuo_ext_t *key, zuo_ext_t *fail);
ZUO_EXPORT zuo_ext_t *zuo_ext_hash_set(zuo_ext_t *ht, zuo_ext_t *key, zuo_ext_t *val);

/* To get more functions, use a symbol key to look them up in the
   kernel environment via `zuo_ext_hash_ref` --- but don't try to
   load, evaluate, or use any modules, yet: */
ZUO_EXPORT zuo_ext_t *zuo_ext_kernel_env();

/* At this stage, use `zuo_ext_apply` to apply primitives that don't
   evaluate. After `zuo_ext_runtime_init`, use this to apply and
   procedure. Arguments are in a list created with `zuo_ext_cons` and
   `zuo_ext_null`: */
ZUO_EXPORT zuo_ext_t *zuo_ext_apply(zuo_ext_t *proc, zuo_ext_t *args);
/* Note: Prior to version 1.2, this function was broken and evaluated
   the procedure and arguments as expressions instead of using them as
   values when `proc` is not a promitive. The following `#define`
   reflects the repair: */
#define ZUO_EXT_APPLY_ALWAYS_EXPECTS_VALUES 1

/* ======================================================================== */
/*
   Startup step 3: finalize `runtime-env` and the full path for
   finding library modules. The `lib_path` argument can be `#f` to
   disable library loading. The `runtime_env` hash table is used as
   the starting point for a `runtime-env` result; include 'exe, 'args,
   and 'script as appropriate; other keys like 'dir are added
   automatically, while non-standard keys are allowed and preserved.
*/
ZUO_EXPORT void zuo_ext_runtime_init(zuo_ext_t *lib_path, zuo_ext_t *runtime_env);

/* After `zuo_ext_runtime_init`, all functionality is available. You
   can load a module from a file by extracting `module->hash` from the
   kernel env. Or you can declare and run a module directly from
   source text, giving it a module path that is eiter a symbolic
   library path or a file path.

   Note that the result of the kernel's `module->hash` function or the
   `zuo_ext_eval_module` is just a hash table. If the module is
   implemented in `zuo` or a related language, you can use the symbol
   `'dynamic-require` to get the `dynamic-require` function, and then
   you can use that function to access provided values. */

ZUO_EXPORT zuo_ext_t *zuo_ext_eval_module(zuo_ext_t *as_module_path, const char *content, long long len);

/* For saving and retriving a value across an evaluation, which is
   when a GC might happen: */
ZUO_EXPORT void zuo_ext_stash_push(zuo_ext_t *v);
ZUO_EXPORT zuo_ext_t *zuo_ext_stash_pop();

#endif

/* ======================================================================== */
/* Here's a example embedding application where the module is
   implemented with `#lang zuo`, so we need to go through
   `dynamic-require` to get provided values. */
#if 0

#include <stdio.h>
#include <string.h>
#include "zuo.h"

/* Link with a copy of "zuo.c" created by `zuo local/image.zuo` so
   that the `zuo` module is available. */

int main() {
  const char *prog = "#lang zuo (provide main) (define (main) (+ 1 2))";
  zuo_ext_t *ht, *dynamic_require, *main, *v;

  /* Step 1 */
  zuo_ext_primitive_init();

  /* Step 2 */
  zuo_ext_image_init(NULL);

  /* Step 3 */
  zuo_ext_runtime_init(zuo_ext_false(), zuo_ext_empty_hash());

  /* Run `prog`: */
  ht = zuo_ext_eval_module(zuo_ext_symbol("main-app"), prog, strlen(prog));

  dynamic_require = zuo_ext_hash_ref(ht,
                                     zuo_ext_symbol("dynamic-require"),
                                     zuo_ext_false());

  main = zuo_ext_apply(dynamic_require,
                       zuo_ext_cons(zuo_ext_symbol("main-app"),
                                    zuo_ext_cons(zuo_ext_symbol("main"),
                                                 zuo_ext_null())));

  v = zuo_ext_apply(main, zuo_ext_null());

  printf("%s\n",
         zuo_ext_string_ptr(zuo_ext_apply(zuo_ext_hash_ref(zuo_ext_kernel_env(),
                                                           zuo_ext_symbol("~s"),
                                                           zuo_ext_false()),
                                          zuo_ext_cons(v, zuo_ext_null()))));

  return 0;
}

#endif

/* ======================================================================== */
/* Here's an example embedding application that makes an extra
   primitive `random-five` available. Beware that this example will not
   work when using an embedded image, however, unless the image is one
   generated by enabling the dump after step 3. */
#if 0

#include <stdio.h>
#include <string.h>
#include "zuo.h"

static zuo_ext_t *random_five(zuo_ext_t *args) {
  return zuo_ext_integer(5);
}

int main() {
  const char *prog = "#lang zuo/kernel (hash 'number (random-five))";
  zuo_ext_t *ht, *v;

  /* Step 1 */
  zuo_ext_primitive_init();
  zuo_ext_add_primitive(random_five, 1, "random-five");

  /* Step 2 */
  zuo_ext_image_init(NULL);

  /* Step 3 */
  zuo_ext_runtime_init(zuo_ext_false(), zuo_ext_empty_hash());

  if (0) {
    const char *dump = "#lang zuo/kernel (dump-image-and-exit (fd-open-output \"image\"))";
    (void)zuo_ext_eval_module(zuo_ext_symbol("dump"), dump, strlen(dump));
    /* Afterward, use
          zuo local/image.zuo --image image
       to generate a ".c" file to link with this example. */
  }

  /* Run `prog`: */
  ht = zuo_ext_eval_module(zuo_ext_symbol("five-app"), prog, strlen(prog));

  /* Inspect the result: */
  v = zuo_ext_hash_ref(ht, zuo_ext_symbol("number"), zuo_ext_false());
  if (zuo_ext_apply(zuo_ext_hash_ref(zuo_ext_kernel_env(),
                                     zuo_ext_symbol("integer?"),
                                     zuo_ext_false()),
                    zuo_ext_cons(v, zuo_ext_null()))
      == zuo_ext_true())
    printf("The answer was %d\n", (int)zuo_ext_integer_value(v));
  else
    printf("Something went wrong!\n");

  return 0;
}

#endif
