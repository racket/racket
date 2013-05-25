/* 

   This extension Defines a new type of Scheme data: a two-dimensional
   matrix of bits.
   
   A client using this extension would look something like this:

      (load-extension "bitmatrix.so")
      (define bm (make-bit-matrix 1000 1000))
      ...
      (bit-matrix-set! bm 500 500 #t)
      ...
      (if (bit-matrix-get bm 500 500) ...)
      ...

*/

#include "escheme.h"

/* Instances of this Bitmatrix structure will be the Scheme bit matirx
   values: */
typedef struct {
  Scheme_Object so; /* Every Scheme value starts with a Scheme_Object,
                       which stars with a type tag.  The
		       format for the rest of the structure is
		       anything we want it to be. */
  unsigned long w, h, l; /* l = w rounded to multiple of LONG_SIZE */
  unsigned long *matrix;
} Bitmatrix;

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
/* Traversal procedures for precise GC: */
static int bm_size(void *p) { 
  return gcBYTES_TO_WORDS(sizeof(Bitmatrix)); 
}
static int bm_mark(void *p) { 
  gcMARK(((Bitmatrix *)p)->matrix);
  return gcBYTES_TO_WORDS(sizeof(Bitmatrix));
}
static int bm_fixup(void *p) { 
  gcFIXUP(((Bitmatrix *)p)->matrix);
  return gcBYTES_TO_WORDS(sizeof(Bitmatrix));
}
END_XFORM_SKIP;
#endif

/* We'll get some Scheme primitives so we can calculate with numbers
   that are potentially bignums: */
static Scheme_Object *mult, *add, *sub, *modulo, *neg;

/* The type tag for bit matrixes, initialized with scheme_make_type */
static Scheme_Type bitmatrix_type;

#define LONG_SIZE 32
#define LOG_LONG_SIZE 5
#define LONG_SIZE_PER_BYTE 4

# define FIND_BIT(p) (1 << (p & (LONG_SIZE - 1)))

/* Helper function to check whether an integer (fixnum or bignum) is
   negative: */
static int negative(Scheme_Object *o)
{
  return SCHEME_TRUEP(_scheme_apply(neg, 1, &o));
}

/* Scheme procedure to make a bit matrix: */
Scheme_Object *make_bit_matrix(int argc, Scheme_Object **argv)
{
  Scheme_Object *size, *rowlength, *a[2];
  unsigned long w, h, s, l, *lp;
  Bitmatrix *bm;

  /* Really fancy: we allow any kind of positive integer for
     specifying the size of a bit matrix. If we get a bignum (or the
     resulting matrix size is a bignum), we'll signal an out-of-memory
     exception. */
  if ((!SCHEME_INTP(argv[0])  && !SCHEME_BIGNUMP(argv[0]))
      || negative(argv[0]))
    scheme_wrong_type("make-bit-matrix", "positive integer", 0, argc, argv);
  if ((!SCHEME_INTP(argv[1])  && !SCHEME_BIGNUMP(argv[1]))
      || (negative(argv[1])))
    scheme_wrong_type("make-bit-matrix", "positive integer", 1, argc, argv);

  a[0] = argv[0];
  a[1] = scheme_make_integer(LONG_SIZE - 1);
  /* Apply the Scheme `add' procedure to argv[0] and argv[1]. Note the
     "_" in "_scheme_apply"; that's a lot faster than "scheme_apply",
     and we know that no continuation jumps will occur (although it
     would be fine if one did. */
  a[0] = _scheme_apply(add, 2, a);
  a[1] = scheme_make_integer(LONG_SIZE);
  a[1] = _scheme_apply(modulo, 2, a);
  a[0] = _scheme_apply(sub, 2, a);
  rowlength = a[0];
  a[1] = argv[1];
  size = _scheme_apply(mult, 2, a);
  if (SCHEME_BIGNUMP(size))
    /* Use scheme_raise_exn to raise exceptions. The first argument
       describes the type of the exception. After an exception-specific
       number of Scheme values (none in this case), the rest of the
       arguments are like printf. */
    scheme_raise_exn(MZEXN_FAIL, "make-bit-matrix: out of memory");
  
  s = SCHEME_INT_VAL(size);
  w = SCHEME_INT_VAL(argv[0]);
  h = SCHEME_INT_VAL(argv[1]);
  l = SCHEME_INT_VAL(rowlength);

  /* Malloc the bit matrix structure. Since we use scheme_malloc, the
     bit matrix value is GC-able. */
  bm = (Bitmatrix *)scheme_malloc_tagged(sizeof(Bitmatrix));
  bm->so.type = bitmatrix_type;

  /* Try to allocate the bit matrix. Handle failure gracefully. Note
     that we use scheme_malloc_atomic since the allocated memory will
     never contain pointers to GC-allocated memory. */
  s = ((s + LONG_SIZE - 1) >> LOG_LONG_SIZE);
  lp = (unsigned long *)scheme_malloc_fail_ok(scheme_malloc_atomic, 
					      sizeof(long) * s);
  if (!lp)
    scheme_raise_exn(MZEXN_FAIL, "make-bit-matrix: out of memory");
  bm->matrix = lp;

  bm->w = w;
  bm->h = h;
  bm->l = l;

  /* Init matirx to all 0s: */
  while (s--) {
    bm->matrix[s] = 0;
  }

  return (Scheme_Object *)bm;
}

/* Internal utility function for error-checking with a fancy error
   message: */
static void range_check_one(char *name, char *which, 
			    int l, int h, int startpos, 
			    int argc, Scheme_Object **argv)
{
  int bad1;

  if (SCHEME_BIGNUMP(argv[startpos])) {
    bad1 = 1;
  } else {
    int v = SCHEME_INT_VAL(argv[startpos]);
    bad1 = ((v < l) || (v > h));
  }

  if (bad1) {
    /* A mismatch exception requires one Scheme value, so we provide
       it before the printf string: */
    char *args;
    long argslen;

    args = scheme_make_args_string("other ", startpos, argc, argv, &argslen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: %s index %s is not in the range [%d,%d]%t",
		     name, which,
		     scheme_make_provided_string(argv[startpos], 1, NULL),
		     l, h,
		     args,
		     argslen);
  }
}

/* Internal utility function that implements most of the work of the
   get- and set- Scheme procedures: */
static Scheme_Object *do_bit_matrix(char *name, int get, int argc, Scheme_Object **argv)
{
  Bitmatrix *bm;
  unsigned long x, y, p, v, m;

  if (SCHEME_TYPE(argv[0]) != bitmatrix_type)
    scheme_wrong_type(name, "bit-matrix", 0, argc, argv);
  if (!SCHEME_INTP(argv[1])  && !SCHEME_BIGNUMP(argv[1]))
    scheme_wrong_type(name, "integer", 1, argc, argv);
  if (!SCHEME_INTP(argv[2])  && !SCHEME_BIGNUMP(argv[2]))
    scheme_wrong_type(name, "integer", 2, argc, argv);

  /* After checking that argv[0] has te bitmatrix_type tag, we can safely perform
     a cast to Bitmatrix*: */
  bm = (Bitmatrix *)argv[0];

  range_check_one(name, "first", 0, bm->w - 1, 1, argc, argv);
  range_check_one(name, "second", 0, bm->h - 1, 2, argc, argv);

  x = SCHEME_INT_VAL(argv[1]);
  y = SCHEME_INT_VAL(argv[2]);

  p = y * bm->l + x;
  m = FIND_BIT(p);
  v = bm->matrix[p >> LOG_LONG_SIZE];
  if (get) {
    return (v & m) ? scheme_true : scheme_false;
  } else {
    if (SCHEME_TRUEP(argv[3]))
      bm->matrix[p >> LOG_LONG_SIZE] = (v | m);
    else
      bm->matrix[p >> LOG_LONG_SIZE] = (v - (v & m));
    return scheme_void;
  }
}

/* Scheme procedure: get a bit from the matrix */
Scheme_Object *bit_matrix_get(int argc, Scheme_Object **argv)
{
  return do_bit_matrix("bit-matrix-get", 1, argc, argv);
}

/* Scheme procedure: set a bit in the matrix */
Scheme_Object *bit_matrix_set(int argc, Scheme_Object **argv)
{
  return do_bit_matrix("bit-matrix-set!", 0, argc, argv);
}

/* Scheme procedure: invert the whole matrix */
Scheme_Object *bit_matrix_invert(int argc, Scheme_Object **argv)
{
  Bitmatrix *bm;
  unsigned long i;

  if (SCHEME_TYPE(argv[0]) != bitmatrix_type)
    scheme_wrong_type("bit-matrix-invert!", "bit-matrix", 0, argc, argv);

  bm = (Bitmatrix *)argv[0];
  
  i = (bm->l * bm->h) >> LOG_LONG_SIZE;
  while (i--) {
    bm->matrix[i] = ~bm->matrix[i];
  }

  return scheme_void;
}

/* Scheme procedure: clear the whole matrix */
Scheme_Object *bit_matrix_clear(int argc, Scheme_Object **argv)
{
  char *name = "bit-matrix-clear!";
  Bitmatrix *bm;
  unsigned long i;

  if (SCHEME_TYPE(argv[0]) != bitmatrix_type)
    scheme_wrong_type(name, "bit-matrix", 0, argc, argv);

  bm = (Bitmatrix *)argv[0];

  i = (bm->l * bm->h) >> LOG_LONG_SIZE;
  while (i--) {
    bm->matrix[i] = 0;
  }

  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  /* Define our new primitives: */

  scheme_add_global("make-bit-matrix",
		    scheme_make_prim_w_arity(make_bit_matrix,
					     "make-bit-matrix",
					     2, 2),
		    env);

  scheme_add_global("bit-matrix-get",
		    scheme_make_prim_w_arity(bit_matrix_get,
					     "bit-matrix-get",
					     3, 3),
		    env);

  scheme_add_global("bit-matrix-set!",
		    scheme_make_prim_w_arity(bit_matrix_set,
					     "bit-matrix-set!",
					     4, 4),
		    env);

  scheme_add_global("bit-matrix-invert!",
		    scheme_make_prim_w_arity(bit_matrix_invert,
					     "bit-matrix-invert!",
					     1, 1),
		    env);

  scheme_add_global("bit-matrix-clear!",
		    scheme_make_prim_w_arity(bit_matrix_clear,
					     "bit-matrix-clear!",
					     1, 1),
		    env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  bitmatrix_type = scheme_make_type("<bit-matrix>");

#ifdef MZ_PRECISE_GC
  /* Register traversal procedures: */
  GC_register_traversers(bitmatrix_type, bm_size, bm_mark, bm_fixup, 1, 0);
#endif

  /* Get some Scheme primitives. Conservative garbage collection sees
     any local variables we use within a function, but we have to register
     static variables: */

  scheme_register_extension_global(&mult, sizeof(Scheme_Object*));
  mult = scheme_builtin_value("*");

  scheme_register_extension_global(&add, sizeof(Scheme_Object*));
  add = scheme_builtin_value("+");

  scheme_register_extension_global(&sub, sizeof(Scheme_Object*));
  sub = scheme_builtin_value("-");

  scheme_register_extension_global(&modulo, sizeof(Scheme_Object*));
  modulo = scheme_builtin_value("modulo");

  scheme_register_extension_global(&neg, sizeof(Scheme_Object*));
  neg = scheme_builtin_value("negative?");

  return scheme_reload(env);
}


Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
