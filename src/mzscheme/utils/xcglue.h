
#ifndef OBJ_SCHEME_H
#define OBJ_SCHEME_H

#include "scheme.h"

#ifdef __cplusplus
extern "C" 
{
#endif

/* Provided by the embedding client: */
void scheme_install_xc_global(char *name, Scheme_Object *val, Scheme_Env *env);
Scheme_Object *scheme_lookup_xc_global(char *name, Scheme_Env *env);

/* initialization: */
void objscheme_init(Scheme_Env *);

/******************************************************************/
/*                   Utilites used by xctocc                      */
/******************************************************************/

  /* >>>>>>>>>>>> WARNING <<<<<<<<<<<<<<
     The following struct declaration is crafted to
     overlay over a Scheme_Structure. */
typedef struct Scheme_Class_Object {
  Scheme_Object so; /* scheme_structure_type */
  void *__type; /* struct type */
  long primflag; /* field 0 */
  void *primdata; /* field 1 */
  /* ... */
} Scheme_Class_Object;
  
typedef Scheme_Prim Scheme_Method_Prim;

#define POFFSET 1
#define THEOBJ p[0]

typedef long ExactLong;

void objscheme_init(Scheme_Env *env);

/* Defining a primitive class: */
Scheme_Object *
objscheme_def_prim_class(void *env, char *name, char *superclass,
			 Scheme_Method_Prim *initf, int nmethods);
void objscheme_add_global_class(Scheme_Object *sclass, char *name,
				void *env);
void objscheme_add_global_interface(Scheme_Object *sclass, char *name,
				    void *env);

void scheme_add_method_w_arity(Scheme_Object *c, const char *name,
			       Scheme_Method_Prim *f, 
			       int mina, int maxa);
void scheme_add_method(Scheme_Object *c, const char *name,
		       Scheme_Method_Prim *f);
void scheme_made_class(Scheme_Object *c);
Scheme_Object* scheme_class_to_interface(Scheme_Object *c, char *name);

Scheme_Object *scheme_make_uninited_object(Scheme_Object *sclass);

/* Maintaining the Scheme - C++ connection */
void objscheme_save_object(void *, Scheme_Object *);
Scheme_Class_Object *objscheme_find_object(void *);
void objscheme_check_valid(Scheme_Object *sclass, const char *name, int n, Scheme_Object **argv);
int objscheme_is_shutdown(Scheme_Object *o);

void objscheme_register_primpointer(void *obj_addr, void *prim_ptr_address);

void objscheme_destroy(void *, Scheme_Object *obj);

/* Finding a method: */
Scheme_Object *objscheme_find_method(Scheme_Object *obj, 
				     Scheme_Object *sclass,
				     char *name, 
				     void **cache);

int objscheme_is_subclass(Scheme_Object *a, Scheme_Object *sup);
int objscheme_is_a(Scheme_Object *o, Scheme_Object *c);

Scheme_Object *objscheme_unbox(Scheme_Object *, const char *where);
Scheme_Object *objscheme_nullable_unbox(Scheme_Object *, const char *where);
Scheme_Object *objscheme_box(Scheme_Object *);
void objscheme_set_box(Scheme_Object *, Scheme_Object *);

int objscheme_istype_bool(Scheme_Object *, const char *stopifbad);
int objscheme_istype_integer(Scheme_Object *, const char *stopifbad);
int objscheme_istype_number(Scheme_Object *, const char *stopifbad);
int objscheme_istype_ExactLong(Scheme_Object *, const char *stopifbad);
int objscheme_istype_double(Scheme_Object *, const char *stopifbad);
int objscheme_istype_pair(Scheme_Object *, const char *stopifbad);
int objscheme_istype_string(Scheme_Object *, const char *stopifbad);
int objscheme_istype_bstring(Scheme_Object *, const char *stopifbad);
int objscheme_istype_pstring(Scheme_Object *, const char *stopifbad);
#define objscheme_istype_mzstring objscheme_istype_string
#define objscheme_istype_mzxstring objscheme_istype_string
int objscheme_istype_pathname(Scheme_Object *, const char *stopifbad);
int objscheme_istype_epathname(Scheme_Object *, const char *stopifbad);
#define objscheme_istype_xpathname objscheme_istype_pathname
int objscheme_istype_char(Scheme_Object *, const char *stopifbad);
int objscheme_istype_closed_prim(Scheme_Object *, const char *stopifbad);
int objscheme_istype_proc2(Scheme_Object *, const char *stopifbad);
int objscheme_istype_box(Scheme_Object *, const char *stopifbad);
int objscheme_istype_nonnegative_symbol_integer(Scheme_Object *, const char *symname, const char *stopifbad);
int objscheme_istype_nonnegative_symbol_double(Scheme_Object *, const char *symname, const char *stopifbad);

Scheme_Object *objscheme_car(Scheme_Object *, const char *where);
Scheme_Object *objscheme_bundle_string(char *);
Scheme_Object *objscheme_bundle_bstring(char *);
#define objscheme_bundle_pstring objscheme_bundle_bstring
Scheme_Object *objscheme_bundle_mzstring(mzchar *);
Scheme_Object *objscheme_bundle_pathname(char *);
#define objscheme_bundle_epathname objscheme_bundle_pathname
#define objscheme_bundle_xpathname objscheme_bundle_pathname
Scheme_Object *objscheme_bundle_nonnegative_symbol_double(double d, const char *symname);

long objscheme_unbundle_integer(Scheme_Object *, const char *);
long objscheme_unbundle_integer_in(Scheme_Object *, long, long, const char *);
long objscheme_unbundle_nonnegative_integer(Scheme_Object *, const char *);
long objscheme_unbundle_nonnegative_symbol_integer(Scheme_Object *, const char *symname, const char *);
ExactLong objscheme_unbundle_ExactLong(Scheme_Object *, const char *);
double objscheme_unbundle_double(Scheme_Object *, const char *);
double objscheme_unbundle_double_in(Scheme_Object *, double, double, const char *);
double objscheme_unbundle_nonnegative_double(Scheme_Object *, const char *);
double objscheme_unbundle_nonnegative_symbol_double(Scheme_Object *, const char *symname, const char *);
int objscheme_unbundle_bool(Scheme_Object *, const char *);
char *objscheme_unbundle_string(Scheme_Object *, const char *);
char *objscheme_unbundle_bstring(Scheme_Object *, const char *);
char *objscheme_unbundle_pstring(Scheme_Object *, const char *);
mzchar *objscheme_unbundle_mzstring(Scheme_Object *, const char *);
#define objscheme_unbundle_mzxstring(a, b) (char *)objscheme_unbundle_mzstring(a, b)
mzchar *objscheme_unbundle_mzstring(Scheme_Object *, const char *);
char *objscheme_unbundle_mutable_bstring(Scheme_Object *, const char *);
mzchar *objscheme_unbundle_mutable_mzstring(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_string(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_bstring(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_pstring(Scheme_Object *, const char *);
mzchar *objscheme_unbundle_nullable_mzstring(Scheme_Object *, const char *);
char *objscheme_unbundle_pathname(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_pathname(Scheme_Object *, const char *);
char *objscheme_unbundle_write_pathname(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_write_pathname(Scheme_Object *, const char *);
char *objscheme_unbundle_epathname(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_epathname(Scheme_Object *, const char *);
char *objscheme_unbundle_xpathname(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_xpathname(Scheme_Object *, const char *);
mzchar objscheme_unbundle_char(Scheme_Object *, const char *);

#define objscheme_bundle_integer scheme_make_integer
#define objscheme_bundle_long objscheme_bundle_integer
#define objscheme_bundle_int objscheme_bundle_integer
#define objscheme_bundle_ExactLong scheme_make_integer_value
#define objscheme_bundle_double scheme_make_double
#define objscheme_bundle_bool(x) ((x) ? scheme_true : scheme_false)
#define objscheme_bundle_char scheme_make_char
#define objscheme_bundle_pair scheme_make_pair

#define objscheme_unbundle_long objscheme_unbundle_integer
#define objscheme_unbundle_int objscheme_unbundle_integer

#define OBJSCHEME_PRIM_METHOD(m, cf) (SCHEME_PRIMP(m) && ((Scheme_Prim *)((Scheme_Primitive_Proc *)m)->prim_val == cf))

#define COPY_JMPBUF(dest, src) memcpy(&dest, &src, sizeof(mz_jmp_buf));

typedef Scheme_Object *(*Objscheme_Bundler)(void *);
void objscheme_install_bundler(Objscheme_Bundler f, long id);
Scheme_Object *objscheme_bundle_by_type(void *realobj, long type);

#define METHODNAME(x, y) y" in "x

#ifndef _MSC_VER
typedef char byte;
#endif
typedef unsigned char ubyte;

typedef char *xc_string;
typedef char *xc_bstring;
typedef char *xc_pstring;
typedef mzchar *xc_mzstring;
typedef char *xc_mzxstring;
#define string xc_string
#define bstring xc_bstring
#define pstring xc_pstring
#define mzstring xc_mzstring
#define mzxstring xc_mzxstring
typedef const char *cstring, *ncstring; 
typedef const char *cpstring, *ncpstring; 
typedef const char *cbstring, *ncbstring; 
typedef const mzchar *cmzstring, *ncmzstring; 
typedef char *nstring;
typedef char *npstring;
typedef char *nbstring;
typedef char *wbstring;
typedef mzchar *wmzstring;

typedef char *pathname;
typedef char *epathname;
typedef char *npathname;
typedef char *xpathname;
typedef char *nxpathname;
typedef char *nepathname;
typedef const char *cpathname;
typedef const char *cnpathname;
typedef char *wpathname;
typedef char *wnpathname;

typedef long nnlong;
typedef int nnint;
typedef double nndouble;

#define XC_SCHEME_NULL scheme_false
#define XC_SCHEME_NULLP(x) SCHEME_FALSEP(x)
#define XC_NULL_STR "#f"

#ifdef __GNUG__
# define WXS_USE_ARGUMENT(x) x = x; /* compiler optimizes it away */
#else
# define WXS_USE_ARGUMENT(x) 
#endif

#ifdef MZ_PRECISE_GC
# ifndef GC_STACK_CALLEE_RESTORE
#  define _SETUP_VAR_STACK(var, n, vs)    void *var[n + 2]; \
                                          var[0] = vs; \
                                          var[1] = (void *)n
#  define SETUP_VAR_STACK(n)              _SETUP_VAR_STACK(__gc_var_stack__, n, GC_variable_stack)
#  define SETUP_VAR_STACK_REMEMBERED(n)   _SETUP_VAR_STACK(__gc_var_stack__, n, __remembered_vs__)
#  define SETUP_VAR_STACK_PRE_REMEMBERED(n)  _SETUP_VAR_STACK(__gc_var_stack__, n, __remembered_vs__[0])
#  define SETUP_PRE_VAR_STACK(n)          _SETUP_VAR_STACK(__gc_pre_var_stack__, n, GC_variable_stack); \
                                          GC_variable_stack = __gc_pre_var_stack__
#  define VAR_STACK_PUSH(p, var)          __gc_var_stack__[p+2] = &(var)
#  define VAR_STACK_PUSH_ARRAY(p, var, n) __gc_var_stack__[p+2] = 0; \
                                          __gc_var_stack__[p+3] = &(var); \
                                          __gc_var_stack__[p+4] = (void *)(n)
#  define PRE_VAR_STACK_PUSH(p, var)      __gc_pre_var_stack__[p+2] = &(var)
#  define SET_VAR_STACK()                 GC_variable_stack = __gc_var_stack__
#  define WITH_VAR_STACK(x)               (SET_VAR_STACK(), x)
#  define REMEMBER_VAR_STACK()            void **__remembered_vs__ = GC_variable_stack
#  define WITH_REMEMBERED_STACK(x)        (GC_variable_stack = __remembered_vs__, x)
#  define READY_TO_RETURN                  /* empty */
#  define READY_TO_PRE_RETURN              /* empty */
# else
#  define _SETUP_VAR_STACK(var, n, vs)    void *var[n + 2]; \
                                          var[0] = vs; \
                                          var[1] = (void *)n; \
                                          GC_variable_stack = var
#  define SETUP_VAR_STACK(n)              _SETUP_VAR_STACK(__gc_var_stack__, n, GC_variable_stack)
#  define SETUP_VAR_STACK_REMEMBERED(n)   SETUP_VAR_STACK(n)
#  define SETUP_VAR_STACK_PRE_REMEMBERED(n) _SETUP_VAR_STACK(__gc_var_stack__, n, __gc_pre_var_stack__[0])
#  define SETUP_PRE_VAR_STACK(n)          _SETUP_VAR_STACK(__gc_pre_var_stack__, n, GC_variable_stack)
#  define VAR_STACK_PUSH(p, var)          __gc_var_stack__[p+2] = &(var)
#  define VAR_STACK_PUSH_ARRAY(p, var, n) __gc_var_stack__[p+2] = 0; \
                                          __gc_var_stack__[p+3] = &(var); \
                                          __gc_var_stack__[p+4] = (void *)(n)
#  define PRE_VAR_STACK_PUSH(p, var)      __gc_pre_var_stack__[p+2] = &(var)
#  define SET_VAR_STACK()                 /* empty */
#  define WITH_VAR_STACK(x)               x
#  define REMEMBER_VAR_STACK()            /* empty */
#  define WITH_REMEMBERED_STACK(x)        x
#  define READY_TO_RETURN                 GC_variable_stack = (void **)__gc_var_stack__[0]
#  define READY_TO_PRE_RETURN             READY_TO_RETURN
# endif

# define CONSTRUCTOR_ARGS(x)             ()
# define CONSTRUCTOR_INIT(x)             /* empty */
# define ASSELF                          sElF->
# define INIT_NULLED_OUT                 = NULLED_OUT
# define INIT_NULLED_ARRAY(x)            = x
# define INA_comma                       ,
#else
# define SETUP_VAR_STACK(n)              /* empty */
# define SETUP_VAR_STACK_REMEMBERED(n)   /* empty */
# define SETUP_VAR_STACK_PRE_REMEMBERED(n) /* empty */
# define SETUP_PRE_VAR_STACK(n)          /* empty */
# define VAR_STACK_PUSH(p, var)          /* empty */
# define VAR_STACK_PUSH_ARRAY(p, var, n) /* empty */
# define PRE_VAR_STACK_PUSH(p, var)      /* empty */
# define SET_VAR_STACK()                 /* empty */
# define WITH_VAR_STACK(x)               x
# define REMEMBER_VAR_STACK()            /* empty */
# define WITH_REMEMBERED_STACK(x)        x
# define READY_TO_RETURN                 /* empty */
# define READY_TO_PRE_RETURN             /* empty */
# define CONSTRUCTOR_ARGS(x)             x
# define CONSTRUCTOR_INIT(x)             x
# define ASSELF                          /* empty */
# define INIT_NULLED_OUT                 /* empty */
# define INIT_NULLED_ARRAY(x)            /* empty */
# define INA_comma                       /* empty */
#endif

#ifdef __cplusplus
};
#endif

#endif
