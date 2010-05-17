/* Example demonstrating how to inject a C++ class into the class
   world of MzLib's class.ss library.

   Since it uses C++, this example can be slightly tricky to compile.
   Specifying a C++ linker (e.g., g++) ensures that the right C++
   libraries get included:
     mzc --cc tree.cxx
     mzc --linker /usr/bin/g++ --ld tree.so tree.o

  The C++ class Tree defines the following:

    Tree(int init_leaves);                       constructor

    int leaves;                                  \ fields
    Tree *left_branch, *right_branch;            /

    void Graft(Tree *left, Tree *right);         method

    virtual void Grow(int n);                    \ overloaded and
    virtual void Grow(char *cmd, char *&result); / with ref param

  The Scheme version of the class has the following methods:

    "get-leaves", "get-left", "get-rght" -- gets field values
    "grow" -- override to replace C++ methods
    "graft" -- takes Scheme tree% objects

  Example use in Scheme:

    (load-extension "tree.so") ; defines tree-primitive-class and
                               ; other things not to be used directly
    (load "tree-finish.ss") ; defines tree%

    (define o (make-object tree% 10))
    (send o get-leaves) ; => 10
    (send o get-left) ; => #f

    (send o grow 2) ; grows new branches on the frontier
    (send o get-left) ; => #<object:tree%>
    (send (send o get-left) get-leaves) ; => 2
    
    (define b (box "sunshine"))
    (send o grow "sunshine" b)
    (unbox b) ; => "sprouted left"

    (define apple-tree%
      (class tree%
        (inherit graft)
        (override grow)
        
        (define grow
          ;; This `grow' drops branches and grows new ones.
          ;; For the command-string form, it does nothing.
          (case-lambda 
           [(n)
            (let ([l (make-object apple-tree%)]
                  [r (make-object apple-tree%)])
              (graft l r))]
           [(cmd result)
            (set-box! result (format "ignoring ~a" cmd))]))

        (super-instantiate () (leaves 1))))

    (define a (make-object apple-tree%))
    (send a get-leaves) ; => 1
    (send a grow 1)
    (send a get-left) ; => #<struct:object:apple-tree%>

    (define o (make-object tree% 10))
    (define a (make-object apple-tree%))
    (send o graft a #f)
    (send o grow 1)   ; C++ calls apple-tree%'s `grow' for `a'
    (send a get-left) ; -> #<struct:object:apple-tree>

    (send a grow "sunshine" b)
    (unbox b) ; => "ignoring sunshine"

 How it Works

   The class.ss library cooperates with primitive classes through a
   `make-primitive-class' function. The glue code in this file
   essentially builds up the necessary arguments to
   `make-primitive-class', and tree-finish.ss actually makes the
   call. In fact, tree.cxx knows nothing about the class
   implementation, and the class implementation knows nothing about
   the glue; they "just happen" to be compatible, but this glue could
   work with a variety of class implementation.

   The glue, furthermore, is split into two parts. The first part is
   specific to the Tree class. The second part is more generic,
   providing a fairly simple objscheme_ interface to class-specific
   glue, such the Tree glue. The second part can be shared for any
   number of C++ classes, and it is similar to code used by GRacket.
*/

#include "escheme.h"

/**********************************************************/
/* The original C++ class: Tree                           */
/**********************************************************/

/* This kind of tree never grows or loses leaves. It only changes when
   it grows subtrees, or when subtrees are grafted onto it. We can
   derive new classes (in Scheme) for trees that can grow leaves and
   fruit. */

class Tree {
private:

  int refcount; /* Suppose the C++ class uses reference counting. */

public:

  /* Public fields: */
  Tree *left_branch, *right_branch;
  int leaves;

  void *user_data; /* Field that we use for pointing back to the
		      Scheme view of the objects. The original class
		      might not be this friendly, but for simplicity
		      we assume that it is. The alternative is to use
		      a hash table. */

  Tree(int init_leaves) {
    left_branch = right_branch = NULL;
    leaves = init_leaves;
    refcount = 1;
    user_data = NULL;
  }

  /* The Grow method is overloaded... */

  virtual void Grow(int n) {
    if (left_branch)
      left_branch->Grow(n);
    else
      left_branch = new Tree(n);
    if (right_branch)
      right_branch->Grow(n);
    else
      right_branch = new Tree(n);
  }

  virtual void Grow(char *command, char *&result) {
    if (!strcmp(command, "sunshine")) {
      if (left_branch)
	left_branch->Grow(command, result);
      else {
	left_branch = new Tree(1);
	result = "sprouted left";
      }
    } else if (!strcmp(command, "water")) {
      if (right_branch)
	right_branch->Grow(command, result);
      else {
	right_branch = new Tree(1);
	result = "sprouted left";
      }
    } else {
      result = "unrecognized command for growing";
    }
  }

  void Graft(Tree *left, Tree *right) {
    Drop(left_branch);
    Drop(right_branch);

    left_branch = left;
    right_branch = right;

    Add(left_branch);
    Add(right_branch);
  }

  /* Note that Graft is not overrideable in C++.
     In Scheme, we might override this method, but
     the C++ code never has to know since it never
     calls the Graft method itself. */

  /* Reference counting utils: */

  static void Add(Tree *t) {
    if (t)
      t->refcount++;
  }
  static void Drop(Tree *t) {
    if (t) {
      t->refcount--;
      if (!t->refcount)
	delete t;
    }
  }
};

/**********************************************************/
/* The glue class: mzTree (C++ calls to Scheme)           */
/**********************************************************/

/* Forward declarations (documented further below) */
void objscheme_init();
void objscheme_add_procedures(Scheme_Env *);
Scheme_Object *objscheme_make_class(const char *name, Scheme_Object *sup, 
				    Scheme_Prim *initf, int num_methods);
Scheme_Object *objscheme_add_method_w_arity(Scheme_Object *c, const char *name, 
					    Scheme_Prim *f, int mina, int maxa);
Scheme_Object *objscheme_make_uninited_object(Scheme_Object *sclass);
Scheme_Object *objscheme_find_method(Scheme_Object *obj, char *name, void **cache);
int objscheme_is_a(Scheme_Object *o, Scheme_Object *c);


/* The #<primitive-class> value: */
static Scheme_Object *tree_class;
/* Cache for lookup of overrideable method: */
static void *grow_method_cache= NULL;
/* To recognize original overrideable method: */
Scheme_Object *grow_prim;

/* We keep a pointer to the Scheme object, and override the
   Grow method to (potentially) dispatch to Scheme. */

class mzTree : public Tree {
public:
  mzTree(int c) : Tree(c) { }

  virtual void Grow(int n) {
    /* Check whether the Scheme class for user_data is 
       actually a derived class that overrides `grow': */
    Scheme_Object *scmobj;
    Scheme_Object *overriding;

    /* Pointer to Scheme instance kept in user_data: */
    scmobj = (Scheme_Object *)user_data;

    /* Look for an overriding `grow' method in scmobj: */
    overriding = objscheme_find_method(scmobj,
				       "grow",
				       &grow_method_cache);

    if (overriding != grow_prim) {
      /* Call Scheme-based overriding implementation: */
      Scheme_Object *argv[2];

      argv[0] = scmobj;
      argv[1] = scheme_make_integer(n);
      _scheme_apply(overriding, 2, argv);
    } else {
      /* Grow is not overridden in Scheme: */
      Tree::Grow(n);
    }
  }

  /* Same strategy for other form of Grow, but we have to
     deal with the "result" parameter: */
  virtual void Grow(char *cmd, char *&result) {
    Scheme_Object *scmobj;
    Scheme_Object *overriding;

    scmobj = (Scheme_Object *)user_data;

    /* Look for an overriding `grow' method in scmobj: */
    overriding = objscheme_find_method(scmobj,
				       "grow",
				       &grow_method_cache);

    if (overriding != grow_prim) {
      /* When calling the Scheme-based overriding implementation,
	 we implement the `result' parameter as a boxed string.
	 The Scheme code mutates the box content to return a 
	 result. */
      Scheme_Object *argv[2], *res;

      argv[0] = scmobj;
      argv[1] = scheme_make_utf8_string(cmd);
      argv[2] = scheme_box(scheme_make_utf8_string(""));

      _scheme_apply(overriding, 3, argv);

      res = scheme_unbox(argv[2]);
      if (!SCHEME_CHAR_STRINGP(res)) {
	scheme_wrong_type("result for tree%'s grow method",
			  "string", -1, 0, &res);
      } else
	result = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[2]), -1, NULL, 0);
    } else {
      Tree::Grow(cmd, result);
    }
  }
};

/**********************************************************/
/* The glue functions (Scheme calls to C++)               */
/**********************************************************/

/* Macro for accessing C++ object pointer from a Scheme object: */
#define OBJSCHEME_GET_CPP_OBJ(obj) scheme_struct_ref(obj, 0)
#define OBJSCHEME_SET_CPP_OBJ(obj, v) scheme_struct_set(obj, 0, v)

/* Used for finalizing: */
void FreeTree(void *scmobj, void *t)
{
  Tree::Drop((Tree *)t);
}

Scheme_Object *Make_Tree(int argc, Scheme_Object **argv)
{
  Scheme_Object *obj;

  /* Unfortunately, init arity is not automatically checked: */
  if (argc != 2)
    scheme_wrong_count("tree% initialization", 2, 2, argc, argv);

  /* Assuming the initializer is only called through
     the class interface, argv[0] is always ok: */
  obj = argv[0];

  if (!SCHEME_INTP(argv[1]))
    scheme_wrong_type("tree% initialization", 
		      "fixnum", 
		      1, argc, argv);

  /* Create C++ instance, and remember pointer back to Scheme instance: */
  Tree *t = new mzTree(SCHEME_INT_VAL(argv[1]));
  t->user_data = obj;

  /* Store C++ pointer in Scheme object: */
  OBJSCHEME_SET_CPP_OBJ(obj, (Scheme_Object *)t);

  /* Free C++ instance when the Scheme object is no longer referenced: */
  scheme_add_finalizer(obj, FreeTree, t);

  return obj;
}

Scheme_Object *Grow(int argc, Scheme_Object **argv)
{
  Scheme_Object *obj = argv[0];

  if (argc == 2) {
    Tree *t;
    int n;

    if (!SCHEME_INTP(argv[1]))
      scheme_wrong_type("tree%'s grow", 
			"fixnum", 
			1, argc, argv);
    n = SCHEME_INT_VAL(argv[1]);
    
    /* Extract the C++ pointer: */
    t = (Tree *)OBJSCHEME_GET_CPP_OBJ(obj);
    
    /* Call method (without override check): */
    t->Tree::Grow(n);
  } else {
    Tree *t;
    char *cmd, *result;

    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("tree%'s grow", 
			"string", 
			1, argc, argv);
    if (!SCHEME_BOXP(argv[2])
	|| !SCHEME_CHAR_STRINGP(SCHEME_BOX_VAL(argv[2])))
      scheme_wrong_type("tree%'s grow", 
			"boxed string", 
			2, argc, argv);

    cmd = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[1]), -1, NULL, 0);
    result = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(SCHEME_BOX_VAL(argv[2])), 1, NULL, 0);

    /* Extract the C++ pointer: */
    t = (Tree *)OBJSCHEME_GET_CPP_OBJ(obj);
    
    /* Call method (without override check): */
    t->Tree::Grow(cmd, result);

    /* Put result back in box: */
    SCHEME_BOX_VAL(argv[2]) = scheme_make_utf8_string(result);
  }
  
  return scheme_void;
}

Scheme_Object *Graft(int argc, Scheme_Object **argv)
{
  Scheme_Object *obj = argv[0];
  Tree *t, *l, *r;

  if (!SCHEME_FALSEP(argv[1]) && !objscheme_is_a(argv[1], tree_class))
    scheme_wrong_type("tree%'s graft", 
		      "tree% object or #f", 
		      1, argc, argv);
  if (!SCHEME_FALSEP(argv[2]) && !objscheme_is_a(argv[2], tree_class))
    scheme_wrong_type("tree%'s graft", 
		      "tree% object or #f", 
		      2, argc, argv);

  /* Extract the C++ pointer for `this': */
  t = (Tree *)OBJSCHEME_GET_CPP_OBJ(obj);

  /* Extract the C++ pointers for the args: */
  l = (SCHEME_FALSEP(argv[1])
       ? (Tree *)NULL
       : (Tree *)OBJSCHEME_GET_CPP_OBJ(argv[1]));
  r = (SCHEME_FALSEP(argv[2])
       ? (Tree *)NULL
       : (Tree *)OBJSCHEME_GET_CPP_OBJ(argv[2]));
  
  /* Call method: */
  t->Graft(l, r);
  
  return scheme_void;
}

Scheme_Object *MarshalTree(Tree *t)
{
  if (!t)
    return scheme_false;
  else if (!t->user_data) {
    /* Object created in C++, not seen by Scheme, yet.
       Create a Scheme version of this object. */
    Scheme_Object *scmobj;

    /* Make Scheme object: */
    scmobj = objscheme_make_uninited_object(tree_class);

    /* Link C++ and Scheme objects: */
    t->user_data = scmobj;
    OBJSCHEME_SET_CPP_OBJ(scmobj, (Scheme_Object *)t);
    
    return scmobj;
  } else
    /* Get pointer back to Scheme: */
    return (Scheme_Object *)t->user_data;
}

Scheme_Object *Get_Left(int argc, Scheme_Object **argv)
{
  Tree *t = (Tree *)OBJSCHEME_GET_CPP_OBJ(argv[0]);
  
  return MarshalTree(t->left_branch);
}

Scheme_Object *Get_Right(int argc, Scheme_Object **argv)
{ 
  Tree *t = (Tree *)OBJSCHEME_GET_CPP_OBJ(argv[0]);
 
  return MarshalTree(t->right_branch);
}

Scheme_Object *Get_Leaves(int argc, Scheme_Object **argv)
{
  Tree *t = (Tree *)OBJSCHEME_GET_CPP_OBJ(argv[0]);
 
  return scheme_make_integer(t->leaves);
}

/**********************************************************/
/* Extension initialization: create the Scheme class      */
/**********************************************************/

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("tree-primitive-class", tree_class, env);

  objscheme_add_procedures(env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  objscheme_init();

  scheme_register_extension_global(&tree_class, sizeof(tree_class));

  tree_class = objscheme_make_class("tree%",    /* name */
				    NULL,       /* superclass */
				    Make_Tree,  /* init func */
				    5);         /* num methods */

  scheme_register_extension_global(&grow_prim, sizeof(grow_prim));

  grow_prim = objscheme_add_method_w_arity(tree_class, "grow",
					   Grow, 1, 2);
  (void)objscheme_add_method_w_arity(tree_class, "graft", 
				     Graft, 2, 2);
  
  (void)objscheme_add_method_w_arity(tree_class, "get-left",
				     Get_Left, 0, 0);
  (void)objscheme_add_method_w_arity(tree_class, "get-right",
				     Get_Right, 0, 0);
  (void)objscheme_add_method_w_arity(tree_class, "get-leaves",
				     Get_Leaves, 0, 0);

  return scheme_reload(env);
}


Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}

/**********************************************************/
/* The generic (class-independent) C++--Scheme glue       */
/**********************************************************/

/* 
   (This code is mostly the same as code used by GRacket, and duplicating
   it is certainly a bad idea in principle, but putting the code in a
   shareable place seems like more work than is worthwhile for now.)

   Scheme side:
   ------------

   This glue provides a new type, #<primitive-class>, and several
   procedures:

      (initialize-primitive-object prim-obj v ...) -
        initializes the primitive object, given initialization
        arguments v ...

      (primitive-class-prepare-struct-type! prim-class gen-property
        gen-value preparer dispatcher) - prepares a class's struct-type for
        objects generated C-side; returns a constructor, predicate,
        and a struct:type for derived classes. The constructor and
        struct:type map the given dispatcher to the class.

        The preparer takes a symbol naming the method. It returns a
        value to be used in future calls to the dispatcher.

        The dispatcher takes two arguments: an object and a
        method-specific value produced by the prepaper. It returns a
        method procedure.

      (primitive-class-find-method prim-class sym) - gets the method
        procedure for the given symbol from the class. The procedure
        consumes "self" and then the rest of the arguments.

   C side:
   -------

     void objscheme_init() - initializes the glue; call this first.

     void objscheme_add_procedures(Scheme_Env *) - installs the
        Scheme-side procedure listed above into the environment.

     Scheme_Object *objscheme_make_class(const char *name,
        Scheme_Object *sup, Scheme_Prim *initf, int num_methods) -
        creates a #<primitive-class> representing a C++ class. The
        initf function is called to create and initialize the C++-side
        when the class is instantiated from Scheme; the first argument
        is the Scheme-side `self' object. The Scheme-side object is a
        struct, and the first field should be set to point to the C++
        object.

        The sup argument is a #<primitive-class> for a superclass, or
        scheme_false. The num_methods argument specifies the number of
        methods that will be added to the class.

     void objscheme_add_method_w_arity(Scheme_Object *c, const char
	*name, Scheme_Prim *f, int mina, int maxa) - adds a method to
	a #<primitive-class>, specifying the method's arity as with
	scheme_make_prim_w_arity().

     Scheme_Object *objscheme_make_uninited_object(Scheme_Object *sclass)
        - creates a Scheme-side object for an existing C++ obj. The
        Scheme-side object is a struct, and the first field should be
        set to point to the C++ object.

     Scheme_Object *objscheme_find_method(Scheme_Object *obj, char
        *name, void **cache) - finds a method by name in a Scheme-side
        object. It is a Scheme procedure for the method (which takes
        the Scheme-side `self' as its first argument). The cache
        pointer should point to static, class-specific space for
        caching lookup information.

     int objscheme_is_a(Scheme_Object *o, Scheme_Object *c) - returns 1
        if the given Scheme-side object is an instance of the given
        #<primitive-class>, 0 otherwise.

*/

typedef struct Objscheme_Class {
  Scheme_Type type;
  const char *name;
  Scheme_Object *sup;
  Scheme_Object *initf;
  int num_methods, num_installed;
  Scheme_Object **names;
  Scheme_Object **methods;
  Scheme_Object *base_struct_type;
  Scheme_Object *struct_type;
} Objscheme_Class;

Scheme_Type objscheme_class_type;

static Scheme_Object *object_struct;
static Scheme_Object *object_property;
static Scheme_Object *preparer_property;
static Scheme_Object *dispatcher_property;

#define CONS(a, b) scheme_make_pair(a, b)

/***************************************************************************/
/* Scheme-side implementation: */

static Scheme_Object *init_prim_obj(int argc, Scheme_Object **argv)
{
  Objscheme_Class *c;
  Scheme_Object *obj = argv[0];

  if (!SCHEME_STRUCTP(argv[0])
      || !scheme_is_struct_instance(object_struct, argv[0]))
    scheme_wrong_type("initialize-primitive-object", "primitive-object", 0, argc, argv);
  
  c = (Objscheme_Class *)scheme_struct_type_property_ref(object_property, obj);

  return _scheme_apply(c->initf, argc, argv);
}

static Scheme_Object *class_prepare_struct_type(int argc, Scheme_Object **argv)
{
  Scheme_Object *name, *base_stype, *stype, *derive_stype;
  Scheme_Object **names, **vals, *a[3], *props;
  Objscheme_Class *c;
  int flags, count;

  if (SCHEME_TYPE(argv[0]) != objscheme_class_type)
    scheme_wrong_type("primitive-class-prepare-struct-type!", "primitive-class", 0, argc, argv);
  if (SCHEME_TYPE(argv[1]) != scheme_struct_property_type)
    scheme_wrong_type("primitive-class-prepare-struct-type!", "struct-type-property", 1, argc, argv);
  scheme_check_proc_arity("primitive-class-prepare-struct-type!", 1, 3, argc, argv);
  scheme_check_proc_arity("primitive-class-prepare-struct-type!", 2, 4, argc, argv);

  c = ((Objscheme_Class *)argv[0]);
  
  stype = c->struct_type;

  name = scheme_intern_symbol(c->name);

  if (stype) {
    scheme_arg_mismatch("primitive-class-prepare-struct-type!",
			"struct-type already prepared for primitive-class: ",
			name);
    return NULL;
  }

  if (SCHEME_TRUEP(c->sup) && !((Objscheme_Class *)c->sup)->base_struct_type) {
    scheme_arg_mismatch("primitive-class-prepare-struct-type!",
			"super struct-type not yet prepared for primitive-class: ",
			name);
    return NULL;
  }

  /* Root for this class.  */

  base_stype = scheme_make_struct_type(name, 
				       (SCHEME_TRUEP(c->sup) 
					? ((Objscheme_Class *)c->sup)->base_struct_type 
					: object_struct),
				       NULL,
				       0, 0, NULL,
				       NULL, NULL);
  c->base_struct_type = base_stype;

  /* Type to use when instantiating from C: */

  props = CONS(CONS(object_property, 
		    argv[0]),
	       scheme_null);

  stype = scheme_make_struct_type(name,
				  base_stype, 
				  NULL,
				  0, 0, NULL,
				  CONS(CONS(argv[1], argv[2]),
				       props),
				  NULL);
  
  c->struct_type = stype;
  
  /* Type to derive from Scheme: */
  
  props = CONS(CONS(preparer_property, argv[3]),
	       CONS(CONS(dispatcher_property, argv[4]),
		    props));

  derive_stype = scheme_make_struct_type(name,
					 base_stype, 
					 NULL,
					 0, 0, NULL,
					 props, 
					 NULL);
  
  /* Type to instantiate from Scheme: */
  
  stype = scheme_make_struct_type(name,
				  base_stype, 
				  NULL,
				  0, 0, NULL,
				  CONS(CONS(argv[1], argv[2]), props),
				  NULL);
  
  /* Need constructor from instantiate type: */
  flags = (SCHEME_STRUCT_NO_TYPE
	   | SCHEME_STRUCT_NO_PRED
	   | SCHEME_STRUCT_NO_GET
	   | SCHEME_STRUCT_NO_SET);
  names = scheme_make_struct_names(name, NULL, flags, &count);
  vals = scheme_make_struct_values(stype, names, count, flags);
  a[0] = vals[0];

  /* Need predicate from base type: */
  flags = (SCHEME_STRUCT_NO_TYPE
	   | SCHEME_STRUCT_NO_CONSTR
	   | SCHEME_STRUCT_NO_GET
	   | SCHEME_STRUCT_NO_SET);
  names = scheme_make_struct_names(name, NULL, flags, &count);
  vals = scheme_make_struct_values(base_stype, names, count, flags);
  a[1] = vals[0];

  /* Need derive type: */
  a[2] = derive_stype;

  return scheme_values(3, a);
}

static Scheme_Object *class_find_meth(int argc, Scheme_Object **argv)
{
  Objscheme_Class *sclass = (Objscheme_Class *)argv[0];
  Scheme_Object *s;
  int i;

  if (SCHEME_TYPE(argv[0]) != objscheme_class_type)
    scheme_wrong_type("primitive-class-find-method", "primitive-class", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type("primitive-class-find-method", "symbol", 1, argc, argv);

  s = argv[1];

  for (i = sclass->num_installed; i--; ) {
    if (SAME_OBJ(sclass->names[i], s))
      return sclass->methods[i];
  }

  return scheme_false;
}

Scheme_Object *objscheme_make_uninited_object(Scheme_Object *sclass)
{
  Scheme_Object *obj;
  Scheme_Object *stype;

  stype = ((Objscheme_Class *)sclass)->struct_type;
  if (!stype) {
    scheme_arg_mismatch("make-primitive-object",
			"struct-type not yet prepared: ",
			sclass);
    return NULL;
  }

  obj = scheme_make_struct_instance(stype, 0, NULL);

  return obj;  
}

/***************************************************************************/
/* C-side implementation: */

Scheme_Object *objscheme_make_class(const char *name, Scheme_Object *sup, 
				    Scheme_Prim *initf, int num_methods)
{
  Objscheme_Class *sclass;
  Scheme_Object *f, **methods, **names;

  sclass = (Objscheme_Class *)scheme_malloc_tagged(sizeof(Objscheme_Class));
  sclass->type = objscheme_class_type;

  if (!sup)
    sup = scheme_false;

  sclass->name = name;
  sclass->sup = sup;

  f = scheme_make_prim(initf);
  sclass->initf = f;

  sclass->num_methods = num_methods;
  sclass->num_installed = 0;

  methods = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * num_methods);
  names = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * num_methods);

  sclass->methods = methods;
  sclass->names = names;

  return (Scheme_Object *)sclass;
}

Scheme_Object *objscheme_add_method_w_arity(Scheme_Object *c, const char *name,
					    Scheme_Prim *f, int mina, int maxa)
{
  Scheme_Object *s;
  Objscheme_Class *sclass;

  sclass = (Objscheme_Class *)c;

  s = scheme_make_prim_w_arity(f, name, mina + 1, (maxa < 0) ? -1 : (maxa + 1));

  sclass->methods[sclass->num_installed] = s;

  s = scheme_intern_symbol(name);

  sclass->names[sclass->num_installed] = s;

  sclass->num_installed++;

  return s;
}

int objscheme_is_a(Scheme_Object *o, Scheme_Object *c)
{
  Scheme_Object *a;

  if (!SCHEME_STRUCTP(o) || !scheme_is_struct_instance(object_struct, o))
    return 0;

  a = scheme_struct_type_property_ref(object_property, o);
  
  while (a && (a != c)) {
    a = ((Objscheme_Class *)a)->sup;
  }

  return !!a;
}

void objscheme_init()
{
  objscheme_class_type = scheme_make_type("<primitive-class>");

  /* Attaches a primitive class to an object: */
  scheme_register_extension_global(&object_property, sizeof(object_property));
  object_property = scheme_make_struct_type_property(scheme_intern_symbol("primitive-object"));
  
  /* Attaches a preparer function to a derived class: */
  scheme_register_extension_global(&preparer_property, sizeof(preparer_property));
  preparer_property = scheme_make_struct_type_property(scheme_intern_symbol("primitive-preparer"));

  /* Attaches a dispatcher function to a derived class: */
  scheme_register_extension_global(&dispatcher_property, sizeof(dispatcher_property));
  dispatcher_property = scheme_make_struct_type_property(scheme_intern_symbol("primitive-dispatcher"));

  /* The base struct type for the Scheme view of a primitive object: */
  scheme_register_extension_global(&object_struct, sizeof(object_struct));
  object_struct = scheme_make_struct_type(scheme_intern_symbol("primitive-object"), 
					  NULL, NULL,
					  0, 2, NULL,
					  NULL, NULL);
}

void objscheme_add_procedures(Scheme_Env *env)
{
  scheme_add_global("initialize-primitive-object",
		    scheme_make_prim_w_arity(init_prim_obj,
					     "initialize-primitive-object",
					     1, -1),
		    env);

  scheme_add_global("primitive-class-prepare-struct-type!",
		    scheme_make_prim_w_arity(class_prepare_struct_type,
					     "primitive-class-prepare-struct-type!",
					     5, 5),
		    env);
  
  scheme_add_global("primitive-class-find-method",
		    scheme_make_prim_w_arity(class_find_meth,
					     "primitive-class-find-method",
					     2, 2),
		    env);
}

Scheme_Object *objscheme_find_method(Scheme_Object *obj, char *name, void **cache)
{
  Scheme_Object *s, *p[2], *dispatcher;

  if (!obj)
    return NULL;

  dispatcher = scheme_struct_type_property_ref(dispatcher_property, (Scheme_Object *)obj);
  if (!dispatcher)
    return NULL;

  if (*cache)
    s = (Scheme_Object *)*cache;
  else {
    s = scheme_intern_symbol(name);
    p[0] = s;
    s = scheme_struct_type_property_ref(preparer_property, (Scheme_Object *)obj);
    if (!s)
      return NULL;
    s = scheme_apply(s, 1, p);
    scheme_register_extension_global((void *)cache, sizeof(Scheme_Object*));
    *cache = s;
  }

  p[0] = obj;
  p[1] = s;
  return _scheme_apply(dispatcher, 2, p);
}
