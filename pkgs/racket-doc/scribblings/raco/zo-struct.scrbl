#lang scribble/doc
@(require scribble/manual
          scribble/core
          (for-label racket/base
                     racket/contract
                     compiler/zo-structs
                     compiler/zo-parse
                     compiler/zo-marshal
                     compiler/decompile
                     racket/set))

@(define-syntax-rule (defstruct+ id fields . rest)
   (defstruct id fields #:prefab . rest))

@title{Bytecode Representation}

@defmodule[compiler/zo-structs]

The @racketmodname[compiler/zo-structs] library defines the bytecode
structures that are produced by @racket[zo-parse] and consumed by
@racket[decompile] and @racket[zo-marshal].

@nested[#:style 'inset]{
@elem[#:style (style #f (list (background-color-property "yellow")))]{@bold{Warning:}}
      The @racketmodname[compiler/zo-structs] library exposes internals
      of the Racket bytecode abstraction. Unlike other Racket
      libraries, @racketmodname[compiler/zo-structs] is subject to
      incompatible changes across Racket versions.}


@defstruct+[zo ()]{
  A supertype for all forms that can appear in compiled code.}

@; --------------------------------------------------
@section{Prefix}

@deftogether[(
@defstruct+[(linkl-directory zo)
            ([table (hash/c (listof symbol?) linkl-bundle?)])]
@defstruct+[(linkl-bundle zo)
            ([table (hash/c (or/c symbol? fixnum?) (or linkl? any/c))])]
)]{
  Wraps compiled code.

 Module and top-level compilation produce one or more linklets that
 represent independent evaluation in a specific phase. Even a single
 top-level expression or a module with only run-time code will
 generate multiple linklets to implement metadata and syntax data. A
 module with no submodules is represented directly by a
 @racket[linkl-bundle], while any other compiled form is represented
 by a @racket[linkl-directory].

 A linklet bundle maps an integer to a linklet representing forms to
 evaluate at the integer-indicated phase. Symbols are mapped to
 metadata, such as a module's name as compiled or a linklet
 implementing literal syntax objects. A linklet directory normally
 maps @racket['()] to the main linklet bundle for a module or a single
 top-level form; for a linklet directory that corresponds to a
 sequence of top-level forms, however, there is no ``main'' linklet
 bundle, and symbol forms of integers are used to order the linkets.
 
 For a module with submodules, the linklet directory maps submodule
 paths (as lists of symbols) to linklet bundles for the corresponding
 submodules.}

@defstruct+[(linkl zo)
            ([name symbol?]
             [importss (listof (listof symbol?))]
             [import-shapess (listof (listof  (or/c #f 'constant 'fixed 
                                                    function-shape? 
                                                    struct-shape?)))]
             [exports (listof symbol?)]
             [internals (listof (or/c symbol? #f))]
             [lifts (listof symbol?)]
             [source-names (hash/c symbol? symbol?)]
             [body (listof (or/c form? any/c))]
             [max-let-depth exact-nonnegative-integer?]
             [need-instance-access? boolean?])]{

  Represents a linklet, which corresponds to a module body or a
  top-level sequence at a single phase.

  The @racket[name] of a linklet is for debugging purposes, similar to
  the inferred name of a @racket[lambda] form.

  The @racket[importss] list of lists describes the linklet's imports.
  Each of the elements of the out list corresponds to an import
  source, and each element of an inner list is the symbolic name of an
  export from that source. The @racket[import-shapess] list is in
  parallel to @racket[imports]; it reflects optimization assumptions
  by the compiler that are used by the bytecode validator and checked
  when the linklet is instantiated.

 The @racket[exports] list describes the linklet's defined names that
 are exported. The @racket[internals] list describes additional
 definitions within the linket, but they are not accessible from the
 outside of a linklet or one of its instances; a @racket[#f] can appear
 in place of an unreferenced internal definition that has been removed.
 The @racket[lifts] list
 is an extension of @racket[internals] for procedures that are lifted
 by the compiler; these procedures have certain properties that can be
 checked by the bytecode validator.

 Each symbol in @racket[exports],
 @racket[internals], and @racket[lifts] must be distinct from any
 other symbol in those lists. The @racket[source-names] table maps
 symbols in @racket[exports], @racket[internals], and @racket[lifts]
 to other symbols, potentially not distinct, that correspond to
 original source names for the definition. The @racket[source-names]
 table is used only for debugging.

 When a linklet is instantiated, variables correponding to the
 flattening of the lists @racket[importss], @racket[exports],
 @racket[internals], and @racket[lifts] are placed in an array (in
 that order) for access via @racket[toplevel] references. The initial
 slot is reserved for a variable-like reference that strongly retains
 a connection to an instance of its enclosing linklet.

 The @racket[bodys] list is the executable content of the linklet. The
 value of the last element in @racket[bodys] may be returned when the
 linklet is instantiated, depending on the way that it's instantiated.

 The @racket[max-let-depth] field indicates the maximum size of the
 stack that will be created by any @racket[body].

 The @racket[need-instance-access?] boolean indicates whether the
 linklet contains a @racket[toplevel] for position 0. A @racket[#t] is
 allowed (but suboptimal) if not such reference is present in the
 linklet body.}

@defstruct+[function-shape
            ([arity procedure-arity?]
             [preserves-marks? boolean?])]{

Represents the shape of an expected import, which should be a function
having the arity specified by @racket[arity]. The
@racket[preserves-marks?]  field is true if calling the function is
expected to leave continuation marks unchanged by the time it
returns.}

@deftogether[(
@defstruct+[struct-shape ()]
@defstruct+[(struct-type-shape struct-shape) ([field-count exact-nonnegative-integer?] [authentic? boolean?])]
@defstruct+[(constructor-shape struct-shape) ([arity exact-nonnegative-integer?])]
@defstruct+[(predicate-shape struct-shape) ([authentic? boolean?])]
@defstruct+[(accessor-shape struct-shape) ([field-count exact-nonnegative-integer?] [authentic? boolean?])]
@defstruct+[(mutator-shape struct-shape) ([field-count exact-nonnegative-integer?] [authentic? boolean?])]
@defstruct+[(struct-type-property-shape struct-shape) ([has-guard? boolean?])]
@defstruct+[(property-predicate-shape struct-shape) ()]
@defstruct+[(property-accessor-shape struct-shape) ()]
@defstruct+[(struct-other-shape struct-shape) ()]
)]{

Represents the shape of an expected import as a structure-type
binding, constructor, etc.}


@; --------------------------------------------------
@section{Forms and Inline Variants}

@defstruct+[(form zo) ()]{
  A supertype for all forms that can appear in a linklet body (including
  @racket[expr]s), except for literals that are represented as
  themselves.}

@defstruct+[(def-values form)
            ([ids (listof toplevel?)]
             [rhs (or/c expr? seq? inline-variant? any/c)])]{
  Represents a @racket[define-values] form.  Each element of
  @racket[ids] references a defined variable in the enclosing linklet.

  After @racket[rhs] is evaluated, the stack is restored to its depth
  from before evaluating @racket[rhs].}

@defstruct+[(inline-variant zo) ([direct expr?]
                                 [inline expr?])]{
 Represents a function that is bound by @racket[define-values], where the
 function has two variants.
 The first variant is used for normal calls to the function. The second may
 be used for cross-module inlining of the function.}

@; --------------------------------------------------
@section{Expressions}

@defstruct+[(expr form) ()]{
  A supertype for all expression forms that can appear in compiled code,
  except for literals that are represented as themselves.}

@defstruct+[(lam expr)
            ([name (or/c symbol? vector?)]
             [flags (listof (or/c 'preserves-marks 'is-method 'single-result 
                                  'only-rest-arg-not-used 'sfs-clear-rest-args))]
             [num-params exact-nonnegative-integer?]
             [param-types (listof (or/c 'val 'ref 'flonum 'fixnum 'extflonum))]
             [rest? boolean?]
             [closure-map (vectorof exact-nonnegative-integer?)]
             [closure-types (listof (or/c 'val/ref 'flonum 'fixnum 'extflonum))]
             [toplevel-map (or/c #f (set/c exact-nonnegative-integer?))]
             [max-let-depth exact-nonnegative-integer?]
             [body (or/c expr? seq? any/c)])]{
  Represents a @racket[lambda] form.  The @racket[name] field is a name
  for debugging purposes.  The @racket[num-params] field indicates the
  number of arguments accepted by the procedure, not counting a rest
  argument; the @racket[rest?] field indicates whether extra arguments
  are accepted and collected into a ``rest'' variable.  The
  @racket[param-types] list contains @racket[num-params] symbols
  indicating the type of each argumet, either @racket['val] for a normal
  argument, @racket['ref] for a boxed argument (representing a mutable
  local variable), @racket['flonum] for a flonum argument,
  or @racket['extflonum] for an extflonum argument. 

  The
  @racket[closure-map] field is a vector of stack positions that are
  captured when evaluating the @racket[lambda] form to create a closure.
  The @racket[closure-types] field provides a corresponding list of
  types, but no distinction is made between normal values and boxed
  values; also, this information is redundant, since it can be inferred
  by the bindings referenced though @racket[closure-map].

  When a closure captures top-level or module-level variables or
  refers to a syntax-object constant, the variables and constants are
  represented in the closure by capturing a prefix (in the sense
  of @racket[prefix]).  The @racket[toplevel-map] field indicates
  which top-level variables (i.e., linklet imports and definitions) are actually used by the
  closure (so that variables in a prefix can be pruned by the run-time
  system if they become unused) and whether any syntax objects are
  used (so that the syntax objects as a group can be similarly
  pruned). A @racket[#f] value indicates either that no prefix is
  captured or all variables and syntax objects in the prefix should be
  considered used. Otherwise, numbers in the set indicate which
  variables and lifted variables are used. Variables are numbered
  consecutively by position in the prefix starting from
  @racket[0], but the number equal to the number of non-lifted
  variables corresponds to syntax objects (i.e., the number is
  include if any syntax-object constant is used). Lifted variables 
  are numbered immediately
  afterward---which means that, if the prefix contains any syntax
  objects, lifted-variable numbers are shifted down relative to a
  @racket[toplevel] by the number of syntax object in the prefix
  (which makes the @racket[toplevel-map] set more compact).

  When the function is called, the rest-argument list (if any) is pushed
  onto the stack, then the normal arguments in reverse order, then the
  closure-captured values in reverse order.  Thus, when @racket[body] is
  run, the first value on the stack is the first value captured by the
  @racket[closure-map] array, and so on.

  The @racket[max-let-depth] field indicates the maximum stack depth
  created by @racket[body] plus the arguments and closure-captured
  values pushed onto the stack.  The @racket[body] field is the
  expression for the closure's body.

  @history[#:changed "6.1.1.8" @elem{Added a number to
  @racket[toplevel-map] to indicate whether any syntax object is used,
  shifting numbers for lifted variables up by one if any syntax object
  is in the prefix.}]}

@defstruct+[(closure expr)
            ([code lam?] [gen-id symbol?])]{
  A @racket[lambda] form with an empty closure, which is a procedure
  constant.  The procedure constant can appear multiple times in the
  graph of expressions for bytecode, and the @racket[code] field can be
  a cycle for a recursive constant procedure; the @racket[gen-id] is
  different for each such constant.}

@defstruct+[(case-lam expr) ([name (or/c symbol? vector?)]
                             [clauses (listof lam?)])]{
  Represents a @racket[case-lambda] form as a combination of
  @racket[lambda] forms that are tried (in order) based on the number of
  arguments given.}

@defstruct+[(let-one expr)
            ([rhs (or/c expr? seq? any/c)]
             [body (or/c expr? seq? any/c)]
             [type (or/c #f 'flonum 'fixnum 'extflonum)]
             [unused? boolean?])]{
  Pushes an uninitialized slot onto the stack, evaluates @racket[rhs]
  and puts its value into the slot, and then runs @racket[body].  If
  @racket[type] is not @racket[#f], then @racket[rhs] must produce a
  value of the corresponding type, and the slot must be accessed by @racket[localref]s that
  expect the type.  If @racket[unused?] is @racket[#t], then the slot
  must not be used, and the value of @racket[rhs] is not actually pushed
  onto the stack (but @racket[rhs] is constrained to produce a single
  value).

  After @racket[rhs] is evaluated, the stack is restored to its depth
  from before evaluating @racket[rhs].  Note that the new slot is
  created before evaluating @racket[rhs].}

@defstruct+[(let-void expr)
            ([count exact-nonnegative-integer?]
             [boxes? boolean?]
             [body (or/c expr? seq? any/c)])]{
  Pushes @racket[count] uninitialized slots onto the stack and then runs
  @racket[body]. If @racket[boxes?] is @racket[#t], then the slots are
  filled with boxes that contain @|undefined-const|.}

@defstruct+[(install-value expr)
            ([count exact-nonnegative-integer?]
             [pos exact-nonnegative-integer?]
             [boxes? boolean?]
             [rhs (or/c expr? seq? any/c)]
             [body (or/c expr? seq? any/c)])]{
  Runs @racket[rhs] to obtain @racket[count] results, and installs them
  into existing slots on the stack in order, skipping the first
  @racket[pos] stack positions. If @racket[boxes?] is @racket[#t], then
  the values are put into existing boxes in the stack slots.

  After @racket[rhs] is evaluated, the stack is restored to its depth
  from before evaluating @racket[rhs].}

@defstruct+[(let-rec expr) ([procs (listof lam?)]
                            [body (or/c expr? seq? any/c)])]{
  Represents a @racket[letrec] form with @racket[lambda] bindings.  It
  allocates a closure shell for each @racket[lambda] form in
  @racket[procs], installs each onto the stack in previously allocated
  slots in reverse order (so that the closure shell for the last element
  of @racket[procs] is installed at stack position @racket[0]), fills
  out each shell's closure (where each closure normally references some
  other just-created closures, which is possible because the shells have
  been installed on the stack), and then evaluates @racket[body].}

@defstruct+[(boxenv expr)
            ([pos exact-nonnegative-integer?]
             [body (or/c expr? seq? any/c)])]{
  Skips @racket[pos] elements of the stack, setting the slot afterward
  to a new box containing the slot's old value, and then runs
  @racket[body]. This form appears when a @racket[lambda] argument is
  mutated using @racket[set!] within its body; calling the function
  initially pushes the value directly on the stack, and this form boxes
  the value so that it can be mutated later.}

@defstruct+[(localref expr)
            ([unbox? boolean?]
             [pos exact-nonnegative-integer?]
             [clear? boolean?]
             [other-clears? boolean?]
             [type (or/c #f 'flonum 'fixnum 'extflonum)])]{
  Represents a local-variable reference; it accesses the value in the
  stack slot after the first @racket[pos] slots.  If @racket[unbox?]  is
  @racket[#t], the stack slot contains a box, and a value is extracted
  from the box.  If @racket[clear?] is @racket[#t], then after the value
  is obtained, the stack slot is cleared (to avoid retaining a reference
  that can prevent reclamation of the value as garbage).  If
  @racket[other-clears?] is @racket[#t], then some later reference to
  the same stack slot may clear after reading.  If @racket[type] is
  not @racket[#f], the slot is known to hold a specific type of value.}

@defstruct+[(toplevel expr)
            ([depth exact-nonnegative-integer?]
             [pos exact-nonnegative-integer?]
             [const? boolean?]
             [ready? boolean?])]{
  Represents a reference to an imported or defined variable within
  a linklet. The @racket[depth] field indicates the number
  of stack slots to skip to reach the prefix array, and @racket[pos] is
  the offset into the array.

  When the @racket[toplevel] is an expression, if both @racket[const?]
  and @racket[ready?] are @racket[#t], then the variable definitely
  will be defined, its value stays constant, and the constant is
  effectively the same for every module instantiation. If only
  @racket[const?] is @racket[#t], then the value is constant, but it
  may vary across instantiations. If only @racket[ready?] is
  @racket[#t], then the variable definitely will be defined, but its
  value may change. If @racket[const?] and @racket[ready?] are both
  @racket[#f], then a check is needed to determine whether the
  variable is defined.

  When the @racket[toplevel] is the left-hand side for
  @racket[def-values], then @racket[const?] is @racket[#f]. If
  @racket[ready?] is @racket[#t], the variable is marked as immutable
  after it is defined.}

@defstruct+[(application expr)
            ([rator (or/c expr? seq? any/c)]
             [rands (listof (or/c expr? seq? any/c))])]{
  Represents a function call.  The @racket[rator] field is the
  expression for the function, and @racket[rands] are the argument
  expressions.  Before any of the expressions are evaluated,
  @racket[(length rands)] uninitialized stack slots are created (to be
  used as temporary space).}

@defstruct+[(branch expr)
            ([test (or/c expr? seq? any/c)]
             [then (or/c expr? seq? any/c)]
             [else (or/c expr? seq? any/c)])]{
  Represents an @racket[if] form.

  After @racket[test] is evaluated, the stack is restored to its depth
  from before evaluating @racket[test].}

@defstruct+[(with-cont-mark expr)
            ([key (or/c expr? seq? any/c)]
             [val (or/c expr? seq? any/c)]
             [body (or/c expr? seq? any/c)])]{
  Represents a @racket[with-continuation-mark] expression.

  After each of @racket[key] and @racket[val] is evaluated, the stack is
  restored to its depth from before evaluating @racket[key] or
  @racket[val].}

@defstruct+[(seq expr) ([forms (listof (or/c expr? any/c))])]{
  Represents a @racket[begin] form.

  After each form in @racket[forms] is evaluated, the stack is restored
  to its depth from before evaluating the form.}

@defstruct+[(beg0 expr) ([seq (listof (or/c expr? seq? any/c))])]{
  Represents a @racket[begin0] expression.
  
  After each expression in @racket[seq] is evaluated, the stack is
  restored to its depth from before evaluating the expression.

  Unlike the @racket[begin0] source form, the first expression in
  @racket[seq] is never in tail position, even if it is the only
  expression in the list.}

@defstruct+[(varref expr) ([toplevel (or/c toplevel? #t #f symbol?)] 
                           [dummy (or/c toplevel? #f)]
                           [constant? boolean?]
                           [from-unsafe? boolean?])]{
  Represents a @racket[#%variable-reference] form. The
  @racket[toplevel] field is @racket[#t] if the original reference was
  to a constant local binding, @racket[#f] if the variable reference
  is for @racket[(#%variable-reference)] and does not refer to a
  specific variable, or a symbol if the variable reference refers to a
  primitive variable. The @racket[dummy] field
  accesses a variable bucket that strongly references its namespace (as
  opposed to a normal variable bucket, which only weakly references its
  namespace); it can be @racket[#f].

  The value of @racket[constant?] is true when the @racket[toplevel]
  field is not @racket[#t] but the referenced variable is known to be
  constant. The value of @racket[from-unsafe?] is true when the module
  that created the reference was compiled in unsafe mode.}

@defstruct+[(assign expr)
            ([id toplevel?]
             [rhs (or/c expr? seq? any/c)]
             [undef-ok? boolean?])]{
  Represents a @racket[set!] expression that assigns to a top-level or
  module-level variable. (Assignments to local variables are represented
  by @racket[install-value] expressions.) If @racket[undef-ok?] is true,
  the assignment to @racket[id] succeeds even if @racket[id] was not
  previously defined (see also @racket[compile-allow-set!-undefined]).

  After @racket[rhs] is evaluated, the stack is restored to its depth
  from before evaluating @racket[rhs].}

@defstruct+[(apply-values expr)
            ([proc (or/c expr? seq? any/c)]
             [args-expr (or/c expr? seq? any/c)])]{
  Represents @racket[(call-with-values (lambda () args-expr) proc)],
  which is handled specially by the run-time system.}


@defstruct+[(with-immed-mark expr)
            ([key (or/c expr? seq? any/c)]
             [def-val (or/c expr? seq? any/c)]
             [body (or/c expr? seq? any/c)])]{

  Represents a @racket[(call-with-immediate-continuation-mark key
  (lambda (_arg) _body) val)] expression that is handled specially by
  the run-time system to avoid a closure allocation. One initialized
  slot is pushed onto the stack after @racket[expr] and @racket[val]
  are evaluated and before @racket[body] is evaluated.

  After each of @racket[key] and @racket[val] is evaluated, the stack is
  restored to its depth from before evaluating @racket[key] or
  @racket[val].}


@defstruct+[(primval expr)
            ([id exact-nonnegative-integer?])]{
  Represents a direct reference to a variable imported from the run-time
  kernel.}

