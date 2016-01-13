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

@defstruct+[(compilation-top zo)
            ([max-let-depth exact-nonnegative-integer?]
             [binding-namess (hash/c exact-nonnegative-integer?
                                     (hash/c symbol? stx?))]
             [prefix prefix?]
             [code (or/c form? any/c)])]{
  Wraps compiled code.

  The @racket[max-let-depth] field indicates the
  maximum stack depth that @racket[code] creates (not counting the
  @racket[prefix] array).

  The @racket[binding-namess] field provides a per-phase mapping from
  symbols that appear in @racket[prefix] for top-level
  @racket[def-values] forms and in top-level @racket[def-syntaxes]
  forms. Each symbol is mapped to an identifier that will be bound
  (after introduction into the namespace) by the definition.

  The @racket[prefix] field describes top-level variables,
  module-level variables, and quoted syntax-objects accessed by
  @racket[code].

  The @racket[code] field contains executable code; it is normally a
  @racket[form], but a literal value is represented as itself.}

@defstruct+[(prefix zo)
            ([num-lifts exact-nonnegative-integer?]
             [toplevels (listof (or/c #f symbol? global-bucket?
                                      module-variable?))]
             [stxs (listof (or stx? #f))]
             [src-inspector-desc symbol?])]{
  Represents a ``prefix'' that is pushed onto the stack to initiate
  evaluation.  The prefix is an array, where buckets holding the
  values for @racket[toplevels] are first, then the buckets for the
  @racket[stxs], then a bucket for another array if @racket[stxs] is
  non-empty, then @racket[num-lifts] extra buckets for lifted local
  procedures.

  In @racket[toplevels], each element is one of the following:
  @itemize[
  @item{a @racket[#f], which indicates a dummy variable that is used
    to access the enclosing module/namespace at run time;}
  @item{a symbol, which is a reference to a variable defined in the
    enclosing module;}
  @item{a @racket[global-bucket], which is a top-level variable (appears
    only outside of modules); or}
  @item{a @racket[module-variable], which indicates a variable imported
    from another module.}
  ]

  The variable buckets and syntax objects that are recorded in a prefix
  are accessed by @racket[toplevel] and @racket[topsyntax] expression
  forms.
  
  When an element of @racket[stxs] is @racket[#f], it coresponds to a
  syntax object that was optimized away at the last minute. The slot
  must not be referenced vt a @racket[topsyntax] form.

  The @racket[src-inspector-desc] field provides an inspector name that
  is used within syntax-object bindings. At run time, the prefix gets
  an inspector, and bindings that reference the same inspector name are
  granted access capabilities through that inspector.}

@defstruct+[(global-bucket zo) ([name symbol?])]{
  Represents a top-level variable, and used only in a @racket[prefix].}

@defstruct+[(module-variable zo)
            ([modidx module-path-index?]
             [sym symbol?]
             [pos exact-integer?]
             [phase exact-nonnegative-integer?]
             [constantness (or/c #f 'constant 'fixed 
                                 function-shape? struct-shape?)])]{
  Represents a top-level variable, and used only in a @racket[prefix].
  The @racket[pos] may record the variable's offset within its module,
  or it can be @racket[-1] if the variable is always located by name.
  The @racket[phase] indicates the phase level of the definition within
  its module. The @racket[constantness] field is either @racket['constant],
  a @racket[function-shape] value, or a @racket[struct-shape] value
  to indicate that
  variable's value is always the same for every instantiation of its module;
  @racket['fixed] to indicate 
  that it doesn't change within a particular instantiation of the module;
  or @racket[#f] to indicate that the variable's value
  can change even for one particular instantiation of its module.}

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
@defstruct+[(struct-type-shape struct-shape) ([field-count exact-nonnegative-integer?])]
@defstruct+[(constructor-shape struct-shape) ([arity exact-nonnegative-integer?])]
@defstruct+[(predicate-shape struct-shape) ()]
@defstruct+[(accessor-shape struct-shape) ([field-count exact-nonnegative-integer?])]
@defstruct+[(mutator-shape struct-shape) ([field-count exact-nonnegative-integer?])]
@defstruct+[(struct-other-shape struct-shape) ()]
)]{

Represents the shape of an expected import as a structure-type
binding, constructor, etc.}

@defstruct+[(stx zo) ([content stx-obj?])]{
  Wraps a syntax object as it appears in a @racket[prefix].}


@; --------------------------------------------------
@section{Forms}

@defstruct+[(form zo) ()]{
  A supertype for all forms that can appear in compiled code (including
  @racket[expr]s), except for literals that are represented as
  themselves.}

@defstruct+[(def-values form)
            ([ids (listof toplevel?)]
             [rhs (or/c expr? seq? inline-variant? any/c)])]{
  Represents a @racket[define-values] form.  Each element of
  @racket[ids] will reference via the prefix either a top-level variable
  or a local module variable.

  After @racket[rhs] is evaluated, the stack is restored to its depth
  from before evaluating @racket[rhs].}

@deftogether[(
@defstruct+[(def-syntaxes form) ([ids (listof symbol?)]
                                 [rhs (or/c expr? seq? any/c)]
                                 [prefix prefix?]
                                 [max-let-depth exact-nonnegative-integer?]
                                 [dummy (or/c toplevel? #f)])]
@defstruct+[(seq-for-syntax form)
            ([forms (listof (or/c form? any/c))]
             [prefix prefix?]
             [max-let-depth exact-nonnegative-integer?]
             [dummy (or/c toplevel? #f)])]
)]{
  Represents a @racket[define-syntaxes] or
  @racket[begin-for-syntax] form.  The @racket[rhs] expression or set of
  @racket[forms] forms has its own @racket[prefix], which is pushed before evaluating
  @racket[rhs] or the @racket[forms]; the stack is restored after obtaining the result values.
  The @racket[max-let-depth] field indicates the maximum size of the
  stack that will be created by @racket[rhs] (not counting
  @racket[prefix]).  The @racket[dummy] variable is used to access the enclosing 
  namespace.}

@defstruct+[(req form) ([reqs stx?]
                        [dummy toplevel?])]{
  Represents a top-level @racket[#%require] form (but not one in a
  @racket[module] form) with a sequence of specifications @racket[reqs].
  The @racket[dummy] variable is used to access the top-level
  namespace.}

@defstruct+[(seq form) ([forms (listof (or/c form? any/c))])]{
  Represents a @racket[begin] form, either as an expression or at the
  top level (though the latter is more commonly a @racket[splice] form).
  When a @racket[seq] appears in an expression position, its
  @racket[forms] are expressions.

  After each form in @racket[forms] is evaluated, the stack is restored
  to its depth from before evaluating the form.}

@defstruct+[(splice form) ([forms (listof (or/c form? any/c))])]{
  Represents a top-level @racket[begin] form where each evaluation is
  wrapped with a continuation prompt.

  After each form in @racket[forms] is evaluated, the stack is restored
  to its depth from before evaluating the form.}

@defstruct+[(inline-variant form) ([direct expr?]
                                   [inline expr?])]{
 Represents a function that is bound by @racket[define-values], where the
 function has two variants.
 The first variant is used for normal calls to the function. The second may
 be used for cross-module inlining of the function.}

@defstruct+[(mod form)
            ([name (or/c symbol? (listof symbol?))]
             [srcname symbol?]
             [self-modidx module-path-index?]
             [prefix prefix?]
             [provides (listof (list/c (or/c exact-integer? #f)
                                       (listof provided?)
                                       (listof provided?)))]
             [requires (listof (cons/c (or/c exact-integer? #f)
                                       (listof module-path-index?)))]
             [body (listof (or/c form? any/c))]
             [syntax-bodies  (listof (cons/c exact-positive-integer?
                                             (listof (or/c def-syntaxes? 
                                                           seq-for-syntax?))))]
             [unexported (listof (list/c exact-nonnegative-integer?
                                         (listof symbol?)
                                         (listof symbol?)))]
             [max-let-depth exact-nonnegative-integer?]
             [dummy toplevel?]
             [lang-info (or/c #f (vector/c module-path? symbol? any/c))]
             [internal-context (or/c #f #t stx? (vectorof stx?))]
             [binding-names (hash/c exact-integer?
                                    (hash/c symbol? (or/c #t stx?)))]
             [flags (listof (or/c 'cross-phase))]
             [pre-submodules (listof mod?)]
             [post-submodules (listof mod?)])]{
  Represents a @racket[module] declaration.

  The @racket[provides] and @racket[requires] lists are each an
  association list from phases to exports or imports.  In the case of
  @racket[provides], each phase maps to two lists: one for exported
  variables, and another for exported syntax.  In the case of
  @racket[requires], each phase maps to a list of imported module paths.

  The @racket[body] field contains the module's run-time (i.e., phase
  0) code. The @racket[syntax-bodies] list has a list of forms for
  each higher phase in the module body; the phases are in order
  starting with phase 1.  The @racket[body] forms use @racket[prefix],
  rather than any prefix in place for the module declaration itself,
  while members of lists in @racket[syntax-bodies] have their own
  prefixes. After each form in @racket[body] or @racket[syntax-bodies]
  is evaluated, the stack is restored to its depth from before
  evaluating the form.

  The @racket[unexported] list contains lists of symbols for
  unexported definitions that can be accessed through macro expansion
  and that are implemented through the forms in @racket[body] and
  @racket[syntax-bodies].  Each list in @racket[unexported] starts
  with a phase level.

  The @racket[max-let-depth] field indicates the maximum stack depth
  created by @racket[body] forms (not counting the @racket[prefix]
  array).  The @racket[dummy] variable is used to access to the
  top-level namespace.

  The @racket[lang-info] value specifies an optional module path that
  provides information about the module's implementation language.

  The @racket[internal-context] value describes the lexical context of
  the body of the module. This value is used by
  @racket[module->namespace]. A @racket[#f] value means that the
  context is unavailable or empty. A @racket[#t] value means that the
  context is computed by re-importing all required modules. A
  syntax-object value embeds lexical information; the syntax object
  should contain a vector of two elements, where the first element of
  the vector is a syntax object for the module's body, which includes
  the outside-edge and inside-edge scopes, and the second element of
  the vector is a syntax object that has just the module's inside-edge
  scope.

  The @racket[binding-names] value provides additional information to
  @racket[module->namespace] to correlate symbol names for variables
  and syntax definitions to identifiers that map to those variables. A
  separate table of names exists for each phase, and a @racket[#t]
  mapping for a name indicates that it is mapped but inaccessible
  (because the relevant scopes are inaccessible).

  The @racket[flags] field records certain properties of the module.
  The @racket['cross-phase] flag indicates that the module body is
  evaluated once and the results shared across instances for all phases; such a
  module contains only definitions of functions, structure types, and
  structure type properties.

  The @racket[pre-submodules] field records @racket[module]-declared
  submodules, while the @racket[post-submodules] field records
  @racket[module*]-declared submodules.}

@defstruct+[(provided zo)
            ([name symbol?]
             [src (or/c module-path-index? #f)]
             [src-name symbol?]
             [nom-src (or/c module-path-index? #f)]
             [src-phase exact-nonnegative-integer?]
             [protected? boolean?])]{
  Describes an individual provided identifier within a @racket[mod]
  instance.}

@; --------------------------------------------------
@section{Expressions}

@defstruct+[(expr form) ()]{
  A supertype for all expression forms that can appear in compiled code,
  except for literals that are represented as themselves and some
  @racket[seq] structures (which can appear as an expression as long as
  it contains only other things that can be expressions).}

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
  which top-level and lifted variables are actually used by the
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
  Represents a reference to a top-level or imported variable via the
  @racket[prefix] array. The @racket[depth] field indicates the number
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

  When the @racket[toplevel] is the right-hand side for
  @racket[def-values], then @racket[const?]  is @racket[#f]. If
  @racket[ready?] is @racket[#t], the variable is marked as immutable
  after it is defined.}

@defstruct+[(topsyntax expr)
            ([depth exact-nonnegative-integer?]
             [pos exact-nonnegative-integer?]
             [midpt exact-nonnegative-integer?])]{
  Represents a reference to a quoted syntax object via the
  @racket[prefix] array.  The @racket[depth] field indicates the number
  of stack slots to skip to reach the prefix array, and @racket[pos] is
  the offset into the array.  The @racket[midpt] value is used
  internally for lazy calculation of syntax information.}

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

@defstruct+[(beg0 expr) ([seq (listof (or/c expr? seq? any/c))])]{
  Represents a @racket[begin0] expression.
  
  After each expression in @racket[seq] is evaluated, the stack is
  restored to its depth from before evaluating the expression.

  Unlike the @racket[begin0] source form, the first expression in
  @racket[seq] is never in tail position, even if it is the only
  expression in the list.}

@defstruct+[(varref expr) ([toplevel (or/c toplevel? #t)] 
                           [dummy (or/c toplevel? #f)])]{
  Represents a @racket[#%variable-reference] form. The @racket[toplevel]
  field is @racket[#t] if the original reference was to a constant local
  binding. The @racket[dummy] field
  accesses a variable bucket that strongly references its namespace (as
  opposed to a normal variable bucket, which only weakly references its
  namespace); it can be @racket[#f].}

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
             [val (or/c expr? seq? any/c)]
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

@; --------------------------------------------------
@section{Syntax Objects}

@defstruct+[(stx-obj zo)
            ([datum any/c]
             [wrap wrap?]
             [srcloc (or/c #f srcloc?)]
             [props (hash/c symbol? any/c)]
             [tamper-status (or/c 'clean 'armed 'tainted)])]{
  Represents a syntax object, where @racket[wrap] contains lexical
  information, @racket[srcloc] is the source location,
  @racket[props] contains preserved properties,
  and @racket[tamper-status] is taint information. When the
  @racket[datum] part is itself compound, its pieces are wrapped
  as @racket[stx-obj]s, too.

  The content of @racket[wrap] is typically cyclic, since it includes
  scopes that contain bindings that refer to scopes.}

@defstruct+[(wrap zo) ([shifts (listof module-shift?)]
                       [simple-scopes (listof scope?)]
                       [multi-scopes (listof (list/c multi-scope? (or/c #f exact-integer?)))])]{
  Lexical information for a syntax object. The @racket[shifts] field
  allows binding information to be relative to the enclosing module's
  run-time path. The @racket[simple-scopes] field records scopes that
  are attached to the syntax object at all phases, and @racket[multi-scopes]
  records phase-specific scopes (which are always attached as a group)
  along with a phase shift for every scope within the group).}

@defstruct+[(module-shift zo) ([from (or/c #f module-path-index?)]
                               [to (or/c #f module-path-index?)]
                               [from-inspector-desc (or/c #f symbol?)]
                               [to-inspector-desc (or/c #f symbol?)])]{

Records a history of module path index replacements. These replacements
are applied in reverse order, and a module instantiation typically adds
one more shift to replace the current ``self'' module path index
with a run-time module path. The @racket[from] and @racket[to]
fields should be both @racket[#f] or both non-@racket[#f].

The @racket[from-inspector-desc] and @racket[to-inspector-desc] fields
similarly should be both @racket[#f] or both non-@racket[#f]. They
record a history of code-inspector replacements.}


@defstruct+[(scope zo) ([name (or/c 'root exact-nonnegative-integer?)]
                        [kind symbol?]
                        [bindings (listof (list/c symbol? (listof scope?) binding?)) #;#:mutable]
                        [bulk-bindings (listof (list/c (listof scope?) all-from-module?)) #;#:mutable]
                        [multi-owner (or/c #f multi-scope?) #;#:mutable])]{

Represents a scope. When @racket[name] is @racket['root] then the
scope represents the unique all-phases scope that is shared among
non-module namespaces. Otherwise, @racket[name] is intended to be
distinct for each @racket[scope] instance within a module or top-level
compilation, but the @racket[eq?]-identity of the @racket[scope]
instance ultimately determines its identity. The @racket[kind] symbol
similarly acts as a debugging hint in the same way as for
@racket[syntax-debug-info].

The @racket[bindings] list indicates some bindings that are associated
with the scope. Each element of the list includes a symbolic name, a
list of scopes (including the enclosing one), and the binding for the
combination of name and scope set. A given symbol can appear in
multiple elements of @racket[bindings], but the combination of the
symbol and scope set are unique within @racket[bindings] and across
all scopes. The mapping of a symbol and scope set to a binding is
recorded with an arbitrary member of the scope set.

The @racket[bulk-bindings] field lists bindings of all exports from a
given module, which is an optimization over including each export in
@racket[bindings]. Elements of @racket[bindings] take precedence over
elements of @racket[bulk-bindings], and earlier elements of
@racket[bulk-bindings] take precedence over later elements.

If the @racket[scope] represents a scope at a particular phase for a
group of phase-specific scopes, @racket[mark-owner] refers to the
group.}


@defstruct+[(multi-scope zo) ([name exact-nonnegative-integer?]
                              [src-name any/c]
                              [scopes (listof (list/c (or/c #f exact-integer?) scope?)) #;#:mutable])]{

Represents a set of phase-specific scopes that are added or removed
from lexical information as a group. As for @racket[scope], the
@racket[name] field is intended to be distinct for different groups,
but the @racket[eq?] identity of the @racket[multi-scope] record
ultimately determines its identity. The @racket[src-name] field
similarly acts as a debugging hint in the same way as for
@racket[syntax-debug-info].

Scopes within the group are instantiated at different phases on
demand. The @racket[scopes] field lists all of the scopes instantiated
for the group, and the phase at which it is instantiated. Each element
of @racket[scopes] must have a @racketidfont{multi-owner} field
value that refers back to the @racket[multi-scope].}


@defstruct+[(binding zo) ()]{

A supertype for all binding representations.}


@defstruct+[(module-binding binding) ([encoded any/c])]{

Represents a binding to a module or top-level definition. The
@racket[encoded] field can be unpacked using
@racket[decode-module-binding], providing the symbol name for which
the binding is the target (since @racket[encoded] can be relative to
that name).}


@defstruct+[(decoded-module-binding binding) ([path (or/c #f module-path-index?)]
                                              [name symbol?]
                                              [phase exact-integer?]
                                              [nominal-path (or/c #f module-path-index?)]
                                              [nominal-export-name symbol?]
                                              [nominal-phase (or/c #f exact-integer?)]
                                              [import-phase (or/c #f exact-integer?)]
                                              [inspector-desc (or/c #f symbol?)])]{

Represents a binding to a module or top-level definition---like
@racket[module-binding], but in normalized form:

@itemlist[

 @item{@racket[path]: the referenced module.}

 @item{@racket[name]: the referenced definition within its module.}

 @item{@racket[phase]: the phase of the referenced definition within
       its module.}

 @item{@racket[nominal-path]: the module that was explicitly imported
       into the binding context; this path can be different from
       @racket[path] when a definition is re-exported.}

 @item{@racket[nominal-export-name]: the name of the binding as
       exported from @racket[nominal-path], which can be different from
       @racket[name] due to renaming on export.}

 @item{@racket[nominal-phase]: the phase of the export from
       @racket[nominal-path], which can be different from @racket[phase]
       due to re-export from a module that imports at a phase level other
       than @racket[0].}

 @item{@racket[import-phase]: the phase of the import of
       @racket[nominal-path], which shifted (if non-@racket[0]) the
       binding phase relative to the export phase from
       @racket[nominal-path].}

 @item{@racket[inspector-desc]: a name for an inspector (mapped to a
       specific inspector at run time) that determines access to the
       definition.}

]}

@defstruct+[(local-binding binding) ([name symbol?])]{

Represents a local binding (i.e., not at the top level or module level).
Such bindings rarely appear in bytecode, since @racket[quote-syntax]
prunes them.}


@defstruct+[(free-id=?-binding binding) ([base (and/c binding?
                                                      (not/c free-id=?-binding?))]
                                         [id stx-obj?]
                                         [phase (or/c #f exact-integer?)])]{

Represents a binding that includes a @racket[free-identifier=?] alias
(to an identifier with a particular phase shift) as well as a base binding.}


@defstruct+[(all-from-module zo) ([path module-path-index?] 
                                  [phase (or/c exact-integer? #f)] 
                                  [src-phase (or/c exact-integer? #f)]
                                  [inspector-desc symbol?]
                                  [exceptions (listof symbol?)]
                                  [prefix (or/c symbol? #f)])]{

Describes a bulk import as an optimization over individual imports of
a module's exports:

@itemlist[

 @item{@racket[path]: the imported module.}
 
 @item{@racket[phase]: the phase of the import module's exports.}

 @item{@racket[src-phase]: the phase at which @racket[path] was
       imported; @racket[src-phase] combined with @racket[phase]
       determines the phase of the bindings.}

 @item{@racket[inspector-desc]: a name for an inspector (mapped to a
       specific inspector at run time) that determines access to the
       definition.}

 @item{@racket[exceptions]: exports of @racket[path] that are omitted
       from the bulk import.}
 
 @item{@racket[prefix]: a prefix, if any, applied (after
       @racket[exceptions]) to each of the imported names.}

]}

 
