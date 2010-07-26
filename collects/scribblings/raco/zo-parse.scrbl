#lang scribble/doc
@(require scribble/manual
          (for-label racket/base
                     racket/contract
                     compiler/zo-parse))

@(define-syntax-rule (defstruct+ id fields . rest)
   (defstruct id fields #:prefab . rest))

@title{API for Parsing Bytecode}

@defmodule[compiler/zo-parse]

@defproc[(zo-parse [in input-port? (current-input-port)]) compilation-top?]{

Parses a port (typically the result of opening a @filepath{.zo} file)
containing bytecode. Beware that the structure types used to represent the
bytecode are subject to frequent changes across Racket versons.

The parsed bytecode is returned in a @racket[compilation-top]
structure. For a compiled module, the @racket[compilation-top]
structure will contain a @racket[mod] structure. For a top-level
sequence, it will normally contain a @racket[seq] or @racket[splice]
structure with a list of top-level declarations and expressions.

The bytecode representation of an expression is closer to an
S-expression than a traditional, flat control string. For example, an
@racket[if] form is represented by a @racket[branch] structure that
has three fields: a test expression, a ``then'' expression, and an
``else'' expression.  Similarly, a function call is represented by an
@racket[application] structure that has a list of argument
expressions.

Storage for local variables or intermediate values (such as the
arguments for a function call) is explicitly specified in terms of a
stack. For example, execution of an @racket[application] structure
reserves space on the stack for each argument result. Similarly, when
a @racket[let-one] structure (for a simple @racket[let]) is executed,
the value obtained by evaluating the right-hand side expression is
pushed onto the stack, and then the body is evaluated. Local variables
are always accessed as offsets from the current stack position. When a
function is called, its arguments are passed on the stack. A closure
is created by transferring values from the stack to a flat closure
record, and when a closure is applied, the saved values are restored
on the stack (though possibly in a different order and likely in a
more compact layout than when they were captured).

When a sub-expression produces a value, then the stack pointer is
restored to its location from before evaluating the
sub-expression. For example, evaluating the right-hand size for a
@racket[let-one] structure may temporarily push values onto the stack,
but the stack is restored to its pre-@racket[let-one] position before
pushing the resulting value and continuing with the body. In addition,
a tail call resets the stack pointer to the position that follows the
enclosing function's arguments, and then the tail call continues by
pushing onto the stack the arguments for the tail-called function.

Values for global and module-level variables are not put directly on
the stack, but instead stored in ``buckets,'' and an array of
accessible buckets is kept on the stack. When a closure body needs to
access a global variable, the closure captures and later restores the
bucket array in the same way that it captured and restores a local
variable. Mutable local variables are boxed similarly to global
variables, but individual boxes are referenced from the stack and
closures.

Quoted syntax (in the sense of @racket[quote-syntax]) is treated like
a global variable, because it must be instantiated for an appropriate
phase. A @racket[prefix] structure within a @racket[compilation-top]
or @racket[mod] structure indicates the list of global variables and
quoted syntax that need to be instantiated (and put into an array on
the stack) before evaluating expressions that might use them.}

@; --------------------------------------------------
@section{Prefix}

@defstruct+[compilation-top ([max-let-depth exact-nonnegative-integer?]
                             [prefix prefix?]
                             [code (or/c form? indirect? any/c)])]{

Wraps compiled code. The @racket[max-let-depth] field indicates the
maximum stack depth that @racket[code] creates (not counting the
@racket[prefix] array). The @racket[prefix] field describes top-level
variables, module-level variables, and quoted syntax-objects accessed
by @racket[code]. The @racket[code] field contains executable code; it
is normally a @racket[form], but a literal value is represented as
itself.}


@defstruct+[prefix ([num-lifts exact-nonnegative-integer?]
                    [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))]
                    [stxs (listof stx?)])]{

Represents a ``prefix'' that is pushed onto the stack to initiate
evaluation. The prefix is an array, where buckets holding the values
for @racket[toplevels] are first, then a bucket for another array if
@racket[stxs] is non-empty, then @racket[num-lifts] extra buckets for
lifted local procedures.

In @racket[toplevels], each element is one of the following:

@itemize[

 @item{a @racket[#f], which indicates a dummy variable that is used to
       access the enclosing module/namespace at run time;}

 @item{a symbol, which is a reference to a variable defined in the
       enclosing module;}

 @item{a @racket[global-bucket], which is a top-level variable
       (appears only outside of modules); or}

 @item{a @racket[module-variable], which indicates a variable imported
       from another module.}

]

The variable buckets and syntax objects that are recorded in a prefix
are accessed by @racket[toplevel] and @racket[topsyntax] expression
forms.}


@defstruct+[global-bucket ([name symbol?])]{

Represents a top-level variable, and used only in a @racket[prefix].}


@defstruct+[module-variable ([modidx module-path-index?]
                             [sym symbol?]
                             [pos exact-integer?]
                             [phase (or/c 0 1)])]{

Represents a top-level variable, and used only in a @racket[prefix].
The @racket[pos] may record the variable's offset within its module,
or it can be @racket[-1] if the variable is always located by name.
The @racket[phase] indicates the phase level of the definition within
its module.}


@defstruct+[stx ([encoded wrapped?])]{

Wraps a syntax object in a @racket[prefix].}


@; --------------------------------------------------
@section{Forms}

@defstruct+[form ()]{

A supertype for all forms that can appear in compiled code (including
@racket[expr]s), except for literals that are represented as
themselves and @racket[indirect] structures to create cycles.}

@defstruct+[(def-values form) ([ids (listof toplevel?)]
                               [rhs (or/c expr? seq? indirect? any/c)])]{

Represents a @racket[define-values] form. Each element of @racket[ids]
will reference via the prefix either a top-level variable or a local
module variable.

After @racket[rhs] is evaluated, the stack is restored to its depth
from before evaluating @racket[rhs].}

@deftogether[(
@defstruct+[(def-syntaxes form) ([ids (listof toplevel?)]
                                 [rhs (or/c expr? seq? indirect? any/c)]
                                 [prefix prefix?]
                                 [max-let-depth exact-nonnegative-integer?])]
@defstruct+[(def-for-syntax form) ([ids (listof toplevel?)]
                                   [rhs (or/c expr? seq? indirect? any/c)]
                                   [prefix prefix?]
                                   [max-let-depth exact-nonnegative-integer?])]
)]{

Represents a @racket[define-syntaxes] or
@racket[define-values-for-syntax] form. The @racket[rhs] expression
has its own @racket[prefix], which is pushed before evaluating
@racket[rhs]; the stack is restored after obtaining the result
values. The @racket[max-let-depth] field indicates the maximum size of
the stack that will be created by @racket[rhs] (not counting
@racket[prefix]).}

@defstruct+[(req form) ([reqs stx?]
                        [dummy toplevel?])]{

Represents a top-level @racket[#%require] form (but not one in a
@racket[module] form) with a sequence of specifications
@racket[reqs]. The @racket[dummy] variable is used to access to the
top-level namespace.}


@defstruct+[(seq form) ([forms (listof (or/c form? indirect? any/c))])]{

Represents a @racket[begin] form, either as an expression or at the
top level (though the latter is more commonly a @racket[splice] form).
When a @racket[seq] appears in an expression position, its
@racket[forms] are expressions.

After each form in @racket[forms] is evaluated, the stack is restored
to its depth from before evaluating the form.}


@defstruct+[(splice form) ([forms (listof (or/c form? indirect? any/c))])]{

Represents a top-level @racket[begin] form where each evaluation is
wrapped with a continuation prompt.

After each form in @racket[forms] is evaluated, the stack is restored
to its depth from before evaluating the form.}


@defstruct+[(mod form) ([name symbol?]
                        [srcname symbol?]
                        [self-modidx module-path-index?]
                        [prefix prefix?]
                        [provides (listof (list/c (or/c exact-integer? #f)
                                                  (listof provided?)
                                                  (listof provided?)))]
                        [requires (listof (cons/c (or/c exact-integer? #f) 
                                                  (listof module-path-index?)))]
                        [body (listof (or/c form? indirect? any/c))]
                        [syntax-body (listof (or/c def-syntaxes? def-for-syntax?))]
                        [unexported (list/c (listof symbol?) (listof symbol?) 
                                            (listof symbol?))]
                        [max-let-depth exact-nonnegative-integer?]
                        [dummy toplevel?]
                        [lang-info (or/c #f (vector/c module-path? symbol? any/c))]
                        [internal-context (or/c #f #t stx?)])]{

Represents a @racket[module] declaration. The @racket[body] forms use
@racket[prefix], rather than any prefix in place for the module
declaration itself (and each @racket[syntax-body] has its own
prefix).

The @racket[provides] and @racket[requires] lists are each an
association list from phases to exports or imports. In the case of
@racket[provides], each phase maps to two lists: one for exported
variables, and another for exported syntax. In the case of
@racket[requires], each phase maps to a list of imported module paths.

The @racket[body] field contains the module's run-time code, and
@racket[syntax-body] contains the module's compile-time code.  After
each form in @racket[body] or @racket[syntax-body] is evaluated, the
stack is restored to its depth from before evaluating the form.

The @racket[unexported] list contains lists of symbols for unexported
definitions that can be accessed through macro expansion. The first
list is phase-0 variables, the second is phase-0 syntax, and the last
is phase-1 variables.

The @racket[max-let-depth] field indicates the maximum stack depth
created by @racket[body] forms (not counting the @racket[prefix]
array).  The @racket[dummy] variable is used to access to the
top-level namespace.

The @racket[lang-info] value specifies an optional module path that
provides information about the module's implementation language.

The @racket[internal-module-context] value describes the lexical
context of the body of the module. This value is used by
@racket[module->namespace]. A @racket[#f] value means that the context
is unavailable or empty. A @racket[#t] value means that the context is
computed by re-importing all required modules. A syntax-object value
embeds an arbitrary lexical context.}

@defstruct+[provided ([name symbol?]
                      [src (or/c module-path-index? #f)]
                      [src-name symbol?]
                      [nom-mod (or/c module-path-index? #f)]
                      [src-phase (or/c 0 1)]
                      [protected? boolean?]
                      [insp (or #t #f void?)])]{

Describes an individual provided identifier within a @racket[mod] instance.}

@; --------------------------------------------------
@section{Expressions}

@defstruct+[(expr form) ()]{

A supertype for all expression forms that can appear in compiled code,
except for literals that are represented as themselves,
@racket[indirect] structures to create cycles, and some @racket[seq]
structures (which can appear as an expression as long as it contains
only other things that can be expressions).}


@defstruct+[(lam expr) ([name (or/c symbol? vector?)]
                        [flags (listof (or/c 'preserves-marks 'is-method 'single-result))]
                        [num-params exact-nonnegative-integer?]
                        [param-types (listof (or/c 'val 'ref 'flonum))]
                        [rest? boolean?]
                        [closure-map (vectorof exact-nonnegative-integer?)]
                        [closure-types (listof (or/c 'val/ref 'flonum))]
                        [max-let-depth exact-nonnegative-integer?]
                        [body (or/c expr? seq? indirect? any/c)])]{

Represents a @racket[lambda] form. The @racket[name] field is a name
for debugging purposes. The @racket[num-params] field indicates the
number of arguments accepted by the procedure, not counting a rest
argument; the @racket[rest?] field indicates whether extra arguments
are accepted and collected into a ``rest'' variable. The
@racket[param-types] list contains @racket[num-params] symbols
indicating the type of each argumet, either @racket['val] for a normal
argument, @racket['ref] for a boxed argument (representing a mutable
local variable), or @racket['flonum] for a flonum argument. The
@racket[closure-map] field is a vector of stack positions that are
captured when evaluating the @racket[lambda] form to create a closure.
The @racket[closure-types] field provides a corresponding list of
types, but no distinction is made between normal values and boxed
values; also, this information is redundant, since it can be inferred by
the bindings referenced though @racket[closure-map].

When the function is called, the rest-argument list (if any) is pushed
onto the stack, then the normal arguments in reverse order, then the
closure-captured values in reverse order. Thus, when @racket[body] is
run, the first value on the stack is the first value captured by the
@racket[closure-map] array, and so on.

The @racket[max-let-depth] field indicates the maximum stack depth
created by @racket[body] plus the arguments and closure-captured
values pushed onto the stack. The @racket[body] field is the
expression for the closure's body.}


@defstruct+[(closure expr) ([code lam?] [gen-id symbol?])]{

A @racket[lambda] form with an empty closure, which is a procedure
constant. The procedure constant can appear multiple times in the
graph of expressions for bytecode, and the @racket[code] field can
refer back to the same @racket[closure] through an @racket[indirect]
for a recursive constant procedure; the @racket[gen-id] is different
for each such constant.}


@defstruct[indirect ([v closure?]) #:mutable #:prefab]{

An indirection used in expression positions to form cycles.}


@defstruct+[(case-lam expr) ([name (or/c symbol? vector?)]
                             [clauses (listof lam?)])]{

Represents a @racket[case-lambda] form as a combination of
@racket[lambda] forms that are tried (in order) based on the number of
arguments given.}


@defstruct+[(let-one expr) ([rhs (or/c expr? seq? indirect? any/c)]
                            [body (or/c expr? seq? indirect? any/c)]
                            [flonum? boolean?]
                            [unused? boolean?])]{

Pushes an uninitialized slot onto the stack, evaluates @racket[rhs]
and puts its value into the slot, and then runs @racket[body]. If
@racket[flonum?] is @racket[#t], then @racket[rhs] must produce a
flonum, and the slot must be accessed by @racket[localref]s that
expect a flonum. If @racket[unused?] is @racket[#t], then the slot
must not be used, and the value of @racket[rhs] is not actually pushed
onto the stack (but @racket[rhs] is constrained to produce a single
value).

After @racket[rhs] is evaluated, the stack is restored to its depth
from before evaluating @racket[rhs]. Note that the new slot is created
before evaluating @racket[rhs].}


@defstruct+[(let-void expr) ([count exact-nonnegative-integer?]
                             [boxes? boolean?]
                             [body (or/c expr? seq? indirect? any/c)])]{

Pushes @racket[count] uninitialized slots onto the stack and then runs
@racket[body]. If @racket[boxes?] is @racket[#t], then the slots are
filled with boxes that contain @|undefined-const|.}


@defstruct+[(install-value expr) ([count exact-nonnegative-integer?]
                                  [pos exact-nonnegative-integer?]
                                  [boxes? boolean?]
                                  [rhs (or/c expr? seq? indirect? any/c)]
                                  [body (or/c expr? seq? indirect? any/c)])]{

Runs @racket[rhs] to obtain @racket[count] results, and installs them
into existing slots on the stack in order, skipping the first
@racket[pos] stack positions. If @racket[boxes?] is @racket[#t], then
the values are put into existing boxes in the stack slots.

After @racket[rhs] is evaluated, the stack is restored to its depth
from before evaluating @racket[rhs].}


@defstruct+[(let-rec expr) ([procs (listof lam?)]
                            [body (or/c expr? seq? indirect? any/c)])]{

Represents a @racket[letrec] form with @racket[lambda] bindings. It
allocates a closure shell for each @racket[lambda] form in
@racket[procs], installs each onto the stack in previously
allocated slots in reverse order (so that the closure shell for the
last element of @racket[procs] is installed at stack position
@racket[0]), fills out each shell's closure (where each closure
normally references some other just-created closures, which is
possible because the shells have been installed on the stack), and
then evaluates @racket[body].}


@defstruct+[(boxenv expr) ([pos exact-nonnegative-integer?]
                           [body (or/c expr? seq? indirect? any/c)])]{

Skips @racket[pos] elements of the stack, setting the slot afterward
to a new box containing the slot's old value, and then runs
@racket[body]. This form appears when a @racket[lambda] argument is
mutated using @racket[set!] within its body; calling the function
initially pushes the value directly on the stack, and this form boxes
the value so that it can be mutated later.}


@defstruct+[(localref expr) ([unbox? boolean?]
                             [pos exact-nonnegative-integer?]
                             [clear? boolean?]
                             [other-clears? boolean?]
                             [flonum? boolean?])]{

Represents a local-variable reference; it accesses the value in the
stack slot after the first @racket[pos] slots. If @racket[unbox?]  is
@racket[#t], the stack slot contains a box, and a value is extracted
from the box. If @racket[clear?] is @racket[#t], then after the value
is obtained, the stack slot is cleared (to avoid retaining a reference
that can prevent reclamation of the value as garbage). If
@racket[other-clears?] is @racket[#t], then some later reference to
the same stack slot may clear after reading. If @racket[flonum?] is
@racket[#t], the slot holds to a flonum value.}


@defstruct+[(toplevel expr) ([depth exact-nonnegative-integer?]
                             [pos exact-nonnegative-integer?]
                             [const? boolean?]
                             [ready? boolean?])]{

Represents a reference to a top-level or imported variable via the
@racket[prefix] array. The @racket[depth] field indicates the number
of stack slots to skip to reach the prefix array, and @racket[pos] is
the offset into the array.

If @racket[const?] is @racket[#t], then the variable definitely will
be defined, and its value stays constant. If @racket[ready?] is
@racket[#t], then the variable definitely will be defined (but its
value might change in the future). If @racket[const?] and
@racket[ready?] are both @racket[#f], then a check is needed to
determine whether the variable is defined.}


@defstruct+[(topsyntax expr) ([depth exact-nonnegative-integer?]
                              [pos exact-nonnegative-integer?]
                              [midpt exact-nonnegative-integer?])]{

Represents a reference to a quoted syntax object via the
@racket[prefix] array. The @racket[depth] field indicates the number
of stack slots to skip to reach the prefix array, and @racket[pos] is
the offset into the array. The @racket[midpt] value is used internally
for lazy calculation of syntax information.}


@defstruct+[(application expr) ([rator (or/c expr? seq? indirect? any/c)]
                                [rands (listof (or/c expr? seq? indirect? any/c))])]{

Represents a function call. The @racket[rator] field is the expression
for the function, and @racket[rands] are the argument
expressions. Before any of the expressions are evaluated,
@racket[(length rands)] uninitialized stack slots are created (to be
used as temporary space).}


@defstruct+[(branch expr) ([test (or/c expr? seq? indirect? any/c)]
                           [then (or/c expr? seq? indirect? any/c)]
                           [else (or/c expr? seq? indirect? any/c)])]{

Represents an @racket[if] form.

After @racket[test] is evaluated, the stack is restored to its depth
from before evaluating @racket[test].}


@defstruct+[(with-cont-mark expr) ([key (or/c expr? seq? indirect? any/c)]
                                   [val (or/c expr? seq? indirect? any/c)]
                                   [body (or/c expr? seq? indirect? any/c)])]{

Represents a @racket[with-continuation-mark] expression. 

After each of @racket[key] and @racket[val] is evaluated, the stack is
restored to its depth from before evaluating @racket[key] or
@racket[val].}

@defstruct+[(beg0 expr) ([seq (listof (or/c expr? seq? indirect? any/c))])]{

Represents a @racket[begin0] expression.

After each expression in @racket[seq] is evaluated, the stack is
restored to its depth from before evaluating the expression.}


@defstruct+[(varref expr) ([toplevel toplevel?])]{

Represents a @racket[#%variable-reference] form.}


@defstruct+[(assign expr) ([id toplevel?]
                           [rhs (or/c expr? seq? indirect? any/c)]
                           [undef-ok? boolean?])]{

Represents a @racket[set!] expression that assigns to a top-level or
module-level variable. (Assignments to local variables are represented
by @racket[install-value] expressions.)

After @racket[rhs] is evaluated, the stack is restored to its depth
from before evaluating @racket[rhs].}


@defstruct+[(apply-values expr) ([proc (or/c expr? seq? indirect? any/c)]
                                 [args-expr (or/c expr? seq? indirect? any/c)])]{

Represents @racket[(call-with-values (lambda () args-expr) proc)],
which is handled specially by the run-time system.}


@defstruct+[(primval expr) ([id exact-nonnegative-integer?])]{

Represents a direct reference to a variable imported from the run-time
kernel.}

@; --------------------------------------------------
@section{Syntax Objects}

@defstruct+[wrapped ([datum any/c]
                     [wraps (listof wrap?)]
                     [certs (or/c list? #f)])]{

Represents a syntax object, where @racket[wraps] contain the lexical
information and @racket[certs] is certificate information. When the
@racket[datum] part is itself compound, its pieces are wrapped, too.}


@defstruct+[wrap ()]{

A supertype for lexical-information elements.}

@defstruct+[(top-level-rename wrap) ([flag boolean?])]{
                                                       
A top-level renaming.}

@defstruct+[(mark-barrier wrap) ([value symbol?])]{
                                                       
A mark barrier.}

@defstruct+[(lexical-rename wrap) ([alist (listof (cons/c symbol? symbol?))])]{

A local-binding mapping from symbols to binding-set names.}


@defstruct+[(phase-shift wrap) ([amt exact-integer?]
                                [src module-path-index?]
                                [dest module-path-index?])]{

Shifts module bindings later in the wrap set.}

@defstruct+[(module-rename wrap) ([phase exact-integer?]
                                 [kind (or/c 'marked 'normal)]
                                 [set-id any/c]
                                 [unmarshals (listof make-all-from-module?)]
                                 [renames (listof module-binding?)]
                                 [mark-renames any/c]
                                 [plus-kern? boolean?])]{

Represents a set of module and import bindings.}

@defstruct+[all-from-module ([path module-path-index?]
                             [phase (or/c exact-integer? #f)]
                             [src-phase (or/c exact-integer? #f)]
                             [exceptions (listof symbol?)]
                             [prefix (or/c symbol? #f)])]{

Represents a set of simple imports from one module within a
@racket[module-rename].}

@defstruct+[module-binding ()]{

A supertype for module bindings.}

@defstruct+[(simple-module-binding module-binding) ([path module-path-index?])]{
                                                                                
Represents a single identifier import within
a @racket[module-rename].}

@defstruct+[(phased-module-binding module-binding) ([path module-path-index?]
                                                    [phase exact-integer?]
                                                    [export-name any/c]
                                                    [nominal-path nominal-path?]
                                                    [nominal-export-name any/c])]{
                                                                                 
Represents a single identifier import within
a @racket[module-rename].}
                                                                                 
@defstruct+[(exported-nominal-module-binding module-binding) ([path module-path-index?]
                                                              [export-name any/c]
                                                              [nominal-path nominal-path?]
                                                              [nominal-export-name any/c])]{ 
                                                                                            
Represents a single identifier import within
a @racket[module-rename].}
                                                                                           
@defstruct+[(nominal-module-binding module-binding) ([path module-path-index?]
                                                     [nominal-path nominal-path?])]{       
                                                                                            
Represents a single identifier import within
a @racket[module-rename].}
                                                                                   
@defstruct+[(exported-module-binding module-binding) ([path module-path-index?]
                                                      [export-name any/c])]{
                                                                                                                                                                    
Represents a single identifier import within
a @racket[module-rename].}
                                                                           
                                                                           
@defstruct+[nominal-path ()]{
                             
A supertype for nominal paths.}

@defstruct+[(simple-nominal-path nominal-path) ([value module-path-index?])]{

Represents a simple nominal path.}

@defstruct+[(imported-nominal-path nominal-path) ([value module-path-index?] 
                                                  [import-phase exact-integer?])]{
                                                                                  
Represents an imported nominal path.}
                                                                                 
@defstruct+[(phased-nominal-path nominal-path) ([value module-path-index?]
                                                [import-phase (or/c false/c exact-integer?)]
                                                [phase exact-integer?])]{
                                                                         
Represents a phased nominal path.}
