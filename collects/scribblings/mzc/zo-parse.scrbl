#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     compiler/zo-parse))

@(define-syntax-rule (defstruct+ id fields . rest)
   (defstruct id fields #:transparent . rest))

@title{Scheme API for Parsing Bytecode}

@defmodule[compiler/zo-parse]

@defproc[(zo-parse [in input-port?]) compilation-top?]{

Parses a port (typically the result of opening a @filepath{.zo} file)
containing byte. The parsed bytecode is returned in a
@scheme[compilation-top] structure.

Beware that the structure types used to represent the bytecode are
subject to frequent changes across PLT Scheme versons.}

@; --------------------------------------------------
@section{Prefix}

@defstruct+[compilation-top ([max-let-depth exact-nonnegative-integer?]
                             [prefix prefix?]
                             [code (or/c form? indirect? any/c)])]{

Wraps compiled code. The @scheme[max-let-depth] field indicates the
maximum stack depth that @scheme[code] creates (not counting the
@scheme[prefix] array). The @scheme[prefix] field describes top-level
variables, module-level variables, and quoted syntax-objects accessed
by @scheme[code]. The @scheme[code] field contains executable code; it
is normally a @scheme[form], but a literal value is represented as
itself.}


@defstruct+[prefix ([num-lifts exact-nonnegative-integer?]
                    [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))]
                    [stxs (listof stx?)])]{

Represents a ``prefix'' that is pushed onto the stack to initiate
evaluation. The prefix is an array, where buckets holding the values
for @scheme[toplevels] are first, then a bucket for another array if
@scheme[stxs] is non-empty, then @scheme[num-lifts] extra buckets for
lifted local procedures.

In @scheme[toplevels], each element is one of the following:

@itemize[

 @item{a @scheme[#f], which indicates a dummy variable that is used to
       access the enclosing module/namespace at run time;}

 @item{a symbol, which is a reference to a variable defined in the
       enclosing module;}

 @item{a @scheme[global-bucket], which is a top-level variable
       (appears only outside of modules); or}

 @item{a @scheme[module-variable], which indicates a variable imported
       from another module.}

]

The variable buckets and syntax objects that are recorded in a prefix
are accessed by @scheme[toplevel] and @scheme[topsyntax] expression
forms.}


@defstruct+[global-bucket ([name symbol?])]{

Represents a top-level variable, and used only in a @scheme[prefix].}


@defstruct+[module-variable ([modidx module-path-index?]
                             [sym symbol?]
                             [pos exact-integer?]
                             [phase (or/c 0 1)])]{

Represents a top-level variable, and used only in a @scheme[prefix].
The @scheme[pos] may record the variable's offset within its module,
or it can be @scheme[-1] if the variable is always located by name.
The @scheme[phase] indicates the phase level of the definition within
its module.}


@defstruct+[stx ([encoded wrapped?])]{

Wraps a syntax object in a @scheme[prefix].}


@; --------------------------------------------------
@section{Forms}

@defstruct+[form ()]{

A supertype for all forms that can appear in compiled code (including
@scheme[expr]s), except for literals that are represented as
themselves and @scheme[indirect] structures to create cycles.}

@defstruct+[(def-values form) ([ids (listof toplevel?)]
                               [rhs (or/c expr? seq? indirect? any/c)])]{

Represents a @scheme[define-values] form. Each element of @scheme[ids]
will reference via the prefix either a top-level variable or a local
module variable.

After @scheme[rhs] is evaluated, the stack is restored to its depth
from before evaluating @scheme[rhs].}

@deftogether[(
@defstruct+[(def-syntaxes form) ([ids (listof toplevel?)]
                                 [rhs (or/c expr? seq? indirect? any/c)]
                                 [prefix prefix?]
                                 [max-let-depth nonnegative-exact-integer?])]
@defstruct+[(def-for-syntax form) ([ids (listof toplevel?)]
                                   [rhs (or/c expr? seq? indirect? any/c)]
                                   [prefix prefix?]
                                   [max-let-depth nonnegative-exact-integer?])]
)]{

Represents a @scheme[define-syntaxes] or
@scheme[define-values-for-syntax] form. The @scheme[rhs] expression
has its own @scheme[prefix], which is pushed before evaluating
@scheme[rhs]; the stack is restored after obtaining the result
values. The @scheme[max-let-depth] field indicates the maximum size of
the stack that will be created by @scheme[rhs] (not counting
@scheme[prefix]).}

@defstruct+[(req form) ([reqs (listof module-path?)]
                        [dummy toplevel?])]{

Represents a top-level @scheme[require] form (but not one in a
@scheme[module] form). The @scheme[dummy] variable is used to access
to the top-level namespace.}


@defstruct+[(mod form) ([name symbol?]
                        [self-modidx module-path-index?]
                        [prefix prefix?]
                        [provides (listof symbol?)]
                        [requires (listof (cons/c (or/c exact-integer? #f) 
                                                  (listof module-path-index?)))]
                        [body (listof (or/c form? indirect? any/c))]
                        [syntax-body (listof (or/c def-syntaxes? def-for-syntax?))]
                        [max-let-depth exact-nonnegative-integer?])]{

Represents a @scheme[module] declaration. The @scheme[body] forms use
@scheme[prefix], rather than any prefix in place for the module
declaration itself (and each @scheme[syntax-body] has its own
prefix). The @scheme[body] field contains the module's run-time code,
and @scheme[syntax-body] contains the module's compile-time code. The
@scheme[max-let-depth] field indicates the maximum stack depth created
by @scheme[body] forms (not counting the @scheme[prefix] array).

After each form in @scheme[body] is evaluated, the stack is restored
to its depth from before evaluating the form.}


@defstruct+[(seq form) ([forms (listof (or/c form? indirect? any/c))])]{

Represents a @scheme[begin] form, either as an expression or at the
top level (though the latter is more commonly a @scheme[splice] form).
When a @scheme[seq] appears in an expression position, its
@scheme[forms] are expressions.

After each form in @scheme[forms] is evaluated, the stack is restored
to its depth from before evaluating the form.}


@defstruct+[(splice form) ([forms (listof (or/c form? indirect? any/c))])]{

Represents a top-level @scheme[begin] form where each evaluation is
wrapped with a continuation prompt.

After each form in @scheme[forms] is evaluated, the stack is restored
to its depth from before evaluating the form.}


@; --------------------------------------------------
@section{Expressions}

@defstruct+[(expr form) ()]{

A supertype for all expression forms that can appear in compiled code,
except for literals that are represented as themselves,
@scheme[indirect] structures to create cycles, and some @scheme[seq]
structures (which can appear as an expression as long as it contains
only other things that can be expressions).}


@defstruct+[(lam expr) ([name (or/c symbol? vector?)]
                        [flags exact-integer?]
                        [num-params exact-nonnegative-integer?]
                        [rest? boolean?]
                        [closure-map (vectorof exact-nonnegative-integer?)]
                        [max-let-depth exact-nonnegative-integer?]
                        [body (or/c expr? seq? indirect? any/c)])]{

Represents a @scheme[lambda] form. The @scheme[name] field is a name
for debugging purposes. The @scheme[num-params] field indicates the
number of arguments accepted by the procedure, not counting a rest
argument; the @scheme[rest?] field indicates whether extra arguments
are accepted and collected into a ``rest'' variable. The
@scheme[closure-map] field is a vector of stack positions that are
captured when evaluating the @scheme[lambda] form to create a closure.

When the function is called, the rest-argument list (if any) is pushed
onto the stack, then the normal arguments in reverse order, then the
closure-captured values in reverse order. Thus, when @scheme[body] is
run, the first value on the stack is the first value captured by the
@scheme[closure-map] array, and so on.

The @scheme[max-let-depth] field indicates the maximum stack depth
created by @scheme[body] (not including arguments and closure-captured
values pushed onto the stack). The @scheme[body] field is the
expression for the closure's body.}


@defstruct+[(closure expr) ([code lam?] [gen-id symbol?])]{

A @scheme[lambda] form with an empty closure, which is a procedure
constant. The procedure constant can appear multiple times in the
graph of expressions for bytecode, and the @scheme[code] field can
refer back to the same @scheme[closure] through an @scheme[indirect]
for a recursive constant procedure; the @scheme[gen-id] is different
for each such constant.}


@defstruct[indirect ([v closure?]) #:mutable #:prefab]{

An indirection used in expression positions to form cycles.}


@defstruct+[(case-lam expr) ([name (or/c symbol? vector?)]
                             [clauses (listof lam?)])]{

Represents a @scheme[case-lambda] form as a combination of
@scheme[lambda] forms that are tried (in order) based on the number of
arguments given.}


@defstruct+[(let-one expr) ([rhs (or/c expr? seq? indirect? any/c)]
                            [body (or/c expr? seq? indirect? any/c)])]{

Pushes an uninitialized slot onto the stack, evaluates @scheme[rhs]
and puts its value into the slot, and then runs @scheme[body].

After @scheme[rhs] is evaluated, the stack is restored to its depth
from before evaluating @scheme[rhs]. Note that the new slot is created
before evaluating @scheme[rhs].}


@defstruct+[(let-void expr) ([count nonnegative-exact-integer?]
                             [boxes? boolean?]
                             [body (or/c expr? seq? indirect? any/c)])]{

Pushes @scheme[count] uninitialized slots onto the stack and then runs
@scheme[body]. If @scheme[boxes?] is @scheme[#t], then the slots are
filled with boxes that contain @|undefined-const|.}


@defstruct+[(install-value expr) ([count nonnegative-exact-integer?]
                                  [pos nonnegative-exact-integer?]
                                  [boxes? boolean?]
                                  [rhs (or/c expr? seq? indirect? any/c)]
                                  [body (or/c expr? seq? indirect? any/c)])]{

Runs @scheme[rhs] to obtain @scheme[count] results, and installs them
into existing slots on the stack in order, skipping the first
@scheme[pos] stack positions. If @scheme[boxes?] is @scheme[#t], then
the values are put into existing boxes in the stack slots.

After @scheme[rhs] is evaluated, the stack is restored to its depth
from before evaluating @scheme[rhs].}


@defstruct+[(let-rec expr) ([procs (listof lam?)]
                            [body (or/c expr? seq? indirect? any/c)])]{

Represents a @scheme[letrec] form with @scheme[lambda] bindings. It
allocates a closure shell for each @scheme[lambda] form in
@scheme[procs], pushes them onto the stack in reverse order, fills out
each shell's closure using the created shells, and then evaluates
@scheme[body].}


@defstruct+[(boxenv expr) ([pos nonnegative-exact-integer?]
                           [body (or/c expr? seq? indirect? any/c)])]{

Skips @scheme[pos] elements of the stack, setting the slot afterward
to a new box containing the slot's old value, and then runs
@scheme[body]. This form appears when a @scheme[lambda] argument is
mutated using @scheme[set!] within its body; calling the function
initially pushes the value directly on the stack, and this form boxes
the value so that it can be mutated later.}


@defstruct+[(localref expr) ([unbox? boolean?]
                             [pos nonnegative-exact-integer?]
                             [clear? boolean?])]{

Represents a local-variable reference; it accesses the value in the
stack slot after the first @scheme[pos] slots. If @scheme[unbox?]  is
@scheme[#t], the stack slot contains a box, and a value is extracted
from the box. If @scheme[clear?] is @scheme[#t], then after the value
is obtained, the stack slot is cleared (to avoid retaining a reference
that can prevent reclamation of the value as garbage).}


@defstruct+[(toplevel expr) ([depth nonnegative-exact-integer?]
                             [pos nonnegative-exact-integer?]
                             [const? boolean?]
                             [ready? boolean?])]{

Represents a reference to a top-level or imported variable via the
@scheme[prefix] array. The @scheme[depth] field indicates the number
of stack slots to skip to reach the prefix array, and @scheme[pos] is
the offset into the array.

If @scheme[const?] is @scheme[#t], then the variable definitely will
be defined, and its value stays constant. If @scheme[ready?] is
@scheme[#t], then the variable definitely will be defined (but its
value might change in the future). If @scheme[const?] and
@scheme[ready?] are both @scheme[#f], then a check is needed to
determine whether the variable is defined.}


@defstruct+[(topsyntax expr) ([depth nonnegative-exact-integer?]
                              [pos nonnegative-exact-integer?]
                              [midpt nonnegative-exact-integer?])]{

Represents a reference to a quoted syntax object via the
@scheme[prefix] array. The @scheme[depth] field indicates the number
of stack slots to skip to reach the prefix array, and @scheme[pos] is
the offset into the array. The @scheme[midpt] value is used internally
for lazy calculation of syntax information.}


@defstruct+[(application expr) ([rator (or/c expr? seq? indirect? any/c)]
                                [rands (listof (or/c expr? seq? indirect? any/c))])]{

Represents a function call. The @scheme[rator] field is the expression
for the function, and @scheme[rands] are the argument
expressions. Before any of the expressions are evaluated,
@scheme[(length rands)] uninitialized stack slots are created (to be
used as temporary space).}


@defstruct+[(branch expr) ([test (or/c expr? seq? indirect? any/c)]
                           [then (or/c expr? seq? indirect? any/c)]
                           [else (or/c expr? seq? indirect? any/c)])]{

Represents an @scheme[if] form.

After @scheme[test] is evaluated, the stack is restored to its depth
from before evaluating @scheme[test].}


@defstruct+[(with-cont-mark expr) ([key (or/c expr? seq? indirect? any/c)]
                                   [val (or/c expr? seq? indirect? any/c)]
                                   [body (or/c expr? seq? indirect? any/c)])]{

Represents a @scheme[with-continuation-mark] expression. 

After each of @scheme[key] and @scheme[val] is evaluated, the stack is
restored to its depth from before evaluating @scheme[key] or
@scheme[val].}

@defstruct+[(beg0 expr) ([seq (listof (or/c expr? seq? indirect? any/c))])]{

Represents a @scheme[begin0] expression.

After each expression in @scheme[seq] is evaluated, the stack is
restored to its depth from before evaluating the expression.}


@defstruct+[(varref expr) ([toplevel toplevel?])]{

Represents a @scheme[#%variable-reference] form.}


@defstruct+[(assign expr) ([id toplevel?]
                           [rhs (or/c expr? seq? indirect? any/c)]
                           [undef-ok? boolean?])]{

Represents a @scheme[set!] expression that assigns to a top-level or
module-level variable. (Assignments to local variables are represented
by @scheme[install-value] expressions.)

After @scheme[rhs] is evaluated, the stack is restored to its depth
from before evaluating @scheme[rhs].}


@defstruct+[(apply-values expr) ([proc (or/c expr? seq? indirect? any/c)]
                                 [args-expr (or/c expr? seq? indirect? any/c)])]{

Represents @scheme[(call-with-values (lambda () args-expr) proc)],
which is handled specially by the run-time system.}


@defstruct+[(primval expr) ([id symbol?])]{

Represents a direct reference to a variable imported from the run-time
kernel.}

@; --------------------------------------------------
@section{Syntax Objects}

@defstruct+[wrapped ([datum any/c]
                     [wraps (listof wrap?)]
                     [certs list?])]{

Represents a syntax object, where @scheme[wraps] contain the lexical
information and @scheme[certs] is certificate information. When the
@scheme[datum] part is itself compound, its pieces are wrapped, too.}


@defstruct+[wrap ()]{

A supertype for lexical-information elements.}


@defstruct+[(lexical-rename wrap) ([alist (listof (cons/c identifier? identifier?))])]{

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
                             [prefix symbol?])]{

Represents a set of simple imports from one module within a
@scheme[module-rename].}

@defstruct+[module-binding ([path module-path-index?]
                            [mod-phase (or/c exact-integer? #f)]
                            [import-phase (or/c exact-integer? #f)]
                            [id symbol?]
                            [nominal-path module-path-index?]
                            [nominal-phase (or/c exact-integer? #f)]
                            [nominal-id (or/c exact-integer? #f)])]{

Represents a single identifier import (i.e., the general case) within
a @scheme[module-rename].}
