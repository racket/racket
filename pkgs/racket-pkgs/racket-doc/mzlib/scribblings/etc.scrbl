#lang scribble/doc
@(require "common.rkt"
          scribble/eval
          (for-label mzlib/etc
                     scheme/bool
                     scheme/local
                     setup/dirs
                     racket/block
                     (only-in scheme build-list build-string build-vector
                              symbol=?)))

@(define etc-eval (make-base-eval))
@interaction-eval[#:eval etc-eval (require mzlib/etc)]

@(begin
   (define-syntax-rule (bind id else-id)
     (begin
       (require (for-label scheme/base))
       (define id (racket lambda))
       (define else-id (racket else))))
   (bind base-lambda base-else))

@mzlib[#:mode title etc]

The @racketmodname[mzlib/etc] library re-exports the following from
@racketmodname[scheme/base] and other libraries:

@racketblock[
  boolean=?
  true
  false
  build-list
  build-string
  build-vector
  compose
  local
  symbol=?
  nand 
  nor
]

@defform[(begin-lifted expr ...+)]

Lifts the @racket[expr]s so that they are evaluated once at the ``top
level'' of the current context, and the result of the last
@racket[expr] is used for every evaluation of the
@racket[begin-lifted] form.

When this form is used as a run-time expression within a module, the
``top level'' corresponds to the module's top level, so that each
@racket[expr] is evaluated once for each invocation of the
module. When it is used as a run-time expression outside of a module,
the ``top level'' corresponds to the true top level. When this form is
used in a @racket[define-syntax], @racket[letrec-syntax],
etc. binding, the ``top level'' corresponds to the beginning of the
binding's right-hand side. Other forms may redefine ``top level''
(using @racket[local-expand/capture-lifts]) for the expressions that
they enclose.

@defform[(begin-with-definitions defn-or-expr ...)]{

The same as @racket[(block defn-or-expr ...)].}

@defform[(define-syntax-set (id ...) defn ...)]{

Similar to @racket[define-syntaxes], but instead of a single body
expression, a sequence of definitions follows the sequence of defined
identifiers. For each @racket[identifier], the @racket[defn]s should
include a definition for @racket[id]@racketidfont{/proc}.  The value
for @racket[id]@racketidfont{/proc} is used as the (expansion-time)
value for @racket[id].

The @racket[define-syntax-set] form is useful for defining a set of
syntax transformers that share helper functions, though
@racket[begin-for-syntax] now serves essentially the same purposes.

@as-examples[
@racketblock[
(define-syntax-set (let-current-continuation 
                    let-current-escape-continuation)
  (define (mk call-id)
    (lambda (stx)
      (syntax-case stx ()
        [(_ id body1 body ...) 
         (with-syntax ([call call-id])
           (syntax (call (lambda (id) body1 body ...))))])))
  (define let-current-continuation/proc
    (mk (quote-syntax call/cc)))
  (define let-current-escape-continuation/proc
    (mk (quote-syntax call/ec))))
]]}


@defform*[#:literals (else)
          [(evcase key-expr (value-expr body-expr ...) ...+)
           (evcase key-expr (value-expr body-expr ...) ... [else body-expr ...])]]{

The @racket[evcase] form is similar to @racket[case], except that
expressions are provided in each clause instead of a sequence of
data. After @racket[key-expr] is evaluated, each @racket[value-expr]
is evaluated until a value is found that is @racket[eqv?] to the key
value; when a matching value is found, the corresponding
@racket[body-expr]s are evaluated and the value(s) for the last is the
result of the entire @racket[evcase] expression.

The @racket[else] literal is recognized either as unbound (like in the
@racketmodname[mzscheme] language) or bound as @|base-else| from
@racketmodname[scheme/base].}


@defproc[(identity [v any/c]) any/c]{

Returns @racket[v].}


@defform/subs[#:literals (val rec vals recs _ values)
              (let+ clause body-expr ...+)
              ([clause (val target expr)
                       (rec target expr)
                       (vals (target ...) expr)
                       (recs (target expr) ...)
                       (_ expr ...)]
               [target id
                       (values id ...)])]{

A binding construct that specifies scoping on a per-binding basis
instead of a per-expression basis.  It helps eliminate rightward-drift
in programs.  It looks similar to @racket[let], except each clause has
an additional keyword tag before the binding variables.

Each @racket[clause] has one of the following forms:

@itemize[

 @item{@racket[(val target expr)] : Binds @racket[target]
 non-recursively to @racket[expr].}

 @item{@racket[(rec target expr)] : Binds @racket[target] recursively to
 @racket[expr].}

 @item{@racket[(vals (target expr) ...)] : The @racket[target]s are
 bound to the @racket[expr]s. The environment of the @racket[expr]s is
 the environment active before this clause.}

 @item{@racket[(recs (target expr) ...)] : The @racket[targets]s are
 bound to the @racket[expr]s. The environment of the @racket[expr]s
 includes all of the @racket[targets]s.}

 @item{@racket[(_ expr ...)] : Evaluates the @racket[expr]s without
 binding any variables.}

]

The clauses bind left-to-right. When a @racket[target] is
@racket[(values id ...)], multiple values returned by the
corresponding expression are bound to the multiple variables.

@examples[
#:eval etc-eval
(let+ ([val (values x y) (values 1 2)])
  (list x y))

(let ([x 1])
  (let+ ([val x 3]
         [val y x])
     y))
]}


@defproc[(loop-until [start any/c] [done? (any/c . -> . any)]
                     [next (any/c . -> . any/c)]
                     [f (any/c . -> . any)])
         void?]{

Repeatedly invokes the @racket[f] procedure until the @racket[done?]
procedure returns @racket[#t]:

@racketblock[
  (define (loop-until start done? next f)
    (let loop ([i start])
      (unless (done? i)
        (f i)
        (loop (next i)))))
]}


@defproc[(namespace-defined? [sym symbol?]) boolean?]{

Returns @racket[#t] if @racket[namespace-variable-value] would return
a value for @racket[sym], @racket[#f] otherwise.}


@defform[(nand expr ...)]{

Same as @racket[(not (and expr ...))].}


@defform[(nor expr ...)]{

Same as @racket[(not (or expr ...))].}


@defform[(opt-lambda formals body ...+)]{

Supports optional (but not keyword) arguments like @base-lambda from
@racket[scheme/base].}


@defform[(recur id bindings body ...+)]{

Equivalent to @racket[(let id bindings body ...+)].}


@defform*[[(rec id value-expr)
           (rec (id arg-id ...) expr)
           (rec (id arg-id ... . rest-id) expr)]]{

Equivalent, respectively, to

@racketblock[
  (letrec ([id value-expr]) id)
  (letrec ([id (lambda (arg-id ...) value-expr)]) id)
  (letrec ([id (lambda (arg-id ... . rest-id) value-expr)]) id)
]}


@defform*[[(this-expression-source-directory)
           (this-expression-source-directory datum)]]{

@margin-note{See @racketmodname[scheme/runtime-path] for a definition form
that works better when creating executables.}

Expands to an expression that evaluates to the directory of the file
containing the source @racket[datum]. If @racket[datum] is not
supplied, then the entire @racket[(this-expression-source-directory)]
expression is used as @racket[datum].

If @racket[datum] has a source module, then the expansion attempts to
determine the module's run-time location. This location is determined
by preserving the lexical context of @racket[datum] in a syntax
object, extracting its source module path at run time, and then
resolving the module path.

Otherwise, @racket[datum]'s source file is determined through source
location information associated with @racket[datum], if it is
present. As a last resort, @racket[current-load-relative-directory] is
used if it is not @racket[#f], and @racket[current-directory] is used
if all else fails.

A directory path derived from source location is always stored in
bytes in the expanded code, unless the file is within the result of
@racket[find-collects-dir], in which case the expansion records the
path relative to @racket[(find-collects-dir)] and then reconstructs it
using @racket[(find-collects-dir)] at run time.}


@defform*[[(this-expression-file-name)
           (this-expression-file-name datum)]]{

Similar to @racket[this-expression-source-directory], except that only
source information associated with @racket[datum] or
@racket[(this-expression-file-name)] is used to extract a filename. If
no filename is available, the result is @racket[#f].}


@defform[#:literals (quote unsyntax scheme)
         (hash-table (#,(racket quote) flag) ... (key-expr val-expr) ...)]{

Creates a new hash-table providing the quoted flags (if any) to
@racket[make-hash-table], and then mapping each key to the
corresponding values.}


@close-eval[etc-eval]
