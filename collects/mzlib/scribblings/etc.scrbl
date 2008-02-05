#lang scribble/doc
@(require "common.ss"
          scribble/eval
          (for-label mzlib/etc
                     scheme/bool
                     scheme/local
                     setup/dirs
                     (only-in scheme build-list build-string build-vector
                              symbol=?)))

@(define etc-eval (make-base-eval))
@interaction-eval[#:eval etc-eval (require mzlib/etc)]

@(begin
  (define-syntax-rule (bind id)
    (begin
     (require scheme/base)
     (define id (scheme lambda))))
  (bind base-lambda))

@mzlib[#:mode title etc]

The @schememodname[mzlib/etc] library re-exports the following
@schememodname[scheme/base] and other libraries:

@schemeblock[
boolean=?
true
false
build-list
build-string
build-vector
compose
local
symbol=?
]

@defform[(begin-lifted expr ...+)]

Lifts the @scheme[expr]s so that they are evaluated once at the ``top
level'' of the current context, and the result of the last
@scheme[expr] is used for every evaluation of the
@scheme[begin-lifted] form.

When this form is used as a run-time expression within a module, the
``top level'' corresponds to the module's top level, so that each
@scheme[expr] is evaluated once for each invocation of the
module. When it is used as a run-time expression outside of a module,
the ``top level'' corresponds to the true top level. When this form is
used in a @scheme[define-syntax], @scheme[letrec-syntax],
etc. binding, the ``top level'' corresponds to the beginning of the
binding's right-hand side. Other forms may redefine ``top level''
(using @scheme[local-expand/capture-lifts]) for the expressions that
they enclose.


@defform[(begin-with-definitions defn-or-expr ...)]{

Supports a mixture of expressions and mutually recursive definitions,
much like a @scheme[module] body. Unlike in a @scheme[module],
however, syntax definitions cannot be used to generate other immediate
definitions (though they can be used for expressions).

The result of the @scheme[begin-with-definitions] form is the result
of the last @scheme[defn-or-expr] if it is an expression,
@|void-const| otherwise. If no @scheme[defn-or-expr] is provided
(after flattening @scheme[begin] forms), the result is @|void-const|.}



@defform[(define-syntax-set (id ...) defn ...)]{

Similar to @scheme[define-syntaxes], but instead of a single body
expression, a sequence of definitions follows the sequence of defined
identifiers. For each @scheme[identifier], the @scheme[defn]s should
include a definition for @scheme[id]@schemeidfont{/proc}.  The value
for @scheme[id]@schemeidfont{/proc} is used as the (expansion-time)
value for @scheme[id].

The @scheme[define-syntax-set] form is useful for defining a set of
syntax transformers that share helper functions, though
@scheme[begin-for-syntax] now serves essentially the same purposes.

@as-examples[
@schemeblock[
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

The @scheme[evcase] form is similar to @scheme[case], except that
expressions are provided in each clause instead of a sequence of
data. After @scheme[key-expr] is evaluated, each @scheme[value-expr]
is evaluated until a value is found that is @scheme[eqv?] to the key
value; when a matching value is found, the corresponding
@scheme[body-expr]s are evaluated and the value(s) for the last is the
result of the entire @scheme[evcase] expression.}


@defproc[(identity [v any/c]) any/c]{

Returns @scheme[v].}


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
in programs.  It looks similar to @scheme[let], except each clause has
an additional keyword tag before the binding variables.

Each @scheme[clause] has one of the following forms:

@itemize{

 @item{@scheme[(val target expr)] : Binds @scheme[target]
 non-recursively to @scheme[expr].}

 @item{@scheme[(rec target expr)] : Binds @scheme[target] recursively to
 @scheme[expr].}

 @item{@scheme[(vals (target expr) ...)] : The @scheme[target]s are
 bound to the @scheme[expr]s. The environment of the @scheme[expr]s is
 the environment active before this clause.}

 @item{@scheme[(recs (target expr) ...)] : The @scheme[targets]s are
 bound to the @scheme[expr]s. The environment of the @scheme[expr]s
 includes all of the @scheme[targets]s.}

 @item{@scheme[(_ expr ...)] : Evaluates the @scheme[expr]s without
 binding any variables.}

}

The clauses bind left-to-right. When a @scheme[target] is
@scheme[(values id ...)], multiple values returned by the
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


@defproc[(loop-until [start any/c][done? (any/c . -> . any)]
                     [next (any/c . -> . any/c)]
                     [f (any/c . -> . any)]) 
         void?]{

Repeatedly invokes the @scheme[f] procedure until the @scheme[done?]
procedure returns @scheme[#t]:

@schemeblock[
(define loop-until
  (lambda (start done? next f)
    (let loop ([i start])
      (unless (done? i)
        (f i)
        (loop (next i))))))
]}


@defproc[(namespace-defined? [sym symbol?]) boolean?]{

Returns @scheme[#t] if @scheme[namespace-variable-value] would return
a value for @scheme[sym], @scheme[#f] otherwise.}


@defform[(nand expr ...)]{

Same as @scheme[(not (and expr ...))].}


@defform[(nor expr ...)]{

Same as @scheme[(not (or expr ...))].}


@defform[(opt-lambda formals body ...+)]{

Supports optional (but not keyword) arguments like @base-lambda from
@scheme[scheme/base].}


@defform[(recur id bindings body ...+)]{

Equivalent to @scheme[(let id bindings body ...+)].}


@defform*[[(rec id value-expr)
           (rec (id arg-id ...) expr)
           (rec (id arg-id ... . rest-id) expr)]]{

Equivalent, respectively, to

@schemeblock[
(letrec ((id value-expr)) id)
(letrec ((id (lambda (arg-id ...) value-expr))) id)
(letrec ((id (lambda (arg-id ... . rest-id) value-expr))) id)
]}


@deftogether[(
@defform[(this-expression-source-directory)]
@defform[(this-expression-file-name)]
)]{

@margin-note{See @schememodname[scheme/runtime-path] for a definition form
that works better when creating executables.}

Expands to an expression that evaluates to the name of the directory
of the file containing the source expression, or the name of the file
containing the source expression.

If the expression has a source module, then the expansion attempts to
determine the module's run-time location. This location is determined
by preserving the original expression as a syntax object, extracting
its source module path at run time, and then resolving the module
path.

Otherwise, the source expression's file is determined through source
location information associated with the syntax, if it is present. If
the expression has no source, or if no directory can be determined at
run time, the expansion falls back to using source-location
information associated with the expression.

As a last resort, @scheme[#f] is used for the file name; for the
directory name, @scheme[current-load-relative-directory] is used if it
is not @scheme[#f], and @scheme[current-directory] is used if all else
fails.

A directory path is stored in bytes in the expanded code, unless the
file is within the result of @scheme[find-collects-dir], in which case
the expansion records the path relative to
@scheme[(find-collects-dir)] and then reconstructs it using
@scheme[(find-collects-dir)] at run time.}


@defform[#:literals (quote unsyntax scheme)
         (hash-table (#,(scheme quote) flag) ... (key-expr val-expr) ...)]{

Creates a new hash-table providing the quoted flags (if any) to
@scheme[make-hash-table], and them mapping each key to the
corresponding values.}
