#lang scribble/manual
@(require scribble/struct
          scribble/decode
          scribble/eval
	  "utils.rkt"
          (for-label racket/base
                     racket/contract
                     syntax/kerncase
                     unstable/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/syntax))
@(the-eval '(require (for-syntax racket/base unstable/syntax)))

@title[#:tag "syntax"]{Syntax}

@defmodule[unstable/syntax]

@unstable[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defparam[current-syntax-context stx (or/c syntax? false/c)]{

The current contextual syntax object, defaulting to @racket[#f].  It
determines the special form name that prefixes syntax errors created
by @racket[wrong-syntax].

@;{
If it is a syntax object with a @racket['report-error-as] syntax
property whose value is a symbol, then that symbol is used as the
special form name. Otherwise, the same rules apply as in
@racket[raise-syntax-error].
}

}

@defproc[(wrong-syntax [stx syntax?] [format-string string?] [v any/c] ...)
         any]{

Raises a syntax error using the result of
@racket[(current-syntax-context)] as the ``major'' syntax object and
the provided @racket[stx] as the specific syntax object. (The latter,
@racket[stx], is usually the one highlighted by DrRacket.) The error
message is constructed using the format string and arguments, and it
is prefixed with the special form name as described under
@racket[current-syntax-context].

@examples[#:eval the-eval
(wrong-syntax #'here "expected ~s" 'there)
(parameterize ((current-syntax-context #'(look over here)))
  (wrong-syntax #'here "expected ~s" 'there))
]

A macro using @racket[wrong-syntax] might set the syntax context at the very
beginning of its transformation as follows:
@RACKETBLOCK[
(define-syntax (my-macro stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      ___)))
]
Then any calls to @racket[wrong-syntax] during the macro's
transformation will refer to @racket[my-macro] (more precisely, the name that
referred to @racket[my-macro] where the macro was used, which may be
different due to renaming, prefixing, etc).

@;{
A macro that expands into a helper macro can insert its own name into
syntax errors raised by the helper macro by installing a
@racket['report-error-as] syntax property on the helper macro
expression.

@examples[#:eval the-eval
(define-syntax (public-macro stx)
  (syntax-case stx ()
    [(public-macro stuff)
     (syntax-property #'(private-macro stuff)
                      'report-error-as
                      (syntax-e #'public-macro))]))
(define-syntax (private-macro stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(private-macro arg)
       (wrong-syntax #'arg "just no good")])))
(public-macro 5)
]
}

}

@;{----}

@defform[(define/with-syntax pattern expr)]{

Definition form of @racket[with-syntax]. That is, it matches the
syntax object result of @racket[expr] against @racket[pattern] and
creates pattern variable definitions for the pattern variables of
@racket[pattern].

@examples[#:eval the-eval
(define/with-syntax (px ...) #'(a b c))
(define/with-syntax (tmp ...) (generate-temporaries #'(px ...)))
#'([tmp px] ...)
]
}

@defform[(define-pattern-variable id expr)]{

Evaluates @racket[expr] and binds it to @racket[id] as a pattern
variable, so @racket[id] can be used in subsequent @racket[syntax]
patterns.

@examples[#:eval the-eval
  (define-pattern-variable name #'Alice)
  #'(hello name)
]

}

@;{----}

@defform[(with-temporaries (temp-id ...) . body)]{

Evaluates @racket[body] with each @racket[temp-id] bound as a pattern
variable to a freshly generated identifier.

@examples[#:eval the-eval
  (with-temporaries (x) #'(lambda (x) x))
]

}

@defproc[(generate-temporary [name-base any/c 'g]) identifier?]{

Generates one fresh identifier. Singular form of
@racket[generate-temporaries]. If @racket[name-base] is supplied, it
is used as the basis for the identifier's name.

}

@defproc[(generate-n-temporaries [n exact-nonnegative-integer?])
         (listof identifier?)]{

Generates a list of @racket[n] fresh identifiers.

}

@;{----}

@defparam[current-recorded-disappeared-uses ids
          (or/c (listof identifier?) false/c)]{

Parameter for tracking disappeared uses. Tracking is ``enabled'' when
the parameter has a non-false value. This is done automatically by
forms like @racket[with-disappeared-uses].

}

@defform[(with-disappeared-uses stx-expr)
         #:contracts ([stx-expr syntax?])]{

Evaluates the @racket[stx-expr], catching identifiers looked up using
@racket[syntax-local-value/catch]. Adds the caught identifiers to the
@racket['disappeared-uses] syntax property of the resulting syntax
object.

}

@defproc[(syntax-local-value/record [id identifier?] [predicate (-> any/c boolean?)])
         any/c]{

Looks up @racket[id] in the syntactic environment (as
@racket[syntax-local-value]). If the lookup succeeds and returns a
value satisfying the predicate, the value is returned and @racket[id]
is recorded as a disappeared use. If the lookup fails or if the value
does not satisfy the predicate, @racket[#f] is returned and the
identifier is not recorded as a disappeared use.

}

@defproc[(record-disappeared-uses [ids (listof identifier?)])
         void?]{

Add @racket[ids] to @racket[(current-recorded-disappeared-uses)].

If not used within the extent of a @racket[with-disappeared-uses] form
or similar, has no effect.

}

@;{----}

@defproc[(format-symbol [fmt string?]
                        [v (or/c string? symbol? identifier? keyword? char? number?)] ...)
         symbol?]{

Like @racket[format], but produces a symbol. The format string must
use only @litchar{~a} placeholders. Identifiers in the argument list
are automatically converted to symbols.

@examples[#:eval the-eval
  (format-symbol "make-~a" 'triple)
]
}

@defproc[(format-id [lctx (or/c syntax? #f)]
                    [#:source src (or/c syntax? #f) #f]
                    [#:props props (or/c syntax? #f) #f]
                    [#:cert cert (or/c syntax? #f) #f]
                    [fmt string?]
                    [v (or/c string? symbol? identifier? keyword? char? number?)] ...)
         identifier?]{

Like @racket[format-symbol], but converts the symbol into an
identifier using @racket[lctx] for the lexical context, @racket[src]
for the source location, @racket[props] for the properties, and
@racket[cert] for the inactive certificates. (See
@racket[datum->syntax].)

The format string must use only @litchar{~a} placeholders. Identifiers
in the argument list are automatically converted to symbols.

@examples[#:eval the-eval
(define-syntax (make-pred stx)
  (syntax-case stx ()
    [(make-pred name)
     (format-id #'name "~a?" (syntax-e #'name))]))
(make-pred pair)
(make-pred none-such)
(define-syntax (better-make-pred stx)
  (syntax-case stx ()
    [(better-make-pred name)
     (format-id #'name #:source #'name
                "~a?" (syntax-e #'name))]))
(better-make-pred none-such)
]

(Scribble doesn't show it, but the DrRacket pinpoints the location of
the second error but not of the first.)
}

@defproc[(internal-definition-context-apply [intdef-ctx internal-definition-context?]
                                            [stx syntax?])
         syntax?]{

Applies the renamings of @racket[intdef-ctx] to @racket[stx].

}

@defproc[(syntax-local-eval [stx syntax?]
                            [intdef-ctx (or/c internal-definition-context? #f) #f])
         any]{

Evaluates @racket[stx] as an expression in the current transformer
environment (that is, at phase level 1), optionally extended with
@racket[intdef-ctx].

@examples[#:eval the-eval
(define-syntax (show-me stx)
  (syntax-case stx ()
    [(show-me expr)
     (begin
       (printf "at compile time produces ~s\n"
               (syntax-local-eval #'expr))
       #'(printf "at run time produes ~s\n"
                 expr))]))
(show-me (+ 2 5))
(define-for-syntax fruit 'apple)
(define fruit 'pear)
(show-me fruit)
#|
(define-syntax (show-me* stx)
  (syntax-case stx ()
    [(show-me expr1)
     (call-with-values (lambda () (syntax-local-eval #'expr1))
                       (lambda vals
                         (with-syntax ([vals vals])
                           #'(quote vals))))]))
(define-for-syntax (sum-and-difference a b)
  (values (+ a b) (- a b)))
(show-me* (sum-and-difference 12 9))
|#
]
}

@addition{Sam Tobin-Hochstadt}
                     
@defform[(with-syntax* ([pattern stx-expr] ...)
           body ...+)]{
Similar to @racket[with-syntax], but the pattern variables are bound in the remaining
@racket[stx-expr]s as well as the @racket[body]s, and the @racket[pattern]s need not 
bind distinct pattern variables; later bindings shadow earlier bindings.

@examples[#:eval the-eval
(with-syntax* ([(x y) (list #'val1 #'val2)]
               [nest #'((x) (y))])
  #'nest)
]
}

@defproc[(syntax-map [f (-> syntax? A)] [stxl syntax?] ...) (listof A)]{
Performs @racket[(map f (syntax->list stxl) ...)].

@examples[#:eval the-eval
(syntax-map syntax-e #'(a b c))]
}

@addition[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defform[(syntax-list template ...)]{

This form constructs a list of syntax objects based on the given templates.  It
is equivalent to @scheme[(syntax->list (syntax (template ...)))].

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(with-syntax ([(x ...) (syntax (1 2 3))]) (syntax-list x ...))
]

}

@defproc[(to-syntax [datum any/c]
                    [#:stx stx (or/c false/c syntax?) #f]
                    [#:src src src/c stx]
                    [#:ctxt ctxt (or/c false/c syntax?) stx]
                    [#:prop prop (or/c false/c syntax?) stx]
                    [#:cert cert (or/c false/c syntax?) stx])
         syntax?]{

A wrapper for @scheme[datum->syntax] with keyword arguments.

The "master" keyword @scheme[#:stx] sets all attributes from a single syntax
object, defaulting to @scheme[#f] for unadorned syntax objects.

The individual keywords @scheme[#:src], @scheme[#:ctxt], @scheme[#:prop], and
@scheme[#:cert] override @scheme[#:stx] for individual syntax object
attributes.  They control source src information, lexical context
information, syntax object properties, and syntax certificates, respectively.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(define blank-stx (to-syntax 'car))
blank-stx
(syntax-e blank-stx)
(free-identifier=? blank-stx #'car)
(define full-stx (to-syntax 'car #:stx #'here))
full-stx
(syntax-e full-stx)
(free-identifier=? full-stx #'car)
(define partial-stx (to-syntax 'car #:ctxt #'here))
partial-stx
(syntax-e partial-stx)
(free-identifier=? partial-stx #'car)
]

}

@section{Syntax Object Source Locations}

@deftogether[(
@defproc[(syntax-source-directory [stx syntax?]) (or/c path? #f)]
@defproc[(syntax-source-file-name [stx syntax?]) (or/c path? #f)]
)]{

These produce the directory and file name, respectively, of the path with which
@scheme[stx] is associated, or @scheme[#f] if @scheme[stx] is not associated
with a path.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(define loc
  (list (build-path "/tmp" "dir" "somewhere.ss")
        #f #f #f #f))
(define stx1 (datum->syntax #f 'somewhere loc))
(syntax-source-directory stx1)
(syntax-source-file-name stx1)
(define stx2 (datum->syntax #f 'nowhere #f))
(syntax-source-directory stx2)
(syntax-source-directory stx2)
]

}

@section{Macro Transformers}

@defproc[(redirect-transformer [id identifier?]) (-> syntax? syntax?)]{

Constructs a function that behaves like a rename transformer; it does not
cooperate with @scheme[syntax-local-value] like a rename transformer does, but
unlike a rename transformer it may be used as a function to transform a syntax
object referring to one identifier into a syntax object referring to another.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
((redirect-transformer #'x) #'a)
((redirect-transformer #'y) #'(a b c))
]

}

@defproc[(head-expand [stx syntax?]
                      [stop-list (listof identifier?) null]
                      [intdef-ctx (or/c internal-definitions-context?
                                        (non-empty-listof internal-definitions-context?)
                                        #f)])
         syntax?]{

This function performs head expansion on @scheme[stx].  In other words, it uses
@scheme[local-expand] to expand @scheme[stx] until its head identifier is a core
form (a member of @scheme[(kernel-form-identifier-list)]) or a member of
@scheme[stop-list], or until it can not be expanded further (e.g. due to error).

It is equivalent to @scheme[(local-expand stx (syntax-local-context) (append
stop-ids (kernel-form-identifier-list) intdef-ctx))].

}

@defproc[(trampoline-transformer
          [f (-> (-> syntax? void?) (-> syntax? syntax?) syntax? syntax?)])
         (-> syntax? syntax?)]{

Produces a transformer that can emit multiple results during macro expansion, to
be spliced together via @scheme[begin].  This can be useful for compound
expansion that relies on transformer definitions, as well as on expansion state
that is difficult to marshall.

Specifically, @scheme[f] is invoked with three arguments.  The first is the
function used to emit intermediate results (other than the last one).  The
second applies the @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{syntax mark} used for the entire
expansion; @scheme[syntax-local-introduce] will not be reliable during this
process.  The third is the syntax object to expand.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(define-syntax magic-begin
  (trampoline-transformer
   (lambda (emit intro stx)
     (syntax-case stx ()
       [(_ term ...)
        (let loop ([terms (syntax->list #'(term ...))])
          (cond
           [(null? terms) #'(begin)]
           [(null? (cdr terms)) (car terms)]
           [else
            (printf "Presto: ~s!\n"
                    (syntax->datum (car terms)))
            (emit (car terms))
            (loop (cdr terms))]))]))))
(magic-begin
 (define x 1)
 (define y 2)
 (+ x y))
]

}

@defproc[(quote-transformer [x any/c]) syntax?]{

Produces a syntax object representing an expression that reconstructs @scheme[x]
when executed, including faithfully reconstructing any syntax objects contained
in @scheme[x].  Note that @scheme[quote] normally converts syntax objects to
non-syntax data, and @scheme[quote-syntax] does the opposite.

@defexamples[
#:eval (eval/require '(for-syntax racket/base unstable/syntax) 'unstable/syntax)
(define-for-syntax x (list 1 #'(2 3) 4))
(define-syntax (the-many-faces-of-x stx)
  (with-syntax ([x x] [qx (quote-transformer x)])
   #'(list (quote x)
           (quote-syntax x)
           qx)))
(the-many-faces-of-x)
]

}
