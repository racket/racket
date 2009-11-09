#lang scribble/manual
@(require scribble/struct
          scribble/decode
          scribble/eval
          (for-label scheme/base
                     scheme/contract
                     unstable/syntax))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/syntax))
@(the-eval '(require (for-syntax scheme/base unstable/syntax)))

@title[#:tag "syntax"]{Syntax}

@defmodule[unstable/syntax]

@defparam[current-syntax-context stx (or/c syntax? false/c)]{

The current contextual syntax object, defaulting to @scheme[#f].  It
determines the special form name that prefixes syntax errors created
by @scheme[wrong-syntax].

@;{
If it is a syntax object with a @scheme['report-error-as] syntax
property whose value is a symbol, then that symbol is used as the
special form name. Otherwise, the same rules apply as in
@scheme[raise-syntax-error].
}

}

@defproc[(wrong-syntax [stx syntax?] [format-string string?] [v any/c] ...)
         any]{

Raises a syntax error using the result of
@scheme[(current-syntax-context)] as the ``major'' syntax object and
the provided @scheme[stx] as the specific syntax object. (The latter,
@scheme[stx], is usually the one highlighted by DrScheme.) The error
message is constructed using the format string and arguments, and it
is prefixed with the special form name as described under
@scheme[current-syntax-context].

@examples[#:eval the-eval
(wrong-syntax #'here "expected ~s" 'there)
(parameterize ((current-syntax-context #'(look over here)))
  (wrong-syntax #'here "expected ~s" 'there))
]

A macro using @scheme[wrong-syntax] might set the syntax context at the very
beginning of its transformation as follows:
@SCHEMEBLOCK[
(define-syntax (my-macro stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      ___)))
]
Then any calls to @scheme[wrong-syntax] during the macro's
transformation will refer to @scheme[my-macro] (more precisely, the name that
referred to @scheme[my-macro] where the macro was used, which may be
different due to renaming, prefixing, etc).

@;{
A macro that expands into a helper macro can insert its own name into
syntax errors raised by the helper macro by installing a
@scheme['report-error-as] syntax property on the helper macro
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

@defform[(define-pattern-variable id expr)]{

Evaluates @scheme[expr] and binds it to @scheme[id] as a pattern
variable, so @scheme[id] can be used in subsequent @scheme[syntax]
patterns.

@examples[#:eval the-eval
  (define-pattern-variable name #'Alice)
  #'(hello name)
]

}

@;{----}

@defform[(with-temporaries (temp-id ...) . body)]{

Evaluates @scheme[body] with each @scheme[temp-id] bound as a pattern
variable to a freshly generated identifier.

@examples[#:eval the-eval
  (with-temporaries (x) #'(lambda (x) x))
]

}

@defproc[(generate-temporary [name-base any/c 'g]) identifier?]{

Generates one fresh identifier. Singular form of
@scheme[generate-temporaries]. If @scheme[name-base] is supplied, it
is used as the basis for the identifier's name.

}

@defproc[(generate-n-temporaries [n exact-nonnegative-integer?])
         (listof identifier?)]{

Generates a list of @scheme[n] fresh identifiers.

}

@;{----}

@defparam[current-caught-disappeared-uses ids
          (or/c (listof identifier?) false/c)]{

Parameter for tracking disappeared uses. Tracking is ``enabled'' when
the parameter has a non-false value. This is done automatically by
forms like @scheme[with-disappeared-uses].

}

@defform[(with-disappeared-uses stx-expr)
         #:contracts ([stx-expr syntax?])]{

Evaluates the @scheme[stx-expr], catching identifiers looked up using
@scheme[syntax-local-value/catch]. Adds the caught identifiers to the
@scheme['disappeared-uses] syntax property of the resulting syntax
object.

}

@defform[(with-catching-disappeared-uses body-expr)]{

Evaluates the @scheme[body-expr], catching identifiers looked up using
@scheme[syntax-local-value/catch]. Returns two values: the result of
@scheme[body-expr] and the list of caught identifiers.

}

@defproc[(syntax-local-value/catch [id identifier?] [predicate (-> any/c boolean?)])
         any/c]{

Looks up @scheme[id] in the syntactic environment (as
@scheme[syntax-local-value]). If the lookup succeeds and returns a
value satisfying the predicate, the value is returned and @scheme[id]
is recorded (``caught'') as a disappeared use. If the lookup fails or
if the value does not satisfy the predicate, @scheme[#f] is returned
and the identifier is not recorded as a disappeared use.

If not used within the extent of a @scheme[with-disappeared-uses] form
or similar, has no effect.

}

@defproc[(record-disappeared-uses [ids (listof identifier?)])
         void?]{

Add @scheme[ids] to the current disappeared uses.

If not used within the extent of a @scheme[with-disappeared-uses] form
or similar, has no effect.

}

@;{----}

@defproc[(format-symbol [fmt string?] [v any/c] ...)
         symbol?]{

Like @scheme[format], but produces a symbol.

@examples[#:eval the-eval
  (format-symbol "make-~s" 'triple)
]
}

@defproc[(format-id [lctx (or/c syntax? #f)]
                    [#:source src (or/c syntax? #f) #f]
                    [#:props props (or/c syntax? #f) #f]
                    [#:cert cert (or/c syntax? #f) #f]
                    [fmt string?] [v any/c] ...)
         identifier?]{

Like @scheme[format-symbol], but converts the symbol into an
identifier using @scheme[lctx] for the lexical context, @scheme[src]
for the source location, @scheme[props] for the properties, and
@scheme[cert] for the inactive certificates. (See
@scheme[datum->syntax].)

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

(Scribble doesn't show it, but the DrScheme pinpoints the location of
the second error but not of the first.)
}
