#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          scheme/sandbox
          (for-label scheme/base
                     scheme/contract
                     scheme/dict
                     syntax/keyword))

@(begin
   (define the-eval
     (parameterize ((sandbox-output 'string)
                    (sandbox-error-output 'string))
       (make-evaluator 'scheme/base #:requires '(syntax/keyword))))
   ;;(void (the-eval '(error-print-source-location #f)))
   (define-syntax-rule (myexamples e ...)
     (parameterize (#|(error-print-source-location #f)|#)
       (examples #:eval the-eval e ...))))


@title[#:tag "stxkeyword"]{Helpers for Processing Keyword Syntax}

The @schememodname[syntax/keyword] module contains procedures for
parsing keyword options in macros.

@defmodule[syntax/keyword]

@schemegrammar[#, @deftech{keyword-table}
               (dict-of keyword (listof check-procedure))]

A keyword-table is a dictionary (@scheme[dict?]) mapping keywords to
lists of @techlink{check-procedures}. (Note that an association list is a
suitable dictionary.) The keyword's arity is the length of the list of
procedures.

@myexamples[
(define my-keyword-table
  (list (list '#:a check-identifier)
        (list '#:b check-expression check-expression)))
]

@schemegrammar[#, @deftech{check-procedure}
               (syntax syntax -> any)]

A check procedure consumes the syntax to check and a context syntax
object for error reporting and either raises an error to reject the
syntax or returns a value as its parsed representation.

@myexamples[
(define (check-stx-string stx context-stx)
  (unless (string? (syntax-e stx))
    (raise-syntax-error #f "expected string" context-stx stx))
  stx)
]

@schemegrammar[#, @deftech{options}
               (listof (list keyword syntax-keyword any ...))]

Parsed options are represented as an list of option entries. Each
entry contains the keyword, the syntax of the keyword (for error
reporting), and the list of parsed values returned by the keyword's
list of check procedures. The list contains the parsed options in the
order they appeared in the input, and a keyword that occurs multiple
times in the input occurs multiple times in the options list.

@defproc[(parse-keyword-options [stx syntax?]
                                [table #, @techlink{keyword-table}]
                                [#:context ctx (or/c false/c syntax?) #f]
                                [#:no-duplicates? no-duplicates? boolean? #f]
                                [#:incompatible incompatible (listof (listof keyword?)) '()]
                                [#:on-incompatible incompatible-handler
                                                   (-> keyword? keyword? 
                                                       #, @techlink{options} syntax? syntax?
                                                       (values #, @techlink{options} syntax?))
                                                   (lambda (....) (error ....))]
                                [#:on-too-short too-short-handler
                                                (-> keyword? #, @techlink{options} syntax? syntax?
                                                    (values #, @techlink{options} syntax?))
                                                (lambda (....) (error ....))]
                                [#:on-not-in-table not-in-table-handler
                                                   (-> keyword? #, @techlink{options} syntax? syntax?
                                                       (values #, @techlink{options} syntax?))
                                                   (lambda (....) (error ....))])
         (values #, @techlink{options} any/c)]{

Parses the keyword options in the syntax @scheme[stx] (@scheme[stx]
may be an improper syntax list). The keyword options are described in
the @scheme[table] association list. Each entry in @scheme[table]
should be a list whose first element is a keyword and whose subsequent
elements are procedures for checking the arguments following the
keyword. The keyword's arity (number of arguments) is determined by
the number of procedures in the entry. Only fixed-arity keywords are
supported.

Parsing stops normally when the syntax list does not have a keyword at
its head (it may be empty, start with a non-keyword term, or it may be
a non-list syntax object). Two values are returned: the parsed
@techlink{options} and the rest of the syntax (generally either a
syntax object or a list of syntax objects).

A variety of errors and exceptional conditions can occur during the
parsing process. The following keyword arguments determine the
behavior in those situations.

The @scheme[#:context ctx] argument is used to report all errors in
parsing syntax. In addition, @scheme[ctx] is passed as the final
argument to all provided handler procedures. Macros using
@scheme[parse-keyword-options] should generally pass the syntax object
for the whole macro use as @scheme[ctx].

If @scheme[no-duplicates?] is a non-false value, then duplicate
keyword options are not allowed. If a duplicate is seen, the keyword's
associated check procedures are not called and an @tech{incompatibility} is
reported.

The @scheme[incompatible] argument is a list of incompatibility
entries, where each entry is a list of @emph{at least two}
keywords. If any keyword in the entry occurs after any other keyword
in the entry, an @tech{incompatibility} is reported.

Note that including a keyword in an incompatibility entry does not
prevent it from occurring multiple times. To disallow duplicates of
some keywords (as opposed to all keywords), include those keywords in
the @scheme[incompatible] list as being incompatible with
themselves. That is, include them twice:

@schemeblock[
(code:comment "Disallow duplicates of only the #:foo keyword")
(parse-keyword-options .... #:incompatible '((#:foo #:foo)))
]

When an @deftech{incompatibility} occurs, the
@scheme[incompatible-handler] is tail-called with the two keywords
causing the incompatibility (in the order that they occurred in the
syntax list, so the keyword triggering the incompatibility occurs
second), the syntax list starting with the occurrence of the second
keyword, and the context (@scheme[ctx]). If the incompatibility is due
to a duplicate, the two keywords are the same.

When a keyword is not followed by enough arguments according to its
arity in @scheme[table], the @scheme[too-short-handler] is tail-called
with the keyword, the @techlink{options} parsed thus far, the syntax list
starting with the occurrence of the keyword, and @scheme[ctx].

When a keyword occurs in the syntax list that is not in
@scheme[table], the @scheme[not-in-table-handler] is tail-called with
the keyword, the @techlink{options} parsed thus far, the syntax list
starting with the occurrence of the keyword, and @scheme[ctx].

Handlers typically escape---all of the default handlers raise
errors---but if they return, they should return two values: the parsed
@techlink{options} and a syntax object; these are returned as the results
of @scheme[parse-keyword-options].

@(myexamples
  (parse-keyword-options
   #'(#:transparent #:property p (lambda (x) (f x)))
   (list (list '#:transparent)
         (list '#:inspector check-expression)
         (list '#:property check-expression check-expression)))
  (parse-keyword-options
   #'(#:transparent #:inspector (make-inspector))
   (list (list '#:transparent)
         (list '#:inspector check-expression)
         (list '#:property check-expression check-expression))
   #:context #'define-struct
   #:incompatible '((#:transparent #:inspector)
                    (#:inspector #:inspector)
                    (#:inspector #:inspector))))

}

@defproc[(parse-keyword-options/eol [stx syntax?]
                                [table #, @techlink{keyword-table}]
                                [#:context ctx (or/c false/c syntax?) #f]
                                [#:no-duplicates? no-duplicates? boolean? #f]
                                [#:incompatible incompatible (listof (list keyword? keyword?)) '()]
                                [#:on-incompatible incompatible-handler
                                                   (-> keyword? keyword? 
                                                       #, @techlink{options} syntax? syntax?
                                                       (values #, @techlink{options} syntax?))
                                                   (lambda (....) (error ....))]
                                [#:on-too-short too-short-handler
                                                (-> keyword? #, @techlink{options} syntax? syntax?
                                                    (values #, @techlink{options} syntax?))
                                                (lambda (....) (error ....))]
                                [#:on-not-in-table not-in-table-handler
                                                   (-> keyword? #, @techlink{options} syntax? syntax?
                                                       (values #, @techlink{options} syntax?))
                                                   (lambda (....) (error ....))]
                                [#:on-not-eol not-eol-handler
                                              (-> #, @techlink{options} syntax? syntax?
                                                  #, @techlink{options})
                                              (lambda (....) (error ....))])
         #, @techlink{options}]{

Like @scheme[parse-keyword-options], but checks that there are no
terms left over after parsing all of the keyword options. If there
are, @scheme[not-eol-handler] is tail-called with the @techlink{options}
parsed thus far, the leftover syntax, and @scheme[ctx].

}

@defproc[(options-select [options #, @techlink{options}]
                         [keyword keyword?])
         (listof list?)]{

Selects the values associated with one keyword from the parsed
@techlink{options}. The resulting list has as many items as there were
occurrences of the keyword, and each element is a list whose length is
the arity of the keyword.

}

@defproc[(options-select-row [options #, @techlink{options}]
                             [keyword keyword?]
                             [#:default default any/c])
         any]{

Like @scheme[options-select], except that the given keyword must occur
either zero or one times in @scheme[options]. If the keyword occurs,
the associated list of parsed argument values is returned. Otherwise,
the @scheme[default] list is returned.

}

@defproc[(options-select-value [options #, @techlink{options}]
                               [keyword keyword?]
                               [#:default default any/c])
         any]{

Like @scheme[options-select], except that the given keyword must occur
either zero or one times in @scheme[options]. If the keyword occurs,
the associated list of parsed argument values must have exactly one
element, and that element is returned. If the keyword does not occur
in @scheme[options], the @scheme[default] value is returned.

}



@defproc[(check-identifier [stx syntax?] [ctx (or/c false/c syntax?)]) identifier?]{

A @techlink{check-procedure} that accepts only identifiers.

}

@defproc[(check-expression [stx syntax?] [ctx (or/c false/c syntax?)]) syntax?]{

A @techlink{check-procedure} that accepts any non-keyword term. It does
not actually check that the term is a valid expression.

}

@defproc[((check-stx-listof [check #, @techlink{check-procedure}])
          [stx syntax?] [ctx (or/c false/c syntax?)])
         (listof any/c)]{

Lifts a @techlink{check-procedure} to accept syntax lists of whatever the
original procedure accepted.

}

@defproc[(check-stx-string [stx syntax?] [ctx (or/c false/c syntax?)]) syntax?]{

A @techlink{check-procedure} that accepts syntax strings.

}

@defproc[(check-stx-boolean [stx syntax?] [ctx (or/c false/c syntax?)])
         syntax?]{

A @techlink{check-procedure} that accepts syntax booleans.

}
