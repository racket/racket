#reader scribble/reader
#lang racket/base
(require "common.rkt"
         scribble/decode
         scribble/struct
         scribble/racket
         racket/list
         racket/pretty
         syntax/docprovide
         (for-syntax racket/base)
         )

(provide prim-variables
         prim-forms
         define-forms/normal
         define-form/explicit-lambda
         intermediate-forms
         prim-ops
         prim-op-defns)

(define (maybe-make-table l t)
  (if (paragraph? t)
      (make-paragraph
       (append l (cons " "
                       (paragraph-content t))))
      (make-table
       "prototype"
       (list (list (make-flow (list (make-paragraph l)))
                   (make-flow (list t)))))))


(define (typeset-type type)
  (let-values ([(in out) (make-pipe)])
    (parameterize ([pretty-print-columns 50])
      (pretty-print type out))
    (port-count-lines! in)
    (read-syntax #f in)))

(define (sort-category category)
  (sort
   (cadr category)
   (lambda (x y)
     (string<=? (symbol->string (car x))
                (symbol->string (car y))))))


(define (make-proto func ctx-stx)
  (maybe-make-table
   (list
    (hspace 2)
    (to-element (datum->syntax ctx-stx (car func)))
    (hspace 1)
    ":"
    (hspace 1))
   (to-paragraph
    (typeset-type (cadr func)))))

(define-syntax-rule (prim-variables (section-prefix) empty true false)
  (make-splice
   (list

@section[#:tag (string-append section-prefix " Pre-Defined Variables")]{Pre-Defined Variables}
@defthing[empty empty?]{

The empty list.}

@defthing[true boolean?]{

The true value.}

@defthing[false boolean?]{

The false value.}

)))

(define-syntax-rule (define-forms/normal define)

(make-splice
 (list
@defform*[[(define (... (name variable variable ...)) expression)]]{

Defines a function named @racket[name]. The @racket[expression] is the body
of the function. When the function is called,
the values of the arguments are inserted into the body in place of the
@racket[variable]s. The function returns the value of that new expression.

The function name's cannot be the same as that of another function or
variable.}

@defform/none[#:literals (define) (define name expression)]{

Defines a variable called @racket[name] with the the value of
@racket[expression]. The variable name's cannot be the same as that of
another function or variable, and @racket[name] itself must not appear in
@racket[expression].}

)))

(define-syntax-rule (define-form/explicit-lambda define lambda)

(make-splice
 (list
    
@defform/none[#:literals (define lambda)
              (... (define name (lambda (variable variable ...) expression)))]{

An alternate way on defining functions. The @racket[name] is the name of
the function, which cannot be the same as that of another function or
variable. 

@defidform/inline[lambda] cannot be used outside of this alternate syntax.
}
)))


(define-syntax-rule (prim-forms 
                     (section-prefix)
                     define 
                     lambda
                     define-struct
                     define-wish
                     cond
                     else
                     if
                     and 
                     or
                     check-expect
                     check-within
                     check-error
                     check-member-of
                     check-range
                     require)
  (make-splice
   (list



@; ----------------------------------------------------------------------


@defform*[[(... (define-struct structure-name (field-name ...)))]]{

Defines a new structure called @racket[field-name]. The structure's fields are
named by the @racket[field-name]s. After the @racket[define-struct], the following new
functions are available:

@itemize[

 @item{@racketidfont{make-}@racket[structure-name] : takes in a number of
       arguments equal to the number of fields in the structure,
       and creates a new instance of that structure.}

 @item{@racket[structure-name]@racketidfont{-}@racket[field-name] : takes in an
       instance of the structure and returns the value in the field named by
       @racket[field-name].}

 @item{@racket[structure-name]@racketidfont{?} : takes in any value, and returns
       @racket[true] if the value is an instance of the structure.}

]

The name of the new functions introduced by @racket[define-struct] must not be the same as that of other functions or
variables, otherwise @racket[define-struct] reports an error.}

#|

@defform*[[(define-wish name)]]{                           
                           
Defines a function called @racket[name] that we wish exists but have not
implemented yet. The wished-for function can be called with one argument, and
are reported in the test report for the current program.

The name of the function cannot be the same as another function or variable.}


@defform/none[#:literals (define-wish)
              (define-wish name expression)]{
Similar to the above form, defines a wished-for function named @racket[name]. If the 
wished-for function is called with one value, it returns the values of @racket[expression]. }
|#

@; ----------------------------------------------------------------------


@defform*[[(... (name expression expression ...))]]{

Calls the function named @racket[name]. The value of the call is the value of
@racket[name]'s body when every one of the function's variables are
replaced by the values of the corresponding @racket[expression]s.

The function named @racket[name] must defined before it can be called. The
number of argument @racket[expression]s must be the same as the number of arguments
expected by the function.}


@; ----------------------------------------------------------------------


@defform*[#:literals (cond else)
          [(... (cond [question-expression answer-expression] ...))
           (... (cond [question-expression answer-expression] ... [else answer-expression]))]]{

Chooses a clause base on a condition. @racket[cond] finds the first
@racket[question-expression] which evaluates to @racket[true], then it evaluates
the corresponding @racket[answer-expression].

If none of the @racket[question-expression]s evaluates to @racket[true],
@racket[cond]'s value is the @racket[answer-expression] of the
@racket[else] clause. If there is no @racket[else], @racket[cond] reports
an error. If the result of a @racket[question-expression] is neither
@racket[true] nor @racket[false], @racket[cond] also reports an error.


@defidform/inline[else] cannot be used outside of @racket[cond].
}


@; ----------------------------------------------------------------------


@defform*[[(if test-expression then-expression else-expression)]]{

When the value of the @racket[test-expression] is @racket[true],
@racket[if] evaluates the @racket[then-expression]. When the test is
@racket[false], @racket[if] evaluates the @racket[else-expression].

If the @racket[test-expression] is neither @racket[true] nor
@racket[false], @racket[if] reports an error.}

@; ----------------------------------------------------------------------


@defform*[[(... (and expression expression expression ...))]]{

@racket[and] evaluates to @racket[true] if all the @racket[expression]s are
@racket[true]. If any @racket[expression] is false, the @racket[and]
expression immediately evaluates to @racket[false] (the expressions to the
right of that expression are not evaluated.)

If any of the expressions evaluate to a value other than @racket[true] or
@racket[false], it is an error.}

@; ----------------------------------------------------------------------


@defform*[[(... (or expression expression expression ...))]]{

@racket[or] evaluates to @racket[true] as soon as one of the
@racket[expression]s is @racket[true] (the expressions to the right of that
expression are not evaluated.) If all the @racket[expression] are false,
@racket[or] is @racket[false].

If any of the expressions evaluate to a value other than @racket[true] or
@racket[false], @racket[or] reports an error.}

@; ----------------------------------------------------------------------


@defform*[[(check-expect expression expected-expression)]]{

Checks that the first @racket[expression] evaluates to the same value as the
@racket[expected-expression].}

@defform*[[(check-within expression expected-expression delta-expression)]]{

Checks that the first @racket[expression] evaluates to a value within
@racket[delta-expression] of the @racket[expected-expression]. If
@racket[delta-expression] is not a number, @racket[check-within] report an
error.}

@defform*[[(check-error expression expression)
           (check-error expression)]]{

Checks that the first @racket[expression] reports an error,
where the error messages matches the string produced by the second
@racket[expression], if it is present.}

@defform*[[(... (check-member-of expression expression expression ...))]]{

Checks that the first @racket[expression] produces the same value as one of
the following @racket[expression]s.}

@defform*[[(check-range expression expression expression)]]{

Checks that the first @racket[expression] produces a number in between the numbers
produced by the second and third @racket[expression]s, inclusive.}
                                                               

@; ----------------------------------------------------------------------


@defform*[[(require string)]]{

Makes the definitions of the module specified by @racket[string]
available in the current module (i.e., the current file), where @racket[string]
refers to a file relative to the current file.

The @racket[string] is constrained in several ways to avoid problems
with different path conventions on different platforms: a @litchar{/}
is a directory separator, @litchar{.} always means the current
directory, @litchar{..} always means the parent directory, path
elements can use only @litchar{a} through @litchar{z} (uppercase or
lowercase), @litchar{0} through @litchar{9}, @litchar{-}, @litchar{_},
and @litchar{.}, and the string cannot be empty or contain a leading
or trailing @litchar{/}.}

@defform/none[#:literals (require)
              (require module-name)]{

Accesses a file in an installed library. The library name is an identifier
with the same constraints as for a relative-path string (though without the
quotes), with the additional constraint that it must not contain a
@litchar{.}.}

@defform/none[#:literals (require lib)
              (... (require (lib string string ...)))]{

Accesses a file in an installed library, making its definitions
available in the current module (i.e., the current file). The first
@racket[string] names the library file, and the remaining
@racket[string]s name the collection (and sub-collection, and so on)
where the file is installed. Each string is constrained in the same
way as for the @racket[(require string)] form.}


@defform/none[#:literals (require planet)
              (require (planet string (string string number number)))]{


Accesses a library that is distributed on the internet via the PLaneT
server, making it definitions available in the current module (i.e.,
current file).}

)))


(define-syntax-rule 
  (intermediate-forms lambda
                      quote
                      quasiquote
                      unquote
                      unquote-splicing
                      local
                      letrec
                      let*
                      let
                      time)

  (make-splice
   (list

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[name]})]
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[part]})]
@defform[(quote name)]
@defform/none[(quote part)]
)]{

A quoted name is a symbol. A quote part is an abbreviation for a nested lists.

Normally, this quotation is written with a @litchar{'}, like
@racket['(apple banana)], but it can also be written with @racket[quote], like
@racket[(@#,racket[quote] (apple banana))].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{`}@racket[name]})]
@defform/none[(unsyntax @elem{@racketvalfont{`}@racket[part]})]
@defform[(quasiquote name)]
@defform/none[(quasiquote part)]
)]{

Like @racket[quote], but also allows escaping to expression ``unquotes.''

Normally, quasi-quotations are written with a backquote, @litchar{`}, like
@racket[`(apple ,(+ 1 2))], but they can also be written with
@racket[quasiquote], like
@racket[(@#,racket[quasiquote] (apple ,(+ 1 2)))].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{,}@racket[expression]})]
@defform[(unquote expression)]
)]{

Under a single quasiquote, @racketfont{,}@racket[expression] escapes from
the quote to include an evaluated expression whose result is inserted
into the abbreviated list.

Under multiple quasiquotes, @racketfont{,}@racket[expression] is really
the literal @racketfont{,}@racket[expression], decrementing the quasiquote count
by one for @racket[expression].

Normally, an unquote is written with @litchar{,}, but it can also be
written with @racket[unquote].}


@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont[",@"]@racket[expression]})]
@defform[(unquote-splicing expression)]
)]{

Under a single quasiquote, @racketfont[",@"]@racket[expression] escapes from
the quote to include an evaluated expression whose result is a list to
splice into the abbreviated list.

Under multiple quasiquotes, a splicing unquote is like an unquote;
that is, it decrements the quasiquote count by one.

Normally, a splicing unquote is written with @litchar{,}, but it can
also be written with @racket[unquote-splicing].}

@defform[(... (local [definition ...] expression))]{

Groups related definitions for use in @racket[expression]. Each
@racket[definition] can be either a variable definition, a function
definition, or a structure definition, using the usual syntax. 

When evaluating @racket[local], each @racket[definition] is evaluated in
order, and finally the body @racket[expression] is evaluated. Only the
expressions within the @racket[local] (including the right-hand-sides of
the @racket[definition]s and the @racket[expression]) may refer to the
names defined by the @racket[definition]s. If a name defined in the
@racket[local] is the same as a top-level binding, the inner one
``shadows'' the outer one. That is, inside the @racket[local], any
references to that name refer to the inner one.}

@; ----------------------------------------------------------------------


@defform[(... (letrec ([name expr-for-let] ...) expression))]{

Like @racket[local], but with a simpler syntax. Each @racket[name] defines
a variables (or a functions) with the value of the corresponding
@racket[expr-for-let].  If @racket[expr-for-let] is a @racket[lambda],
@racket[letrec] defines a function, otherwise it defines a variable.}

@defform[(... (let* ([name expr-for-let] ...) expression))]{

Like @racket[letrec], but each @racket[name] can only be used in
@racket[expression], and in @racket[expr-for-let]s occuring after that
@racket[name].}

@defform[(... (let ([name expr-for-let] ...) expression))]{

Like @racket[letrec], but the defined @racket[name]s can be used only in
the last @racket[expression], not the @racket[expr-for-let]s next to the
@racket[name]s.}


@; ----------------------------------------------------------------------


@defform[(time expression)]{

Measures the time taken to evaluate @racket[expression]. After evaluating
@racket[expression], @racket[time] prints out the time taken by the
evaluation (including real time, time taken by the cpu, and the time spent
collecting free memory). The value of @racket[time] is the same as that of @racket[expression].}
)))

(define (prim-ops lib ctx-stx)
  (let ([ops (map (lambda (cat)
                    (cons (car cat)
                          (list (cdr cat))))
                  (lookup-documentation lib 'procedures))])
    (make-table
     #f
     (cons
      (list
       (make-flow
        (list
         (make-paragraph
          (list "In function calls, the function appearing immediatly after the open parenthesis can be any functions
defined with " (racket define) " or " (racket define-struct) ", or any one of:")))))
      (apply
       append
       (map (lambda (category)
              (cons
               (list (make-flow
                      (list
                       (make-paragraph (list (hspace 1)
                                             (bold (car category)))))))
               (map (lambda (func)
                      (list
                       (make-flow
                        (list
                         (make-proto func ctx-stx)))))
                    (sort-category category))))
            ops))))))


(define (prim-op-defns lib ctx-stx not-in)
  (make-splice
   (let ([ops (map (lambda (cat)
                     (cons (car cat)
                           (list (cdr cat))))
                   (lookup-documentation lib 'procedures))]
         [not-in-ns (map (lambda (not-in-mod)
                           (let ([ns (make-base-namespace)])
                             (parameterize ([current-namespace ns])
                               (namespace-require `(for-label ,not-in-mod)))
                             ns))
                         not-in)])
     (apply
      append
      (map (lambda (category)
             (cons
              (subsection #:tag-prefix (format "~a" lib) (car category))
              (filter values
                      (map
                       (lambda (func)
                         (let ([id (datum->syntax ctx-stx (car func))])
                           (and (not (ormap
                                      (lambda (ns)
                                        (free-label-identifier=?
                                         id
                                         (parameterize ([current-namespace ns])
                                           (namespace-syntax-introduce (datum->syntax #f (car func))))))
                                      not-in-ns))
                                (let ([desc-strs (cddr func)])
                                  (defthing/proc
                                    id
                                    (to-paragraph (typeset-type (cadr func)))
                                    desc-strs)))))
                       (sort-category category)))))
           ops)))))

