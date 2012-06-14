#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define check-eval (make-base-eval))
@(interaction-eval #:eval check-eval (require (for-syntax racket/base)))

@(define-syntax-rule (racketblock/eval #:eval e body ...)
   (begin
     (interaction-eval #:eval e body) ...
     (racketblock body ...)))

@title[#:tag "proc-macros" #:style 'toc]{General Macro Transformers}

The @racket[define-syntax] form creates a @deftech{transformer
binding} for an identifier, which is a binding that can be used at
compile time while expanding expressions to be evaluated at run time.
The compile-time value associated with a transformer binding can be
anything; if it is a procedure of one argument, then the binding is
used as a macro, and the procedure is the @deftech{macro transformer}.

The @racket[syntax-rules] and @racket[syntax-id-rules] forms are
macros that expand to procedure forms. For example, if you evaluate a
@racket[syntax-rules] form directly (instead of placing on the
right-hand of a @racket[define-syntax] form), the result is a
procedure:

@interaction[
(syntax-rules () [(nothing) something])
]

Instead of using @racket[syntax-rules], you can write your own macro
transformer procedure directly using @racket[lambda]. The argument to
the procedure is a value that represents the source form, and the
result of the procedure must be a value that represents the
replacement form.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "stx-obj"]{Syntax Objects}

The input and output of a macro transformer (i.e., source and
replacement forms) are represented as @deftech{syntax objects}. A
syntax object contains symbols, lists, and constant values (such as
numbers) that essentially correspond to the @racket[quote]d form of
the expression. For example, a representation of the expression
@racket[(+ 1 2)] contains the symbol @racket['+] and the numbers
@racket[1] and @racket[2], all in a list. In addition to this quoted
content, a syntax object associates source-location and
lexical-binding information with each part of the form. The
source-location information is used when reporting syntax errors (for
example), and the lexical-biding information allows the macro system
to maintain lexical scope. To accommodate this extra information, the
represention of the expression @racket[(+ 1 2)] is not merely
@racket['(+ 1 2)], but a packaging of @racket['(+ 1 2)] into a syntax
object.

To create a literal syntax object, use the @racket[syntax] form:

@interaction[
(eval:alts (#,(racket syntax) (+ 1 2)) (syntax (+ 1 2)))
]

In the same way that @litchar{'} abbreviates @racket[quote],
@litchar{#'} abbreviates @racket[syntax]:

@interaction[
#'(+ 1 2)
]

A syntax object that contains just a symbol is an @deftech{identifier
syntax object}. Racket provides some additional operations specific to
identifier syntax objects, including the @racket[identifier?]
operation to detect identifiers. Most notably,
@racket[free-identifier=?]  determines whether two identifiers refer
to the same binding:

@interaction[
(identifier? #'car)
(identifier? #'(+ 1 2))
(free-identifier=? #'car #'cdr)
(free-identifier=? #'car #'car)
(require (only-in racket/base [car also-car]))
(free-identifier=? #'car #'also-car)
(free-identifier=? #'car (let ([car 8])
                           #'car))
]

The last example above, in particular, illustrates how syntax objects
preserve lexical-context information.

To see the lists, symbols, numbers, @|etc| within a syntax object, use
@racket[syntax->datum]:

@interaction[
(syntax->datum #'(+ 1 2))
]

The @racket[syntax-e] function is similar to @racket[syntax->datum],
but it unwraps a single layer of source-location and lexical-context
information, leaving sub-forms that have their own information wrapped
as syntax objects:

@interaction[
(syntax-e #'(+ 1 2))
]

The @racket[syntax-e] function always leaves syntax-object wrappers
around sub-forms that are represented via symbols, numbers, and other
literal values. The only time it unwraps extra sub-forms is when
unwrapping a pair, in which case the @racket[cdr] of the pair may be
recursively unwrapped, depending on how the syntax object was
constructed.

The opposite of @racket[syntax->datum] is, of course,
@racket[datum->syntax].  In addition to a datum like @racket['(+ 1
2)], @racket[datum->syntax] needs an existing syntax object to donate
its lexical context, and optionally another syntax object to donate
its source location:

@interaction[
(datum->syntax #'lex
               '(+ 1 2)
               #'srcloc)
]

In the above example, the lexical context of @racket[#'lex] is used
for the new syntax object, while the source location of
@racket[#'srcloc] is used.

When the second (i.e., the ``datum'') argument to
@racket[datum->syntax] includes syntax objects, those syntax objects
are preserved intact in the result. That is, deconstructing the result
with @racket[syntax-e] eventually produces the syntax objects that
were given to @racket[datum->syntax].


@; ----------------------------------------

@section[#:tag "syntax-case"]{Mixing Patterns and Expressions: @racket[syntax-case]}

The procedure generated by @racket[syntax-rules] internally uses
@racket[syntax-e] to deconstruct the given syntax object, and it uses
@racket[datum->syntax] to construct the result. The
@racket[syntax-rules] form doesn't provide a way to escape from
pattern-matching and template-construction mode into an arbitrary
Racket expression.

The @racket[syntax-case] form lets you mix pattern matching, template
construction, and arbitrary expressions:

@specform[(syntax-case stx-expr (literal-id ...)
            [pattern expr]
            ...)]

Unlike @racket[syntax-rules], the @racket[syntax-case] form does not
produce a procedure. Instead, it starts with a @racket[_stx-expr]
expression that determines the syntax object to match against the
@racket[_pattern]s. Also, each @racket[syntax-case] clause has a
@racket[_pattern] and @racket[_expr], instead of a @racket[_pattern]
and @racket[_template]. Within an @racket[_expr], the @racket[syntax]
form---usually abbreviated with @litchar{#'}---shifts into
template-construction mode; if the @racket[_expr] of a clause starts
with @litchar{#'}, then we have something like a @racket[syntax-rules]
form:

@interaction[
(syntax->datum
 (syntax-case #'(+ 1 2) ()
  [(op n1 n2) #'(- n1 n2)]))
]

We could write the @racket[swap] macro using @racket[syntax-case]
instead of @racket[define-syntax-rule] or @racket[syntax-rules]:

@racketblock[
(define-syntax swap
  (lambda (stx)
    (syntax-case stx ()
      [(swap x y) #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp))])))
]

One advantage of using @racket[syntax-case] is that we can provide
better error reporting for @racket[swap]. For example, with the
@racket[define-syntax-rule] definition of @racket[swap], then
@racket[(swap x 2)] produces a syntax error in terms of @racket[set!],
because @racket[2] is not an identifier. We can refine our
@racket[syntax-case] implementation of @racket[swap] to explicitly
check the sub-forms:

@racketblock[
(define-syntax swap
  (lambda (stx)
    (syntax-case stx ()
      [(swap x y) 
       (if (and (identifier? #'x)
                (identifier? #'y))
           #'(let ([tmp x])
               (set! x y)
               (set! y tmp))
           (raise-syntax-error #f
                               "not an identifier"
                               stx
                               (if (identifier? #'x) 
                                   #'y 
                                   #'x)))])))
]

With this definition, @racket[(swap x 2)] provides a syntax error
originating from @racket[swap] instead of @racket[set!].

In the above definition of @racket[swap], @racket[#'x] and
@racket[#'y] are templates, even though they are not used as the
result of the macro transformer. This example illustrates how
templates can be used to access pieces of the input syntax, in this
case for checking the form of the pieces. Also, the match for
@racket[#'x] or @racket[#'y] is used in the call to
@racket[raise-syntax-error], so that the syntax-error message can
point directly to the source location of the non-identifier.

@; ----------------------------------------

@section[#:tag "with-syntax"]{@racket[with-syntax] and @racket[generate-temporaries]}

Since @racket[syntax-case] lets us compute with arbitrary Racket
expression, we can more simply solve a problem that we had in
writing @racket[define-for-cbr] (see
@secref["pattern-macro-example"]), where we needed to generate a
set of names based on a sequence @racket[id ...]:

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     ....
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body) ....]))
]

@margin-note{This example uses @racket[(define-syntax (_id _arg) _body ...+)],
 which is equivalent to @racket[(define-syntax _id (lambda (_arg) _body ...+))].}

In place of the @racket[....]s above, we need to bind @racket[get
...] and @racket[put ...] to lists of generated identifiers. We
cannot use @racket[let] to bind @racket[get] and @racket[put],
because we need bindings that count as pattern variables, instead
of normal local variables. The @racket[with-syntax] form lets us
bind pattern variables:

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     (with-syntax ([(get ...) ....]
                   [(put ...) ....])
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body))]))
]

Now we need an expression in place of @racket[....] that
generates as many identifiers as there are @racket[id] matches in
the original pattern. Since this is a common task, Racket
provides a helper function, @racket[generate-temporaries], that
takes a sequence of identifiers and returns a sequence of
generated identifiers:

@racketblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     (with-syntax ([(get ...) (generate-temporaries #'(id ...))]
                   [(put ...) (generate-temporaries #'(id ...))])
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body))]))
]

This way of generating identifiers is normally easier to think
about than tricking the macro expander into generating names with
purely pattern-based macros.

In general, the right-hand side of a @racket[with-syntax]
binding is a pattern, just like in @racket[syntax-case]. In fact,
a @racket[with-syntax] form is just a @racket[syntax-case] form
turned partially inside-out.

@; ----------------------------------------

@section[#:tag "stx-phases"]{Compile and Run-Time Phases}

As sets of macros get more complicated, you might want to write
your own helper functions, like
@racket[generate-temporaries]. For example, to provide good
syntax-error messsage, @racket[swap], @racket[rotate], and
@racket[define-cbr] all should check that certain sub-forms in
the source form are identifiers. We could use a
@racket[check-ids] to perform this checking everywhere:

@racketblock/eval[
#:eval check-eval
(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))

(define-syntax (rotate stx)
  (syntax-case stx ()
    [(rotate a c ...)
     (begin
       (check-ids stx #'(a c ...))
       #'(shift-to (c ... a) (a c ...)))]))
]

The @racket[check-ids] function can use the @racket[syntax->list]
function to convert a syntax-object wrapping a list into a list
of syntax objects:

@racketblock[
(define (check-ids stx forms)
  (for-each
   (lambda (form)
     (unless (identifier? form)
       (raise-syntax-error #f
                           "not an identifier"
                           stx
                           form)))
   (syntax->list forms)))
]

If you define @racket[swap] and @racket[check-ids] in this way,
however, it doesn't work:

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b))
]

The problem is that @racket[check-ids] is defined as a run-time
expression, but @racket[swap] is trying to use it at compile time. In
interactive mode, compile time and run time are interleaved, but they
are not interleaved within the body of a module, and they are not
interleaved or across modules that are compiled ahead-of-time. To help
make all of these modes treat code consistently, Racket separates the
binding spaces for different phases.

To define a @racket[check-ids] function that can be referenced at
compile time, use @racket[begin-for-syntax]:

@racketblock/eval[
#:eval check-eval
(begin-for-syntax
  (define (check-ids stx forms)
    (for-each
     (lambda (form)
       (unless (identifier? form)
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             form)))
     (syntax->list forms))))
]

With this for-syntax definition, then @racket[swap] works:

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b) (list a b))
(swap a 1)
]

When organizing a program into modules, you may want to put helper
functions in one module to be used by macros that reside on other
modules. In that case, you can write the helper function using
@racket[define]:

@racketmod[#:file
"utils.rkt"
racket

(provide check-ids)

(define (check-ids stx forms)
  (for-each
   (lambda (form)
     (unless (identifier? form)
       (raise-syntax-error #f
                           "not an identifier"
                           stx
                           form)))
   (syntax->list forms)))
]

Then, in the module that implements macros, import the helper function
using @racket[(require (for-syntax "utils.rkt"))] instead of
@racket[(require "utils.rkt")]:

@racketmod[
racket

(require (for-syntax "utils.rkt"))

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))
]

Since modules are separately compiled and cannot have circular
dependencies, the @filepath["utils.rkt"] module's run-time body can be
compiled before the compiling the module that implements
@racket[swap].  Thus, the run-time definitions in
@filepath["utils.rkt"] can be used to implement @racket[swap], as long
as they are explicitly shifted into compile time by @racket[(require
(for-syntax ....))].

The @racketmodname[racket] module provides @racket[syntax-case],
@racket[generate-temporaries], @racket[lambda], @racket[if], and more
for use in both the run-time and compile-time phases. That is why we
can use @racket[syntax-case] in the @exec{racket} @tech{REPL} both
directly and in the right-hand side of a @racket[define-syntax]
form.

The @racketmodname[racket/base] module, in contrast, exports those
bindings only in the run-time phase. If you change the module above
that defines @racket[swap] so that it uses the
@racketmodname[racket/base] language instead of
@racketmodname[racket], then it no longer works. Adding
@racket[(require (for-syntax racket/base))] imports
@racket[syntax-case] and more into the compile-time phase, so that the
module works again.

Suppose that @racket[define-syntax] is used to define a local macro in
the right-hand side of a @racket[define-syntax] form. In that case,
the right-hand side of the inner @racket[define-syntax] is in the
@deftech{meta-compile phase level}, also known as @deftech{phase level
2}. To import @racket[syntax-case] into that phase level, you would
have to use @racket[(require (for-syntax (for-syntax racket/base)))]
or, equivalently, @racket[(require (for-meta 2 racket/base))].  For example,

@codeblock|{
#lang racket/base
(require  ;; This provides the bindings for the definition
          ;; of shell-game.
          (for-syntax racket/base)
 
          ;; And this for the definition of
          ;; swap.
          (for-syntax (for-syntax racket/base)))

(define-syntax (shell-game stx)

  (define-syntax (swap stx)
    (syntax-case stx ()
      [(_ a b)
       #'(let ([tmp a])
           (set! a b)
           (set! b tmp))]))
  
  (syntax-case stx ()
    [(_ a b c)
     (let ([a #'a] [b #'b] [c #'c])
       (when (= 0 (random 2)) (swap a b))
       (when (= 0 (random 2)) (swap b c))
       (when (= 0 (random 2)) (swap a c))
       #`(list #,a #,b #,c))]))

(shell-game 3 4 5)
(shell-game 3 4 5)
(shell-game 3 4 5)
}|

Negative phase levels also exist. If a macro uses a helper function
that is imported @racket[for-syntax], and if the helper function
returns syntax-object constants generated by @racket[syntax], then
identifiers in the syntax will need bindings at @deftech{phase level
-1}, also known as the @deftech{template phase level}, to have any
binding at the run-time phase level relative to the module that
defines the macro.

@; ----------------------------------------

@include-section["phases.scrbl"]

@; ----------------------------------------

@include-section["syntax-taints.scrbl"]

@close-eval[check-eval]
