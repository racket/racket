#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(define check-eval (make-base-eval))
@(interaction-eval #:eval check-eval (require (for-syntax scheme/base)))

@(define-syntax-rule (schemeblock/eval #:eval e body ...)
   (begin
    (interaction-eval #:eval e body) ...
    (schemeblock body ...)))

@title[#:tag "proc-macros" #:style 'toc]{General Macro Transformers}

The @scheme[define-syntax] form creates a @deftech{transformer
binding} for an identifier, which is a binding that can be used at
compile time while expanding expressions to be evaluated at run time.
The compile-time value associated with a transformer binding can be
anything; if it is a procedure of one argument, then the binding is
used as a macro, and the procedure is the @deftech{macro transformer}.

The @scheme[syntax-rules] and @scheme[syntax-id-rules] forms are
macros that expand to procedure forms. For example, if you evaluate a
@scheme[syntax-rules] form directly (instead of placing on the
right-hand of a @scheme[define-syntax] form), the result is a
procedure:

@interaction[
(syntax-rules () [(nothing) something])
]

Instead of using @scheme[syntax-rules], you can write your own macro
transformer procedure directly using @scheme[lambda]. The argument to
the procedure is a values that represents the source form, and the
result of the procedure must be a value that represents the
replacement form.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "stx-obj"]{Syntax Objects}

The input and output of a macro transformer (i.e., source and
replacement forms) are represented as @deftech{syntax objects}. A
syntax object contains symbols, lists, and constant values (such as
numbers) that essentially correspond to the @scheme[quote]d form of
the expression. For example, a representation of the expression
@scheme[(+ 1 2)] contains the symbol @scheme['+] and the numbers
@scheme[1] and @scheme[2], all in a list. In addition to this quoted
content, a syntax object associates source-location and
lexical-binding information with each part of the form. The
source-location information is used when reporting syntax errors (for
example), and the lexical-biding information allows the macro system
to maintain lexical scope. To accommodate this extra information, the
represention of the expression @scheme[(+ 1 2)] is not merely
@scheme['(+ 1 2)], but a packaging of @scheme['(+ 1 2)] into a syntax
object.

To create a literal syntax object, use the @scheme[syntax] form:

@interaction[
(eval:alts (#,(scheme syntax) (+ 1 2)) (syntax (+ 1 2)))
]

In the same way that @litchar{'} abbreviates @scheme[quote],
@litchar{#'} abbreviates @scheme[syntax]:

@interaction[
#'(+ 1 2)
]

A syntax object that contains just a symbol is an @deftech{identifier
syntax object}. Scheme provides some additional operations specific to
identifier syntax objects, including the @scheme[identifier?]
operation to detect identifiers. Most notably,
@scheme[free-identifier=?]  determines whether two identifiers refer
to the same binding:

@interaction[
(identifier? #'car)
(identifier? #'(+ 1 2))
(free-identifier=? #'car #'cdr)
(free-identifier=? #'car #'car)
(require (only-in scheme/base [car also-car]))
(free-identifier=? #'car #'also-car)
(free-identifier=? #'car (let ([car 8])
                           #'car))
]

The last example above, in particular, illustrates how syntax objects
preserve lexical-context information.

To see the lists, symbols, numbers, @|etc| within a syntax object, use
@scheme[syntax->datum]:

@interaction[
(syntax->datum #'(+ 1 2))
]

The @scheme[syntax-e] function is similar to @scheme[syntax->datum],
but it unwraps a single layer of source-location and lexical-context
information, leaving sub-forms that have their own information wrapped
as syntax objects:

@interaction[
(syntax-e #'(+ 1 2))
]

The @scheme[syntax-e] function always leaves syntax-object wrappers
around sub-forms that are represented via symbols, numbers, and other
literal values. The only time it unwraps extra sub-forms is when
unwrapping a pair, in which case the @scheme[cdr] of the pair may be
recursively unwrapped, depending on how the syntax object was
constructed.

The opposite of @scheme[syntax->datum] is, of course,
@scheme[datum->syntax].  In addition to a datum like @scheme['(+ 1
2)], @scheme[datum->syntax] needs an existing syntax object to donate
its lexical context, and optionally another syntax object to donate
its source location:

@interaction[
(datum->syntax #'lex
               '(+ 1 2)
               #'srcloc)
]

In the above example, the lexical context of @scheme[#'lex] is used
for the new syntax object, while the source location of
@scheme[#'srcloc] is used.

When the second (i.e., the ``datum'') argument to
@scheme[datum->syntax] includes syntax objects, those syntax objects
are preserved intact in the result. That is, deconstructing the result
with @scheme[syntax-e] eventually produces the syntax objects that
were given to @scheme[datum->syntax].


@; ----------------------------------------

@section[#:tag "syntax-case"]{Mixing Patterns and Expressions: @scheme[syntax-case]}

The procedure generated by @scheme[syntax-rules] internally uses
@scheme[syntax-e] to deconstruct the given syntax object, and it uses
@scheme[datum->syntax] to construct the result. The
@scheme[syntax-rules] form doesn't provide a way to escape from
pattern-matching and template-construction mode into an arbitrary
Scheme expression.

The @scheme[syntax-case] form lets you mix pattern matching, template
construction, and arbitrary expressions:

@specform[(syntax-case stx-expr (literal-id ...)
            [pattern expr]
            ...)]

Unlike @scheme[syntax-rules], the @scheme[syntax-case] form does not
produce a procedure. Instead, it starts with a @scheme[_stx-expr]
expression that determines the syntax object to match against the
@scheme[_pattern]s. Also, each @scheme[syntax-case] clause has a
@scheme[_pattern] and @scheme[_expr], instead of a @scheme[_pattern]
and @scheme[_template]. Within an @scheme[_expr], the @scheme[syntax]
form---usually abbreviated with @litchar{#'}---shifts into
template-construction mode; if the @scheme[_expr] of a clause starts
with @litchar{#'}, then we have something like a @scheme[syntax-rules]
form:

@interaction[
(syntax->datum
 (syntax-case #'(+ 1 2) ()
  [(op n1 n2) #'(- n1 n2)]))
]

We could write the @scheme[swap] macro using @scheme[syntax-case]
instead of @scheme[define-syntax-rule] or @scheme[syntax-rules]:

@schemeblock[
(define-syntax swap
  (lambda (stx)
    (syntax-case stx ()
      [(swap x y) #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp))])))
]

One advantage of using @scheme[syntax-case] is that we can provide
better error reporting for @scheme[swap]. For example, with the
@scheme[define-syntax-rule] definition of @scheme[swap], then
@scheme[(swap x 2)] produces a syntax error in terms of @scheme[set!],
because @scheme[2] is not an identifier. We can refine our
@scheme[syntax-case] implementation of @scheme[swap] to explicitly
check the sub-forms:

@schemeblock[
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

With this definition, @scheme[(swap x 2)] provides a syntax error
originating from @scheme[swap] instead of @scheme[set!].

In the above definition of @scheme[swap], @scheme[#'x] and
@scheme[#'y] are templates, even though they are not used as the
result of the macro transformer. This example illustrates how
templates can be used to access pieces of the input syntax, in this
case for checking the form of the pieces. Also, the match for
@scheme[#'x] or @scheme[#'y] is used in the call to
@scheme[raise-syntax-error], so that the syntax-error message can
point directly to the source location of the non-identifier.

@; ----------------------------------------

@section[#:tag "with-syntax"]{@scheme[with-syntax] and @scheme[generate-temporaries]}

Since @scheme[syntax-case] lets us compute with arbitrary Scheme
expression, we can more simply solve a problem that we had in
writing @scheme[define-for-cbr] (see
@secref["pattern-macro-example"]), where we needed to generate a
set of names based on a sequence @scheme[id ...]:

@schemeblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     ....
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body) ....]))
]

@margin-note{This example uses @scheme[(define-syntax (_id _arg) _body ...+)],
 which is equivalent to @scheme[(define-syntax _id (lambda (_arg) _body ...+))].}

In place of the @scheme[....]s above, we need to bind @scheme[get
...] and @scheme[put ...] to lists of generated identifiers. We
cannot use @scheme[let] to bind @scheme[get] and @scheme[put],
because we need bindings that count as pattern variables, instead
of normal local variables. The @scheme[with-syntax] form lets us
bind pattern variables:

@schemeblock[
(define-syntax (define-for-cbr stx)
  (syntax-case stx ()
    [(_ do-f (id ...) body)
     (with-syntax ([(get ...) ....]
                   [(put ...) ....])
       #'(define (do-f get ... put ...)
           (define-get/put-id id get put) ... 
           body))]))
]

Now we need an expression in place of @scheme[....] that
generates as many identifiers as there are @scheme[id] matches in
the original pattern. Since this is a common task, Scheme
provides a helper function, @scheme[generate-temporaries], that
takes a sequece of identifiers and returns a sequence of
generated identifiers:

@schemeblock[
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

In general, the right-hand side of a @scheme[with-syntax]
binding is a pattern, just like in @scheme[syntax-case]. In fact,
a @scheme[with-syntax] form is just a @scheme[syntax-case] form
turned partially inside-out.

@; ----------------------------------------

@section[#:tag "stx-phases"]{Compile and Run-Time Phases}

As sets of macros get more complicated, you might want to write
your own helper functions, like
@scheme[generate-temporaries]. For example, to provide good
syntax-error messsage, @scheme[swap], @scheme[rotate], and
@scheme[define-cbr] all should check that certain sub-forms in
the source form are identifiers. We could use a
@scheme[check-ids] to perform this checking everywhere:

@schemeblock/eval[
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

The @scheme[check-ids] function can use the @scheme[syntax->list]
function to convert a syntax-object wrapping a list into a list
of syntax objects:

@schemeblock[
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

If you define @scheme[swap] and @scheme[check-ids] in this way,
however, it doesn't work:

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b))
]

The problem is that @scheme[check-ids] is defined as a run-time
expression, but @scheme[swap] is trying to use it at compile time. In
interactive mode, compile time and run time are interleaved, but they
are not interleaved within the body of a module, and they are not
interleaved or across modules that are compiled ahead-of-time. To help
make all of these modes treat code consistently, Scheme separates the
binding spaces for different phases.

To define a @scheme[check-ids] function that can be referenced at
compile time, use @scheme[define-for-syntax]:

@schemeblock/eval[
#:eval check-eval
(define-for-syntax (check-ids stx forms)
  (for-each
   (lambda (form)
     (unless (identifier? form)
       (raise-syntax-error #f
                           "not an identifier"
                           stx
                           form)))
   (syntax->list forms)))
]

With this for-syntax definition, then @scheme[swap] works:

@interaction[
#:eval check-eval
(let ([a 1] [b 2]) (swap a b) (list a b))
(swap a 1)
]

When organizing a program into modules, you may want to put helper
functions in one module to be used by macros that reside on other
modules. In that case, you can write the helper function using
@scheme[define]:

@schememod[#:file 
"utils.ss"
scheme

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
using @scheme[(require (for-syntax "utils.ss"))] instead of
@scheme[(require "utils.ss")]:

@schememod[
scheme

(require (for-syntax "utils.ss"))

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) (begin
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])
                      (set! x y)
                      (set! y tmp)))]))
]

Since modules are separately compiled and cannot have circular
dependencies, the @filepath["utils.ss"] module's run-time body can be
compiled before the compiling the module that implements
@scheme[swap].  Thus, the run-time definitions in
@filepath["utils.ss"] can be used to implement @scheme[swap], as long
as they are explicitly shifted into compile time by @scheme[(require
(for-syntax ....))].

The @schememodname[scheme] module provides @scheme[syntax-case],
@scheme[generate-temporaries], @scheme[lambda], @scheme[if], and more
for use in both the run-time and compile-time phases. That is why we
can use @scheme[syntax-case] in the @scheme[mzscheme] @tech{REPL} both
directly and in the right-hand side of a @scheme[define-syntax]
form.

The @schememodname[scheme/base] module, in contrast, exports those
bindings only in the run-time phase. If you change the module above
that defines @scheme[swap] so that it uses the
@schememodname[scheme/base] language instead of
@schememodname[scheme], then it no longer works. Adding
@scheme[(require (for-syntax scheme/base))] imports
@scheme[syntax-case] and more into the compile-time phase, so that the
module works again.

Suppose that @scheme[define-syntax] is used to define a local macro in
the right-hand side of a @scheme[define-syntax] form. In that case,
the right-hand side of the inner @scheme[define-syntax] is in the
@deftech{meta-compile phase level}, also known as @deftech{phase level
2}. To import @scheme[syntax-case] into that phase level, you would
have to use @scheme[(require (for-syntax (for-syntax scheme/base)))]
or, equivalently, @scheme[(require (for-meta 2 scheme/base))].

Negative phase levels also exist. If a macro uses a helper function
that is imported @scheme[for-syntax], and if the helper function
returns syntax-object constants generated by @scheme[syntax], then
identifiers in the syntax will need bindings at @deftech{phase level
-1}, also known as the @deftech{template phase level}, to have any
binding at the run-time phase level relative to the module that
defines the macro.

@; ----------------------------------------

@include-section["certificates.scrbl"]

@close-eval[check-eval]
