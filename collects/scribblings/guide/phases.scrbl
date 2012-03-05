#lang scribble/manual

@title[#:tag "phases"]{Phases}

@(require scribble/manual scribble/eval)

@section{Bindings}

A phase can be thought of as a way to separate computations. Imagine starting
two racket processes. If you ignore socket communication they will not have a
way to share anything. Racket effectively allows multiple invocations to exist
in the same process and phases are the mechanism that keeps them separate, just
like in the multiple process case. Similar to socket communication for processes
phases allow instances of Racket to share information through an explicit
protocol.

Bindings exist in a phase. The link between a binding and its phase is
represented by an integer called a phase level. Phase level 0 is the phase used
for "plain" definitions, so

@racketblock[
(define age 5)
]

Will put a binding for @racket[age] into phase 0. @racket[age] can be defined at
higher phases levels easily

@racketblock[
(begin-for-syntax
  (define age 5))
]

Now @racket[age] is defined at phase level 1. We can easily mix these two definitions
in the same module, there is no clash between the two @racket[age]'s because
they are defined at different phase levels.

@(define age-eval (make-base-eval))
@(interaction-eval #:eval age-eval
                (require (for-syntax racket/base)))

@examples[#:eval age-eval
(define age 3)
(begin-for-syntax
  (define age 9))
]

@racket[age] at phase level 0 has a value of 3 and @racket[age] at phase level 1 has a value of 9.

Syntax objects can refer to these bindings. Essentially they capture the binding as a value that can be passed around.

@racketblock[#'age]

Is a syntax object that represents the @racket[age] binding. But which
@racket[age] binding? In the last example there are two @racket[age]'s, one at
phase level 0 and one at phase level 1. Racket will imbue @racket[#'age] with lexical
information for all phase levels, so the answer is both!

Racket knows which @racket[age] to use when the syntax object is used. I'll use eval just for a second to prove a point.

First we bind @racket[#'age] to a pattern variable so we can use it in a template and then just print it.

@examples[#:eval age-eval
(eval (with-syntax ([age #'age])
        #'(printf "~a\n" age)))
]

We get 3 because @racket[age] at phase 0 level is bound to 3.

@examples[#:eval age-eval
(eval (with-syntax ([age #'age])
        #'(begin-for-syntax
            (printf "~a\n" age))))
]

We get 9 because we are using @racket[age] at phase level 1 instead of 0. How
does Racket know we wanted to use @racket[age] at phase 1 instead of 0? Because
of the @racket[begin-for-syntax]. @racket[begin-for-syntax] evaluates the
expressions inside it at phase level 1. So you can see that we started with the
same syntax object, @racket[#'age], and was able to use it in two different ways
-- at phase level 0 and at phase level 1.

When a syntax object is created its lexical context is immediately set up.
Syntax objects provided from a module retain their lexical context and will
reference bindings that existed in the module it came from.

The following example defines @racket[button] at phase level 0 bound to the
value 0 and @racket[sbutton] which binds the syntax object for @racket[button]
in module @racket[a].

@examples[
(module a racket
(define button 0)
(provide (for-syntax sbutton))
@code:comment{why not (define sbutton #'button) ? I will explain later}
(define-for-syntax sbutton #'button)
)

(module b racket
(require 'a)
(define button 8)
(define-syntax (m stx)
  sbutton)
(m)
)

(require 'b)
]

The result of the @racket[m] macro will be whatever value @racket[sbutton] is
bound to, which is @racket[#'button]. The @racket[#'button] that
@racket[sbutton] knows that @racket[button] is bound from the @racket[a] module
at phase level 0. Even though there is another @racket[button] in @racket[b]
this will not confuse Racket.

Note that @racket[sbutton] is bound at phase level 1 by virtue of defining it with
@racket[define-for-syntax]. This is needed because @racket[m] is a macro so its
body executes at one phase higher than it was defined at. Since it was defined
at phase level 0 the body will execute at phase level 1, so any bindings in the body also
need to be bound at phase level 1.

@section{Modules}

Phases and bindings can get very confusing when used with modules. Racket allows
us to import a module at an arbitrary phase using require.

@racketblock[
(require "a.rkt") @code:comment{import at phase 0}
(require (for-syntax "a.rkt")) @code:comment{import at phase 1}
(require (for-template "a.rkt")) @code:comment{import at phase -1}
(require (for-meta 5 "a.rkt" )) @code:comment{import at phase 5}
]

What does it mean to 'import at phase 1'? Effectively it means that all the
bindings from that module will have their phase increased by one. Similarly when
importing at phase -1 all bindings from that module will have their phase
decreased by one.

@examples[
(module c racket
  (define x 0) ;; x is defined at phase 0
  (provide x)
)

(module d racket
 (require (for-syntax 'c))
 )
]

Now in the @racket[d] module there will be a binding for @racket[x] at phase 1 instead of phase 0.

So lets look at module @racket[a] from above and see what happens if we try to create a
binding for the @racket[#'button] syntax object at phase 0.

@(define button-eval (make-base-eval))
@(interaction-eval #:eval button-eval
                   (require (for-syntax racket/base)))
@examples[#:eval button-eval
(define button 0)
(define sbutton #'button)
]

Now both @racket[button] and @racket[sbutton] are defined at phase 0. The lexical
context of @racket[#'button] will know that there is a binding for
@racket[button] at
phase 0. In fact it seems like things are working just fine, if we try to eval
@racket[sbutton] here we will get 0.

@examples[#:eval button-eval
(eval sbutton)
]

But now lets use sbutton in a macro.

@examples[#:eval button-eval
(define-syntax (m stx)
  sbutton)
(m)
]

We get an error 'reference to an identifier before its definition: sbutton'.
Clearly @racket[sbutton] is not defined at phase level 1 so we cannot refer to
it inside the macro. Lets try to use @racket[sbutton] in another module by
putting the button definitions in a module and importing it at phase level 1.
Then we will get @racket[sbutton] at phase level 1.

@examples[
(module a racket
  (define button 0)
  (define sbutton #'button)
  (provide sbutton))

(module b racket
  (require (for-syntax 'a)) ;; now we have sbutton at phase level 1
  (define-syntax (m stx)
   sbutton)
  (m)
)
]

Racket says that @racket[button] is unbound now. When @racket[a] is imported at
phase level 1 we have the following bindings

@verbatim{
button at phase level 1
sbutton at phase level 1
}

So the macro @racket[m] can see a binding for @racket[sbutton] at phase level 1
and will return the @racket[#'button] syntax object which refers to
@racket[button] binding at phase 0. But there is no @racket[button] at phase
level 0 in @racket[b], there is only a @racket[button] at phase 1, so we get an
error.  That is why @racket[sbutton] needed to be bound at phase 1 in
@racket[a]. In that case we would have had the following bindings after doing
@racket[(require 'a)]

@verbatim{
button at phase level 0
sbutton at phase level 1
}

In this scenario we can use @racket[sbutton] in the macro since @racket[sbutton]
is bound at phase level 1 and when the macro finishes it will refer to a
@racket[button] binding at phase level 0.

However, if we import @racket[a] at phase level 1 we can still manage to use
@racket[sbutton] to get access to @racket[button]. The trick is to create a
syntax object that will be evaluated at phase level 1 instead of 0. We can do
that with @racket[begin-for-syntax].

@examples[
(module a racket
(define button 0)
(define sbutton #'button)
(provide sbutton))

(module b racket
(require (for-syntax 'a))
(define-syntax (m stx)
  (with-syntax ([x sbutton])
    #'(begin-for-syntax
        (printf "~a\n" x))))
(m))
]

Module @racket[b] has @racket[button] and @racket[sbutton] bound at phase level 1. The output of the macro will be 

@racketblock[
(begin-for-syntax
  (printf "~a\n" button))
  ]

Because @racket[sbutton] will turn into @racket[button] when the template is expanded.
Now this expression will work because @racket[button] is bound at phase level 1.

Now you might try to cheat the phase system by importing @racket[a] at both
phase level 0 and phase level 1. Then you would have the following bindings

@verbatim{
button at phase level 0
sbutton at phase level 0
button at phase level 1
sbutton at phase level 1
}

So just using @racket[sbutton] in a macro should work

@examples[
(module a racket
  (define button 0)
  (define sbutton #'button)
  (provide sbutton))

(module b racket
(require 'a
         (for-syntax 'a))
(define-syntax (m stx)
  sbutton)
(m))
]

The @racket[sbutton] inside the @racket[m] macro comes from the
@racket[(for-syntax 'a)]. For this macro to work there must be a @racket[button]
at phase 0 bound, and there is one from the plain @racket[(require 'a)] imported
at phase 0. But in fact this macro doesn't work, it says @racket[button] is
unbound. The key is that @racket[(require 'a)] and @racket[(require (for-syntax
'a))] are different instantiations of the same module.  The @racket[sbutton] at
phase 1 only refers to the @racket[button] at phase level 1, not the @racket[button]
bound at phase 0 from a different instantation, even from the same file.

So this means that if you have a two functions in a module, one that produces a
syntax object and one that matches on it (say using @racket[syntax/parse]) the
module needs to be imported once at the proper phase. The module can't be
imported once at phase 0 and again at phase level 1 and be expected to work.

@examples[
(module x racket

(require (for-syntax syntax/parse)
         (for-template racket/base))
                  
(provide (all-defined-out))

(define button 0)
(define (make) #'button)
(define-syntax (process stx)
  (define-literal-set locals (button))
  (syntax-parse stx
    [(_ (n (~literal button))) #'#''ok])))

(module y racket
(require (for-meta 1 'x)
         (for-meta 2 'x racket/base)
         ;; (for-meta 2 racket/base)
         )
         
(begin-for-syntax
  (define-syntax (m stx)
    (with-syntax ([out (make)])
      #'(process (0 out))))) 
    
(define-syntax (p stx)
  (m))

(p))
]

@racket[make] is being used in @racket[y] at phase 2 and returns the
@racket[#'button] syntax object which refers to @racket[button] bound at phase
level 0 inside @racket[x] and at phase 2 in @racket[y] from @racket[(for-meta 2
'x)].  The @racket[process] macro is imported at phase level 1 from
@racket[(for-meta 1 'x)] and knows that @racket[button] should be bound at phase
level 1 so when the @racket[syntax-parse] is executed inside @racket[process] it
is looking for @racket[button] bound at phase level 1 but it sees a phase level
2 binding and so doesn't match.

To fix this we can provide @racket[make] at phase level 1 relative to @racket[x] and
just import it at phase level 1 in @racket[y].

@examples[
(module x racket
(require (for-syntax syntax/parse)
         (for-template racket/base))
                  
(provide (all-defined-out))

(define button 0)
(provide (for-syntax make))
(define-for-syntax (make) #'button)
(define-syntax (process stx)
(define-literal-set locals (button))
  (syntax-parse stx
    [(_ (n (~literal button))) #'#''ok])))

(module y racket
(require (for-meta 1 'x)
         ;; (for-meta 2 'x racket/base)
         (for-meta 2 racket/base)
         )
         
(begin-for-syntax
  (define-syntax (m stx)
    (with-syntax ([out (make)])
      #'(process (0 out))))) 
    
(define-syntax (p stx)
  (m))

(p))

(require 'y)
]

@(close-eval age-eval)
@(close-eval button-eval)
