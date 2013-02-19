#lang scribble/manual
@(require scribble/eval unstable/scribblings/utils racket (for-label racket/contract unstable/options))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract unstable/options))

@title[#:tag "options"]{Options}
@unstable-header[]

@defmodule[unstable/options]

@defproc[(option/c [c contract?]
                   [#:tester tester (or/c (-> any boolean?) 'dont-care) 'dont-care]
                   [#:invariant invariant (or/c (-> any boolean?) 'dont-care) 'dont-care]
                   [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                   [#:flat? flat? boolean? #f]
                   [#:struct struct-id (or/c identifier? 'none) 'none])
         contract?]{                                             
                                             
Returns a contract that recognizes vectors or hashes or instances of 
struct @racket[struct-id]. The data structure must match @racket[c] and pass the
@racket[tester]. 

When an @racket[option/c] contract is attached to a value, the value is checked against the 
@racket[tester], if @racket[tester] is a predicate. After that, contract checking is disabled for the value.

If @racket[exercise-option] is applied to a value guarded by an @racket[option/c]
contract, then @racket[exercise-option] returns the value with contract checking 
enabled for @racket[c]. If the @racket[invariant] argument is a predicate, then 
@racket[exercise-option] returns the value with contract checking enabled for 
@racket[(invariant/c c 
                          invariant
                          #:immutable immutable
                          #:flat? flat?
                          #:struct struct-id)].

The arguments @racket[flat?] and @racket[immutable] should be provided only if @racket[invariant]
is a predicate. In any other case, the result is a contract error.

@defexamples[
#:eval the-eval

(module server0 racket
  (require unstable/options)
  (provide 
   (contract-out 
    [vec (option/c (vectorof number?))]))
  (define vec (vector 1 2 3 4)))
(require 'server0)
(vector-set! vec 1 'foo)
(vector-ref vec 1)

(module server1 racket
  (require unstable/options)
  (provide 
   (contract-out 
    [vec (option/c (vectorof number?) #:tester sorted?)]))
  (define vec (vector 1 42 3 4))
  (define (sorted? vec)
    (for/and ([el vec]
              [cel (vector-drop vec 1)])
             (<= el cel))))
(require 'server1)
]  


}

@defproc[(exercise-option [x any/c]) any/c]{

Returns @racket[x] with contract ckecking enabled if an @racket[option/c] guards
@racket[x]. In any other case the result is an error.                                          
                                            
@defexamples[
#:eval the-eval
(module server2 racket
  (require unstable/options)
  (provide (contract-out [foo (option/c (-> number? symbol?))]))
  (define foo (λ (x) x)))
(require 'server2 unstable/options)
(define e-foo (exercise-option foo))
(foo 1)
(e-foo 'wrong)
(exercise-option e-foo)]  
} 
                                      
@defform[(transfer-option id ...)]{
                                   
A @racket[_provide-spec] for use in @racket[provide] (currently only for
the same @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{phase level}
as the @racket[provide] form; for example,
@racket[transfer-option] cannot be nested within @racket[for-syntax]). Each @racket[id]
is provided from the module if @racket[id] is bound to a value guarded with an 
@racket[option/c] contract. In addition, @racket[transfer-option] modifies the blame 
information for the @racket[option/c] contract by adding the providing module and its client 
to the positive and negative blame parties respectively. If @racket[id] is not bound to a value guarded with an 
@racket[option/c] contract, then the result is a contract error.
}                   

 @defexamples[
#:eval the-eval
(module server3 racket
  (require unstable/options)
  (provide (contract-out [foo (option/c (-> number? symbol?))]))
  (define foo (λ (x) x)))
(module middleman racket
  (require unstable/options 'server3)
  (provide (transfer-option foo)))
(require 'middleman unstable/options)
(define e-foo (exercise-option foo))
(e-foo 1)
(e-foo 'wrong)
(module server4 racket
  (require unstable/options)
  (provide [transfer-option boo])
  (define (boo x) x))
(require 'server4)]  
}


@defproc[(waive-option [x any/c]) any/c]{ 

If an @racket[option/c] guards @racket[x], then @racket[waive-option] returns 
@racket[x] without the @racket[option/c] guard. 
In any other case the result is an error.

@defexamples[
#:eval the-eval
(module server5 racket
  (require unstable/options)
  (provide (contract-out [bar (option/c (-> number? symbol?))]))
  (define bar (λ (x) x)))
(require 'server5 unstable/options)
(define e-bar (waive-option bar))
(e-bar 1)
(exercise-option e-bar)
(waive-option e-bar)]  
}    

}



@defproc[(invariant/c [c contract?]
                   [invariant (-> any boolean?)]
                   [#:immutable immutable (or/c #t #f 'dont-care) 'dont-care]
                   [#:flat? flat? boolean? #f]
                   [#:struct struct-id (or/c identifier? 'none) 'none])
         contract?]{

Returns a contract that recognizes vectors or hashes or instances of 
struct @racket[struct-id]. The data structure must match @racket[c] and satisfy the
@racket[invariant] argument. 

If the @racket[flat?] argument is @racket[#t], then the resulting contract is
a flat contract, and the @racket[c] arguments must also be flat contracts.  Such
flat contracts will be unsound if applied to a mutable data structure, as they will not
check future operations on the vector.

If the @racket[immutable] argument is @racket[#t] and the @racket[c] arguments are
flat contracts, the result will be a flat contract.  If the @racket[c] arguments
are chaperone contracts, then the result will be a chaperone contract.


@defexamples[
#:eval the-eval
(module server6 racket
   (require unstable/options)
   (provide
     change
     (contract-out 
      [vec (invariant/c 
            any/c
            sorted?)]))
  (define vec (vector 1 2 3 4 5))
  (define (change) (vector-set! vec 2 42))
  (define (sorted? vec)
    (for/and ([el vec]
              [cel (vector-drop vec 1)])
      (<= el cel))))
(require 'server6)
(vector-set! vec 2 42)
(change)
(vector-ref vec 2)]

}
                   
                   
@(close-eval the-eval)

