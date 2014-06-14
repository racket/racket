#lang scribble/manual
@(require scribble/eval unstable/scribblings/utils racket (for-label racket/contract unstable/options))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract unstable/options))

@title[#:tag "options"]{Option Contracts}
@unstable-header[]

@defmodule[unstable/options]

This module introduces @defterm{option contracts}, a flavor of behavioral
software contracts. With option contracts developers control in a
programmatic manner whether, when, and how often contracts are
checked. Using this flavor of contracts, Racketeers can mimic any compiler
flag system but also create run-time informed checking systems. 

@defproc[(option/c [c contract?]
                   [#:with-contract with boolean? #f]
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
@racket[tester], if @racket[tester] is a predicate. After that,
contract checking is disabled for the value, if @racket[with] is @racket[#f]. If @racket[with]
is @racket[#t] contract checking for the value remains enabled for @racket[c].

If @racket[waive-option] is applied to a value guarded by an @racket[option/c]
contract, then @racket[waive-option] returns the value after removing the @racket[option/c] guard.
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
    [vec (option/c (vectorof number?) #:with-contract #t)]))
  (define vec (vector 1 2 3 4)))
(require 'server1)
(vector-set! vec 1 'foo)

(module server2 racket
  (require unstable/options)
  (provide 
   (contract-out 
    [vec (option/c (vectorof number?) #:tester sorted?)]))
  (define vec (vector 1 42 3 4))
  (define (sorted? vec)
    (for/and ([el vec]
              [cel (vector-drop vec 1)])
             (<= el cel))))
(require 'server2)
]  


}

@defproc[(exercise-option [x any/c]) any/c]{

Returns @racket[x] with contract checking enabled if an @racket[option/c] guards
@racket[x]. In any other case it returns @racket[x]. The result of @racket[exercise-option]
loses the guard related to @racket[option/c], if it has one to begin with, and thus its contract checking status cannot change further.
                                            
@defexamples[
#:eval the-eval
(module server3 racket
  (require unstable/options)
  (provide (contract-out [foo (option/c (-> number? symbol?))]))
  (define foo (λ (x) x)))
(require 'server3 unstable/options)
(define e-foo (exercise-option foo))
(foo 42)
(e-foo 'wrong)
((exercise-option e-foo) 'wrong)
]  
} 
                                      
@defthing[transfer/c contract?]{

A contract that accepts any value. If the value is guarded with an 
@racket[option/c] contract, @racket[transfer/c] modifies the blame 
information for the @racket[option/c] contract by adding the providing module and its client 
to the positive and negative blame parties respectively. If the value is not a value guarded with an 
@racket[option/c] contract, then @racket[transfer/c] is equivalent to @racket[any/c].
}                   

 @defexamples[
#:eval the-eval
(module server4 racket
  (require unstable/options)
  (provide (contract-out [foo (option/c (-> number? symbol?))]))
  (define foo (λ (x) x)))
(module middleman racket
  (require unstable/options 'server4)
  (provide (contract-out [foo transfer/c])))
(require 'middleman unstable/options)
(define e-foo (exercise-option foo))
(e-foo 1)
;(e-foo 'wrong)
(module server5 racket
  (require unstable/options)
  (provide (contract-out [boo transfer/c]))
  (define (boo x) x))
(require 'server5)
(boo 42)] 



@defproc[(waive-option [x any/c]) any/c]{ 

If an @racket[option/c] guards @racket[x], then @racket[waive-option] returns 
@racket[x] without the @racket[option/c] guard. 
In any other case it returns @racket[x]. The result of @racket[waive-option]
loses the guard related to @racket[option/c], if it had one to begin with, and thus its contract checking status cannot change further.

@defexamples[
#:eval the-eval
(module server6 racket
  (require unstable/options)
  (provide (contract-out [bar (option/c (-> number? symbol?))]))
  (define bar (λ (x) x)))
(require 'server6 unstable/options)
(define e-bar (waive-option bar))
(e-bar 'wrong)
((waive-option e-bar) 'wrong)]  
}    

@defproc[(tweak-option [x any/c]) any/c]{ 

If an @racket[option/c] guards @racket[x] and contract checking for @racket[x] is enabled, 
then @racket[tweak-option] returns 
@racket[x] with contract checking for @racket[x] disabled. 
If an @racket[option/c] guards @racket[x] and contract checking for @racket[x] is disabled, 
then @racket[tweak-option] returns 
@racket[x] with contract checking for @racket[x] enabled. 
In any other case it returns @racket[x]. The result of @racket[tweak-option]
retains the guard related to @racket[option/c] if it has one to begin with and thus its contract checking status can change further
using @racket[tweak-option], @racket[exercise-option] or @racket[waive-option].

@defexamples[
#:eval the-eval
(module server7 racket
  (require unstable/options)
  (provide (contract-out [bar (option/c (-> number? symbol?))]))
  (define bar (λ (x) x)))
(require 'server7 unstable/options)
(define t-bar (tweak-option bar))
(t-bar 'wrong)
((tweak-option t-bar) 'wrong)
((waive-option t-bar) 'wrong)
((exercise-option t-bar) 'wrong)
]  
}    
 
 
@defproc[(has-option? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] has an option contract.
}

@defproc[(has-option-with-contract? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] has an option contract with contract checking enabled.
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
(module server8 racket
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
(require 'server8)
(vector-set! vec 2 42)
(change)
(vector-ref vec 2)]

}


                   
@(close-eval the-eval)

