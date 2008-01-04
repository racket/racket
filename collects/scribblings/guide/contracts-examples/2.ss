;; chapter 2: A Parameteric (Simple) Stack 

;; --- stack -------------------------------------------------------------------

(module stack mzscheme 
  (require (lib "contract.ss") (lib "etc.ss"))
  
  ;; a contract utility 
  (define (eq/c x) (lambda (y) (eq? x y)))

  ;; implementation
  (define-struct stack (list p? eq))

  (define (initialize p? eq) (make-stack '() p? eq))
  (define (push s x) 
    (make-stack (cons x (stack-list s)) (stack-p? s) (stack-eq s)))
  (define (item-at s i) (list-ref (reverse (stack-list s)) (- i 1)))
  (define (count s) (length  (stack-list s)))
  (define (is-empty? s) (null? (stack-list s)))
  (define not-empty? (compose not is-empty?))
  (define (pop s) (make-stack (cdr (stack-list s)) (stack-p? s) (stack-eq s)))
  (define (top s) (car (stack-list s)))
  ;; end of implementation 
  
  ;; interface
  (provide/contract
   ;; predicate
   [stack?     (-> any/c boolean?)]
   ;; primitive queries
   ;; how many items are on the stack? 
   [count      (-> stack? natural-number/c)]
   ;; which item is at the given position? 
   [item-at    (->r ([s stack?][i (and/c positive? (<=/c (count s)))])
                    (stack-p? s))]
   ;; derived queries 
   ;; is the stack empty? 
   [is-empty?  (->d stack? (lambda (s) (eq/c (= (count s) 0))))]
   ;; which item is at the top of the stack 
   [top        (->pp ([s (and/c stack? not-empty?)])
                     #t ;; no precondition 
		     (stack-p? s) ;; a stack item 
                     t ;; the name of the result
		     ;; the postcondition 
                     ([stack-eq s] t (item-at s (count s))))]
   ;; creation 
   [initialize (->r ([p contract?][s (p p . -> . boolean?)])
                    ;; Mitchel and McKim use (= (count s) 0) here to express
                    ;; the post-condition in terms of a primitive query
                    (and/c stack? is-empty?))]
   ;; commands
   ;; add an item to the top of the stack 
   [push       (->pp ([s stack?][x (stack-p? s)]) 
                     #t ;; pre 
		     stack? ;; result kind 
                     sn ;; name of result
		     ;; post: 
                     (and (= (+ (count s) 1) (count sn))
                          ([stack-eq s] x (top sn))))]
   ;; remove the item at the top of the stack
   [pop        (->pp ([s (and/c stack? not-empty?)])
                     #t ;; pre 
		     stack? ;; result kind 
                     sn ;; name of result
		     ;; post:
                     (= (- (count s) 1) (count sn)))])
  ;; end of interface
  )

;; --- tests -------------------------------------------------------------------
    
(module test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 2)))
  (require (lib "contract.ss"))
  (require stack)
  
  (define s (push (push (initialize (flat-contract integer?) =) 2) 1))

  (test/text-ui
   (make-test-suite 
    "stack"
    (make-test-case 
     "empty"
     (assert-true (is-empty? (initialize (flat-contract integer?) =))))
    (make-test-case 
     "push"
     (assert-true (stack? s)))
    (make-test-case 
     "push exn"
     <font color="red">;; Noel & Ryan: this is how things should work:
     #;(assert-exn exn:fail:contract?
       (push (initialize (flat-contract integer?)) 'a))</font>
     ;; this is what I have to do: 
     (assert-true (with-handlers ([exn:fail:contract? (lambda _ #t)])
                    (push (initialize (flat-contract integer?)) 'a)
                    #f)))
    (make-test-case 
     "pop" 
     (assert-true (stack? (pop s))))
    (make-test-case 
     "top"
     (assert = (top s) 1))))
  )

(require test)
