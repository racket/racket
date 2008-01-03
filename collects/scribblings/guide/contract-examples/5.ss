;; chapter 5: A Queue

;; --- queue -------------------------------------------------------------------

;; <font color="blue">Note: this queue doesn't implement the capacity restriction
;; of McKim and Mitchell's queue but this is really minor and easy to add.</font>

(module queue mzscheme 
  (require (lib "contract.ss") (lib "etc.ss"))
  
  ;; a contract utility 
  (define (all-but-last l) (reverse (cdr (reverse l))))
  (define (eq/c x) (lambda (y) (eq? x y)))  
  
  ;; implementation
  (define-struct queue (list p? eq))
  
  (define (initialize p? eq) (make-queue '() p? eq))
  (define items queue-list)
  (define (put q x) 
    (make-queue (append (queue-list q) (list x)) (queue-p? q) (queue-eq q)))
  (define (count s) (length  (queue-list s)))
  (define (is-empty? s) (null? (queue-list s)))
  (define not-empty? (compose not is-empty?))
  (define (remove s) (make-queue (cdr (queue-list s)) (queue-p? s) (queue-eq s)))
  (define (head s) (car (queue-list s)))
  
  ;; interface
  (provide/contract
   ;; predicate 
   [queue?     (-> any/c boolean?)]
   ;; primitive queries
   ;; <font color="red">Imagine providing this 'query' for the interface of the module
   ;; only. Then in Scheme, there is no reason to have count or is-empty? 
   ;; around (other than providing it to clients). After all items is 
   ;; exactly as cheap as count. </font>
   [items      (->d queue? (compose listof queue-p?))]
   ;; derived queries 
   [count      (->r ([q queue?]) 
                    ;; <font color="red">We could express this second part of the post
                    ;; condition even if count were a module "attribute"
                    ;; in the language of Eiffel; indeed it would use the 
                    ;; exact same syntax (minus the arrow and domain). </font>
                    (and/c natural-number/c (=/c (length (items q)))))]
   [is-empty?  (->r ([q queue?])
                    (and/c boolean? (eq/c (null? (items q)))))]
   [head      (->r ([q (and/c queue? (compose not is-empty?))])
                   (and/c (queue-p? q) (eq/c (car (items q)))))]
   ;; creation
   [initialize (-> contract? (contract? contract? . -> . boolean?) 
                   (and/c queue? (compose null? items)))]
   ;; commands
   [put        (->r ([oldq queue?][i (queue-p? oldq)]) 
                    (and/c queue? 
                           (lambda (q)
                             (define old-items (items oldq))
                             (equal? (items q) (append old-items (list i))))))]
   [remove     (->r ([oldq (and/c queue? (compose not is-empty?))]) 
                    (and/c queue?
                           (lambda (q)
                             (equal? (cdr (items oldq)) (items q)))))])
  ;; end of interface
  )

;; --- tests -------------------------------------------------------------------

(module test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 2)))
  (require (lib "contract.ss"))
  (require queue)
  
  (define s (put (put (initialize (flat-contract integer?) =) 2) 1))
  
  (test/text-ui
   (make-test-suite 
    "queue"
    (make-test-case 
     "empty"
     (assert-true (is-empty? (initialize (flat-contract integer?) =))))
    (make-test-case 
     "put"
     (assert-true (queue? s)))
    (make-test-case 
     "count"
     (assert = (count s) 2))
    (make-test-case 
     "put exn"
     #;(assert-exn exn:fail:contract?
                   (push (initialize (flat-contract integer?)) 'a))
       (assert-true (with-handlers ([exn:fail:contract? (lambda _ #t)])
                      (put (initialize (flat-contract integer?)) 'a)
                      #f)))
    (make-test-case 
     "remove" 
     (assert-true (queue? (remove s))))
    (make-test-case 
     "head"
     (assert = (head s) 2))))
  )

(require test)
