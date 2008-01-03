;; chapter 1: A Customer Manager Component for Managing Customer Relationships 

;; --- common data definitions ------------------------------------------------

(module basic-customer-details mzscheme 
  (require (lib "contract.ss"))
  
  ;; data definitions 
  
  (define id? symbol?)
  (define id-equal? eq?)
  (define-struct basic-customer (id name address))
  
  ;; interface 
  (provide/contract 
   [id?                   (-> any/c boolean?)]
   [id-equal?             (-> id? id? boolean?)]
   [struct basic-customer ((id id?) (name string?) (address string?))])
  ;; end of interface
  )

;; --- the customer manager ----------------------------------------------------

(module customer-manager mzscheme
  (require (lib "contract.ss") (lib "etc.ss") (lib "list.ss"))
  
  (require basic-customer-details)
  
  ;; implementation 
  ;; [listof (list basic-customer? secret-info)]
  (define all '())
  
  (define (find c) 
    (define (has-c-as-key p) (id-equal? (basic-customer-id (car p)) c))
    (define x (filter has-c-as-key all))
    (if (pair? x) (car x) x))
  
  (define (active? c)
    (define f (find c))
    (pair? (find c)))
  
  (define not-active? (compose not active? basic-customer-id))
  
  (define count 0)
  
  (define (add c)
    (set! all (cons (list c 'secret) all))
    (set! count (+ count 1)))
  
  (define (name id)
    (define bc-with-id (find id))
    (basic-customer-name (car bc-with-id)))
  
  (define (set-name id name)
    (define bc-with-id (find id))
    (set-basic-customer-name! (car bc-with-id) name))
  
  (define c0 0)
  ;; end of implementation 
  
  ;; interface
  (provide/contract
   ;; how many customers are in the db?  
   [count    natural-number/c]
   ;; is the customer with this id active? 
   [active?  (-> id? boolean?)]
   ;; what is the name of the customer with this id? 
   [name     (-> (and/c id? active?) string?)]
   ;; change the name of the customer with this id 
   [set-name (->pp ([id id?] [nn string?])
                   #t    ;; no additional precondition 
		   any/c ;; result contract 
		   ____  ;; the result's name (irrelevant)
		   ;; the postcondition
		   (string=? (name id) nn))]
   ;; add a customer 
   [add      (->d (and/c basic-customer? not-active?)
                  (lambda (bc)
		    (let ([c0 count])
		      (lambda (void) (> count c0)))))]
   #;
   [add      (->pp ([bc (and/c basic-customer? not-active?)])
	           <font color="red">;; A pre-post condition contract must use a side-effect
                   ;; to express this contract via post-conditions</font>
                   (set! c0 count) ;; pre
		   any/c ;; result contract 
                   _____ ;; result name 
		   (> count c0))])
  ;; end of interface
  )

;; --- tests -------------------------------------------------------------------

(module test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 2)))
  (require basic-customer-details customer-manager)
  
  (add (make-basic-customer 'mf "matthias" "brookstone"))
  (add (make-basic-customer 'rf "robby" "beverly hills park"))
  (add (make-basic-customer 'fl "matthew" "pepper clouds town"))
  (add (make-basic-customer 'sk "shriram" "i city"))
  
  (test/text-ui
   (make-test-suite 
    "manager"
    (make-test-case 
     "id lookup"
     (assert equal? "matthias" (name 'mf)))
    (make-test-case 
     "count"
     (assert = 4 count))
    (make-test-case 
     "active?"
     (assert-true (active? 'mf)))
    (make-test-case 
     "active? 2"
     (assert-false (active? 'kk)))
    (make-test-case 
     "set name"
     (assert-true (void? (set-name 'mf "matt")))))))

(require test)
