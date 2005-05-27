;; This library is used by match.ss
;

;;! (function stx-length
;;          (form (syntax-length syntax-obj) -> int)
;;          (contract syntax-object -> int)
;;          (example (syntax-length (syntax iraq war idiocy)) -> 3))
;;  Returns the length of the top-level syntax list.
(define stx-length (lambda (syntax-obj)
                     (length (syntax->list syntax-obj))))

;;! (function stx-?
;;          (form (stx? test val) -> bool)
;;          (contract ((any -> bool) syntax-object) -> bool)
;;          (example (stx-? number? (syntax 4)) -> #t))
;;  Applies predicate test to the syntax object val and returns the resulting
;; boolean value.
(define stx-? (lambda (test val)
                (test (syntax-object->datum val))))

;;!(function stx-equal?
;;          (form (stx-equal? a b) -> bool)
;;          (contract (syntax-object syntax-object) -> bool)
;;          (example (stx-equal? (syntax 5) (syntax 5)) -> #t))
;; Check the equality of two syntax objects by after applying
;; syntax-object->datum to the objects first.  Checks equaltiy of
;; syntax objects after they have had all syntax data stripped away.
(define stx-equal? (lambda (a b)
                     (equal? (syntax-object->datum a)
                             (syntax-object->datum b))))

;;!(function symbol-append
;;          (form (symbol-append . args) -> symbol)
;;          (contract ((symbol or number) ...) -> symbol)
;;          (example (symbol-append 'hello 5 'goodbye) -> 'hello5goodbye))
;; This function takes any number of arguments which can be either
;; symbols or numbers and returns one symbol which is the
;; concatenation of the input.
(define symbol-append (lambda l
                        (string->symbol
                         (apply
                          string-append
                          (map (lambda (x)
                                 (cond
                                  ((symbol? x) (symbol->string x))
                                  ((number? x) (number->string x))
                                  (else x)))
                               l)))))

;;!(function struct-pred-accessors-mutators
;;          (form (struct-pred-accessors-mutators struct-name failure-thunk)
;;                ->
;;                (values pred accessors mutators parental-chain))
;;          (contract (syntax-object (any -> void))
;;                     ->
;;                     (values (any -> bool) list list)))
;; This function takes a syntax-object that is the name of a structure
;; as well as a failure thunk.  It returns three values.  The first is
;; a predicate for the structure.  The second is a list of accessors
;; in the same order as the fields of the structure declaration.  The
;; third is a list of mutators for the structure also in the same
;; order.  The last is a list of supertypes of this struct. The
;; failure thunk is invoked if the struct-name is not bound to a
;; structure.
(define struct-pred-accessors-mutators
  (let ((accessors-index 3)
        (mutators-index 4)
        (pred-index 2)
        (super-type-index 5)
        (handle-acc-list
         (lambda (l)
           (letrec ((RC
                     (lambda (ac-list)
                       (cond ((null? ac-list) '())
                             ((not (car ac-list)) '())
                             (else (cons (car ac-list)
                                         (RC (cdr ac-list))))))))
             (reverse (RC l))))))
    (lambda (struct-name failure-thunk)
      (letrec ((get-lineage
                (lambda (struct-name)
                  (let ((super 
                         (list-ref 
                          (syntax-local-value struct-name 
                                              failure-thunk)
                          super-type-index)))
                    (cond ((equal? super #t) '())
                          ((equal? super #f) '()) ;; not sure what to do in case where super-type is unknown
                          (else
                           (cons super (get-lineage super))))))))
            (let ((info-on-struct (syntax-local-value struct-name failure-thunk)))
              (if (struct-declaration-info? info-on-struct)
                  (let ((accessors (handle-acc-list
                                    (list-ref info-on-struct accessors-index)))
                        (mutators (handle-acc-list
                                   (list-ref info-on-struct mutators-index)))
                        (pred (list-ref info-on-struct pred-index))
                        (parental-chain (get-lineage struct-name)))
                    (values pred accessors mutators (cons struct-name parental-chain)))
                  (failure-thunk)))))))




;;!(function get-exp-var
;;          (form (get-exp-var) -> syntax)
;;          (contract () -> syntax)
;;          (example (get-exp-var) -> (syntax exp754)))
;; This function just produces unique identifiers for expressions.
(define get-exp-var (lambda () #`#,(gensym 'exp)))

;;!(function in
;;          (form (in e l) -> bool)
;;          (contract (s-exp list) -> bool)
;;          (example (in '(number? x) (list '(number? x))) -> #t))
;; This function is responsible for determining which tests are
;; redundant.  If e can be determined to be true from the list of
;; tests l then e is "in" l.
(define in (lambda (e l)
             (or 
                 (ormap
                  (lambda (el)
                    (or (equal? e el)
                        (and 
                         (eq? (car e) 'struct-pred)
                         (eq? (car el) 'struct-pred)
                         (member (caaddr e) (caddr el))
                         (equal? (cadddr e) (cadddr el))))) l)
                 (and (eq? (car e) 'list?)
                      (or (member `(null? ,(cadr e)) l)
                          (member `(pair? ,(cadr e)) l)))
                 (and (eq? (car e) 'not)
                      (let* ((srch (cadr e))
                             (const-class (equal-test? srch)))
                        ;(write srch)
                        (cond
                         ((equal? (car srch) 'struct-pred)
                          (let mem ((l l))
                            (if (null? l)
                                #f
                                (let ((x (car l)))
                                  (if (and (equal? (car x)
                                                   'struct-pred)
                                           (not (equal? (cadr x) (cadr srch)))
                                           ; the current struct type should not
                                           ; be a member of the parental-chain of 
                                           (not (member (caaddr x) (caddr srch))) 
                                           (equal? (cadddr x) (cadddr srch)))
                                      #t
                                      (mem (cdr l)))))))        
                         (const-class  
                          (let mem ((l l))
                            (if (null? l)
                                #f
                                (let ((x (car l)))
                                  (or (and (equal?
                                            (cadr x)
                                            (cadr srch))
                                           (disjoint? x)
                                           (not (equal?
                                                 const-class
                                                 (car x))))
                                      (equal?
                                       x
                                       `(not (,const-class
                                              ,(cadr srch))))
                                      (and (equal?
                                            (cadr x)
                                            (cadr srch))
                                           (equal-test?
                                            x)
                                           (not (equal?
                                                 (caddr
                                                  srch)
                                                 (caddr
                                                  x))))
                                      (mem (cdr l)))))))
                         ((disjoint? srch) 
                          (let mem ((l l))
                            (if (null? l)
                                #f
                                (let ((x (car l)))
                                  (or (and (disjoint? x)
                                           (not (equal?
                                                 (car x)
                                                 (car srch)))
                                           (cond ((equal?
                                                   (car srch)
                                                   'struct-pred)
                                                  (equal? 
                                                   (cadr x)
                                                   ;; we use cadddr here to access the expression
                                                   ;; because struct predicates carry some extra baggage
                                                   ;; They have the form (struct-pred <predicate> <list of super types> <exp>)
                                                   (cadddr srch)))
                                                 ((equal?
                                                   (car x)
                                                   'struct-pred)
                                                  (equal? 
                                                   (cadr srch)
                                                   ;; we use cadddr here to access the expression
                                                   ;; because struct predicates carry some extra baggage
                                                   (cadddr x)))
                                                 (else (equal?
                                                        (cadr x)
                                                        (cadr srch)))))
                                      (mem (cdr l)))))))
                         ((eq? (car srch) 'list?) 
                          (let mem ((l l))
                            (if (null? l)
                                #f
                                (let ((x (car l)))
                                  (or (and (equal?
                                            (cadr x)
                                            (cadr srch))
                                           (disjoint?
                                            x)
                                           (not (memq (car x)
                                                      '(list?
                                                        pair?
                                                        null?))))
                                      (mem (cdr l)))))))
                         ((vec-structure? srch) 
                          (let mem ((l l))
                            (if (null? l)
                                #f
                                (let ((x (car l)))
                                  (or (and (equal?
                                            (cadr x)
                                            (cadr srch))
                                           (or (disjoint?
                                                x)
                                               (vec-structure?
                                                x))
                                           (not (equal?
                                                 (car x)
                                                 'vector?))
                                           (not (equal?
                                                 (car x)
                                                 (car srch))))
                                      (equal?
                                       x
                                       `(not (vector?
                                              ,(cadr srch))))
                                      (mem (cdr l)))))))
                         (else #f)))))))

;;!(function equal-test?
;;          (form (equal-test? tst) -> (or symbol
;;                                         #f))
;;          (contract s-exp -> (or symbol
;;                                         #f))
;;          (example (equal-test? '(equal? x 5))
;;                   -> 'number?)
;;          (example (equal-test? '(symbol? x))
;;                   -> #f))
;; This function returns false if the s-exp does not represent an
;; "equal?" test.  If it does then this function returns a
;; predicate for the data type that the test is testing.
(define equal-test? (lambda (tst)
                      (and (eq? (car tst) 'equal?)
                           (let ((p (caddr tst)))
                             (cond
                              ((string? p) 'string?)
                              ((boolean? p) 'boolean?)
                              ((char? p) 'char?)
                              ((number? p) 'number?)
                              ((and (pair? p)
                                    (pair? (cdr p))
                                    (null? (cddr p))
                                    (eq? 'quote (car p))
                                    (symbol? (cadr p))) 'symbol?)
                              (else #f))))))

(define match:disjoint-predicates
  '(struct-pred null? pair? symbol? boolean? number? string? char?
          procedure? vector?
          box? promise?)) 

(define match:vector-structures '())

;;!(function disjoint?
;;          (form (disjoint? tst))
;;          (contract s-exp -> bool)
;;          (example (disjoint? 'pair?) -> #t))
;; This function retirns true if the predicate is disjoint.
(define disjoint?
  (lambda (tst)
    (memq (car tst) match:disjoint-predicates)))

(define vec-structure? (lambda (tst)
                         (memq (car tst) match:vector-structures)))
;;!(function add-a
;;          (form (add-a exp-syntax) -> syntax)
;;          (contract syntax -> syntax)
;;          (example (add-a (syntax (cdr x))) -> (syntax (cadr x))))
;; Add car operation, ie. given (c...r x), return (ca...r x).
(define add-a
  (lambda (exp-syntax)
    (syntax-case exp-syntax ()
      ((car-thing exp)
       (let ((new (assq (syntax-object->datum (syntax car-thing)) c---rs)))
         (if new
             (quasisyntax/loc exp-syntax (#,(cadr new) exp))
             (syntax/loc exp-syntax (car (car-thing exp))))))
      (exp (syntax/loc exp-syntax (car exp))))))

;;!(function add-d
;;          (form (add-d exp-syntax) -> syntax)
;;          (contract syntax -> syntax)
;;          (example (add-a (syntax (cdr x))) -> (syntax (cddr x))))
;; Add cdr operation, ie. given (c...r x), return (cd...r x).
(define add-d
  (lambda (exp-syntax)
    (syntax-case exp-syntax ()
      ((car-thing exp)
       (let ((new (assq (syntax-object->datum (syntax car-thing)) c---rs)))
         (if new
             (quasisyntax/loc exp-syntax (#,(cddr new) exp))
             (syntax/loc exp-syntax (cdr (car-thing exp))))))
      (exp (syntax/loc exp-syntax (cdr exp))))))

(define c---rs '((car caar . cdar)
                 (cdr cadr . cddr)
                 (caar caaar . cdaar)
                 (cadr caadr . cdadr)
                 (cdar cadar . cddar)
                 (cddr caddr . cdddr)
                 (caaar caaaar . cdaaar)
                 (caadr caaadr . cdaadr)
                 (cadar caadar . cdadar)
                 (caddr caaddr . cdaddr)
                 (cdaar cadaar . cddaar)
                 (cdadr cadadr . cddadr)
                 (cddar caddar . cdddar)
                 (cdddr cadddr . cddddr)))

(define get-c---rs '((caar car . car)
                     (cadr cdr . car)
                     (cdar car . cdr)
                     (cddr cdr . cdr)
                     (caaar caar . car)
                     (caadr cadr . car)
                     (cadar cdar . car)
                     (caddr cddr . car)
                     (cdaar caar . cdr)
                     (cdadr cadr . cdr)
                     (cddar cdar . cdr)
                     (cdddr cddr . cdr)
                     (caaaar caaar . car)
                     (caaadr caadr . car)
                     (caadar cadar . car)
                     (caaddr caddr . car)
                     (cadaar cdaar . car)
                     (cadadr cdadr . car)
                     (caddar cddar . car)
                     (cadddr cdddr . car)
                     (cdaaar caaar . cdr)
                     (cdaadr caadr . cdr)
                     (cdadar cadar . cdr)
                     (cdaddr caddr . cdr)
                     (cddaar cdaar . cdr)
                     (cddadr cdadr . cdr)
                     (cdddar cddar . cdr)
                     (cddddr cdddr . cdr)))

;;!(function stx-dot-dot-k?
;;          (form (stx-dot-dot-k? syn) -> bool)
;;          (contract syntax -> bool)
;;          (example (stx-dot-dot-k? (syntax ..3)) -> #t))
;; This function is a predicate that returns true if the argument
;; is syntax represents a ... or ___ syntax where the last dot or
;; underscore can be an integer
(define stx-dot-dot-k?
  (lambda (syn)
    (dot-dot-k? (syntax-object->datum syn))))

;;!(function implied
;;          (form (implied test) -> list)
;;          (contract s-exp -> list))
;; This function is given a s-expression for a test and returns a
;; list of tests that are implied by that test.  The implied test
;; would have to be true if the argument is true.
(define (implied test)
  (let* ((pred (car test))
         (exp (cadr test)))
    (cond
     ((equal? pred 'equal?)
      (let ((ex (caddr test)))
        (cond ((string? ex)
               (list `(string? ,ex)))
              ((boolean? ex)
               (list `(boolean? ,exp)))
              ((char? ex)
               (list `(char? ,exp)))
              ((number? ex)
               (list `(number? ,exp)))
              ((and (pair? ex)
                    (eq? 'quote (car ex)))
               (list `(symbol? ,exp)))
              (else '()))))
     ((equal? pred 'null?)
      (list `(list? ,exp)))
     (else '()))))
