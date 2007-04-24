(module match-helper mzscheme
  
  (provide (all-defined)
           (all-from "syntax-utils.ss"))
  
  (require (lib "struct.ss" "syntax")
           "syntax-utils.ss"
           "match-error.ss"
           (lib "list.ss"))
  
  (require-for-template mzscheme)
  
  ;; define a syntax-transformer in terms of a two-argument function  
  (define-syntax define-proc 
    (syntax-rules ()
      [(_ nm func)
       (define-syntax (nm stx) (func stx stx))]))
  
  ;; bind an identifier to be syntax/loc with a particular location, in an expression
  (define-syntax md-help
    (syntax-rules ()
      [(md-help id stx e)
       (let-syntax ([id (syntax-rules () [(id arg) (syntax/loc stx arg)])])
         e)]))
  
  (define (constant-data? v)
    (or
     (string? v)
     (boolean? v)
     (char? v)
     (number? v)
     (keyword? v)))
  
  
  ;;!(function symbol-append
  ;;          (form (symbol-append . args) -> symbol)
  ;;          (contract ((symbol or number) ...) -> symbol)
  ;;          (example (symbol-append 'hello 5 'goodbye) -> 'hello5goodbye))
  ;; This function takes any number of arguments which can be either
  ;; symbols or numbers and returns one symbol which is the
  ;; concatenation of the input.
  (define (symbol-append . l)
    (define (data->string x)
      (cond
        [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [else x]))      
    (string->symbol (apply string-append (map data->string l))))
  
  ;;!(function struct-pred-accessors-mutators
  ;;          (form (struct-pred-accessors-mutators struct-name)
  ;;                ->
  ;;                (values pred accessors mutators parental-chain))
  ;;          (contract (syntax-object)
  ;;                     ->
  ;;                     (values (any -> bool) list list list)))
  ;; This function takes a syntax-object that is the name of a structure.
  ;; It returns four values.  The first is
  ;; a predicate for the structure.  The second is a list of accessors
  ;; in the same order as the fields of the structure declaration.  The
  ;; third is a list of mutators for the structure also in the same
  ;; order.  The last is a list of supertypes of this struct. An
  ;; error is raised if the struct-name is not bound to a
  ;; structure.
  (define (struct-pred-accessors-mutators struct-name)
    (define accessors-index 3)
    (define mutators-index 4)
    (define pred-index 2)
    (define super-type-index 5)
    (define (failure-thunk)
      (match:syntax-err struct-name 
                        "not a defined structure"))
    (define (local-val sn) (syntax-local-value sn failure-thunk))
    ;; accessor/mutator lists are stored in reverse order, and can contain #f
    ;; we only filter out a mutator if the accessor is also false.
    ;; this function returns 2 lists of the same length if the inputs were the same length
    (define (handle-acc/mut-lists accs muts)
      (let*-values ([(filtered-lists) (filter (lambda (x) (car x)) (map list accs muts))]
                    [(accs muts) (values (map car filtered-lists)
                                         (map cadr filtered-lists))])
        (values (reverse accs)
                (reverse muts))))
    
    ;; this produces a list of all the super-types of this struct
    ;; ending when it reaches the top of the hierarchy, or a struct that we can't access
    (define (get-lineage struct-name)
      (let ([super (list-ref 
                    (local-val struct-name)
                    super-type-index)])
        (cond [(equal? super #t) '()] ;; no super type exists
              [(equal? super #f) '()] ;; super type is unknown
              [else (cons super (get-lineage super))])))
    
    (define info-on-struct (local-val struct-name))
    
    (define (ref-info i) (list-ref info-on-struct i))
    
    (unless (struct-declaration-info? info-on-struct) (failure-thunk))
    
    (let*-values ([(acc-list) (ref-info accessors-index)]
                  [(mut-list) (ref-info mutators-index)]
                  [(pred) (ref-info pred-index)]
                  [(accessors mutators) (handle-acc/mut-lists acc-list mut-list)]      
                  [(parental-chain) (get-lineage struct-name)])
      (values pred accessors mutators (cons struct-name parental-chain)))
    )
  
  
  
  
  
  
  ;;!(function in
  ;;          (form (in e l) -> bool)
  ;;          (contract (s-exp list) -> bool)
  ;;          (example (in '(number? x) (list '(number? x))) -> #t))
  ;; This function is responsible for determining which tests are
  ;; redundant.  If e can be determined to be true from the list of
  ;; tests l then e is "in" l.
  (define (in e l)
    (or 
     (ormap
      (lambda (el)
        (or (equal? e el)
            (and 
             (eq? (car e) 'struct-pred)
             (eq? (car el) 'struct-pred)
             (member (caaddr e) (caddr el))
             (equal? (cadddr e) (cadddr el))))) l)
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
              (else #f))))))
  
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
  (define (equal-test? tst)
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
             (else #f)))))
  
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
  (define (disjoint? tst)
    (memq (car tst) match:disjoint-predicates))
  
  (define (vec-structure? tst)
    (memq (car tst) match:vector-structures))

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
  
  
  ;;! (function pattern-var?
  ;;    (form (pattern-var? pattern-element) -> bool)
  ;;    (contract syntax -> bool)
  ;;    (example (pattern-var? #'x) -> #t)
  ;;    )
  ;; This function takes a syntax object and determines if it
  ;; qualifies as a pattern variable.
  (define (pattern-var? x)
    (let ([x (syntax-object->datum x)])
      (and (symbol? x)
           (not (dot-dot-k? x))
           (not (memq x '(_
                          quasiquote
                          quote
                          unquote
                          unquote-splicing
                          ;                      hash-table
                          ;                      list-no-order
                          ;                       list-rest
                          ;                       list
                          ;                       app
                          ;                       struct
                          ;                       var
                          ;                       vector
                          ;                       box
                          ;                       ?
                          ;                       and
                          ;                       or
                          ;                       not
                          ;                       set!
                          ;                       get!
                          ))))))
  
  ;;!(function dot-dot-k?
  ;;          (form (dot-dot-k? s) -> bool)
  ;;          (contract any -> bool)
  ;;          (example (dot-dot-k? '..3) -> 3))
  ;; This function is a predicate that returns the number of elements required 
  ;; by the pattern
  ;; (dot-dot-k? '..3) -> 3
  ;; (dot-dot-k? '...) -> 0
  (define (dot-dot-k? s)
    (define (./_ c)
      (or (equal? c #\.)
          (equal? c #\_)))
    (and (symbol? s)
         (if (memq s '(... ___)) 0
             (let* ((s (symbol->string s)))                    
               (and (<= 3 (string-length s))
                    (./_ (string-ref s 0))
                    (./_ (string-ref s 1))                    
                    (string->number
                     (substring s 2)))))))
  
  
  (define node-count (make-parameter 0))
  
  (define convert-patterns? (make-parameter #f))
  
  (define match-equality-test (make-parameter equal?))
  
  ;; a helper for timing testing
  
  (define-values (print-time initer)
    (let* ((t (current-milliseconds))
           (orig t))
      (values
       (lambda (msg)
         (void)
	 #;(let ((t* (current-milliseconds)))
           (printf "~a: (total: ~a real: ~a diff: ~a)~n" msg (- t* orig) t* (- t* t))
           (set! t t*)))
       (lambda () (void)#;(set! t (current-milliseconds)) #;(set! orig t)))))

  
  )
