#lang racket/base

;; ---------------------------------------------------------------------------------------------------
;; provides functions for specifying the shape of big-bang and universe clauses:

(provide function-with-arity expr-with-check err)

;; ... and for checking and processing them 

(provide ;; constraint: the first kw is the original one 
         ;; and it is also the name of the field in the class
         ->args
         contains-clause?)

(require racket/function
         racket/list
         racket/bool
         (for-syntax racket/base syntax/parse)
         (for-template "clauses-spec-aux.rkt"
                       racket
                       (rename-in lang/prim (first-order->higher-order f2h))))

;; ---------------------------------------------------------------------------------------------------
;; specifying the shape of clauses 

(define-syntax (expr-with-check stx)
  (syntax-case stx ()
    [(_ check> msg)
     #`(lambda (tag)
         (lambda (p)
           (syntax-case p ()
             [(_ x) #`(check> #,tag x)]
             [_ (err tag p msg)])))]))

(define-syntax function-with-arity
  (syntax-rules ()
    [(_ arity)
     (lambda (tag)
       (lambda (p [tag tag])
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           [_ (err tag p)])))]
    [(_ arity #:except extra ...)
     (lambda (tag)
       (lambda (p [tag tag])
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           extra ...
           [_ (err tag p)])))]))

(define (err spec p . xtras)
  (define x (cadr spec))
  (raise-syntax-error (if (syntax? x) (syntax-e x) x)
                      (if (null? xtras)
                          "illegal specification"
                          (string-append "illegal specification: " (car xtras)))
                      p))

;; ---------------------------------------------------------------------------------------------------
;; processing actual clauses 

;; KeyWord [Listof Clause] -> Boolean 
;; does this list of clauses contain one that starts with kw?
(define (contains-clause? kw clause-list)
  (memf (lambda (clause) (free-identifier=? kw (car (syntax->list clause)))) clause-list))

#|
  transform the clauses into the initial arguments specification 
  for a new expression that instantiates the appropriate class
  
  ensure that the initial state (state0) is not in the shape of a clause

  ensure that all clauses mention only keywords specified in AllSpec or PartSpec
  move the contracts from AppSpecl and PartSpec to the clauses 
  
  run ->rec? over all used keywords to discover the presence of special clauses
  
  if anything fails, use the legal keyword to specialize the error message
|#
(define (->args tag stx state0 clauses Spec ->rec?)
  (define kwds (map (compose (curry datum->syntax stx) car) Spec))
  (define spec (clauses-use-kwd (syntax->list clauses) ->rec? tag kwds))
  (duplicates? tag spec)
  (not-a-clause tag stx state0 kwds)
  (map (lambda (s) 
         (define kw (first s))
         (define kw-alt (second s))
         (define r
           (let loop ([spec spec])
             (cond
               [(null? spec) #false]
               [(or (free-identifier=? (caar spec) kw)
                    (free-identifier=? (caar spec) kw-alt))
                ; (syntax->list (cdar spec))
                (datum->syntax
                 #f
                 (for/list ([i (syntax->list (cdar spec))])
                   (define n  (string->symbol (format "~a handler" (syntax-e (caar spec)))))
                   (syntax-property  i 'inferred-name n))
                 (cdar spec))]
               [else (loop (cdr spec))])))
         (if r 
             (let ([f (third s)])
               (if (procedure-arity-includes? f 2)
                   (f r `',(car (syntax->list r)))
                   (f r)))
             (fourth s)))
       Spec))

;; check whether rec? occurs, produces list of keyword x clause pairs 
(define (clauses-use-kwd stx:list ->rec? tag kwds)
  (define kwd-in? (->kwds-in kwds))
  (map (lambda (stx)
         (syntax-case stx ()
           [(kw . E) (kwd-in? #'kw) (begin (->rec? #'kw #'E) (cons #'kw stx))]
           [(kw . E)
	    (let* ([stx2 #'kw]
		   [kw (syntax-e stx2)]
		   [kw-appears-as-symbol
		     (member kw (map syntax-e kwds))
		     #;
		     (for/or ((n kwds))
		       (symbol=? kw (syntax-e n)))])
              (if kw-appears-as-symbol
                  (raise-syntax-error
		    tag (format "the ~a keyword seems to have been used as a variable" kw) stx2)
                  (raise-syntax-error
		    tag (format "~a clauses are not allowed within ~a" kw tag) stx)))]
           [_ (raise-syntax-error tag "expected a clause, but found something else" stx)]))
       stx:list))


;; [Listof SyntaxIdentifier] -> (Syntax -> Boolean)
(define (->kwds-in kwds)
  (lambda (k)
    (and (identifier? k) (for/or ([n kwds]) (free-identifier=? k n)))))

;; Symbol Syntax Syntax [Listof Kw] -> true
;; effect: if state0 looks like a clause, raise special error 
(define (not-a-clause tag stx state0 kwds)
  (syntax-case state0 ()
    [(kw . E) 
     ((->kwds-in kwds) #'kw) 
     (raise-syntax-error tag "expected an initial state, but found a clause" stx)]
    [_ #t]))

;; Symbol [Listof kw] -> true
;; effect: raise syntax error about duplicated clause 
(define (duplicates? tag lox)
  (let duplicates? ([lox lox])
    (cond
      [(empty? lox) false]
      [else
       (let* ([f (caar lox)]
              [id (syntax-e f)]
              [x (memf (lambda (x) (free-identifier=? (car x) f)) (rest lox))])
         (if x 
             (raise-syntax-error tag (format "duplicate ~a clause" id) (cdar x))
             (duplicates? (rest lox))))])))
