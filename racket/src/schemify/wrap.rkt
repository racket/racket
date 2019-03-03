#lang racket/base
(require racket/private/primitive-table
         (for-syntax racket/base))

(provide unwrap unwrap-list
         wrap?
         wrap-pair? wrap-null? wrap-car wrap-cdr wrap-list?
         wrap-eq? wrap-equal?
         in-wrap-list
         wrap-property
         wrap-property-set
         wrap-source
         reannotate
         reannotate/new-srcloc)

(import-from-primitive-table
 #%kernel
 [syntax? correlated?]
 [syntax-e correlated-e]
 [syntax-property correlated-property]
 [datum->syntax datum->correlated]
 [syntax-source correlated-source]
 [syntax-line correlated-line]
 [syntax-column correlated-column]
 [syntax-position correlated-position]
 [syntax-span correlated-span])

(define (unwrap v)
  (if (correlated? v)
      (correlated-e v)
      v))

(define (wrap? v)
  (correlated? v))

(define (unwrap-list v)
  (cond
    [(null? v) null]
    [(pair? v)
     (let ([r (unwrap-list (cdr v))])
       (if (eq? r (cdr v))
           v
           (cons (car v) r)))]
    [(correlated? v) (unwrap-list (correlated-e v))]
    [else v]))

(define (wrap-car v)
  (if (correlated? v)
      (car (correlated-e v))
      (car v)))

(define (wrap-cdr v)
  (if (correlated? v)
      (cdr (correlated-e v))
      (cdr v)))

(define (wrap-pair? v)
  (pair? (unwrap v)))

(define (wrap-null? v)
  (null? (unwrap v)))

(define (wrap-list? v)
  (cond
    [(null? v) #t]
    [(correlated? v) (wrap-list? (correlated-e v))]
    [(pair? v) (wrap-list? (cdr v))]
    [else #f]))

(define (wrap-eq? a b)
  (eq? (unwrap a) (unwrap b)))

(define (wrap-equal? a b)
  (let ([b (unwrap b)])
    (or (and (not (pair? a))
             (equal? a b))
        (and (pair? a)
             (pair? b)
             (wrap-equal? (car a) (car b))
             (wrap-equal? (car a) (car b))))))

(define (wrap-property a key)
  (and (correlated? a)
       (correlated-property a key)))

(define (wrap-property-set a key val)
  (correlated-property a key val))

(define (wrap-source a)
  (cond
    [(correlated? a)
     (values (correlated-source a)
             (correlated-line a)
             (correlated-column a)
             (correlated-position a)
             (correlated-span a))]
    [else (values #f #f #f #f #f)]))

(define (reannotate old-term new-term)
  (if (correlated? old-term)
      (datum->correlated #f new-term old-term old-term)
      new-term))

(define (reannotate/new-srcloc old-term new-term new-srcloc)
  (datum->correlated #f new-term new-srcloc old-term))

(define-sequence-syntax in-wrap-list
  (lambda (stx) (raise-argument-error "allowed only in `for` forms" stx))
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ lst-expr)]
       (for-clause-syntax-protect
        #'[(id)
           (:do-in
            ;;outer bindings
            ([(lst) lst-expr])
            ;; outer check
            (void)
            ;; loop bindings
            ([lst lst])
            ;; pos check
            (not (wrap-null? lst))
            ;; inner bindings
            ([(id) (if (wrap-pair? lst) (wrap-car lst) lst)]
             [(rest) (if (wrap-pair? lst) (wrap-cdr lst) null)])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            (rest))])]
      [_ #f])))
