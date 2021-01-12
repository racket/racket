#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "simple.rkt")

(provide unnest-let)

;; Rotate something like
;;
;;  (let ([x (let ([y <simple>])
;;              <rhs>)])
;;    <body>)
;;
;; to
;;
;;  (let ([y <simple>])
;;    (let ([x <rhs>])
;;       <body>)))
;; 
;; to better expose procedure bindings for the lifting phase.
;;
;; For `letrec*`, we rewrite to
;;
;;  (letrec* ([y <simple>]
 ;;           [x <rhs>])
;;    <body>)
;;
;; because <simple> might refer to `x`. We only do that when <simple>
;; and <rhs> are immediate `lambda` forms, though, to avoid
;; pessimizing a set of mutually recursive functions.

(define (unnest-let e prim-knowns knowns imports mutated simples unsafe-mode?)
  (match e
    [`(,let-id (,binds ...) . ,body)
     (cond
       [(or (eq? let-id 'let)
            (eq? let-id 'letrec*))
        (let loop ([binds binds]
                   [accum-binds '()]
                   [wraps '()]
                   [convert? #f])
          (cond
            [(null? binds)
             (if (not convert?)
                 e
                 (let loop ([wraps wraps] [e `(,let-id ,(reverse accum-binds) . ,body)])
                   (cond
                     [(null? wraps) e]
                     [else
                      (loop (cdr wraps)
                            `(,(caar wraps) ,(cdar wraps) ,e))])))]
            [else
             (match (car binds)
               [`[,id (,nest-let-id ([,ids ,rhss] ...)
                                    ,body)]
                (cond
                  [(not (or (eq? let-id 'let)
                            (immediate-lambda? body)))
                   e]
                  [(and (or (eq? 'let nest-let-id)
                            (and (eq? 'letrec* nest-let-id)
                                 (not (could-be-loop? ids body))))
                        (for/and ([rhs (in-list rhss)])
                          (and (or (eq? 'let let-id)
                                   (immediate-lambda? rhs))
                               (simple? rhs prim-knowns knowns imports mutated simples unsafe-mode?))))
                   (match (car binds)
                     [`[,_ (,_ ,inner-binds ,_)]
                      (cond
                        [(eq? 'let let-id)
                         ;; let: can lift out
                         (loop (cdr binds)
                               (cons `[,id ,body] accum-binds)
                               (cons (cons nest-let-id
                                           inner-binds)
                                     wraps)
                               #t)]
                        [else
                         ;; letrec: need to keep in same set of bindings
                         (loop (cdr binds)
                               (cons `[,id ,body] (append inner-binds accum-binds))
                               wraps
                               #t)])])]
                  [else (loop (cdr binds)
                              (cons (car binds) accum-binds)
                              wraps
                              convert?)])]
               [`[,_ ,rhs]
                (if (or (eq? let-id 'let)
                        (immediate-lambda? rhs))
                    (loop (cdr binds)
                          (cons (car binds) accum-binds)
                          wraps
                          convert?)
                    e)])]))]
       [else e])]
    [`,_ e]))

(define (immediate-lambda? e)
  (match e
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`,_ #f]))

(define (could-be-loop? ids body)
  (and (pair? ids)
       (null? (cdr ids))
       (wrap-pair? body)
       (eq? (unwrap (car ids))
            (unwrap (wrap-car body)))))
