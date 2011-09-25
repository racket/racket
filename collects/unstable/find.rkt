#lang racket/base
(require racket/contract/base
         unstable/struct)

(provide/contract
 [find
  (->* ((-> any/c any/c)
        any/c)
       (#:stop-on-found? any/c
        #:stop (or/c #f (-> any/c any/c))
        #:get-children (or/c #f (-> any/c (or/c #f list?))))
       list?)]
 [find-first
  (->* ((-> any/c any/c)
        any/c)
       (#:stop (or/c #f (-> any/c any/c))
        #:get-children (or/c #f (-> any/c (or/c #f list?)))
        #:default any/c)
       any/c)])

(define (find pred x
              #:stop-on-found? [stop-on-found? #f]
              #:stop [stop #f]
              #:get-children [get-children #f])
  (define (loop x acc)
    (cond [(pred x)
           (let ([acc (cons x acc)])
             (if stop-on-found?
                 acc
                 (loop/nf x acc)))]
          [else
           (loop/nf x acc)]))
  ;; loop/nt: x is "not found"; look in its children
  (define (loop/nf x acc)
    (cond [(and stop (stop x))
           acc]
          [(and get-children (get-children x))
           => (lambda (children) (loop* children acc))]
          [(pair? x)
           (let ([acc (loop (car x) acc)])
             (loop (cdr x) acc))]
          [(vector? x)
           (for/fold ([acc acc]) ([elem (in-vector x)])
             (loop elem acc))]
          [(box? x)
           (loop (unbox x) acc)]
          [(struct->list x #:on-opaque 'skip)
           => (lambda (elems)
                (loop* elems acc))]
          ;; unreachable, since
          ;; (struct->list X #:on-opaque 'skip) always returns a list
          [else acc]))
  (define (loop* xs acc)
    (for/fold ([acc acc]) ([elem (in-list xs)])
      (loop elem acc)))
  (reverse (loop x null)))
;; Eli: This looks borderline too generic to be useful, also in the fact that
;;   the documentation tends to explain things in terms of the implementation
;;   (eg, the description of #:stop).  In any case, you should definitely
;;   rename it -- `find' is too common in different ways (see srfi-1 or cltl).

(define (find-first pred x
                    #:stop [stop #f]
                    #:get-children [get-children #f]
                    #:default [default #f])
  (let/ec return
    (define (pred* x)
      (and (pred x) (return x)))
    (find pred* x #:stop stop #:get-children get-children)
    (if (procedure? default)
        (default)
        default)))
