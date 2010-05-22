#lang scheme/base
(require (for-syntax scheme/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MULTIPLE VALUES TOOLS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(vl expr)
     (syntax/loc stx
       (call-with-values (lambda () expr) list))]))

(define (map/list n f ls)
  (cond
   [(andmap null? ls) (build-list n (lambda (i) null))]
   [(andmap pair? ls)
    (let* ([vs (values->list (apply f (map car ls)))]
           [k (length vs)])
      (unless (= k n)
        (error 'map/values
               "~a produced ~a values, not ~a: ~e"
               f k n vs))
      (map cons vs (map/list n f (map cdr ls))))]
   [else (error 'map/values "list lengths differ")]))

(define (map/values n f . ls)
  (apply values (map/list n f ls)))

(define (map2 f . ls)
  (apply values (map/list 2 f ls)))

(define (foldr/list f vs ls)
  (cond
   [(andmap null? ls) vs]
   [(andmap pair? ls)
    (values->list
     (apply
      f
      (append
       (map car ls)
       (foldr/list f vs (map cdr ls)))))]
   [else (error 'foldr/values "list lengths differ")]))

(define (foldr/values f vs . ls)
  (apply values (foldr/list f vs ls)))

(define (foldl/list f vs ls)
  (cond
   [(andmap null? ls) vs]
   [(andmap pair? ls)
    (foldl/list
     f
     (values->list (apply f (append (map car ls) vs)))
     (map cdr ls))]
   [else (error 'foldl/values "list lengths differ")]))

(define (foldl/values f vs . ls)
  (apply values (foldl/list f vs ls)))

(provide map2 map/values foldr/values foldl/values values->list)
