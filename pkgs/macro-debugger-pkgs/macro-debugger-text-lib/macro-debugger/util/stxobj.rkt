#lang racket/base
(require (rename-in racket/contract/base [-> c:->])
         ffi/unsafe)

(define lib (ffi-lib #f))

(define get-marks
  (get-ffi-obj "scheme_stx_extract_marks" lib
               (_fun _scheme -> _scheme)))

#|
(define (simplify-marks marklist)
  (simplify* (sort marklist <)))

(define (simplify* marklist)
  (cond [(null? marklist) marklist]
        [(null? (cdr marklist)) marklist]
        [(= (car marklist) (cadr marklist))
         (simplify* (cddr marklist))]
        [else
         (let ([result (simplify* (cdr marklist))])
           (if (eq? result (cdr marklist))
               marklist
               (cons (car marklist) result)))]))

(provide simplify-marks)
|#

(provide/contract
 [get-marks
  ;; syntax? check needed for safety!
  (c:-> syntax? any)])
