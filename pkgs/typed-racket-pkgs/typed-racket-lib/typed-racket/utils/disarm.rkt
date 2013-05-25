#lang racket/base

(provide disarm*)

;; Typed Racket runs after macro expansion, and it must be priviledged,
;; so it can just disarm all taints (and arm everything afterward).

(define (disarm* stx)
  (let loop ([v stx])
    (cond
     [(syntax? v)
      (let* ([stx (syntax-disarm v orig-insp)]
             [r (loop (syntax-e stx))])
        (if (eq? r (syntax-e stx))
            stx
            (datum->syntax stx r stx stx)))]
     [(pair? v) (let ([a (loop (car v))]
                      [d (loop (cdr v))])
                  (if (and (eq? a (car v))
                           (eq? d (cdr v)))
                      v
                      (cons a d)))]
     [else v])))

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))


