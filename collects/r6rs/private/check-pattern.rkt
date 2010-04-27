#lang scheme/base

(require (for-template (only-in scheme/base ...)))

(provide check-pat-ellipses)

(define ((check-pat-ellipses orig-stx) stx)
  (let loop ([stx stx][car-ok? #f])
    (cond
     [(syntax? stx) (loop (syntax-e stx) car-ok?)]
     [(pair? stx)
      (if (and (not car-ok?)
               (identifier? (car stx))
               (free-identifier=? (car stx) (quote-syntax ...)))
          (raise-syntax-error #f
                              "ellipsis without preceding form"
                              orig-stx
                              (car stx))
          (begin (loop (car stx) #f)
                 (loop (cdr stx) #t)))]
     [(vector? stx)
      (for-each (lambda (stx) (loop stx #f)) (vector->list stx))]
     [else (void)])))


