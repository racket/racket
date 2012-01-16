#lang racket/base

(require (for-syntax racket/base racket/class racket/draw)
         racket/class racket/draw)

(provide compiled-bitmap compiled-bitmap-list)

(define-for-syntax (make-3d-bitmap ctxt bm)
  (define p (open-output-bytes))
  (send bm save-file p 'png)
  (with-syntax ([bs  (datum->syntax ctxt (get-output-bytes p))])
    (syntax/loc ctxt
      (make-object bitmap% (open-input-bytes bs) 'png/alpha))))

(define-syntax (compiled-bitmap stx)
  (syntax-case stx ()
    [(_ expr)  (syntax/loc stx
                 (let-syntax ([maker  (λ (inner-stx)
                                        (define bm expr)
                                        (unless (is-a? bm bitmap%)
                                          (raise-syntax-error
                                           'compiled-bitmap
                                           (format "expected argument of type <bitmap%>; given ~e" bm)
                                           #'expr))
                                        (make-3d-bitmap inner-stx bm))])
                   (maker)))]))

(define-syntax (compiled-bitmap-list stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker  (λ (inner-stx)
                              (define bms expr)
                              (unless (and (list? bms) (andmap (λ (bm) (is-a? bm bitmap%)) bms))
                                (raise-syntax-error
                                 'compiled-bitmap-list
                                 (format "expected argument of type <list of bitmap%>; given ~e" bms)
                                 #'expr))
                              (with-syntax ([(bm (... ...))
                                             (map (λ (e) (make-3d-bitmap inner-stx e)) bms)])
                                #'(list bm (... ...))))])
         (maker)))]))
