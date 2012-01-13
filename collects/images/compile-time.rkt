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
                 (let-syntax ([maker  (λ (inner-stx) (make-3d-bitmap inner-stx expr))])
                   (maker)))]))

(define-syntax (compiled-bitmap-list stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax/loc stx
       (let-syntax ([maker  (λ (inner-stx)
                              (with-syntax ([(bm (... ...))
                                             (map (λ (e) (make-3d-bitmap inner-stx e)) expr)])
                                #'(list bm (... ...))))])
         (maker)))]))
