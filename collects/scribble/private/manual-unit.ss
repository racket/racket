#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../basic.ss"
         "manual-utils.ss"
         "manual-vars.ss"
         "manual-style.ss"
         "manual-scheme.ss"
         "manual-bind.ss"
         "manual-ex.ss"
         "manual-proc.ss"
         scheme/contract
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide defsignature
         defsignature/splice
         sigelem)

(define-syntax-rule (defsignature name (super ...) body ...)
  (with-togetherable-scheme-variables
   ()
   ()
   (*defsignature (quote-syntax name)
                  (list (quote-syntax super) ...)
                  (lambda () (list body ...))
                  #t)))

(define-syntax-rule (defsignature/splice name (super ...) body ...)
  (with-togetherable-scheme-variables
   ()
   ()
   (*defsignature (quote-syntax name)
                  (list (quote-syntax super) ...)
                  (lambda () (list body ...))
                  #f)))

(define-struct sig-desc (in))
(define (signature-desc . l)
  (make-sig-desc l))

(provide/contract
 [signature-desc (() () #:rest (listof pre-flow?) . ->* . sig-desc?)])

(define (*defsignature stx-id supers body-thunk indent?)
  (*defthing
   (list stx-id)
   (list (syntax-e stx-id))
   #t
   (list (make-element #f '("signature")))
   (lambda ()
     (define in
       (parameterize ([current-signature (make-sig stx-id)]) (body-thunk)))
     (if indent?
       (let-values ([(pre-body post-body)
                     (let loop ([in in][pre-accum null])
                       (cond [(null? in) (values (reverse pre-accum) null)]
                             [(whitespace? (car in))
                              (loop (cdr in) (cons (car in) pre-accum))]
                             [(sig-desc? (car in))
                              (loop (cdr in)
                                    (append (reverse (sig-desc-in (car in)))
                                            pre-accum))]
                             [else (values (reverse pre-accum) in)]))])
         `(,@pre-body
           ,(make-blockquote
             "leftindent"
             (flow-paragraphs (decode-flow post-body)))))
       in))))
