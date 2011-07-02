#lang scheme/base
(require "../struct.rkt"
         "../search.rkt"
         "../scheme.rkt"
         "../basic.rkt"
         "manual-scheme.rkt"
         (for-syntax scheme/base))

(provide ;; public:
         method xmethod)
; XXX unknown contracts
(provide *method **method
         method-tag
         name-this-object)

(define-syntax-rule (method a b)
  (*method 'b (quote-syntax a)))

(define-syntax-rule (xmethod a b)
  (elem (method a b) " in " (racket a)))

(define (*method sym id)
  (**method sym id))

(define (**method sym id/tag)
  (let ([content (list (symbol->string sym))])
    ((if (identifier? id/tag)
       (lambda (c mk)
         (make-delayed-element
          (lambda (ren p ri)
            (let ([tag (find-scheme-tag p ri id/tag #f)])
              (if tag (list (mk tag)) content)))
          (lambda () (car content))
          (lambda () (car content))))
       (lambda (c mk) (mk id/tag)))
     content
     (lambda (tag)
       (make-element symbol-color
                     (list (make-link-element value-link-color content
                                              (method-tag tag sym))))))))

(define (method-tag vtag sym)
  (list 'meth (list (cadr vtag) sym)))

(define (name-this-object type-sym)
  (to-element
   (string->symbol
    (regexp-replace
     #rx"(%|<%>|-mixin)$"
     (format "_a~a-~s"
             (if (member (string-ref (symbol->string type-sym) 0)
                         '(#\a #\e #\i #\o #\u))
               "n"
               "")
             type-sym)
     ""))))
