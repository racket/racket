#lang racket/base
(require (for-syntax racket/base
                     "../../parse.ss"
                     "../private/rep-data.rkt"
                     "../private/kws.rkt")
         "../private/runtime-progress.rkt"
         "../private/runtime.rkt")
(provide define-primitive-splicing-syntax-class)

(define-syntax (define-primitive-splicing-syntax-class stx)

  (define-syntax-class attr
    (pattern name:id
             #:with depth #'0)
    (pattern [name:id depth:nat]))

  (syntax-parse stx
    [(dssp (name:id param:id ...)
       (~or (~once (~seq #:attrs (a:attr ...))
                   #:name "attributes declaration")
            (~once (~seq #:description description)
                   #:name "description declaration")) ...
       proc:expr)
     #'(begin
         (define (get-description param ...)
           description)
         (define parser
           (lambda (x cx pr es fh cp success param ...)
             (let ([stx (datum->syntax cx x cx)])
               (let ([result
                      (let/ec escape
                        (cons 'ok
                              (proc stx
                                    (lambda ([msg #f] [stx #f])
                                      (escape (list 'error msg stx))))))])
                 (case (car result)
                   ((ok)
                    (apply success
                           ((mk-check-result pr 'name '(a.name ...) x cx fh cp) (cdr result))))
                   ((error)
                    (let ([es
                           (list* (cons (expect:thing (get-description param ...) #f) stx)
                                  (cons (expect:message (cadr result)) (caddr result))
                                  es)])
                      (fh (failure pr es)))))))))
         (define-syntax name
           (make-stxclass 'name (arity (length '(param ...)) (length '(param ...)) '() '())
                          '(#s(attr a.name a.depth #f) ...)
                          (quote-syntax parser)
                          #t
                          #s(options #t #t)
                          #f)))]))

(define (mk-check-result pr name attr-names x cx fh cp)
  (lambda (result)
    (unless (list? result)
      (error name "parser returned non-list"))
    (let ([rlength (length result)])
      (unless (= rlength (+ 2 (length attr-names)))
        (error name "parser returned list of wrong length; expected length ~s, got ~e"
               (+ 2 (length attr-names))
               result))
      ;; Ignore (car result), supposed to be rest-x
      ;; Easier to recompute it and get rest-cx right, too.
      (let ([skip (cadr result)])
        (unless (exact-nonnegative-integer? skip)
          (error name "expected exact nonnegative integer for second element of result list, got ~e"
                 skip))
        (let-values ([(rest-x rest-cx) (stx-list-drop/cx x cx skip)])
          (list* fh cp rest-x rest-cx (ps-add-cdr pr skip)
                 (cddr result)))))))
