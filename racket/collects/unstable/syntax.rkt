#lang racket/base
;; owner: ryanc (and cce and stamourv, where noted)
(require racket/syntax
         (for-syntax racket/base)
         (for-template racket/base))

(provide ;; by endobson
         syntax-length

         ;; by cce:
         syntax-source-file-name
         syntax-source-directory

         ;; by stamourv:
         format-unique-id
         syntax-within?

         ;; by ryanc
         explode-module-path-index
         phase-of-enclosing-module
         make-variable-like-transformer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  By Carl Eastlund, below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Locations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-source-directory stx)
  (let* ([source (syntax-source stx)])
    (and (path-string? source)
         (let-values ([(base file dir?) (split-path source)])
           (and (path? base)
                (path->complete-path base
                                     (or (current-load-relative-directory)
                                         (current-directory))))))))

(define (syntax-source-file-name stx)
  (let* ([f (syntax-source stx)])
    (and (path-string? f)
         (let-values ([(base file dir?) (split-path f)]) file))))

;; by stamourv:

(define (format-unique-id lctx
                          #:source [src #f]
                          #:props [props #f]
                          #:cert [cert #f]
                          fmt . args)
  ((make-syntax-introducer) (apply format-id
                                   lctx #:source src #:props props #:cert cert
                                   fmt args)))

;; is syntax a contained within syntax b, inclusively
(define (syntax-within? a b)
  (let ([pos-a  (syntax-position a)]
        [span-a (syntax-span a)]
        [pos-b  (syntax-position b)]
        [span-b (syntax-span b)])
    (and pos-a span-a pos-b span-b
         (<= pos-b pos-a)
         (>= (+ pos-b span-b) (+ pos-a span-a)))))


;; by ryanc

(define (explode-module-path-index mpi)
  (let-values ([(x y) (module-path-index-split mpi)])
    (cons x
          (if (module-path-index? y)
              (explode-module-path-index y)
              (list y)))))

(define-syntax-rule (phase-of-enclosing-module)
  (variable-reference->module-base-phase
   (#%variable-reference)))

(define (make-variable-like-transformer ref-stx [set!-handler #f])
  (unless (syntax? ref-stx)
    (raise-type-error 'make-variable-like-transformer "syntax?" ref-stx))
  (unless (or (syntax? set!-handler) (procedure? set!-handler) (eq? set!-handler #f))
    (raise-type-error 'make-variable-like-transformer "(or/c syntax? procedure? #f)" set!-handler))
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [id
        (identifier? #'id)
        ref-stx]
       [(set! id val)
        (cond [(procedure? set!-handler)
               (set!-handler stx)]
              [(syntax? set!-handler)
               (with-syntax ([setter set!-handler])
                 (syntax/loc stx (setter val)))]
              [else
               (raise-syntax-error #f "cannot mutate identifier" stx #'id)])]
       [(id . args)
        (let ([stx* (cons #'(#%expression id) (cdr (syntax-e stx)))])
          (datum->syntax stx stx* stx))]))))

;; by endobson

(define (syntax-length stx)
  (let ((list (syntax->list stx)))
    (and list (length list))))
