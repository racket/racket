#lang racket/base
;; owner: ryanc (and cce and stamourv, where noted)
(require racket/syntax
         (for-syntax racket/base)
         (for-template racket/base)
         syntax/transformer ; for re-export
         syntax/location) ; for re-export

(provide ;; by endobson
         syntax-length

         ;; by stamourv:
         format-unique-id
         syntax-within?

         ;; by ryanc
         explode-module-path-index
         phase-of-enclosing-module

         ;; re-export, for backwards compatibility
         make-variable-like-transformer
         syntax-source-file-name
         syntax-source-directory)

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

;; by endobson

(define (syntax-length stx)
  (let ((list (syntax->list stx)))
    (and list (length list))))
