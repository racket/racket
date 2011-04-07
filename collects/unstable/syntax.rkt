#lang racket/base
;; owner: ryanc (and cce and stamourv, where noted)
(require racket/syntax
         syntax/stx)

(provide (rename-out [stx-map syntax-map])
         syntax-list

         ;; by cce:

         syntax-source-file-name
         syntax-source-directory

         ;; by stamourv:

         format-unique-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  By Carl Eastlund, below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Pattern Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (syntax-list template ...)
  (syntax->list (syntax (template ...))))

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
