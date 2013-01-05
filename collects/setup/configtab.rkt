#lang racket/base
;; Defines a language to be used by the "config.rkt" file

(require racket/promise
         (for-syntax racket/base))

(provide (rename-out [config-module-begin #%module-begin])
         define
         #%datum quote)

;; These are the name that need to be provided
;;  by the "config.rkt" library:
(define-for-syntax path-exports
  '(doc-dir
    doc-search-dirs
    dll-dir
    lib-dir
    lib-search-dirs
    include-dir
    include-search-dirs
    bin-dir))
(define-for-syntax string-exports
  '(cgc-suffix
    3m-suffix))
(define-for-syntax flag-exports
  '(absolute-installation?))

;; ----------------------------------------
;; For configure into into absolute paths

(define use-default (delay #f))

(define (to-path l)
  (cond [(string? l) (complete-path (string->path l))]
        [(bytes? l) (complete-path (bytes->path l))]
        [(list? l) (map to-path l)]
        [else l]))

(define (complete-path p)
  (cond [(complete-path? p) p]
        [(absolute-path? p) (exe-relative p)]
        [else
         (or (parameterize ([current-directory (find-system-path 'orig-dir)])
               (find-executable-path (find-system-path 'exec-file) p))
             (exe-relative p))]))

(define (exe-relative p)
  (let ([exec (path->complete-path 
               (find-executable-path (find-system-path 'exec-file))
               (find-system-path 'orig-dir))])
    (let-values ([(base name dir?) (split-path exec)])
      (path->complete-path p base))))

;; ----------------------------------------
;;  module-begin

(define-syntax config-module-begin
  (lambda (stx)
    (syntax-case stx (define define-values)
      [(_ (define-values (name) val))
       ;; This can happen because a lone definition is expanded
       #'(config-module-begin (define name val))]
      [(_ (define name val) ...)
       (let ([names (syntax->list #'(name ...))])
         (unless (andmap identifier? names)
           (raise-syntax-error #f "bad syntax" stx))
         (for-each (lambda (name)
                     (unless (or (memq (syntax-e name) path-exports)
                                 (memq (syntax-e name) string-exports)
                                 (memq (syntax-e name) flag-exports))
                       (raise-syntax-error #f "not a config name" name)))
                   names)
         (let ([syms (map syntax-e names)])
           (let loop ([names names][syms syms])
             (cond [(null? names) 'done]
                   [(memq (car syms) (cdr syms))
                    (raise-syntax-error #f "duplicate definition" (car names))]
                   [else (loop (cdr names) (cdr syms))]))
           (with-syntax ([(expr ...)
                          (map (lambda (name val)
                                 (cond
                                  [(memq (syntax-e name) path-exports)
                                   #`(delay (to-path #,val))]
                                  [(memq (syntax-e name) string-exports)
                                   #`(delay #,val)]
                                  [else val]))
                               (syntax->list #'(name ...))
                               (syntax->list #'(val  ...)))])
             (define ((mkdef v) id)
               (if (memq id syms)
                   '()
                   (list #`(define #,(datum->syntax stx id stx) (thwart-inline #,v)))))
             (syntax-property
              #`(#%plain-module-begin
                 (provide #,@path-exports #,@string-exports #,@flag-exports)
                 (define name expr) ...
                 #,@(apply append (map (mkdef #'use-default) path-exports))
                 #,@(apply append (map (mkdef #'use-default) string-exports))
                 #,@(apply append (map (mkdef #'#f) flag-exports)))
              'certify-mode 'transparent))))])))

;; Hack: we want to be able to replace "config.rkt" and recompile it
;; in-place, without recompiling bytecode that may depend on it.
;; To enable that, ensure that the exports of a configuration are
;; not inlined.
(define thwart-inline #f)
(set! thwart-inline (lambda (v) v))
