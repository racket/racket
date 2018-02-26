#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "parsed.rkt"
         "rebuild.rkt")

(provide (struct-out expanded+parsed)
         (struct-out semi-parsed-define-values)
         (struct-out semi-parsed-begin-for-syntax)
         extract-syntax
         parsed-only
         syntax-only)

;; When expanding a module, we may need to compile and instantiate it,
;; too (as or for submodules), so keep both expanded and compiled
;; variants of a form together:
(struct expanded+parsed (s parsed) #:authentic)

;; A `define-values` or `begin-for-syntax-form` is in limbo though
;; some passes.
(struct semi-parsed-define-values (s syms ids rhs) #:authentic)
(struct semi-parsed-begin-for-syntax (s body) #:authentic)

(define (extract-syntax s)
  (if (expanded+parsed? s)
      (expanded+parsed-s s)
      s))

(define (parsed-only l)
  (for/list ([i (in-list l)]
             #:when (or (parsed? i)
                        (expanded+parsed? i)
                        (semi-parsed-begin-for-syntax? i)))
    (cond
     [(expanded+parsed? i)
      (expanded+parsed-parsed i)]
     [(semi-parsed-begin-for-syntax? i)
      (parsed-begin-for-syntax (semi-parsed-begin-for-syntax-s i)
                               (parsed-only (semi-parsed-begin-for-syntax-body i)))]
     [else i])))

(define (syntax-only l)
  (for/list ([i (in-list l)]
             #:when (or (syntax? i)
                        (expanded+parsed? i)
                        (semi-parsed-begin-for-syntax? i)))
    (cond
     [(expanded+parsed? i) (expanded+parsed-s i)]
     [(semi-parsed-begin-for-syntax? i)
      ;; If `l` is after skipping `module*` expansion, then we may
      ;; still have semi-parsed `begin-for-syntax`
      (define s (semi-parsed-begin-for-syntax-s i))
      (define nested-bodys (semi-parsed-begin-for-syntax-body i))
      (let ([disarmed-s (syntax-disarm s)])
        (define-match m disarmed-s '(begin-for-syntax _ ...))
        (rebuild s `(,(m 'begin-for-syntax) ,@(syntax-only nested-bodys))))]
     [else i])))
