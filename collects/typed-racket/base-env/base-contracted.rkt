#lang racket/base

;; This file provides Typed Racket bindings for values that need
;; contract protection, even in typed code.

(require "../utils/utils.rkt"
         (env init-envs)
         (types abbrev union)
         (utils any-wrap)
         (only-in (rep type-rep)
                  make-Prompt-Tagof))

;; this submodule defines the contracted versions
(module contracted racket/base
  (require racket/contract
           (rename-in
            racket/base
            [default-continuation-prompt-tag -default-continuation-prompt-tag])
           "../utils/utils.rkt"
           (utils any-wrap))

  (provide default-continuation-prompt-tag)

  ;; default tag should use Any wrappers
  (define default-continuation-prompt-tag
    (contract (-> (prompt-tag/c any-wrap/c #:call/cc any-wrap/c))
              -default-continuation-prompt-tag
              'typed 'untyped)))

(require (for-template (submod "." contracted))
         (submod "." contracted))

(provide default-continuation-prompt-tag)

;; set up the type environment
(define-initial-env initialize-contracted
  [default-continuation-prompt-tag
    (-> (make-Prompt-Tagof Univ (-> ManyUniv Univ)))])
