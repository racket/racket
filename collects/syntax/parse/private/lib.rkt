#lang racket/base
(require "sc.rkt"
         "keywords.rkt"
         (for-syntax racket/base))

(provide identifier
         boolean
         str
         character
         keyword
         number
         integer
         exact-integer
         exact-nonnegative-integer
         exact-positive-integer

         id
         nat
         char

         expr
         static)

;; == Integrable syntax classes ==

(define-syntax-class identifier
  #:description (quote "identifier")
  (pattern (~fail #:unless (identifier? this-syntax))))

(define-syntax-class keyword
  #:description (quote "keyword")
  (pattern (~fail #:unless (keyword? (syntax-e this-syntax)))))

(define-syntax-class expr
  #:description (quote "expression")
  (pattern (~fail #:when (keyword? (syntax-e this-syntax)))))

;; == Normal syntax classes ==

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes () #:opaque #:commit
    (pattern (~and x (~fail #:unless (pred (syntax-e #'x)))))))

;;(define-pred-stxclass identifier symbol?)
;;(define-pred-stxclass keyword keyword?)
(define-pred-stxclass boolean boolean?)
(define-pred-stxclass character char?)

(define-syntax-class str #:attributes () #:opaque #:commit
  #:description "string"
  (pattern (~and x (~fail #:unless (string? (syntax-e #'x))))))

(define-pred-stxclass number number?)
(define-pred-stxclass integer integer?)
(define-pred-stxclass exact-integer exact-integer?)
(define-pred-stxclass exact-nonnegative-integer exact-nonnegative-integer?)
(define-pred-stxclass exact-positive-integer exact-positive-integer?)

;; Aliases
(define-syntax id (make-rename-transformer #'identifier))
(define-syntax nat (make-rename-transformer #'exact-nonnegative-integer))
(define-syntax char (make-rename-transformer #'character))

(define notfound (box 'notfound))

(define-syntax-class (static pred name)
  #:attributes (value)
  #:description name
  #:commit
  (pattern x:id
           #:fail-unless (syntax-transforming?)
                         "not within the dynamic extent of a macro transformation"
           #:attr value (syntax-local-value #'x (lambda () notfound))
           #:fail-when (eq? (attribute value) notfound) #f
           #:fail-unless (pred (attribute value)) #f))
