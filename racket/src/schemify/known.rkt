#lang racket/base

;; Ths module uses `#:omit-define-syntaxes` and doesn't use
;; `struct-out` so that none of the exports are syntax bindings.

(provide known-constant known-constant?
         known-consistent known-consistent?
         known-copy? known-copy known-copy-id
         known-literal known-literal? known-literal-expr
         known-procedure known-procedure? known-procedure-arity-mask
         known-procedure/can-inline known-procedure/can-inline? known-procedure/can-inline-expr
         known-procedure/can-inline/need-imports known-procedure/can-inline/need-imports?
         known-procedure/can-inline/need-imports-needed
         known-procedure/succeeds known-procedure/succeeds?
         known-procedure/pure known-procedure/pure?
         known-procedure/has-unsafe known-procedure/has-unsafe? known-procedure/has-unsafe-alternate
         known-struct-type known-struct-type? known-struct-type-type
         known-struct-type-field-count known-struct-type-pure-constructor?
         known-constructor known-constructor? known-constructor-type
         known-predicate known-predicate? known-predicate-type
         known-accessor known-accessor? known-accessor-type
         known-mutator known-mutator? known-mutator-type
         known-field-accessor known-field-accessor? known-field-accessor-type-id known-field-accessor-pos
         known-field-mutator known-field-mutator? known-field-mutator-type-id known-field-mutator-pos
         known-field-accessor/need-imports known-field-accessor/need-imports? known-field-accessor/need-imports-needed
         known-field-mutator/need-imports known-field-mutator/need-imports? known-field-mutator/need-imports-needed
         known-struct-type-property/immediate-guard known-struct-type-property/immediate-guard?
         a-known-constant
         a-known-consistent)

;; reflects an immutable variable, but nothing is known about the
;; variable's value
(struct known-constant () #:prefab #:omit-define-syntaxes)

;; the value at run time always has the same "shape", such as always being
;; a procedure of 1 argument, always being a structure type, or always
;; being a predicate for a structure type
(struct known-consistent () #:prefab #:omit-define-syntaxes #:super struct:known-constant)

;; copy propagation --- use for local bindings or copies of primitives, only
(struct known-copy (id) #:prefab #:omit-define-syntaxes #:super struct:known-constant)

;; literal for constant propagation:
(struct known-literal (expr) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

;; procedure with arity mark; the procedure has to be a procedure from the host
;; Scheme's perspective --- not an applicable struct or chaperoned procedure, which
;; means that parameters don't count
(struct known-procedure (arity-mask) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

(struct known-procedure/can-inline (expr) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-procedure/can-inline/need-imports (needed) ; (list (cons <sym> (cons <sym> <#f-or-index>)) ...)
  #:prefab #:omit-define-syntaxes #:super struct:known-procedure/can-inline)

;; procedure that never raises an exception or otherwise captures/escapes the calling context
(struct known-procedure/succeeds () #:prefab #:omit-define-syntaxes #:super struct:known-procedure)

;; procedure that accepts any arguments and is functional so that it can be reordered
(struct known-procedure/pure () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/succeeds)

;; procedure with an unsafe variant, especially ones that won't get substituted
;; simply by compiling in unsafe mode
(struct known-procedure/has-unsafe (alternate) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)

(struct known-struct-type (type field-count pure-constructor?) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

;; procedures with a known connection to a structure type:
(struct known-constructor (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure)
(struct known-predicate (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure)
(struct known-accessor (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-mutator (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-field-accessor (type-id pos) #:prefab #:omit-define-syntaxes #:super struct:known-accessor)
(struct known-field-mutator (type-id pos) #:prefab #:omit-define-syntaxes #:super struct:known-mutator)
(struct known-field-accessor/need-imports (needed) #:prefab #:omit-define-syntaxes #:super struct:known-field-accessor)
(struct known-field-mutator/need-imports (needed) #:prefab #:omit-define-syntaxes #:super struct:known-field-mutator)

(struct known-struct-type-property/immediate-guard () #:prefab #:omit-define-syntaxes)

(define a-known-constant (known-constant))
(define a-known-consistent (known-consistent))
