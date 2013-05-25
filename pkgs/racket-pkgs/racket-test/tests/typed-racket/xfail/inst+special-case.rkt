#lang typed/racket

;; In tc-app, code paths for annotated operators and special case typing rules
;; (for, e.g., `vector-ref') are mutually exclusive.
;; The annotated operator case matches first and falls back to the regular
;; application rules, skipping the special cases.
;; This example, because it uses a heterogenous vector, needs the special type
;; rule, and therefore doesn't typecheck.

(ann ((inst vector-ref String) #(1.0) 0) Float)
