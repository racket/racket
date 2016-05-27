#lang racket/base
(require racket/unit
         racket/contract
	 "sig.rkt"
	 compiler/embed 
	 "embed-sig.rkt")

(define-unit-from-context compiler:embed@ compiler:embed^)
(provide compiler:embed@)
