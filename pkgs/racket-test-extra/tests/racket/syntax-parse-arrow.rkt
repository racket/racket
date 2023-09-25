#lang racket/base

(require syntax/parse
         drracket/check-syntax)

(provide code stx check-syntax-annotations)

(define code
  (string-append
   "(module anonymous racket/base\n"
   "    (require syntax/parse)\n"
   "    (lambda (stx)\n"
   "      (syntax-parse stx\n"
   "        [e #'e]\n"
   "        [e #'e])))\n"))

(define stx
  (read-syntax "<racket-test-extra>/tests/racket/dummy-file.rkt"
               (open-input-string code)))

(define check-syntax-annotations
  (show-content stx))

(module+ test
  (require racket/match rackunit)
  ;; check that #'e points to e at L13
  (check-match check-syntax-annotations
               `(,_ ...
                 #(syncheck:add-arrow/name-dup/pxpy 108 109 ,_ ,_ 112 113 ,_ ,_ ,_ ...)
                 ,_ ...))
  ;; check that #'e points to e at L14
  (check-match check-syntax-annotations
               `(,_ ...
                 #(syncheck:add-arrow/name-dup/pxpy 124 125 ,_ ,_ 128 129 ,_ ,_ ,_ ...)
                 ,_ ...)))
