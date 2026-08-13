#lang racket/base

;; Tests for `pattern-bound-identifiers` (provided for-syntax by racket/match).

(require racket/match rackunit racket/list
         (for-syntax racket/base racket/match))

(provide pattern-bound-identifiers-tests)

;; Compute, at compile time, the sorted symbols bound by PAT in the
;; standard match syntax.
(define-syntax (bound-syms stx)
  (syntax-case stx ()
    [(_ pat)
     (with-syntax ([(id ...) (pattern-bound-identifiers #'pat)])
       #'(sort (map symbol->string (list 'id ...)) string<?))]))

(define pattern-bound-identifiers-tests
  (test-suite
   "pattern-bound-identifiers"
   (test-case "list pattern binds each variable"
     (check-equal? (bound-syms (list a b c)) '("a" "b" "c")))
   (test-case "literal pattern binds nothing"
     (check-equal? (bound-syms (list 1 2 3)) '()))
   (test-case "wildcard binds nothing"
     (check-equal? (bound-syms _) '()))
   (test-case "single identifier binds itself"
     (check-equal? (bound-syms x) '("x")))
   (test-case "cons pattern"
     (check-equal? (bound-syms (cons hd tl)) '("hd" "tl")))
   (test-case "and / predicate sub-patterns"
     (check-equal? (bound-syms (and x (? number? n))) '("n" "x")))
   (test-case "nested and ellipsis"
     (check-equal? (bound-syms (list (list a b) ...)) '("a" "b")))
   (test-case "duplicate (non-linear) identifiers are de-duplicated"
     (check-equal? (bound-syms (list same same)) '("same")))
   (test-case "box and vector patterns"
     (check-equal? (bound-syms (box b)) '("b"))
     (check-equal? (bound-syms (vector v1 v2)) '("v1" "v2")))
   (test-case "app pattern binds result"
     (check-equal? (bound-syms (app add1 r)) '("r")))
   (test-case "result is a list of identifiers"
     ;; Sanity: the for-syntax call returns identifiers, not symbols.
     (define-syntax (all-ids? stx)
       (syntax-case stx ()
         [(_ pat)
          (datum->syntax #'here
                         (andmap identifier? (pattern-bound-identifiers #'pat)))]))
     (check-true (all-ids? (list a b))))
   (test-case "non-syntax argument is rejected"
     ;; The error is raised at expansion time, so check via `expand`.
     (check-exn exn:fail:syntax?
                (lambda ()
                  (expand #'(let ()
                              (define-syntax (oops s)
                                (pattern-bound-identifiers 'not-syntax))
                              (oops))))))))
