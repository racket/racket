#lang racket/base

(require rackunit racket/match
         (for-syntax racket/base))

(define/match ((curried x) y)
  [((? number? x) y) (+ x y)]
  [((? symbol? x) y) x])

(check-equal? ((curried 3) 5) 8)
(check-equal? ((curried 'foo) 5) 'foo)

(define/match (fact n)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])

(check-equal? (fact 0) 1)
(check-equal? (fact 5) 120)

(define/match (foo #:bar [bar 5] [baz 7])
  [(5 7) #t]
  [(_ _) #f])

(check-true (foo))
(check-true (foo #:bar 5 7))
(check-false (foo #:bar 7 8))

(define/match (foo2 #:qux qux #:bar [bar 5] [baz 7])
  [(1 5 7) #t]
  [(_ _ _) #f])

(check-true (foo2 #:qux 1))
(check-false (foo2 #:qux 2))
(check-true (foo2 #:qux 1 #:bar 5 7))

(define/match (f [x 3] . rst)
  [(3 '(1 2)) #t]
  [(_ _) #f])

(check-true (f 3 1 2))
(check-false (f))
(check-false (f 2))
(check-false (f 2 4 5))

(require (prefix-in legacy: mzlib/match))
(legacy:define/match (fact-2 n)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])

(check-equal? (fact-2 0) 1)
(check-equal? (fact-2 5) 120)

(legacy:define/match (list-fun lst)
  [((1 2 3)) #t]
  [((_ _ _ )) #f])

(check-equal? (list-fun '(1 2 3)) #t)
(check-equal? (list-fun '(4 5 6)) #f)

(struct Foo (x y z w))
(check-true (match (Foo 0 3 2 'last)
              [(Foo 0 [#:z 2] 3 'last) #t]
              [_ #f]))
(check-true (match (Foo 0 3 2 'dropped)
              [(struct Foo #:first (0 [#:z 2] 3)) #t]
              [_ #f]))
(check-true (match (Foo 'dropped 0 2 3)
              [(struct Foo #:last (0 [#:z 2] 3)) #t]
              [_ #f]))
(check-true (match (Foo 'dropped 2 0 3)
              [(struct Foo #:last (0 [#:y 2] 3)) #t]
              [_ #f]))

(define-syntax (check-syntax-error stx)
  (syntax-case stx ()
    [(_ name e exp)
     (with-handlers ([exn:fail:syntax?
                      (λ (e)
                         (if (regexp-match (syntax-e #'exp) (exn-message e))
                             #'(void)
                             #`(fail-check (format "Test ~a failed with different error message: ~a" 'name #,(exn-message e)))))])
       (with-syntax ([e (local-expand #'e 'expression #f)])
         #'(fail-check (format "Test ~a successfully expanded when expecting syntax error" #''name))))]))

(check-syntax-error bad-field-name
                    (match (Foo 0 1 2 3)
                      [(Foo [#:bad 0]) #f])
                    #rx"field name \\(bad\\) not associated with structure Foo")
(check-syntax-error first-mode-name-conflict
                    (match (Foo 0 1 2 3)
                      [(Foo 0 [#:x 0] 1 2) #f])
                    #rx"named field pattern \\(x\\) given after 1 unnamed pattern but the field is in position 0 for structure Foo")
(check-syntax-error last-mode-name-conflict
                    (match (Foo 0 1 2 3)
                      [(struct Foo #:last ([#:w 3] 3)) #f])
                    #rx"named field pattern \\(w\\) given before 1 unnamed pattern but the field is 0 from last for structure Foo in #:last mode")
(check-syntax-error default-full-when-unnamed-given
                    (match (Foo 0 1 2 3)
                      [(Foo [#:w 3] 0) #f])
                    #rx"wrong number of fields for structure Foo: expected 4 but got 2")
