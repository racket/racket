(require "test-harness.ss"
         ;unit-syntax
         (lib "unit-syntax.ss" "mzlib" "private")
	 )

 ;; check-id
(parameterize ([error-syntax #'check-id])
  (test-runtime-error exn:fail:syntax? "check-id: not id"
    (check-id #'1))
  (test-runtime-error exn:fail:syntax? "check-id: not id"
    (check-id #'(x y)))
  (test bound-identifier=? #'x (check-id #'x)))

 ;; checked-syntax->list
(parameterize ([error-syntax #'checked-syntax->list])
  (test-runtime-error exn:fail:syntax? "checked-syntax->list: dot"
    (checked-syntax->list #'(a b . c)))
  (test-runtime-error exn:fail:syntax? "checked-syntax->list: not list"
    (checked-syntax->list #'a))
  (test lst-bound-id=? (list #'a #'b #'c)
    (checked-syntax->list #'(a b c)))
  (test '()
    (checked-syntax->list #'())))

;; checked-tag
(parameterize([error-syntax #'check-tagged])
  (test-runtime-error exn:fail:syntax? "check-tagged: missing all"
    ((check-tagged (λ (x) x)) #'(tag)))
  (test-runtime-error exn:fail:syntax? "check-tagged: missing syntax"
    ((check-tagged (λ (x) x)) #'(tag a)))
  (test-runtime-error exn:fail:syntax? "check-tagged: too much"
    ((check-tagged (λ (x) x)) #'(tag a b c)))
  (test-runtime-error exn:fail:syntax? "check-tagged: dot"
    ((check-tagged (λ (x) x)) #'(tag a . c)))
  (test-runtime-error exn:fail:syntax? "check-tagged: bad id"
    ((check-tagged (λ (x) x)) #'(tag 1 c)))
  (test stx-bound-identifier=? #'c
    (cdr ((check-tagged (λ (x) x)) #'(tag b c))))
  (test 'b
    (car ((check-tagged (λ (x) x)) #'(tag b c))))
  (test #f
    (car ((check-tagged (λ (x) x)) #'1)))
  (test 1
    (syntax-e (cdr ((check-tagged (λ (x) x)) #'1)))))

;; check-:-clause-syntax
(parameterize ((error-syntax #'check-:-clause-syntax))
  (test-runtime-error exn:fail:syntax? "check-:-clause-syntax: malformed"
    (check-:-clause-syntax #'x))
  (test-runtime-error exn:fail:syntax? "check-:-clause-syntax: malformed"
    (check-:-clause-syntax #'(x y)))
  (test-runtime-error exn:fail:syntax? "check-:-clause-syntax: malformed"
    (check-:-clause-syntax #'(x y z)))
  (test-runtime-error exn:fail:syntax? "check-:-clause-syntax: dot"
    (check-:-clause-syntax #'(x : . z)))
  (test-runtime-error exn:fail:syntax? "check-:-clause-syntax: malformed"
    (check-:-clause-syntax #'(x : z a)))
  
  (test lst-bound-id=? (list #'a #'b)
        (list (car (check-:-clause-syntax #'(a : b)))
              (cdr (check-:-clause-syntax #'(a : b)))))
  )                      

;; check-spec-syntax
(parameterize ((error-syntax #'check-spec-syntax))
  (define (css x) (check-spec-syntax x #t identifier?))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: unknown keyword"
    (css #'(x y)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: dot"
    (css #'(x . y)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: not id"
    (css #'1))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: not id"
    (css #'(only 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: not id"
    (css #'(except 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: not id"
    (css #'(rename 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: not id"
    (css #'(prefix x 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: only, no args"
    (css #'(only)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: only, arg not id"
    (css #'(only test-sig 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: only, dot"
    (css #'(only . test-sig)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: only, dot"
    (css #'(only test-sig x . y)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: except, no args"
    (css #'(except)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: except, arg not id"
    (css #'(except test-sig 1)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: except, dot"
    (css #'(except . test-sig)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: except, dot"
    (css #'(except test-sig x . y)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: prefix, no args"
    (css #'(prefix)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: prefix, one arg"
    (css #'(prefix a)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: prefix, dot"
    (css #'(prefix a . b)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: prefix, too many args"
    (css #'(prefix a b c)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename, no args"
    (css #'(rename)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename, dot"
    (css #'(rename . test-sig)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename, dot"
    (css #'(rename test-sig . (a x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, length"
    (css #'(rename test-sig (x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, length"
    (css #'(rename test-sig (a b x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, dot"
    (css #'(rename test-sig (a . x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, dot"
    (css #'(rename test-sig (a x) (a . x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, id"
    (css #'(rename test-sig (1 x))))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: rename clause, id"
    (css #'(rename test-sig (a 1))))
  (test (void)
    (css #'(prefix x (except (rename (only y))))))
  (test (void)
    (css #'(only (except (rename (prefix x y) (a b) (c d)) e f g) h i j)))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: bad keyword"
    (check-spec-syntax #'(only x) #f identifier?))
  (test-runtime-error exn:fail:syntax? "check-spec-syntax: bad keyword"
    (check-spec-syntax #'(except x) #f identifier?))
  )

;; check-unit-syntax
(parameterize ((error-syntax #'check-unit-syntax))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: no import or export"
    (check-unit-syntax #'()))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: bad import"
    (check-unit-syntax #'((export))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: bad import"
    (check-unit-syntax #'((impor) (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: bad import"
    (check-unit-syntax #'(import (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: no export"
    (check-unit-syntax #'((import))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: bad export"
    (check-unit-syntax #'((import) (expor))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: bad export"
    (check-unit-syntax #'((import) export)))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: malformed body (dot)"
    (check-unit-syntax #'((import) (export) 1 . 2)))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: malformed import (dot)"
    (check-unit-syntax #'((import . a) (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: malformed export (dot)"
    (check-unit-syntax #'((import) (export . a))))
  (test-runtime-error exn:fail:syntax? "check-unit-syntax: malformed init-depend (dot)"
    (check-unit-syntax #'((import) (export) (init-depend . a))))
  (test stx-bound-id=? #'((import a b c) (export a b c) (init-depend) 1 2 3)
        (check-unit-syntax #'((import a b c) (export a b c) 1 2 3)))
  (test stx-bound-id=? #'((import a b c) (export a b c) (init-depend x y) 1 2 3)
        (check-unit-syntax #'((import a b c) (export a b c) (init-depend x y) 1 2 3)))
  (test bound-identifier=? #'init-depend
        (car (syntax-e (caddr (syntax->list (check-unit-syntax #'((import) (export))))))))
  )

;; check-unit-body-syntax
(parameterize ((error-syntax #'check-unit-body-syntax))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: no exp or import or export"
    (check-unit-body-syntax #'()))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: no import or export"
    (check-unit-body-syntax #'(1)))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad import"
    (check-unit-body-syntax #'(1 (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad import"
    (check-unit-body-syntax #'(1 (impor) (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad import"
    (check-unit-body-syntax #'(1 import (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: no export"
    (check-unit-body-syntax #'(1 (import))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad export"
    (check-unit-body-syntax #'(1 (import) (expor))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad export"
    (check-unit-body-syntax #'(1 (import) export)))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: malformed import (dot)"
    (check-unit-body-syntax #'(1 (import . a) (export))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: malformed export (dot)"
    (check-unit-body-syntax #'(1 (import) (export . a))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: malformed init-depend (dot)"
    (check-unit-body-syntax #'(1 (import) (export) (init-depend . a))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad init-depend"
    (check-unit-body-syntax #'(1 (import) (export) (init-depen))))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: too many"
    (check-unit-body-syntax #'(1 (import) (export) (init-depend) x)))
  (test-runtime-error exn:fail:syntax? "check-unit-body-syntax: bad dot"
    (check-unit-body-syntax #'(1 (import) (export) . (init-depend))))
  (test stx-bound-id=? #'(1 (import a b c) (export a b c) (init-depend))
        (check-unit-body-syntax #'(1 (import a b c) (export a b c))))
  (test `(1 (import a b c) (export a b c) (init-depend x y))
        (syntax-object->datum (check-unit-body-syntax #'(1 (import a b c) (export a b c) (init-depend x y)))))
  (test bound-identifier=? #'init-depend
        (car (syntax-e (cadddr (syntax->list (check-unit-body-syntax #'(1 (import) (export))))))))
  )

;; check-link-line-syntax
(parameterize ((error-syntax #'check-link-line-syntax))
  (test-runtime-error exn:fail:syntax? "check-link-line-syntax: malformed"
    (check-link-line-syntax #'1))
  (test-runtime-error exn:fail:syntax? "check-link-line-syntax: bad export list"
    (check-link-line-syntax #'(a b)))
  (test-runtime-error exn:fail:syntax? "check-link-line-syntax: missing unit expression"
    (check-link-line-syntax #'(())))
  (test-runtime-error exn:fail:syntax? "check-link-line-syntax: dot"
    (check-link-line-syntax #'((a . b) u)))
  (test-runtime-error exn:fail:syntax? "check-link-line-syntax: dot"
    (check-link-line-syntax #'((a b) u c . d)))
  (test (void)
    (check-link-line-syntax #'((a b c) u 1 2 3)))
  )

;; check-compound-syntax
(parameterize ((error-syntax #'check-compound-syntax))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: no import, export, or link"
    (check-compound-syntax #'()))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: import malformed"
    (check-compound-syntax #'(import (export) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: import malformed"
    (check-compound-syntax #'((impor) (export) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: import dot"
    (check-compound-syntax #'((import a b . 3) (export) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: missing link and export clause"
    (check-compound-syntax #'((import))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: bad export"
    (check-compound-syntax #'((import) export (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: bad export"
    (check-compound-syntax #'((import) (expor) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: export dot"
    (check-compound-syntax #'((import) (export a . b) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: missing link clause"
    (check-compound-syntax #'((import) (export))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: 2 link clauses"
    (check-compound-syntax #'((import) (export) (link) (link))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: 2 export clauses"
    (check-compound-syntax #'((import) (export) (link) (export))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: 2 import clauses"
    (check-compound-syntax #'((import) (export) (link) (import))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: link dot"
    (check-compound-syntax #'((import) (export) (link a b . 3))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: link clause malformed"
    (check-compound-syntax #'((import) (export) (lnk))))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: link clause malformed"
    (check-compound-syntax #'((import) (export) link)))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: after link clause"
    (check-compound-syntax #'((import) (export) (link) 3)))
  (test-runtime-error exn:fail:syntax? "check-compound-syntax: dot"
    (check-compound-syntax #'((import) (export) (link) . 3)))
  
  (test stx-bound-id=? #'((a b)
                          (c d)
                          (((e f) g h i)
                           (() x)))
    (check-compound-syntax #'((link ((e f) g h i)
                                    (() x))
                              (export c d)
                              (import a b))))
  )

;; check-def-syntax
(parameterize ((error-syntax #'check-def-syntax))
  (test-runtime-error exn:fail:syntax? "define-values: missing ids and expr"
    (check-def-syntax #'(define-values)))
  (test-runtime-error exn:fail:syntax? "define-values: missing expr"
    (check-def-syntax #'(define-values (a))))
  (test-runtime-error exn:fail:syntax? "define-values: 2 expr"
    (check-def-syntax #'(define-values (a) 1 2)))
  (test-runtime-error exn:fail:syntax? "define-values: dot"
    (check-def-syntax #'(define-values (a) . 1)))
  (test-runtime-error exn:fail:syntax? "define-values: bad ids"
    (check-def-syntax #'(define-values x 1)))
  (test-runtime-error exn:fail:syntax? "define-values: bad id"
    (check-def-syntax #'(define-values (1) 1)))
  (test-runtime-error exn:fail:syntax? "define-values: bad id (dot)"
    (check-def-syntax #'(define-values (a . b) 1)))
  (test-runtime-error exn:fail:syntax? "define-syntaxes: bad id (dot)"
    (check-def-syntax #'(define-syntaxes (a . b) 1)))
  
  (test (void)
    (check-def-syntax #'(define-values (a b c) 1)))
  (test (void)
    (check-def-syntax #'(define-values () 1)))
  (test (void)
    (check-def-syntax #'(define-syntaxes (a b c) 1)))
  
  )
