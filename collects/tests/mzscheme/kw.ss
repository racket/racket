
(load-relative "loadtest.ss")

(SECTION 'kw)

(require (lib "kw.ss"))

;; make sure that lambda/kw behaves as lambda
(test 1 (lambda/kw () 1))
(test 1 (lambda/kw (x) 1) 0)
(test '() (lambda/kw x x))
(test '(1 2) (lambda/kw x x) 1 2)
(test '(1 2) (lambda/kw (x . xs) xs) 0 1 2)
;; even with keywords
(test #:x (lambda/kw () #:x))
(test #:x (lambda/kw (x) #:x) #:y)
(test '(#:x #:y) (lambda/kw x x) #:x #:y)
(test '(#:x #:y) (lambda/kw (x . xs) xs) #:z #:x #:y)

;; just using #:rest is the same as a dot
(let ([f (lambda/kw (#:rest r) r)])
  (test '()    f)
  (test '(1)   f 1)
  (test '(1 2) f 1 2))
(let ([f (lambda/kw (x #:rest r) r)])
  (test '()    f 0)
  (test '(1)   f 0 1)
  (test '(1 2) f 0 1 2))
