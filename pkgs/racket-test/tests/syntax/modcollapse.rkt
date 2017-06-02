#lang racket/base
(require syntax/modcollapse)

(define (check got expected)
  (unless (equal? got expected)
    (error 'check "failed: ~s vs. ~s" got expected)))

(define here-dir (find-system-path 'temp-dir))
(define here (build-path here-dir "dummy.rkt"))

(define self (module-path-index-join #f #f))

(check (collapse-module-path-index self)
       #f)
(check (collapse-module-path-index self here)
       here)

(define (check-collapse p expected [relative-expected expected]
                        #:here [here here])
  (check (collapse-module-path p here)
         expected)
  
  (define i (module-path-index-join p self))
  (check (collapse-module-path-index i here)
         expected)
  (check (collapse-module-path-index i)
         relative-expected)
  
  (define i2 (module-path-index-join p #f))
  (check (collapse-module-path-index i2 here)
         expected)
  (check (collapse-module-path-index i2)
         relative-expected))

(check-collapse "local.rkt"
                (build-path here-dir "local.rkt")
                "local.rkt")

(check-collapse (string->path "local.rkt")
                (build-path here-dir "local.rkt")
                (string->path "local.rkt"))

(check-collapse (path->complete-path "local.rkt")
                (path->complete-path "local.rkt"))

(check-collapse '(file "local.rkt")
                (build-path here-dir "local.rkt")
                '(file "local.rkt"))

(define (check-racket-lib p)
  (check-collapse p '(lib "racket/main.rkt")))
(check-racket-lib 'racket)
(check-racket-lib '(lib "racket"))
(check-racket-lib '(lib "racket/main.rkt"))

(check-collapse '(planet foo/bar)
                '(planet "main.rkt" ("foo" "bar.plt")))

(check-collapse '(submod "." test)
                `(submod ,here test)
                '(submod "." test))

(check-collapse '(submod "local.rkt" test)
                `(submod ,(build-path here-dir "local.rkt") test)
                '(submod "local.rkt" test))
(check-collapse '(submod "local.rkt" test)
                `(submod ,(build-path here-dir "local.rkt") test)
                `(submod "local.rkt" test)
                #:here `(submod ,here other))

(define rel-rel (module-path-index-join
                 "apple.rkt"
                 (module-path-index-join
                  "banana.rkt"
                  self)))
(check (collapse-module-path-index rel-rel)
       "apple.rkt")
(check (collapse-module-path-index rel-rel
                                   here)
       (build-path here-dir "apple.rkt"))

(define rel-rel/p (module-path-index-join
                   "apple.rkt"
                   (module-path-index-join
                    (string->path "banana.rkt")
                    self)))
(check (collapse-module-path-index rel-rel/p)
       (build-path 'same "apple.rkt"))
(check (collapse-module-path-index rel-rel/p
                                   here)
       (build-path here-dir "apple.rkt"))

(define rel-rel/f (module-path-index-join
                   "apple.rkt"
                   (module-path-index-join
                    '(file "banana.rkt")
                    self)))
(check (collapse-module-path-index rel-rel/f)
       (build-path 'same "apple.rkt"))
(check (collapse-module-path-index rel-rel/f
                                   here)
       (build-path here-dir "apple.rkt"))

(define rel/f-rel (module-path-index-join
                   '(file "apple.rkt")
                   (module-path-index-join
                    "banana.rkt"
                    self)))
(check (collapse-module-path-index rel/f-rel)
       (build-path 'same "apple.rkt"))
(check (collapse-module-path-index rel/f-rel
                                   here)
       (build-path here-dir "apple.rkt"))


(define submod-submod (module-path-index-join
                       '(submod ".." test)
                       (module-path-index-join
                        '(submod "." inner)
                        self)))
(check (collapse-module-path-index submod-submod)
       '(submod "." inner ".." test))
(check (collapse-module-path-index submod-submod
                                   here)
       `(submod ,here test))

(define submod-submod-foo (module-path-index-join
                           '(submod ".." test)
                           (module-path-index-join
                            '(submod "." inner)
                            (module-path-index-join
                             "foo.rkt"
                             self))))
(check (collapse-module-path-index submod-submod-foo)
       '(submod "foo.rkt" inner ".." test))
(check (collapse-module-path-index submod-submod-foo
                                   here)
       `(submod ,(build-path here-dir "foo.rkt") test))


(define dirrel (module-path-index-join
                "x/banana.rkt"
                self))
(check (collapse-module-path-index dirrel)
       "x/banana.rkt")
(check (collapse-module-path-index dirrel
                                   here)
       (build-path here-dir "x/banana.rkt"))

(define rel-dirrel (module-path-index-join
                    "apple.rkt"
                    (module-path-index-join
                     "x/banana.rkt"
                     self)))
(check (collapse-module-path-index rel-dirrel)
       "x/apple.rkt")
(check (collapse-module-path-index rel-dirrel
                                   here)
       (build-path here-dir "x" "apple.rkt"))

