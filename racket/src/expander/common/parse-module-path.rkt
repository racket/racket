#lang racket/base

(provide module-path?)

;; This parser for module paths is written in a relatively primitive
;; style, becaue it's applied often and we want it to be fast.

(define (module-path? v)
  (or (and (pair? v)
           (eq? (car v) 'submod)
           (submodule-module-path? v))
      (root-module-path? v)))

(define (root-module-path? v)
  (or (path? v)
      (and (string? v)
           (string-module-path? v))
      (and (symbol? v)
           (symbol-module-path? v))
      (and (pair? v)
           (case (car v)
             [(quote) (and (pair? (cdr v))
                           (symbol? (cadr v))
                           (null? (cddr v)))]
             [(lib) (lib-module-path? v)]
             [(file) (and (pair? (cdr v))
                          (string? (cadr v))
                          (path-string? (cadr v))
                          (null? (cddr v)))]
             [(planet) (planet-module-path? v)]
             [else #f]))))

(define (submodule-module-path? v)
  (and (pair? (cdr v))
       (list? v)
       (or (equal? (cadr v) "..")
           (equal? (cadr v) ".")
           (root-module-path? (cadr v)))
       (for/and ([e (in-list (cddr v))])
         (or (equal? e "..")
             (symbol? e)))))

(define (string-module-path? v)
  (module-path-string? v #:dots-dir-ok? #t #:just-file-ok? #t #:file-end-ok? #t))

(define (symbol-module-path? v)
  (module-path-string? (symbol->string v)))

(define (lib-module-path? v)
  (and (list? v)
       (pair? (cdr v))
       (let loop ([v (cdr v)] [first? #t])
         (or (null? v)
             (and (string? (car v))
                  (module-path-string? (car v)
                                       #:just-file-ok? first?
                                       #:file-end-ok? first?)
                  (loop (cdr v) #f))))))

(define (planet-module-path? v)
  (and (list? v)
       (case (length v)
         [(1) #f]
         [(2)
          (define e (cadr v))
          (cond
           [(string? e)
            (module-path-string? e
                                 #:for-planet? #t
                                 #:file-end-ok? #t)]
           [(symbol? e)
            (module-path-string? (symbol->string e)
                                 #:for-planet? #t)]
           [else #f])]
         [else
          (define file (cadr v))
          (define pkg (caddr v))
          (define subs (cdddr v))
          (and file
               (module-path-string? file
                                    #:just-file-ok? #t
                                    #:file-end-ok? #t)
               (and (list? pkg)
                    (<= 2 (length pkg) 4)
                    (planet-user/pkg-string? (car pkg))
                    (planet-user/pkg-string? (cadr pkg))
                    (or (null? (cddr pkg))
                        (planet-version-number? (caddr pkg))
                        (and (or (null? (cddr pkg))
                                 (planet-version-minor-spec? (cadddr pkg))))))
               (for/and ([sub (in-list subs)])
                 (module-path-string? sub)))])))

(define (planet-version-number? v)
  (exact-nonnegative-integer? v))
  
(define (planet-version-minor-spec? v)
  (or (planet-version-number? v)
      (and (pair? v)
           (list? v)
           (= 2 (length v))
           (case (car v)
             [(= + -)
              (planet-version-number? (cadr v))]
             [else
              (and (planet-version-number? (car v))
                   (planet-version-number? (cadr v)))]))))

;; ----------------------------------------

(define (module-path-string? v
                             #:for-planet? [for-planet? #f]
                             #:dots-dir-ok? [dots-dir-ok? #f]
                             #:just-file-ok? [just-file-ok? #f]
                             #:file-end-ok? [file-end-ok? #f])
  (define len (string-length v))
  (and (positive? len)
       (not (char=? #\/ (string-ref v 0)))
       (not (char=? #\/ (string-ref v (sub1 len))))
       (let-values ([(start-package-version-pos end-package-version-pos)
                     (if for-planet?
                         (check-planet-part v len)
                         (values 0 0))])
         (and
          start-package-version-pos
          (let loop ([i (sub1 len)]
                     [prev-was-slash? #f]
                     [saw-slash? (not file-end-ok?)]
                     [saw-dot? #f])
            (cond
             [(not (negative? i))
              ;; check next character
              (define c (string-ref v i))
              (cond
               [(char=? c #\/)
                (and (not prev-was-slash?)
                     (loop (sub1 i) #t #t saw-dot?))]
               [(char=? c #\.)
                (if (and ((add1 i) . < . len)
                         (not (char=? (string-ref v (add1 i)) #\/))
                         (not (char=? (string-ref v (add1 i)) #\.)))
                    (and (not saw-slash?) ; can't have suffix on a directory
                         (loop (sub1 i) #f saw-slash? #t))
                    (loop (sub1 i) #f saw-slash? saw-dot?))]
               [(or (plain-char? c)
                    (and (char=? c #\%)
                         ((+ i 2) . < . len)
                         (hex-sequence? v (add1 i))))
                (loop (sub1 i) #f saw-slash? saw-dot?)]
               [(and (i . >= . start-package-version-pos) (i . < . end-package-version-pos))
                ;; We've already checked characters in the package-version range
                (loop (sub1 i) #f saw-slash? saw-dot?)]
               [else #f])]
             [else
              ;; checked all characters
              (and
               ;; can't have a file name with no directory
               (not (and (not just-file-ok?)
                         saw-dot?
                         (not saw-slash?)))
               
               (or dots-dir-ok?
                   ;; double-check for delimited "." or ".."
                   (let loop ([i 0])
                     (cond
                      [(= i len) #t]
                      [(char=? (string-ref v i) #\.)
                       (and
                        ;; not "."
                        (not (or (= len (add1 i))
                                 (char=? (string-ref v (add1 i)) #\/)))
                        ;; not ".."
                        (not (and (char=? (string-ref v (add1 i)) #\.)
                                  (or (= len (+ i 2))
                                      (char=? (string-ref v (+ i 2)) #\/))))
                        ;; Skip over "."s:
                        (loop (let loop ([i i])
                                (if (char=? #\. (string-ref v i))
                                    (loop (add1 i))
                                    i))))]
                      [else (loop (add1 i))]))))]))))))

(define (planet-user/pkg-string? v)
  (and (string? v)
       (let ([len (string-length v)])
         (and (positive? len)
              (for/and ([c (in-string v)]
                        [i (in-naturals)])
                (or (plain-char? c)
                    (char=? #\. c)
                    (and (char=? #\% c)
                         (i . < . (- len 2))
                         (hex-sequence? v (add1 i)))))))))

(define (plain-char? c)
  (or (char<=? #\a c #\z)
      (char<=? #\A c #\Z)
      (char<=? #\0 c #\9)
      (char=? #\- c)
      (char=? #\_ c)
      (char=? #\+ c)))

(define (hex-sequence? s i)
  (define c1 (string-ref s i))
  (define c2 (string-ref s (add1 i)))
  (and (hex-char? c1)
       (hex-char? c2)
       (let ([c (integer->char (+ (* (hex-char->integer c1) 16)
                                  (hex-char->integer c2)))])
         (not (plain-char? c)))))
         
(define (hex-char? c)
  (or (char<=? #\a c #\f)
      (char<=? #\0 c #\9)))

(define (hex-char->integer c)
  (cond
   [(char<=? #\a c #\f)
    (- (char->integer c) (+ 10 (char->integer #\a)))]
   [(char<=? #\A c #\F)
    (- (char->integer c) (+ 10 (char->integer #\A)))]
   [else
    (- (char->integer c) (char->integer #\0))]))

;; ----------------------------------------

(define (check-planet-part v len)
  ;; Must have at least two slashes, and a version spec is allowed between them
  (define-values (start-package-version-pos end-package-version-pos colon1-pos colon2-pos)
    (let loop ([j 0]
               [start-package-version-pos #f] [end-package-version-pos #f]
               [colon1-pos #f] [colon2-pos #f])
      (cond
       [(= j len) (values start-package-version-pos (or end-package-version-pos j)
                          colon1-pos colon2-pos)]
       [else
        (case (string-ref v j)
          [(#\/)
           (loop (add1 j)
                 (or start-package-version-pos (add1 j))
                 (and start-package-version-pos
                      (or end-package-version-pos j))
                 colon1-pos colon2-pos)]
          [(#\:)
           (cond
            [colon2-pos (values #f #f #f #f)]
            [colon1-pos
             (loop (add1 j)
                   start-package-version-pos end-package-version-pos
                   colon1-pos j)]
            [else
             (loop (add1 j)
                   start-package-version-pos end-package-version-pos
                   j #f)])]
          [else
           (loop (add1 j)
                 start-package-version-pos end-package-version-pos
                 colon1-pos colon2-pos)])])))
  
  (cond
   [(and start-package-version-pos
         (end-package-version-pos . > . start-package-version-pos)
         (or (not colon2-pos) ((add1 colon2-pos) . < . end-package-version-pos)))
    (cond
     [colon1-pos
      ;; Check that the version spec is well-formed
      (define colon1-end (or colon2-pos end-package-version-pos))
      (cond
       [(and (integer-sequence? v (add1 colon1-pos) colon1-end)
             (or (not colon2-pos)
                 (case (string-ref v (add1 colon2-pos))
                   [(#\=)
                    (integer-sequence? v (+ 2 colon2-pos) end-package-version-pos)]
                   [(#\> #\<)
                    (cond
                     [(and ((+ 2 colon2-pos) . < . end-package-version-pos)
                           (char=? #\= (string-ref v (+ colon2-pos 2))))
                      (integer-sequence? v (+ 3 colon2-pos) end-package-version-pos)]
                     [else
                      (integer-sequence? v (+ 2 colon2-pos) end-package-version-pos)])]
                   [else
                    (integer-range-sequence? v (add1 colon2-pos) end-package-version-pos)])))
        ;; Version spec => need to skip a range
        (values colon1-pos end-package-version-pos)]
       [else
        ;; Bad version spec
        (values #f #f)])]
     [else
      ;; No version spec => nothing to skip later
      (values 0 0)])]
   [else
    ;; Bad 'planet path
    (values #f #f)]))

(define (integer-sequence? s start end)
  (and (start . < . end)
       (for/and ([i (in-range start end)])
         (char<=? #\0 (string-ref s i) #\9))))
              
(define (integer-range-sequence? s start end)
  (and (start . < . end)
       (for/and ([i (in-range start end)])
         (define c (string-ref s i))
         (or (char=? c #\-)
             (char<=? #\0 c #\9)))
       (1 . >= . (for/sum ([i (in-range start end)])
                   (if (char=? (string-ref s i) #\-)
                       1
                       0)))))

;; ----------------------------------------

(module+ test
  (define (test ok? v)
    (unless (equal? ok? (module-path? v))
      (error 'module-path?-test "failed ~s; expected ~a" v ok?)))

  (test #t "hello")
  (test #t "hello.rkt")
  (test #f "hello*ss")
  (test #t "hello%2ess")
  (test #t "hello%00ss")
  (test #f "hello%2Ess")
  (test #f "hello%41ss")
  (test #f "hello%4")
  (test #f "hello%")
  (test #f "hello%q0")
  (test #f "hello%0q")
  (test #f "foo.rkt/hello")
  (test #f "foo/")
  (test #f "a/foo/")
  (test #f "/foo.rkt")
  (test #f "/a/foo.rkt")
  (test #f "a/foo.rkt/b")
  (test #t "a/foo%2ess/b")
  (test #t "a/_/b")
  (test #t "a/0123456789+-_/b.---")
  (test #t "a/0123456789+-_/b.-%2e")
  (test #t "../foo.rkt")
  (test #t "x/../foo.rkt")
  (test #t "x/./foo.rkt")
  (test #t "x/.")
  (test #t "x/..")

  (test #f "@")
  (test #f "\0")
  (test #f "x@")
  (test #f "x\0")
  (test #f "@x")
  (test #f "\0x")

  (test #t (collection-file-path "module.rktl" "tests" "racket"))
  (test #t (string->path "x"))

  (test #t 'hello)
  (test #f 'hello/)
  (test #f 'hello.rkt)
  (test #t 'hello%2ess)
  (test #f 'hello%2Ess)
  (test #f 'hello/a.rkt)
  (test #f '/hello/a.rkt)
  (test #f '/hello)
  (test #f '/a/hello)
  (test #f 'a//hello)
  (test #f '../hello)
  (test #f './hello)
  (test #f 'a/../hello)
  (test #f 'b/./hello)
  (test #f 'b/*/hello)

  (test #t '(lib "hello"))
  (test #f '(lib "hello/"))
  (test #f '(lib "hello/../b"))
  (test #t '(lib "hello/a"))
  (test #t '(lib "hello/a.rkt"))
  (test #f '(lib "hello.bb/a.rkt"))
  (test #f '(lib "/hello/a.rkt"))
  (test #t '(lib "hello/a.rkt" "ack"))
  (test #t '(lib "hello/a.rkt" "ack" "bar"))
  (test #t '(lib "hello/a.rkt" "ack/bar"))
  (test #f '(lib "hello/a.rkt" "ack/"))
  (test #f '(lib "hello/a.rkt" "ack" "/bar"))
  (test #f '(lib "hello/a.rkt" "ack" ".."))
  (test #f '(lib "hello/a.rkt" "ack" bar))
  (test #f '(lib "hello/a.rkt"  . bar))
  (test #f '(lib . "hello/a.rkt"))
  (test #f '(lib))

  (test #f '(planet))
  (test #f '(planet robby))
  (test #t '(planet robby/redex))
  (test #t '(planet robby%2e/%2eredex))
  (test #f '(planet robby%2/redex))
  (test #f '(planet robby/redex%2))
  (test #f '(planet robby/redex/))
  (test #f '(planet robby/redex/foo/))
  (test #f '(planet /robby/redex/foo))
  (test #f '(planet robby/redex.plt/foo))
  (test #f '(planet robby/redex/foo.rkt))
  (test #f '(planet robby/redex/foo.rkt/bar))
  (test #f '(planet robby/../foo))
  (test #t '(planet robby/redex/foo))
  (test #t '(planet robby/redex/foo/bar))
  (test #t '(planet robby/redex:7/foo))
  (test #t '(planet robby/redex:7))
  (test #t '(planet robby/redex:7:8/foo))
  (test #t '(planet robby/redex:7:<=8/foo))
  (test #t '(planet robby/redex:7:>=8/foo))
  (test #t '(planet robby/redex:7:8-9/foo))
  (test #t '(planet robby/redex:7:8-9))
  (test #t '(planet robby/redex:700:800-00900/foo))
  (test #t '(planet robby/redex:700:800-00900/foo%2e))
  (test #f '(planet robby/redex:=7/foo))
  (test #f '(planet robby/redex::8/foo))
  (test #f '(planet robby/redex:7:/foo))
  (test #f '(planet robby/redex.plt:7:8/foo))
  (test #f '(planet robby/redex:a/foo))
  (test #f '(planet robby/redex:7:a/foo))
  (test #f '(planet robby/redex:7:a-10/foo))
  (test #f '(planet robby/redex:7:10-a/foo))

  (test #f '(planet "foo.rkt"))
  (test #t '(planet "foo.rkt" ("robby" "redex.plt")))
  (test #f '(planet "../foo.rkt" ("robby" "redex.plt")))
  (test #t '(planet "foo.rkt" ("robby" "redex.plt" 7 (7 8))))
  (test #t '(planet "foo.rkt" ("robby" "redex.plt" 7 8)))
  (test #t '(planet "foo.rkt" ("robby" "redex.plt" 7 (= 8))))
  (test #t '(planet "foo.rkt" ("robby" "redex.plt") "sub" "deeper"))
  (test #t '(planet "foo%2e.rkt" ("robby%2e" "redex%2e.plt") "sub%2e" "%2edeeper"))
  
  (test #t '(submod "."))
  (test #t '(submod "." x))
  (test #t '(submod "." x y))
  (test #t '(submod "." x ".." y))
  (test #t '(submod "." x ".." y ".." ".." ".."))
  (test #f '(submod "." "x" y))
  (test #f '(submod "." x "y"))
  (test #t '(submod ".."))
  (test #t '(submod ".." x))
  (test #t '(submod ".." x y))
  (test #f '(submod ".." "x" y))
  (test #f '(submod ".." x "y"))
  (test #t '(submod ".." ".."))
  (test #f '(submod ".." "."))

  (test #t '(submod x a b))
  (test #f '(submod x "a" b))

  (test #t '(submod 'x a))
  (test #t '(submod 'x))
  
  (printf "Passed all tests\n"))
