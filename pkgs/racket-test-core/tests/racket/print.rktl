
;; Test printing in as-expression mode

(load-relative "loadtest.rktl")

(Section 'printing)

(require racket/flonum
         racket/fixnum)

(let ([ptest (lambda (s v)
               (define (to-string v)
                 (format "~v" v))
               (define (to-pretty-string v)
                 (pretty-format v))
               (define (to-string/not-expression v)
                 (parameterize ([print-as-expression #f])
                   (to-string v)))
               (test (regexp-replace* #rx"\n *" s " ") to-string v)
               (test s to-pretty-string v)
               (test (format "~s" v) to-string/not-expression v))])
  (define-struct a (x y))
  (define-struct b (x y) #:transparent #:mutable)
  (define-struct c (x y) #:prefab)
  (define (custom-printer get-xy)
    (lambda (v port mode)
      (define-values (d-x d-y) (get-xy))
      (define (recur v)
        (case mode
          [(#f) (display v port)]
          [(#t) (write v port)]
          [else (print v port mode)]))
      (write-char #\< port)
      (write mode port)
      (write-char #\space port)
      (recur (d-x v))
      (write-char #\space port)
      (recur (d-y v))
      (write-char #\> port)))
  (define-struct d (x y) 
    #:property prop:custom-write (custom-printer (lambda () (values d-x d-y)))
    #:property prop:custom-print-quotable 'maybe)
  (define-struct e (x y) 
    #:property prop:custom-write (custom-printer (lambda () (values e-x e-y)))
    #:property prop:custom-print-quotable 'never)
  (define-struct f (x y) 
    #:property prop:custom-write (custom-printer (lambda () (values f-x f-y))))
  (define-struct f2 (x y) 
    #:property prop:custom-write (custom-printer (lambda () (values f2-x f2-y)))
    #:property prop:custom-print-quotable 'self)
  (define-struct g (x y) 
    #:property prop:custom-write (custom-printer (lambda () (values g-x g-y)))
    #:property prop:custom-print-quotable 'always)

  (ptest "1" 1)
  (parameterize ([print-boolean-long-form #f])
    (ptest "#t" #t)
    (ptest "#f" #f))
  (parameterize ([print-boolean-long-form #t])
    (ptest "#true" #t)
    (ptest "#false" #f))
  (ptest "1/2" 1/2)
  (ptest "#f" #f)
  (ptest "#\\x" #\x)
  (ptest "\"apple\"" "apple")
  (ptest "\"\U1f3f4\u200d\u2620\ufe0f\"" "\U1f3f4\u200d\u2620\ufe0f") ; pirate flag

  (ptest "'apple" 'apple)
  (ptest "'|apple banana|" '|apple banana|)
  (ptest "'||" '||)
  (ptest "'|;|" '|;|)
  (ptest "'|`|" '|`|)
  (ptest "'|\uFEFF_|" (string->symbol "\uFEFF_"))
  (ptest "'#:apple" '#:apple)
  (ptest "'#:|apple pie|" '#:|apple pie|)
  (ptest "'#:|.|" '#:|.|)
  (ptest "'#:|#|" '#:|#|)
  (ptest "'#:|#q|" '#:|#q|)
  (ptest "'#:#%" '#:#%)
  (ptest "'#:101" '#:101)
  (ptest "'#%apple" '#%apple)
  (ptest "\"apple\"" "apple")
  (ptest "#\"apple\"" #"apple")
  (ptest "#rx\"apple\"" #rx"apple")
  (ptest "#rx#\"apple\"" #rx#"apple")
  (ptest "'()" '())
  (ptest "#<procedure:add1>" add1)
  (ptest "'<1 o t>" (d 'o 't))
  (ptest "<0 'o 't>" (e 'o 't))
  (ptest "<0 'o 't>" (f 'o 't))
  (ptest "<0 'o 't>" (f2 'o 't))
  (ptest "'<1 o t>" (g 'o 't))

  (ptest "'#&1" (box 1))
  (ptest "'(1 . 2)" (cons 1 2))
  (ptest "'(1 2)" (list 1 2))
  (ptest "'(1 2 . 3)" (list* 1 2 3))
  (ptest "'#(1 2 3)" (vector 1 2 3))

  (ptest "'#hash((1 . 2))" (hash 1 2))
  (ptest "'#hash((1 . 2))" (make-hash (list (cons 1 2))))
  (ptest "'#hasheq((1 . 2))" (hasheq 1 2))
  (ptest "'#hasheq((1 . 2))" (make-hasheq (list (cons 1 2))))
  (ptest "'#hasheqv((1 . 2))" (hasheqv 1 2))
  (ptest "'#hasheqv((1 . 2))" (make-hasheqv (list (cons 1 2))))
  (ptest "'#hashalw((1 . 2))" (hashalw 1 2))
  (ptest "'#hashalw((1 . 2))" (make-hashalw (list (cons 1 2))))

  (ptest "#<stencil 5: \"a\" b>" (stencil-vector 5 "a" 'b))
  (ptest "#<stencil 1: #0=(#0#)>" (stencil-vector 1 (read (open-input-string "#0=(#0#)"))))

  (ptest "(mcons 1 2)" (mcons 1 2))
  (ptest "(mcons 1 '())" (mcons 1 null))

  (ptest "#<a>" (a 1 2))
  (ptest "(b 1 2)" (b 1 2))
  (ptest "'#s(c 1 2)" (c 1 2))
  (test "#s(c 1 2)" 'prefab
        (parameterize ([print-unreadable #f])
          (format "~s" (c 1 2))))

  (let ([s (b 1 2)])
    (ptest "(list (cons (b 1 2) 0) (cons (b 1 2) 0))" (list (cons s 0) (cons s 0))))

  (ptest "'<1 1 2>" (d 1 2))
  (ptest "'<1 1 #<a>>" (d 1 (a 1 2)))
  (ptest "<0 1 (b 1 2)>" (d 1 (b 1 2)))

  (ptest "'#&#<a>" (box (a 1 2)))
  (ptest "(box (b 1 2))" (box (b 1 2)))
  (ptest "'#&#s(c 1 2)" (box (c 1 2)))

  (ptest "'(#<a>)" (list (a 1 2)))
  (ptest "(list (b 1 2))" (list (b 1 2)))
  (ptest "'(#s(c 1 2))" (list (c 1 2)))
  (ptest "'(<1 1 2>)" (list (d 1 2)))
  (ptest "(list <0 1 2>)" (list (e 1 2)))
  (ptest "'(<1 1 2>)" (list (f 1 2)))
  (ptest "'(<1 1 2>)" (list (f2 1 2)))
  (ptest "'(<1 1 2>)" (list (g 1 2)))
  (ptest "(list <0 1 (b 1 2)>)" (list (d 1 (b 1 2))))
  (ptest "(list <0 1 (b 1 2)>)" (list (e 1 (b 1 2))))
  (ptest "'(<1 1 #(struct:b 1 2)>)" (list (f 1 (b 1 2))))
  (ptest "'(<1 1 #(struct:b 1 2)>)" (list (f2 1 (b 1 2))))
  (ptest "'(<1 1 #(struct:b 1 2)>)" (list (g 1 (b 1 2))))

  (ptest "'(0 . #<a>)" (cons 0 (a 1 2)))
  (ptest "(cons 0 (b 1 2))" (cons 0 (b 1 2)))
  (ptest "'(0 . #s(c 1 2))" (cons 0 (c 1 2)))

  (ptest "'#(#<a>)" (vector (a 1 2)))
  (ptest "(vector (b 1 2))" (vector (b 1 2)))
  (ptest "'#(#s(c 1 2))" (vector (c 1 2)))

  (ptest "'#hash((0 . #<a>))" (hash 0 (a 1 2)))
  (ptest "(hash 0 (b 1 2))" (hash 0 (b 1 2)))
  (ptest "'#hash((0 . #s(c 1 2)))" (hash 0 (c 1 2)))

  (ptest "(mcons 0 #<a>)" (mcons 0 (a 1 2)))
  (ptest "(mcons 0 (b 1 2))" (mcons 0 (b 1 2)))
  (ptest "(mcons 0 '#s(c 1 2))" (mcons 0 (c 1 2)))

  (ptest "(list '(1 2) #<a> (b 1 2))" (list '(1 2) (a 1 2) (b 1 2)))
  (ptest "(list* '(1 2) #<a> (b 1 2))" (list* '(1 2) (a 1 2) (b 1 2)))
  (ptest "(vector '(1 2) #<a> (b 1 2))" (vector '(1 2) (a 1 2) (b 1 2)))
  (ptest "(hash '(1 2) (b 1 2))" (hash '(1 2) (b 1 2)))

  (ptest "'(#<procedure:add1>)" (list add1))
  (ptest "'#(#<procedure:add1>)" (vector add1))
  (ptest "#<procedure:add1>" add1)
  (ptest "#<procedure:x->y>" (procedure-rename add1 'x->y))
  (ptest "#<procedure:#x,y>" (procedure-rename add1 '|#x,y|))
  (let ()
    (struct a (x))
    (ptest "#<procedure:a->x>" (procedure-rename a-x 'a->x)))

  (ptest "(arity-at-least 1)" (arity-at-least 1))

  (ptest "#0='(#0#)" (read (open-input-string "#0=(#0#)")))
  (ptest "#0='(#0# #0#)" (read (open-input-string "#0=(#0# #0#)")))
  (ptest "#0='(#0# . #0#)" (read (open-input-string "#0=(#0# . #0#)")))
  (ptest "#0='(#0# #0# . #0#)" (read (open-input-string "#0=(#0# #0# . #0#)")))
  (ptest "#0='#(#0# #0#)" (read (open-input-string "#0=#(#0# #0#)")))
  (ptest "#0='#&#0#" (read (open-input-string "#0=#&#0#")))
  (ptest "#0=(list (b 1 2) #0#)" (let ([v (make-placeholder #f)])
                                   (placeholder-set! v (list (b 1 2) v))
                                   (make-reader-graph v)))
  (ptest "#0=(list* (b 1 2) 8 #0#)" (let ([v (make-placeholder #f)])
                                      (placeholder-set! v (list* (b 1 2) 8 v))
                                      (make-reader-graph v)))
  (ptest "#0=(cons (b 1 2) #0#)" (let ([v (make-placeholder #f)])
                                   (placeholder-set! v (cons (b 1 2) v))
                                   (make-reader-graph v)))
  (ptest "#0=(vector (b 1 2) #0#)" (let ([v (make-placeholder #f)])
                                     (placeholder-set! v (vector (b 1 2) v))
                                     (make-reader-graph v)))
  (ptest "#0='#s(c 1 #0#)" (let ([v (make-placeholder #f)])
                             (placeholder-set! v (c 1 v))
                             (make-reader-graph v)))
  (ptest "#0=(c (b 1 2) #0#)" (let ([v (make-placeholder #f)])
                                (placeholder-set! v (c (b 1 2) v))
                                (make-reader-graph v)))

  (ptest "'(apple\n  \"0000000000000000000000000000000000000000000000000000000000000000000000\")"
         (list 'apple (make-string 70 #\0)))
  (ptest "'#(apple\n   \"0000000000000000000000000000000000000000000000000000000000000000000000\")"
         (vector 'apple (make-string 70 #\0)))
  (ptest "'(apple\n  .\n  \"0000000000000000000000000000000000000000000000000000000000000000000000\")"
         (cons 'apple (make-string 70 #\0)))
  (ptest "'#hash((apple\n        .\n        \"0000000000000000000000000000000000000000000000000000000000000000000000\"))"
         (hash 'apple (make-string 70 #\0)))

  (ptest "(list\n (b 1 2)\n \"00000000000000000000000000000000000000000000000000000000000000000\")"
         (list (b 1 2) (make-string 65 #\0)))
  (ptest "(cons\n (b 1 2)\n \"00000000000000000000000000000000000000000000000000000000000000000\")"
         (cons (b 1 2) (make-string 65 #\0)))
  (ptest "(vector\n (b 1 2)\n \"00000000000000000000000000000000000000000000000000000000000000000\")"
         (vector (b 1 2) (make-string 65 #\0)))
  (ptest "(mcons\n (b 1 2)\n \"00000000000000000000000000000000000000000000000000000000000000000\")"
         (mcons (b 1 2) (make-string 65 #\0)))
  (ptest "(hash\n (b 1 2)\n \"00000000000000000000000000000000000000000000000000000000000000000\")"
         (hash (b 1 2) (make-string 65 #\0)))
  (ptest "(box\n (b 1 \"00000000000000000000000000000000000000000000000000000000000000000000\"))"
         (box (b 1 (make-string 68 #\0))))

  (ptest "#0='(#0#\n     \"iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii\"\n     #0#)" 
         (read (open-input-string "#0=(#0# \"iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii\" #0#)")))

  (ptest "''a" ''a)
  (ptest "'`a" '`a)
  (ptest "'`,#,#`a" '`,#,#`a)
  (ptest "'`,#,#`,@#,@a" '`,#,#`,@#,@a)
  (ptest "'`,#,#`,@, @a" '`,#,#`,@, @a)
  (ptest "'`,#,#`,@#, @a" '`,#,#`,@#, @a)
  (ptest "'`,#,#`,@,|@a,|" '`,#,#`,@,|@a,|)
  (ptest "'`,#,#`,@#,|@a,|" '`,#,#`,@#,|@a,|)

  (ptest "(fxvector 1 10000 3)" (fxvector 1 10000 3))
  (ptest "(flvector 1.1 10000.1 3.1 0.0)" (flvector 1.1 10000.1 3.1 0.0))

  (void))

(let ([in-string (lambda (f v)
                   (let ([o (open-output-bytes)])
                     (f v o)
                     (get-output-string o)))])
  (test "Π" in-string write 'Π) ;; UTF-8 encoding can be misinterpreted as having a space
  (test "Σ" in-string write 'Σ) ;; interesting because it's special-casing
  (test "ς" in-string write 'ς) ;; also special-casing
  (test "σ" in-string write 'σ)
  (test "|a\xA0b|" in-string write (string->symbol "a\xA0b"))
  (parameterize ([read-case-sensitive #f])
    (test "|Π|" in-string write 'Π)
    (test "|Σ|" in-string write 'Σ)
    (test "σ" in-string write 'σ)
    (test "|ς|" in-string write 'ς))
  (parameterize ([read-accept-bar-quote #f])
    (test "Π" in-string write 'Π)
    (test "Σ" in-string write 'Σ)
    (test "a\\\xA0b" in-string write (string->symbol "a\xA0b"))))

;; ----------------------------------------

(test #t unquoted-printing-string? (unquoted-printing-string "a b"))
(test #f unquoted-printing-string? "a b")
(test #f unquoted-printing-string? 7)

(test "a b" unquoted-printing-string-value (unquoted-printing-string "a b"))

(test "a b" format "~s" (unquoted-printing-string "a b"))
(test "a b" format "~a" (unquoted-printing-string "a b"))
(test "a b" format "~v" (unquoted-printing-string "a b"))
(parameterize ([error-print-width 10])
  (test "a b1234..." format "~.s" (unquoted-printing-string "a b12345678"))
  (test "a b1234..." format "~.a" (unquoted-printing-string "a b12345678"))
  (test "a b1234..." format "~.v" (unquoted-printing-string "a b12345678"))
  (test "who: oops\n  field: a b12345678\n"
        'raise-arguments-error
        (parameterize ([current-error-port (open-output-bytes)]
                       [error-print-context-length 0])
          (call-with-continuation-prompt
           (lambda ()
             (raise-arguments-error 'who "oops" "field" (unquoted-printing-string "a b12345678")))
           (default-continuation-prompt-tag)
           void)
          (get-output-string (current-error-port)))))

;; Check GC interaction:
(let ([l (for/list ([i 100])
           (unquoted-printing-string "1 2 3"))])
  (collect-garbage)
  (for ([ups (in-list l)])
    (test "1 2 3" format "~s" ups)))

;; ----------------------------------------

(let ([p (build-path (current-directory) "something")])
  ;; path value in compiled code => path appears in .zo format:
  (let ([o (open-output-string)])
    (write (compile p) o)
    (test #t 'path-in-code?1 (regexp-match? (regexp-quote (path->bytes (current-directory))) (get-output-string o))))
  ;; `current-write-relative-directory' set => path not in .zo format: 
  (let ([o (open-output-string)])
    (parameterize ([current-write-relative-directory (current-directory)])
      (write (compile p) o)
    (test #f 'path-in-code?2 (regexp-match? (regexp-quote (path->bytes (current-directory))) (get-output-string o)))))
  ;; try all possible supers that have at least two path elements:
  (let loop ([super (current-directory)])
    (let ([super (let-values ([(base name dir?) (split-path super)])
                   (if (eq? base 'root)
                       #f
                       base))])
      (when (and super
                 ((length (explode-path super)) . >= . 2))
        ;; `current-write-relative-directory' set => super can be in .zo format: 
        (let ([o (open-output-string)])
          (parameterize ([current-write-relative-directory (current-directory)])
            (write (compile (build-path super "other")) o)
            (test #t 'path-in-code?3 (regexp-match? (regexp-quote (path->bytes super)) (get-output-string o)))))
        (let ([o (open-output-string)])
          (parameterize ([current-write-relative-directory (cons (current-directory)
                                                                 super)])
            (write (compile (build-path super "other")) o)
            (test #f 'path-in-code?4 (regexp-match? (regexp-quote (path->bytes super)) (get-output-string o)))))
        (loop super)))))

;; ----------------------------------------
;; make sure the minimum value (3) is ok for `error-print-width':
(parameterize ([error-print-width 3])
  (test "..." format "~.a" "abcd")
  (test "abc" format "~.a" "abc")
  (test "ab" format "~.a" "ab")
  (test 3 error-print-width))

(parameterize ([error-print-width 3])
  (struct show-a ()
    #:property prop:custom-write
    (lambda (self port mode)
      (fprintf port "a")))
  (struct show-nothing ()
    #:property prop:custom-write
    (lambda (self port mode)
      (void)))
  (test "a" format "~e" (show-a))
  (test "..." format "~e" (list (show-a)))
  (test "" format "~e" (show-nothing))
  (test "'()" format "~e" (list (show-nothing))))

;; ----------------------------------------
;; make sure +inf.0, 3, and 0 are ok for `print-syntax-width':
(parameterize ([print-syntax-width +inf.0])
  (test +inf.0 print-syntax-width))
(parameterize ([print-syntax-width 0])
  (test 0 print-syntax-width))
(parameterize ([print-syntax-width 3])
  (test 3 print-syntax-width))

;; ----------------------------------------
;; Try to provoke a stack overflow during printing of truncated
;; values, ensuring that the stack-overflow handling doesn't
;; interfere with the printer escape on truncation:

(with-handlers ([void void])
  (let loop ([i 6013])
    (if (zero? i)
        (break-thread)
        (with-handlers ([void (lambda (x y) x)])
          (loop (sub1 i))))))

;; ----------------------------------------
;; Check that printing a hash table doesn't crash
;; if the table changes while it's being printed
;; (or checked for cycles):

(let ()
  (define ht (make-hash))

  (struct trouble (t)
          #:property prop:custom-write
          (lambda (s p mode)
            (hash-set! ht (random) 'ok)
            (fprintf p "trouble")))

  (for ([i (in-range 100)])
    (hash-set! ht (trouble i) i))

  (for ([i 20])
    (define o (open-output-bytes))
    (print ht o)))

;; ----------------------------------------
;; Check that recursive printing accepts a structure
;; with the `prop:output-port` property

(let ()
  (struct p (out) #:property prop:output-port 0)
  (struct s ()
          #:property prop:custom-write
          (lambda (v out mode)
            (display "ok" (p out))))
  (define o (open-output-bytes))
  (write (s) (p o))
  (test "ok" get-output-string o))

;; ----------------------------------------
;; Check that some values are allowed in a srcloc source
;; in printed compiled code, and some values are not

(let ()
  (define (try v [result-v v] #:ok? [ok? #t])
    (define-values (i o) (make-pipe))
    (define c (compile `,(srcloc v 1 2 3 4)))
    (cond
     [ok?
      (parameterize ([current-write-relative-directory (current-directory)])
        (write c o))
      (test result-v
            srcloc-source
            (parameterize ([current-load-relative-directory (build-path (current-directory) "sub")])
              (eval (parameterize ([read-accept-compiled #t])
                      (read i)))))]
     [else
      (err/rt-test (write c o) (lambda (exn) (and (exn:fail? exn)
                                                  (regexp-match? #rx"cannot marshal" (exn-message exn)))))]))

  (try #f)
  (try 'apple)
  (try "apple")
  (try #"apple")
  (try (string->path "apple") "apple")
  (try (build-path 'up) "..")
  (try (build-path 'same) ".")
  (try (build-path 'up "apple") ".../apple")
  (try (build-path "x" 'up "apple") ".../apple")
  (try (build-path "apple" 'up) ".../apple/..")
  (try (build-path "apple" 'same) ".../apple/.")
  (try (build-path "x" "apple" 'up) ".../apple/..")
  (try (build-path "x" "apple" 'same) ".../apple/.")
  (let ([d (car (filesystem-root-list))])
    (try (build-path d 'up) (path->string (build-path d 'up))))
  (try (build-path (current-directory) "apple")
       (build-path (current-directory) "sub" "apple"))

  (try 7 #:ok? #f)
  (try (box 7) #:ok? #f))

;; Check that some other values are allowed as quoted in compiled code
(for-each (lambda (v)
            (define s (open-output-bytes))
            (write (compile v) s)
            (test v
                  values
                  (eval (parameterize ([read-accept-compiled #t])
                          (read (open-input-bytes (get-output-bytes s)))))))
          (list
           1
           "apple"
           (vector 1 2 3)
           (fxvector 1 2 3 -100)
           (flvector 1.0 2.0 3.0 +inf.0 +nan.0)))

;; ----------------------------------------
;; Test print parameters

(let ()
  (define (test-print/all x wri dis prn prx pr1
                          #:pretty-write [pp-wri wri]
                          #:pretty-display [pp-dis dis]
                          #:pretty-print/not-expr [pp-prn prn]
                          #:pretty-print/expr [pp-prx prx]
                          #:pretty-print/1 [pp-pr1 pr1])
    (define (in-string f v)
      (let ([o (open-output-bytes)])
        (f v o)
        (get-output-string o)))
    (define (print/not-expr v [o (current-output-port)])
      (parameterize ([print-as-expression #f])
        (print v o)))
    (define (pretty-print/not-expr v [o (current-output-port)])
      (parameterize ([print-as-expression #f])
        (pretty-print v o)))
    (define (print/depth-1 v [o (current-output-port)])
      (print v o 1))
    (define (pretty-print/depth-1 v [o (current-output-port)])
      (pretty-print v o 1))

    (define pretty-non-exp-ok?
      (and
       ;; not consulted by `pretty-print`:
       (print-reader-abbreviations)))

    (test wri in-string write x)
    (test (string-append pp-wri "\n") in-string pretty-write x)
    (test dis in-string display x)
    (test (string-append pp-dis "\n") in-string pretty-display x)
    (test prn in-string print/not-expr x)
    (test (string-append pp-prn "\n") in-string pretty-print/not-expr x)
    (test prx in-string print x)
    (test (string-append pp-prx "\n") in-string pretty-print x)
    (test pr1 in-string print/depth-1 x)
    (test (string-append pp-pr1 "\n") in-string pretty-print/depth-1 x))

  (define-syntax (for*/parameterize stx)
    (syntax-case stx ()
      [(_ ([p cl] rest ...) body ...)
       #'(for ([v cl])
           (parameterize ([p v])
             (for*/parameterize (rest ...) body ...)))]
      [(_ () body ...)
       #'(let () body ...)]))

  (define-struct a (x y))
  (define-struct b (x y) #:transparent #:mutable)
  (define-struct c (x y) #:prefab)

  (struct s () #:transparent)
  (define x (s)) ; a shared value to use in the test

  (struct s+ (v) #:transparent)
  (struct sub s+ ())
  (test-print/all (sub '(x))
                  "#(struct:sub (x) ...)"
                  "#(struct:sub (x) ...)"
                  "#(struct:sub (x) ...)"
                  "(sub '(x) ...)"
                  "#(struct:sub (x) ...)")

  (parameterize ([print-graph #t])
  (for*/parameterize ([print-pair-curly-braces (in-list '(#t #f))]
                      [print-mpair-curly-braces (in-list '(#t #f))])

    (parameterize ([print-mpair-curly-braces #t])
      (test-print/all (mcons 1 2)
                      "{1 . 2}" "{1 . 2}" "{1 . 2}" "(mcons 1 2)" "{1 . 2}")
      (test-print/all (mcons 1 (mcons 2 3))
                      "{1 2 . 3}" "{1 2 . 3}" "{1 2 . 3}" "(mcons 1 (mcons 2 3))" "{1 2 . 3}")
      (test-print/all (mcons 1 '())
                      "{1}" "{1}" "{1}" "(mcons 1 '())" "{1}")
      (test-print/all (mcons 1 (mcons 2 '()))
                      "{1 2}" "{1 2}" "{1 2}" "(mcons 1 (mcons 2 '()))" "{1 2}"))
    (parameterize ([print-mpair-curly-braces #f])
      (test-print/all (mcons 1 2)
                      "(1 . 2)" "(1 . 2)" "(1 . 2)" "(mcons 1 2)" "(1 . 2)")
      (test-print/all (mcons 1 (mcons 2 3))
                      "(1 2 . 3)" "(1 2 . 3)" "(1 2 . 3)" "(mcons 1 (mcons 2 3))" "(1 2 . 3)")
      (test-print/all (mcons 1 '())
                      "(1)" "(1)" "(1)" "(mcons 1 '())" "(1)")
      (test-print/all (mcons 1 (mcons 2 '()))
                      "(1 2)" "(1 2)" "(1 2)" "(mcons 1 (mcons 2 '()))" "(1 2)"))

    (parameterize ([print-pair-curly-braces #t])
      (test-print/all (cons 1 2)
                      "{1 . 2}" "{1 . 2}" "{1 . 2}" "'{1 . 2}" "{1 . 2}")
      (test-print/all (cons 1 (cons 2 3))
                      "{1 2 . 3}" "{1 2 . 3}" "{1 2 . 3}" "'{1 2 . 3}" "{1 2 . 3}")
      (test-print/all (list 1)
                      "{1}" "{1}" "{1}" "'{1}" "{1}")
      (test-print/all (list 1 2)
                      "{1 2}" "{1 2}" "{1 2}" "'{1 2}" "{1 2}")
      (test-print/all (cons x x)
                      "{#0=#(struct:s) . #0#}" "{#0=#(struct:s) . #0#}" "{#0=#(struct:s) . #0#}" "(cons #0=(s) #0#)" "{#0=#(struct:s) . #0#}")
      (test-print/all (cons 1 (cons x x))
                      "{1 #0=#(struct:s) . #0#}" "{1 #0=#(struct:s) . #0#}" "{1 #0=#(struct:s) . #0#}" "(list* 1 #0=(s) #0#)" "{1 #0=#(struct:s) . #0#}")
      (test-print/all (list (cons x x))
                      "{{#0=#(struct:s) . #0#}}" "{{#0=#(struct:s) . #0#}}" "{{#0=#(struct:s) . #0#}}" "(list (cons #0=(s) #0#))" "{{#0=#(struct:s) . #0#}}")
      (test-print/all (list x x)
                      "{#0=#(struct:s) #0#}" "{#0=#(struct:s) #0#}" "{#0=#(struct:s) #0#}" "(list #0=(s) #0#)" "{#0=#(struct:s) #0#}"))
    (parameterize ([print-pair-curly-braces #f])
      (test-print/all (cons 1 2)
                      "(1 . 2)" "(1 . 2)" "(1 . 2)" "'(1 . 2)" "(1 . 2)")
      (test-print/all (cons 1 (cons 2 3))
                      "(1 2 . 3)" "(1 2 . 3)" "(1 2 . 3)" "'(1 2 . 3)" "(1 2 . 3)")
      (test-print/all (list 1)
                      "(1)" "(1)" "(1)" "'(1)" "(1)")
      (test-print/all (list 1 2)
                      "(1 2)" "(1 2)" "(1 2)" "'(1 2)" "(1 2)")
      (test-print/all (cons x x)
                      "(#0=#(struct:s) . #0#)" "(#0=#(struct:s) . #0#)" "(#0=#(struct:s) . #0#)" "(cons #0=(s) #0#)" "(#0=#(struct:s) . #0#)")
      (test-print/all (cons 1 (cons x x))
                      "(1 #0=#(struct:s) . #0#)" "(1 #0=#(struct:s) . #0#)" "(1 #0=#(struct:s) . #0#)" "(list* 1 #0=(s) #0#)" "(1 #0=#(struct:s) . #0#)")
      (test-print/all (list (cons x x))
                      "((#0=#(struct:s) . #0#))" "((#0=#(struct:s) . #0#))" "((#0=#(struct:s) . #0#))" "(list (cons #0=(s) #0#))" "((#0=#(struct:s) . #0#))")
      (test-print/all (list x x)
                      "(#0=#(struct:s) #0#)" "(#0=#(struct:s) #0#)" "(#0=#(struct:s) #0#)" "(list #0=(s) #0#)" "(#0=#(struct:s) #0#)"))

    (for*/parameterize ([print-vector-length (in-list '(#t #f))])
      (test-print/all (a 1 2)
                      "#<a>" "#<a>" "#<a>" "#<a>" "#<a>")
      (test-print/all (c 1 1)
                      "#s(c 1 1)" "#s(c 1 1)" "#s(c 1 1)" "'#s(c 1 1)" "#s(c 1 1)")
      (test-print/all (c 1 2)
                      "#s(c 1 2)" "#s(c 1 2)" "#s(c 1 2)" "'#s(c 1 2)" "#s(c 1 2)"))

    (parameterize ([print-pair-curly-braces #f])
      (let ([s (b 1 2)])
        (test-print/all (list (cons s 0) (cons s 2))
                        "((#0=#(struct:b 1 2) . 0) (#0# . 2))" "((#0=#(struct:b 1 2) . 0) (#0# . 2))" "((#0=#(struct:b 1 2) . 0) (#0# . 2))"
                        "(list (cons #0=(b 1 2) 0) (cons #0# 2))" "((#0=#(struct:b 1 2) . 0) (#0# . 2))"))
      (let ([s (b 1 2)])
        (set-b-x! s s)
        (test-print/all (list s)
                        "(#0=#(struct:b #0# 2))" "(#0=#(struct:b #0# 2))" "(#0=#(struct:b #0# 2))"
                        "(list #0=(b #0# 2))"
                        "(#0=#(struct:b #0# 2))")))

    (parameterize ([print-vector-length #t])
      (test-print/all (b 1 1)
                      "#3(struct:b 1)" "#(struct:b 1 1)" "#3(struct:b 1)" "(b 1 1)" "#3(struct:b 1)")
      (test-print/all (b 1 2)
                      "#3(struct:b 1 2)" "#(struct:b 1 2)" "#3(struct:b 1 2)" "(b 1 2)" "#3(struct:b 1 2)")
      (test-print/all (b 'b 'b)
                      "#3(struct:b b)" "#(struct:b b b)" "#3(struct:b b)" "(b 'b 'b)" "#3(struct:b b)")

      (test-print/all (b 'struct:b 'struct:b)
                      "#3(struct:b)" "#(struct:b struct:b struct:b)" "#3(struct:b)" "(b 'struct:b 'struct:b)" "#3(struct:b)")
      (test-print/all (c x x)
                      "#s(c #0=#1(struct:s) #0#)" "#s(c #0=#(struct:s) #0#)" "#s(c #0=#1(struct:s) #0#)" "(c #0=(s) #0#)" "#s(c #0=#1(struct:s) #0#)")
      (test-print/all (vector 1 2 3 4 5)
                      "#5(1 2 3 4 5)" "#(1 2 3 4 5)" "#5(1 2 3 4 5)" "'#5(1 2 3 4 5)" "#5(1 2 3 4 5)")
      (test-print/all (vector 1 2 3 3 3)
                      "#5(1 2 3)" "#(1 2 3 3 3)" "#5(1 2 3)" "'#5(1 2 3)" "#5(1 2 3)")
      (test-print/all (vector (b 1 1) 2 3 3 3)
                      "#5(#3(struct:b 1) 2 3)" "#(#(struct:b 1 1) 2 3 3 3)" "#5(#3(struct:b 1) 2 3)" "(vector (b 1 1) 2 3 3 3)" "#5(#3(struct:b 1) 2 3)")
      (test-print/all (vector)
                      "#0()" "#()" "#0()" "'#0()" "#0()")
      (test-print/all (vector 1)
                      "#1(1)" "#(1)" "#1(1)" "'#1(1)" "#1(1)")
      (test-print/all (vector 1 1)
                      "#2(1)" "#(1 1)" "#2(1)" "'#2(1)" "#2(1)")
      (test-print/all (vector 1 1 1)
                      "#3(1)" "#(1 1 1)" "#3(1)" "'#3(1)" "#3(1)")
      (test-print/all (fxvector 1 2 3 4 5)
                      "#fx5(1 2 3 4 5)" "#fx(1 2 3 4 5)" "#fx5(1 2 3 4 5)" "(fxvector 1 2 3 4 5)" "#fx5(1 2 3 4 5)")
      (test-print/all (fxvector 1 2 3 3 3)
                      "#fx5(1 2 3)" "#fx(1 2 3 3 3)" "#fx5(1 2 3)" "(fxvector 1 2 3 3 3)" "#fx5(1 2 3)")
      (test-print/all (fxvector)
                      "#fx0()" "#fx()" "#fx0()" "(fxvector)" "#fx0()")
      (test-print/all (flvector 1.0 2.0 3.0 4.0 5.0)
                      "#fl5(1.0 2.0 3.0 4.0 5.0)" "#fl(1.0 2.0 3.0 4.0 5.0)" "#fl5(1.0 2.0 3.0 4.0 5.0)" "(flvector 1.0 2.0 3.0 4.0 5.0)" "#fl5(1.0 2.0 3.0 4.0 5.0)")
      (test-print/all (flvector 1.0 2.0 3.0 3.0 3.0)
                      "#fl5(1.0 2.0 3.0)" "#fl(1.0 2.0 3.0 3.0 3.0)" "#fl5(1.0 2.0 3.0)" "(flvector 1.0 2.0 3.0 3.0 3.0)" "#fl5(1.0 2.0 3.0)")
      (test-print/all (flvector)
                      "#fl0()" "#fl()" "#fl0()" "(flvector)" "#fl0()"))
    (parameterize ([print-vector-length #f])
      (test-print/all (b 1 1)
                      "#(struct:b 1 1)" "#(struct:b 1 1)" "#(struct:b 1 1)" "(b 1 1)" "#(struct:b 1 1)")
      (test-print/all (b 1 2)
                      "#(struct:b 1 2)" "#(struct:b 1 2)" "#(struct:b 1 2)" "(b 1 2)" "#(struct:b 1 2)")
      (test-print/all (b 'b 'b)
                      "#(struct:b b b)" "#(struct:b b b)" "#(struct:b b b)" "(b 'b 'b)" "#(struct:b b b)")
      (test-print/all (b 'struct:b 'struct:b)
                      "#(struct:b struct:b struct:b)" "#(struct:b struct:b struct:b)" "#(struct:b struct:b struct:b)" "(b 'struct:b 'struct:b)" "#(struct:b struct:b struct:b)")
      (test-print/all (c x x)
                      "#s(c #0=#(struct:s) #0#)" "#s(c #0=#(struct:s) #0#)" "#s(c #0=#(struct:s) #0#)" "(c #0=(s) #0#)" "#s(c #0=#(struct:s) #0#)")
      (test-print/all (vector 1 2 3 4 5)
                      "#(1 2 3 4 5)" "#(1 2 3 4 5)" "#(1 2 3 4 5)" "'#(1 2 3 4 5)" "#(1 2 3 4 5)")
      (test-print/all (vector 1 2 3 3 3)
                      "#(1 2 3 3 3)" "#(1 2 3 3 3)" "#(1 2 3 3 3)" "'#(1 2 3 3 3)" "#(1 2 3 3 3)")
      (test-print/all (vector (b 1 1) 2 3 3 3)
                      "#(#(struct:b 1 1) 2 3 3 3)" "#(#(struct:b 1 1) 2 3 3 3)" "#(#(struct:b 1 1) 2 3 3 3)" "(vector (b 1 1) 2 3 3 3)" "#(#(struct:b 1 1) 2 3 3 3)")
      (test-print/all (vector)
                      "#()" "#()" "#()" "'#()" "#()")
      (test-print/all (vector 1)
                      "#(1)" "#(1)" "#(1)" "'#(1)" "#(1)")
      (test-print/all (vector 1 1)
                      "#(1 1)" "#(1 1)" "#(1 1)" "'#(1 1)" "#(1 1)")
      (test-print/all (vector 1 1 1)
                      "#(1 1 1)" "#(1 1 1)" "#(1 1 1)" "'#(1 1 1)" "#(1 1 1)")
      (test-print/all (fxvector 1 2 3 4 5)
                      "#fx(1 2 3 4 5)" "#fx(1 2 3 4 5)" "#fx(1 2 3 4 5)" "(fxvector 1 2 3 4 5)" "#fx(1 2 3 4 5)")
      (test-print/all (fxvector 1 2 3 3 3)
                      "#fx(1 2 3 3 3)" "#fx(1 2 3 3 3)" "#fx(1 2 3 3 3)" "(fxvector 1 2 3 3 3)" "#fx(1 2 3 3 3)")
      (test-print/all (fxvector)
                      "#fx()" "#fx()" "#fx()" "(fxvector)" "#fx()")
      (test-print/all (flvector 1.0 2.0 3.0 4.0 5.0)
                      "#fl(1.0 2.0 3.0 4.0 5.0)" "#fl(1.0 2.0 3.0 4.0 5.0)" "#fl(1.0 2.0 3.0 4.0 5.0)" "(flvector 1.0 2.0 3.0 4.0 5.0)" "#fl(1.0 2.0 3.0 4.0 5.0)")
      (test-print/all (flvector 1.0 2.0 3.0 3.0 3.0)
                      "#fl(1.0 2.0 3.0 3.0 3.0)" "#fl(1.0 2.0 3.0 3.0 3.0)" "#fl(1.0 2.0 3.0 3.0 3.0)" "(flvector 1.0 2.0 3.0 3.0 3.0)" "#fl(1.0 2.0 3.0 3.0 3.0)")
      (test-print/all (flvector)
                      "#fl()" "#fl()" "#fl()" "(flvector)" "#fl()"))

    (void))

  (parameterize ([print-reader-abbreviations #f])
    (test-print/all (list ''a ',a '`a ',@a '#'a '#,a '#`a '#,@a)
                    "((quote a) (unquote a) (quasiquote a) (unquote-splicing a) (syntax a) (unsyntax a) (quasisyntax a) (unsyntax-splicing a))"
                    #:pretty-write "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "((quote a) (unquote a) (quasiquote a) (unquote-splicing a) (syntax a) (unsyntax a) (quasisyntax a) (unsyntax-splicing a))"
                    #:pretty-display "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "((quote a) (unquote a) (quasiquote a) (unquote-splicing a) (syntax a) (unsyntax a) (quasisyntax a) (unsyntax-splicing a))"
                    #:pretty-print/not-expr "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "'('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "('a ,a `a ,@a #'a #,a #`a #,@a)"))

  (parameterize ([print-reader-abbreviations #t])
    (test-print/all (list ''a ',a '`a ',@a '#'a '#,a '#`a '#,@a)
                    "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "((quote a) (unquote a) (quasiquote a) (unquote-splicing a) (syntax a) (unsyntax a) (quasisyntax a) (unsyntax-splicing a))"
                    #:pretty-display "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "'('a ,a `a ,@a #'a #,a #`a #,@a)"
                    "('a ,a `a ,@a #'a #,a #`a #,@a)"))

  (parameterize ([print-reader-abbreviations #t])
    (test-print/all (list (mcons 'unquote '()) (vector (mcons 1 2) 'unquote '()))
                    "({unquote} #({1 . 2} unquote ()))"
                    "({unquote} #({1 . 2} unquote ()))"
                    "({unquote} #({1 . 2} unquote ()))"
                    "(list (mcons 'unquote '()) (vector (mcons 1 2) 'unquote '()))"
                    "({unquote} #({1 . 2} unquote ()))"))

  (test-print/all (stencil-vector 5 "a" 'b)
                  "#<stencil 5: \"a\" b>"
                  "#<stencil 5: a b>"
                  "#<stencil 5: \"a\" b>"
                  "#<stencil 5: \"a\" b>"
                  "#<stencil 5: \"a\" b>")
  (test-print/all (stencil-vector 5 "a" (read (open-input-string "#0=(#0#)")))
                  "#<stencil 5: \"a\" #0=(#0#)>"
                  "#<stencil 5: a #0=(#0#)>"
                  "#<stencil 5: \"a\" #0=(#0#)>"
                  "#<stencil 5: \"a\" #0=(#0#)>"
                  "#<stencil 5: \"a\" #0=(#0#)>")

  (void)))

;; ----------------------------------------
;; More `prop:custom-write` and `prop:custom-print-quotable` checking.
;; Make sure the `prop:custom-write` callback gets an approrpriate
;; printing mode, even when looking for quoting modes and cycles, and
;; check behavior when the callback synthesizes a new lists. The
;; `ptest` tests above already do a lot of that, but this test covers
;; some additional corners.
;; Based on an example by Ryan Kramer.

(let ()
  (struct my-struct (item) #:transparent)

  (define modes '())

  (define (check-saw-mode . alts)
    (define ms (reverse modes))
    (set! modes '())
    (test #t ms (and (member ms alts) #t)))

  (define-syntax-rule (expect e output)
    (let ([o (open-output-bytes)])
      (parameterize ([current-output-port o])
        e)
      (test output get-output-string o)))

  (define (go port mode val)
    (set! modes (cons mode modes))
    (case mode
      [(#f #t 1)
       (display "#<mine: " port)
       (if mode
           (write val port) 
           (display val port))
       (display ">" port)]
      [else
       (display "(mine " port)
       (print val port mode)
       (display ")" port)]))

  (struct mine (content)
    #:property
    prop:custom-write
    (lambda (v port mode)
      (go port mode (mine-content v))))

  (struct mine/copy (content)
    #:property
    prop:custom-write
    (lambda (v port mode)
      (go port mode (apply list (mine/copy-content v)))))

  (struct mine/always (content)
    #:property
    prop:custom-print-quotable 'always
    #:property
    prop:custom-write
    (lambda (v port mode)
      (go port mode (mine/always-content v))))

  (struct mine/maybe (content)
    #:property
    prop:custom-print-quotable 'maybe
    #:property
    prop:custom-write
    (lambda (v port mode)
      (go port mode (mine/maybe-content v))))

  (struct mine/copy/always (content)
    #:property
    prop:custom-print-quotable 'always
    #:property
    prop:custom-write
    (lambda (v port mode)
      (go port mode (apply list (mine/copy/always-content v)))))

  (define (show println writeln displayln)
    (define b (box 'CONTENT))
    (define x (list b (my-struct '(1 a))))

    (printf "List\n")
    (expect (println x) "(list '#&CONTENT (my-struct '(1 a)))\n")
    (expect (writeln x) "(#&CONTENT #(struct:my-struct (1 a)))\n")
    (expect (displayln x) "(#&CONTENT #(struct:my-struct (1 a)))\n")

    (printf "Wrapped list\n")
    (define y (mine x))
    (expect (println y) "(mine (list '#&CONTENT (my-struct '(1 a))))\n")
    (check-saw-mode '(0 0))
    (expect (writeln y) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln y) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#f #f))

    (printf "Wrapped list 'always\n")
    (define z (mine/always x))
    (expect (println z) "'#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(1 1))
    (expect (writeln z) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln z) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#f #f))

    (printf "Wrapped list copied on print\n")
    (define y/c (mine/copy x))
    (expect (println y/c) "(mine '(#&CONTENT #(struct:my-struct (1 a))))\n")
    (check-saw-mode '(0 0))
    (expect (writeln y/c) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln y/c) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#f #f))

    (printf "Wrapped list copied on print 'always\n")
    (define z/c (mine/copy/always x))
    (expect (println z/c) "'#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(1 1))
    (expect (writeln z/c) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln z/c) "#<mine: (#&CONTENT #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#f #f))
    
    (printf "Wrapped cycle list\n")
    (set-box! b x)
    ;; The printer may need two passes to sort out cycles
    (expect (println y) "(mine #0=(list '#&#0# (my-struct '(1 a))))\n")
    (check-saw-mode '(0 0) '(0 0 0))
    (expect (writeln y) "#<mine: #0=(#&#0# #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#t #t) '(#t #t #t))
    (expect (displayln y) "#<mine: #0=(#&#0# #(struct:my-struct (1 a)))>\n")
    (check-saw-mode '(#f #f) '(#f #f #f))

    (printf "Wrapped quotable list\n")
    (define yq (mine '(#&CONTENT)))
    (expect (println yq) "(mine '(#&CONTENT))\n")
    (check-saw-mode '(0 0))
    (expect (writeln yq) "#<mine: (#&CONTENT)>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln yq) "#<mine: (#&CONTENT)>\n")
    (check-saw-mode '(#f #f))

    (printf "Wrapped quotable list 'maybe\n")
    (define yqm (mine/maybe '(#&CONTENT)))
    (expect (println yqm) "'#<mine: (#&CONTENT)>\n")
    (check-saw-mode '(0 1)) ; guess unquoted, discovered to be quoted
    (expect (writeln yqm) "#<mine: (#&CONTENT)>\n")
    (check-saw-mode '(#t #t))
    (expect (displayln yqm) "#<mine: (#&CONTENT)>\n")
    (check-saw-mode '(#f #f))

    (void))

  (show println writeln displayln)
  (show pretty-print pretty-write pretty-display))

;; ----------------------------------------

(let ()
  (struct named-procedure (procedure name)
    #:property prop:procedure (struct-field-index procedure)
    #:property prop:object-name (struct-field-index name))

  (define f (named-procedure (lambda (x) x) "string name"))
  (test "#<procedure:string name>" format "~s" f)
  (test "string name" object-name f)

  (define f2 (named-procedure (lambda (x) x) '("string name")))
  (test "#<procedure>" format "~s" f2)
  (test '("string name") object-name f2)

  (define f3 (procedure-rename f 'other-name))
  (test "#<procedure:other-name>" format "~a" f3)
  (test 'other-name object-name f3))

(let ()
  (struct named-procedure (procedure name)
    #:property prop:procedure (struct-field-index procedure)
    #:property prop:object-name (struct-field-index name)
    #:transparent)

  (define f (named-procedure (procedure-rename (lambda (x) x) 'inner) "string name"))
  (test "(named-procedure #<procedure:inner> \"string name\")" format "~v" f)
  (test "string name" object-name f)

  (define f2 (named-procedure (procedure-rename (lambda (x) x) 'inner) '("string name")))
  (test "(named-procedure #<procedure:inner> '(\"string name\"))" format "~v" f2)
  (test '("string name") object-name f2)

  (define f3 (procedure-rename f 'other-name))
  (test "#<procedure:other-name>" format "~a" f3)
  (test 'other-name object-name f3))

;; ----------------------------------------

(parameterize ([global-port-print-handler
                (lambda (v o [depth 0])
                  (display "<redacted>" o))])
  (let ([o (open-output-string)])
    (print '(hello) o)
    (test "<redacted>" get-output-string o)
    (default-global-port-print-handler '(hello) o)
    (test "<redacted>'(hello)" get-output-string o)
    (default-global-port-print-handler '(hello) o 1)
    (test "<redacted>'(hello)(hello)" get-output-string o)))

;; ----------------------------------------

(report-errs)
