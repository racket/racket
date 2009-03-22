#lang scheme/base

(require tests/eli-tester scribble/text/syntax-utils
         scheme/runtime-path scheme/port scheme/sandbox
         (prefix-in doc: (lib "scribblings/scribble/preprocessor.scrbl")))

(define-runtime-path text-dir "text")
(define-runtime-path this-dir ".")

(define (tests)
  (begin/collect-tests)
  (preprocessor-tests))

(define (begin/collect-tests)
  (test

   ;; begin/collect scope etc
   (begin/collect 1) => 1
   (begin/collect 1 2 3) => '(1 2 3)
   (begin/collect) => '()
   (begin/collect (define x 1) x) => 1
   (begin/collect (define x 1)) => '()
   (begin/collect (define x 1) x x x) => '(1 1 1)
   (begin/collect (define x 1) (define y 2) x y x y) => '(1 2 1 2)
   (begin/collect (define x 1) x (define y 2) y) => '(1 2)
   (begin/collect (define x 1) x (define y 2)) => '(1)
   (begin/collect (define x 1) x x (define y 2) y y) => '(1 1 2 2)
   (begin/collect (define x 1) x (define x 2) x) => '(1 2)
   (begin/collect (define x 1) x x (define x 2) x x) => '(1 1 2 2)
   (begin/collect (define (x) y) (define y 1) (x) (x) (x)) => '(1 1 1)
   (begin/collect (define x 1) x (define y 2) x) => '(1 1)
   (begin/collect (define x 1) x x (define y 2) x x) => '(1 1 1 1)
   (begin/collect (define x 1) x x (define y x) y y) => '(1 1 1 1)
   (begin/collect (define (x) y) (define y 1) (x) (x)
                  (define (x) y) (define y 2) (x) (x))
   => '(1 1 2 2)
   (begin/collect (define-syntax-rule (DEF x y) (define x y)) (DEF x 1) x x)
   => '(1 1)
   (begin/collect (define-syntax-rule (DEF x y) (define x y)) 1 (DEF x 2) x)
   => '(1 2)
   (begin/collect (define-syntax-rule (DEF x y) (define x y))
                  (DEF x 1) x x
                  (DEF x 2) x x)
   => '(1 1 2 2)
   (begin/collect (define (x) y)
                  (define-syntax-rule (DEF x y) (define x y))
                  (DEF y 1) (x) (x)
                  (DEF y 2) (x) (x))
   => '(1 1 1 1)
   (let ([y 1]) (begin/collect y y (define x y) x y x)) => '(1 1 1 1 1)
   (let ([y 1]) (begin/collect y y (define y 2) y y)) => '(1 1 2 2)
   (let ([y 1]) (begin/collect (define (x) y) (x) (x))) => '(1 1)
   (let ([y 1]) (begin/collect (define (x) y) (define y 2) (x) (x))) => '(2 2)
   (let ([y 1]) (begin/collect (define (x) y) (x) (x) (define y 2) y y))
   => '(1 1 2 2)
   (let ([y 1]) (begin/collect (define (x) y) (x) (x) (define y 2) y y (x)))
   => '(1 1 2 2 1)
   (let ([y 1]) (begin/collect (define (x) y) (x) (x) (define y 2) (x) y y))
   => '(1 1 1 2 2)
   (begin/collect (begin (define (x) y)
                         (define-syntax-rule (DEF x y) (define x y))
                         (define y 2))
                  (x) (x))
   => '(2 2)
   (begin/collect (define (x) y)
                  (begin (define-syntax-rule (DEF x y) (define x y))
                         (define y 2))
                  (x) (x))
   => '(2 2)
   (begin/collect (define (x) y)
                  (define-syntax-rule (DEF x y) (define x y))
                  (begin (define y 2))
                  (x) (x))
   => '(2 2)
   (begin/collect (begin (begin (begin (define (x) y))
                                (begin (define-syntax-rule (DEF x y)
                                         (define x y))))
                         (begin (begin (define y 2))
                                (begin (x)))
                         (begin (x))))
   => '(2 2)
   (begin/collect 1
                  (define (f x #:< [< "<"] #:> [> ">"]) (list < x >))
                  (f 1)
                  (f #:< "[" 2)
                  (f 3 #:> "]" #:< "["))
   => '(1 ("<" 1 ">") ("[" 2 ">") ("[" 3 "]"))

   ))

(define (preprocessor-tests)
  ;; (sample-file-tests)
  (in-documentation-tests))

(define (sample-file-tests)
  (parameterize ([current-directory text-dir])
    (for ([ifile (map path->string (directory-list))]
          #:when (and (file-exists? ifile)
                      (regexp-match? #rx"^i[0-9]+\\.ss$" ifile)))
      (define ofile (regexp-replace #rx"^i([0-9]+)\\..*$" ifile "o\\1.txt"))
      (define expected (call-with-input-file ofile
                         (lambda (i) (read-bytes (file-size ofile) i))))
      (define o (open-output-bytes))
      (parameterize ([current-output-port o])
        (dynamic-require (path->complete-path ifile) #f))
      (test (get-output-bytes o) => expected))))

(define (in-documentation-tests)
  (define (text-test line in-text out-text more)
    (define-values (i o) (make-pipe 512))
    (define-values (expected len-to-read)
      (let ([m (regexp-match-positions #rx"\n\\.\\.\\.$" out-text)])
        (if m
          (values (substring out-text 0 (caar m)) (caar m))
          (values out-text #f))))
    ;; test with name indicating the source
    (define-syntax-rule (t . stuff)
      (test ;#:failure-message
            ;(format "preprocessor test failure at line ~s" line)
            . stuff))
    (parameterize ([current-directory this-dir]
                   [sandbox-output o]
                   [sandbox-error-output current-output-port])
      (define exn #f)
      (define thd #f)
      (define (run)
        ;; only need to evaluate the module, so we have its output; but do that
        ;; in a thread, since we might want to look at just a prefix of an
        ;; infinite output
        (with-handlers ([void (lambda (e) (set! exn e))])
          (make-module-evaluator in-text)
          (close-output-port o)))
      (for ([m more])
        (call-with-output-file (car m) #:exists 'truncate
          (lambda (o) (display (cdr m) o))))
      (set! thd (thread run))
      (t (with-limits 2 #f
           (if len-to-read (read-string len-to-read i) (port->string i)))
         => expected)
      (t (begin (kill-thread thd) (cond [exn => raise] [else #t])))))
  (call-with-trusted-sandbox-configuration
    (lambda ()
      (for ([t (in-list (doc:tests))])
        (begin (apply text-test t))))))

;; run all
(test do (tests))
