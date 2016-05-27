#lang racket/base
(require racket/pretty
         compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile
         racket/file)

(define ex-mod1
  '(module m racket
     (begin-for-syntax
      (define fs 10)
      (list fs))
     (define-syntax (m stx)
       #'10)
     (m)
     (begin-for-syntax
      (list fs))))

(define ex-mod2
  '(module m racket
     (define t 8)
     (define s 10)
     (provide t (protect-out s))))

(define ex-mod3
  '(module m racket/base
     (module* a racket/base
       (provide a)
       (define a 1)
       (module* a+ racket/base
         (define a+ 1.1)))
     (module* b racket/base
       (require (submod "." ".." a))
       (provide b)
       (define b (+ a 1)))))

(define ex-mod4
  '(module m racket/base
     (module a racket/base
       (provide a)
       (define a 1)
       (module a+ racket/base
         (define a+ 1.1)))
     (module b racket/base
       (require (submod "." ".." a))
       (provide b)
       (define b (+ a 1)))))

(define ex-mod5
  '(module m racket/base
     (module a racket/base
       (provide a)
       (define a 1)
       (module* a+ racket/base
         (define a+ 1.1)))
     (module* b racket/base
       (require (submod "." ".." a))
       (provide b)
       (define b (+ a 1)))))

(define (check ex-mod)
  (let ([c (parameterize ([current-namespace (make-base-namespace)])
             (compile ex-mod))])
    (let ([o (open-output-bytes)])
      (write c o)
      (let ([p (zo-parse (open-input-bytes (get-output-bytes o)))])
        (let ([b (zo-marshal p)])
          ;; Check that submodule table is ok:
          (parameterize ([read-accept-compiled #t]
                         [current-output-port (open-output-bytes)])
            (define f (make-temporary-file))
            (call-with-output-file f #:exists 'truncate (lambda (f) (display b f)))
            (dynamic-require f #f))
          (let ([p2 (zo-parse (open-input-bytes b))]
                [to-string (lambda (p)
                             (let ([o (open-output-bytes)])
                               (print p o)
                               (get-output-string o)))])
            (define s1 (to-string p))
            (define s2 (to-string p2))
            (unless (equal? s1 s2)
              (error 'zo "failed on example: ~e\n~s\n~s" ex-mod s1 s2))))))))

(for-each check (list ex-mod1 ex-mod2 ex-mod3 ex-mod4 ex-mod5))
