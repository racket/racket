#lang racket/base
(require racket/pretty
         compiler/zo-parse
         compiler/zo-marshal
         compiler/decompile)

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
          (let ([p2 (zo-parse (open-input-bytes b))]
                [to-string (lambda (p)
                             (let ([o (open-output-bytes)])
                               (print p o)
                               (get-output-string o)))])
            (unless (equal? (to-string p) (to-string p2))
              (error 'zo "failed on example: ~e" ex-mod))))))))

(for-each check (list ex-mod1 ex-mod2 ex-mod3 ex-mod4 ex-mod5))
