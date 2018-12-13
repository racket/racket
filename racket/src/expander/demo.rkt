#lang racket/base
(require "main.rkt"
         "common/set.rkt")

;; ----------------------------------------

(define demo-ns (make-namespace))
(namespace-attach-module (current-namespace) ''#%kernel demo-ns)

(namespace-require ''#%kernel demo-ns)
(namespace-require '(for-syntax '#%kernel) demo-ns)

(define check-reexpand? #f)
(define check-serialize? #f)

(define (expand-expression e #:namespace [ns demo-ns])
  (expand (namespace-syntax-introduce (datum->syntax #f e) ns)
          ns))

(define (expand+compile-expression e
                                   #:namespace [ns demo-ns]
                                   #:serializable? [serializable? #f])
  (define exp-e (expand-expression e #:namespace ns))
  (define c (compile (if check-reexpand? exp-e e) ns (or serializable?
                                                         check-serialize?)))
  (define ready-c (if check-serialize?
                      (let ([o (open-output-bytes)])
                        (display c o)
                        (parameterize ([read-accept-compiled #t])
                          (read (open-input-bytes (get-output-bytes o)))))
                      c))
  (values exp-e ready-c))

(define (compile-expression e
                            #:namespace [ns demo-ns]
                            #:serializable? [serializable? #f])
  (define-values (exp-e ready-c)
    (expand+compile-expression e #:namespace ns #:serializable? serializable?))
  ready-c)

(define (compile+eval-expression e #:namespace [ns demo-ns])
  (define-values (exp-e ready-c)
    (expand+compile-expression e #:namespace ns))
  (values exp-e
          (eval ready-c ns)))

(define (eval-expression e #:check [check-val 'no-value-to-check]
                         #:namespace [ns demo-ns])
  (define-values (c v) (compile+eval-expression e #:namespace ns))
  (unless (eq? check-val 'no-value-to-check)
    (unless (equal? v check-val)
      (error "check failed:" v "vs." check-val)))
  v)

(define (eval-expression/interleaved e #:check [check-val 'no-value-to-check]
                                     #:namespace [ns demo-ns])
  (define v (eval e ns))
  (unless (eq? check-val 'no-value-to-check)
    (check-value v check-val))
  v)

(define (check-value v check-val)
  (unless (equal? v check-val)
    (error "check failed:" v "vs." check-val)))

(define-syntax-rule (check-print expr out ...)
  (check-thunk-print (lambda () expr) out ...))

(define (check-thunk-print t . outs)
  (define o (open-output-bytes))
  (parameterize ([current-output-port o])
    (t))
  (write-bytes (get-output-bytes o))
  (define o-expected (open-output-bytes))
  (for ([out (in-list outs)]) (println out o-expected))
  (unless (equal? (get-output-bytes o)
                  (get-output-bytes o-expected))
    (error "output check failed:"
           (get-output-bytes o)
           "vs." (get-output-bytes o-expected))))

(define-syntax-rule (check-error expr rx)
  (check-thunk-error (lambda () expr) rx))

(define (check-thunk-error t rx)
  (void)
  (with-handlers ([exn:fail? (lambda (exn)
                               (unless (regexp-match? rx (exn-message exn))
                                 (error "wrong error" (exn-message exn)))
                               `(ok ,(exn-message exn)))])
    (t)
    (error "shouldn't get here")))

;; ----------------------------------------

(compile+eval-expression
 '(+ 1 1))

(compile+eval-expression
 '(case-lambda
   [(x) (set! x 5)]
   [(x y) (begin0 y x)]
   [() (with-continuation-mark 1 2 3)]))

(compile+eval-expression
 '(lambda (x) (define-values (y) x) y))

(compile+eval-expression
 '(lambda (x)
   (define-syntaxes (y) (lambda (stx) (quote-syntax 7)))
   y))

;; Expands to `let-values`:
(compile+eval-expression
 '(lambda (x)
   (define-values (z) 1)
   (define-values (y) z)
   y))

;; Expands to two separate `letrec-values`:
(compile+eval-expression
 '(lambda (x)
   (define-values (z) (lambda () y))
   (define-values (y) 1)
   (define-values (q) (lambda () q))
   z))

;; Same as previous:
(compile+eval-expression
 '(lambda (x)
   (letrec-syntaxes+values
    ()
    ([(z) (lambda () y)]
     [(y) 1]
     [(q) (lambda () q)])
    z)))

(compile+eval-expression
 '(let-values ([(z) 9])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (car (cdr (syntax-e stx))))])
    ([(x) 5] [(y) (lambda (z) z)])
    (let-values ([(z) 10])
      (begin z (if (m 10) 1 2))))))

"expansion not captured"
(eval-expression
 #:check 'x-1
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax x))])
    ()
    (let-values ([(x) 'x-3])
      (m)))))

"non-capturing expansion"
(eval-expression
 #:check 'x-3
 '(let-values ([(x) 'x-1])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (datum->syntax
             #f
             (list (quote-syntax let-values)
                   (list (list (list (quote-syntax x))
                               (quote-syntax 'x-2)))
                   (car (cdr (syntax-e stx))))))])
    ()
    (let-values ([(x) 'x-3])
      (m x)))))

"distinct generated variables"
(eval-expression
 #:check '(2 1)
 '(letrec-syntaxes+values
   ([(gen) (lambda (stx)
             (let-values ([(vals) (syntax-e (car (cdr (syntax-e stx))))]
                          [(binds) (syntax-e (car (cdr (cdr (syntax-e stx)))))]
                          [(refs) (syntax-e (car (cdr (cdr (cdr (syntax-e stx))))))])
               (datum->syntax
                #f
                (if (null? vals)
                    (list (quote-syntax bind) binds refs)
                    (list (quote-syntax gen)
                          (cdr vals)
                          (cons (list (list (quote-syntax x))
                                      (car vals))
                                binds)
                          (cons (quote-syntax x)
                                refs))))))]
    [(bind) (lambda (stx)
              (let-values ([(binds) (car (cdr (syntax-e stx)))]
                           [(refs) (car (cdr (cdr (syntax-e stx))))])
                (datum->syntax
                 (quote-syntax here)
                 (list (quote-syntax let-values)
                       binds
                       (cons (quote-syntax list)
                             refs)))))])
   ()
   (gen (1 2) () ())))

"use-site scopes (so not ambiguous)"
(eval-expression
 #:check 'ok
 '((let-values ()
     (define-syntaxes (identity)
       (lambda (stx)
         (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
           (datum->syntax
            (quote-syntax here)
            (list 'lambda '(x)
                  (list 'let-values (list
                                     (list (list misc-id) ''other))
                        'x))))))
     (identity x))
   'ok))

"use-site scope remove from binding position"
(eval-expression
 #:check 'still-ok
 '(let-values ()
   (define-syntaxes (define-identity)
     (lambda (stx)
       (let-values ([(id) (car (cdr (syntax-e stx)))])
         (datum->syntax
          (quote-syntax here)
          (list 'define-values (list id) '(lambda (x) x))))))
   (define-identity f)
   (f 'still-ok)))

"compile-time scopes pruned by `quote-syntax`"
(namespace-require '(for-meta 2 '#%kernel) demo-ns)
(eval-expression
 #:check 'bound
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (define-syntaxes (wrap) ; to provoke a use-site scope
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (wrap (quote-syntax x)))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound))
                id2))))])
   ()
   (m)))

"`(quote-syntax .... #:local)` doesn't prune"
(eval-expression
 #:check 'bound-2
 '(letrec-syntaxes+values
   ([(m)
     (lambda (stx)
       (let-values ([(id1) (let-values ([(x) 1])
                             (quote-syntax x #:local))]
                    [(id2) (let-values ([(x) 1])
                             (define-syntaxes (wrap)
                               (lambda (stx) (car (cdr (syntax-e stx)))))
                             (quote-syntax x #:local))])
         (datum->syntax
          (quote-syntax here)
          (list 'let-values (list (list (list id1) ''bound-1)
                                  (list (list id2) ''bound-2))
                id2))))])
   ()
   (m)))

"non-transformer binding misuse"
(check-error
 (expand-expression '(letrec-syntaxes+values
                      ([(v) 1])
                      ()
                      v))
 #rx"illegal use of syntax")

"free-identifier=? and bound-identifier=?"
(eval-expression
 #:check '(a (#t #f #t)
           b (#f #f #t)
           c (#t #f #t)
           d (#t #f #f)
           e (#f #f #t) (#f #f #f)
           f ((#t #f) (#f #f)))
 '(let-values ([(x) 0])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (datum->syntax
             (quote-syntax here)
             (list (quote-syntax quote)
                   (list
                    (free-identifier=? (quote-syntax x) (car (cdr (syntax-e stx))))
                    (bound-identifier=? (quote-syntax x) (car (cdr (syntax-e stx))))
                    (bound-identifier=? (car (cdr (syntax-e stx)))
                                        (car (cdr (cdr (syntax-e stx)))))))))])
    ()
    (list 
     'a
     (m x x)
     'b
     (let-values ([(x) 1])
       (m x x))
     'c
     (letrec-syntaxes+values
      ([(n) (lambda (stx)
              (quote-syntax (m x x)))])
      ()
      (n))
     'd
     (letrec-syntaxes+values
      ([(o) (lambda (stx)
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax m)
                     (car (cdr (syntax-e stx)))
                     (quote-syntax x))))])
      ()
      (o x))
     'e
     (m not-x not-x)
     (m not-x also-not-x)
     'f
     (letrec-syntaxes+values
      ([(p) (lambda (stx)
              (letrec-syntaxes+values
               ([(q) (lambda (nested-stx)
                       (datum->syntax
                        (quote-syntax here)
                        (list (quote-syntax quote)
                              ;; These `free-identifier=?` test should be at phase 1:
                              (list
                               (free-identifier=? (quote-syntax stx) (car (cdr (syntax-e nested-stx))))
                               (free-identifier=? (quote-syntax stx) (car (cdr (cdr (syntax-e nested-stx)))))))))])
               ()
               (datum->syntax
                (quote-syntax here)
                (list (quote-syntax quote)
                      (list (q stx not-stx)
                            (let-values ([(stx) 0])
                              (q stx stx)))))))])
      ()
      (p))))))

"syntax-local-value"
(eval-expression
 '(let-values ([(x) 1])
   (letrec-syntaxes+values
    ([(x-id) (quote-syntax x)])
    ()
    (letrec-syntaxes+values
     ([(m) (lambda (stx) (syntax-local-value (quote-syntax x-id)))])
     ()
     (let-values ([(x) 2])
       (m)))))
 #:check 1)

"local-expand"
(eval-expression
 '(let-values ([(x) 10])
   (letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax (something x)))])
    ()
    (letrec-syntaxes+values
     ([(n) (lambda (stx) (car
                     (cdr
                      (syntax-e
                       (local-expand (car (cdr (syntax-e stx)))
                                     'expression
                                     (list (quote-syntax #%app)))))))])
     ()
     (let-values ([(x) 20])
       (n (m))))))
 #:check 10)

"local-expand-expression"
(eval-expression
 '(letrec-syntaxes+values
   ([(m) (lambda (stx) (quote-syntax 5))])
   ()
   (letrec-syntaxes+values
    ([(n) (lambda (stx)
            (let-values ([(expr already)
                          (syntax-local-expand-expression (car (cdr (syntax-e stx))))])
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax +)
                     (quote-syntax 1)
                     already))))])
    ()
    (n (m))))
 #:check 6)

(check-error
 (eval-expression
  '(letrec-syntaxes+values
    ([(m) (lambda (stx) (quote-syntax 5))])
    ()
    (letrec-syntaxes+values
     ([(n) (lambda (stx)
             (let-values ([(expr already)
                           (syntax-local-expand-expression (car (cdr (syntax-e stx))))])
               (datum->syntax
                #f
                (list
                 (quote-syntax let-values)
                 (list (list (list (quote-syntax x)) (quote-syntax 1)))
                 already))))])
     ()
     (n (m)))))
 #rx"expanded syntax not in its original lexical context")

"internal definition context"
(eval-expression
 '(let-values ([(x) 10])
   (letrec-syntaxes+values
    ([(m) (lambda (stx)
            (let-values ([(id) (car (cdr (syntax-e stx)))]
                         [(id2) (car (cdr (cdr (syntax-e stx))))]
                         [(intdef) (syntax-local-make-definition-context)])
              (syntax-local-bind-syntaxes (list id)
                                          (quote-syntax (lambda (stx) (quote-syntax 5)))
                                          intdef)
              (syntax-local-bind-syntaxes (list id2)
                                          #f
                                          intdef)
              (datum->syntax
               (quote-syntax here)
               (list (quote-syntax let-values)
                     (list (list (list
                                  (let-values ([(id2-by-expand)
                                                (car
                                                 (cdr
                                                  (syntax-e (local-expand (datum->syntax
                                                                           #f
                                                                           (list (quote-syntax quote)
                                                                                 id2))
                                                                          (list 'intdef)
                                                                          (list (quote-syntax quote))
                                                                          intdef))))]
                                               [(id2-by-intro)
                                                (internal-definition-context-introduce
                                                 intdef
                                                 id2)]
                                               [(flip) (make-syntax-introducer)])
                                    (if (bound-identifier=? id2-by-expand id2-by-intro)
                                        (let-values ([(delta)
                                                      (make-syntax-delta-introducer
                                                       (flip (quote-syntax here))
                                                       (quote-syntax here))])
                                          (syntax-local-identifier-as-binding
                                           (delta (flip id2-by-intro) 'remove)))
                                        (error "should have been the same"))))
                                 7))
                     (local-expand (datum->syntax
                                    (quote-syntax here)
                                    (list (quote-syntax +)
                                          (list id)
                                          id2))
                                   (list 'intdef)
                                   (list)
                                   intdef)))))])
                          
    ()
    (m x y)))
 #:check 12)

"set! transformer"
(eval-expression
 '(let-values ([(real-one) 1]
               [(check-one) (lambda (v)
                              (if (equal? v 1)
                                  'ok
                                  'oops))])
   (letrec-syntaxes+values
    ([(one)
      (make-set!-transformer
       (lambda (stx)
         (if (pair? (syntax-e stx))
             (if (free-identifier=? (car (syntax-e stx))
                                    (quote-syntax set!))
                 (datum->syntax
                  (quote-syntax here)
                  (list (quote-syntax check-one)
                        (car (cdr (cdr (syntax-e stx))))))
                 (datum->syntax
                  stx
                  (cons
                   (quote-syntax list)
                   (cons
                    (quote-syntax real-one)
                    (cdr (syntax-e stx))))))
             (quote-syntax real-one))))])
    ()
    (list one
          (set! one 5)
          (set! one 1)
          (one 8))))
 #:check (list 1 'oops 'ok '(1 8)))

"rename transformer"
(eval-expression
 '(let-values ([(f) (lambda (v) (+ v 1))])
   (letrec-syntaxes+values
    ([(g) (make-rename-transformer (quote-syntax f))])
    ()
    (list (letrec-syntaxes+values
           ([(m) (lambda (stx)
                   (datum->syntax (quote-syntax here)
                                  (free-identifier=? (quote-syntax f)
                                                     (quote-syntax g))))])
           ()
           (m))
          (let-values ([(h) g]) (h 0))
          (g 1)
          (begin
            (set! g 3)
            f)
          (letrec-syntaxes+values
           ([(f-id) (quote-syntax f)])
           ()
           (letrec-syntaxes+values
            ([(g-id) (make-rename-transformer (quote-syntax f-id))])
            ()
            (letrec-syntaxes+values
             ([(m) (lambda (stx) (syntax-local-value (quote-syntax g-id)))])
             ()
             (+ 1 (m))))))))
 #:check (list #t 1 2 3 4))

"lifts in transformer; same number twice"
(eval-expression '(letrec-syntaxes+values
                   ([(n) (lambda (stx)
                           (letrec-syntaxes+values
                            ([(m) (lambda (stx)
                                    (datum->syntax
                                     (quote-syntax here)
                                     (list
                                      (quote-syntax begin)
                                      (list (quote-syntax print)
                                            (syntax-local-lift-expression
                                             (quote-syntax (random))))
                                      (list (quote-syntax newline)))))])
                            ()
                            (datum->syntax (quote-syntax here)
                                           (m))))])
                   ()
                   (list (n) (n))))

"local-expand/capture-lifts"
(eval-expression '(letrec-syntaxes+values
                   ([(m) (lambda (stx)
                           (syntax-local-lift-expression (quote-syntax 1)))])
                   ()
                   (letrec-syntaxes+values
                    ([(n) (lambda (stx)
                            (datum->syntax
                             (quote-syntax here)
                             (list (quote-syntax quote)
                                   (local-expand/capture-lifts
                                    (quote-syntax (m))
                                    'expression
                                    '()))))])
                    ()
                    (let-values ([(x) (n)])
                      (list (car x)
                            (car (car (cdr x)))
                            (car (cdr (cdr (car (cdr x)))))))))
                 #:check '(begin define-values 1))

"get shadower"
(eval-expression
 '(let-values ([(x) 1])
   (letrec-syntaxes+values
    ([(m)
      (lambda (stx)
        (datum->syntax
         #f
         (list (quote-syntax let-values)
               (list
                (list
                 (list (syntax-local-introduce
                        (syntax-local-get-shadower (quote-syntax x))))
                 (quote-syntax 2)))
               (car (cdr (syntax-e stx))))))])
    ()
    (let-values ([(x) 3])
      (m x))))
 #:check 2)

"top-level definitions"
(eval-expression '(define-values (top-x) 'x-at-top))
(eval-expression 'top-x #:check 'x-at-top)
(check-error (eval-expression 'top-y) #rx"undefined")
(eval-expression '(define-values (top-f) (lambda () top-y)))
(check-error (eval-expression '(top-f)) #rx"undefined")
(eval-expression '(define-values (top-y) 'y-at-top))
(eval-expression '(top-f) #:check 'y-at-top)
(eval-expression '(define-values (top-y) 'changed-y-at-top))
(eval-expression '(top-f) #:check 'changed-y-at-top)
(eval-expression '(define-syntaxes (top-m) (lambda (stx)
                                             (datum->syntax
                                              #f
                                              (list (quote-syntax quote)
                                                    (car (cdr (syntax-e stx))))))))
(eval-expression '(top-m 8) #:check 8)
(eval-expression '(define-syntaxes (top-def-top-x)
                   (lambda (stx)
                     (quote-syntax
                      (begin
                        (define-values (top-x) 'macro-introduced-top-x)
                        top-x)))))
(eval-expression/interleaved '(top-def-top-x) #:check 'macro-introduced-top-x)
(eval-expression 'top-x #:check 'x-at-top)
(eval-expression '(begin ; check compilation of multiple top-level forms
                   (define-values (top-z) 'z-at-top)
                   top-z)
                 #:check 'z-at-top)

;; ----------------------------------------

(define (eval-module-declaration mod #:namespace [ns demo-ns])
  (parameterize ([current-namespace ns])
    (eval-expression mod #:namespace ns)))

(eval-module-declaration '(module m0 '#%kernel
                           (define-values (x) 0)
                           (print x) (newline)))

(check-print
 (eval-expression '(#%require 'm0))
 0)

(eval-module-declaration '(module m1 '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (begin-for-syntax
                             (define-values (ten) (quote-syntax 10)))
                           (define-syntaxes (m) (lambda (stx) ten))
                           (define-values (x) 1)
                           (print x) (newline)
                           (define-values (posn make-posn struct:posn posn? 
                                                posn-x posn-y
                                                set-posn-x! set-posn-y!)
                             (values 1 2 3 4 5 6 7 8))
                           (#%provide (prefix-all-defined def:)
                                      (struct posn (x y))) 
                           (print (m)) (newline)
                           (m)))

(eval-module-declaration '(module m2 '#%kernel
                           (#%require 'm1)
                           (print def:x) (newline)))

(check-print
 (eval-expression '(#%require 'm2))
 1
 10
 1)

(eval-module-declaration '(module with-use-site-scope '#%kernel
                           (#%require (for-syntax '#%kernel))

                           (define-syntaxes (identity)
                             (lambda (stx)
                               (let-values ([(misc-id) (car (cdr (syntax-e stx)))])
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list 'lambda '(x)
                                        (list 'let-values (list
                                                           (list (list misc-id) ''other))
                                              'x))))))
                           (identity x)

                           (define-syntaxes (define-identity)
                             (lambda (stx)
                               (datum->syntax
                                #f
                                (list (quote-syntax define-values)
                                      (list (car (cdr (syntax-e stx))))
                                      (quote-syntax  (lambda (x) x))))))
                           (define-identity f)
                           (print (f 5)) (newline)
                           
                           (define-syntaxes (define-x)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax begin-for-syntax)
                                      (list (quote-syntax define-values)
                                            (list (car (cdr (syntax-e stx))))
                                            (quote-syntax 'ct-5))))))
                           (define-x ct-5)
                           (define-syntaxes (ct-five)
                             (lambda (stx)
                               (datum->syntax (quote-syntax here)
                                              (list (quote-syntax quote)
                                                    ct-5))))
                           (print (ct-five)) (newline)))

(check-print
 (namespace-require ''with-use-site-scope demo-ns)
 5
 'ct-5)

(eval-module-declaration '(module definition-shadows-initial-require '#%kernel
                           (#%require (rename '#%kernel orig:list list))
                           (#%provide list)
                           (define-values (list)
                             (lambda (a b)
                               (print a) (newline)
                               (orig:list a b)))))

(eval-module-declaration '(module definition-shadows-plain-require '#%kernel
                           (#%require '#%kernel)
                           (#%provide map)
                           (define-values (map)
                             (lambda (f l)
                               (if (null? l)
                                   '()
                                   (cons (car l) ; don't use `f`
                                         (map f (cdr l))))))))

(eval-module-declaration '(module require-shadows-initial-require '#%kernel
                           (#%require 'definition-shadows-initial-require
                                      'definition-shadows-plain-require)
                           (print (map pair? (list 'a 'b))) (newline)))

(check-print
 (namespace-require ''require-shadows-initial-require demo-ns)
 'a
 '(a b))

(check-error
 (eval-module-declaration '(module m '#%kernel
                            (#%require '#%kernel 
                                       'definition-shadows-initial-require)))
 #rx"already required")

(eval-module-declaration '(module m '#%kernel
                            (define-values (list) 5)
                            (#%require '#%kernel)))

;; ----------------------------------------

(check-print
 (eval-module-declaration '(module forward-reference-in-begin-for-syntax '#%kernel
                            (#%require (for-syntax '#%kernel))
                            (begin-for-syntax
                              (define-values (even) (lambda () odd)))
                            (begin-for-syntax
                              (define-values (odd) (lambda () even)))
                            (begin-for-syntax
                              (define-values (assign-later!) (lambda () (set! later also-later))))
                            (begin-for-syntax
                             (define-values (later) 5)
                             (define-values (also-later) 6)
                             (assign-later!)
                             (print later) (newline))))
 6
 6) ; re-expansion

;; ----------------------------------------

(eval-module-declaration '(module random-n '#%kernel
                           (define-values (n) (random))
                           (#%provide n)))

(eval-module-declaration '(module use-random-n '#%kernel
                           (#%require 'random-n
                                      (for-syntax '#%kernel
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (print (m)) (newline)
                           (print (m)) (newline)
                           (print n) (newline)
                           (print n) (newline)))

"print same number twice, then different number twice"
(namespace-require ''use-random-n demo-ns)

;; ----------------------------------------

;; Fresh compile-time, same run-time:
(eval-module-declaration '(module use-random-n-again '#%kernel
                           (#%require 'random-n
                                      (for-syntax '#%kernel
                                                  'random-n))
                           (define-syntaxes (m)
                             (lambda (stx) (datum->syntax (quote-syntax here)
                                                     n)))
                           (print (m)) (newline)
                           (print n) (newline)))

"first number is fresh, second number is same"
(namespace-require ''use-random-n-again demo-ns)

;; ----------------------------------------

;; Check phase shifting of syntax objects:
(eval-module-declaration '(module two-xes '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (define-values (x) 0)
                           (begin-for-syntax
                            (define-values (x) 1))
                           (#%provide x
                                      (for-syntax x))))

(eval-module-declaration '(module use-two-xes '#%kernel
                           (#%require (for-template 'two-xes)
                                      (for-syntax '#%kernel))
                           (define-values (rt-x-ref) (quote-syntax x))
                           (begin-for-syntax
                             (define-values (ct-x-ref) (quote-syntax x)))
                           (#%provide rt-x-ref
                                      (for-syntax ct-x-ref))))

(eval-module-declaration '(module use-x-ref '#%kernel
                           (#%require 'use-two-xes
                                      (for-syntax '#%kernel
                                                  'use-two-xes))
                           (define-syntaxes (ct-m) (lambda (stx) ct-x-ref))
                           (define-syntaxes (rt-m) (lambda (stx) rt-x-ref))
                           (print (ct-m)) (newline)
                           (print (rt-m)) (newline)))

(check-print
 (namespace-require ''use-x-ref demo-ns)
 1
 0)

;; ----------------------------------------
;; Custom `#%module-begin'

(eval-module-declaration '(module printing-mb '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (#%provide (all-from-except '#%kernel #%module-begin)
                                      (rename module-begin #%module-begin))
                           (define-syntaxes (module-begin)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (cons
                                 (quote-syntax #%module-begin)
                                 (map (lambda (b)
                                        (datum->syntax
                                         (quote-syntax here)
                                         (list (quote-syntax begin)
                                               (list (quote-syntax print) b)
                                               (list (quote-syntax newline)))))
                                      (cdr (syntax-e stx)))))))))

(eval-module-declaration '(module printed 'printing-mb
                           (+ 1 2)
                           (+ 3 4)))

(check-print
 (namespace-require ''printed demo-ns)
 3
 7)

(eval-module-declaration '(module intro-printed-submodule '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (#%provide m)
                           (define-syntaxes (m)
                             (lambda (stx)
                               (quote-syntax
                                (module sub 'printing-mb
                                  (+ 5 6)
                                  (+ 7 8)))))))

(eval-module-declaration '(module printed-submodule '#%kernel
                           (#%require 'intro-printed-submodule)
                           (m)))

(check-print
 (namespace-require '(submod 'printed-submodule sub) demo-ns)
 11
 15)

;; ----------------------------------------
;; local-expanding `#%module-begin'

(eval-module-declaration '(module local-expanding-mb '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (#%provide (all-from-except '#%kernel #%module-begin)
                                      (rename module-begin #%module-begin))
                           (define-syntaxes (module-begin)
                             (lambda (stx)
                               (local-expand (datum->syntax
                                              #f
                                              (cons
                                               (quote-syntax #%module-begin)
                                               (cdr (syntax-e stx))))
                                             'module-begin
                                             null)))))

(eval-module-declaration '(module local-expanded-mb 'local-expanding-mb
                           (define-values (x) 'x)
                           (print x) (newline)))

(check-print
 (namespace-require ''local-expanded-mb demo-ns)
 'x)

;; ----------------------------------------
;; Submodule

(eval-module-declaration '(module with-pre-submodule '#%kernel
                           (module a '#%kernel
                             (#%provide a)
                             (define-values (a) 'a))
                           (#%require (submod "." a))
                           (print a) (newline)))

(check-print
 (namespace-require ''with-pre-submodule demo-ns)
 'a)

(eval-module-declaration '(module with-post-submodule '#%kernel
                           (#%provide b)
                           (define-values (b) 'b)
                           (module* b '#%kernel
                             (#%require (submod ".."))
                             (print b) (newline))))

(check-print
 (namespace-require '(submod 'with-post-submodule b) demo-ns)
 'b)

(eval-module-declaration '(module with-#f-submodule '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (define-values (c) 'c)
                           (define-syntaxes (c2) (lambda (stx) (quote-syntax c)))
                           (module* c #f
                             (print c) (newline)
                             (print c2) (newline))))

(check-print
 (namespace-require '(submod 'with-#f-submodule c) demo-ns)
 'c
 'c)

(eval-module-declaration '(module used-by-shifted-submodule '#%kernel
                           (define-values (x) 'x)
                           (#%provide x)))

(eval-module-declaration '(module with-shifted-pre-submodule '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (begin-for-syntax
                             (module xa '#%kernel
                               (#%require 'used-by-shifted-submodule)
                               (#%provide xa)
                               (define-values (xa) x)))
                           (#%require (submod "." xa))
                           (print xa) (newline)))

(check-print
 (namespace-require ''with-shifted-pre-submodule demo-ns)
 'x)

(eval-module-declaration '(module with-shifted-#f-submodule '#%kernel
                           (#%require (for-syntax '#%kernel
                                                  'used-by-shifted-submodule))
                           (define-values (d) 'd)
                           (begin-for-syntax
                             (define-values (d-stx) (quote-syntax d))
                             (module* d #f
                               (#%provide get-d-stx)
                               x
                               (define-values (get-d-stx) (lambda () d-stx))))))

(eval-module-declaration '(module use-shifted-#f-submodule '#%kernel
                           (#%require (for-syntax '#%kernel
                                                  (submod 'with-shifted-#f-submodule d)))
                           (define-syntaxes (m) (lambda (stx) (get-d-stx)))
                           (print (m)) (newline)))

(check-print
 (namespace-require ''use-shifted-#f-submodule demo-ns)
 'd)

(eval-module-declaration '(module with-#f-submodule-provide '#%kernel
                           (define-values (e) 'e)
                           (module* e #f
                             (#%provide e))))

(eval-module-declaration '(module use-submodule-provide '#%kernel
                           (#%require (submod 'with-#f-submodule-provide e))
                           (print e) (newline)))

(check-print
 (namespace-require ''use-submodule-provide demo-ns)
 'e)

;; ----------------------------------------
;; rename-transformer provide redirection

(eval-module-declaration '(module provides-original-binding '#%kernel
                           (#%provide x)
                           (define-values (x) 'x)))

(eval-module-declaration '(module provides-rename-transformer '#%kernel
                           (#%require (for-syntax '#%kernel)
                                      'provides-original-binding)
                           (#%provide y)
                           (define-syntaxes (y) (make-rename-transformer
                                                 (quote-syntax x)))))

(eval-module-declaration '(module checks-free=id '#%kernel
                           (#%require (for-syntax '#%kernel)
                                      'provides-original-binding
                                      'provides-rename-transformer)
                           (print (if (free-identifier=? (quote-syntax x)
                                                         (quote-syntax y))
                                      'free=id
                                      'not-free=id))
                           (newline)))

(check-print
 (namespace-require ''checks-free=id demo-ns)
 'free=id)

;; ----------------------------------------
;; syntax-local-value of module binding

(eval-module-declaration '(module define-non-transformer '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (#%provide car-id)
                           (define-syntaxes (car-id) (quote-syntax car))))

(eval-module-declaration '(module use-non-transformer '#%kernel
                           (#%require (for-syntax '#%kernel)
                                      'define-non-transformer)
                           (define-syntaxes (m)
                             (lambda (stx) (syntax-local-value (quote-syntax car-id))))
                           (print ((m) '(1 2)))
                           (newline)))

(check-print
 (namespace-require ''use-non-transformer demo-ns)
 1)

;; ----------------------------------------
;; syntax-local-lift-{expression,module}, etc.

(eval-module-declaration '(module lifts '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (module pre '#%kernel
                             (#%provide pre)
                             (define-values (pre) 'pre))
                           (define-syntaxes (m)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list
                                 (quote-syntax begin)
                                 (list (quote-syntax print)
                                       (syntax-local-lift-expression
                                        (quote-syntax (+ 1 2))))
                                 (list (quote-syntax newline))))))
                           (m)
                           (list (m))
                           (define-values (dummy) (m))
                           (define-syntaxes (n)
                             (lambda (stx)
                               (syntax-local-lift-module
                                (quote-syntax (module sub '#%kernel
                                                (#%provide sub)
                                                (define-values (sub) 'sub))))
                               (syntax-local-lift-module
                                (quote-syntax (module* main #f
                                                (print x) (newline))))
                               (syntax-local-lift-module-end-declaration
                                (quote-syntax (define-values (done) 'done)))
                               (syntax-local-lift-provide
                                (quote-syntax done))
                               (let-values ([(pre-id) (syntax-local-lift-require
                                                       (quote-syntax (submod "." pre))
                                                       (quote-syntax pre))])
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list
                                   (quote-syntax begin)
                                   (list (quote-syntax print) pre-id)
                                   (quote-syntax (newline))
                                   (quote-syntax (#%require (submod "." sub)))
                                   (quote-syntax (print sub))
                                   (quote-syntax (newline)))))))
                           (n)
                           (define-values (x) '*)
                           (define-syntaxes (as-expr)
                             (lambda (stx)
                               ;; (syntax-local-lift-module-end-declaration
                               ;;  (quote-syntax (define-values (fail) 'this-wont-work)))
                               (syntax-local-lift-module-end-declaration
                                (quote-syntax (begin (print 'end) (newline))))
                               (quote-syntax (void))))
                           (list (as-expr))))

(check-print
 (namespace-require '(submod 'lifts main) demo-ns)
 3
 3
 3
 'pre
 'sub
 'end
 '*)

(eval-module-declaration '(module use-lifted-provide '#%kernel
                           (#%require 'lifts)
                           (print done) (newline)))

(check-print
 (namespace-require ''use-lifted-provide demo-ns)
 'done)

;; ----------------------------------------
;; `local-transformer-expand`

(eval-module-declaration '(module local-transformer-expand '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (define-syntaxes (m)
                             (lambda (stx)
                               (datum->syntax
                                #f
                                (list
                                 (quote-syntax letrec-syntaxes+values)
                                 (list
                                  (list (list (car (cdr (syntax-e stx))))
                                        (local-transformer-expand
                                         (car (cdr (cdr (syntax-e stx))))
                                         'expression
                                         '())))
                                 (list)
                                 (car (cdr (cdr (cdr (syntax-e stx)))))))))
                           (begin-for-syntax
                             (#%require (for-syntax '#%kernel))
                             (define-syntaxes (tm)
                               (lambda (stx)
                                 (quote-syntax (quote-syntax 'local-trans)))))
                           (print (m p (lambda (stx) (tm)) (p))) (newline)))

(check-print
 (namespace-require ''local-transformer-expand demo-ns)
 'local-trans)

;; ----------------------------------------
;; `expand` in `#%provide`

(eval-module-declaration '(module expand-provide '#%kernel
                           (#%require (for-syntax '#%kernel))
                           (module sub '#%kernel
                             (#%provide a-sub b-sub)
                             (define-values (a-sub) 'a-sub)
                             (define-values (b-sub) 'b-sub))
                           (#%require (submod "." sub))
                           (define-values (a-here) 'a-here)
                           (define-values (b-here) 'b-here)
                           (define-syntaxes (all-a)
                             (lambda (stx)
                               (let-values ([(here) (syntax-local-module-defined-identifiers)]
                                            [(there) (syntax-local-module-required-identifiers
                                                      '(submod "." sub)
                                                      0)]
                                            [(keep-a) (lambda (id)
                                                        (regexp-match? #rx"^a"
                                                                       (symbol->string
                                                                        (syntax-e id))))])
                                 (define-values (filter)
                                   (lambda (f l)
                                     (if (null? l)
                                         null
                                         (if (f (car l))
                                             (cons (car l) (filter f (cdr l)))
                                             (filter f (cdr l))))))
                                 (datum->syntax
                                  #f
                                  (cons
                                   (quote-syntax begin)
                                   (append
                                    (filter keep-a (hash-ref here 0))
                                    (filter keep-a (cdr (assv 0 there)))))))))
                           (#%provide (expand (all-a)))))

(eval-module-declaration '(module use-expand-provide '#%kernel
                           (#%require 'expand-provide)
                           (print (list a-sub a-here)) (newline)))

(check-print
 (namespace-require ''use-expand-provide demo-ns)
 (list 'a-sub 'a-here))

;; ----------------------------------------
;; cross-phase persistent declaration

(eval-module-declaration '(module cross-phase-persistent '#%kernel
                           (#%declare #:cross-phase-persistent)
                           (#%require '#%kernel)
                           (#%provide gen)
                           (define-values (gen) (gensym "g"))
                           (module ignored '#%kernel)
                           (module* also-ignored '#%kernel)
                           (begin
                             (define-values (y) (lambda () (error "anything")))
                             (define-values (x) (case-lambda
                                                  [() (error "anything")]
                                                  [(x) (set! x x)])))
                           (define-values (z) (list
                                               #t
                                               (cons 1 2)
                                               "string"
                                               #"bytes"
                                               'symbol
                                               (gensym)
                                               (string->uninterned-symbol "u")))))

(eval-module-declaration '(module use-cross-phase-persistent '#%kernel
                           (#%require (for-syntax '#%kernel
                                                  'cross-phase-persistent)
                                      (for-meta 2 '#%kernel
                                                'cross-phase-persistent))
                           (begin-for-syntax
                             (define-syntaxes (ctct-gen)
                               (lambda (stx)
                                 (datum->syntax
                                  (quote-syntax here)
                                  (list (quote-syntax quote)
                                        gen)))))
                           (define-syntaxes (ct-gen)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax quote)
                                      gen))))
                           (define-syntaxes (use-ctct-gen)
                             (lambda (stx)
                               (datum->syntax
                                (quote-syntax here)
                                (list (quote-syntax quote)
                                      (ctct-gen)))))

                           (print (equal? (ct-gen) (use-ctct-gen))) (newline)))

(check-print
 (namespace-require ''use-cross-phase-persistent demo-ns)
 #t)

;; ----------------------------------------
;; for-label imports

(eval-module-declaration '(module provides-title '#%kernel
                           (#%provide title)
                           (define-values (title) "Of Mice and Men")))

(eval-module-declaration '(module imports-title-for-label '#%kernel
                           (#%require (for-label 'provides-title))
                           (print (identifier-binding (quote-syntax title))) (newline)
                           (print (cadr (identifier-label-binding (quote-syntax title)))) (newline)))

(check-print
 (namespace-require ''imports-title-for-label demo-ns)
 #f
 'title)

;; ----------------------------------------
;; namespace-attach

(eval-module-declaration '(module provides-random-r '#%kernel
                           (define-values (r) (random))
                           (#%provide r)))


(define random-r (parameterize ([current-namespace demo-ns])
                   (dynamic-require ''provides-random-r 'r)))
(unless (equal? random-r (parameterize ([current-namespace demo-ns])
                           (dynamic-require ''provides-random-r 'r)))
  (error "not the same random number"))
'ok-dynamic

(define other-ns (make-namespace))
(namespace-attach-module demo-ns ''provides-random-r other-ns)

(unless (equal? random-r (parameterize ([current-namespace other-ns])
                           (dynamic-require ''provides-random-r 'r)))
  (error "not the same random number after attach"))
'ok-instance

(namespace-attach-module demo-ns ''provides-random-r other-ns) ; re-attach ok
'ok-reattach

(define third-ns (make-namespace))
(namespace-attach-module-declaration demo-ns ''provides-random-r third-ns)

(when (equal? random-r (parameterize ([current-namespace third-ns])
                         (dynamic-require ''provides-random-r 'r)))
  (error "the same random number after declaration attach"))
'ok-declaration

(namespace-attach-module-declaration demo-ns ''provides-random-r third-ns) ; re-attach ok
(check-error
 (namespace-attach-module demo-ns ''provides-random-r third-ns)
 #rx"different instance")

(define has-already-ns (make-namespace))
(namespace-attach-module (current-namespace) ''#%kernel has-already-ns)
(namespace-require ''#%kernel has-already-ns)
(eval-module-declaration '(module provides-random-r '#%kernel
                           (define-values (r) 5)
                           (#%provide r))
                         #:namespace has-already-ns)
(parameterize ([current-namespace has-already-ns])
  (dynamic-require ''provides-random-r 'r))
(check-error
 (namespace-attach-module-declaration demo-ns ''provides-random-r has-already-ns)
 #rx"different declaration")

;; ----------------------------------------
;; module redeclaration

(eval-module-declaration '(module to-be-redeclared '#%kernel
                           (define-values (tbr-x) 'x)
                           (print tbr-x) (newline)))
(check-print
 (eval-expression '(#%require 'to-be-redeclared))
 'x)

(check-print
 (eval-module-declaration '(module to-be-redeclared '#%kernel
                            (define-values (tbr-y) 'y)
                            (print tbr-y) (newline)))
 'y)

;; ----------------------------------------
;; module exports

(define one-of-each-provide-at-each-phase-expr
  '(module one-of-each-provide-at-each-phase '#%kernel
    (#%require (for-syntax '#%kernel)
               (for-meta 2 '#%kernel))
    (define-values (one) 1)
    (define-values (also-one) 1)
    (define-syntaxes (one-s) (quote-syntax 1))
    (begin-for-syntax
      (define-values (two) 2)
      (define-values (also-two) 2)
      (define-syntaxes (two-s) (quote-syntax 2)))
    (#%provide one one-s
               (for-syntax two two-s))))

(eval-module-declaration one-of-each-provide-at-each-phase-expr)

(parameterize ([current-namespace demo-ns])
  (eval-expression '(call-with-values
                     (lambda () (module->exports ''one-of-each-provide-at-each-phase))
                     list)
                   #:check '(((0 (one ())) (1 (two ())))
                             ((0 (one-s ())) (1 (two-s ()))))))

(parameterize ([current-namespace demo-ns])
  (eval-expression '(module->indirect-exports ''one-of-each-provide-at-each-phase)
                   #:check '((0 also-one) (1 also-two))))

(check-value (call-with-values
              (lambda () (module-compiled-exports
                     (compile one-of-each-provide-at-each-phase-expr demo-ns)))
              list)
             '(((0 (one ())) (1 (two ())))
               ((0 (one-s ())) (1 (two-s ())))))

(check-value (module-compiled-indirect-exports
              (compile one-of-each-provide-at-each-phase-expr demo-ns))
             '((0 also-one) (1 also-two)))

;; ----------------------------------------
;; top-level fallbacks

(define s-only-in-demo (namespace-syntax-introduce (datum->syntax #f 'car) demo-ns))

(define alt-ns (make-namespace))
(namespace-attach-module demo-ns ''#%kernel alt-ns)

(define s-also-in-alt (namespace-syntax-introduce s-only-in-demo alt-ns))
(define s-only-in-alt (namespace-syntax-introduce (datum->syntax #f 'car) alt-ns))

(check-value (hash-ref (syntax-debug-info s-only-in-demo) 'fallbacks #f)
             #f)
(check-value (hash-ref (syntax-debug-info s-only-in-alt) 'fallbacks #f)
             #f)
(check-value (length (hash-ref (syntax-debug-info s-also-in-alt) 'fallbacks null))
             1)
(check-value (list->set (hash-ref (syntax-debug-info s-also-in-alt) 'context #f))
             (set-union (list->set (hash-ref (syntax-debug-info s-only-in-demo) 'context #f))
                        (list->set (hash-ref (syntax-debug-info s-only-in-alt) 'context #f))))

(check-value (cadr (identifier-binding s-only-in-demo))
             'car)
(check-value (identifier-binding s-only-in-alt)
             #f)
(check-value (cadr (identifier-binding s-also-in-alt))
             'car)

(namespace-require ''#%kernel alt-ns)
(check-value (cadr (identifier-binding s-only-in-alt))
             'car)

(eval-module-declaration '(module kar '#%kernel
                           (#%provide (rename kar car))
                           (define-values (kar) 5))
                         #:namespace alt-ns)
(eval-expression '(#%require 'kar) #:namespace alt-ns)
(eval-expression 'car #:namespace alt-ns
                 #:check 5)

(check-value (cadr (identifier-binding s-only-in-alt))
             'kar)
(check-value (cadr (identifier-binding s-also-in-alt))
             'car) ; because using combined scopes is ambiguous

;; ----------------------------------------
;; Explicitly constructed binding sets

(eval-expression
 #:check '(#t #f)
 '(let-values ([(bs) (syntax-binding-set-extend
                      (syntax-binding-set)
                      'car 0 (module-path-index-join ''#%runtime #f))])
    (list
     (free-identifier=? (syntax-binding-set->syntax bs 'car)
                        (quote-syntax car))
     (free-identifier=? (syntax-binding-set->syntax bs 'cdr)
                        (quote-syntax cdr)))))

;; ----------------------------------------
;; Machine-independent compilation format and recompilation

(let ([mi-c (parameterize ([current-compile-target-machine #f])
              (compile-expression `(module to-recompile '#%kernel
                                     (define-values (x) 0)
                                     (print x)
                                     (newline))
                                  #:serializable? #t))])
  (parameterize ([current-namespace demo-ns])
    (define c2 (compiled-expression-recompile mi-c))
    (eval c2 demo-ns)
    (check-print
     (namespace-require ''to-recompile demo-ns)
     0)))
