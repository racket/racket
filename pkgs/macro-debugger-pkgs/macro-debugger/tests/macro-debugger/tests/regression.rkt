#lang racket/base
(require rackunit)
(require macro-debugger/model/debug
         macro-debugger/model/steps
         "../test-setup.rkt")
(provide regression-tests)

(define regression-tests
  (test-suite "Regression tests"
    ;; Fixed 9/2006: mismatched binding+bound variables
    (test-case "hiding on binding forms"
      (let ([stx (stx/hide-all (trace #'(let ([x 1]) x)))])
        (with-syntax ([(?let ([?x-def _]) ?x-use) stx])
          (check-pred identifier? #'?x-def)
          (check-pred identifier? #'?x-use)
          (check bound-identifier=? #'?x-def #'?x-use))))
    ;; Fixed 10/2/2006: bad handling of renames
    (test-case "renames in lsv, etc"
      (check-pred syntax? (stx/hide-none (trace #'(let () 1))))
      (check-pred syntax? (stx/hide-none (trace #'(letrec () 1))))
      (check-pred syntax? (stx/hide-none (trace #'(let-syntax () 1)))))
    ;; Fixed 10/2/2006: error above manifests in classes, too
    (test-case "renames in lsv, via class"
      (check-pred syntax? (stx/hide-none (trace #'(class object% (super-new))))))
    ;; Fixed 10/2/2006: PR 8305: Error in module (pass2)
    (test-case "interrupted expr in module body"
      (check-equal? (stx/hide-standard
                     (trace '(module m mzscheme (define x (lambda)))))
                    #f)
      (check-equal? (stx/hide-standard
                     (trace '(module m mzscheme (void) (define x (lambda)))))
                    #f))
    ;; Error in module (pass1)
    (test-case "interrupted module-body element"
      (check-equal? (stx/hide-standard (trace '(module m mzscheme (define x))))
                    #f)
      (check-equal? (stx/hide-standard (trace '(module m mzscheme (void) (define x))))
                    #f))
    ;; Fixed 11/13/2006: error in lsv rhs
    (test-case "error in lsv rhs"
      (check-pred interrupted-node? 
                  (trace #'(letrec-syntaxes+values ([(x) (error 'gotcha)]) ()
                                                   'never-reached))))
    ;; Fixed 11/13/2006: lifting in module
    (test-case "lift in module"
      (check-pred syntax?
                  (stx/hide-none
                   (trace '(module m mzscheme
                             (require mzlib/etc)
                             (define x (begin-lifted 1)))))))

    ;; Fixed 2/9/2007: defstx in brules misparsed & mishandled
    (test-case "reductions & internal define-syntax"
      (reductions
       (trace '(let ([x 1])
                 (define-syntax m
                   (syntax-rules ()
                     [(_ x) (begin (lambda (x) x) (lambda (x) x) x)]))
                 (m x)))))

    ;; Fixed 2/9/2007: Handled b:defstx in hiding code
    (test-case "reductions & internal define-syntax"
      (check-pred syntax?
                  (stx/hide-none
                   (trace/t '(lambda ()
                               (define-syntaxes (m) (lambda _ (quote-syntax *)))
                               (m))))))

    ;; Fixed 2/9/2007: missing stx->list before length
    (test-case "hiding error & stx pairs"
      (check-pred syntax?
               (stx/hide-none
                (trace '(let-syntax ([m (syntax-rules () [(_ x) (begin x)])])
                          (m *))))))
    (test-case "hiding in block, splicing"
      (stx/hide-none
       (trace '(let-syntax ([m (syntax-rules () [(_ x) (begin x)])])
                 *
                 (m *)))))
    (test-case "hiding in block, variable"
      (stx/hide-none
       (trace '(let-syntax ([m (syntax-rules () [(_ x) x])])
                 (list (m *)) ;; FIXME
                 *))))
    (test-case "hiding in block, expression"
      (check-pred syntax?
               (stx/hide-none
                (trace '(let-syntax ([m (syntax-rules () [(_ x) (list x)])])
                          (m *))))))

    ;; Reported by robby (2/8/2007), traced to bug in expander
    (test-case "hiding & lambda in module"
      (check-pred syntax?
                  (stx/hide-none
                   (trace '(module m '#%kernel
                             (#%module-begin (lambda () 'a)))))))

    ;; Discovered 5/7/2007
    (test-case "hiding and error within lambda"
      (let ([rs (parameterize ((macro-policy hide-all-policy))
                  (reductions (trace '(with-handlers () (lambda)))))])
        (check-pred list? rs)
        (check-true (andmap misstep? rs))
        (check-true (= 1 (length rs)))))

    ;; Discovered 5/7/2007
    (test-case "hiding and error within lambda 2"
      (let ([rs (parameterize ((macro-policy hide-all-policy))
                  (reductions (trace '(with-handlers ([void void]) (lambda)))))])
        (check-pred list? rs)
        (check-true (andmap misstep? rs))
        (check-true (= 1 (length rs)))))

    ;; Distilled from Robby bug report (5/12/2007)
    ;; Fixed 5/17/2007
    (test-case "hiding: keeping lifts in sync"
      (let ([freshname (gensym)])
        (eval `(module ,freshname racket/base
                 (require racket/contract)
                 (provide/contract
                  [f (-> integer? integer?)]
                  [c integer?])
                 (define (f x) (add1 x))
                 (define c 1)))
        (let ([rs (parameterize ((macro-policy standard-policy))
                    (reductions
                     (trace `(module m mzscheme
                               (require (quote ,freshname))
                               (define (g y) c)
                               (define h c)
                               (add1 (g 2))))))])
          (check-pred list? rs)
          (for ([x (in-list rs)])
            (check-true (not (misstep? x)))))))

    ;; Bug from samth (6/5/2007)
    ;; problem seems to come from define-syntax -> letrec-syntaxes+values
    ;; transformation, undoes expansion of srhss (so rename fails)
    (test-case "more rename/frontier troubles"
      (let ([rs (reductions
                 (trace '(module m (lib "htdp-advanced.ss" "lang")
                           (local [(define x 1)] x))))])
        (check-pred list? rs)))

    ;; Distilled from Sam/typed-scheme (8/24/2007)
    (test-case "transformer calls 'expand'"
      (check-pred deriv?
                  (trace '(let-syntax ([m (lambda (stx)
                                            (syntax-case stx ()
                                              [(m e)
                                               (expand #'e)]))])
                            (m 4)))))
    (test-case "define-syntaxes rhs calls 'expand'"
      (check-pred deriv?
                  (trace '(define-syntax m (expand '(or 1 2))))))
    (test-case "lsv rhs calls 'expand'"
      (check-pred deriv?
                  (trace '(let-syntax ([m (expand '(or 1 2))]) 'nothing))))

    ;; Added 2/18/2008
    (test-case "interrupted module-begin"
      (let* ([freshname (gensym)]
             [rs (parameterize ((macro-policy standard-policy))
                   (reductions
                    (trace `(module m mzscheme
                              (require ,freshname)
                              (define (g y) c)
                              (define h c)
                              (add1 (g 2))))))])
        (check-pred list? rs)
        (check-true (ormap misstep? rs))))

    ;; Added 1/3/2008
    ;; Based on PR 10000
    (test-case "eval within module expansion"
      (let ([freshname (gensym)])
        (eval `(module ,freshname scheme
                 (provide meval)
                 (define-syntax (meval stx)
                   (syntax-case stx ()
                     [(meval e)
                      (parameterize ((current-namespace (make-base-namespace)))
                        (eval `(define one '1))
                        (let ([v (eval `(+ 1 ,#'e))])
                          #`(quote #,v)))]))))
        (eval `(require ',freshname))
        (check-pred deriv?
                    (trace `(meval (+ 1 2))))
        (check-pred deriv?
                    (trace `(module m mzscheme
                              (require ',freshname)
                              (meval (+ 1 2)))))))

    (test-case "macro def within begin"
      (let ([rs (reductions
                 (parameterize ((current-namespace testing-namespace))
                   (trace '(begin
                             (define-syntax-rule (m x e)
                               (define x e))
                             (m y 12)))))])
        (check-pred list? rs)
        (check-false (ormap misstep? rs))
        (check-true (for/or ([step rs])
                      (equal? (syntax->datum (state-e (protostep-s1 step)))
                              '(m y 12))
                      (equal? (syntax->datum (state-e (step-s2 step)))
                              '(define y 12)))
                    "looking for m => define")))

    ;; Added 3/12/2012 based on bug from cce
    (test-case "begin-for-syntax ends with phase1 eval"
      (let ([d (trace '(module m '#%kernel
                         (#%module-begin
                          (#%require (for-syntax '#%kernel))
                          (begin-for-syntax
                           (syntax-local-value (quote-syntax lambda) void)))))])
        (check-pred deriv? d)
        (check-pred ok-node? d)))
    (test-case "syntax-local-value in provide"
      (let ([d (trace '(module m racket/base
                         (#%plain-module-begin
                          (provide (except-out (all-defined-out) x y))
                          (define-values (x) 1)
                          (define-values (y) 2))))])
        (check-pred deriv? d)
        (check-pred ok-node? d)))

    ;; Added 10/11/2012 based on bug from mflatt,shriram
    (test-case "recover from jump"
      (let ([d (trace '(module m racket/base
                         (require (for-syntax racket/base))
                         (define-syntax (convert-error stx)
                           (syntax-case stx ()
                             [(convert-error expr)
                              (with-handlers ([exn? (lambda (e) #'(quote error))])
                                (local-expand #'expr 'expression null))]))
                         (convert-error (lambda))))])
        (check-pred deriv? d)
        (check-pred ok-node? d)))
    ))
