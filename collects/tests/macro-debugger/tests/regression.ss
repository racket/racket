#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
         macro-debugger/model/debug
         macro-debugger/model/steps
         "../test-setup.ss")
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
                             (require (lib "etc.ss"))
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
        (eval `(module ,freshname mzscheme
                 (require (lib "contract.ss"))
                 (provide/contract [f (integer? . -> . integer?)]
                                   [c integer?])
                 (define (f x) (add1 x))
                 (define c 1)))
        (let ([rs (parameterize ((macro-policy standard-policy))
                    (reductions
                     (trace `(module m mzscheme
                               (require ',freshname)
                               (define (g y) c)
                               (define h c)
                               (add1 (g 2))))))])
          (check-pred list? rs)
          (check-true (andmap step? rs)))))

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
    ))
