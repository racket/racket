#lang racket/base

(module util racket/base
  (require setup/path-to-relative
           racket/runtime-path
           "test-util.rkt"
           (for-syntax racket/base))
  (provide exec-syntax-error-tests
           exec-runtime-error-tests
           syn-err-test-namespace)
  
  (define-runtime-path this-dir ".")
  
  (define syn-err-test-namespace (make-base-namespace))
  (parameterize ([current-namespace syn-err-test-namespace])
    (eval '(require redex/reduction-semantics)))
  
  (define (syntax-error-test-setup thunk)
    (parameterize ([current-namespace syn-err-test-namespace])
      (with-handlers ([exn:fail:syntax? 
                       (λ (exn) 
                         (values (exn-message exn)
                                 (map source-location (exn:fail:syntax-exprs exn))))])
        (thunk))))
  (define (runtime-error-test-setup thunk)
    (define errortrace-key (dynamic-require 'errortrace/errortrace-key 'errortrace-key))
    (parameterize ([current-compile ((dynamic-require 'errortrace/errortrace-lib
                                                      'make-errortrace-compile-handler))])
      (with-handlers ([exn:fail? 
                       (λ (exn) 
                         (values (exn-message exn)
                                 (let ([ans (let ([marks (continuation-mark-set->list
                                                          (exn-continuation-marks exn)
                                                          errortrace-key)])
                                              (if (null? marks) '() (list (cdar marks))))])
                                   (let loop ([ans ans])
                                     (cond
                                       [(pair? ans) (cons (loop (car ans)) (loop (cdr ans)))]
                                       [(path? ans) (path->relative-string/library ans)]
                                       [else ans])))))])
        (thunk))))
  
  (define ((exec-error-tests setup exec) path)
    (for ([test (read-tests (build-path this-dir path))])
      (exec-error-test test exec setup)))
  (define exec-syntax-error-tests
    (exec-error-tests syntax-error-test-setup expand))
  (define exec-runtime-error-tests
    (exec-error-tests runtime-error-test-setup eval))
  
  (define (exec-error-test spec exec setup)
    (define-values (file line expected-message expected-sources test)
      (make-error-test spec))
    (let-values ([(actual-message actual-sources)
                  (setup (λ () (begin (exec test) (values "" '()))))])
      (test/proc (λ () actual-message) expected-message line file)
      (test/proc (λ () actual-sources) expected-sources line file)))
  
  (define (make-error-test spec)
    (syntax-case spec ()
      [(message named-pieces body)
       (make-error-test (syntax/loc spec (message named-pieces () body)))]
      [(message ([loc-name loc-piece] ...) ([non-loc-name non-loc-piece] ...) body)
       (values (and (path? (syntax-source spec))
                    (path->relative-string/library (syntax-source spec)))
               (syntax-line spec)
               (syntax-e #'message)
               (map source-location (syntax->list #'(loc-piece ...)))
               #'(let-syntax ([subst 
                               (λ (stx)
                                 (syntax-case (syntax-local-introduce stx) ()
                                   [(_ loc-name ... non-loc-name ...)
                                    #'body]))])
                   (subst loc-piece ... non-loc-piece ...)
                   (void)))]))
  
  (define (source-location stx)
    (list (and (path? (syntax-source stx))
               (path->relative-string/library (syntax-source stx))) 
          (syntax-line stx) 
          (syntax-column stx) 
          (syntax-position stx)
          (syntax-span stx)))
  
  (define (read-tests path)
    (call-with-input-file path
      (λ (port)
        (port-count-lines! port)
        (let loop ()
          (define test (read-syntax path port))
          (if (eof-object? test)
              '()
              (cons test (loop))))))))

(require "test-util.rkt"
         redex/reduction-semantics
         (for-syntax racket/base)
         'util)

(reset-count)

(parameterize ([current-namespace syn-err-test-namespace])
  (eval (quote-syntax
         (define-language syn-err-lang
           (M (M M)
              number)
           (E hole
              (E M)
              (number E))
           (X (number any)
              (any number))
           (Q (Q ...)
              variable)
           (UN (add1 UN)
               zero)))))


(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require redex/reduction-semantics redex/pict))
  (eval '(define-language L
           (s a b c)))
  (exec-runtime-error-tests "run-err-tests/define-union-language.rktd"))

(exec-syntax-error-tests "syn-err-tests/language-definition.rktd")

;; term with #:lang tests
(exec-syntax-error-tests "syn-err-tests/term-lang.rktd")

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require redex/reduction-semantics))
  (exec-runtime-error-tests "run-err-tests/judgment-form-undefined.rktd"))

(exec-syntax-error-tests "syn-err-tests/metafunction-definition.rktd")

(exec-syntax-error-tests "syn-err-tests/relation-definition.rktd")

(exec-syntax-error-tests "syn-err-tests/reduction-relation-definition.rktd")

(exec-syntax-error-tests "syn-err-tests/redex-let.rktd")

(exec-syntax-error-tests "syn-err-tests/judgment-form-definition.rktd")
(exec-syntax-error-tests "syn-err-tests/judgment-holds.rktd")

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require redex/reduction-semantics))
  (eval '(define-language L
           (s a b c)))
  (eval '(define-judgment-form L
           #:mode (ctc-fail I O)
           #:contract (ctc-fail s s)
           [(ctc-fail a q)]
           [(ctc-fail b s)
            (ctc-fail q s)]
           [(ctc-fail c s)
            (ctc-fail a s)]))
  (exec-runtime-error-tests "run-err-tests/judgment-form-contracts.rktd")
  (exec-runtime-error-tests "run-err-tests/judgment-form-undefined.rktd")
  (exec-runtime-error-tests "run-err-tests/judgment-form-ellipses.rktd"))

(parameterize ([current-namespace (make-base-namespace)])
  (eval '(require redex/reduction-semantics))
  (eval '(define-language L))
  (eval '(define-metafunction L
           ∨ : boolean boolean -> boolean
           [(∨ #f #f) #f]
           [(∨ boolean boolean) #t]))
  (exec-runtime-error-tests "run-err-tests/metafunction-no-match.rktd"))

(require redex/private/term
         redex/private/lang-struct)
(define-namespace-anchor here)
  (define ns (namespace-anchor->namespace here))
  (parameterize ([current-namespace ns])
    (exec-runtime-error-tests "run-err-tests/term.rktd"))
  
  (exec-syntax-error-tests "syn-err-tests/term.rktd")
  
(print-tests-passed 'err-loc-test.rkt)