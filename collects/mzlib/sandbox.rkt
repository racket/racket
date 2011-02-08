#lang scheme/base

(require scheme/sandbox
         (prefix-in mz: (only-in mzscheme make-namespace)))

(provide sandbox-init-hook
         sandbox-reader
         sandbox-input
         sandbox-output
         sandbox-error-output
         sandbox-propagate-breaks
         sandbox-coverage-enabled
         sandbox-namespace-specs
         sandbox-override-collection-paths
         sandbox-security-guard
         sandbox-path-permissions
         sandbox-network-guard
         sandbox-make-inspector
         sandbox-eval-limits
         kill-evaluator
         break-evaluator
         set-eval-limits
         put-input
         get-output
         get-error-output
         get-uncovered-expressions
         call-with-limits
         with-limits
         exn:fail:resource?
         exn:fail:resource-resource
         (rename-out [*make-evaluator make-evaluator]
                     [gui? mred?]))

(define-namespace-anchor anchor)

;; Compatbility:
;;   * recognize 'r5rs, etc, and wrap them as a list.
;;   * 'begin form of reqs
;;   * more aggressively extract requires from lang and reqs
(define *make-evaluator
  (case-lambda
    [(lang reqs . progs)
     (with-ns-params
      (lambda ()
        (let ([beg-req? (and (list? reqs)
                             (pair? reqs)
                             (eq? 'begin (car reqs)))]
              [reqs (or reqs '())]
              [lang (or lang '(begin))])
          (keyword-apply
           make-evaluator
           '(#:allow-read #:requires)
           (list (extract-requires lang reqs)
                 (if beg-req? null reqs))
           (case lang
             [(r5rs beginner beginner-abbr intermediate intermediate-lambda
               advanced)
              (list 'special lang)]
             [else lang])
           (append (if beg-req? (cdr reqs) null) progs)))))]
    [(mod) (with-ns-params (lambda () (make-module-evaluator mod)))]))

(define (make-mz-namespace)
  (let ([ns (mz:make-namespace)])
    ;; Because scheme/sandbox needs scheme/base:
    (namespace-attach-module (namespace-anchor->namespace anchor)
                             'scheme/base ns)
    ns))

(define (with-ns-params thunk)
  (let ([v (sandbox-namespace-specs)])
    (cond [(and (not gui?) (eq? (car v) make-base-namespace))
           (parameterize ([sandbox-namespace-specs
                           (cons make-mz-namespace (cdr v))])
             (thunk))]
          [(and gui? (eq? (car v) (dynamic-require 'mred 'make-gui-namespace)))
           (parameterize
               ([sandbox-namespace-specs
                 ;; Simulate the old make-namespace-with-mred:
                 (cons (lambda ()
                         (let ([ns (make-mz-namespace)]
                               [ns2 ((dynamic-require
                                      'mred 'make-gui-namespace))])
                           (namespace-attach-module ns2 'mred ns)
                           (namespace-attach-module ns2 'scheme/class ns)
                           (parameterize ([current-namespace ns])
                             (namespace-require 'mred)
                             (namespace-require 'scheme/class))
                           ns))
                       (cdr v))])
             (thunk))]
          [else (thunk)])))

(define (literal-identifier=? x y)
  (or (free-identifier=? x y) (eq? (syntax-e x) (syntax-e y))))

(define (extract-requires language requires)
  (define (find-requires forms)
    (let loop ([forms (reverse forms)] [reqs '()])
      (if (null? forms)
        reqs
        (loop (cdr forms)
              (syntax-case* (car forms) (require) literal-identifier=?
                [(require specs ...)
                 (append (syntax->datum #'(specs ...)) reqs)]
                [_else reqs])))))
  (let* ([requires (if (and (pair? requires) (eq? 'begin (car requires)))
                     (find-requires (cdr requires))
                     null)]
         [requires (cond [(string? language) requires]
                         [(not (pair? language)) requires]
                         [(memq (car language) '(lib file planet quote))
                          requires]
                         [(eq? (car language) 'begin)
                          (append (find-requires (cdr language)) requires)]
                         [else (error 'extract-requires
                                      "bad language spec: ~e" language)])])
    requires))
