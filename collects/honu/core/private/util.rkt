#lang racket/base

(provide (except-out (all-defined-out) test-delimiter))
(require "debug.rkt"
         racket/match
         syntax/parse
         syntax/parse/experimental/reflect
         racket/set
         (for-syntax racket/base
                     racket/set
                     syntax/parse
                     "debug.rkt"
                     )
         syntax/stx
         racket/list)


(define (delim-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define extract-until
  (case-lambda
    [(r ids keep?)
     (let loop ([r r][val-stxs null])
       (cond
         [(stx-null? r)
          (values #f #f #f)]
         [(and (identifier? (stx-car r))
               (ormap (lambda (id)
                        (delim-identifier=? id (stx-car r)))
                      ids))
          (values (reverse (if keep?
                             (cons (stx-car r) val-stxs) 
                             val-stxs))
                  r
                  (stx-car r))]
         [else
           (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))]
    [(r ids) (extract-until r ids #f)]))

(define-syntax-rule (call-values values-producing function)
  (call-with-values (lambda () values-producing) function))

;; shortcut for treating arguments as syntax objects
(define-syntax (syntax-lambda stx)
  (syntax-case stx ()
    [(_ (arg ...) body ...)
     (with-syntax ([(temp ...) (generate-temporaries #'(arg ...))])
       #'(lambda (temp ...)
           (with-syntax ([arg temp] ...)
             body ...)))]))

;; removes the last element of a list
(define (drop-last lst)
  (take lst (sub1 (length lst))))

(define (test-delimiter)
  (let* ([original #'(a b c d e)]
         [delimiter #'c]
         [expected-before #'(a b)]
         [expected-rest #'(c d e)]
         [expected-delimiter #'c]
         )
    (let-values ([(before rest hit) (extract-until original (list delimiter))])
      ;; is there a better way to test equality between two syntaxes?
      (when (not (and (equal? (syntax->datum expected-before)
                              (map syntax->datum before))
                      (equal? (syntax->datum expected-rest)
                              (map syntax->datum rest))
                      (equal? (syntax->datum expected-delimiter)
                              (syntax->datum hit))))
        (debug "failure: original ~a until ~a\n" (syntax->datum original) (map syntax->datum (list delimiter)))
        (debug " before expected ~a actual ~a\n" (syntax->datum expected-before) (map syntax->datum before))
        (debug " rest expected ~a actual ~a\n" (syntax->datum expected-rest) (map syntax->datum rest))
        (debug " delimiter expected ~a actual ~a\n" (syntax->datum expected-delimiter) (syntax->datum hit))
        ))))

;; better version of caddadadr-type functions
(define-syntax (list-match stx)
  (define (convert-pattern pattern)
    (syntax-case pattern ()
      [(x ...) (with-syntax ([(x* ...) (map convert-pattern (syntax->list #'(x ...)))])
                 #'(list x* ...))]
      [x #'x]))
  (define (extract-variable pattern)
    (define (non-wildcard-identifier? x)
      (and (identifier? x)
           (not (eq? (syntax-e x) '_))))
    (syntax-case pattern ()
      [(a x ...)
       (if (non-wildcard-identifier? #'a)
         #'a
         (or (extract-variable #'a)
             (extract-variable #'(x ...))))]
      [x (if (non-wildcard-identifier? #'x)
           #'x
           #f)]))
  (syntax-case stx ()
    [(_ pattern expression)
     (with-syntax ([match-pattern (convert-pattern #'pattern)]
                   [match-variable (extract-variable #'pattern)])
       #'(match expression
           [match-pattern match-variable]))]))

;; local-binding and module-binding check that the identifier is bound at the given phase
(define-syntax (local-binding stx)
  (syntax-parse stx
    [(_ name)
     (define type (identifier-binding #'name))
     (when (not (eq? type 'lexical))
       (raise-syntax-error 'local-binding "not bound locally" #'name))
     #'#'name]))

(define-syntax (module-binding stx)
  (syntax-parse stx
    [(_ name level)
     (define type (identifier-binding #'name (syntax-e #'level)))
     (when (not (and (list? type)
                     (= (length type) 7)))
       (raise-syntax-error 'module-binding
                           (format "not bound by a module at phase ~a" (syntax-e #'level))
                           #'name))
     #'#'name]))

(define-syntax-rule (literal-syntax-class literal)
  (let ()
    (define-literal-set set (literal))
    (define-syntax-class class
      ;; The problem is that 'literal' is unmarked but 'set' is marked.
      ;; The #:literal-sets option is kind of like a binding form: only identifiers
      ;; having the same marks are treated as literals.
      ;; The fix is
      ;;   #:literal-sets ([set #:at literal])
      ;; which means treat any identifier whose name is listed in 'set' and whose lexical context matches 'literal' as a literal.
      ;; - Ryan
      #:literal-sets ([set #:at literal])
      [pattern literal])
    (reify-syntax-class class)))

(define-syntax (for/union stx)
  (syntax-case stx ()
    [(_ clauses . body)
     #'(for/fold/derived stx ([accum-set (set)])
                         clauses
                         (set-union accum-set (let () . body)))]))
