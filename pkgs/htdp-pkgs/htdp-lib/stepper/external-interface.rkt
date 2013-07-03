#lang racket/base

(require racket/contract
         "private/annotate.rkt"
         "private/marks.rkt")

;; an external interface for the stepper.

;; a handler gets a list of marks, a symbol indicating
;; the kind of break, and an optional list of values 
;; (associated with certain kinds of break).
(define handler-ctct (-> (or/c list? false/c)
                         symbol?
                         (or/c list? false/c)
                         any))

(provide/contract
 [step-program-file (-> path-string? handler-ctct any/c)]
 [step-program-string (-> string? string? handler-ctct any/c)])


(define (step-program-file path handler)
  (define stx (path->stx path))
  (expand-annotate-and-run stx handler))

(define (step-program-string name prog-text handler)
  (define stx (string->stx name prog-text))
  (expand-annotate-and-run stx handler))

(define-namespace-anchor here-anchor)

(define (expand-annotate-and-run stx handler)
  (define expanded
    ;; should this be a blank namespace instead?
    (parameterize ([current-namespace 
                    (namespace-anchor->namespace here-anchor)])
      (expand stx)))
  #;(printf "~s\n" expanded)
  (define module-name
    (syntax-case expanded ()
      [(#%module name . rest) (syntax-e #'name)]))
  #;(printf "~s\n" module-name)
  ;; unwrap the continuation-mark-set->list part:
  (define (wrapped-handler mark-set kind vals)
    (handler (cond [mark-set (extract-mark-list mark-set)]
                   [else #f])
             kind
             vals))
  (eval-syntax (annotate expanded wrapped-handler #t))
  (dynamic-require `(quote ,module-name) #f))

;; given a path, return the read syntax. Expects 
;; a file starting with #lang
(define (path->stx path)
  (parameterize ([port-count-lines-enabled #t]
                 [read-accept-reader #t])
    (call-with-input-file path
      (lambda (port) (read-syntax path port)))))

;; read the program, return an expanded
;; module with the given name.  Expects 
;; a program starting with "#lang"
(define (string->stx prog-name prog-text)
  ;; I'm worried about the bogusness of this path:
  (define fabricated-module-path
    (build-path "/" "tmp" (string-append prog-name ".rkt")))
  (define raw-stx
    (parameterize ([port-count-lines-enabled #t]
                   [read-accept-reader #t])
      (define port (open-input-string prog-text fabricated-module-path))
      (read-exactly-one fabricated-module-path port)))
  raw-stx)

;; read one syntax-object, make sure there are no more.
(define (read-exactly-one path port)
  (define first-stx (read-syntax path port))
  (define second-stx (read-syntax "bogus" port))
  (unless (eof-object? second-stx)
    (error 'read-exactly-one
           "expected only one syntax expression in port, got additional exp: ~s\n" (syntax->datum second-stx)))
  first-stx)


