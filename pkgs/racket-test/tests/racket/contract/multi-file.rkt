#lang racket/base
(require "test-util.rkt"
         racket/file)

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  

  (define exn:fail:contract:blame-object (contract-eval 'exn:fail:contract:blame-object))
  (define exn:fail:contract:blame? (contract-eval 'exn:fail:contract:blame?))
  
  ;; build-and-run : (listof (cons/c string[filename]
  ;;                           (cons/c string[lang-line]
  ;;                              (listof sexp[body-of-module]))) 
  ;;              -> any
  ;; sets up the files named by 'test-case', dynamically requires the first one, deletes the files
  ;; and returns/raises-the-exception from the require'd file
  (define (build-and-run test-case)
    (define dir (make-temporary-file "contract-test~a" 'directory))
    (for ([f (in-list test-case)])
      (call-with-output-file (build-path dir (car f))
        (lambda (port)
          (display (cadr f) port)
          (newline port)
          (for ([sexp (in-list (cddr f))])
            (fprintf port "~s\n" sexp)))))
    (dynamic-wind
     void
     (lambda () (contract-eval `(dynamic-require ,(build-path dir (car (car test-case))) #f)))
     (lambda ()
       (for ([f (in-list test-case)])
         (delete-file (build-path dir (car f))))
       (delete-directory dir))))
  
  (define (get-last-part-of-path sexp)
    (define str (format "orig-blame: ~s" sexp))
    (define m (regexp-match #rx"[/\\]([-a-z0-9.]*)[^/\\]*$" str))
    (if m (cadr m) str))
  
  ;; basic negative blame case
  (let ([blame
         (exn:fail:contract:blame-object
          (with-handlers ((exn:fail:contract:blame? values))
            (build-and-run
             (list (list "a.rkt"
                         "#lang racket/base"
                         '(require "b.rkt")
                         '(f #f))
                   (list "b.rkt"
                         "#lang racket/base"
                         '(require racket/contract)
                         '(provide/contract [f (-> integer? integer?)])
                         '(define (f x) 1))))))])
    (ctest "a.rkt"
           'multi-file-blame1-positive
           (,get-last-part-of-path (blame-positive ,blame)))
    (ctest "b.rkt"
           'multi-file-blame1-negative
           (,get-last-part-of-path (blame-negative ,blame))))
  
  ;; basic positive blame case
  (let ([blame
         (exn:fail:contract:blame-object
          (with-handlers ((exn:fail:contract:blame? values))
            (build-and-run
             (list (list "a.rkt"
                         "#lang racket/base"
                         '(require "b.rkt")
                         '(f 1))
                   (list "b.rkt"
                         "#lang racket/base"
                         '(require racket/contract)
                         '(provide/contract [f (-> integer? integer?)])
                         '(define (f x) #f))))))])
    (ctest "b.rkt"
           'multi-file-blame2-positive
           (,get-last-part-of-path (blame-positive ,blame)))
    (ctest "a.rkt"
           'multi-file-blame2-negative
           (,get-last-part-of-path (blame-negative ,blame))))
  
  ;; positive blame via a re-provide
  (let ([blame
         (exn:fail:contract:blame-object
          (with-handlers ((exn:fail:contract:blame? values))
            (build-and-run
             (list (list "a.rkt"
                         "#lang racket/base"
                         '(require "b.rkt")
                         '(f 1))
                   (list "b.rkt"
                         "#lang racket/base"
                         '(require "c.rkt")
                         '(provide f))
                   (list "c.rkt"
                         "#lang racket/base"
                         '(require racket/contract)
                         '(provide/contract [f (-> integer? integer?)])
                         '(define (f x) #f))))))])
    (ctest "c.rkt"
           'multi-file-blame3-positive
           (,get-last-part-of-path (blame-positive ,blame)))
    (ctest "a.rkt"
           'multi-file-blame3-negative
           (,get-last-part-of-path (blame-negative ,blame))))
  
  ;; negative blame via a re-provide
  (let ([blame
         (exn:fail:contract:blame-object
          (with-handlers ((exn:fail:contract:blame? values))
            (build-and-run
             (list (list "a.rkt"
                         "#lang racket/base"
                         '(require "b.rkt")
                         '(f #f))
                   (list "b.rkt"
                         "#lang racket/base"
                         '(require "c.rkt")
                         '(provide f))
                   (list "c.rkt"
                         "#lang racket/base"
                         '(require racket/contract)
                         '(provide/contract [f (-> integer? integer?)])
                         '(define (f x) 1))))))])
    (ctest "a.rkt"
           'multi-file-blame4-positive
           (,get-last-part-of-path (blame-positive ,blame)))
    (ctest "c.rkt"
           'multi-file-blame4-negative
           (,get-last-part-of-path (blame-negative ,blame))))
  
  ;; have some sharing in the require graph
  (let ([blame
         (exn:fail:contract:blame-object
          (with-handlers ((exn:fail:contract:blame? values))
            (build-and-run
             (list (list "client.rkt"
                         "#lang racket/base"
                         '(require "server.rkt" "other.rkt")
                         '(turn-init #f))
                   (list "server.rkt"
                         "#lang racket/base"
                         '(require racket/contract)
                         '(provide/contract [turn-init (-> number? any)])
                         '(define turn-init void))
                   (list "other.rkt"
                         "#lang racket/base"
                         '(require "server.rkt"))))))])
    (ctest "client.rkt"
           'multi-file-blame5-positive
           (,get-last-part-of-path (blame-positive ,blame)))
    (ctest "server.rkt"
           'multi-file-blame5-negative
           (,get-last-part-of-path (blame-negative ,blame)))))
