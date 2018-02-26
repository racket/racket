#lang racket/base
(require "../common/contract.rkt"
         "readtable.rkt"
         (rename-in "../syntax/read-syntax.rkt"
                    [read-syntax raw:read-syntax]
                    [read-syntax/recursive raw:read-syntax/recursive]
                    [read raw:read]
                    [read/recursive raw:read/recursive]
                    [read-language raw:read-language]))

(provide read-syntax
         read-syntax/recursive
         read
         read/recursive
         read-language)

(define (read-syntax [src (object-name (current-input-port))] [in (current-input-port)])
  (check 'read-syntax input-port? in)
  (raw:read-syntax src in))

(define (read-syntax/recursive [src (object-name (current-input-port))]
                               [in (current-input-port)]
                               [start #f]
                               [readtable (current-readtable)]
                               [graph? #t])
  (check 'read-syntax/recursive input-port? in)
  (unless (or (char? start) (not start))
    (raise-argument-error 'read-syntax/recursive "(or/c char? #f)" start))
  (unless (or (readtable? readtable) (not readtable))
    (raise-argument-error 'read-syntax/recursive "(or/c readtable? #f)" readtable))
  (raw:read-syntax/recursive src in start readtable graph?))

(define (read [in (current-input-port)])
  (check 'read input-port? in)
  (raw:read in))

(define (read/recursive [in (current-input-port)]
                        [start #f]
                        [readtable (current-readtable)]
                        [graph? #t])
  (check 'read/recursive input-port? in)
  (unless (or (char? start) (not start))
    (raise-argument-error 'read/recursive "(or/c char? #f)" start))
  (unless (or (readtable? readtable) (not readtable))
    (raise-argument-error 'read/recursive "(or/c readtable? #f)" readtable))
  (raw:read/recursive in start readtable graph?))

(define (read-language [in (current-input-port)]
                       [fail-thunk read-language-fail-thunk])
  (check 'read-language input-port? in)
  (unless (and (procedure? fail-thunk)
               (procedure-arity-includes? fail-thunk 0))
    (raise-argument-error 'read-language "(procedure-arity-includes?/c 0)" fail-thunk))
  (raw:read-language in (if (eq? fail-thunk read-language-fail-thunk)
                            #f
                            fail-thunk)))

;; Not actually called --- just used to recognize a default
(define (read-language-fail-thunk) (error "fail"))
