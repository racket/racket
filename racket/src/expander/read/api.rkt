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

(define/who (read-syntax [src (object-name (current-input-port))] [in (current-input-port)])
  (check who input-port? in)
  (raw:read-syntax src in))

(define/who (read-syntax/recursive [src (object-name (current-input-port))]
                                   [in (current-input-port)]
                                   [start #f]
                                   [readtable (current-readtable)]
                                   [graph? #t])
  (check who input-port? in)
  (check who char? #:or-false start)
  (check who readtable? #:or-false readtable)
  (raw:read-syntax/recursive src in start readtable graph?))

(define/who (read [in (current-input-port)])
  (check who input-port? in)
  (raw:read in))

(define/who (read/recursive [in (current-input-port)]
                            [start #f]
                            [readtable (current-readtable)]
                            [graph? #t])
  (check who input-port? in)
  (check who char? #:or-false start)
  (check who readtable? #:or-false readtable)
  (raw:read/recursive in start readtable graph?))

(define/who (read-language [in (current-input-port)]
                           [fail-thunk read-language-fail-thunk])
  (check who input-port? in)
  (check who (procedure-arity-includes/c 0) fail-thunk)
  (raw:read-language in (if (eq? fail-thunk read-language-fail-thunk)
                            #f
                            fail-thunk)))

;; Not actually called --- just used to recognize a default
(define (read-language-fail-thunk) (error "fail"))
