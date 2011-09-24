#lang racket/base
(require racket/contract/base
         "base.rkt"
         "private/odbc/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [odbc-connect
  (->* (#:dsn (or/c string? #f))
       (#:user (or/c string? #f)
        #:password (or/c string? #f)
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1)
        #:use-place boolean?)
       connection?)]
 [odbc-driver-connect
  (->* (string?)
       (#:notice-handler (or/c 'output 'error output-port? procedure?)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1)
        #:use-place boolean?)
       connection?)]
 [odbc-data-sources
  (-> (listof (list/c string? string?)))]
 [odbc-drivers
  (-> (listof (cons/c string? any/c)))])
