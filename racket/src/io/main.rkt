#lang racket/base
(require "sandman/main.rkt"
         "port/main.rkt"
         "path/main.rkt"
         "string/main.rkt"
         "converter/main.rkt"
         "locale/main.rkt"
         "format/main.rkt"
         "print/main.rkt"
         "error/main.rkt"
         "srcloc/main.rkt"
         "logger/main.rkt"
         "file/main.rkt"
         "filesystem-change-evt/main.rkt"
         "security/main.rkt"
         "envvar/main.rkt"
         "sha/main.rkt"
         "subprocess/main.rkt"
         "host/processor-count.rkt"
         "network/main.rkt"
         "foreign/main.rkt"
         "time/main.rkt"
         "unsafe/main.rkt"
         "machine/main.rkt"
         "run/main.rkt"
         "port/parameter.rkt"
         "path/system.rkt"
         (only-in "host/rktio.rkt"
                  rktio-place-init!
                  rktio-place-destroy!)
         (submod "error/main.rkt"
                 place-init)
         (only-in "sandman/ltps.rkt"
                  shared-ltps-place-init!)
         (only-in "locale/cache.rkt"
                  convert-cache-init!)
         (only-in "network/address.rkt"
                  address-init!)
         (submod "subprocess/main.rkt" init)
         (only-in "locale/parameter.rkt"
                  sync-locale!)
         "port/place.rkt")

(provide (all-from-out "port/main.rkt")
         (all-from-out "path/main.rkt")
         (all-from-out "string/main.rkt")
         (all-from-out "converter/main.rkt")
         (all-from-out "locale/main.rkt")
         (all-from-out "format/main.rkt")
         (all-from-out "print/main.rkt")
         (all-from-out "error/main.rkt")
         (all-from-out "srcloc/main.rkt")
         (all-from-out "logger/main.rkt")
         (all-from-out "file/main.rkt")
         (all-from-out "filesystem-change-evt/main.rkt")
         (all-from-out "security/main.rkt")
         (all-from-out "envvar/main.rkt")
         (all-from-out "sha/main.rkt")
         (all-from-out "subprocess/main.rkt")
         (all-from-out "host/processor-count.rkt")
         (all-from-out "network/main.rkt")
         (all-from-out "foreign/main.rkt")
         (all-from-out "time/main.rkt")
         (all-from-out "unsafe/main.rkt")
         (all-from-out "machine/main.rkt")
         (all-from-out "run/main.rkt")
         make-place-ports+fds
         io-place-init!
         io-place-destroy!
         get-original-error-port)

(define (io-place-init! in-fd out-fd err-fd cust plumber)
  (rktio-place-init!)
  (convert-cache-init!)
  (logger-init!)
  (shared-ltps-place-init!)
  (install-error-value->string-handler!)
  (init-current-directory!)
  (init-current-ports! in-fd out-fd err-fd cust plumber)
  (subprocess-init!)
  (address-init!)
  (sync-locale!))

(define (io-place-destroy!)
  ;; We expect everything based on rktio to be destroyed at this point
  ;; via custodian shutdown
  (rktio-place-destroy!))
