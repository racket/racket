#lang racket/base
(require "../common/bytes-no-nuls.rkt"
         "../locale/string.rkt"
         "../host/rktio.rkt")

(provide bytes-no-nuls?
         bytes-environment-variable-name?
         normalize-key)

(define (bytes-environment-variable-name? k)
  (and (bytes-no-nuls? k)
       (rktio_is_ok_envvar_name rktio k)))

(define (normalize-key k)
  (if (rktio_are_envvar_names_case_insensitive rktio)
      (bytes->immutable-bytes
       (string->bytes/locale (string-foldcase (bytes->string/locale k))))
      k))
