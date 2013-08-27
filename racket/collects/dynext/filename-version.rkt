#lang racket/base

;; This module provides the string that should replace xxxxxxx's in
;; file names.  The version number is combined into a single integer,
;; and converted to a string in base 36.

(provide filename-version-part)
(require version/utils)

(define filename-version-part
  (let* ([ver    (version->integer (version))]
         [digits "0123456789abcdefghijklmnopqrstuvwxyz"]
         [radix  (string-length digits)])
    (let loop ([n ver] [r '()])
      (cond [(> n 0) (loop (quotient n radix)
                           (cons (string-ref digits (modulo n radix)) r))]
            [(< (length r) 7) (loop n (cons #\_ r))]
            [else (list->string r)]))))
