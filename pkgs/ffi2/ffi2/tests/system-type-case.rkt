#lang racket/base
(require ffi2
         rackunit)

(check-equal? (ffi2-sizeof (system-type-case
                            os
                            [(macosx) int8_t]
                            [else int32_t]))
              (if (eq? (system-type) 'macosx)
                  1
                  4))

(check-equal? (ffi2-sizeof (system-type-case
                            os*
                            [(windows) int8_t]
                            [else int32_t]))
              (if (eq? (system-type 'os*) 'windows)
                  1
                  4))

(check-equal? (ffi2-sizeof (system-type-case
                            arch
                            [(x86_64 i386) int8_t]
                            [(aarch64) int16_t]
                            [else int32_t]))
              (case (system-type 'arch)
                [(x86_64 i386) 1]
                [(aarch64) 2]
                [else 4]))

(check-equal? (ffi2-sizeof (system-type-case
                            word
                            [(32 64) int8_t]
                            [else int32_t]))
              1)

(define-ffi2-type proc_t (int_t . -> . int_t
                                #:abi (system-type-case
                                       os
                                       [(windows) cdecl_abi]
                                       [else default_abi])))
