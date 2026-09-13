#lang racket/base
(require ffi2
         rackunit)

(define c-lib (ffi2-lib (if (eq? 'windows (system-type))
                            ;; use msvcrt, because `sprintf` found when linked
                            ;; via ucrt doesn't seem to be the usual one,
                            ;; but msvcrt's has the expected ABI
                            "msvcrt.dll"
                            #f)))

(define c-sprintf-ptr (ffi2-lib-ref c-lib "sprintf"))
(define c-sprintf (ffi2-procedure c-sprintf-ptr
                                  (bytes_ptr_t string_t . -> . void_t)))
(define c-sprintf/int (ffi2-procedure c-sprintf-ptr
                                      (bytes_ptr_t string_t #:varargs int_t . -> . void_t)))
(define c-sprintf/double/int/string (ffi2-procedure c-sprintf-ptr
                                                    (bytes_ptr_t string_t #:varargs double_t int_t string_t
                                                                 . -> . void_t)))

(define bstr (make-bytes 100 (char->integer #\x)))
(define (fill-out expect)
  (bytes-append expect
                #"\0"
                (make-bytes (- (bytes-length bstr) (bytes-length expect) 1) (char->integer #\x))))

(check-true (void? (c-sprintf bstr "Hello, World!")))
(check-equal? bstr (fill-out #"Hello, World!"))

(check-true (void? (c-sprintf/int bstr "Number of hellos: %d" 2)))
(check-equal? bstr (fill-out #"Number of hellos: 2"))

(check-true (void? (c-sprintf/double/int/string bstr "Message: %.1f%% hellos, %d total, %s"
                                                100.0 3 "all good")))
(check-equal? bstr (fill-out #"Message: 100.0% hellos, 3 total, all good"))
