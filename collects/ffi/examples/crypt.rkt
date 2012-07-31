#lang racket/base

(require ffi/unsafe)

(define libcrypt (ffi-lib "libcrypt"))

(provide crypt)
(define crypt
  (get-ffi-obj "crypt" libcrypt (_fun _string _string -> _bytes)))

(define set-key*
  (get-ffi-obj "setkey" libcrypt (_fun _bytes -> _void)))
(define encrypt*
  (get-ffi-obj "encrypt" libcrypt (_fun _bytes _bool -> _void)))

;; see the encrypt(3) man page for the following

(define (*crypt str key flag)
  (set-key* key)
  (let ([str (string8->keystring str)])
    (encrypt* str flag)
    (keystring->string8 str)))

(provide encrypt decrypt)
(define (encrypt str key) (*crypt (string->bytes/utf-8 str) key #f))
(define (decrypt str key) (bytes->string/utf-8 (*crypt str key #t)))

(define (string8->keystring str)
  (let* ([len (bytes-length str)]
         [str (cond
               [(> len 8) (subbytes str 0 8)]
               [(< len 8) (bytes-append str (make-bytes (- 8 len) 32))]
               [else str])]
         [bin (apply string-append
                     (map (lambda (x)
                            (let* ([bin (format "~b" x)]
                                   [len (string-length bin)])
                              (if (< (string-length bin) 8)
                                (string-append (make-string (- 8 len) #\0) bin)
                                bin)))
                          (bytes->list str)))])
    (list->bytes
     (map (lambda (x)
            (case x
              [(#\0) 0] [(#\1) 1]
              [else (error 'string8->keystring "something bad happened")]))
          (string->list bin)))))

(define (keystring->string8 bin)
  (unless (= 64 (bytes-length bin))
    (error 'keystring->string8 "bad input size: ~s" bin))
  (let ([bin (apply string (map (lambda (x)
                                  (case x
                                    [(0) #\0] [(1) #\1]
                                    [else (error 'keystring->string8
                                                 "something bad happened")]))
                                (bytes->list bin)))])
    (apply bytes
           (let loop ([n (- 64 8)] [r '()])
             (if (< n 0)
               r
               (loop (- n 8) (cons (string->number (substring bin n (+ n 8)) 2)
                                   r)))))))
