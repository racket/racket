;; Copyright (c) 2009-2013 Derick Eddington.  All rights reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Inspired by Danny Yoo's get-environment PLaneT package.

#lang scheme/base

(require scheme/foreign)
(provide (rename-out (getenv get-environment-variable))
         get-environment-variables)

(unsafe!)

(define (get-environment-variables)
  (define (split-strings l)
    (for/list ([next (in-list l)])
      (let loop ((i 0) (len (string-length next)))
        (if (< i len)
            (if (char=? #\= (string-ref next i))
                (cons (substring next 0 i)
                      (substring next (+ 1 i) len))
                                     (loop (+ 1 i) len))
            (cons next #f)))))
  (case (system-type)
    [(windows)
     (let ([get (get-ffi-obj "GetEnvironmentStringsW" #f (_fun #:abi 'stdcall -> _bytes))]
           [free (get-ffi-obj "FreeEnvironmentStringsW" #f (_fun #:abi 'stdcall _pointer -> _void))])
       (let* ([strs (get)]
              [len (let loop ([i 0])
                     (if (and (= 0 (ptr-ref strs _byte i))
                              (= 0 (ptr-ref strs _byte (+ i 1)))
                              (= 0 (ptr-ref strs _byte (+ i 2)))
                              (= 0 (ptr-ref strs _byte (+ i 3))))
                         (+ i 4)
                         (loop (+ i 2))))]
              [strs (cast strs _pointer (_bytes o len))])
         (begin0
          (split-strings
           (let loop ([pos 0])
             (let ([m (regexp-match-positions #rx"(?:..)*?\0\0" strs pos)])
               (if m
                   (if (= (cdar m) (+ pos 2))
                       null
                       (cons (cast (ptr-add strs pos) _pointer _string/utf-16)
                             (loop (cdar m))))
                   null))))
          (free strs))))]
    [else
     (let ([environ (get-ffi-obj "environ" (ffi-lib #f) _pointer)])
       (split-strings
        (let loop ([i 0])
          (let ((next (ptr-ref environ _string/locale i)))
            (if next
                (cons next (loop (+ 1 i)))
                null)))))]))
