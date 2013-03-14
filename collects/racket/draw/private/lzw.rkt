#lang racket/base

;;; Translated from Skippy for Common Lisp:
;;;
;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;; Id: lzw.lisp,v 1.11 2007/01/03 22:01:10 xach Exp

(provide lzw-decompress)

(define (make-input-bitstream bstr)
  (let ([pos 0]
        [val 0]
        [bits 0]
        [limit (bytes-length bstr)])
    (lambda (n)
      (let loop ()
        (cond
         [(n . <= . bits)
          (begin0
           (bitwise-and val (sub1 (arithmetic-shift 1 n)))
           (set! val (arithmetic-shift val (- n)))
           (set! bits (- bits n)))]
         [(= pos limit)
          (add1 (arithmetic-shift 1 n))]
         [else
          (set! val (bitwise-ior (arithmetic-shift (bytes-ref bstr pos) 
                                                   bits)
                                 val))
          (set! pos (add1 pos))
          (set! bits (+ 8 bits))
          (loop)])))))

(define (read-bits n bstream)
  (bstream n))

(define (lzw-decompress result-bstr code-size bstr)
  (let* ((entries (make-vector 4096 -1))
         (preds (make-vector 4096 -1))
         (clear-code (expt 2 code-size))
         (end-of-input (+ clear-code 1))
         (next-entry-index (+ clear-code 2))
         (compression-size (add1 code-size))
         (compression-threshold (* clear-code 2))
         (pos 0)
         (bitstream (make-input-bitstream bstr)))
    (for ([i (in-range clear-code)])
      (vector-set! entries i i))
    (letrec ([reset-table 
              (lambda ()
                (vector-fill! preds -1)
                (for ([i (in-range clear-code 4096)])
                  (vector-set! entries i -1))
                (set! next-entry-index (+ clear-code 2))
                (set! compression-size (add1 code-size))
                (set! compression-threshold (* clear-code 2)))]
             [root-value 
              (lambda (code)
                (let loop ([code code])
                  (let ([pred (vector-ref preds code)])
                    (if (negative? pred)
                        (vector-ref entries code)
                        (loop pred)))))]
             [increase-compression-size!
              (lambda ()
                (set! compression-size (min 12 (add1 compression-size)))
                (set! compression-threshold (* compression-threshold 2)))]
             [add-entry 
              (lambda (entry pred)
                (when (>= pred next-entry-index)
                  (error "Corrupt data in LZW stream"))
                (vector-set! preds next-entry-index pred)
                (vector-set! entries next-entry-index entry)
                (let ([result next-entry-index])
                  (set! next-entry-index (add1 next-entry-index))
                  (when (>= next-entry-index compression-threshold)
                    (increase-compression-size!))
                  result))]
             [code-depth 
              (lambda (code)
                (let loop ([depth 0][code code])
                  (let ([pred (vector-ref preds code)])
                    (if (negative? pred)
                        depth
                        (loop (add1 depth) pred)))))]
             [output-code-string 
              (lambda (code)
                (let ([j pos])
                  (let ([i (+ pos (code-depth code))])
                    (set! pos (add1 i))
                    (if (>= i (bytes-length result-bstr))
                        (log-warning "Too much input data for image, ignoring extra")
                        (let loop ([code code]
                                   [i i])
                          ;; (printf "set ~a\n" (vector-ref entries code))
                          (bytes-set! result-bstr i (vector-ref entries code))
                          (when (i . > . j)
                            (loop (vector-ref preds code)
                                  (sub1 i))))))))])
      (let loop ([last-code -1])
        (let ([code (read-bits compression-size bitstream)])
          ;; (printf "~s: ~s ~s ~s\n" compression-size code clear-code end-of-input)
          (cond 
           [(= code clear-code)
            (reset-table)
            (loop -1)]
           [(= code end-of-input)
            (void)]
           [(= last-code -1)
            (output-code-string code)
            (loop code)]
           [else
            (let ([entry (vector-ref entries code)])
              (if (negative? entry)
                  (let ([root (root-value last-code)])
                    (output-code-string (add-entry root last-code)))
                  (let ([root (root-value code)])
                    (add-entry root last-code)
                    (output-code-string code))))
            (loop code)]))))))
