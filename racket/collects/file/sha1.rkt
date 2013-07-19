#lang racket/base

#|
Originally:

Copyright (c) 2006 Eric Knauel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the authors may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(provide sha1
         sha1-bytes
         bytes->hex-string)

(define 32-mask #xFFFFFFFF)

(define (mod2+ a b)
  (bitwise-and (+ a b) 32-mask))

(define (mod5+ a b c d e)
  (bitwise-and (+ a b c d e) 32-mask))

;;; make a mask with COUNT 1 bits shifted START bits left
(define (make-extract-mask start count)
  (arithmetic-shift (sub1 (arithmetic-shift 1 count)) start))

;;; rotate I COUNT bits to the left. Assumes length of 32 bits
(define (circular-shift-left i count)
  (bitwise-ior 
   (arithmetic-shift (bitwise-and i (sub1 (arithmetic-shift 1 (- 32 count))))
                     count)
   (arithmetic-shift i (- count 32))))

(define (calc-blocks-needed bits)
  (if (<= bits 448)
      1
      (let* ((full-blocks (+ 1 (quotient bits 512)))
	     (rest (- (* full-blocks 512) bits)))
	(if (< rest 64)
	    (+ 1 full-blocks)
	    full-blocks))))

;;; convert NUM-BYTES from BV (starting at START) into an integer
(define (bytes->block bv start num-bytes)
  (let lp ((i 0) (block 0))
    (if (= i num-bytes)
	block
	(lp (+ i 1) 
	    (bitwise-ior 
	     block
	     (arithmetic-shift (bytes-ref bv (+ start i))
			       (* 8 (- num-bytes (+ i 1)))))))))

;;; enough space for 64 bit length info?
(define (enough-space-for-length-info? last-block-len)
  (<= last-block-len 56))

;;; add padding to BLOCK
(define (pad-block block unused-bits)
  (bitwise-ior
   (arithmetic-shift block unused-bits)
   (arithmetic-shift #b1 (- unused-bits 1))))

(define (prepare-message! blocks unused-bits total-message-len)
  (let* ((len (vector-length blocks))
	 (spare-block-index (- len 1))
	 (last-block-index (- len 2))
	 (last-block (vector-ref blocks last-block-index)))
   (cond
    ((>= unused-bits (+ 64 1))
     ;; there is enough space to store the message length in the last
     ;; block
     (vector-set! blocks
		  last-block-index
		  ;(+ (pad-block last-block 512) total-message-len))
		  (+ (pad-block last-block unused-bits) total-message-len))
     last-block-index)
    ((zero? unused-bits)
     ;; we need the spare block.  There is no space to pad the last
     ;; block.
     (vector-set! blocks
		  spare-block-index
		  ;(+ (pad-block 0 512) total-message-len))
		  (+ (pad-block 0 unused-bits) total-message-len))
     spare-block-index)
    (else
     ;; we need the spare block. First pad the last-block to 512 bits
     (vector-set! blocks
		  last-block-index
		  ;(pad-block last-block 512))
		  (pad-block last-block unused-bits))
     ;; Now write the length into the spare block
     (vector-set! blocks spare-block-index total-message-len)
     spare-block-index))))

;;; generate a vector with masks that decompose a 512-bit block into
;;; 16 32-bit words (stored in a vector)
(define (make-split-block-vector)
  (let ((vec (make-vector 16 0)))
    (do ((i 0 (+ i 32))
	 (j 0 (+ j 1)))
	((>= i 512) vec)
      (vector-set! vec
		   j
		   (make-extract-mask i 32)))))

(define split-block-masks
  (make-split-block-vector))

;;; decompose a 512-bit block into 16 32-bit words (stored in a
;;; vector)
(define (split-block block)
  (let ((vec (make-vector 16 0)))
    (do ((i 0 (+ i 1)))
	((>= i 16) vec)
      (vector-set! 
       vec (- 15 i)
       (arithmetic-shift
	(bitwise-and (vector-ref split-block-masks i)
		     block)
	(- (* i 32)))))))

;;; extend a vector with 16 32-bit words into a vector of 80 32-bit
;;; words
(define (extend-word-blocks word-block)
  (let ((vec (make-vector 80 0)))

    (do ((i 0 (+ i 1)))
	((> i 15) (values))
      (vector-set! vec i (vector-ref word-block i)))

    (do ((i 16 (+ i 1)))
	((> i 79) vec)
      (vector-set! 
       vec i
       (circular-shift-left
	(bitwise-xor (vector-ref vec (- i 3))
		     (bitwise-xor (vector-ref vec (- i 8))
				  (bitwise-xor (vector-ref vec (- i 14))
					       (vector-ref vec (- i 16)))))
	1)))))

;;; the nonlinear functions used by SHA1
(define (nonlinear-sha1-function i x y z)
  (cond
   ((<= i 19)
    (bitwise-xor (bitwise-and x y) 
		 (bitwise-and (bitwise-not x) z)))
   ((<= i 39)
    (bitwise-xor (bitwise-xor x y) z))
   ((<= i 59)
    (bitwise-xor
     (bitwise-xor (bitwise-and x y)
		  (bitwise-and x z))
     (bitwise-and y z)))
   (else
    (bitwise-xor (bitwise-xor x y) z))))

;;; the SHA1 "constants"
(define (sha1-constant i)
  (cond
   ((<= i 19) #x5a827999)
   ((<= i 39) #x6ed9eba1)
   ((<= i 59) #x8f1bbcdc)
   (else #xca62c1d6)))

;;; append five 32 bits to a 160 bit hash number
(define (append-hash h0 h1 h2 h3 h4)
  (bitwise-ior
   (bitwise-ior
    (bitwise-ior
     (bitwise-ior h4 (arithmetic-shift h3 32))
     (arithmetic-shift h2 64))
    (arithmetic-shift h1 96))
   (arithmetic-shift h0 128)))

;;; SHA1 main loop
(define (sha1-loop extended-words h0 h1 h2 h3 h4)
  (let lp ((i 0) (a h0) (b h1) (c h2) (d h3) (e h4))
    (if (= i 80)
	(values a b c d e)
	(lp (+ i 1)
	    (mod5+ (circular-shift-left a 5)
		   (nonlinear-sha1-function i b c d)
		   e
		   (vector-ref extended-words i)
		   (sha1-constant i))
	    a
	    (circular-shift-left b 30)
	    c
	    d))))

(define (calculate-sha1 blocks last-index)
  (let lp ((index 0)
	   (h0 #x67452301) (h1 #xefcdab89) (h2 #x98badcfe)
	   (h3 #x10325476) (h4 #xc3d2e1f0))
    (if (> index last-index)
	(append-hash h0 h1 h2 h3 h4)
	(let* ((block (vector-ref blocks index))
	       (word-blocks (split-block block))
	       (extended-words (extend-word-blocks word-blocks)))
          (let-values ([(a b c d e)
                        (sha1-loop extended-words h0 h1 h2 h3 h4)])
            (let ((h0 (mod2+ h0 a))
                  (h1 (mod2+ h1 b))
                  (h2 (mod2+ h2 c))
                  (h3 (mod2+ h3 d))
                  (h4 (mod2+ h4 e)))
              (lp (+ index 1) h0 h1 h2 h3 h4)))))))

;;; returns a vector of blocks (a block is a 512 bit integer) and the
;;; number of unused bits in the last block.
(define (byte-string->blocks bv)
  (let* ((bytes (bytes-length bv))
	 (vec (make-vector (+ 1 (+ 1 (quotient bytes (quotient 512 8)))) 0))
	 (bits 0))
    ;; the last element is a spare element---just needed if the
    ;; message length doesn't fit into the last message block.
    (do ((i 0 (+ i 64))
	 (j 0 (+ j 1)))
	((> (+ i 64) bytes)
	 (vector-set! vec j (bytes->block bv i (- bytes i)))
	 (values vec 
		 (* 8 (- 64 (- bytes i))) 
		 (+ bits (* 8 (- bytes i)))))
      (set! bits (+ bits 512))
      (vector-set! vec j (bytes->block bv i 64)))))

(define (sha1-hash-string str)
  (sha1-hash-bytes (string->bytes/utf-8 str)))

(define (sha1-hash-bytes bv)
  (let-values ([(blocks unused-bits total-length)
                (byte-string->blocks bv)])
    (let ((last-index (prepare-message! blocks unused-bits total-length)))
      (calculate-sha1 blocks last-index))))

(define (make-hash-as-bytes-mask)
  (let* ((len (quotient 160 8))
	 (vec (make-vector len 0)))
    (do ((i 0 (+ i 8))
	 (j 0 (+ j 1)))
	((>= i 160) vec)
      (vector-set! vec j (make-extract-mask i 8)))))

(define hash-as-bytes-masks
  (make-hash-as-bytes-mask))

(define (hash-value->bytes int)
  (let* ((len (vector-length hash-as-bytes-masks))
         (bv (make-bytes len 0)))
     (do ((i 0 (+ i 1)))
         ((>= i len) bv)
       (bytes-set!
        bv (- (- len 1) i)
	(arithmetic-shift
	 (bitwise-and (vector-ref hash-as-bytes-masks i)
		      int)
	 (- (* i 8)))))))

(define (sha1-input in)
  (let ([p (open-output-bytes)]
        [bstr (make-bytes 4096)])
    (let loop ()
      (let ([c (read-bytes! bstr in)])
        (unless (eof-object? c)
          (write-bytes bstr p 0 c)
          (loop))))
    (sha1-hash-bytes (get-output-bytes p))))

(define (sha1 in)
  (bytes->hex-string (hash-value->bytes (sha1-input in))))

(define (sha1-bytes in)
  (hash-value->bytes (sha1-input in)))

(define (bytes->hex-string bstr)
  (let* ([len (bytes-length bstr)]
         [bstr2 (make-bytes (* len 2))]
         [digit
          (lambda (v)
            (if (v . < . 10)
                (+ v (char->integer #\0))
                (+ v (- (char->integer #\a) 10))))])
    (for ([i (in-range len)])
      (let ([c (bytes-ref bstr i)])
        (bytes-set! bstr2 (* 2 i) (digit (arithmetic-shift c -4)))
        (bytes-set! bstr2 (+ (* 2 i) 1) (digit (bitwise-and c #xF)))))
    (bytes->string/latin-1 bstr2)))
