;;; Parts from "newhash.ss" in Chez Scheme's imp<lementation

;;; newhash.ss
;;; Copyright 1984-2016 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;; Use only `eq-hashtable-try-atomic-cell` on this table,
;; except in `update-eq-hash-code-table-size!`:
(define codes (make-weak-eq-hashtable))

(define counter 12345)

;; Called by the collect handler when no other threads are running.
;; Calling `eq-hashtable-set!` and `eq-hashtable-delete!` here gives
;; the table a chance to resize either larger or smaller, since we
;; otherwise use only `eq-hashtable-try-atomic-cell` on the table.
(define (update-eq-hash-code-table-size!)
  (let ([p (cons #f #f)])
    (eq-hashtable-set! codes p 0)
    (eq-hashtable-delete! codes p)))

(define (eq-hash-code x)
  (cond
   [(and (symbol? x)
         ;; Avoid forcing the universal name of a gensym,
         ;; which is more expensive than registering in
         ;; the `codes` table.
         (not (gensym? x)))
    (symbol-hash x)]
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else
    (let ([p (eq-hashtable-try-atomic-cell codes x counter)])
      (cond
       [p
        (let ([n (cdr p)])
          (when (fx= counter n)
            (set! counter (fx1+ n)))
          n)]
       [else
        ;; There was contention, so try again
        (eq-hash-code x)]))]))

(define (mix-hash-code hc)
  (let ([hc2 (fx+/wraparound hc (fxsll/wraparound (fx+/wraparound hc 1) 10))])
    (fxlogxor hc2 (fxsrl hc2 6))))

(define (->fx v)
  (if (fixnum? v)
      v
      (bitwise-and v (greatest-fixnum))))

(define (->fx/checked who v)
  (cond
    [(fixnum? v) v]
    [(bignum? v) (bitwise-and v (greatest-fixnum))]
    [else (raise-arguments-error who
                                 "hash procedure returned a value other than an exact integer"
                                 "result" v)]))

;; Mostly copied from Chez Scheme's "newhash.ss":
(define number-hash
  (lambda (z)
    (cond
      [(fixnum? z) (if (fx< z 0) (fxand z (most-positive-fixnum)) z)]
      [(flonum? z) (#3%$flhash z)]
      [(bignum? z) (let ([len (integer-length z)])
                     (let loop ([i 0] [hc 0])
                       (cond
                         [(fx>= i len) hc]
                         [else
                          (let ([next-i (fx+ i (fx- (fixnum-width) 1))])
                            (loop next-i
                                  (fx+/wraparound (bitwise-bit-field z i next-i)
                                                  (mix-hash-code hc))))])))]
      [(ratnum? z) (number-hash (+ (* (numerator z) 5) (denominator z)))]
      [else (logand (logxor (lognot (number-hash (real-part z))) (number-hash (imag-part z)))
                    (most-positive-fixnum))])))

(define (eqv-hash-code x)
  (cond
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else (eq-hash-code x)]))

;; We don't use `equal-hash` because we need impersonators to be able
;; to generate the same hash code as the unwrapped value.
(define (equal-hash-code x)
  (let-values ([(hc burn) (equal-hash-loop x 0 0)])
    hc))

;; A #t result implies that `equal-hash-code` and equality checking is
;; pretty fast
(define (fast-equal-hash-code? x)
  (or (boolean? x)
      (null? x)
      (number? x)
      (char? x)
      (symbol? x)
      (and (#%$record? x)
           (not (#%$record-hash-procedure x)))))

(define (equal-secondary-hash-code x)
  (let-values ([(hc burn) (equal-secondary-hash-loop x 0 0)])
    hc))

(define MAX-HASH-BURN 128)

(define (equal-hash-loop x burn hc)
  (cond
    [(fx> burn MAX-HASH-BURN) (values hc burn)]
    [(boolean? x) (values (fx+/wraparound hc (if x #x0ace0120 #x0cafe121)) burn)]
    [(null? x) (values (fx+/wraparound hc #x0cabd122) burn)]
    [(number? x) (values (fx+/wraparound hc (number-hash x)) burn)]
    [(char? x) (values (fx+/wraparound hc (char->integer x)) burn)]
    [(symbol? x) (values (fx+/wraparound hc (symbol-hash x)) burn)]
    [(string? x) (values (fx+/wraparound hc (string-hash x)) burn)]
    [(bytevector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(fxvector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(flvector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(box? x) (equal-hash-loop (unbox x) (fx+ burn 1) (fx+/wraparound hc 1))]
    [(pair? x)
     (let-values ([(hc0 burn) (equal-hash-loop (car x) (fx+ burn 2) 0)])
       (let ([hc (fx+/wraparound (mix-hash-code hc) hc0)]
             [r (cdr x)])
         (if (and (pair? r) (list? r))
             ;; If it continues as a list, don't count cdr direction as burn:
             (equal-hash-loop r (fx- burn 2) hc)
             (equal-hash-loop r burn hc))))]
    [(vector? x)
     (let ([len (vector-length x)])
       (cond
         [(fx= len 0) (values (fx+/wraparound hc 1) burn)]
         [else
          (let vec-loop ([i 0] [burn burn] [hc hc])
            (cond
              [(fx= i len) (values hc burn)]
              [else
               (let-values ([(hc0 burn) (equal-hash-loop (vector-ref x i) (fx+ burn 2) 0)])
                 (vec-loop (fx+ i 1)
                           burn
                           (fx+/wraparound (mix-hash-code hc) hc0)))]))]))]
    [(hash? x)
     ;; Treat hash-table hashing specially, so it can be order-insensitive
     (let ([burn (fx* (fxmax burn 1) 2)])
       (let ([hc (fx+/wraparound hc (->fx (hash-hash-code
                                           x
                                           (lambda (x)
                                             (let-values ([(hc0 burn0) (equal-hash-loop x burn 0)])
                                               hc0)))))])
         (values hc burn)))]
    [(mpair? x)
     (let-values ([(hc0 burn) (equal-hash-loop (mcar x) (fx+ burn 2) 0)])
       (let ([hc (fx+/wraparound (mix-hash-code hc) (fx+/wraparound hc0 5))])
         (equal-hash-loop (mcdr x) burn hc)))]
    [(and (#%$record? x) (#%$record-hash-procedure x))
     => (lambda (rec-hash)
          (let ([burn (fx+ burn 2)])
            (let ([hc (fx+/wraparound hc (->fx/checked
                                          'equal-hash-code
                                          (rec-hash x (lambda (x)
                                                        (let-values ([(hc0 burn0) (equal-hash-loop x burn 0)])
                                                          (set! burn burn0)
                                                          hc0)))))])
              (values hc burn))))]
    [(impersonator? x)
     ;; If an impersonator wraps a value where `equal?` hashing is
     ;; `eq?` hashing, such as for a procedure, then make sure
     ;; we discard the impersonator wrapper.
     (equal-hash-loop (impersonator-val x) burn hc)]
    [else (values (fx+/wraparound hc (eq-hash-code x)) burn)]))

(define (equal-secondary-hash-loop x burn hc)
  (cond
    [(fx> burn MAX-HASH-BURN) (values hc burn)]
    [(boolean? x) (values (fx+/wraparound hc 1) burn)]
    [(null? x) (values (fx+/wraparound hc 2) burn)]
    [(number? x) (values (fx+/wraparound hc (number-secondary-hash x)) burn)]
    [(char? x) (values (fx+/wraparound hc (fxnot (char->integer x))) burn)]
    [(symbol? x) (values (fx+/wraparound hc (fxnot (symbol-hash x))) burn)]
    [(string? x) (values (fx+/wraparound hc (fxnot (string-hash x))) burn)]
    [(bytevector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(fxvector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(flvector? x) (values (fx+/wraparound hc (equal-hash x)) burn)]
    [(box? x) (equal-secondary-hash-loop (unbox x) (fx+ burn 1) (fx+/wraparound hc 10))]
    [(pair? x)
     (let-values ([(hc0 burn) (equal-secondary-hash-loop (car x) (fx+ burn 2) 0)])
       (let ([hc (fx+/wraparound (mix-hash-code hc) hc0)])
         (equal-secondary-hash-loop (cdr x) burn hc)))]
    [(vector? x)
     (let ([len (vector-length x)])
       (let vec-loop ([i 0] [burn burn] [hc (mix-hash-code hc)])
         (cond
           [(fx= i len) (values hc burn)]
           [else
            (let-values ([(hc0 burn) (equal-secondary-hash-loop (vector-ref x i) (fx+ burn 2) 0)])
              (vec-loop (fx+ i 1)
                        burn
                        (fx+/wraparound (mix-hash-code hc) hc0)))])))]
    [(hash? x)
     ;; Treat hash-table hashing specially, so it can be order-insensitive
     (let ([burn (fx* (fxmax burn 1) 2)])
       (let ([hc (fx+/wraparound hc (->fx (hash-hash-code
                                           x
                                           (lambda (x)
                                             (let-values ([(hc0 burn0) (equal-secondary-hash-loop x burn 0)])
                                               hc0)))))])
         (values hc burn)))]
    [(mpair? x)
     (let-values ([(hc0 burn) (equal-secondary-hash-loop (mcar x) (fx+ burn 2) 0)])
       (let ([hc (fx+/wraparound (mix-hash-code hc) hc0)])
         (equal-secondary-hash-loop (mcdr x) burn hc)))]
    [(and (#%$record? x)
          (or (struct-property-ref 'secondary-hash (#%$record-type-descriptor x) #f)
              ;; to use default hash proc as default secondary hash proc:
              (#%$record-hash-procedure x)))
     => (lambda (rec-hash)
          (let ([burn (fx+ burn 2)])
            (let ([hc (fx+/wraparound hc (->fx/checked
                                          'equal-secondary-hash-code
                                          (rec-hash x (lambda (x)
                                                        (let-values ([(hc0 burn0) (equal-secondary-hash-loop x burn 0)])
                                                          (set! burn burn0)
                                                          hc0)))))])
              (values hc burn))))]
    [(impersonator? x)
     (equal-secondary-hash-loop (impersonator-val x) burn hc)]
    [else (values (fx+/wraparound hc (fxnot (eq-hash-code x))) burn)]))

(define number-secondary-hash
  (lambda (z)
    (cond
      [(fixnum? z) (fxnot z)]
      [(flonum? z) (fxsll (#3%$flhash z) 2)]
      [(bignum? z) (fx+/wraparound (integer-length z)
                                   (bitwise-bit-field z 0 (fx- (fixnum-width) 1)))]
      [(ratnum? z) (hash-code-combine (number-secondary-hash (numerator z))
                                      (number-secondary-hash (denominator z)))]
      [else (hash-code-combine (number-secondary-hash (real-part z))
                               (number-secondary-hash (imag-part z)))])))

(define (hash-code-combine hc v)
  (fx+/wraparound (mix-hash-code (->fx hc))
                  (->fx v)))

;; Required to be associative and commutative:
(define (hash-code-combine-unordered a b)
  (fx+/wraparound (->fx a)
                  (->fx b)))
