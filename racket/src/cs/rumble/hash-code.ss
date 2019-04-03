;;; Parts from "newhash.ss" in Chez Scheme's implementation

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

(define codes (make-weak-eq-hashtable))
(define counter 12345)

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
    (with-global-lock
     (or (eq-hashtable-ref codes x #f)
         (let ([c (fx1+ counter)])
           (set! counter c)
           (eq-hashtable-set! codes x counter)
           c)))]))

;; Mostly copied from Chez Scheme's "newhash.ss":
(define number-hash
  (lambda (z)
    (cond
     [(fixnum? z) (if (fx< z 0) (fxnot z) z)]
     [(flonum? z) (#3%$flhash z)]
     [(bignum? z) (modulo z (most-positive-fixnum))]
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
  (call-with-values (lambda () (equal-hash-loop x 0 0))
    (lambda (hc burn) (logand hc (most-positive-fixnum)))))

(define (equal-secondary-hash-code x)
  (cond
   [(boolean? x) 1]
   [(null? x) 2]
   [(number? x) 3]
   [(char? x) 4]
   [(symbol? x) 5]
   [(string? x) 6]
   [(bytevector? x) 7]
   [(box? x) 8]
   [(pair? x) 9]
   [(vector? x) (vector-length x)]
   [(#%$record? x) (eq-hash-code (record-rtd x))]
   [(impersonator? x) (equal-secondary-hash-code (impersonator-val x))]
   [else 100]))

(define MAX-HASH-BURN 128)

(define (equal-hash-loop x burn hc)
  (let* ([+/fx
          (lambda (hc k)
            (#3%fx+ hc k))]
         [sll/fx
          (lambda (hc i)
            (#3%fxsll hc i))]
         [->fx
          (lambda (v)
            (if (fixnum? v)
                v
                (modulo v (greatest-fixnum))))]
         [mix1
          (lambda (hc)
            (let ([hc2 (+/fx hc (sll/fx hc 10))])
              (fxlogxor hc2 (fxsrl hc2 6))))]
         [mix2
          (lambda (hc)
            (mix1 hc))])
    (cond
     [(fx> burn MAX-HASH-BURN) (values hc burn)]
     [(boolean? x) (values (+/fx hc (if x #x0ace0120 #x0cafe121)) burn)]
     [(null? x) (values (+/fx hc #x0cabd122) burn)]
     [(number? x) (values (+/fx hc (number-hash x)) burn)]
     [(char? x) (values (+/fx hc (char->integer x)) burn)]
     [(symbol? x) (values (+/fx hc (symbol-hash x)) burn)]
     [(string? x) (values (+/fx hc (string-hash x)) burn)]
     [(bytevector? x) (values (+/fx hc (equal-hash x)) burn)]
     [(box? x) (equal-hash-loop (unbox x) (fx+ burn 1) (+/fx hc 1))]
     [(pair? x)
      (let-values ([(hc0 burn) (equal-hash-loop (car x) (fx+ burn 2) 0)])
        (let ([hc (+/fx (mix1 hc) hc0)]
              [r (cdr x)])
          (if (and (pair? r) (list? r))
              ;; If it continues as a list, don't count cdr direction as burn:
              (equal-hash-loop r (fx- burn 2) hc)
              (equal-hash-loop r burn hc))))]
     [(vector? x)
      (let ([len (vector-length x)])
        (cond
         [(fx= len 0) (values (+/fx hc 1) burn)]
         [else
          (let vec-loop ([i 0] [burn burn] [hc hc])
            (cond
             [(fx= i len) (values hc burn)]
             [else
              (let-values ([(hc0 burn) (equal-hash-loop (vector-ref x i) (fx+ burn 2) 0)])
                (vec-loop (fx+ i 1)
                          burn
                          (+/fx (mix2 hc) hc0)))]))]))]
     [(hash? x)
      ;; Treat hash-table hashing specially, so it can be order-insensitive
      (let ([burn (fx* (fxmax burn 1) 2)])
        (let ([hc (+/fx hc (->fx (hash-hash-code
                                  x
                                  (lambda (x)
                                    (let-values ([(hc0 burn0) (equal-hash-loop x burn 0)])
                                      hc0)))))])
          (values hc burn)))]
     [(and (#%$record? x) (#%$record-hash-procedure x))
      => (lambda (rec-hash)
           (let ([burn (fx+ burn 2)])
             (let ([hc (+/fx hc (->fx (rec-hash x (lambda (x)
                                                    (let-values ([(hc0 burn0) (equal-hash-loop x burn 0)])
                                                      (set! burn burn0)
                                                      hc0)))))])
               (values hc burn))))]
     [(impersonator? x)
      ;; If an impersonator wraps a value where `equal?` hashing is
      ;; `eq?` hashing, such as for a procedure, then make sure
      ;; we discard the impersonator wrapper.
      (equal-hash-loop (impersonator-val x) burn hc)]
     [else (values (+/fx hc (eq-hash-code x)) burn)])))

(define (hash-code-combine hc v)
  (bitwise-and (+ (bitwise-arithmetic-shift-left hc 2)
                  v)
               (greatest-fixnum)))

(define (hash-code-combine-unordered hc v)
  (bitwise-and (+ hc v)
               (greatest-fixnum)))
