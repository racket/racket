;;; cptypes-lattice.ss
;;; Copyright 1984-2020 Cisco Systems, Inc.
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

(module cptypes-lattice
 (predicate-implies?
  predicate-disjoint?
  predicate-intersect
  predicate-union)

  ; strange properties of bottom here:
  ; (implies? x bottom): only for x=bottom
  ; (implies? bottom y): always
  ; (disjoint? x bottom): never
  ; (disjoint? bottom y): never
  ; check (implies? x bottom) before (implies? x something)
  (define (predicate-implies? x y)
    (and x
         y
         (or (eq? x y)
             (eq? x 'bottom)
             (cond
               [(Lsrc? y)
                (and (Lsrc? x)
                     (nanopass-case (Lsrc Expr) y
                       [(quote ,d1)
                        (nanopass-case (Lsrc Expr) x
                          [(quote ,d2) (eqv? d1 d2)]
                          [else #f])]
                       [else #f]))]
               [(pred-$record/rtd? y)
                (and (pred-$record/rtd? x)
                     (let ([x-rtd (pred-$record/rtd-rtd x)]
                           [y-rtd (pred-$record/rtd-rtd y)])
                       (cond
                         [(record-type-sealed? y-rtd)
                          (eqv? x-rtd y-rtd)]
                         [else
                          (let loop ([x-rtd x-rtd])
                            (or (eqv? x-rtd y-rtd)
                                (let ([xp-rtd (record-type-parent x-rtd)])
                                  (and xp-rtd (loop xp-rtd)))))])))]
               [(pred-$record/ref? y)
                (and (pred-$record/ref? x)
                     (eq? (pred-$record/ref-ref x)
                          (pred-$record/ref-ref y)))]
               [(case y
                  [(null-or-pair) (or (eq? x 'pair)
                                      (check-constant-is? x null?))]
                  [(fixnum) (check-constant-is? x target-fixnum?)]
                  [(bignum) (check-constant-is? x target-bignum?)]
                  [(exact-integer)
                   (or (eq? x 'fixnum)
                       (eq? x 'bignum)
                       (check-constant-is? x (lambda (x) (and (integer? x)
                                                              (exact? x)))))]
                  [(flonum) (check-constant-is? x flonum?)]
                  [(real) (or (eq? x 'fixnum)
                              (eq? x 'bignum)
                              (eq? x 'exact-integer)
                              (eq? x 'flonum)
                              (check-constant-is? x real?))]
                  [(number) (or (eq? x 'fixnum)
                                (eq? x 'bignum)
                                (eq? x 'exact-integer)
                                (eq? x 'flonum)
                                (eq? x 'real)
                                (check-constant-is? x number?))]
                  [(gensym) (check-constant-is? x gensym?)]
                  [(uninterned-symbol) (check-constant-is? x uninterned-symbol?)]
                  [(interned-symbol) (check-constant-is? x (lambda (x)
                                                             (and (symbol? x)
                                                                  (not (gensym? x))
                                                                  (not (uninterned-symbol? x)))))]
                  [(symbol) (or (eq? x 'gensym)
                                (eq? x 'uninterned-symbol)
                                (eq? x 'interned-symbol)
                                (check-constant-is? x symbol?))]
                  [(char) (check-constant-is? x char?)]
                  [(boolean) (check-constant-is? x boolean?)]
                  [(true) (and (not (check-constant-is? x not))
                               (not (eq? x 'boolean))
                               (not (eq? x 'ptr)))] ; only false-rec, boolean and ptr may be `#f
                  [($record) (or (pred-$record/rtd? x)
                                 (pred-$record/ref? x)
                                 (check-constant-is? x #3%$record?))]
                  [(vector) (check-constant-is? x vector?)] ; i.e. '#()
                  [(string) (check-constant-is? x string?)] ; i.e. ""
                  [(bytevector) (check-constant-is? x bytevector?)] ; i.e. '#vu8()
                  [(fxvector) (check-constant-is? x fxvector?)] ; i.e. '#vfx()
                  [(flvector) (check-constant-is? x flvector?)] ; i.e. '#vfl()
                  [(ptr) #t]
                  [else #f])]
               [else #f]))))

  (define (predicate-disjoint? x y)
    (and x
         y
         ; a pred-$record/ref may be any other kind or record
         (not (and (pred-$record/ref? x)
                   (predicate-implies? y '$record)))
         (not (and (pred-$record/ref? y)
                   (predicate-implies? x '$record)))
         ; boolean and true may be a #t
         (not (and (eq? x 'boolean)
                   (eq? y 'true)))
         (not (and (eq? y 'boolean)
                   (eq? x 'true)))
         ; the other types are included or disjoint
         (not (predicate-implies? x y))
         (not (predicate-implies? y x))))

  (define (predicate-intersect x y)
    (cond
      [(predicate-implies? x y) x]
      [(predicate-implies? y x) y]
      [(predicate-disjoint? y x)
       'bottom]
      [(or (and (eq? x 'boolean) (eq? y 'true))
           (and (eq? y 'boolean) (eq? x 'true)))
       true-rec]
      [else (or x y)])) ; if there is no exact option, at least keep the old value

  (define (predicate-union x y)
    (cond
      [(predicate-implies? y x) x]
      [(predicate-implies? x y) y]
      [(find (lambda (t)
               (and (predicate-implies? x t)
                    (predicate-implies? y t)))
             '(char null-or-pair $record
               gensym uninterned-symbol interned-symbol symbol
               fixnum bignum exact-integer flonum real number
               boolean true ptr))] ; ensure they are order from more restrictive to less restrictive
      [else #f]))

)
