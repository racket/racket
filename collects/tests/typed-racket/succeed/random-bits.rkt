; MODULE DEFINITION FOR SRFI-27
; =============================
;
; Sebastian.Egner@philips.com, Mar-2002, in PLT 204
;
; This file contains the top-level definition for the 54-bit integer-only
; implementation of SRFI 27 for the PLT 204 DrScheme system.
;
; 1. The core generator is implemented in 'mrg32k3a-a.scm'.
; 2. The generic parts of the interface are in 'mrg32k3a.scm'.
; 3. The non-generic parts (record type, time, error) are here.
;
; load the module with
;   (require srfi/random-bits)
;
; history of this file:
;   SE, 17-May-2003: initial version

(module random-bits typed-scheme
  #;(require srfi/9)
  #;(require srfi/23)

  (provide
   random-integer random-real default-random-source
   make-random-source random-source? random-source-state-ref
   random-source-state-set! random-source-randomize!
   random-source-pseudo-randomize!
   random-source-make-integers random-source-make-reals)

  (define-type-alias Nb Integer)
  (define-type-alias Flt Number)
  (define-type-alias Nbs (Listof Nb))
  (define-type-alias State (Vectorof Integer))
  (define-type-alias SpList (cons 'lecuyer-mrg32k3a (Listof Nb)))
  (define-typed-struct :random-source (
                                       [state-ref : ( -> SpList)]
                                       [state-set! : ((Listof Nb)-> Void)]
                                       [randomize! : ( -> Void)]
                                       [pseudo-randomize! : (Integer Integer -> Void)]
                                       [make-integers : (-> (Integer -> Integer)) ]
                                       [make-reals : ( Nb * -> ( -> Number))]))
  (define-type-alias Random :random-source)
  (define: (:random-source-make
            [state-ref : ( -> SpList)]
            [state-set! : ((Listof Nb)-> Void)]
            [randomize! : ( -> Void)]
            [pseudo-randomize! : (Integer Integer -> Void)]
            [make-integers : (-> (Integer -> Integer)) ]
            [make-reals : (Nb * -> (-> Number))])
            : Random
    (make-:random-source state-ref state-set! randomize! pseudo-randomize! make-integers make-reals ))

  #;(define-record-type :random-source
    (:random-source-make
     state-ref
     state-set!
     randomize!
     pseudo-randomize!
     make-integers
     make-reals)
     :random-source?
    (state-ref :random-source-state-ref)
    (state-set! :random-source-state-set!)
    (randomize! :random-source-randomize!)
    (pseudo-randomize! :random-source-pseudo-randomize!)
    (make-integers :random-source-make-integers)
    (make-reals :random-source-make-reals))

  (define: :random-source-current-time : ( -> Nb )
   current-milliseconds) ;;on verra apres

; implementation begins here

; 54-BIT INTEGER IMPLEMENTATION OF THE "MRG32K3A"-GENERATOR
; =========================================================
;
; Sebastian.Egner@philips.com, Mar-2002.
;
; This file is an implementation of Pierre L'Ecuyer's MRG32k3a
; pseudo random number generator. Please refer to 'mrg32k3a.scm'
; for more information.
;
; compliance:
;   Scheme R5RS with integers covering at least {-2^53..2^53-1}.
;
; history of this file:
;   SE, 18-Mar-2002: initial version
;   SE, 22-Mar-2002: comments adjusted, range added
;   SE, 25-Mar-2002: pack/unpack just return their argument

; the actual generator


(define: (mrg32k3a-random-m1 [state : State]) : Nb
  (let ((x11 (vector-ref state 0))
	(x12 (vector-ref state 1))
	(x13 (vector-ref state 2))
	(x21 (vector-ref state 3))
	(x22 (vector-ref state 4))
	(x23 (vector-ref state 5)))
    (let ((x10 (modulo (- (* 1403580 x12) (* 810728 x13)) 4294967087))
	  (x20 (modulo (- (* 527612 x21) (* 1370589 x23)) 4294944443)))
      (vector-set! state 0 x10)
      (vector-set! state 1 x11)
      (vector-set! state 2 x12)
      (vector-set! state 3 x20)
      (vector-set! state 4 x21)
      (vector-set! state 5 x22)
      (modulo (- x10 x20) 4294967087))))

; interface to the generic parts of the generator

(define: (mrg32k3a-pack-state [unpacked-state : State]) : State
  unpacked-state)

(define: (mrg32k3a-unpack-state [state : State] ) : State
  state)

(define: (mrg32k3a-random-range) : Integer ; m1
  4294967087)

(define: (mrg32k3a-random-integer [state : State] [range : Nb]) : Nb ; rejection method
  (let* ((q (quotient 4294967087 range))
	 (qn (* q range)))
    (do:  : Nb ((x : Nb (mrg32k3a-random-m1 state) (mrg32k3a-random-m1 state))) ;;no alias accepted
	((< x qn) (quotient x q)))))

(define: (mrg32k3a-random-real [state : State]) : Number ; normalization is 1/(m1+1)
  (* 0.0000000002328306549295728 (+ 1.0 (mrg32k3a-random-m1 state))))


; GENERIC PART OF MRG32k3a-GENERATOR FOR SRFI-27
; ==============================================
;
; Sebastian.Egner@philips.com, 2002.
;
; This is the generic R5RS-part of the implementation of the MRG32k3a
; generator to be used in SRFI-27. It is based on a separate implementation
; of the core generator (presumably in native code) and on code to
; provide essential functionality not available in R5RS (see below).
;
; compliance:
;   Scheme R5RS with integer covering at least {-2^53..2^53-1}.
;   In addition,
;     SRFI-23: error
;
; history of this file:
;   SE, 22-Mar-2002: refactored from earlier versions
;   SE, 25-Mar-2002: pack/unpack need not allocate
;   SE, 27-Mar-2002: changed interface to core generator
;   SE, 10-Apr-2002: updated spec of mrg32k3a-random-integer

; Generator
; =========
;
; Pierre L'Ecuyer's MRG32k3a generator is a Combined Multiple Recursive
; Generator. It produces the sequence {(x[1,n] - x[2,n]) mod m1 : n}
; defined by the two recursive generators
;
;   x[1,n] = (               a12 x[1,n-2] + a13 x[1,n-3]) mod m1,
;   x[2,n] = (a21 x[2,n-1] +                a23 x[2,n-3]) mod m2,
;
; where the constants are
;   m1       = 4294967087 = 2^32 - 209    modulus of 1st component
;   m2       = 4294944443 = 2^32 - 22853  modulus of 2nd component
;   a12      =  1403580                   recursion coefficients
;   a13      =  -810728
;   a21      =   527612
;   a23      = -1370589
;
; The generator passes all tests of G. Marsaglia's Diehard testsuite.
; Its period is (m1^3 - 1)(m2^3 - 1)/2 which is nearly 2^191.
; L'Ecuyer reports: "This generator is well-behaved in all dimensions
; up to at least 45: ..." [with respect to the spectral test, SE].
;
; The period is maximal for all values of the seed as long as the
; state of both recursive generators is not entirely zero.
;
; As the successor state is a linear combination of previous
; states, it is possible to advance the generator by more than one
; iteration by applying a linear transformation. The following
; publication provides detailed information on how to do that:
;
;    [1] P. L'Ecuyer, R. Simard, E. J. Chen, W. D. Kelton:
;        An Object-Oriented Random-Number Package With Many Long
;        Streams and Substreams. 2001.
;        To appear in Operations Research.
;
; Arithmetics
; ===========
;
; The MRG32k3a generator produces values in {0..2^32-209-1}. All
; subexpressions of the actual generator fit into {-2^53..2^53-1}.
; The code below assumes that Scheme's "integer" covers this range.
; In addition, it is assumed that floating point literals can be
; read and there is some arithmetics with inexact numbers.
;
; However, for advancing the state of the generator by more than
; one step at a time, the full range {0..2^32-209-1} is needed.


; Required: Backbone Generator
; ============================
;
; At this point in the code, the following procedures are assumed
; to be defined to execute the core generator:
;
;   (mrg32k3a-pack-state unpacked-state) -> packed-state
;   (mrg32k3a-unpack-state packed-state) -> unpacked-state
;      pack/unpack a state of the generator. The core generator works
;      on packed states, passed as an explicit argument, only. This
;      allows native code implementations to store their state in a
;      suitable form. Unpacked states are #(x10 x11 x12 x20 x21 x22)
;      with integer x_ij. Pack/unpack need not allocate new objects
;      in case packed and unpacked states are identical.
;
;   (mrg32k3a-random-range) -> m-max
;   (mrg32k3a-random-integer packed-state range) -> x in {0..range-1}
;      advance the state of the generator and return the next random
;      range-limited integer.
;        Note that the state is not necessarily advanced by just one
;      step because we use the rejection method to avoid any problems
;      with distribution anomalies.
;        The range argument must be an exact integer in {1..m-max}.
;      It can be assumed that range is a fixnum if the Scheme system
;      has such a number representation.
;
;   (mrg32k3a-random-real packed-state) -> x in (0,1)
;      advance the state of the generator and return the next random
;      real number between zero and one (both excluded). The type of
;      the result should be a flonum if possible.

; Required: Record Data Type
; ==========================
;
; At this point in the code, the following procedures are assumed
; to be defined to create and access a new record data type:
;
;   (:random-source-make a0 a1 a2 a3 a4 a5) -> s
;     constructs a new random source object s consisting of the
;     objects a0 .. a5 in this order.
;
;   (:random-source? obj) -> bool
;     tests if a Scheme object is a :random-source.
;
;   (:random-source-state-ref         s) -> a0
;   (:random-source-state-set!        s) -> a1
;   (:random-source-randomize!        s) -> a2
;   (:random-source-pseudo-randomize! s) -> a3
;   (:random-source-make-integers     s) -> a4
;   (:random-source-make-reals        s) -> a5
;     retrieve the values in the fields of the object s.

; Required: Current Time as an Integer
; ====================================
;
; At this point in the code, the following procedure is assumed
; to be defined to obtain a value that is likely to be different
; for each invokation of the Scheme system:
;
;   (:random-source-current-time) -> x
;     an integer that depends on the system clock. It is desired
;     that the integer changes as fast as possible.


; Accessing the State
; ===================

(define: (mrg32k3a-state-ref [packed-state : State ]) : (cons 'lecuyer-mrg32k3a (Listof Nb))
  (cons 'lecuyer-mrg32k3a
        (vector->list (mrg32k3a-unpack-state packed-state))))

(define: (mrg32k3a-state-set [external-state : (Listof Nb)]) : State

  (define: (check-value [x : Nb] [m : Nb]) : Boolean
    (if (and (integer? x)
             (exact? x)
             (<= 0 x (- m 1)))
        #t
        (error "illegal value" x)))

  (if (and (list? external-state)
           (= (length external-state) 7)
           (eq? (car external-state) 'lecuyer-mrg32k3a))
      (let: ((s : (Listof Nb) (cdr external-state)))
        (check-value (list-ref s 0) mrg32k3a-m1)
        (check-value (list-ref s 1) mrg32k3a-m1)
        (check-value (list-ref s 2) mrg32k3a-m1)
        (check-value (list-ref s 3) mrg32k3a-m2)
        (check-value (list-ref s 4) mrg32k3a-m2)
        (check-value (list-ref s 5) mrg32k3a-m2)
        (when (or (zero? (+ (list-ref s 0) (list-ref s 1) (list-ref s 2)))
                  (zero? (+ (list-ref s 3) (list-ref s 4) (list-ref s 5))))
          (error "illegal degenerate state" external-state))
        (mrg32k3a-pack-state (list->vector s)))
      (error "malformed state" external-state)))


; Pseudo-Randomization
; ====================
;
; Reference [1] above shows how to obtain many long streams and
; substream from the backbone generator.
;
; The idea is that the generator is a linear operation on the state.
; Hence, we can express this operation as a 3x3-matrix acting on the
; three most recent states. Raising the matrix to the k-th power, we
; obtain the operation to advance the state by k steps at once. The
; virtual streams and substreams are now simply parts of the entire
; periodic sequence (which has period around 2^191).
;
; For the implementation it is necessary to compute with matrices in
; the ring (Z/(m1*m1)*Z)^(3x3). By the Chinese-Remainder Theorem, this
; is isomorphic to ((Z/m1*Z) x (Z/m2*Z))^(3x3). We represent such a pair
; of matrices
;   [ [[x00 x01 x02],
;      [x10 x11 x12],
;      [x20 x21 x22]], mod m1
;     [[y00 y01 y02],
;      [y10 y11 y12],
;      [y20 y21 y22]]  mod m2]
; as a vector of length 18 of the integers as written above:
;   #(x00 x01 x02 x10 x11 x12 x20 x21 x22
;     y00 y01 y02 y10 y11 y12 y20 y21 y22)
;
; As the implementation should only use the range {-2^53..2^53-1}, the
; fundamental operation (x*y) mod m, where x, y, m are nearly 2^32,
; is computed by breaking up x and y as x = x1*w + x0 and y = y1*w + y0
; where w = 2^16. In this case, all operations fit the range because
; w^2 mod m is a small number. If proper multiprecision integers are
; available this is not necessary, but pseudo-randomize! is an expected
; to be called only occasionally so we do not provide this implementation.

(define: mrg32k3a-m1 : Nb 4294967087) ; modulus of component 1
(define: mrg32k3a-m2 : Nb 4294944443) ; modulus of component 2

(define: mrg32k3a-initial-state : (Vectorof Nb); 0 3 6 9 12 15 of A^16, see below
  '#( 1062452522
      2961816100
       342112271
      2854655037
      3321940838
      3542344109))

(define: mrg32k3a-generators : (Listof State) '(#(0 0 0 0 0)) ) ; computed when needed -> Changer #f by a State to hava right type.
(define: (mrg32k3a-pseudo-randomize-state [i : Integer] [j : Integer]) : State

  (define: (product [A : State] [B : State]) : State ; A*B in ((Z/m1*Z) x (Z/m2*Z))^(3x3)

    (define: w     : Nb 65536) ; wordsize to split {0..2^32-1}
    (define: w-sqr1 : Nb 209)   ; w^2 mod m1
    (define: w-sqr2 : Nb 22853) ; w^2 mod m2

    (define: (lc [i0 : Natural] [i1 : Natural] [i2 : Natural] [j0 : Natural] [j1 : Natural] [j2 : Natural] [m : Nb] [w-sqr : Nb ]): Nb ; linear combination
      (let ((a0h (quotient (vector-ref A i0) w))
            (a0l (modulo   (vector-ref A i0) w))
            (a1h (quotient (vector-ref A i1) w))
            (a1l (modulo   (vector-ref A i1) w))
            (a2h (quotient (vector-ref A i2) w))
            (a2l (modulo   (vector-ref A i2) w))
            (b0h (quotient (vector-ref B j0) w))
            (b0l (modulo   (vector-ref B j0) w))
            (b1h (quotient (vector-ref B j1) w))
            (b1l (modulo   (vector-ref B j1) w))
            (b2h (quotient (vector-ref B j2) w))
            (b2l (modulo   (vector-ref B j2) w)))
        (modulo
         (+ (* (+ (* a0h b0h)
                  (* a1h b1h)
                  (* a2h b2h))
               w-sqr)
            (* (+ (* a0h b0l)
                  (* a0l b0h)
                  (* a1h b1l)
                  (* a1l b1h)
                  (* a2h b2l)
                  (* a2l b2h))
               w)
            (* a0l b0l)
            (* a1l b1l)
            (* a2l b2l))
         m)))

    (vector
     (lc  0  1  2   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_00 mod m1
     (lc  0  1  2   1  4  7  mrg32k3a-m1 w-sqr1) ; (A*B)_01
     (lc  0  1  2   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  3  4  5   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_10
     (lc  3  4  5   1  4  7  mrg32k3a-m1 w-sqr1)
     (lc  3  4  5   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   0  3  6  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   1  4  7  mrg32k3a-m1 w-sqr1)
     (lc  6  7  8   2  5  8  mrg32k3a-m1 w-sqr1)
     (lc  9 10 11   9 12 15  mrg32k3a-m2 w-sqr2) ; (A*B)_00 mod m2
     (lc  9 10 11  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc  9 10 11  11 14 17  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14   9 12 15  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc 12 13 14  11 14 17  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17   9 12 15  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17  10 13 16  mrg32k3a-m2 w-sqr2)
     (lc 15 16 17  11 14 17  mrg32k3a-m2 w-sqr2)))

  (define: (power [A : State ] [e : Nb]) : State ; A^e
    (cond
     ((zero? e)
      '#(1 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 1))
     ((= e 1)
      A)
     ((even? e)
      (power (product A A) (quotient e 2)))
     (else
      (product (power A (- e 1)) A))))

  (define: (power-power [A : State] [b : Nb]) : State ; A^(2^b)
    (if (zero? b)
        A
        (power-power (product A A) (- b 1))))

  (define: A  : State                      ; the MRG32k3a recursion
    '#(     0 1403580 4294156359
            1       0          0
            0       1          0
       527612       0 4293573854
            1       0          0
            0       1          0))

  ; check arguments
  (when (not (and (integer? i)
                  (exact? i)
                  (integer? j)
                  (exact? j)))
    (error "i j must be exact integer" i j))

  ; precompute A^(2^127) and A^(2^76) only once

  (when #t #;(not mrg32k3a-generators)
    (set! mrg32k3a-generators
          (list (power-power A 127)
                (power-power A  76)
                (power A 16))))

  ; compute M = A^(16 + i*2^127 + j*2^76)
  (let ((M (product
            (list-ref mrg32k3a-generators 2)
            (product
             (power (list-ref mrg32k3a-generators 0)
                    (modulo i (expt 2 28)))
             (power (list-ref mrg32k3a-generators 1)
                    (modulo j (expt 2 28)))))))
    (mrg32k3a-pack-state
     (vector
      (vector-ref M 0)
      (vector-ref M 3)
      (vector-ref M 6)
      (vector-ref M 9)
      (vector-ref M 12)
      (vector-ref M 15)))))

; True Randomization
; ==================
;
; The value obtained from the system time is feed into a very
; simple pseudo random number generator. This in turn is used
; to obtain numbers to randomize the state of the MRG32k3a
; generator, avoiding period degeneration.

(define: (mrg32k3a-randomize-state [state : State]) : State

  ; G. Marsaglia's simple 16-bit generator with carry
  (define: m : Nb 65536)
  (define: x : Nb (modulo (:random-source-current-time) m))
  (define: (random-m) : Nb
    (let ((y (modulo x m)))
      (set! x (+ (* 30903 y) (quotient x m)))
      y))
  (define: (random [n : Nb]) : Nb ; m < n < m^2
    (modulo (+ (* (random-m) m) (random-m)) n))

  ; modify the state
  (let ((m1 mrg32k3a-m1)
        (m2 mrg32k3a-m2)
        (s (mrg32k3a-unpack-state state)))
    (mrg32k3a-pack-state
     (vector
      (+ 1 (modulo (+ (vector-ref s 0) (random (- m1 1))) (- m1 1)))
      (modulo (+ (vector-ref s 1) (random m1)) m1)
      (modulo (+ (vector-ref s 2) (random m1)) m1)
      (+ 1 (modulo (+ (vector-ref s 3) (random (- m2 1))) (- m2 1)))
      (modulo (+ (vector-ref s 4) (random m2)) m2)
      (modulo (+ (vector-ref s 5) (random m2)) m2)))))


; Large Integers
; ==============
;
; To produce large integer random deviates, for n > m-max, we first
; construct large random numbers in the range {0..m-max^k-1} for some
; k such that m-max^k >= n and then use the rejection method to choose
; uniformly from the range {0..n-1}.

(define: mrg32k3a-m-max : Integer
  (mrg32k3a-random-range))

(define: (mrg32k3a-random-power [state : State] [k : Nb]) : Nb ; n = m-max^k, k >= 1
  (if (= k 1)
      (mrg32k3a-random-integer state mrg32k3a-m-max)
      (+ (* (mrg32k3a-random-power state (- k 1)) mrg32k3a-m-max)
         (mrg32k3a-random-integer state mrg32k3a-m-max))))

(define: (mrg32k3a-random-large [state : State] [n : Nb]) : Nb ; n > m-max
  (do: : Integer ((k : Integer 2 (+ k 1))
                  (mk : Integer (* mrg32k3a-m-max mrg32k3a-m-max) (* mk mrg32k3a-m-max)))
      ((>= mk n)
       (let* ((mk-by-n (quotient mk n))
              (a (* mk-by-n n)))
         (do: : Integer ((x : Integer (mrg32k3a-random-power state k)
                            (mrg32k3a-random-power state k)))
              ((< x a) (quotient x mk-by-n)))))))


; Multiple Precision Reals
; ========================
;
; To produce multiple precision reals we produce a large integer value
; and convert it into a real value. This value is then normalized.
; The precision goal is unit <= 1/(m^k + 1), or 1/unit - 1 <= m^k.
; If you know more about the floating point number types of the
; Scheme system, this can be improved.

(define: (mrg32k3a-random-real-mp [state : State] [unit : Real]) : Number
  (do: : Real ((k : Integer 1 (+ k 1))
               (u : Real (- (/ 1 unit) 1) (/ u mrg32k3a-m1)))
      ((<= u 1)
       (/ (exact->inexact (+ (mrg32k3a-random-power state k) 1))
          (exact->inexact (+ (expt mrg32k3a-m-max k) 1))))))


; Provide the Interface as Specified in the SRFI
; ==============================================
;
; An object of type random-source is a record containing the procedures
; as components. The actual state of the generator is stored in the
; binding-time environment of make-random-source.

(define: (make-random-source) : Random
  (let: ((state : State (mrg32k3a-pack-state ; make a new copy
                         (list->vector (vector->list mrg32k3a-initial-state)))))
    (:random-source-make
     (lambda: ()
       (mrg32k3a-state-ref state))
     (lambda: ([new-state : (Listof Integer)])
       (set! state (mrg32k3a-state-set new-state)))
     (lambda: ()
       (set! state (mrg32k3a-randomize-state state)))
     (lambda: ([i : Integer] [j : Integer])
       (set! state (mrg32k3a-pseudo-randomize-state i j)))
     (lambda: ()
       (lambda: ([n : Nb])
         (cond
          ((not (and (integer? n) (exact? n) (positive? n)))
           (error "range must be exact positive integer" n))
          ((<= n mrg32k3a-m-max)
           (mrg32k3a-random-integer state n))
          (else
           (mrg32k3a-random-large state n)))))
     (lambda: [args : Nb *]
       (cond
        ((null? args)
         (lambda ()
           (mrg32k3a-random-real state)))
        ((null? (cdr args))
         (let: ((unit : Flt (car args)))
           (cond
            ((not (and (real? unit) (< 0 unit 1)))
             (error "unit must be real in (0,1)" unit))
            ((<= (- (/ 1 unit) 1) mrg32k3a-m1)
             (lambda: ()
               (mrg32k3a-random-real state)))
            (else
             (lambda: ()
               (mrg32k3a-random-real-mp state unit))))))
        (else
         (error "illegal arguments" args)))))))

(define: random-source? : (Any -> Boolean : Random)
  :random-source?)

(define: (random-source-state-ref [s : Random]) : SpList
  ((:random-source-state-ref s)))

(define: (random-source-state-set! [s : Random] [state : Nbs]) : Void
  ((:random-source-state-set! s) state))

(define: (random-source-randomize! [s : Random]) : Void
  ((:random-source-randomize! s)))

(define: (random-source-pseudo-randomize! [s : Random] [i : Nb] [j : Nb]): Void
  ((:random-source-pseudo-randomize! s) i j))

; ---

(define: (random-source-make-integers [s : Random]): (Nb -> Nb)
  ((:random-source-make-integers s)))

(define: (random-source-make-reals [s : Random] . [unit : Nb *]) : ( -> Flt)
  (apply (:random-source-make-reals s)  unit))

; ---

(define: default-random-source : Random
  (make-random-source))

(define: random-integer : (Nb -> Nb)
  (random-source-make-integers default-random-source))

(define: random-real : ( -> Flt )
  (random-source-make-reals default-random-source))


) ; module ends
