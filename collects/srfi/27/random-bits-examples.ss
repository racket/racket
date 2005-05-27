(module random-bits-examples mzscheme
  (require (lib "random-bits.ss" "srfi" "27"))

  (define ascii->char integer->char)

; CONFIDENCE TESTS FOR SRFI-27 "Sources of Random Bits"
; =====================================================
;
; Sebastian.Egner@philips.com, 2002.
;
; This file contains a small collection of checks for the
; implementation of SRFI-27. It is not meant to be complete
; or to test the actual properties of the underlying generator.
; It is merely meant to run the code and to check some of the
; assumptions made by specification. There is an interface to
; G. Marsaglia's DIEHARD battery of tests for random number
; generators, though.
;
; running this file in Scheme 48 0.57 with SRFI-27 installed:
;
;   ,open srfi-27 srfi-23 ascii
;   ,load conftest.scm
;
; speed in Scheme 48 0.57 with SRFI-27 opened:
;
;   ,time (do ((k 0 (+ k 1))) ((= k 100000)) (random-integer 2))
;   ; about  30000/s on P3@800MHz for the Scheme-impl.
;   ; about 160000/s on P3@800MHz for the C/Scheme-impl.
;
;   ,time (do ((k 0 (+ k 1))) ((= k 100000)) (random-real))
;   ; about   3000/s on P3@800MHz for the Scheme-impl.
;   ; about 180000/s on P3@800MHz for the C/Scheme-impl.
;
; speed in PLT 204 with SRFI-27 opened:
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-integer 2)))
;   ; about 30000/s on P3@800MHz for the Scheme-impl.
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-real)))
;   ; about 50000/s on P3@800MHz for the Scheme-impl.
;
; running this file in Gambit 3.0 (interpreter) with SRFI-27 loaded:
;
;   (define ascii->char integer->char)
;   (load "conftest.scm")
;
; compiling a simple program in Gambit 3.0 (compiler) using this file:
;
;   1. create 'a.scm' with content;
;     (include "srfi-27-c.scm")
;     (time (do ((k 0 (+ k 1))) ((= k 1000000)) (random-integer 2)))
;     (time (do ((k 0 (+ k 1))) ((= k 1000000)) (random-real)))
;   2. compile Scheme into C (verbose paths for clarity)
;     GAMBCDIR=$GAMBIT/lib LD_LIBRARY_PATH=$GAMBIT/lib $GAMBIT/gsc/gsc a
;   3. compile and link C code into an executable
;     gcc -I$GAMBIT/lib -o a a.c a_.c -lm -lgambc -L$GAMBIT/lib
;   4. run the executable
;     GAMBCDIR=$GAMBIT/lib LD_LIBRARY_PATH=$GAMBIT/lib a
;
; speed in Gambit 3.0 with SRFI-27 loaded:
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-integer 2)))
;   ; about 5000/s on P3@800MHz, interpreted
;   ; about 200000/s on P3@800MHz, compiled
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-real)))
;   ; about 25000/s on P3@800MHz, interpreted
;   ; about 400000/s on P3@800MHz, compiled
;
; history of this file:
;   SE, 19-Mar-2002: initial version, based on earlier tests
;   SE, 22-Mar-2002: adapted to new procedure names
;   SE, 25-Mar-2002: more descriptive output
;   SE, 04-Apr-2002: some quick timings; check up


; (check expr)
;    evals expr and issues an error if it is not #t.

(define (check expr)
  (if (not (eq? (eval expr (interaction-environment)) #t))
      (error "check failed" expr)))

; Basic Tests of the Interface
; ============================

(define (my-random-integer n)
  (let ((x (random-integer n)))
    (if (<= 0 x (- n 1))
        x
        (error "(random-integer n) returned illegal value" x))))

(define (my-random-real)
  (let ((x (random-real)))
    (if (< 0 x 1)
        x
        (error "(random-real) returned illegal value" x))))

(define (check-basics-1)

  ; generate increasingly large numbers
  (display "; generating large numbers [bits]: ")
  (do ((k 0 (+ k 1))
       (n 1 (* n 2)))
      ((> k 1024))
    (display k)
    (display " ")
    (my-random-integer n))
  (display "ok")
  (newline)

  ; generate some reals
  (display "; generating reals [1000 times]: ")
  (do ((k 0 (+ k 1))
       (x (my-random-real) (+ x (my-random-real))))
      ((= k 1000)
       x))
  (display "ok")
  (newline)

  ; get/set the state
  (display "; get/set state: ")
  (let* ((state1 (random-source-state-ref default-random-source))
         (x1 (my-random-integer (expt 2 32)))
         (state2 (random-source-state-ref default-random-source))
         (x2 (my-random-integer (expt 2 32))))
    (random-source-state-set! default-random-source state1)
    (let ((y1 (my-random-integer (expt 2 32))))
      (if (not (= x1 y1))
          (error "state get/set doesn't work" x1 y1 state1)))
    (random-source-state-set! default-random-source state2)
    (let ((y2 (my-random-integer (expt 2 32))))
      (if (not (= x2 y2))
          (error "state get/set doesn't work" x2 y2 state2))))
  (display "ok")
  (newline)

  ; randomize!
  (display "; randomize!: ")
  (let* ((state1 (random-source-state-ref default-random-source))
         (x1 (my-random-integer (expt 2 32))))
    (random-source-state-set! default-random-source state1)
    (random-source-randomize! default-random-source)
    (let ((y1 (my-random-integer (expt 2 32))))
      (if (= x1 y1)
          (error "random-source-randomize! didn't work" x1 state1))))
  (display "ok")
  (newline)

  ; pseudo-randomize!
  (display "; pseudo-randomize!: ")
  (let* ((state1 (random-source-state-ref default-random-source))
         (x1 (my-random-integer (expt 2 32))))
    (random-source-state-set! default-random-source state1)
    (random-source-pseudo-randomize! default-random-source 0 1)
    (let ((y1 (my-random-integer (expt 2 32))))
      (if (= x1 y1)
          (error "random-source-pseudo-randomize! didn't work" x1 state1)))
    (random-source-state-set! default-random-source state1)
    (random-source-pseudo-randomize! default-random-source 1 0)
    (let ((y1 (my-random-integer (expt 2 32))))
      (if (= x1 y1)
          (error "random-source-pseudo-randomize! didn't work" x1 state1))))
  (display "ok")
  (newline)
  (newline))


; Testing the MRG32k3a Generator (if implemented)
; ===============================================

; (check-mrg32k3a)
;   tests if the underlying generator is the MRG32k3a generator
;   as implemented in the reference implementation. This function
;   is useful to check whether the reference implementation computes
;   the right numbers.

(define (check-mrg32k3a)
 
  ; check if the initial state is A^16 * (1 0 0 1 0 0)
  (display "; check A^16 * (1 0 0 1 0 0)")
  (let* ((s (make-random-source))
         (state1 (random-source-state-ref s))
	 (rand (random-source-make-reals s)))
    (random-source-state-set! s '(lecuyer-mrg32k3a 1 0 0 1 0 0))
    (do ((k 0 (+ k 1)))
	((= k 16)
	 (let ((state2 (random-source-state-ref s)))
	   (if (not (equal? state1 state2))
	       (error "16-th state after (1 0 0 1 0 0) is wrong"))))
	(rand)))
  (display "ok")
  (newline)

  ; check if pseudo-randomize! advances properly
  (display "; checking (random-source-pseudo-randomize! s 1 2)")
  (let ((s (make-random-source)))
    (random-source-pseudo-randomize! s 1 2)
    (if (not (equal? (random-source-state-ref s)
		     '(lecuyer-mrg32k3a 
		       1250826159 
		       3004357423 
		        431373563 
		       3322526864 
		        623307378 
		       2983662421)))
	(error "pseudo-randomize! gives wrong result")))
  (display "ok")
  (newline)

  ; run the check published by Pierre L'Ecuyer:
  ;   Note that the reference implementation deals slightly different
  ;   with reals mapping m1-1 into 1-1/(m1+1) and not into 0 as in
  ;   L'Ecuyer's original proposal. However, for the first 10^7 reals
  ;   that makes no difference as m1-1 is not generated.
  (display "; checking (random-source-pseudo-randomize! s 1 2)...")
  (let* ((x 0.0) 
	 (s (make-random-source))
	 (rand (random-source-make-reals s)))
    (random-source-state-set!
     s
     '(lecuyer-mrg32k3a 12345 12345 12345 12345 12345 12345))
    (do ((k 0 (+ k 1)))
	((= k 10000000)
	 (if (not (< (abs (- x 5001090.95)) 0.01))
	     (error "bad sum over 10^7 reals" x)))
      (set! x (+ x (rand)))))
  (display "ok")
  (newline))


; Writing Data to DIEHARD
; =======================

; (write-diehard filename s bytes-per-call calls)
;    creates a binary file to which bytes-per-call * calls bytes are
;    written. The bytes are obtained from the random source s using
;    the range n = (expt 256 bytes-per-call).
;       The intention of write-diehard is to give implementors a 
;    '15 min.'-way of running their favourite random number generator 
;    through a pretty tough testsuite.
;
;    try: For the reference implementation, the call
;
;       (write-diehard "outfile" (make-random-source) 4 2867200)
;
;    should create a file that looks as follows (od -A x -t x1 outfile):
;
;       0000000 92 bb 7e db 1b 14 f6 bb bb 54 a1 55 c2 3e cd ca
;       0000010 23 01 20 35 06 47 65 b0 52 4c b8 c0 21 48 af 67
;       0000020 63 a9 8c 78 50 73 29 08 62 d1 22 7f a6 89 96 77
;       0000030 98 28 65 2d 2d 8b f9 52 41 be 8e 3f c5 84 0f ca
;       0000040 c0 fa 03 d6 f0 65 9d 3a 9b ab 6f fe d1 aa 5f 92
;       0000050 0f ea f6 3b 78 b9 fe ad 63 5e 49 f1 9d c9 8e 2f
;       0000060 53 a9 5d 32 d4 20 51 1d 1c 2e 82 f0 8b 26 40 c0
;       ...total length is 11468800 bytes.
;
;    The message digest is md5sum = 4df554f56cb5ed251bd04b0d50767443.
;
;    try: For the reference implementation, the call
;
;       (write-diehard "outfile" (make-random-source) 3 3822934)
;
;    should create a file that looks as follows (od -A x -t x1 outfile):
;
;       000000 bb 7e db 30 a3 49 14 f6 bb d0 f2 d0 54 a1 55 8b
;       000010 8c 03 3e cd ca a3 88 1d 01 20 35 e8 50 c8 47 65
;       000020 b0 e7 d9 28 4c b8 c0 f2 82 35 48 af 67 42 3e 8a
;       000030 a9 8c 78 12 ef b6 73 29 08 ff e9 71 d1 22 7f 52
;       000040 b8 f0 89 96 77 dc 71 86 28 65 2d c2 82 fc 8b f9
;       000050 52 d7 23 2a be 8e 3f 61 a8 99 84 0f ca 44 83 65
;       000060 fa 03 d6 c2 11 c0 65 9d 3a c2 7a dd ab 6f fe 1c
;       ...total length is 11468802 bytes.
;
;    The message digest is md5sum = 750ac219ff40c50bb2d04ff5eff9b24c.

(define (write-diehard filename s bytes-per-call calls)
  (let ((port (open-output-file filename))
	(rand (random-source-make-integers s))
        (n (expt 256 bytes-per-call)))
    (do ((i 0 (+ i 1))) 
        ((= i calls) 
         (close-output-port port))
      (let ((x (rand n)))
        (do ((k 0 (+ k 1))) ((= k bytes-per-call))
          (write-char (ascii->char (modulo x 256)) port)
          (set! x (quotient x 256)))))))
         
; run some tests
(check-basics-1)
(display "passed (check-basics-1)")
(newline)

) ; end module