(module compression mzscheme  
  (provide write-unary         read-unary
           write-gamma         read-gamma
           write-delta         read-delta
           write-number/delta  read-number/delta 
           write-number/gamma  read-number/gamma 
           write-number/unary  read-number/unary 
           write-number        read-number)
  
  (require "planet/bit-io.scm") ; (planet "bit-io.scm" ("soegaard" "bit-io.plt" 2 0))
  
  ;;;
  ;;; UNARY CODE
  ;;;
  
  ; The unary code for an integer n>=1 is n-1 one bits followed by a zero bit.
  ; The code for 3 is 110.
  
  (define write-unary
    (case-lambda
      [(n)
       (write-unary n (current-output-bit-port))]
      [(n out-bit-port)
       (unless (and (integer? n) (positive? n))
         (error #f "a positive integer was expected, got: " n))
       (if (> n 1)
           (write-bits (sub1 n)
                       (sub1 (arithmetic-shift 2 (sub1 (sub1 n))))
                       out-bit-port))
       (write-bits 1 0 out-bit-port)]))
  
  (define read-unary
    (case-lambda
      [()
       (read-unary (current-input-bit-port))]
      [(in-bit-port)
       (do ([n 1 (+ n 1)])
         [(= (read-bits 1 in-bit-port) 0)
          n])]))
  
  ;;;
  ;;; GAMMA CODE
  ;;;
  
  ; The gamma code for an integer x>=1 consists of the
  ; unary code 1+floor(log x) followed by floor(log x) bits
  ; representing x-2^floor(log x) in binary.
  ; The code for 9 is 1110 001, since floor(log 9)=3.
  
  
  (define write-gamma
    (case-lambda
      [(n)
       (write-gamma n (current-output-bit-port))]
      [(n out-bit-port)
       (unless (and (integer? n) (positive? n))
         (error #f "a positive integer was expected, got: " n))
       (let ([floor-log-n (sub1 (integer-length n))])
         (write-unary (add1 floor-log-n) out-bit-port)
         (write-bits floor-log-n 
                     (- n (arithmetic-shift 1 floor-log-n))
                     out-bit-port))]))
  
  (define read-gamma
    (case-lambda
      [()
       (read-gamma (current-input-bit-port))]
      [(in-bit-port)
       (let ([floor-log-n (sub1 (read-unary in-bit-port))])
         (if (zero? floor-log-n)
             1 
             (+ (read-bits floor-log-n in-bit-port)
                (arithmetic-shift 1 floor-log-n))))]))
  
  ;;; 
  ;;; DELTA CODE
  ;;;
  
  (define write-delta
    (case-lambda
      [(n)
       (write-delta n (current-output-bit-port))]
      [(n out-bit-port)
       (unless (and (integer? n) (positive? n))
         (error #f "a positive integer was expected, got: " n))
       (let ([floor-log-n (sub1 (integer-length n))])
         (write-gamma (add1 floor-log-n) out-bit-port)
         (write-bits floor-log-n 
                     (- n (arithmetic-shift 1 floor-log-n))
                     out-bit-port))]))
  
  (define read-delta
    (case-lambda
      [()
       (read-delta (current-input-bit-port))]
      [(in-bit-port)
       (let ([floor-log-n (sub1 (read-gamma in-bit-port))])
         (if (zero? floor-log-n)
             1 
             (+ (read-bits floor-log-n in-bit-port)
                (arithmetic-shift 1 floor-log-n))))]))
  
  
  ;;;
  ;;; NUMBERS
  ;;;
  
  (define (make-number-writer writer)
    (letrec ([w
              (case-lambda
                [(n)
                 (w n (current-output-bit-port))]
                [(n out-bit-port)
                 (writer (add1 n) out-bit-port)])])
      w))
  
  (define (make-number-reader reader)
    (letrec ([r
              (case-lambda 
                [()
                 (r (current-input-bit-port))]
                [(in-bit-port)
                 (sub1 (reader in-bit-port))])])
      r))
  
  (define write-number/delta (make-number-writer write-delta))
  (define write-number/gamma (make-number-writer write-gamma))
  (define write-number/unary (make-number-writer write-unary))
  
  (define read-number/delta (make-number-reader read-delta))
  (define read-number/gamma (make-number-reader read-gamma))
  (define read-number/unary (make-number-reader read-unary))
  
  (define write-number write-number/delta)
  (define read-number  read-number/delta)
  
  ;;;
  ;;; TEST
  ;;;

  #|

  (require (planet "42.ss" ("soegaard" "srfi.plt")))
  
  (define (display* x)
    (display (list x))
    x)
  
  (require (planet "io.ss" ("dherman" "io.plt")))
  
  ; Write the numbers from 1 to 100 to a temporary file
  ; using write-unary, then read them with read-unary.
  
  (define N 10000)
  
  (define (numbers-test test-name reader writer)
    (let* ([numbers 
            (list-ec (: n 1 N) n)]
           [numbers-read
            (with-temporary-file file ()
              (with-output-to-bit-file file
                (λ ()
                  (for-each writer numbers))
                'replace)
              (display (format "~a\n" (list test-name (file-size file))))
              (with-input-from-bit-file file
                (λ ()
                  (list-ec (:repeat (length numbers))
                           (:let number-read (reader))
                           ; (begin (display number-read) (display " "))
                           number-read))))])
      ;(display numbers-read)
      (if (equal? numbers numbers-read)
          (begin 
            (display (list test-name 'ok))
            (newline))
          (begin
            (display (list test-name 'FAIL))
            (newline)))))
  
  ;(time (numbers-test 'unary read-unary write-unary))
  (time (numbers-test 'gamma read-gamma write-gamma))
  (time (numbers-test 'delta read-delta write-delta))

 |#
  
  )