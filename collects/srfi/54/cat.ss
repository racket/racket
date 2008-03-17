;; based on soo's (the author of the SRFI) R6RS implemenations
#lang scheme/base

(provide cat)

(define (expr->string v writer)
  (let ([port (open-output-string)])
    (writer v port)
    (get-output-string port)))

(define (take-both-end str take)
  (let ((left (car take)))
    (cond
      ((string? left)
       (if (null? (cdr take))
         (string-append left str)
         (if (list? take)
           (let ((right (cadr take)))
             (if (string? right)
               (string-append left str right)
               (if (zero? right)
                 ""
                 (let* ((lt-str (string-append left str))
                        (lt-len (string-length lt-str)))
                   (if (negative? right)
                     (if (positive? (+ lt-len right))
                       (substring lt-str 0 (+ lt-len right))
                       "")
                     (if (< right lt-len)
                       (substring lt-str (- lt-len right) lt-len)
                       lt-str))))))
           (let ((right (cdr take)))
             (if (string? right)
               (string-append left str str right)
               (if (zero? right)
                 (string-append left str)
                 (let ((len (string-length str)))
                   (if (negative? right)
                     (if (positive? (+ len right))
                       (string-append
                        left str (substring str 0 (+ len right)))
                       (string-append left str))
                     (if (< right len)
                       (string-append
                        left str (substring str (- len right) len))
                       (string-append left str str))))))))))
      ((zero? left)
       (if (null? (cdr take))
         str
         (if (list? take)
           (let ((right (cadr take)))
             (if (string? right)
               (string-append str right)
               (if (zero? right)
                 ""
                 (let ((lt-len (string-length str)))
                   (if (negative? right)
                     (if (positive? (+ lt-len right))
                       (substring str 0 (+ lt-len right))
                       "")
                     (if (< right lt-len)
                       (substring str (- lt-len right) lt-len)
                       str))))))
           (let ((right (cdr take)))
             (if (string? right)
               (string-append str str right)
               (if (zero? right)
                 str
                 (let ((len (string-length str)))
                   (if (negative? right)
                     (if (positive? (+ len right))
                       (string-append
                        str (substring str 0 (+ len right)))
                       str)
                     (if (< right len)
                       (string-append
                        str (substring str (- len right) len))
                       (string-append str str))))))))))
      (else
       (let* ((len (string-length str))
              (lt-str (if (positive? left)
                        (if (< left len)
                          (substring str 0 left)
                          str)
                        (if (positive? (+ len left))
                          (substring str (abs left) len)
                          ""))))
         (if (null? (cdr take))
           lt-str
           (if (list? take)
             (let ((right (cadr take)))
               (if (string? right)
                 (string-append lt-str right)
                 (if (zero? right)
                   ""
                   (let ((lt-len (string-length lt-str)))
                     (if (negative? right)
                       (if (positive? (+ lt-len right))
                         (substring lt-str 0 (+ lt-len right))
                         "")
                       (if (< right lt-len)
                         (substring lt-str (- lt-len right) lt-len)
                         lt-str))))))
             (let ((right (cdr take)))
               (if (string? right)
                 (string-append lt-str str right)
                 (if (zero? right)
                   lt-str
                   (if (negative? right)
                     (if (positive? (+ len right))
                       (string-append
                        lt-str (substring str 0 (+ len right)))
                       lt-str)
                     (if (< right len)
                       (string-append
                        lt-str (substring str (- len right) len))
                       (string-append lt-str str)))))))))))))

(define (str-char-index str char start end)
  (let lp ((n start))
    (if (= n end)
      #f
      (if (char=? char (string-ref str n))
        n
        (lp (+ n 1))))))

(define (str-numeric-index str start end)
  (let lp ((n start))
    (if (= n end)
      #f
      (if (char-numeric? (string-ref str n))
        n
        (lp (+ n 1))))))

(define (str-numeric? str start end)
  (let lp ((n start))
    (if (= n end)
      #t
      (if (char-numeric? (string-ref str n))
        (lp (+ n 1))
        #f))))

(define (fixnum-string-separate str sep num sig)
  (let* ((len (string-length str))
         (dot-index (str-char-index str #\. 1 len)))
    (if dot-index
      (if sig
        (if (and (str-numeric? str 1 dot-index)
                 (str-numeric? str (+ 1 dot-index) len))
          (string-append
           (apply string-append
                  (let loop ((ini 0)
                             (pos (+ 1 (let ((pos (remainder
                                                   (- dot-index 1) num)))
                                         (if (zero? pos) num pos)))))
                    (if (< pos dot-index)
                      (cons (substring str ini pos)
                            (cons sep (loop pos (+ pos num))))
                      (list (substring str ini dot-index)))))
           "."
           (apply string-append
                  (let loop ((ini (+ 1 dot-index))
                             (pos (+ 1 dot-index num)))
                    (if (< pos len)
                      (cons (substring str ini pos)
                            (cons sep (loop pos (+ pos num))))
                      (list (substring str ini len))))))
          str)
        (if (and (str-numeric? str 0 dot-index)
                 (str-numeric? str (+ 1 dot-index) len))
          (string-append
           (apply string-append
                  (let loop ((ini 0)
                             (pos (let ((pos (remainder dot-index num)))
                                    (if (zero? pos) num pos))))
                    (if (< pos dot-index)
                      (cons (substring str ini pos)
                            (cons sep (loop pos (+ pos num))))
                      (list (substring str ini dot-index)))))
           "."
           (apply string-append
                  (let loop ((ini (+ 1 dot-index))
                             (pos (+ 1 dot-index num)))
                    (if (< pos len)
                      (cons (substring str ini pos)
                            (cons sep (loop pos (+ pos num))))
                      (list (substring str ini len))))))
          str))
      (if sig
        (if (str-numeric? str 1 len)
          (apply string-append
                 (let loop ((ini 0)
                            (pos (+ 1 (let ((pos (remainder (- len 1)
                                                            num)))
                                        (if (zero? pos) num pos)))))
                   (if (< pos len)
                     (cons (substring str ini pos)
                           (cons sep (loop pos (+ pos num))))
                     (list (substring str ini len)))))
          str)
        (if (str-numeric? str 0 len)
          (apply string-append
                 (let loop ((ini 0)
                            (pos (let ((pos (remainder len num)))
                                   (if (zero? pos) num pos))))
                   (if (< pos len)
                     (cons (substring str ini pos)
                           (cons sep (loop pos (+ pos num))))
                     (list (substring str ini len)))))
          str)))))

(define (separate str sep num)
  (let ((len (string-length str))
        (n (abs num)))
    (apply string-append
           (let loop ((ini 0)
                      (pos (if (negative? num)
                             n
                             (let ((pos (remainder len n)))
                               (if (zero? pos) n pos)))))
             (if (< pos len)
               (cons (substring str ini pos)
                     (cons sep (loop pos (+ pos n))))
               (list (substring str ini len)))))))

(define (every? pred ls)		;not for list but for pair & others
  (let lp ((ls ls))
    (if (pair? ls)
      (if (pred (car ls))
        (lp (cdr ls))
        #f)
      (if (null? ls)
        #t
        (if (pred ls)
          #t
          #f)))))

(define (every-within-number? pred ls n) ;not for list but for pair & others
  (let lp ((ls ls) (num 0))
    (if (pair? ls)
      (if (and (< num n) (pred (car ls)))
        (lp (cdr ls) (+ num 1))
        #f)
      (if (null? ls)
        #t
        (if (and (< num n) (pred ls))
          #t
          #f)))))

(define (exact-integer/string? ns)
  (or (and (integer? ns)
           (exact? ns))
      (string? ns)))

(define (mold str pre)
  (let* ((len (string-length str))
         (ind (str-char-index str #\. 1 (- len 1))))
    (if ind
      (let ((d-len (- len (+ ind 1))))
        (cond
          ((= d-len pre) str)
          ((< d-len pre) (string-append str (make-string (- pre d-len) #\0)))
          ;;((char<? #\4 (string-ref str (+ 1 ind pre)))
          ;;(let ((com (expt 10 pre)))
          ;;  (number->string (/ (round (* (string->number str) com)) com))))
          ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
               (and (char=? #\5 (string-ref str (+ 1 ind pre)))
                    (or (< (+ 1 pre) d-len)
                        (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
                              '(#\1 #\3 #\5 #\7 #\9)))))
           (apply
            string
            (let* ((minus (char=? #\- (string-ref str 0)))
                   (str (substring str (if minus 1 0) (+ 1 ind pre)))
                   (char-list
                    (reverse
                     ;;(let lp ((index (- (string-length str) 1))
                     (let lp ((index (- (+ ind pre) (if minus 1 0)))
                              (raise #t))
                       (if (= -1 index)
                         (if raise '(#\1) '())
                         (let ((chr (string-ref str index)))
                           (if (char=? #\. chr)
                             (cons chr (lp (- index 1) raise))
                             (if raise
                               (if (char=? #\9 chr)
                                 (cons #\0 (lp (- index 1) raise))
                                 (cons (integer->char
                                        (+ 1 (char->integer chr)))
                                       (lp (- index 1) #f)))
                               (cons chr (lp (- index 1) raise))))))))))
              (if minus (cons #\- char-list) char-list))))
          (else
           (substring str 0 (+ 1 ind pre)))))
      (string-append str "." (make-string pre #\0)))))

(define (mold-non-finites str pre)
  (let* ((len (string-length str))
         (ind (str-char-index str #\. 1 (- len 1)))
         (d-len (- len (+ ind 1))))
    (if (char-numeric? (string-ref str (- ind 1)))
      (cond
        ((= d-len pre) str)
        ((< d-len pre) (string-append str (make-string (- pre d-len) #\0)))
        ;;((char<? #\4 (string-ref str (+ 1 ind pre)))
        ;;(let ((com (expt 10 pre)))
        ;;  (number->string (/ (round (* (string->number str) com)) com))))
        ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
             (and (char=? #\5 (string-ref str (+ 1 ind pre)))
                  (or (< (+ 1 pre) d-len)
                      (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
                            '(#\1 #\3 #\5 #\7 #\9)))))
         (apply
          string
          (let* ((minus (char=? #\- (string-ref str 0)))
                 (str (substring str (if minus 1 0) (+ 1 ind pre)))
                 (char-list
                  (reverse
                   ;;(let lp ((index (- (string-length str) 1))
                   (let lp ((index (- (+ ind pre) (if minus 1 0)))
                            (raise #t))
                     (if (= -1 index)
                       (if raise '(#\1) '())
                       (let ((chr (string-ref str index)))
                         (if (char=? #\. chr)
                           (cons chr (lp (- index 1) raise))
                           (if raise
                             (if (char=? #\9 chr)
                               (cons #\0 (lp (- index 1) raise))
                               (cons (integer->char
                                      (+ 1 (char->integer chr)))
                                     (lp (- index 1) #f)))
                             (cons chr (lp (- index 1) raise))))))))))
            (if minus (cons #\- char-list) char-list))))
        (else
         (substring str 0 (+ 1 ind pre))))
      (error "cat: infinities or nans cannot have precisions"))))

(define (e-mold str pre)
  (let* ((len (string-length str))
         (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
      (string-append (mold (substring str 0 e-index) pre)
                     (substring str e-index len))
      (mold-non-finites str pre))))

(define (flonum-mold str pre)
  (let* ((len (string-length str))
         (e-index (str-char-index str #\e 1 (- len 1))))
    (string-append (mold (substring str 0 e-index) pre)
                   (substring str e-index len))))

#;(define (remove-zero str len negative)
    (if negative
      (let lp ((n 1))
        (let ((c (string-ref str n)))
          (cond
            ((char=? #\0 c) (lp (+ 1 n)))
            ((char=? #\. c)
             (if (= n 2)
               str
               (string-append "-" (substring str (- n 1) len))))
            (else
             (if (= n 1)
               str
               (string-append "-" (substring str n len)))))))
      (let lp ((n 0))
        (let ((c (string-ref str n)))
          (cond
            ((char=? #\0 c) (lp (+ 1 n)))
            ((char=? #\. c)
             (if (= n 1)
               str
               (substring str (- n 1) len)))
            (else
             (if (zero? n)
               str
               (substring str n len))))))))

(define (real->fixnum-string n)
  (let* ((str (number->string (exact->inexact n)))
         (len (string-length str))
         (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
      (let ((e-number (string->number (substring str (+ 1 e-index) len)))
            (d-index (str-char-index str #\. 1 e-index)))
        (if (negative? e-number)
          (if d-index
            (if (negative? n)
              (let ((p-number (- (abs e-number) (- d-index 1))))
                (if (negative? p-number)
                  (let ((pnumber (+ 1 (abs p-number))))
                    (string-append (substring str 0 pnumber)
                                   "."
                                   (substring str pnumber d-index)
                                   (substring str (+ 1 d-index)
                                              e-index)))
                  (string-append "-0."
                                 (make-string p-number #\0)
                                 (substring str 1 d-index)
                                 (substring str (+ 1 d-index)
                                            e-index))))
              (let ((p-number (- (abs e-number) d-index)))
                (if (negative? p-number)
                  (let ((pnumber (abs p-number)))
                    (string-append (substring str 0 pnumber)
                                   "."
                                   (substring str pnumber d-index)
                                   (substring str (+ 1 d-index)
                                              e-index)))
                  (string-append "0."
                                 (make-string p-number #\0)
                                 (substring str 0 d-index)
                                 (substring str (+ 1 d-index)
                                            e-index)))))
            (if (negative? n)
              (let ((p-number (- (abs e-number) (- e-index 1))))
                (if (negative? p-number)
                  (let ((pnumber (+ 1 (abs p-number))))
                    (string-append (substring str 0 pnumber)
                                   "."
                                   (substring str pnumber e-index)))
                  (string-append "-0."
                                 (make-string p-number #\0)
                                 (substring str 1 e-index))))
              (let ((p-number (- (abs e-number) e-index)))
                (if (negative? p-number)
                  (let ((pnumber (abs p-number)))
                    (string-append (substring str 0 pnumber)
                                   "."
                                   (substring str pnumber e-index)))
                  (string-append "0."
                                 (make-string p-number #\0)
                                 (substring str 0 e-index))))))
          (if d-index
            (let ((p-number (- e-number (- e-index (+ d-index 1)))))
              (if (negative? p-number)
                ;; A procedure REMOVE-ZERO is unnecessary
                ;; due to number->string.
                ;; 0.00123 -> 00.0123 or 000123
                ;; -0.00123 -> -00.0123 or -000123
                ;;(remove-zero (string-append
                ;;	      (substring str 0 d-index)
                ;;	      (substring str (+ 1 d-index)
                ;;			 (+ 1 d-index e-number))
                ;;	      "."
                ;;	      (substring str (+ 1 d-index e-number)
                ;;			 e-index))
                ;;	     e-index
                ;;	     (< n 0))
                (string-append (substring str 0 d-index)
                               (substring str (+ 1 d-index)
                                          (+ 1 d-index e-number))
                               "."
                               (substring str (+ 1 d-index e-number)
                                          e-index))
                ;; A procedure REMOVE-ZERO is unnecessary
                ;; due to number->string.
                ;; 0.00123 -> 00.0123 or 000123
                ;; -0.00123 -> -00.0123 or -000123
                ;;(remove-zero (string-append
                ;;	      (substring str 0 d-index)
                ;;	      (substring str (+ 1 d-index) e-index)
                ;;	      (make-string p-number #\0)
                ;;	      ".0")
                ;;	     (+ e-index p-number 1)
                ;;	     (< n 0))))
                (string-append (substring str 0 d-index)
                               (substring str (+ 1 d-index) e-index)
                               (make-string p-number #\0) ".0")))
            (string-append (substring str 0 e-index)
                           (make-string e-number #\0)
                           ".0"))))
      (let ((d-index (str-char-index str #\. 1 (- len 1))))
        (if (char-numeric? (string-ref str (- d-index 1)))
          str
          (error "cat: infinities or nans cannot be changed into fixed-point numbers"))))))

(define (non-0-index str start)
  (let lp ((n start))
    (if (char=? #\0 (string-ref str n))
      (lp (+ 1 n))
      n)))

(define (non-0-index-right str end)
  (let lp ((n (- end 1)))
    (if (char=? #\0 (string-ref str n))
      (lp (- n 1))
      n)))

#;(define (non-0-dot-index-right str end)
    (let lp ((n (- end 1)))
      (let ((c (string-ref str n)))
        (if (or (char=? #\0 c) (char=? #\. c))
          (lp (- n 1))
          n))))

(define (real->flonum-string n)
  (let* ((str (number->string (exact->inexact n)))
         (len (string-length str))
         (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
      str
      (let ((d-index (str-char-index str #\. 1 (- len 1))))
        (if (< -1 n 1)
          (if (zero? n)
            (string-append str "e+0") ;for -0.0 or +0.0
            (let ((n-index (non-0-index str (+ 1 d-index))))
              (string-append (if (negative? n) "-" "")
                             (substring str n-index (+ 1 n-index))
                             "."
                             (if (= n-index (- len 1))
                               "0"
                               (substring str (+ 1 n-index) len))
                             "e-"
                             (number->string (- n-index d-index)))))
          ;;(let ((n-index (non-0-dot-index-right str len)))
          ;;  (if (< n-index d-index)
          (let ((n-index (non-0-index-right str len)))
            (if (= n-index d-index)
              (let ((n-index (non-0-index-right str d-index)))
                (if (char-numeric? (string-ref str n-index))
                  (if (negative? n)
                    (string-append (substring str 0 2)
                                   "."
                                   (if (= n-index 1)
                                     "0"
                                     (substring str 2
                                                (+ 1 n-index)))
                                   "e+"
                                   (number->string (- d-index 2)))
                    (string-append (substring str 0 1)
                                   "."
                                   (if (= n-index 0)
                                     "0"
                                     (substring str 1
                                                (+ 1 n-index)))
                                   "e+"
                                   (number->string (- d-index 1))))
                  (error "cat: infinities or nans cannot be changed into floating-point numbers")))
              (if (negative? n)
                (string-append (substring str 0 2)
                               "."
                               (substring str 2 d-index)
                               (substring str (+ 1 d-index)
                                          (+ 1 n-index))
                               "e+"
                               (number->string (- d-index 2)))
                (string-append (substring str 0 1)
                               "."
                               (substring str 1 d-index)
                               (substring str (+ 1 d-index)
                                          (+ 1 n-index))
                               "e+"
                               (number->string (- d-index 1)))))))))))

(define-syntax wow-cat-end
  (syntax-rules ()
    ((wow-cat-end z n)
     (car z))
    ((wow-cat-end z n t)
     (let ((n (car z)))
       (if t n (error "cat: too many argument" z))))
    ((wow-cat-end z n t ts)
     (let ((n (car z)))
       (if t ts (error "cat: too many argument" z))))
    ((wow-cat-end z n t ts fs)
     (let ((n (car z)))
       (if t ts fs)))))

(define-syntax wow-cat!
  (syntax-rules ()
    ((wow-cat! z n d)
     (let ((n (car z)))
       (set! z (cdr z))
       n))
    ((wow-cat! z n d t)
     (let ((n (car z)))
       (if t
         (begin (set! z (cdr z)) n)
         (let lp ((head (list n)) (tail (cdr z)))
           (if (null? tail)
             d
             (let ((n (car tail)))
               (if t
                 (begin (set! z (append (reverse head) (cdr tail))) n)
                 (lp (cons n head) (cdr tail)))))))))
    ((wow-cat! z n d t ts)
     (let ((n (car z)))
       (if t
         (begin (set! z (cdr z)) ts)
         (let lp ((head (list n)) (tail (cdr z)))
           (if (null? tail)
             d
             (let ((n (car tail)))
               (if t
                 (begin (set! z (append (reverse head) (cdr tail))) ts)
                 (lp (cons n head) (cdr tail)))))))))
    ((wow-cat! z n d t ts fs)
     (let ((n (car z)))
       (if t
         (begin (set! z (cdr z)) ts)
         (begin (set! z (cdr z)) fs))))))

(define-syntax %alet-cat*
  (syntax-rules ()
    ((%alet-cat* z ((n d t ...)) bd ...)
     (let ((n (if (null? z)
                d
                (if (null? (cdr z))
                  (wow-cat-end z n t ...)
                  (error "cat: too many arguments" (cdr z))))))
       bd ...))
    ((%alet-cat* z ((n d t ...) . e) bd ...)
     (let ((n (if (null? z)
                d
                (wow-cat! z n d t ...))))
       (%alet-cat* z e bd ...)))
    ((%alet-cat* z e bd ...)
     (let ((e z)) bd ...))))

(define-syntax alet-cat*
  (syntax-rules ()
    ((alet-cat* z (a . e) bd ...)
     (let ((y z))
       (%alet-cat* y (a . e) bd ...)))))

(define (cat object . rest)
  (if (null? rest)
    (cond
      ((number? object) (number->string object))
      ((symbol? object) (symbol->string object))
      ((boolean? object) (if object "#t" "#f"))
      ((char? object) (string object))
      ((string? object) object)
      (else (expr->string object display)))
    (alet-cat* rest
               ((width 0 (and (integer? width) (exact? width)))
                (writer display (procedure? writer))
                (port #f (or (boolean? port) (output-port? port))
                      (if (eq? port #t) (current-output-port) port))
                (char #\space (char? char))
                (precision #f (and (integer? precision) (inexact? precision)))
                (radix 'decimal (memq radix '(decimal octal binary hexadecimal)))
                (point #f (memq point '(fixnum flonum)))
                (sign #f (eq? 'sign sign))
                (exactness #f (memq exactness '(exact inexact)))
                ;;(take #f (and (pair? take)
                ;;	       (every-within-number? exact-integer/string? 2)))
                (take #f (and (pair? take)
                              (exact-integer/string? (car take))
                              (or (null? (cdr take))
                                  (and (list? take)
                                       (null? (cddr take))
                                       (exact-integer/string? (cadr take)))
                                  (exact-integer/string? (cdr take)))))
                (pipe #f (and (pair? pipe) (every? procedure? pipe)))
                (separator #f (and (pair? separator)
                                   (char? (car separator))
                                   (or (null? (cdr separator))
                                       (and (list? separator)
                                            (null? (cddr separator))
                                            (exact-integer? (cadr separator)))))))
               (let ((str
                      (if (number? object)
                        (if (or (eq? writer display)
                                (eq? writer write))
                          (let* ((inexact-sign
                                  (and (not (eq? radix 'decimal))
                                       (or (and (or precision point)
                                                (error "cat: non-decimal cannot have a decimal point"))
                                           (and (inexact? object)
                                                (not (eq? exactness 'exact)))
                                           (eq? exactness 'inexact))
                                       "#i"))
                                 (str
                                  (cond
                                    (point
                                     (if (eq? point 'fixnum)
                                       (if precision
                                         (let ((p (inexact->exact
                                                   (abs precision))))
                                           (if (real? object)
                                             (mold
                                              (real->fixnum-string object) p)
                                             (let ((imag-str
                                                    (real->fixnum-string
                                                     (imag-part object))))
                                               (string-append
                                                (mold
                                                 (real->fixnum-string
                                                  (real-part object)) p)
                                                ;; for N+0.0i
                                                (if (char-numeric?
                                                     (string-ref imag-str 0))
                                                  "+" "")
                                                (mold imag-str p)
                                                "i"))))
                                         (if (real? object)
                                           (real->fixnum-string object)
                                           (let ((imag-str
                                                  (real->fixnum-string
                                                   (imag-part object))))
                                             (string-append
                                              (real->fixnum-string
                                               (real-part object))
                                              ;; for N+0.0i
                                              (if (char-numeric?
                                                   (string-ref imag-str 0))
                                                "+" "")
                                              imag-str
                                              "i"))))
                                       (if precision ;'flonum
                                         (let ((p (inexact->exact
                                                   (abs precision))))
                                           (if (real? object)
                                             (flonum-mold
                                              (real->flonum-string object) p)
                                             (let ((imag-str
                                                    (real->flonum-string
                                                     (imag-part object))))
                                               (string-append
                                                (flonum-mold
                                                 (real->flonum-string
                                                  (real-part object)) p)
                                                ;; for N+0.0i
                                                (if (char-numeric?
                                                     (string-ref imag-str 0))
                                                  "+" "")
                                                (flonum-mold imag-str p)
                                                "i"))))
                                         (if (real? object)
                                           (real->flonum-string object)
                                           (let ((imag-str
                                                  (real->flonum-string
                                                   (imag-part object))))
                                             (string-append
                                              (real->flonum-string
                                               (real-part object))
                                              ;; for N+0.0i
                                              (if (char-numeric?
                                                   (string-ref imag-str 0))
                                                "+" "")
                                              imag-str
                                              "i"))))))
                                    (precision
                                     (let ((p (inexact->exact (abs precision))))
                                       (if (real? object)
                                         (e-mold (number->string
                                                  (exact->inexact object)) p)
                                         (let ((imag-str
                                                (number->string
                                                 (exact->inexact
                                                  (imag-part object)))))
                                           (string-append
                                            (e-mold (number->string
                                                     (exact->inexact
                                                      (real-part object))) p)
                                            ;; for N+0.0i
                                            (if (char-numeric?
                                                 (string-ref imag-str 0))
                                              "+" "")
                                            (e-mold imag-str p)
                                            "i")))))
                                    (else
                                     (number->string
                                      (cond
                                        (inexact-sign (inexact->exact object))
                                        (exactness (if (eq? exactness 'exact)
                                                     (inexact->exact object)
                                                     (exact->inexact object)))
                                        (else object))
                                      (cdr (assq radix '((decimal . 10)
                                                         (octal . 8)
                                                         (hexadecimal . 16)
                                                         (binary . 2))))))))
                                 (str
                                  (if separator
                                    (fixnum-string-separate
                                     str
                                     (string (car separator))
                                     (if (null? (cdr separator))
                                       3 (abs (cadr separator)))
                                     (negative? (real-part object)))
                                    str))
                                 (str
                                  (string-append
                                   (or inexact-sign "")
                                   (if (or (and precision
                                                (not point)
                                                (or (eq? exactness 'exact)
                                                    (and (exact? object)
                                                         ;;(not (eq? exactness
                                                         ;;	  'inexact))
                                                         (not exactness)
                                                         (or (positive? precision)
                                                             (eqv? precision
                                                                   0.0)))))
                                           (and point
                                                (eq? exactness 'exact)))
                                     "#e" "")
                                   (cdr (assq radix
                                              '((decimal . "")
                                                (octal . "#o")
                                                (hexadecimal . "#x")
                                                (binary . "#b"))))
                                   (if (and sign
                                            ;;(positive? (real-part object)))
                                            ;; for 0.0
                                            (char-numeric? (string-ref str 0)))
                                     "+" "")
                                   str))
                                 (str (if pipe
                                        (if (list? pipe)
                                          (let loop ((str ((car pipe) str))
                                                     (fns (cdr pipe)))
                                            (if (null? fns)
                                              str
                                              (loop ((car fns) str)
                                                    (cdr fns))))
                                          (apply
                                           string-append
                                           (let loop ((fns pipe))
                                             (if (procedure? fns)
                                               (list (fns str))
                                               (cons ((car fns) str)
                                                     (loop (cdr fns)))))))
                                        str))
                                 (str (if take (take-both-end str take) str))
                                 (pad (- (abs width) (string-length str))))
                            (cond
                              ((<= pad 0) str)
                              ((positive? width)
                               (if (char-numeric? char)
                                 (let* ((len (string-length str))
                                        (index (str-numeric-index str 0 len)))
                                   (if index
                                     ;;(if (zero? index)
                                     (if (or (zero? index)
                                             ;; for infinities and nans
                                             (char=?
                                              (string-ref str (- index 1))
                                              #\.))
                                       (string-append
                                        (make-string pad char) str)
                                       (string-append
                                        (substring str 0 index)
                                        (make-string pad char)
                                        (substring str index len)))
                                     (string-append
                                      (make-string pad char) str)))
                                 (string-append (make-string pad char) str)))
                              (else (string-append str (make-string pad char)))))
                          (let* ((str (expr->string object writer))
                                 (str (if separator
                                        (fixnum-string-separate
                                         str
                                         (string (car separator))
                                         (if (null? (cdr separator))
                                           3 (abs (cadr separator)))
                                         (negative? (real-part object)))
                                        str))
                                 (str (if pipe
                                        (if (list? pipe)
                                          (let loop ((str ((car pipe) str))
                                                     (fns (cdr pipe)))
                                            (if (null? fns)
                                              str
                                              (loop ((car fns) str)
                                                    (cdr fns))))
                                          (apply
                                           string-append
                                           (let loop ((fns pipe))
                                             (if (procedure? fns)
                                               (list (fns str))
                                               (cons ((car fns) str)
                                                     (loop (cdr fns)))))))
                                        str))
                                 (str (if take (take-both-end str take) str))
                                 (pad (- (abs width) (string-length str))))
                            (cond
                              ((<= pad 0) str)
                              ((positive? width)
                               (if (char-numeric? char)
                                 (let* ((len (string-length str))
                                        (index (str-numeric-index str 0 len)))
                                   (if index
                                     ;;(if (zero? index)
                                     (if (or (zero? index)
                                             ;; for infinities and nans
                                             (char=?
                                              (string-ref str (- index 1))
                                              #\.))
                                       (string-append
                                        (make-string pad char)
                                        str)
                                       (string-append
                                        (substring str 0 index)
                                        (make-string pad char)
                                        (substring str index len)))
                                     (string-append (make-string pad char)
                                                    str)))
                                 (string-append (make-string pad char) str)))
                              (else
                               (string-append str (make-string pad char))))))
                        (let* ((str
                                (if (eq? writer display)
                                  (cond
                                    ((symbol? object) (symbol->string object))
                                    ((boolean? object) (if object "#t" "#f"))
                                    ((char? object) (string object))
                                    ((string? object) object)
                                    (else (expr->string object writer)))
                                  (if (eq? writer write)
                                    (cond
                                      ((symbol? object)
                                       (symbol->string object))
                                      ((boolean? object)
                                       (if object "#t" "#f"))
                                      (else (expr->string object writer)))
                                    (expr->string object writer))))
                               (str (if (and separator
                                             (not (null? (cdr separator))))
                                      (separate str (string (car separator))
                                                (cadr separator))
                                      str))
                               (str (if pipe
                                      (if (list? pipe)
                                        (let loop ((str ((car pipe) str))
                                                   (fns (cdr pipe)))
                                          (if (null? fns)
                                            str
                                            (loop ((car fns) str) (cdr fns))))
                                        (apply string-append
                                               (let loop ((fns pipe))
                                                 (if (procedure? fns)
                                                   (list (fns str))
                                                   (cons ((car fns) str)
                                                         (loop (cdr fns)))))))
                                      str))
                               (str (if take (take-both-end str take) str))
                               (pad (- (abs width) (string-length str))))
                          (cond
                            ((<= pad 0) str)
                            ((positive? width)
                             (string-append (make-string pad char) str))
                            (else
                             (string-append str (make-string pad char))))))))
                 (if port
                   (display str port)
                   str)))))
