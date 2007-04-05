(module cat mzscheme
  
  (provide cat)
  
  (define-syntax alet-cat*		; borrowed from SRFI-86
    (syntax-rules ()
      ((alet-cat* z (a . e) bd ...)
       (let ((y z))
         (%alet-cat* y (a . e) bd ...)))))
  
  (define-syntax %alet-cat*		; borrowed from SRFI-86
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
  
  (define-syntax wow-cat!			; borrowed from SRFI-86
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
  
  (define-syntax wow-cat-end		; borrowed from SRFI-86
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
  
  (define (str-index str char)
    (let ((len (string-length str)))
      (let lp ((n 0))
        (and (< n len)
             (if (char=? char (string-ref str n))
                 n
                 (lp (+ n 1)))))))
  
  (define (every? pred ls)
    (let lp ((ls ls))
      (or (null? ls)
          (and (pred (car ls))
               (lp (cdr ls))))))
  
  (define (part pred ls)
    (let lp ((ls ls) (true '()) (false '()))
      (cond
        ((null? ls) (cons (reverse true) (reverse false)))
        ((pred (car ls)) (lp (cdr ls) (cons (car ls) true) false))
        (else (lp (cdr ls) true (cons (car ls) false))))))
  
  (define (e-mold num pre)
    (let* ((str (number->string (exact->inexact num)))
           (e-index (str-index str #\e)))
      (if e-index
          (string-append (mold (substring str 0 e-index) pre)
                         (substring str e-index (string-length str)))
          (mold str pre))))
  
  (define (mold str pre)
    (let ((ind (str-index str #\.)))
      (if ind
          (let ((d-len (- (string-length str) (+ ind 1))))
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
                         (let lp ((index (- (string-length str) 1))
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
  
  (define (separate str sep num opt)
    (let* ((len (string-length str))
           (pos (if opt
                    (let ((pos (remainder (if (eq? opt 'minus) (- len 1) len)
                                          num)))
                      (if (= 0 pos) num pos))
                    num)))
      (apply string-append
             (let loop ((ini 0)
                        (pos (if (eq? opt 'minus) (+ pos 1) pos)))
               (if (< pos len)
                   (cons (substring str ini pos)
                         (cons sep (loop pos (+ pos num))))
                   (list (substring str ini len)))))))
  
  (define (cat object . rest)
    (let* ((str-rest (part string? rest))
           (str-list (car str-rest))
           (rest-list (cdr str-rest)))
      (if (null? rest-list)
          (apply string-append
                 (cond
                   ((number? object) (number->string object))
                   ((string? object) object)
                   ((char? object) (string object))
                   ((boolean? object) (if object "#t" "#f"))
                   ((symbol? object) (symbol->string object))
                   (else
                    (get-output-string
                     (let ((str-port (open-output-string)))
                       (write object str-port)
                       str-port))))
                 str-list)
          (alet-cat* rest-list
                     ((width 0 (and (integer? width) (exact? width)))
                      (port #f (or (boolean? port) (output-port? port))
                            (if (eq? port #t) (current-output-port) port))
                      (char #\space (char? char))
                      (converter #f (and (pair? converter)
                                         (procedure? (car converter))
                                         (procedure? (cdr converter))))
                      (precision #f (and (integer? precision)
                                         (inexact? precision)))
                      (sign #f (eq? 'sign sign))
                      (radix 'decimal
                             (memq radix '(decimal octal binary hexadecimal)))
                      (exactness #f (memq exactness '(exact inexact)))
                      (separator #f (and (list? separator)
                                         (< 0 (length separator) 3)
                                         (char? (car separator))
                                         (or (null? (cdr separator))
                                             (let ((n (cadr separator)))
                                               (and (integer? n) (exact? n)
                                                    (< 0 n))))))
                      (writer #f (procedure? writer))
                      (pipe #f (and (list? pipe)
                                    (not (null? pipe))
                                    (every? procedure? pipe)))
                      (take #f (and (list? take)
                                    (< 0 (length take) 3)
                                    (every? (lambda (x)
                                              (and (integer? x) (exact? x)))
                                            take))))
                     (let* ((str
                             (cond
                               ((and converter
                                     ((car converter) object))
                                (let* ((str ((cdr converter) object))
                                       (pad (- (abs width) (string-length str))))
                                  (cond
                                    ((<= pad 0) str)
                                    ((< 0 width) (string-append (make-string pad char) str))
                                    (else (string-append str (make-string pad char))))))
                               ((number? object)
                                (and (not (eq? radix 'decimal)) precision
                                     (error "cat: non-decimal cannot have a decimal point"))
                                (and precision (< precision 0) (eq? exactness 'exact)
                                     (error "cat: exact number cannot have a decimal point without exact sign"))
                                (let* ((exact-sign (and precision
                                                        (<= 0 precision)
                                                        (or (eq? exactness 'exact)
                                                            (and (exact? object)
                                                                 (not (eq? exactness
                                                                           'inexact))))
                                                        "#e"))
                                       (inexact-sign (and (not (eq? radix 'decimal))
                                                          (or (and (inexact? object)
                                                                   (not (eq? exactness
                                                                             'exact)))
                                                              (eq? exactness 'inexact))
                                                          "#i"))
                                       (radix-sign (cdr (assq radix
                                                              '((decimal . #f)
                                                                (octal . "#o")
                                                                (binary . "#b")
                                                                (hexadecimal . "#x")))))
                                       (plus-sign (and sign (< 0 (real-part object)) "+"))
                                       (exactness-sign (or exact-sign inexact-sign))
                                       (str
                                        (if precision
                                            (let ((precision (inexact->exact
                                                              (abs precision)))
                                                  (imag (imag-part object)))
                                              (if (= 0 imag)
                                                  (e-mold object precision)
                                                  (string-append
                                                   (e-mold (real-part object) precision)
                                                   (if (< 0 imag) "+" "")
                                                   (e-mold imag precision)
                                                   "i")))
                                            (number->string
                                             (cond
                                               (inexact-sign (inexact->exact object))
                                               (exactness
                                                (if (eq? exactness 'exact)
                                                    (inexact->exact object)
                                                    (exact->inexact object)))
                                               (else object))
                                             (cdr (assq radix '((decimal . 10)
                                                                (octal . 8)
                                                                (binary . 2)
                                                                (hexadecimal . 16)))))))
                                       (str
                                        (if (and separator
                                                 (not (or (and (eq? radix 'decimal)
                                                               (str-index str #\e))
                                                          (str-index str #\i)
                                                          (str-index str #\/))))
                                            (let ((sep (string (car separator)))
                                                  (num (if (null? (cdr separator))
                                                           3 (cadr separator)))
                                                  (dot-index (str-index str #\.)))
                                              (if dot-index
                                                  (string-append
                                                   (separate (substring str 0 dot-index)
                                                             sep num (if (< object 0)
                                                                         'minus #t))
                                                   "."
                                                   (separate (substring
                                                              str (+ 1 dot-index)
                                                              (string-length str))
                                                             sep num #f))
                                                  (separate str sep num (if (< object 0)
                                                                            'minus #t))))
                                            str))
                                       (pad (- (abs width)
                                               (+ (string-length str)
                                                  (if exactness-sign 2 0)
                                                  (if radix-sign 2 0)
                                                  (if plus-sign 1 0))))
                                       (pad (if (< 0 pad) pad 0)))
                                  (if (< 0 width)
                                      (if (char-numeric? char)
                                          (if (< (real-part object) 0)
                                              (string-append (or exactness-sign "")
                                                             (or radix-sign "")
                                                             "-"
                                                             (make-string pad char)
                                                             (substring str 1
                                                                        (string-length
                                                                         str)))
                                              (string-append (or exactness-sign "")
                                                             (or radix-sign "")
                                                             (or plus-sign "")
                                                             (make-string pad char)
                                                             str))
                                          (string-append (make-string pad char)
                                                         (or exactness-sign "")
                                                         (or radix-sign "")
                                                         (or plus-sign "")
                                                         str))
                                      (string-append (or exactness-sign "")
                                                     (or radix-sign "")
                                                     (or plus-sign "")
                                                     str
                                                     (make-string pad char)))))
                               (else
                                (let* ((str (cond
                                              (writer (get-output-string
                                                       (let ((str-port
                                                              (open-output-string)))
                                                         (writer object str-port)
                                                         str-port)))
                                              ((string? object) object)
                                              ((char? object) (string object))
                                              ((boolean? object) (if object "#t" "#f"))
                                              ((symbol? object) (symbol->string object))
                                              (else (get-output-string
                                                     (let ((str-port (open-output-string)))
                                                       (write object str-port)
                                                       str-port)))))
                                       (str (if pipe
                                                (let loop ((str ((car pipe) str))
                                                           (fns (cdr pipe)))
                                                  (if (null? fns)
                                                      str
                                                      (loop ((car fns) str)
                                                            (cdr fns))))
                                                str))
                                       (str
                                        (if take
                                            (let ((left (car take))
                                                  (right (if (null? (cdr take))
                                                             0 (cadr take)))
                                                  (len (string-length str)))
                                              (define (substr str beg end)
                                                (let ((end (cond
                                                             ((< end 0) 0)
                                                             ((< len end) len)
                                                             (else end)))
                                                      (beg (cond
                                                             ((< beg 0) 0)
                                                             ((< len beg) len)
                                                             (else beg))))
                                                  (if (and (= beg 0) (= end len))
                                                      str
                                                      (substring str beg end))))
                                              (string-append
                                               (if (< left 0)
                                                   (substr str (abs left) len)
                                                   (substr str 0 left))
                                               (if (< right 0)
                                                   (substr str 0 (+ len right))
                                                   (substr str (- len right) len))))
                                            str))
                                       (pad (- (abs width) (string-length str))))
                                  (cond
                                    ((<= pad 0) str)
                                    ((< 0 width) (string-append (make-string pad char) str))
                                    (else (string-append str (make-string pad char))))))))
                            (str (apply string-append str str-list)))
                       (and port (display str port))
                       str)))))
  )