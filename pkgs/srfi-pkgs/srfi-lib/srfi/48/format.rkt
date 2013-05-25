; SRFI 48
; Zhu Chongkai   mrmathematica@yahoo.com
; 28-May-2005
(module format mzscheme
  
  (require mzlib/pretty)
  
  (provide s:format)
  
  (define (s:format . args)
    (cond
      ((null? args)
       (raise (make-exn:fail:contract:arity
               "format: expects at least 1 argument, given 0"
               (current-continuation-marks))))
      ((string? (car args))
       (apply s:format #f args))
      ((< (length args) 2)
       (raise (make-exn:fail:contract:arity
               "format: expects at least 1 string arguments, given 0"
               (current-continuation-marks))))
      (else
       (let ((output-port   (car  args))
             (format-string (cadr args))
             (args          (cddr args)))
         (let ((port
                (cond ((output-port? output-port) output-port)
                      ((eq? output-port #t) (current-output-port))
                      ((eq? output-port #f) (open-output-string))
                      (else (raise-type-error 'format "output-port/boolean" 0 args)))))
           
           (define (point-five? n)
             (let ((absn (abs n)))
               (= 0.5 (- absn (truncate absn)))))
           
           (define (round* n scale) ;; assert scale < 0
             ;; Note: Scheme's "round to even" rule for 0.5*
             (let ((one (expt 10 (- scale))))
               (/ (round (* n one)) one)))
           
           (define (string-index str c)
             (let ((len (string-length str)))
               (let loop ((i 0))
                 (cond ((= i len) #f)
                       ((eqv? c (string-ref str i)) i)
                       (else (loop (+ i 1)))))))
           
           (define (string-grow str len char)
             (let ((off (- len (string-length str))))
               (if (positive? off)
                   (string-append (make-string off char) str)
                   str)))
           
           (define (string-pad-right str len char)
             (let ((slen (string-length str)))
               (cond ((< slen len)
                      (string-append str (make-string (- len slen) char)))
                     ((> slen len)
                      (substring (number->string
                                  (round* (string->number str) len))
                                 0
                                 len))
                     (else str))))
           
           (define (format-fixed number-or-string width digits)
             (cond
               ((string? number-or-string)
                (string-grow number-or-string width #\space))
               ((number? number-or-string)
                (let ((real (real-part number-or-string))
                      (imag (imag-part number-or-string)))
                  (cond
                    ((not (zero? imag))
                     (string-grow
                      (string-append (format-fixed real 0 digits)
                                     (if (negative? imag) "" "+")
                                     (format-fixed imag 0 digits)
                                     "i")
                      width
                      #\space))
                    (digits
                     (let* ((rounded-number (exact->inexact (round* real (- digits))))
                            (rounded-string (number->string rounded-number))
                            (dot-index (string-index  rounded-string #\.))
                            (exp-index (string-index  rounded-string #\e))
                            (length    (string-length rounded-string))
                            (pre-string
                             (cond
                               (exp-index
                                (if dot-index
                                    (substring rounded-string 0 (+ dot-index 1))
                                    (substring rounded-string 0 (+ exp-index 1))))
                               (dot-index
                                (substring rounded-string 0 (+ dot-index 1)))
                               (else
                                rounded-string)))
                            (exp-string
                             (if exp-index
                                 (substring rounded-string exp-index length)
                                 ""))
                            (frac-string
                             (if exp-index
                                 (substring rounded-string (+ dot-index 1) exp-index)
                                 (substring rounded-string (+ dot-index 1) length))))
                       (string-grow
                        (string-append pre-string
                                       (if dot-index "" ".")
                                       (string-pad-right frac-string digits #\0)
                                       exp-string)
                        width
                        #\space)))
                    (else ;; no digits
                     (string-grow (number->string real) width #\space)))))
               (else
                (raise-type-error 'format "number/string" number-or-string))))
           
           (define documentation-string
             "(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
")
           
           (define (require-an-arg args)
             (unless (pair? args)
               (raise-mismatch-error 'format "too few arguments: " args)))
           
           (define (format-help format-strg arglist)
             (letrec ((length-of-format-string (string-length format-strg))
                      (anychar-dispatch
                       (lambda (pos arglist last-was-newline)
                         (if (>= pos length-of-format-string)
                             arglist ; return unused args
                             (let ((char (string-ref format-strg pos)))
                               (cond
                                 ((eqv? char #\~)
                                  (tilde-dispatch (+ pos 1) arglist last-was-newline))
                                 (else
                                  (write-char char port)
                                  (anychar-dispatch (+ pos 1) arglist #f)))))))
                      (has-newline?
                       (lambda (whatever last-was-newline)
                         (or (eqv? whatever #\newline)
                             (and (string? whatever)
                                  (let ((len (string-length whatever)))
                                    (if (zero? len)
                                        last-was-newline
                                        (eqv? #\newline
                                              (string-ref whatever (- len 1)))))))))
                      (tilde-dispatch
                       (lambda (pos arglist last-was-newline)
                         (cond
                           ((>= pos length-of-format-string)
                            (write-char #\~ port) ; tilde at end of string is just output
                            arglist)
                           (else
                            (case (char-upcase (string-ref format-strg pos))
                              ((#\A)       ; Any -- for humans
                               (require-an-arg arglist)
                               (let ((whatever (car arglist)))
                                 (display whatever port)
                                 (anychar-dispatch (+ pos 1) 
                                                   (cdr arglist) 
                                                   (has-newline? whatever
                                                                 last-was-newline))))
                              ((#\S)       ; Slashified -- for parsers
                               (require-an-arg arglist)
                               (let ((whatever (car arglist)))
                                 (write whatever port)
                                 (anychar-dispatch (+ pos 1)
                                                   (cdr arglist)
                                                   (has-newline? whatever
                                                                 last-was-newline))))
                              ((#\W)
                               (require-an-arg arglist)
                               (let ((whatever (car arglist)))
                                 (write whatever port)
                                 (anychar-dispatch (+ pos 1)
                                                   (cdr arglist)
                                                   (has-newline? whatever
                                                                 last-was-newline))))
                              ((#\D)       ; Decimal
                               (require-an-arg arglist)
                               (display (number->string (car arglist) 10) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\X)       ; HeXadecimal
                               (require-an-arg arglist)
                               (display (number->string (car arglist) 16) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\O)       ; Octal
                               (require-an-arg arglist)
                               (display (number->string (car arglist)  8) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\B)       ; Binary
                               (require-an-arg arglist)
                               (display (number->string (car arglist)  2) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\C)       ; Character
                               (require-an-arg arglist)
                               (write-char (car arglist) port)
                               (anychar-dispatch (+ pos 1)
                                                 (cdr arglist)
                                                 (eqv? (car arglist) #\newline)))
                              ((#\~)       ; Tilde
                               (write-char #\~ port)
                               (anychar-dispatch (+ pos 1) arglist #f))
                              ((#\%)       ; Newline
                               (newline port)
                               (anychar-dispatch (+ pos 1) arglist #t))
                              ((#\&)      ; Freshline
                               (unless last-was-newline
                                 (newline port))
                               (anychar-dispatch (+ pos 1) arglist #t))
                              ((#\_)       ; Space
                               (write-char #\space port)
                               (anychar-dispatch (+ pos 1) arglist #f))
                              ((#\T)       ; Tab
                               (write-char #\tab port)
                               (anychar-dispatch (+ pos 1) arglist #f))
                              ((#\Y)       ; Pretty-print
                               (pretty-print (car arglist) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\F)
                               (require-an-arg arglist)
                               (display (format-fixed (car arglist) 0 #f) port)
                               (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                               ;; gather "~w[,d]F" w and d digits
                               (let loop ((index (+ pos 1))
                                          (w-digits (list (string-ref format-strg pos)))
                                          (d-digits '())
                                          (in-width? #t))
                                 (if (>= index length-of-format-string)
                                     (raise-mismatch-error 'format
                                                           "improper numeric format directive in "
                                                           format-strg)
                                     (let ((next-char (string-ref format-strg index)))
                                       (cond
                                         ((char-numeric? next-char)
                                          (if in-width?
                                              (loop (+ index 1)
                                                    (cons next-char w-digits)
                                                    d-digits
                                                    in-width?)
                                              (loop (+ index 1)
                                                    w-digits
                                                    (cons next-char d-digits)
                                                    in-width?)))
                                         ((char=? next-char #\F)
                                          (let ((width
                                                 (string->number (list->string (reverse w-digits))))
                                                (digits
                                                 (if (zero? (length d-digits))
                                                     #f
                                                     (string->number (list->string (reverse d-digits))))))
                                            (display (format-fixed (car arglist) width digits) port)
                                            (anychar-dispatch (+ index 1) (cdr arglist) #f)))
                                         ((char=? next-char #\,)
                                          (if in-width?
                                              (loop (+ index 1)
                                                    w-digits
                                                    d-digits
                                                    #f)
                                              (raise-mismatch-error 'format
                                                                    "too many commas in directive "
                                                                    format-strg)))
                                         (else
                                          (raise-mismatch-error 'format
                                                                "~w.dF directive ill-formed in "
                                                                format-strg)))))))
                              ((#\? #\K)       ; indirection -- take next arg as format string
                               (cond           ;  and following arg as list of format args
                                 ((< (length arglist) 2)
                                  (raise-mismatch-error 'format
                                                        "less arguments than specified for ~?: "
                                                        arglist))
                                 ((not (string? (car arglist)))
                                  (raise-mismatch-error 'format
                                                        "~? requires a string: "
                                                        (car arglist)))
                                 (else
                                  (format-help (car arglist) (cadr arglist))
                                  (anychar-dispatch (+ pos 1) (cddr arglist) #f))))
                              ((#\H)      ; Help
                               (display documentation-string port)
                               (anychar-dispatch (+ pos 1) arglist #t))
                              (else
                               (raise-mismatch-error 'format
                                                     "unknown tilde escape: "
                                                     (string-ref format-strg pos)))))))))
               (anychar-dispatch 0 arglist #f)))
           
           (let ((unused-args (format-help format-string args)))
             (if (not (null? unused-args))
                 (raise-mismatch-error 'format "unused arguments " unused-args))
             (if (eq? output-port #f)    ;; if format into a string 
                 (get-output-string port)) ;; then return the string
             )))))))
