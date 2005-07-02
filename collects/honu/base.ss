(module base mzscheme

  (require (lib "class.ss"))

  (define null%
    (class object%
      (super-new)))
  
  (define null-obj (new null%))

  (define Any<%>
    (interface ()))
  
  (define (printString s)
    (display s)
    '())

  (define (printLine s)
    (display s)
    (newline)
    '())
  
  (define (readChar arg-tuple)
    (read-char))

  (define (readLine arg-tuple)
    (read-line))

  (define (stringToInt s)
    (let ([number (string->number s)])
      (if (and number (integer? number))
          number
          (error (format "Tried to convert \"~a\" to an integer" s)))))

  (define (stringToFloat s)
    (let ([number (string->number s)])
      (if (and number (inexact? number))
          number
          (error (format "Tried to convert \"~a\" to an float" s)))))

  (define (intToString i)
    (number->string i))

  (define (floatToString f)
    (number->string f))
  
  (define (charToString c)
    (string c))

  (define (strlen s)
    (string-length s))

  (define (substr arg-tuple)
    (let-values ([(s start end) (apply values arg-tuple)])
      (cond
        [(< start 0)
         (error (format "Start index for substr must be positive, got ~a" start))]
        [(> start end)
         (error (format "Start index (~a) must be <= end index (~a)" start end))]
        [(> end (string-length s))
         (error (format "End index for substr must be <= strlen(s), got ~a" end))]
        [else (substring s start end)])))

  (define (charAt arg-tuple)
    (let-values ([(s i) (apply values arg-tuple)])
      (cond
        [(< i 0)
         (error (format "Index for charAt must be non-negative, got ~a" i))]
        [(> i (- (string-length s) 1))
         (error (format "Index for charAt must be < strlen(s), got ~a" i))]
        [else (string-ref s i)])))

  (provide (all-from mzscheme)
           (all-from (lib "class.ss"))
           (all-defined)))
