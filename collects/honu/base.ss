(module base mzscheme

  (require (lib "class.ss"))

  (define (printStr s)
    (display s))

  (define (printLine s)
    (display s)
    (newline))
  
  (define (readChar)
    (read-char))

  (define (readLine)
    (read-line))

  (define (strToInt s)
    (let ([number (string->number s)])
      (if (and number (integer? number))
          number
          (error (format "Tried to convert \"~a\" to an integer" s)))))

  (define (strToFloat s)
    (let ([number (string->number s)])
      (if (and number (inexact? number))
          number
          (error (format "Tried to convert \"~a\" to an float" s)))))

  (define (intToStr i)
    (number->string i))

  (define (floatToStr f)
    (number->string f))
  
  (define (charToStr c)
    (string c))

  (define (strLen s)
    (string-length s))

  (define (substr s start end)
    (cond
      [(< start 0)
       (error (format "Start index for substr must be positive, got ~a" start))]
      [(> start end)
       (error (format "Start index (~a) must be <= end index (~a)" start end))]
      [(> end (string-length s))
       (error (format "End index for substr must be <= strLen(s), got ~a" end))]
      [else (substring s start end)]))

  (define (charAt s i)
    (cond
      [(< i 0)
       (error (format "Index for charAt must be positive, got ~a" i))]
      [(> i (- (string-length s) 1))
       (error (format "Index for charAt must be < strLen(s), got ~a" i))]
      [else (string-ref s i)]))

  (provide (all-from mzscheme)
           (all-from (lib "class.ss"))
           (all-defined)))
