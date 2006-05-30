;Java runtime utilities
;Kathryn Gray 
;July 2001

;This module provides functions needed at runtime for compiled Java code

(module runtime mzscheme
  
  (require (lib "class.ss")
           (lib "Object.ss" "profj" "libs" "java" "lang")
           (lib "String.ss" "profj" "libs" "java" "lang")
           (lib "Throwable.ss" "profj" "libs" "java" "lang")
           (lib "ArithmeticException.ss" "profj" "libs" "java" "lang")
           (lib "ClassCastException.ss" "profj" "libs" "java" "lang")
           (lib "NullPointerException.ss" "profj" "libs" "java" "lang")
           (lib "parameters.ss" "profj"))
  
  (provide convert-to-string shift not-equal bitwise mod divide-dynamic divide-int 
           divide-float and or cast-primitive cast-reference instanceof-array nullError
           check-eq? dynamic-equal? compare compare-within)

  (define (check-eq? obj1 obj2)
    (or (eq? obj1 obj2)
        (cond
          ((is-a? obj1 wrapper) (send obj1 compare obj1 obj2))
          ((is-a? obj2 wrapper) (send obj2 compare obj1 obj2))
          (else #f))))
  
  (define (dynamic-equal? val1 val2)
    (cond
      ((number? val1) (= val1 val2))
      (else (check-eq? val1 val2))))
  
  ;convert-to-string: (U string int real bool char Object) -> string
  (define (convert-to-string data)
    (cond
      ((string? data) (make-java-string data))
      ((number? data) (make-java-string (number->string data)))
      ((boolean? data) 
       (make-java-string (if data "true" "false")))
      ((char? data) (make-java-string (string data)))
      ((is-a? data ObjectI) (send data toString))
      ((is-a? data object%) (make-java-string "SchemeObject"))
      (else (error 'JavaRuntime:Internal_Error:convert-to-string
                   (format "Convert to string given unsupported data: ~s" data)))))
  
  ;Performs arithmetic shifts on the given integers. 
  ;shift: symbol int int -> int
  (define (shift op left right)
    (case op
      ((<<) (arithmetic-shift left right))
      ((>>) (arithmetic-shift left (- right)))
      ((>>>) (+ (arithmetic-shift left (- right)) (arithmetic-shift 2 (bitwise-not right))))))
 
  ;not-equal: num num -> bool
  (define (not-equal left right) 
    (if (number? left)
        (not (= left right))
        (not (eq? left right))))
  
  ;bitwise: symbol (U (int int) (bool bool)) -> int
  (define (bitwise op left right)
    (if (number? left)
        (case op
          ((&) (bitwise-and left right))
          ((^) (bitwise-xor left right))
          ((or) (bitwise-ior left right)))
        (case op
          ((&) (and left right))
          ((^) (and (not (and left right))
                    (or left right)))
          ((or) (or left right)))))

  ;divide-dynamic: number number -> number
  (define (divide-dynamic left right)
    (if (or (inexact? left) (inexact? right))
        (divide-float left right)
        (divide-int left right)))
  
  ;divide-int: int int -> int
  (define (divide-int left right)
    (when (zero? right)
      (create-java-exception ArithmeticException
                             "Illegal division by zero"
                             (lambda (exn msg)
                               (send exn ArithmeticException-constructor-java.lang.String msg))
                             (current-continuation-marks)))
    (quotient left right))
  
  ;divide-float: float float -> float
  (define (divide-float left right)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    (current-continuation-marks))))
    (if (and (exact? left) (exact? right))
        (exact->inexact (/ left right))
        (/ left right)))
     
  ;modulo: number number -> number
  (define (mod left right)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    (current-continuation-marks))))
    (remainder left right))
  
  (define (raise-class-cast msg)
    (raise (create-java-exception ClassCastException
                                  msg
                                  (lambda (exn msg)
                                    (send exn ClassCastException-constructor-java.lang.String msg))
                                  (current-continuation-marks))))
  
  (define (make-brackets dim)
    (if (= 0 dim)
        ""
        (string-append "[]" (make-brackets (sub1 dim)))))
  
  ;cast-primitive: value symbol int -> value
  (define (cast-primitive val type dim)
    ;(printf "cast-primitive ~a ~a ~n" val type)
    (if (> dim 0)
        (if (send val check-prim-type type dim)
            val
            (raise-class-cast 
             (format "Cast to ~a~a failed for ~a" type (make-brackets dim) (send (convert-to-string val) get-mzscheme-string))))
        (case type
          ((boolean)
           (unless (boolean? val)
             (raise-class-cast (format "Cast to boolean failed for ~a" 
                                       (send (convert-to-string val) get-mzscheme-string))))
           val)
          ((byte short int long)
           (cond
             ((and (number? val) (inexact? val)) (truncate (inexact->exact val)))
             ((and (number? val) (exact? val) (rational? val)) (truncate val))
             ((and (number? val) (exact? val)) val)
             ((char? val) (char->integer val))
             (else (raise-class-cast (format "Cast to ~a failed for ~a"
                                             type
                                             (send (convert-to-string val) get-mzscheme-string))))))
          ((char)
           (cond
             ((char? val) val)
             ((and (number? val) (exact? val)) (integer->char val))
             (else (raise-class-cast (format "Cast to character failed for ~a"
                                             (send (convert-to-string val) get-mzscheme-string))))))
          ((float double)
           (cond
             ((and (number? val) (inexact? val)) val)
             ((and (number? val) (exact? val)) (exact->inexact val))
             ((number? val) val)
             ((char? val) (char->integer val))
             (else (raise-class-cast (format "Cast to ~a failed for ~a" type
                                             (send (convert-to-string val) get-mzscheme-string)))))))))
  
  ;cast-reference: value class class class int symbol-> value
  (define (cast-reference val type ca-type gc-type dim name)
    (if (> dim 0)
        (if (send val check-ref-type type dim)
            val
            (raise-class-cast
             (format "Cast to ~a~a failed for ~a" name (make-brackets dim) (send (convert-to-string val) get-mzscheme-string))))
        (cond
          ((and (eq? Object type) (is-a? val ObjectI)) val)
          ((and (is-a? val convert-assert-Object) (is-a? val ca-type)) val)
          ((is-a? val convert-assert-Object)
           (or (send val down-cast type ca-type) 
               (raise-class-cast (format "Cast to ~a failed for ~a" name (send val my-name)))))
          ((and (is-a? val guard-convert-Object) (is-a? val gc-type)) val)
          ((is-a? val guard-convert-Object)
           (or (send val down-cast type gc-type)
               (raise-class-cast (format "Cast to ~a failed for ~a" name (send val my-name)))))
          ((is-a? val type) val)
          (else (raise-class-cast (format "Cast to ~a failed for ~a" name (send val my-name)))))))
  
  ;instanceof-array: bool val (U class sym) int -> bool
  (define (instanceof-array prim? val type dim)
    (if prim?
        (send val check-prim-type type dim)
        (send val check-ref-type type dim)))
  
  ;nullError: symbol -> void
  (define (nullError kind)
    (raise
     (create-java-exception NullPointerException
                            (case kind
                              ((method) 
                               "This value cannot access a method to call as it is null and therefore has no methods")
                              ((field) 
                               "This value cannot retrieve a field as it is null and therefore has no fields"))
                            (lambda (exn msg)
                              (send exn NullPointerException-constructor-java.lang.String msg))
                            (current-continuation-marks))))
  
  ;compare: val val (list symbol string ...) string (U #f object)-> boolean
  (define (compare test act info src test-obj)
    (compare-within test act 0.0 info src test-obj #f))
  
  ;compare-within: val val val (list symbol string) (U #f object) . boolean -> boolean
  (define (compare-within test act range info src test-obj . within?)
    (when test-obj (send test-obj add-check))
    (letrec ((java-equal?
              (lambda (v1 v2 visited-v1 visited-v2)
                (or (eq? v1 v2)
                    (already-seen? v1 v2 visited-v1 visited-v2)
                    (cond 
                      ((and (number? v1) (number? v2))
                       (if (or (inexact? v1) (inexact? v2))
                           (<= (abs (- v1 v2)) range)                         
                           (= v1 v2)))
                      ((and (object? v1) (object? v2))
                       (cond
                         ((equal? "String" (send v1 my-name))
                          (and (equal? "String" (send v2 my-name))
                               (equal? (send v1 get-mzscheme-string) (send v2 get-mzscheme-string))))
                         ((equal? "array" (send v1 my-name))
                          (and (equal? "array" (send v2 my-name))
                               (= (send v1 length) (send v2 length))
                               (let ((v1-vals (array->list v1))
                                     (v2-vals (array->list v2)))
                                 (andmap (lambda (x) x)
                                         (map java-equal? v1-vals v2-vals 
                                              (map (lambda (v) (cons v1 visited-v1)) v1-vals)
                                              (map (lambda (v) (cons v2 visited-v2)) v2-vals))))))
                         (else
                          (and (equal? (send v1 my-name) (send v2 my-name))
                               (let ((v1-fields (send v1 field-values))
                                     (v2-fields (send v2 field-values)))
                                 (and (= (length v1-fields) (length v2-fields))
                                      (andmap (lambda (x) x) 
                                              (map java-equal? v1-fields v2-fields 
                                                   (map (lambda (v) (cons v1 visited-v1)) v1-fields)
                                                   (map (lambda (v) (cons v2 visited-v2)) v2-fields)))))))))
                      ((and (not (object? v1)) (not (object? v2))) (equal? v1 v2))
                      (else #f))))))
      (let ((res (java-equal? test act null null)))
        (unless res
          (when test-obj
            (send test-obj
                  check-failed
                  (append '("check expected ")
                          (list (case (car info)
                                  ((field) 
                                   (format "the ~a field of class ~a to have value "
                                           (caddr info) (cadr info)))
                                  ((static-field) 
                                   (format "the class field ~a of ~a to have value "
                                           (caddr info) (cadr info)))
                                  ((var) 
                                   (format "the local variable ~a to have value" (cadr info)))
                                  ((alloc) 
                                   (format "the instantiation of class ~a with values with types ~a to produce a "
                                           (cadr info)
                                           (caddr info)
                                           ))
                                  ((call) (format "the call to method ~a from ~a, with values with types ~a, to produce the value "
                                                  (caddr info) (cadr info) (cadddr info)))
                                  ((array) "the array value ")
                                  ((unary) (format "the unary operation ~a to produce " (cadr info)))
                                  ((value) "value ")))
                          (if (null? within?)
                              (list "within " (send test-obj format-value range)
                                    " of " (send test-obj format-value act))
                              (list (send test-obj format-value act)))
                          '(", instead found ")
                          (list (send test-obj format-value test)))
                  src)))
        res)))
                            
  
   ;array->list: java-array -> (list 'a)
  (define (array->list v)
    (letrec ((len (send v length))
             (build-up
              (lambda (c)
                (if (= c len)
                    null
                    (cons (send v access c)
                          (build-up (add1 c)))))))
      (build-up 0)))  
  
  ;already-seen?: 'a 'a (list 'a) (list 'a)-> bool
  (define (already-seen? v1 v2 visited-v1 visited-v2)
    (cond
      ((and (null? visited-v1) (null? visited-v2)) #f)
      ((memq v1 visited-v1)
       (let ((position-v1 (get-position v1 visited-v1 0)))
         (eq? v2 (list-ref visited-v2 position-v1))))
      (else #f)))
  
  ;get-position: 'a (list 'a) int -> int
  (define (get-position v1 visited pos)
    (if (eq? v1 (car visited))
        pos
        (get-position v1 (cdr visited) (add1 pos))))
  
  )