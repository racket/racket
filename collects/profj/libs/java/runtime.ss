(module runtime scheme/base
  
  (require scheme/class
           scheme/list

           profj/libs/java/lang/Object
           profj/libs/java/lang/String
           profj/libs/java/lang/Throwable
           profj/libs/java/lang/ArithmeticException
           profj/libs/java/lang/ClassCastException
           profj/libs/java/lang/NullPointerException
           )
  
  (provide convert-to-string shift not-equal bitwise mod divide-dynamic divide-int 
           divide-float and or cast-primitive cast-reference instanceof-array nullError
           check-eq? dynamic-equal? compare compare-within check-catch check-mutate check-by
           compare-rand check-effect)

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
  (define (divide-dynamic left right marks)
    (if (or (inexact? left) (inexact? right))
        (divide-float left right)
        (divide-int left right)))
  
  ;divide-int: int int -> int
  (define (divide-int left right marks)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    marks)))
    (quotient left right))
  
  ;divide-float: float float -> float
  (define (divide-float left right marks)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    marks)))
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
             (format "Cast to ~a~a failed for ~a." name (make-brackets dim) (send (convert-to-string val) get-mzscheme-string))))
        (cond
          ((and (eq? Object type) (is-a? val ObjectI)) val)
          ((and (is-a? val convert-assert-Object) (is-a? val ca-type)) val)
          ((is-a? val convert-assert-Object)
           (or (send val down-cast type ca-type) 
               (raise-class-cast (format "Cast to ~a failed for ~a." name (send val my-name)))))
          ((and (is-a? val guard-convert-Object) (is-a? val gc-type)) val)
          ((is-a? val guard-convert-Object)
           (or (send val down-cast type gc-type)
               (raise-class-cast (format "Cast to ~a failed for ~a." name (send val my-name)))))
          ((is-a? val type) val)
          (else (raise-class-cast (format "Cast to ~a failed for ~a." name (send val my-name)))))))
  
  ;instanceof-array: bool val (U class sym) int -> bool
  (define (instanceof-array prim? val type dim)
    (if prim?
        (send val check-prim-type type dim)
        (send val check-ref-type type dim)))
  
  ;nullError: symbol -> void
  (define (nullError kind marks)
    (raise
     (create-java-exception NullPointerException
                            (case kind
                              ((method) 
                               "This value cannot access a method to call as it is null and therefore has no methods")
                              ((field) 
                               "This value cannot retrieve a field as it is null and therefore has no fields"))
                            (lambda (exn msg)
                              (send exn NullPointerException-constructor-java.lang.String msg))
                            marks #;(current-continuation-marks))))
  
  (define in-check-mutate? (make-parameter #f))
  (define stored-checks (make-parameter null))
  
  ;compare: val val (list symbol string ...) string (U #f object) boolean-> boolean
  (define (compare test act info src test-obj catch?)
    (compare-within test act 0.0001 info src test-obj catch? #f))
  
  (define exception (gensym 'exception))
  ;(make-exn-thrown exn boolean string)
  (define-struct exn-thrown (exception expected? cause))
  
  (define (java-equal? v1 v2 visited-v1 visited-v2 range use-range?)
    (or (eq? v1 v2)
        (already-seen? v1 v2 visited-v1 visited-v2)
        (cond 
          ((and (number? v1) (number? v2))
           (if (or (inexact? v1) (inexact? v2) use-range?)
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
                             (map (lambda (v1i v2i v1-valsi v2-valsi)
                                    (java-equal? v1i v2i v1-valsi v2-valsi range use-range?))
                                  v1-vals v2-vals 
                                  (map (lambda (v) (cons v1 visited-v1)) v1-vals)
                                  (map (lambda (v) (cons v2 visited-v2)) v2-vals)
                                  )))))
             (else
              (and (equal? (send v1 my-name) (send v2 my-name))
                   (let ((v1-fields (send v1 field-values))
                         (v2-fields (send v2 field-values)))
                     (and (= (length v1-fields) (length v2-fields))
                          (andmap (lambda (x) x) 
                                  (map 
                                   (lambda (v1-f v2-f v1-fvs v2-fvs)
                                     (java-equal? v1-f v2-f v1-fvs v2-fvs range use-range?))
                                   v1-fields v2-fields 
                                   (map (lambda (v) (cons v1 visited-v1)) v1-fields)
                                   (map (lambda (v) (cons v2 visited-v2)) v2-fields)))))))))
          ((and (not (object? v1)) (not (object? v2))) (equal? v1 v2))
          (else #f))))

  
  ;compare-within: (-> val) val val (list symbol string) (U #f object) boolean . boolean -> boolean
  (define (compare-within test act range info src test-obj catch? . within?)
    (let ((fail? #f))
      (set! test 
            (with-handlers ([exn? 
                             (lambda (e) 
                               (set! fail? #t)
                               (list exception catch? e "eval"))])
              (test)))
      (let ([res (if fail? #f (java-equal? test act null null range (not (null? within?))))]
            [values-list (append (list act test) (if (null? within?) (list range) null))])
        (if (in-check-mutate?)
            (stored-checks (cons (list res 'check-expect info values-list src) (stored-checks)))
            (report-check-result res 'check-expect info values-list src test-obj))
        res)))

  ;check-catch: (-> val) string class (list string) src object -> boolean
  (define (check-catch test name thrown info src test-obj)
    (let* ([result (with-handlers ([(lambda (e) (and (exn? e)
                                                     ((exception-is-a? thrown) e)))
                                    (lambda (e) #t)]
                                   [(lambda (e) (and (exn? e)
                                                     ((exception-is-a? Throwable) e)))
                                    (handle-exception
                                     (lambda (e) (send e my-name)))])
                     (test)
                     #f)]
           [return (and (boolean? result) result)]
           [values-list (cons name (if (boolean? result) null (list result)))])
      (if (in-check-mutate?)
          (stored-checks (cons (list return 'check-catch info values-list src) (stored-checks)))
          (report-check-result return 'check-catch info values-list src test-obj))
      return))
  
  ;check-by: (-> val) value (value value -> boolean) (list string) string src object -> boolean
  (define (check-by test act comp info meth src test-obj)
    (let* ([fail? #f]
           [test (with-handlers ([exn? 
                                  (lambda (e)
                                    (set! fail? #t)
                                    (list exception e "eval"))])
                   (test))]
           [result (with-handlers ([exn? 
                                    (lambda (e)
                                      (set! fail? #t)
                                      (list exception e "comp"))])
                     (and (not fail?)
                          (comp test act)))]
           [values-list (list act test meth result)])
      (if (in-check-mutate?)
          (stored-checks (cons (list (and (not fail?) result) 'check-by info values-list src) (stored-checks)))
          (report-check-result (and (not fail?) result) 'check-by info values-list src test-obj))
      (and (not fail?) result)))
  
  ;compare-rand: (-> val) (listof value) [list string] src object -> boolean
  (define (compare-rand test range info src test-obj)
    (let* ([fail? #f]
           [test-val (with-handlers ((exn?
                                      (lambda (e)
                                        (set! fail? #t)
                                        (list exception e))))
                       (test))]
           [expected-vals range]
           [result
            (and (not fail?)
                 (ormap (lambda (e-v) (java-equal? test-val e-v null null 0.001 #t))
                        expected-vals))]
           [res-list (list range test-val)])
      (if (in-check-mutate?)
       (stored-checks (cons (list (and (not fail?) result) 'check-rand info res-list src test-obj)
                            (stored-checks)))
       (report-check-result (and (not fail?) result) 'check-rand info res-list src test-obj))
      (and (not fail?) result)))

  ;check-mutate: (-> val) (-> boolean) (list string) src object -> boolean
  (define (check-mutate mutatee check info src test-obj)
    (mutatee)
    (parameterize ([in-check-mutate? #t] [stored-checks null])
      (let ([result-value (check)]
            [mutate-msg-prefix (string-append "check following the "
                                              (construct-info-msg info)
                                              " expected ")])
        (when test-obj
          (let report-results ([checks (stored-checks)])
            (unless (null? checks)
              (let ([current-check (first checks)])
                (send test-obj add-check)
                (unless (first current-check)
                  (send test-obj check-failed
                        (compose-message test-obj 
                                         (second current-check) 
                                         (third current-check)
                                         (fourth current-check)
                                         mutate-msg-prefix)
                        (fifth current-check) #f)))
              (report-results (cdr checks)))))
        result-value)))
  
  ;check-effects: (-> (listof val)) (-> (listof val)) (list string) src object -> boolean
  (define (check-effect tests checks info src test-obj)
    (let ([app (lambda (thunk) (thunk))])
      (for-each app tests)
      (andmap app checks)))
  
  (define (report-check-result res check-kind info values src test-obj)
    (when test-obj 
      (send test-obj add-check)
      (unless res 
        (send test-obj
              check-failed
              (compose-message test-obj check-kind info values #f)
              src #f))))

  (define (compose-message test-obj check-kind info values mutate-message)
    (letrec ([test-format (construct-info-msg info)]
             [eval-exception-raised? #f]
             [comp-exception-raised? #f]
             [exception-not-error? #f]
             [formatted-values (map (lambda (v) 
                                      (cond
                                        [(and (pair? v) (eq? (car v) exception))
                                         (if (equal? (cadddr v) "eval")
                                             (set! eval-exception-raised? #t)
                                             (set! comp-exception-raised? #t))
                                         (set! exception-not-error? (cadr v))
                                         (send test-obj format-value (caddr v))]
                                        [(pair? v)
                                         (map (lambda (v) (send test-obj format-value v)) v)]
                                        [else (send test-obj format-value v)])) values)]
             [expected-format
              (case check-kind
                ((check-expect check-by) "to produce ")
                ((check-rand) "to produce one of ")
                ((check-catch) "to throw an instance of "))])
      (cond 
        [(not (eq? 'check-by check-kind))
         (append (list (if mutate-message mutate-message "check expected ")
                       test-format
                       expected-format)
                 (if (eq? 'check-rand check-kind)
                     (list-format (first formatted-values))
                     (list (first formatted-values)))
                 (case check-kind
                   ((check-expect)
                    (append (if (= (length formatted-values) 3)
                                (list ", within " (third formatted-values))
                                null)
                            (cond
                              [(and eval-exception-raised? (not exception-not-error?))
                               (list ", instead a " (second formatted-values) " exception occurred")]
                              [(and eval-exception-raised? exception-not-error?)
                               (list", instead an error occurred")]
                              [else
                               (list ", instead found " (second formatted-values))])))
                   ((check-rand)
                    (cond
                      [(and eval-exception-raised? (not exception-not-error?))
                       (list ", instead a " (second formatted-values) " exception occurred")]
                      [(and eval-exception-raised? exception-not-error?)
                       (list", instead an error occurred")]
                      [else
                       (list ", instead found " (second formatted-values))]))
                   ((check-catch)
                    (if (= (length formatted-values) 1)
                        (list ", instead no exceptions occurred")
                        (list ", instead an instance of " (second formatted-values) " was thrown"))))
                 (list "."))]
        [(and (eq? check-kind 'check-by)
              comp-exception-raised?)
         (list "check encountered a " (fourth formatted-values)
               " exception when using " (third formatted-values)
               " to compare the actual value " (second formatted-values)
               " with the expected result " (first formatted-values) ".")]
        [(and (eq? check-kind 'check-by) eval-exception-raised?)
         (list "check expected a value to use in " (third formatted-values)
               " with argument " (first formatted-values)
               " instead, a " (second formatted-values)
               " exception occurred.")]
        [else
         (list "check received the value " (second formatted-values)
               " to compare to " (first formatted-values)
               " using " (third formatted-values)
               ". This value did not match the expectation.")])))
  
  (define (list-format l)
    (cond
      [(= (length l) 1) l]
      [(= (length l) 2) (list (car l) "or" (cadr l))]
      [else
       (letrec ([ins 
                 (lambda (l)
                   (cond
                     [(null? l) l]
                     [(null? (cdr l)) (list " or" (car l))]
                     [else 
                      (cons (car l) (cons "," (ins (cdr l))))]))])
         (ins l))]))

  ;construct-info-msg (list symbol string ...) -> string
  (define (construct-info-msg info)
    (case (first info)
      ((field) 
       (format "the ~a field of class ~a " (third info) (second info)))
      ((static-field)
       (format "the class field ~a of ~a " (third info) (second info)))
      ((var)
       (format "the local variable ~a " (second info)))
      ((alloc)
       (format "the instantiation of ~a, using values with types ~a, "
               (second info) (third info)))
      ((call) 
       (format "the call to method ~a of ~a, using values with types ~a, "
               (third info) (second info) (fourth info)))
      ((array) "the array value ")
      ((unary) (format "the unary operation ~a " (second info)))
      ((assignment) (format "the assignment of ~a" (construct-info-msg (cdr info))))
      ((value) "value ")))
  
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
