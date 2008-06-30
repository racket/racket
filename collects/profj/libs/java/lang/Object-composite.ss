(module Object-composite scheme/base
  
  (require scheme/class
           (prefix-in c: scheme/contract)
           errortrace/errortrace-lib
           
           profj/libs/java/lang/Comparable
           profj/libs/java/io/Serializable)
  #;(require "compile-lang-syntax.ss")  
  
;                                            
;         ;;         ;                       
;    ;;;   ;                            ;    
;   ;   ;  ;                            ;    
;   ;   ;  ;;;;   ;;;;    ;;;    ;;;   ;;;;; 
;   ;   ;  ;   ;     ;   ;   ;  ;   ;   ;    
;   ;   ;  ;   ;     ;   ;;;;;  ;       ;    
;   ;   ;  ;   ;     ;   ;      ;       ;    
;   ;   ;  ;   ;     ;   ;   ;  ;   ;   ;   ;
;    ;;;  ; ;;;      ;    ;;;    ;;;     ;;; 
;                    ;                       
;                    ;                       
;                 ;;;                        

  ;Object.java
  (provide ObjectI Object-Mix Object
           wrap-convert-assert-Object convert-assert-Object guard-convert-Object dynamic-Object/c static-Object/c)
  
  ;Object interface, and a mixin to create objects from.
  
  (define ObjectI
    (interface () Object-constructor clone equals-java.lang.Object finalize getClass
      hashCode notify notifyAll toString wait wait-long wait-long-int my-name
      equals hash-code to-string get-class))
  
  (define Object-Mix
    (lambda (parent)
      (class* parent (ObjectI)
        (inspect #f)
        (define/public (Object-constructor) (void))
        
        ;Needs to do something
        (define/public clone (lambda () void))
        
        (define/public (equals-java.lang.Object obj) 
          (or (eq? this obj)
              (and (is-a? obj wrapper)
                   (send obj compare this obj))))
        (define/public (equals obj) (send this equals-java.lang.Object obj))
        
        ;Needs to do something
        (define/public (finalize) void)
        
        (public-final getClass get-class)
        (define (getClass)
          (error 'ProfessorJ:getClass 
                 (format "ProfessorJ does not support getClass calls. ~e" 
                         (send this toString))))
        (define (get-class) (getClass))
        
        (define/public (hashCode) (eq-hash-code this))
        (define/public (hash-code) (send this hashCode))
        
        ;Needs to do something when Threads more implemented
        (public-final notify |notifyAll|)
        (define (notify) void)
        (define (notifyAll) void)
        
        (define/public (my-name) "Object")
        (define/public (toString)
          (make-java-string (format "~a@~a" (send this my-name) (send this hashCode))))
        (define/public (to-string) (send this toString))
        
        (public-final wait wait-long wait-long-int)
        (define wait (lambda () void))
        (define wait-long (lambda (l) void))
        (define wait-long-int (lambda (l i) void))

        (define/public (field-names) null)
        (define/public (field-values) null)
        
        (define/public (fields-for-display)
          (let ((field-name-list (send this field-names))
                (field-value-list (send this field-values)))
            (lambda ()
              (if (null? field-name-list) 
                  #f
                  (begin0 (list (car field-name-list) (car field-value-list))
                          (set! field-name-list (cdr field-name-list))
                          (set! field-value-list (cdr field-value-list)))))))
        
        (super-instantiate ()))))
  
  (define Object (Object-Mix object%))
  
  (define (wrap-convert-assert-Object obj p n s c)
    (if (string? obj)
        (make-java-string string)
        (begin
          (c:contract (object-contract
                       (clone (c:-> c:any/c))
                       (equals-java.lang.Object (c:-> c:any/c c:any/c))
                       (finalize (c:-> c:any/c))
                       (getClass (c:-> c:any/c))
                       (hashCode (c:-> c:any/c))
                       (notify (c:-> c:any/c))
                       (notifyAll (c:-> c:any/c))
                       (toString (c:-> c:any/c))
                       (wait (c:-> c:any/c))
                       (wait-long (c:-> c:any/c c:any/c))
                       (wait-long-int (c:-> c:any/c c:any/c c:any/c))) obj p n s)
          (make-object convert-assert-Object obj p n s c))))

  (define-local-member-name get-wrapped)
  (define wrapper (interface () get-wrapped))
  (provide wrapper)  
  
  (define convert-assert-Object
    (class* object% (wrapper)
      
      (init w)
      (init-field pos-blame neg-blame src cc-marks)
      
      (define wrapped null)
      (set! wrapped w)
      
      (define/public (get-wrapped) wrapped)
      (define/public (compare obj1 obj2)
        (cond
          ((and (is-a? obj1 wrapper) (is-a? obj2 wrapper))
           (compare (send obj1 get-wrapped) (send obj2 get-wrapped)))
          ((is-a? obj1 wrapper)
           (compare (send obj1 get-wrapped) obj2))
          ((is-a? obj2 wrapper)
           (compare obj1 (send obj2 get-wrapped)))
          (else (eq? obj1 obj2))))
      
      (define/public (down-cast class wrapped-class)
        (and (check-instance class)
             (make-object wrapped-class wrapped pos-blame neg-blame src cc-marks)))
      
      (define/public (check-instance class)
        (if (is-a? wrapped wrapper)
            (send wrapped check-instance class)
            (is-a? wrapped class)))
      
      (define/public (clone) (send wrapped clone))        
      (define/public (equals-java.lang.Object obj)
        (let ((val (send wrapped equals-java.lang.Object 
                         (make-object guard-convert-Object obj pos-blame neg-blame src cc-marks))))
          (unless (boolean? val)
            (raise (make-exn:fail (format "~a broke ~a contract here; Object's equals expects boolean return, given ~a"
                                          pos-blame neg-blame val)
                                  cc-marks)))
          val))
              
      (define/public (finalize) (send wrapped finalize))
      (define/public (getClass) (send wrapped getClass))

      (define/public (hashCode) 
        (let ((val (send wrapped hashCode)))
          (unless (integer? val)
            (raise (make-exn:fail
                    (format "~a broke ~a contract here; Object's hashCode expects int return, given ~a"
                            pos-blame neg-blame val)
                    cc-marks)))
          val))
      
      (define/public (notify) (send wrapped notify))
      (define/public (notifyAll) (send wrapped notifyAll))
      (define/public (toString)
        (let ((val (send wrapped toString)))
          (unless (string? val)
            (raise (make-exn:fail
                    (format "~a broke ~a contract here: Object's toString expects String return, given ~a"
                            pos-blame neg-blame val)
                    cc-marks)))
          (make-java-string val)))
      (define/public (wait) (send wrapped wait))
      (define/public (wait-long l) (send wrapped wait-long l))
      (define/public (wait-long-int l i) (send wrapped wait-long l i))
      (define/public (my-name) (send wrapped my-name))
      (define/public (field-names) (send wrapped field-names))
      (define/public (field-values) (send wrapped field-values))        
      (define/public (fields-for-display) (send wrapped fields-for-display))
      
      (public-final pos-blame* neg-blame* src* cc-marks*)
      (define (pos-blame*) (ca-pos-blame* this))
      (define (neg-blame*) (ca-neg-blame* this))
      (define (src*) (ca-src* this))
      (define (cc-marks*) (ca-cc-marks* this))
      
      (super-instantiate ())))
  
  (define ca-pos-blame* (class-field-accessor convert-assert-Object pos-blame))
  (define ca-neg-blame* (class-field-accessor convert-assert-Object neg-blame))
  (define ca-src* (class-field-accessor convert-assert-Object src))
  (define ca-cc-marks* (class-field-accessor convert-assert-Object cc-marks))
  
  (define dynamic-Object/c
    (c:flat-named-contract "Object" (lambda (v) (is-a? v convert-assert-Object))))
  
  (define guard-convert-Object
    (class* object% (wrapper)
      
      (init w)
      (init-field pos-blame neg-blame src cc-marks)
      
      (define wrapped null)
      (set! wrapped w)
      
      (define/public (get-wrapped) wrapped)
      
      (define/public (compare obj1 obj2)
        (cond
          ((and (is-a? obj1 wrapper) (is-a? obj2 wrapper))
           (compare (send obj1 get-wrapped) (send obj2 get-wrapped)))
          ((is-a? obj1 wrapper)
           (compare (send obj1 get-wrapped) obj2))
          ((is-a? obj2 wrapper)
           (compare obj1 (send obj2 get-wrapped)))
          (else (eq? obj1 obj2))))
      
      (define/public (down-cast class wrapped-class)
        (and (check-instance class)
             (make-object wrapped-class wrapped pos-blame neg-blame src cc-marks)))
      
      (define/public (check-instance class)
        (if (is-a? wrapped wrapper)
            (send wrapped check-instance class)
            (is-a? wrapped class)))
      
      (define/public (clone) (send wrapped clone))
      (define/public (equals-java.lang.Object . obj)
        (unless (= (length obj) 1)
          (raise (make-exn:fail:contract:arity
                  (format "~a broke ~a contract here: Object's equals expects to be called with 1 argument, given ~n"
                          pos-blame neg-blame (length obj))
                  cc-marks)))
        (send wrapped equals-java.lang.Object (wrap-convert-assert-Object (car obj) pos-blame neg-blame src cc-marks)))
      (define/public (equals . obj)
        (unless (= (length obj) 1)
          (raise (make-exn:fail:contract:arity
                  (format "~a broke ~a contract here: Object's equals expects to be called with 1 argument, given ~n"
                          pos-blame neg-blame (length obj))
                  cc-marks)))
        (send wrapped equals-java.lang.Object (wrap-convert-assert-Object (car obj) pos-blame neg-blame src cc-marks)))
      (define/public (finalize) (send wrapped finalize))
      (define/public (getClass) (send wrapped getClass))
      (define/public (get-class) (send wrapped getClass))
      (define/public (hashCode) (send wrapped hashCode))
      (define/public (hash-code) (send wrapped hashCode))
      (define/public (notify) (send wrapped notify))
      (define/public (notifyAll) (send wrapped notifyAll))
      (define/public (notify-all) (send wrapped notifyAll))        
      (define/public (toString)
        (send (send wrapped toString) get-mzscheme-string))
      (define/public (to-string) (send (send wrapped toString) get-mzscheme-string))
      (define/public (wait) (send wrapped wait))
      (define/public (wait-long . l)
        (unless (= (length l) 1)
          (raise (make-exn:fail:contract:arity
                  (format "~a broke ~a contract here: Object's wait-long expects to be called with 1 argument, given ~n"
                          pos-blame neg-blame (length l))
                  cc-marks)))
        (unless (integer? (car l))
          (raise (make-exn:fail
                  (format "~a broke ~a contract here: Object's wait that takes a long argument expected long, given ~a"
                          pos-blame neg-blame (car l))
                  cc-marks)))
        (send wrapped wait-long (car l)))
      (define/public (wait-long-int . l) 
        (unless (= (length l) 2)
          (raise (make-exn:fail:contract:arity
                  (format "~a broke ~a contract here: Object's wait-long-int expects to be called with 2 arguments, given ~n"
                          pos-blame neg-blame (length l))
                  cc-marks)))
        (unless (integer? (car l))
          (raise (make-exn:fail
                  (format "~a broke ~a contract here: Object's wait-long-int expected long, given ~a"
                          pos-blame neg-blame (car l))
                  cc-marks)))
        (unless (integer? (cadr l))
          (raise (make-exn:fail
                  (format "~a broke ~a contract here: Object's wait-long-int expected int, given ~a"
                          pos-blame neg-blame (cadr l))
                  cc-marks)))
        (send wrapped wait-long (car l) (cadr l)))
      (define/public (my-name) (send wrapped my-name))
      (define/public (field-names) (send wrapped field-names))
      (define/public (field-values) (send wrapped field-values))
      (define/public (fields-for-display) (send wrapped fields-for-display))
      (define/public (get-pos) (gc-pos-blame* this))
      
      (public-final pos-blame* neg-blame* src* cc-marks*)
      (define (pos-blame*) (gc-pos-blame* this))
      (define (neg-blame*) (gc-neg-blame* this))
      (define (src*) (gc-src* this))
      (define (cc-marks*) (gc-cc-marks* this))
      
      (super-instantiate ())))
  
  (define gc-pos-blame* (class-field-accessor guard-convert-Object pos-blame))
  (define gc-neg-blame* (class-field-accessor guard-convert-Object neg-blame))
  (define gc-src* (class-field-accessor guard-convert-Object src))
  (define gc-cc-marks* (class-field-accessor guard-convert-Object cc-marks))
  
  (define static-Object/c
    (c:flat-named-contract "Object" (lambda (v) (is-a? v guard-convert-Object))))  
      
;                                     
;                                     
;   ;;;                               
;     ;                               
;    ; ;   ; ;;;  ; ;;;  ;;;;  ;;; ;;;
;    ; ;    ;      ;         ;  ;   ; 
;   ;;;;;   ;      ;      ;;;;  ;   ; 
;   ;   ;   ;      ;     ;   ;   ; ;  
;   ;   ;   ;      ;     ;   ;   ;;;  
;  ;;; ;;; ;;;;   ;;;;    ;;; ;   ;   
;                                 ;   
;                                 ;   
;                               ;;    

  ;;array implementation:
  (provide make-java-array is-java-array? make-runtime-type array->list)
  
  (define java-array
    (class* Object ()
      (inspect #f)
      (define array null)
      (define rt #f)
      (define/public (get-rt) rt)
      
      (define/private (check-runtime-type val)
        (if (<= (runtime-type-dim rt) 1)
            (if (symbol? (runtime-type-type rt))
                (case (runtime-type-type rt)
                  ((byte short int long) (and (number? val) (not (inexact? val))))
                  ((char) (char? val))
                  ((float double) (number? val) #;(and (number? val) (inexact? val))))
                (is-a? val (runtime-type-type rt)))
            (and
             (is-a? val java-array)
             (= (sub1 (runtime-type-dim rt))
                (runtime-type-dim (send val get-rt)))
             (eq? (runtime-type-type rt) (runtime-type-type (send val get-rt))))))
      
      (define/public (check-prim-type type dim)
        (and (eq? (runtime-type-type rt) type)
             #;(= dim (runtime-type-dim rt))))
      (define/public (check-ref-type type dim)
        (and (eq? (runtime-type-type rt) type)
             #;(= dim (runtime-type-dim rt))))
      
      (define/private (default-val) 
        (if (and (= 1(runtime-type-dim rt)) (symbol? (runtime-type-type rt)))
            (case (runtime-type-type rt)
              ((byte short int long float double) 0)
              ((char) #\null)
              ((boolean) #f))
            null))

      (define/public (length) (vector-length array))
      
      (define/private (array-out-of-bounds i)
        (raise (create-java-exception ArrayIndexOutOfBoundsException
                                      (format "Array index out of bounds. Range is 0 to ~a, given ~a"
                                              (sub1 (vector-length array)) i)
                                      (lambda (obj msg)
                                        (send obj ArrayIndexOutOfBoundsException-constructor-java.lang.String msg))
                                      (current-continuation-marks))))
      
      (define/public (access index) 
        (when (or (< index 0) (>= index (length)))
          (array-out-of-bounds index))
        (vector-ref array index))
      
      (define/public (set index val)
        (when (or (< index 0) (>= index (length)))
          (array-out-of-bounds index))
        (if (check-runtime-type val)
            (vector-set! array index val)
            (raise (create-java-exception ArrayStoreException
                                          "Array given incompatible type"
                                          (lambda (obj msg) 
                                            (send obj ArrayStoreException-constructor-java.lang.String msg))
                                          (current-continuation-marks)))))
                                        
      (define/public (make-uninitialized size type)
        (when (< size 0)
          (raise (create-java-exception NegativeArraySizeException
                                        (format "Size for the array must be greater than 0, given ~a" size)
                                        (lambda (obj msg)
                                          (send obj NegativeArraySizeException-constructor-java.lang.String msg))
                                        (current-continuation-marks))))
        (set! rt type)
        (set! array (make-vector size (default-val))))
      
      (define/public (make-initialized type vals)
        (set! rt type)
        (set! array (list->vector vals)))
      
      (define/public (make-multi-dimension type size default-val)
        (set! rt type)
        (set! array (make-vector size default-val)))
      
      (define/public (make-uninit-multi type sizes)
        (set! rt type)
        (set! array 
              (if (null? (cdr sizes))
                  (make-vector (car sizes) (default-val))
                  (let ((vec (make-vector (car sizes))))
                    (let loop ((idx 0))
                      (unless (>= idx (car sizes))
                        (vector-set! vec idx (make-java-array (make-runtime-type (runtime-type-type type)
                                                                                 (sub1 (runtime-type-dim type)))
                                                              (cdr sizes) null))
                        (loop (add1 idx))))
                    vec))))
      
      (define/override (my-name) "array")
      
      (super-instantiate ())))
  
  (define (make-java-array type size vals)
    (let ((array (make-object java-array)))
      (cond
        ((list? size) (send array make-uninit-multi type size))
        ((null? vals) (send array make-uninitialized size type))
        ((list? vals) (send array make-initialized type vals))
        (else (send array make-multi-dimension type size vals)))
      array))
  
  (define (is-java-array? obj) (is-a? obj java-array))
  
  (define-struct runtime-type (type dim) #:transparent)
  
  (define (array->list array start stop)
    (if (= start stop)
        null
        (cons (send array access start)
              (array->list array (add1 start) stop))))
  
  
;                                            
;                          ;                 
;    ;;;;   ;                                
;   ;   ;   ;                                
;   ;      ;;;;;  ; ;;;  ;;;   ; ;;;    ;;; ;
;   ;;;     ;      ;       ;    ;;  ;  ;   ; 
;     ;;;   ;      ;       ;    ;   ;  ;   ; 
;       ;   ;      ;       ;    ;   ;  ;   ; 
;   ;   ;   ;   ;  ;       ;    ;   ;  ;   ; 
;   ;;;;     ;;;  ;;;;   ;;;;; ;;;  ;;  ;;;; 
;                                          ; 
;                                          ; 
;                                       ;;;  

  ;;String.java
  (provide make-java-string String String-valueOf-java.lang.Object String-valueOf-char1 
           String-valueOf-char1-int-int String-copyValueOf-char1-int-int String-copyValueOf-char1
           String-valueOf-boolean String-valueOf-char String-valueOf-int String-valueOf-long 
           String-valueOf-float String-valueOf-double)
  
  (define (make-java-string s)
    (let ((obj (make-object String)))
      (send obj make-mzscheme-string s)
      obj))
  
  (define String
    (class* Object (Comparable Serializable)

      (inspect #f)
      ;private field containing scheme string
      (define text "")
      ;Accessor for scheme string
      (define/public (get-mzscheme-string) text)
      
      ;Constructors
      (define/public (String-constructor) (send this Object-constructor))      

      (define/public (String-constructor-java.lang.String string)
        (send this Object-constructor)
        (set! text (send string get-mzscheme-string)))    
      
      (define/public (String-constructor-char1 chars)
        (send this Object-constructor)
        (set! text (list->string (array->list chars 0 (send chars length)))))
    
      (define/public (String-constructor-char1-int-int chars offset count)
        (send this Object-constructor)
        (set! text (list->string (array->list chars offset count))))
    
      ;Does not take into account char-set: PROBLEM
      (define/public (String-constructor-byte1-int-int-java.lang.String ascii offset len char-set)
        (send this Object-constructor)
        (set! text (list->string (map integer->char (array->list ascii offset len)))))
      (define/public (|String-constructor-byte1-java.lang.String| ascii char-set)
        (send this Object-constructor)
        (set! text (list->string (map integer->char (array->list ascii 0 (send ascii length))))))
    
      ;currently broken until I figure out how to deal appropriately with hi bytes
      (define/public (String-constructor-byte1-int-int-int ascii hi offset count)
        (send this Object-constructor))
      (define/public (String-constructor-byte1-int ascii hi)
        (send this Object-constructor))
    
      (define/public (String-constructor-StringBuffer buffer)
        (send this Object-constructor)
        (set! text (substring (send (send buffer toString) get-mzscheme-string)
                              0 
                              (send buffer length))))
    
      ;Constructor to use when a string is constructed by ""
      (define/public (make-mzscheme-string str)
        (send this Object-constructor)
        (set! text str))
      
      (define/override (toString) this)
    
      ; -> int
      (define/public (length) (string-length text))
      ; int -> char
      (define/public  (charAt-int index) (string-ref text index))
      (define/public (char-at i) (charAt-int i))
    
      ;-> void
      (define/public (getChars-int-int-char1-int begin end dest i)
        (letrec ((build-char-array
                  (lambda (offset index)
                    (if (= offset end)
                        (void)
                        (begin
                          (send dest set index (string-ref text offset))
                          (build-char-array (add1 offset) (add1 index)))))))
          (build-char-array begin i)))
      (define/public (get-chars b e d i) (getChars-int-int-char1-int b e d i))
    
      ;Does not mess with charset
      (define/public (getBytes)
        (letrec ((array (make-java-array (make-runtime-type 'byte) (length) null))
                 (build-byte-array
                  (lambda (index)
                    (if (= index (length))
                        array
                        (begin
                          (send array set index (char->integer (string-ref text index)))
                          (build-byte-array (add1 index)))))))
          (build-byte-array 0)))
      (define/public (getBytes-java.lang.String charset)
        (getBytes))
    
      (define/public (getBytes-int-int-byte1-int begin end dest i)
        (letrec ((build-byte-array
                  (lambda (offset index)
                    (if (= offset end)
                        (void)
                        (begin
                          (send dest set index (char->integer (string-ref text offset)))
                          (build-byte-array (add1 offset) (add1 index)))))))
          (build-byte-array begin i)))

      (define/public (contentEquals-java.lang.StringBuffer buf)
        (equals-java.lang.Object (send buf toString)))
      (define/public (content-equals b) (contentEquals-java.lang.StringBuffer b))
    
      ;Object -> boolean
      (define/override (equals-java.lang.Object obj)
        (and (is-a? obj String)
             (equal? text (send (send obj toString) get-mzscheme-string))))
      
      ;Object -> boolean
      (define/public (equalsIgnoreCase-java.lang.String str)
        (string-ci=? text (send str get-mzscheme-string)))
      (define/public (equals-ignore-case s) (equalsIgnoreCase-java.lang.String s))

      ;find-diff-chars: int int string-> (values int int)
      (define/private (find-diff-chars i stop-length compare-string)
        (if (>= i stop-length)
            (values #f #f)
            (if (not (equal? (string-ref text i) (string-ref compare-string i)))
                (values (char->integer (string-ref text i)) (char->integer (string-ref compare-string i)))
                (find-diff-chars (add1 i) stop-length compare-string))))
      
      ;min: int int -> int
      (define/private (min x y)
        (cond
          ((= x y) x)
          ((< x y) x)
          (else y)))
      
      ;String -> int
      (define/public (compareTo-java.lang.String str)
        (let* ((string (send str get-mzscheme-string))
               (text-l (string-length text))
               (str-l (string-length string)))
          (cond
            ((equals-java.lang.Object str) 0)
            (else
             (let-values (((int-text int-str) (find-diff-chars 0 (min text-l str-l) string)))
               (if int-text
                   (- int-text int-str)
                   (- text-l str-l)))))))
  
      ;Object -> int: Throws ClassCastException
      (define/public (compareTo-java.lang.Object obj)
        (if (is-a? obj String)
            (compareTo-java.lang.String obj)
            (raise (create-java-exception ClassCastException
                                          (format "ClassCastException: Expected argument of class String given ~a"
                                                  (send (send obj toString) get-mzscheme-string))
                                          (lambda (obj msg) (send obj ClassCastException-constructor-String msg))
                                          (current-continuation-marks)))))
      
      ;String -> int
      (define/public (compareToIgnoreCase-java.lang.String str)
        (letrec ((string (send str get-mzscheme-string))
                 (find-diff-chars
                  (lambda (i)
                    (if (>= i (length text))
                        (error 'comparetostring "Opps, internal error")
                        (if (not (char-ci=? (string-ref text i) (string-ref string i)))
                            (values (char->integer (string-ref text i)) (char->integer (string-ref string i)))
                            (find-diff-chars (add1 i))))))
                 (text-l (string-length text))
                 (str-l (string-length string)))
          (cond
            ((equalsIgnoreCase-java.lang.String str) 0)
            ((string-ci<? text string) 
             (if (= text-l str-l)
                 (let-values (((int-text int-str) (find-diff-chars 0)))
                   (- int-text int-str))
                 (- text-l str-l)))                               
            ((string-ci>? text string) 
             (if (= text-l str-l)
                 (let-values (((int-text int-str) (find-diff-chars 0)))
                   (- int-text int-str))
                 (- text-l str-l))))))
      (define/public (compare->ignore-case s) (compareToIgnoreCase-java.lang.String s))
    
      ;int String int int -> boolean
      (define/public (regionMatches-int-java.lang.String-int-int toffset jstr ooffset len)
        (let ((str (send jstr get-mzscheme-string)))
          (and (not (negative? toffset))
               (not (negative? ooffset))
               (<= (+ toffset len) (string-length text))
               (<= (+ ooffset len) (string-length str))
               (string=? (substring text toffset (+ toffset len)) 
                         (substring str ooffset (+ ooffset len))))))
    
      ;.... -> boolean
      (define/public (regionMatches-boolean-int-java.lang.String-int-int case? toffset jstr ooffset len)
        (let ((str (send jstr get-mzscheme-string)))
          (and (not (negative? toffset))
               (not (negative? ooffset))
               (<= (+ toffset len) (string-length text))
               (<= (+ ooffset len) (string-length str))
               ((if case? string=? string-ci=?) (substring text toffset (+ toffset len)) 
                (substring str ooffset (+ ooffset len))))))
    
      ; .... -> boolean
      (define/public (startsWith-java.lang.String-int Jprefix offset)
        (let ((prefix (send Jprefix get-mzscheme-string)))
          (and (not (negative? offset))
               (<= (+ offset (string-length prefix)) (string-length text))
               (string=? prefix (substring text offset (+ offset (string-length prefix)))))))
    
      ;..... -> boolean
      (define/public (startsWith-java.lang.String Jprefix)
        (let ((prefix (send Jprefix get-mzscheme-string)))
          (and (<= (string-length prefix) (string-length text))
               (string=? prefix (substring text 0 (string-length prefix))))))     
          
      (define/public (endsWith-java.lang.String Jsuffix)
        (let ((suffix (send Jsuffix get-mzscheme-string)))
          (and (<= (string-length suffix) (string-length text))
               (string=? suffix (substring text (- (string-length text) (string-length suffix)) (string-length text))))))
      (define/public (ends-with s) (endsWith-java.lang.String s))
      
      ; -> int
      (define/override (hashCode)
        (let ((hash 0))
          (let loop ([index 0])
            (unless (>= index (string-length text))
              (set! hash (+ hash (* (char->integer (string-ref text index)) 
                                    (expt 31 (- (string-length text) (add1 index))))))
              (loop (add1 index))))
          hash))
    
    ; character int -> int
    (define/private (find-char ch pos)
      (if (>= pos (string-length text))
          -1
          (if (char=? ch (string-ref text pos))
              pos
              (find-char ch (add1 pos)))))
      
      ;character int int -> int
      (define/private (find-last-char ch pos lpos)
        (if (>= pos (string-length text))
            lpos
            (if (char=? ch (string-ref text pos))
                (find-last-char ch (add1 pos) pos)
                (find-last-char ch (add1 pos) lpos))))
      
      ; string int -> int
      (define/private (find-str sch-str str pos)
        (if (> (+ pos (string-length sch-str)) (string-length text))
            -1
            (if (startsWith-java.lang.String-int str pos)
                pos
                (find-str sch-str str (add1 pos)))))
    
      ; string int int -> int
      (define/private (find-last-string sch-str str pos lpos)
        (if (> (+ pos (string-length sch-str)) (string-length text))
            lpos
            (if (startsWith-java.lang.String-int str pos)
                (find-last-string sch-str str (add1 pos) pos)
                (find-last-string sch-str str (add1 pos) lpos))))
    
      (define/public (indexOf-int ch) (find-char (if (number? ch) (integer->char ch) ch) 0))
      (define/public (indexOf-int-int ch offset) (find-char (if (number? ch) (integer->char ch) ch) offset))
      (define/public (indexOf-java.lang.String str) (find-str (send str get-mzscheme-string) str 0))    
      (define/public (indexOf-java.lang.String-int str offset) (find-str (send str get-mzscheme-string) str offset))
      
      (define/public (lastIndexOf-int ch) 
        (find-last-char (if (number? ch) (integer->char ch) ch) 0 -1))
      (define/public (lastIndexOf-int-int ch offset) 
        (find-last-char (if (number? ch) (integer->char ch) ch) offset -1))
      (define/public (lastIndexOf-java.lang.String str) (find-last-string (send str get-mzscheme-string) str 0 -1))
      (define/public (lastIndexOf-java.lang.String-int str offset) (find-last-string (send str get-mzscheme-string) str offset -1))
    
      ;int -> String
      (define/public (substring-int index) 
        (substring-int-int index (sub1 (string-length text))))
    
      ;... -> String
      (define/public (substring-int-int begin end)
        (when (< begin 0) 
          (raise (make-runtime-error 
                  (format "First argument to substring must be greater than 0, given ~a." begin))))
        (when (>= begin (string-length text))
          (raise (make-runtime-error
                  (format "First argument to substring must be smaller than the string's length ~a, given ~a." (string-length text) begin))))
        (when (> end (string-length text))
          (raise (make-runtime-error
                  (format "Second argument to substring must be smaller than the string's length ~a, given ~a." (string-length text) end))))
        (when (< end 0)
          (raise (make-runtime-error
                  (format "Second argument to substring must be greater than 0, given ~a." end))))
        (when (> begin end)
          (raise (make-runtime-error
                  (format "First argument to substring must be less than the second, given ~a and ~a." begin end))))
        (make-java-string (substring text begin end)))
    
      (define/public (subSequence-int-int begin end)
        (error 'subSequence "Internal Error: subsequence is unimplemented because charSequence is unimplemented"))
      (define/public (sub-sequence i j) (subSequence-int-int i j))
      
      ;String -> String
      (define/public (concat-java.lang.String Jstr)
        (let ((str (send Jstr get-mzscheme-string)))
          (make-java-string (string-append text str))))
      (define/public (concat s) (concat-java.lang.String s))
    
      ; .. -> String
      (define/public (replace-char-char old new)
        (let ((new-text (apply string-append (map string (string->list text)))))
          (let loop ([index 0])
            (let ((pos (find-char old index)))
              (unless (= -1 pos)
                (string-set! new-text pos new)
                (loop (add1 index)))))
          (make-java-string new-text)))
      (define/public (replace c1 c2) (replace-char-char c1 c2))
    
      ;Does not currently work. Needs to replace regex in text with replace and return new string; PROBLEM
      (define/public (replaceAll-java.lang.String-java.lang.String regex replace)
        (error 'replaceAll "Internal error: replaceAll is unimplemented at this time"))
      (define/public (replace-all s s2) (replaceAll-java.lang.String-java.lang.String s s2))
    
      (define/public (replaceFirst-java.lang.String-java.lang.String regex replace)
        (error 'replaceFirst "Internal error: replaceFirst is unimplemented at this time"))
      (define/public (replace-first s s2) (replaceFirst-java.lang.String-java.lang.String s s2))
    
      (define/public (matches-java.lang.String regex)
        (error 'matches "Internal error: matches is unimplemented at this time"))
      (define/public (matches s) (matches-java.lang.String s))
    
      (define/public (split-java.lang.String-int regex limit)
        (error 'split "Internal error: split is unimplemented at this time"))
      
      (define/public (split-java.lang.String regex)
        (error 'split "Internal error: split is unimplemented at this time"))
    
      ; -> String
      (define/public (toLowerCase)
        (make-java-string (apply string-append (map string (map char-downcase (string->list text))))))
    
      ;Does not take Locale into account
      (define/public (toLowerCase-java.util.Locale locale)
        (toLowerCase))
    
      (define/public (toUpperCase)
        (make-java-string (apply string-append (map string (map char-upcase (string->list text))))))
    
      ;Does not take Locale into account: Problem
      (define/public (toUpperCase-java.util.Locale locale)
        (toUpperCase))
    
      ;... -> String
      (define/public (trim)
        (error 'trim "Internal error: trim is unimplemented at this time."))
      
      (define/public (toCharArray) (make-java-array 'char 0 (string->list text)))
      (define/public (to-char-array) (toCharArray))
    
      ;PROBLEM I am not sure what the side effects of this should be in context! PROBLEM!
      (define/public intern  
        (lambda () this))
      
      (define/override (my-name) "String")      
      (super-instantiate ())))
  
  ;valueOf -> String
  (define (String-valueOf-java.lang.Object obj)
    (if (null? obj)
        (make-java-string "null")
        (send obj |toString|)))
  (define (String-valueOf-char1 data) 
    (make-java-string (list->string (array->list data 0 (send data length)))))

  ;Should throw exceptions
  (define (String-valueOf-char1-int-int data offset len)
    (make-java-string (list->string (array->list data offset len))))

  (define (String-valueOf-boolean b) (make-java-string (if b "true" "false")))
  (define (String-valueOf-char c) (make-java-string (string c)))
  (define (String-valueOf-int i) (make-java-string (number->string i)))
  (define (String-valueOf-long l) (make-java-string (number->string l)))
  (define (String-valueOf-float f) (make-java-string (number->string f)))
  (define (String-valueOf-double d) (make-java-string (number->string d)))
  
  ;copyValueOf -> String
  (define (String-copyValueOf-char1-int-int data offset count)
    (String-valueOf-char1-int-int data offset count))
  (define (String-copyValueOf-char1 data) (String-valueOf-char1 data))
  
  ;Comparator
  (define String-CASE_INSENSITIVE_ORDER null)
  
  
;                                                                 
;         ;;                                 ;;      ;;;          
;  ;;;;;;; ;                                  ;        ;          
;  ;  ;  ; ;                                  ;        ;          
;  ;  ;  ; ; ;;   ; ;;;   ;;;  ;;; ;;; ;;;;   ;;;;     ;     ;;;  
;     ;    ;;  ;   ;     ;   ;  ;   ;      ;  ;   ;    ;    ;   ; 
;     ;    ;   ;   ;     ;   ;  ; ; ;   ;;;;  ;   ;    ;    ;;;;; 
;     ;    ;   ;   ;     ;   ;  ; ; ;  ;   ;  ;   ;    ;    ;     
;     ;    ;   ;   ;     ;   ;   ; ;   ;   ;  ;   ;    ;    ;   ; 
;   ;;;;; ;;; ;;; ;;;;    ;;;    ; ;    ;;; ;; ;;;   ;;;;;;  ;;;  
;                                                                 
;                                                                 
;                                                                 
;Throwable and exceptions

  (provide Throwable (struct-out java:exception)
           exception-is-a? handle-exception create-java-exception)
  
  (define Throwable
    (class* Object (Serializable)
      
      (inspect #f)
      ;private fields
      ;message: String
      (define message (make-java-string ""))
      ;stack: continuation-mark-set
      (define stack null)
      ;java:exception
      (define exception null)
      ;cause: Throwable
      (define cause null)
      
      ;Constructors, set the stack and message
      (define/public (Throwable-constructor)
        (set! stack (current-continuation-marks))
        (send this Object-constructor))
                
      (define/public (Throwable-constructor-java.lang.String msg)
        (set! message msg)
        (set! stack (current-continuation-marks))
        (send this Object-constructor))
      
      (define/public (Throwable-constructor-java.lang.String-java.lang.Throwable msg cse)
        (set! message msg)
        (set! cause cse)
        (set! stack (current-continuation-marks))
        (send this Object-constructor))
      
      (define/public (Throwable-constructor-java.lang.Throwable cse)
        (set! message (if (null? cse) null (send cse toString)))
        (set! cause cse)
        (set! stack (current-continuation-marks))
        (send this Object-constructor))    
      
      (public-final set-exception! get-mzscheme-exception)
      ;Used to interoperate with mzscheme exceptions: set and get the current exception
      (define (set-exception! exn)
        (set! exception exn)
        (set! stack (exn-continuation-marks exn)))
      (define (get-mzscheme-exception) exception)

      ;Needs to throw exceptions. Needs to be callable only once per object
      (define/public (initCause-java.lang.Throwable cse)
        (set! cause cse)
        this)
      
      ; -> String
      (define/public (getMessage) message)
      (define/public (getCause) cause)
      (define/public (getLocalizedMessage) (send this getMessage))
      (define/public (get-message) (send this getMessage))
      (define/public (get-cause) (send this getCause))
      (define/public (get-localized-message) (send this getLocalizedMessage))
      
      (define/public (setStackTrace-java.lang.StackTraceElement1 elments)
        (error 'setStackTrace "Internal error: setStackTrace will not be implemented until strack trace element s implemented"))
      (define/public (getStackTrace)
        (error 'getStackTrace "Internal error: getStackTrace will not be implemented until StackTraceElement is implemented"))
      (define/public (set-stack-trace e) (send this setStackTrace-java.lang.StackTraceElement1 e))
      (define/public (get-stack-trace) (send this getStackTrace))
      
       ; -> string
      (define/override (toString)
        (if (null? message)
            (make-java-string (send this my-name))
            (make-java-string (format "~a: ~a" 
                                      (send this my-name) 
                                      (send (send this getMessage) get-mzscheme-string)))))

      ; -> void
      (define/public (printStackTrace)
        (print-error-trace (current-output-port) (make-exn message stack)))

      ;These functions do not work correctly yet, and won't until printStreams are implemented
      (define/public printStackTrace-PrintStream (lambda (printStream) void)) 
      (define/public printStackTrace-PrintWriter (lambda (pW) void))
      
      ;This function does nothing at this time
      (define/public (fillInStackTrace) this)
      (define/public (fill-in-stack-trace) (send this fillInStackTrace))
      
      ; -> string
      (define/override (my-name) "Throwable")
      
      (define/override (field-names)
        (cond
          [(and (null? cause) 
                (equal? "" (send message get-mzscheme-string))) null]
          [(null? cause) (list "message")]
          [(equal? "" (send message get-mzscheme-string)) (list "cause")]
          [else (list "message" "cause")]))
      (define/override (field-values)
        (cond
          [(and (null? cause)
                (equal? "" (send message get-mzscheme-string))) null]
          [(null? cause) (list message)]
          [(equal? "" (send message get-mzscheme-string)) (list cause)]
          [else (list message cause)]))
      
      (super-instantiate ())))
  
  ;(make-java-exception string continuation-mark-set Throwable)
  ;Where Throwable is an object descending from class Throwable
  (define-struct (java:exception exn) (object) #:mutable)

  ;exception-is-a?: class -> (exn -> bool)
  (define (exception-is-a? class)
    (lambda (exn)
      (is-a? (java:exception-object exn) class)))

  (define (handle-exception actions)
    (lambda (exn)
      (actions (java:exception-object exn))))
  
  ;PROBLEM: create java exception misused by String implementation
  (define (create-java-exception class msg constructor marks)
    (let* ((exn (make-object class))
           (str (make-java-string msg))
           (scheme-exn (make-java:exception msg marks exn)))
      (constructor exn str)
      (send exn set-exception! scheme-exn)
      scheme-exn))

  (define (make-runtime-error t)
    (create-java-exception
     RuntimeException t
     (lambda (exn str)
       (send exn RuntimeException-constructor-java.lang.String
             (make-java-string str)))
     (current-continuation-marks)))
  
  (provide convert-assert-Throwable wrap-convert-assert-Throwable dynamic-Throwable/c 
           guard-convert-Throwable static-Throwable/c)
  
  (define (wrap-convert-assert-Throwable obj p n s c)
    (c:contract (object-contract
                 (init-cause (c:-> c:any/c c:any/c))
                 (get-message (c:-> c:any/c))
                 (get-cause (c:-> c:any/c))
                 (get-localized-message (c:-> c:any/c))
                 (setStackTrace-java.lang.StackTraceElement1 (c:-> c:any/c c:any/c))
                 (get-stack-trace (c:-> c:any/c))
                 (printStackTrace (c:-> c:any/c))
                 (printStackTrace-PrintStream (c:-> c:any/c c:any/c))
                 (printStackTrace-PrintWriter (c:-> c:any/c c:any/c))
                 (fill-in-stack-trace (c:-> c:any/c))
                 (clone (c:-> c:any/c))
                 (equals-java.lang.Object (c:-> c:any/c c:any/c))
                 (finalize (c:-> c:any/c))
                 (get-class (c:-> c:any/c))
                 (hash-code (c:-> c:any/c))
                 (notify (c:-> c:any/c))
                 (notify-all (c:-> c:any/c))
                 (to-string (c:-> c:any/c))
                 (wait (c:-> c:any/c))
                 (wait-long (c:-> c:any/c c:any/c))
                 (wait-long-int (c:-> c:any/c c:any/c c:any/c))) obj p n s)
    (make-object convert-assert-Throwable obj p n s c))
  
  (define convert-assert-Throwable
    (class convert-assert-Object

      (init w p n s c)
      (super-instantiate (w p n s c))
      
      (define wrapped null)
      (set! wrapped w)
      
      (define/public (set-exception! exn) (send wrapped set-exception! exn))
      (define/public (get-mzscheme-exception) (send wrapped get-mzscheme-exception))
      (define/public (initCause-java.lang.Throwable cse)
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (wrap-convert-assert-Throwable
           (send wrapped initCause-java.lang.Throwable 
                 (make-object guard-convert-Throwable cse pb nb sr cc))
           pb nb sr cc)))
      (define/public (getMessage) 
        (let ([val (send wrapped getMessage)]
              [pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (if (string? val)
              (make-java-string val)
              (raise 
               (make-exn:fail
                (format "~a broke ~a contract here; Throwable's getMessage expects string return, given ~a"
                        pb nb val)
                cc)))))
      (define/public (getCause)        
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
        (wrap-convert-assert-Throwable (send wrapped getCause) pb nb sr cc)))
      (define/public (getLocalizedMessage)
        (let ([val (send wrapped getLocalizedMessage)]
              [pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (if (string? val)
              (make-java-string val)
              (raise (make-exn:fail
                      (format "~a broke ~a contract here; Throwable's getLocalizedMessage expects string return, given ~a"
                              pb nb val)
                      cc)))))
      (define/public (setStackTrace-java.lang.StackTraceElement1 elements)
        (send wrapped setStackTrace-java.lang.StackTraceElement1 elements))
      (define/public (getStackTrace) (send wrapped getStackTrace))
      (define/public (printStackTrace)  (send wrapped printStackTrace))
      (define/public (printStackTrace-PrintStream printStream) (send wrapped printStackTrace-PrintStream)) 
      (define/public (printStackTrace-PrintWriter pW) (send wrapped printStackTrace-PrintWriter))
      (define/public (fillInStackTrace) (send wrapped fillInStackTrace))      
      ))
  
  (define dynamic-Throwable/c
    (c:flat-named-contract "Throwable" (lambda (v) (is-a? v convert-assert-Throwable))))
     
  (define guard-convert-Throwable
    (class guard-convert-Object

      (init w p n s c)
      (super-instantiate (w p n s c))
      
      (define wrapped null)
      (set! wrapped w)

      (define/public (set-exception! exn) (send wrapped set-exception! exn))
      (define/public (get-mzscheme-exception) (send wrapped get-mzscheme-exception))
      (define/public (initCause-java.lang.Throwable . cse)
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])          
        (unless (= 1 (length cse))
          (raise (make-exn:fail:contract:arity
                  (format "~a broke ~a contract here: Throwable's initCause expects to be called with 1 argument, given ~n"
                          pb nb (length cse))
                  cc)))
        (make-object guard-convert-Throwable
          (send wrapped initCause-java.lang.Throwable 
                (wrap-convert-assert-Throwable (car cse) pb nb sr cc)))))
      (define/public (init-cause . cse)
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (unless (= 1 (length cse))
            (raise (make-exn:fail:contract:arity
                    (format "~a broke ~a contract here: Throwable's initCause expects to be called with 1 argument, given ~n"
                            pb nb (length cse))
                  cc)))
          (make-object guard-convert-Throwable
            (send wrapped initCause-java.lang.Throwable 
                  (wrap-convert-assert-Throwable (car cse) pb nb sr cc))
            pb nb sr cc)))
      (define/public (getMessage) (send (send wrapped getMessage) get-mzscheme-string))
      (define/public (get-message) (send (send wrapped getMessage) get-mzscheme-string))
      (define/public (getCause) 
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (make-object guard-convert-Throwable (send wrapped getCause) pb nb sr cc)))
      (define/public (get-cause) 
        (let ([pb (send this pos-blame*)]
              [nb (send this neg-blame*)]
              [sr (send this src*)]
              [cc (send this cc-marks*)])
          (make-object guard-convert-Throwable (send wrapped getCause) pb nb sr cc)))
      (define/public (getLocalizedMessage) (send (send wrapped getLocalizedMessage) get-mzscheme-string))
      (define/public (get-localized-message) (send (send wrapped getLocalizedMessage) get-mzscheme-string))
      (define/public (setStackTrace-java.lang.StackTraceElement1 elements)
        (send wrapped setStackTrace-java.lang.StackTraceElement1 elements))
      (define/public (set-stack-trace t) 
        (send wrapped setStackTrace-java.lang.StackTraceElement1 t))
      (define/public (getStackTrace) (send wrapped getStackTrace))
      (define/public (get-stack-trace) (send wrapped getStackTrace))
      (define/public (printStackTrace)  (send wrapped printStackTrace))
      (define/public (printStackTrace-PrintStream printStream) (send wrapped printStackTrace-PrintStream printStream))
      (define/public (printStackTrace-PrintWriter pW) (send wrapped printStackTrace-PrintWriter pW))
      (define/public (fillInStackTrace) (send wrapped fillInStackTrace))
      (define/public (fill-in-stack-trace) (send wrapped fillInStackTrace))
      
      ))
  
  (define static-Throwable/c
    (c:flat-named-contract "Throwable" (lambda (v) (is-a? v guard-convert-Throwable))))  
  
  (provide wrap-convert-assert-Class guard-convert-Class wrap-convert-assert-PrintString wrap-convert-assert-PrintWriter)
  
  (define (wrap-convert-assert-Class . args) (void))
  (define guard-convert-Class (class object% (super-new)))
  (define (wrap-convert-assert-PrintString . args) (void))
  (define (wrap-convert-assert-PrintWriter . args) (void))
  
  #;(compile-rest-of-lang (list "Object" "Throwable" "String" "Exception" "RuntimeException" "Comparable"))
  
  (provide Exception guard-convert-Exception convert-assert-Exception wrap-convert-assert-Exception
           dynamic-Exception/c static-Exception/c)  
  (define Exception
    (class* Throwable ()
      (super-instantiate ())
      (define/public (Exception-constructor) (send this Throwable-constructor))
      (define/public (Exception-constructor-java.lang.String s)
        (send this Throwable-constructor-java.lang.String s))
      (define/public (Exception-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this Throwable-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (Exception-constructor-java.lang.Throwable cause)
        (send this Throwable-constructor-java.lang.Throwable cause))
      (define/override (my-name) "Exception")))
  (define (wrap-convert-assert-Exception obj p n s c)
    (make-object convert-assert-Exception obj p n s c))
  (define convert-assert-Exception
    (class convert-assert-Throwable 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-Exception
    (class guard-convert-Throwable
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-Exception/c
    (c:flat-named-contract "Exception" (lambda (c) (is-a? c convert-assert-Exception))))
  (define static-Exception/c
    (c:flat-named-contract "Exception" (lambda (c) (is-a? c guard-convert-Exception))))
  
  (provide RuntimeException guard-convert-RuntimeException convert-assert-RuntimeException wrap-convert-assert-RuntimeException
           dynamic-RuntimeException/c static-RuntimeException/c)  
  (define RuntimeException
    (class* Exception ()
      (super-instantiate ())
      (define/public (RuntimeException-constructor) (send this Exception-constructor))
      (define/public (RuntimeException-constructor-java.lang.String s)
        (send this Exception-constructor-java.lang.String s))
      (define/public (RuntimeException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this Exception-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (RuntimeException-constructor-java.lang.Throwable cause)
        (send this Exception-constructor-java.lang.Throwable cause))
      (define/override (my-name) "RuntimeException")))
  (define (wrap-convert-assert-RuntimeException obj p n s c)
    (make-object convert-assert-RuntimeException obj p n s c))
  (define convert-assert-RuntimeException
    (class convert-assert-Exception 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-RuntimeException
    (class guard-convert-Exception
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-RuntimeException/c
    (c:flat-named-contract "RuntimeException" (lambda (c) (is-a? c convert-assert-RuntimeException))))
  (define static-RuntimeException/c
    (c:flat-named-contract "RuntimeException" (lambda (c) (is-a? c guard-convert-RuntimeException))))
  
  (provide IndexOutOfBoundsException guard-convert-IndexOutOfBoundsException convert-assert-IndexOutOfBoundsException wrap-convert-assert-IndexOutOfBoundsException
           dynamic-IndexOutOfBoundsException/c static-IndexOutOfBoundsException/c)  
  (define IndexOutOfBoundsException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (IndexOutOfBoundsException-constructor) (send this RuntimeException-constructor))
      (define/public (IndexOutOfBoundsException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (IndexOutOfBoundsException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (IndexOutOfBoundsException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "IndexOutOfBoundsException")))
  (define (wrap-convert-assert-IndexOutOfBoundsException obj p n s c)
    (make-object convert-assert-IndexOutOfBoundsException obj p n s c))
  (define convert-assert-IndexOutOfBoundsException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-IndexOutOfBoundsException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-IndexOutOfBoundsException/c
    (c:flat-named-contract "IndexOutOfBoundsException" (lambda (c) (is-a? c convert-assert-IndexOutOfBoundsException))))
  (define static-IndexOutOfBoundsException/c
    (c:flat-named-contract "IndexOutOfBoundsException" (lambda (c) (is-a? c guard-convert-IndexOutOfBoundsException))))
  
  (provide ArrayIndexOutOfBoundsException guard-convert-ArrayIndexOutOfBoundsException convert-assert-ArrayIndexOutOfBoundsException wrap-convert-assert-ArrayIndexOutOfBoundsException
           dynamic-ArrayIndexOutOfBoundsException/c static-ArrayIndexOutOfBoundsException/c)  
  (define ArrayIndexOutOfBoundsException
    (class* IndexOutOfBoundsException ()
      (super-instantiate ())
      (define/public (ArrayIndexOutOfBoundsException-constructor) (send this IndexOutOfBoundsException-constructor))
      (define/public (ArrayIndexOutOfBoundsException-constructor-java.lang.String s)
        (send this IndexOutOfBoundsException-constructor-java.lang.String s))
      (define/public (ArrayIndexOutOfBoundsException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this IndexOutOfBoundsException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (ArrayIndexOutOfBoundsException-constructor-java.lang.Throwable cause)
        (send this IndexOutOfBoundsException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "ArrayIndexOutOfBoundsException")))
  (define (wrap-convert-assert-ArrayIndexOutOfBoundsException obj p n s c)
    (make-object convert-assert-ArrayIndexOutOfBoundsException obj p n s c))
  (define convert-assert-ArrayIndexOutOfBoundsException
    (class convert-assert-IndexOutOfBoundsException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-ArrayIndexOutOfBoundsException
    (class guard-convert-IndexOutOfBoundsException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-ArrayIndexOutOfBoundsException/c
    (c:flat-named-contract "ArrayIndexOutOfBoundsException" (lambda (c) (is-a? c convert-assert-ArrayIndexOutOfBoundsException))))
  (define static-ArrayIndexOutOfBoundsException/c
    (c:flat-named-contract "ArrayIndexOutOfBoundsException" (lambda (c) (is-a? c guard-convert-ArrayIndexOutOfBoundsException))))

  (provide ArrayStoreException guard-convert-ArrayStoreException convert-assert-ArrayStoreException wrap-convert-assert-ArrayStoreException
           dynamic-ArrayStoreException/c static-ArrayStoreException/c)  
  (define ArrayStoreException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (ArrayStoreException-constructor) (send this RuntimeException-constructor))
      (define/public (ArrayStoreException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (ArrayStoreException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (ArrayStoreException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "ArrayStoreException")))
  (define (wrap-convert-assert-ArrayStoreException obj p n s c)
    (make-object convert-assert-ArrayStoreException obj p n s c))
  (define convert-assert-ArrayStoreException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-ArrayStoreException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-ArrayStoreException/c
    (c:flat-named-contract "ArrayStoreException" (lambda (c) (is-a? c convert-assert-ArrayStoreException))))
  (define static-ArrayStoreException/c
    (c:flat-named-contract "ArrayStoreException" (lambda (c) (is-a? c guard-convert-ArrayStoreException))))

  (provide NegativeArraySizeException guard-convert-NegativeArraySizeException convert-assert-NegativeArraySizeException wrap-convert-assert-NegativeArraySizeException
           dynamic-NegativeArraySizeException/c static-NegativeArraySizeException/c)  
  (define NegativeArraySizeException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (NegativeArraySizeException-constructor) (send this RuntimeException-constructor))
      (define/public (NegativeArraySizeException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (NegativeArraySizeException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (NegativeArraySizeException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "NegativeArraySizeException")))
  (define (wrap-convert-assert-NegativeArraySizeException obj p n s c)
    (make-object convert-assert-NegativeArraySizeException obj p n s c))
  (define convert-assert-NegativeArraySizeException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-NegativeArraySizeException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-NegativeArraySizeException/c
    (c:flat-named-contract "NegativeArraySizeException" (lambda (c) (is-a? c convert-assert-NegativeArraySizeException))))
  (define static-NegativeArraySizeException/c
    (c:flat-named-contract "NegativeArraySizeException" (lambda (c) (is-a? c guard-convert-NegativeArraySizeException))))

  (provide ClassCastException guard-convert-ClassCastException convert-assert-ClassCastException wrap-convert-assert-ClassCastException
           dynamic-ClassCastException/c static-ClassCastException/c)  
  (define ClassCastException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (ClassCastException-constructor) (send this RuntimeException-constructor))
      (define/public (ClassCastException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (ClassCastException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (ClassCastException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "ClassCastException")))
  (define (wrap-convert-assert-ClassCastException obj p n s c)
    (make-object convert-assert-ClassCastException obj p n s c))
  (define convert-assert-ClassCastException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-ClassCastException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-ClassCastException/c
    (c:flat-named-contract "ClassCastException" (lambda (c) (is-a? c convert-assert-ClassCastException))))
  (define static-ClassCastException/c
    (c:flat-named-contract "ClassCastException" (lambda (c) (is-a? c guard-convert-ClassCastException))))

  (provide ArithmeticException guard-convert-ArithmeticException convert-assert-ArithmeticException wrap-convert-assert-ArithmeticException
           dynamic-ArithmeticException/c static-ArithmeticException/c)  
  (define ArithmeticException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (ArithmeticException-constructor) (send this RuntimeException-constructor))
      (define/public (ArithmeticException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (ArithmeticException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (ArithmeticException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "ArithmeticException")))
  (define (wrap-convert-assert-ArithmeticException obj p n s c)
    (make-object convert-assert-ArithmeticException obj p n s c))
  (define convert-assert-ArithmeticException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-ArithmeticException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-ArithmeticException/c
    (c:flat-named-contract "ArithmeticException" (lambda (c) (is-a? c convert-assert-ArithmeticException))))
  (define static-ArithmeticException/c
    (c:flat-named-contract "ArithmeticException" (lambda (c) (is-a? c guard-convert-ArithmeticException))))

  (provide NullPointerException guard-convert-NullPointerException convert-assert-NullPointerException wrap-convert-assert-NullPointerException
           dynamic-NullPointerException/c static-NullPointerException/c)  
  (define NullPointerException
    (class* RuntimeException ()
      (super-instantiate ())
      (define/public (NullPointerException-constructor) (send this RuntimeException-constructor))
      (define/public (NullPointerException-constructor-java.lang.String s)
        (send this RuntimeException-constructor-java.lang.String s))
      (define/public (NullPointerException-constructor-java.lang.string-java.lang.Throwable s cause)
        (send this RuntimeException-constructor-java.lang.String-java.lang.Throwable s cause))
      (define/public (NullPointerException-constructor-java.lang.Throwable cause)
        (send this RuntimeException-constructor-java.lang.Throwable cause))
      (define/override (my-name) "NullPointerException")))
  (define (wrap-convert-assert-NullPointerException obj p n s c)
    (make-object convert-assert-NullPointerException obj p n s c))
  (define convert-assert-NullPointerException
    (class convert-assert-RuntimeException 
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define guard-convert-NullPointerException
    (class guard-convert-RuntimeException
      (init w p n s c)
      (super-instantiate (w p n s c))))
  (define dynamic-NullPointerException/c
    (c:flat-named-contract "NullPointerException" (lambda (c) (is-a? c convert-assert-NullPointerException))))
  (define static-NullPointerException/c
    (c:flat-named-contract "NullPointerException" (lambda (c) (is-a? c guard-convert-NullPointerException))))

  (define stm-wrapper (interface () log get-field set-field!))
  (provide stm-wrapper)
  
  )
