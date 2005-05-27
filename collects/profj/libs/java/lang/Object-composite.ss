#cs
(module Object-composite mzscheme
  
  (require (lib "class.ss")
           (lib "errortrace-lib.ss" "errortrace")
           (lib "Comparable.ss" "profj" "libs" "java" "lang")
           (lib "Serializable.ss" "profj" "libs" "java" "io"))
  (require "compile-lang-syntax.ss")    
  
  ;Runtime needed code
  (define (javaRuntime:convert-to-string data)
    (cond
      ((number? data) (make-java-string (number->string data)))
      ((boolean? data) 
       (make-java-string (if data "true" "false")))
      ((char? data) (make-java-string (string data)))
      ((is-a? data ObjectI) (send data toString))
      ((is-a? data object%) (make-java-string "SchemeObject"))
      (else (error 'JavaRuntime:Internal_Error:convert-to-string
                   (format "Convert to string given unsupported data: ~s" data)))))
  
  
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
  (provide ObjectI Object-Mix Object)
  
  ;Object interface, and a mixin to create objects from.
  
  (define ObjectI
    (interface () Object-constructor clone equals-java.lang.Object finalize getClass
      hashCode notify notifyAll toString wait wait-long wait-long-int my-name))
  
  (define Object-Mix
    (lambda (parent)
      (class* parent (ObjectI)
        
        (define/public (Object-constructor) (void))
        
        ;Needs to do something
        (define/public clone (lambda () void))
        
        (define/public (equals-java.lang.Object obj) (eq? this obj))
        
        ;Needs to do something
        (define/public (finalize) void)
        
        (public-final getClass)
        (define (getClass)
          (error 'ProfessorJ:getClass 
                 (format "ProfessorJ does not support getClass calls. ~e" 
                         (send this toString))))
        
        (define/public (hashCode) (eq-hash-code this))
        
        ;Needs to do something when Threads more implemented
        (public-final notify |notifyAll|)
        (define (notify) void)
        (define (notifyAll) void)
        
        (define/public (my-name) "Object")
        (define/public (toString)
          (make-java-string (format "~a@~a" (send this my-name) (send this hashCode))))
        
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
      
      (define array null)
      (define rt #f)
      (define/public (get-rt) rt)
      
      (define/private (check-runtime-type val)
        (if (<= (runtime-type-dim rt) 1)
            (if (symbol? (runtime-type-type rt))
                (case (runtime-type-type rt)
                  ((byte short int long) (and (number? val) (not (inexact? val))))
                  ((char) (char? val))
                  ((float double) (and (number? val) (inexact? val))))
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
              ((char) #\null))
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
  
  (define-struct runtime-type (type dim) (make-inspector))
  
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
    
      ;Object -> boolean
      (define/override (equals-java.lang.Object obj)
        (and (is-a? obj String)
             (equal? text (send (send obj toString) get-mzscheme-string))))
      
      ;Object -> boolean
      (define/public (equalsIgnoreCase-java.lang.String str)
        (string-ci=? text (send str get-mzscheme-string)))

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
      (define/public (substring-int index) (make-java-string (substring text index (string-length text))))
    
      ;... -> String
      (define/public (substring-int-int begin end) (make-java-string (substring text begin end)))
    
      (define/public (subSequence-int-int begin end)
        (error 'subSequence "Internal Error: subsequence is unimplemented because charSequence is unimplemented"))
    
      ;String -> String
      (define/public (concat-java.lang.String Jstr)
        (let ((str (send Jstr get-mzscheme-string)))
          (make-java-string (string-append text str))))
    
      ; .. -> String
      (define/public (replace-char-char old new)
        (let ((new-text (apply string-append (map string (string->list text)))))
          (let loop ([index 0])
            (let ((pos (find-char old index)))
              (unless (= -1 pos)
                (string-set! new-text pos new)
                (loop (add1 index)))))
          (make-java-string new-text)))
    
      ;Does not currently work. Needs to replace regex in text with replace and return new string; PROBLEM
      (define/public (replaceAll-java.lang.String-java.lang.String regex replace)
        (error 'replaceAll "Internal error: replaceAll is unimplemented at this time"))
    
      (define/public (replaceFirst-java.lang.String-java.lang.String regex replace)
        (error 'replaceFirst "Internal error: replaceFirst is unimplemented at this time"))
    
      (define/public (matches-java.lang.String regex)
        (error 'matches "Internal error: matches is unimplemented at this time"))
    
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
    
      ;PROBLEM I am not sure what the side effects of this are supposed to be! PROBLEM!
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

  (provide Throwable (struct java:exception (object))
           exception-is-a? handle-exception create-java-exception)
  
  (define Throwable
    (class* Object (Serializable)
      
      ;private fields
      ;message: String
      (define message "")
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
        (set! message (if (null? cse) null (send cse |toString|)))
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
      
      (define/public (setStackTrace-java.lang.StackTraceElement1 elments)
        (error 'setStackTrace "Internal error: setStackTrace will not be implemented until strack trace element s implemented"))
      (define/public (getStackTrace)
        (error 'getStackTrace "Internal error: getStackTrace will not be implemented until StackTraceElement is implemented"))
      
       ; -> string
      (define/override (toString)
        (if (null? message)
            (make-java-string (send this my-name))
            (make-java-string (format "~a: ~a" 
                                      (send this my-name) 
                                      (send (send this getMessage) get-mzscheme-string)))))
      
      ; -> void
      (define/public (printStackTrace)
        (print-error-trace (current-output-port)
                           (make-exn message stack)))

      ;These functions do not work correctly yet, and won't until printStreams are implemented
      (define/public printStackTrace-PrintStream (lambda (printStream) void)) 
      (define/public printStackTrace-PrintWriter (lambda (pW) void))
      
      ;This function does nothing at this time
      (define/public (fillInStackTrace) this)
      
      ; -> string
      (define/override (my-name) "Throwable")
      
      (super-instantiate ())))
  
  ;(make-java-exception string continuation-mark-set Throwable)
  ;Where Throwable is an object descending from class Throwable
  (define-struct (java:exception exn) (object))

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
  
  (compile-rest-of-lang (list "Object" "Throwable" "String" "Exception" "RuntimeException" "Comparable"))
  
  )