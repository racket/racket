(module types mzscheme

  (require 
   (only (lib "1.ss" "srfi") lset-intersection)
   (lib "etc.ss")
   (lib "pretty.ss")
   (lib "list.ss")
   (lib "class.ss")
   "ast.ss")
  
  (provide (all-defined-except sort number-assign-conversions remove-dups meth-member?
                               generate-require-spec))
      
  ;; symbol-type = 'null | 'string | 'boolean | 'char | 'byte | 'short | 'int
  ;;             | 'long | 'float | 'double | 'void | 'dynamic
  ;; reference-type = 'null | 'string | (make-ref-type string (list string))
  ;; array-type = (make-array-type type int)
  ;; type = symbol-type
  ;;      | reference-type
  ;;      | array-type
  ;;      | dynamic-val
  ;;      | unknown-ref

  (define-struct ref-type (class/iface path) (make-inspector))
  (define-struct array-type (type dim))
  
  (define object-type (make-ref-type "Object" `("java" "lang")))
  (define string-type (make-ref-type "String" `("java" "lang")))
  (define throw-type (make-ref-type "Throwable" `("java" "lang")))
  (define runtime-exn-type (make-ref-type "RuntimeException" `("java" "lang")))
  (define serializable-type (make-ref-type "Serializable" `("java" "io")))
  (define comparable-type (make-ref-type "Comparable" `("java" "lang")))
  (define cloneable-type (make-ref-type "Cloneable" `("java" "lang")))
  
;                                                                                                          
;                                                                                                          
;                                                   ;                       ;         ;                    
;                                                   ;                       ;                              
;  ;;;;;;;                                          ;                       ;                              
;     ;                                             ;                       ;                              
;     ;     ;    ;  ; ;;;    ;;;;             ;;;   ; ;;;    ;;;;     ;;;   ;   ;   ;;;     ; ;;;    ;;; ; 
;     ;      ;   ;  ;;  ;;   ;  ;;           ;   ;  ;;   ;   ;  ;;   ;   ;  ;  ;      ;     ;;   ;  ;;  ;; 
;     ;      ;  ;   ;    ;  ;    ;          ;       ;    ;  ;    ;  ;       ; ;       ;     ;    ;  ;    ; 
;     ;      ;  ;   ;    ;  ;;;;;;          ;       ;    ;  ;;;;;;  ;       ;;;       ;     ;    ;  ;    ; 
;     ;       ; ;   ;    ;  ;               ;       ;    ;  ;       ;       ;  ;      ;     ;    ;  ;    ; 
;     ;       ;;    ;;  ;;   ;   ;           ;   ;  ;    ;   ;   ;   ;   ;  ;   ;     ;     ;    ;  ;;  ;; 
;     ;        ;    ; ;;;     ;;;             ;;;   ;    ;    ;;;     ;;;   ;    ;  ;;;;;   ;    ;   ;;; ; 
;              ;    ;                                                                                    ; 
;             ;     ;                                                                                ;  ;; 
;            ;;     ;                                                                                 ;;;  
;                                                                                                          

  
  ;; reference-type: 'a -> boolean
  (define (reference-type? x)
    (if (and (dynamic-val? x) (dynamic-val-type x))
        (reference-type? (dynamic-val-type x))
        (or (dynamic-val? x) 
            (unknown-ref? x)
            (ref-type? x) 
            (memq x `(null string)))))
  
  ;;reference-or-array-type: 'a -> boolean
  (define (reference-or-array-type? x)
    (or (reference-type? x)
        (array-type? x)))

  ;;is-string?: 'a -> boolean
  (define (is-string-type? s)
    (if (dynamic-val? s)
        (is-string-type? (dynamic-val-type s))
        (and (reference-type? s)
             (or (eq? 'string s) (type=? s string-type)))))
  
  ;; 4.2
  ;; prim-integral-type?: 'a -> boolean
  (define (prim-integral-type? t)
    (cond
      ((and (dynamic-val? t) (dynamic-val-type t)) 
       (prim-integral-type? (dynamic-val-type t)))
      ((dynamic-val? t) #t)
      (else (memq t `(byte short int long char)))))
  ;; prim-numeric-type?: 'a -> boolean
  (define (prim-numeric-type? t)
    (cond 
      ((and (dynamic-val? t) (dynamic-val-type t))
       (prim-numeric-type? (dynamic-val-type t)))
      ((dynamic-val? t) #t)
      (else (or (prim-integral-type? t) (memq t `(float double))))))
  
  ;; type=?: type type -> boolean
  (define (type=? t1 t2)
    (cond
      ((and (symbol? t1) (symbol? t2))
       (symbol=? t1 t2))
      ((and (ref-type? t1) (ref-type? t2))
       (and (string=? (ref-type-class/iface t1) (ref-type-class/iface t2))
            (= (length (ref-type-path t1)) (length (ref-type-path t2)))
            (andmap
             (lambda (x y)
               (string=? x y))
             (ref-type-path t1)
             (ref-type-path t2))))
      ((and (array-type? t1) (array-type? t2))
       (and (= (array-type-dim t1) (array-type-dim t2))
            (type=? (array-type-type t1) (array-type-type t2))))
      ((or (symbol? t1) (symbol? t2))
       (or (or (and (eq? t1 'null) (ref-type? t2))
               (and (eq? t2 'null) (ref-type? t1)))
           (and (eq? t1 'string) (type=? t2 string-type))
           (and (eq? t2 'string) (type=? t1 string-type))))
      (else #f)))                     
  
  ;; 5.1.2
  ;; widening-prim-conversion: symbol-type symbol-type -> boolean
  (define (widening-prim-conversion to from)
    (cond
      ((symbol=? 'short to)
       (symbol=? 'byte from))
      ((symbol=? 'int to)
       (memq from `(byte short char)))
      ((symbol=? 'long to)
       (memq from `(byte short char int)))
      ((symbol=? 'float to)
       (memq from `(byte short char int long)))
      ((symbol=? 'double to)
       (memq from `(byte short char int long float)))))
  
  ;; 5.1.4
  ;; widening-ref-conversion: type type type-records -> boolean
  (define (widening-ref-conversion to from type-recs)
    (cond
      ((and (symbol? from) (symbol=? from 'null))
       (or (ref-type? to) (symbol=? 'string to) (array-type? to)))
      ((and (symbol? from) (symbol=? from 'string))
       (or (type=? to object-type) 
           (type=? to serializable-type) 
           (type=? to comparable-type)))
      ((and (ref-type? from) (ref-type? to))
       (or (is-subclass? from to type-recs)
           (implements? from to type-recs)
           (and (is-interface? from type-recs)
                (type=? object-type to))))
      ((array-type? from)
       (or (type=? object-type to)
           (type=? cloneable-type to)
           (type=? serializable-type to)
           (and (array-type? to) (= (array-type-dim from) (array-type-dim to))
                (assignment-conversion (array-type-type to) (array-type-type from) type-recs))))
      (else #f)))
  
  ;; 5.2
  ;; SKIP - possible narrowing conversion for constants
  ;; assignment-conversion: type type type-records -> boolean
  (define (assignment-conversion to from type-recs)
    (cond
      ((dynamic-val? to)
       (cond
         ((dynamic-val-type to) => (lambda (t) (assignment-conversion t from type-recs)))
         (else (set-dynamic-val-type! to from) #t)))
      ((dynamic-val? from)
       (cond
         ((dynamic-val-type from) => (lambda (t) (assignment-conversion to t type-recs)))
         (else (set-dynamic-val-type! from to) #t)))
      ((eq? to 'dynamic) #t)
      ((type=? to from) #t)
      ((and (prim-numeric-type? to) (prim-numeric-type? from))
       (widening-prim-conversion to from))
      (else
       (widening-ref-conversion to from type-recs))))
  
  ;castable?: reference-type reference-type type-records -> boolean
  (define (castable? from to type-recs)
    (or (dynamic-val? from)
        (dynamic-val? to)
        (eq? 'dynamic to)
        (eq? 'null from)
        (eq? 'null to)
        (let ((from-record (and (not (array-type? from)) (send type-recs get-class-record from)))
              (to-record (and (not (array-type? to))
                              (get-record (send type-recs get-class-record to) type-recs))))
          (cond
            ((and to-record from-record
                  (class-record-class? from-record)
                  (class-record-class? to-record))
             (or (is-eq-subclass? from to type-recs)
                 (is-eq-subclass? to from type-recs)))
            ((and to-record from-record (class-record-class? from-record))
             (or (not (memq 'final (class-record-modifiers from-record)))
                 (implements? from to type-recs)))
            ((and (not to-record) from-record (class-record-class? from-record))
             (type=? object-type from))
            ((and to-record from-record (class-record-class? to-record))
             (or (not (memq 'final (class-record-modifiers to-record)))
                 (implements? to from type-recs)))
            ((and to-record from-record (not (class-record-class? to-record)))
             (not (signature-conflicts? (class-record-methods to-record)
                                        (class-record-methods from-record))))
            ((and (not from-record) to-record (class-record-class? to-record))
             (type=? object-type to))
            ((and (not from-record) to-record)
             (or (type=? serializable-type to type-recs)
                 (type=? cloneable-type to type-recs)))
            (else
             (or (type=? (array-type-type to) (array-type-type from))
                 (castable? (array-type-type from)
                            (array-type-type to))))))))
  
  ;Do the two lists of method signatures have conflicting methods
  ;signature-conflicts? (list method-record) (list method-record) -> bool
  (define (signature-conflicts? methods1 methods2)
    (let ((same-sigs (lset-intersection signature-equals? methods1 methods2))
          (same-rets (lset-intersection full-signature-equals? methods1 methods2)))
      (not (= (length same-sigs) (length same-rets)))))

  ;Do the two methods have same name and argument types
  ;signature-equals? method-record method-record -> bool
  (define (signature-equals? m1 m2)
    (and (equal? (method-record-name m1)
                 (method-record-name m2))
         (= (length (method-record-atypes m1))
            (length (method-record-atypes m2)))
         (andmap type=? (method-record-atypes m1) (method-record-atypes m2))))
  ;Do the two methods have the same name, arguments and return types
  ;full-signagure-equals? method-record method-record -> bool
  (define (full-signature-equals? m1 m2)
    (and (signature-equals? m1 m2)
         (type=? (method-record-rtype m1) (method-record-rtype m2))))
    
  ;; type-spec-to-type: type-spec (U #f (list string) symbol type-records -> type
  (define (type-spec-to-type ts container-class level type-recs)
    (let* ((ts-name (type-spec-name ts))
           (t (cond
                ((memq ts-name `(null string boolean char byte short int long float double void ctor dynamic)) ts-name)
                ((name? ts-name) (name->type ts-name container-class (type-spec-src ts) level type-recs)))))
      (if (> (type-spec-dim ts) 0)
          (make-array-type t (type-spec-dim ts))
          t)))

  ;name->type: name (U (list string) #f) src symbol type-records -> type
  (define (name->type n container-class src level type-recs)
    (let ((name (id-string (name-id n)))
          (path (map id-string (name-path n))))
      (type-exists? name path container-class src level type-recs)
      (make-ref-type name (if (null? path) (send type-recs lookup-path name (lambda () null)) path)))) 
  
  ;; type-exists: string (list string) (U (list string) #f) src symbol type-records -> (U record procedure)
  (define (type-exists? name path container-class src level type-recs)
    (send type-recs get-class-record (cons name path) container-class
          ((get-importer type-recs) (cons name path) type-recs level src)))
    
  ;; is-interface?: (U type (list string) 'string) type-records-> boolean
  (define (is-interface? t type-recs)
    (not (class-record-class? 
          (get-record (send type-recs get-class-record t) type-recs))))
  
  ;;Is c1 a subclass of c2?
  ;; is-subclass?: (U type (list string) 'string) ref-type type-records -> boolean
  (define (is-subclass? c1 c2 type-recs)
    (let ((cr (get-record (send type-recs get-class-record c1) type-recs)))
      (member (cons (ref-type-class/iface c2) (ref-type-path c2))
              (class-record-parents cr))))

  ;Does c1 implement c2?
  ;; implements?: (U type (list string) 'string) ref-type type-records -> boolean
  (define (implements? c1 c2 type-recs)
    (let ((cr (get-record (send type-recs get-class-record c1) type-recs)))
      (member (cons (ref-type-class/iface c2) (ref-type-path c2))
              (class-record-ifaces cr))))

  ;;Is class1 a subclass or equal to class2?
  ;is-eq-subclass: type type type-records -> boolean
  (define (is-eq-subclass? class1 class2 type-recs)
    (or (type=? class1 class2)
        (and (reference-type? class1)
             (reference-type? class2)
             (is-subclass? class1 class2 type-recs))))
  
;                                                                                                          
;                                                                                                          
;                                                                                                          
;            ;;;                                                                                 ;         
;     ;;;;     ;                                    ;;;;;                                        ;         
;    ;    ;    ;                                    ;    ;;                                      ;         
;   ;;         ;                                    ;     ;                                      ;         
;   ;          ;     ;;;;    ;;;;    ;;;;           ;     ;  ;;;;     ;;;    ;;;;    ; ;;;   ;;; ;   ;;;;  
;   ;          ;         ;  ;    ;  ;    ;          ;    ;;  ;  ;;   ;   ;  ;;  ;;   ;;     ;;  ;;  ;    ; 
;   ;          ;     ;;;;;  ;;      ;;              ;;;;;;  ;    ;  ;       ;    ;   ;      ;    ;  ;;     
;   ;          ;    ;;   ;   ;;;;    ;;;;           ;    ;  ;;;;;;  ;       ;    ;   ;      ;    ;   ;;;;  
;   ;;         ;    ;    ;       ;       ;          ;     ; ;       ;       ;    ;   ;      ;    ;       ; 
;    ;    ;    ;    ;   ;;  ;    ;  ;    ;          ;     ;  ;   ;   ;   ;  ;;  ;;   ;      ;;  ;;  ;    ; 
;     ;;;;   ;;;;;   ;;; ;   ;;;;    ;;;;           ;      ;  ;;;     ;;;    ;;;;    ;       ;;; ;   ;;;;  
;                                                                                                          
;                                                                                                          
;                                                                                                          
    
  ;; (make-class-record (list string) (list symbol) boolean boolean (list field-record) 
  ;;                    (list method-records) (list inner-record) (list (list strings)) (list (list strings)))
  ;; After full processing fields and methods should contain all inherited fields 
  ;; and methods.  Also parents and ifaces should contain all super-classes/ifaces
  (define-struct class-record (name modifiers class? object? fields methods inners parents ifaces) (make-inspector))

  (define interactions-record (make-class-record (list "interactions") null #f #f null null null null null))
  
  ;; (make-field-record string (list symbol) bool (list string) type)
  (define-struct field-record (name modifiers init? class type) (make-inspector))
  
  ;; (make-method-record string (list symbol) type (list type) (list type) (U bool method-record) string)
  (define-struct method-record (name modifiers rtype atypes throws override class) (make-inspector))

  ;;(make-inner-record string (list symbol) bool)
  (define-struct inner-record (name modifiers class?) (make-inspector))

  ;;(make-scheme-record string (list string) path (list dynamic-val))
  (define-struct scheme-record (name path dir provides))
  
  ;;(make-dynamic-val (U type method-contract unknown-ref))
  (define-struct dynamic-val (type) (make-inspector))
  
  ;;(make-unknown-ref (U method-contract field-contract))
  (define-struct unknown-ref (access) (make-inspector))
  
  ;;(make-method-contract string type (list type) (U #f string))
  (define-struct method-contract (name return args prefix) (make-inspector))
  
  ;;(make-field-contract string type)
  (define-struct field-contract (name type))
  
;                                                                                      
;                                                                            ;;        
;    ;                                                                        ;        
;    ;                                                                        ;        
;   ;;;;; ;;; ;;;; ;;;    ;;;          ; ;;;   ;;;    ;;;    ;;;   ; ;;;   ;;;;   ;;;  
;    ;     ;   ;  ;   ;  ;   ;          ;     ;   ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
;    ;     ;   ;  ;   ;  ;;;;;  ;;;;;   ;     ;;;;;  ;      ;   ;   ;     ;   ;   ;;;  
;    ;      ; ;   ;   ;  ;              ;     ;      ;      ;   ;   ;     ;   ;      ; 
;    ;   ;  ;;;   ;   ;  ;   ;          ;     ;   ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
;     ;;;    ;    ;;;;    ;;;          ;;;;    ;;;    ;;;    ;;;   ;;;;    ;;; ;  ;;;  
;            ;    ;                                                                    
;            ;    ;                                                                    
;          ;;    ;;;                                                                   
                                                                                                                                                  
  ;Class to store various information per package compilation
  (define type-records 
    (class object%
      
      (field (importer 
              (lambda () 
                (error 'internal-error "type-records importer field was not set"))))
      
      ;Stores type information and require syntax per compile or execution
      (define records (make-hash-table 'equal))
      (define requires (make-hash-table 'equal))
      (define package-contents (make-hash-table 'equal))
      
      ;Stores per-class information accessed by location
      (define class-environment (make-hash-table))
      (define class-require (make-hash-table))

      (define compilation-location (make-hash-table))
      
      (define class-reqs null)
      (define location #f)
      
      ;add-class-record: class-record -> void
      (define/public (add-class-record r)
        (hash-table-put! records (class-record-name r) r))
      ;add-to-records: (list string) ( -> 'a) -> void
      (define/public (add-to-records key thunk)
        (hash-table-put! records key thunk))
      
      ;; get-class-record: (U type (list string) 'string) (U (list string) #f) ( -> 'a) -> 
      ;;                                            (U class-record scheme-record procedure)
      (define/public get-class-record
        (opt-lambda (ctype [container #f] [fail (lambda () null)])
          (let*-values (((key key-path) (normalize-key ctype))
                        ((key-inner) (when (cons? container) (string-append (car container) "." key)))
                        ((outer-record) (when (cons? container) (get-class-record container)))
                        ((path) (if (null? key-path) (lookup-path key (lambda () null)) key-path))
                        ((inner-path) (if (null? key-path) (lookup-path key-inner (lambda () null)) key-path))
                        ((new-search)
                         (lambda ()
                           (if (null? path) 
                               (fail)
                               (let ((back-path (reverse path)))
                                 (search-for-record key (car back-path) 
                                                    (reverse (cdr back-path)) (lambda () #f) fail))))))
            ;(printf "get-class-record: ~a~n" ctype)
            ;(hash-table-for-each records (lambda (k v) (printf "~a -> ~a~n" k v)))
            (cond
              ((and container 
                    (not (null? outer-record))
                    (not (eq? outer-record 'in-progress))
                    (member key (map inner-record-name (class-record-inners (get-record outer-record this)))))
               (hash-table-get records (cons key-inner (cdr container)) fail))
              ((and container (not (null? outer-record)) (eq? outer-record 'in-progress))
               (let ((res (hash-table-get records (cons key-inner inner-path) (lambda () #f))))
                 (or res
                     (hash-table-get records (cons key path) new-search))))
              (else
               (hash-table-get records (cons key path) new-search))))))

      ;normalize-key: (U 'strung ref-type (list string)) -> (values string (list string))
      (define/private (normalize-key ctype)
        (cond
          ((eq? ctype 'string) (values "String" `("java" "lang")))
          ((ref-type? ctype) (values (ref-type-class/iface ctype) (ref-type-path ctype)))
          ((cons? ctype) (values (car ctype) (cdr ctype)))
          (else (values ctype null))))
      
      ;search-for-record string string (list string) (-> #f) (-> 'a) -> class-record
      (define/private (search-for-record class-name new-prefix path test-fail fail)
        (let* ((new-class-name (string-append new-prefix "." class-name))
               (rec? (hash-table-get records (cons new-class-name path) test-fail))
               (back-path (reverse path)))
          (cond
            (rec? rec?)
            ((null? path) (fail))
            (else (search-for-record new-class-name (car back-path) (reverse (cdr back-path)) test-fail fail)))))                  
      
      ;add-package-contents: (list string) (list string) -> void
      (define/public (add-package-contents package classes)
        (let ((existing-classes (hash-table-get package-contents package (lambda () null))))
          (if (null? existing-classes)
              (hash-table-put! package-contents package classes)
              (hash-table-put! package-contents package (non-dup-append classes existing-classes)))))

      (define/private (non-dup-append cl pa)
        (cond
          ((null? cl) pa)
          ((member (car cl) pa) (non-dup-append (cdr cl) pa))
          (else (cons (car cl) (non-dup-append (cdr cl) pa)))))
      
      ;get-package-contents: (list string) ( -> 'a) -> (list string)
      (define/public (get-package-contents package fail)
        (hash-table-get package-contents package fail))
      
      ;add-to-env: string (list string) file -> void
      (define/public (add-to-env class path loc)
        (hash-table-put! (hash-table-get class-environment loc
                                         (lambda ()
                                           (let ((new-t (make-hash-table 'equal)))
                                             (hash-table-put! class-environment loc new-t)
                                             new-t)))
                         class
                         path))
      
      ;Returns the environment of classes for the current location
      ;get-class-env: -> (list string)
      (define/public (get-class-env)
        (hash-table-map (hash-table-get class-environment location) (lambda (key val) key)))
      
      (define (env-failure)
        (error 'class-environment "Internal Error: environment does not have location"))
      
      ;lookup-path: string ( -> 'a) -> (U (list string) #f)
      (define/public (lookup-path class fail)
        ;(printf "lookup ~a~n" class)
        ;(hash-table-for-each (hash-table-get class-environment location)
        ;                    (lambda (k v) (printf "~a -> ~a~n" k v)))
        (if location
            (hash-table-get (hash-table-get class-environment 
                                            location 
                                            env-failure)
                            class fail)
            (fail)))
      
      ;add-require-syntax: (list string) (list syntax syntax) -> void
      (define/public (add-require-syntax name syn)
        (get-require-syntax #t name (lambda () (hash-table-put! requires (cons #t name) (car syn))))
        (get-require-syntax #f name (lambda () (hash-table-put! requires (cons #f name) (cadr syn)))))
      
      (define (syntax-fail)
        (error 'syntax "Internal Error: syntax did not have given req"))
      
      ;get-require-syntax: bool (list string) . ( -> 'a)  -> syntax
      (define/public (get-require-syntax prefix? name . fail)
        (hash-table-get requires (cons prefix? name) (if (null? fail) syntax-fail (car fail))))
        
      ;add-class-req: name boolean location -> void
      (define/public (add-class-req name pre loc)
        (hash-table-put! (hash-table-get class-require
                                         loc
                                         (lambda () (let ((new-t (make-hash-table 'equal)))
                                                      (hash-table-put! class-require loc new-t)
                                                      new-t)))
                         name pre))
      
      ;require-fail
      (define (require-fail)
        (error 'require-prefix "Internal Error: require does not have location"))
      
      ;require-prefix?: (list string) ( -> 'a) -> bool
      (define/public (require-prefix? name fail)
        (hash-table-get (hash-table-get class-require location require-fail) name fail))
      
      (define/private (member-req req reqs)
        (and (not (null? reqs))
             (or (and (equal? (req-class req) (req-class (car reqs)))
                      (equal? (req-path req) (req-path (car reqs))))
                 (member-req req (cdr reqs)))))

      (define/public (set-compilation-location loc dir)  (hash-table-put! compilation-location loc dir))
      (define/public (get-compilation-location)
        (hash-table-get compilation-location location 
                        (lambda () (error 'get-compilation-location "Internal error: location not found"))))
      (define/public (set-composite-location name dir) (hash-table-put! compilation-location name dir))
      (define/public (get-composite-location name)
        ;(printf "get-composite-location for ~a~n" name)
        ;(hash-table-for-each compilation-location
        ;                     (lambda (k v) (printf "~a -> ~a~n" k v)))
        (hash-table-get compilation-location name 
                        (lambda () (error 'get-composite-location "Internal error: name not found"))))
      
      (define/public (add-req req)
        (unless (member-req req class-reqs)
          (set! class-reqs (cons req class-reqs))))
      (define/public (get-class-reqs) class-reqs)
      (define/public (set-class-reqs reqs) (set! class-reqs reqs))

      (define/public (set-location! l) (set! location l))
      (define/public (get-location) location)

      (define interaction-package null)
      (define interaction-fields null)
      (define interaction-boxes null)      
      (define execution-loc #f)
      
      (define/public (set-interactions-package p) (set! interaction-package p))
      (define/public (get-interactions-package) interaction-package)
      (define/public (add-interactions-field rec)
        (set! interaction-fields (cons rec interaction-fields)))
      (define/public (get-interactions-fields)
        interaction-fields)
      (define/public (clear-interactions)
        (set! interaction-fields null))
      (define/public (add-interactions-box box)
        (set! interaction-boxes (cons box interaction-boxes)))
      (define/public (get-interactions-boxes) (reverse interaction-boxes))
      (define/public (set-execution-loc! loc) (set! execution-loc loc))
      
      (define/public (give-interaction-execution-names)
        (when execution-loc
          (hash-table-for-each (hash-table-get class-environment execution-loc)
                               (lambda (k v) (add-to-env k v 'interactions)))
          (set! execution-loc #f)))
      
      (super-instantiate ())))
  
  (define get-importer (class-field-accessor type-records importer))
  (define set-importer! (class-field-mutator type-records importer))

;                                                          

;                                                          
;     ;;;;            ;       ;                            
;    ;    ;           ;       ;                            
;   ;        ;;;;   ;;;;;;  ;;;;;;   ;;;;    ; ;;    ;;;;  
;   ;        ;  ;;    ;       ;      ;  ;;   ;;  ;  ;    ; 
;   ;    ;; ;    ;    ;       ;     ;    ;   ;      ;;     
;   ;     ; ;;;;;;    ;       ;     ;;;;;;   ;       ;;;;  
;   ;     ; ;         ;       ;     ;        ;           ; 
;    ;    ;  ;   ;    ;       ;      ;   ;   ;      ;    ; 
;     ;;;;    ;;;      ;;;     ;;;    ;;;    ;       ;;;;  
;                                                          
;                                                          
  
  ;get-record: (U class-record procedure) type-records -> class-record
  (define (get-record rec type-recs)
    (cond
      ((procedure? rec) 
       (let ((location (send type-recs get-location)))
         (begin0 (rec) 
                 (send type-recs set-location! location))))
      (else rec)))
  
  ;; get-field-record: string class-record (-> 'a) -> field-record
  (define (get-field-record fname c fail)
    (let ((frec (filter (lambda (f)
                          (string=? (field-record-name f) fname))
                        (class-record-fields c))))
      (cond
        ((null? frec) (fail))
        (else (car frec)))))

  ;get-field-records: class-record -> (list field-record)
  (define (get-field-records c) (class-record-fields c))
  
  ;; get-method-records: string class-record type-records -> (list method-record)
  (define (get-method-records mname c type-recs)
    (filter (lambda (m)
              (string=? (method-record-name m) mname))
            (if (class-record-class? c)
                (class-record-methods c)
                (append (class-record-methods c) (get-object-methods type-recs)))))
  
  (define (get-object-methods type-recs)
    (class-record-methods (send type-recs get-class-record object-type)))

  ;remove-dups: (list method-record) -> (list method-record)
  (define (remove-dups methods)
    (cond
      ((null? methods) methods)
      ((meth-member? (car methods) (cdr methods))
       (remove-dups (cdr methods)))
      (else (cons (car methods) (remove-dups (cdr methods))))))

  ;meth-member? method-record (list method-record) -> bool
  (define (meth-member? meth methods)
    (and (not (null? methods))
         (or (andmap type=? (method-record-atypes meth) 
                            (method-record-atypes (car methods)))
             (meth-member? meth (cdr methods)))))

  ;sort: (list number) -> (list number)
  (define (sort l)
    (quicksort l (lambda (i1 i2) (< (car i1) (car i2)))))
  
  ;number-assign-conversion: (list type) (list type) type-records -> int
  (define (number-assign-conversions site-args method-args type-recs)
    (cond
      ((null? site-args) 0)
      ((and (assignment-conversion (car site-args) (car method-args) type-recs)
            (not (type=? (car site-args) (car method-args))))
       (add1 (number-assign-conversions (cdr site-args) (cdr method-args) type-recs)))
      (else (number-assign-conversions (cdr site-args) (cdr method-args) type-recs))))
  
  ;; resolve-overloading: (list method-record) (list type) (-> 'a) (-> 'a) (-> 'a) type-records-> method-record
  (define (resolve-overloading methods arg-types arg-count-fail method-conflict-fail  no-method-fail type-recs)
    (let* ((a (length arg-types))
           (m-atypes method-record-atypes)
           (a-convert? (lambda (t1 t2) (assignment-conversion t1 t2 type-recs)))
           (methods (remove-dups (filter (lambda (mr) (= a (length (m-atypes mr)))) methods)))
           (methods-same (filter (lambda (mr) 
                                   (andmap type=? (m-atypes mr) arg-types))
                                 methods))
           (assignable (filter (lambda (mr)
                                 (andmap a-convert? (m-atypes mr) arg-types))
                               methods))
           (assignable-count (sort 
                              (map (lambda (mr)
                                     (list (number-assign-conversions arg-types (m-atypes mr) type-recs)
                                           mr))
                                   assignable))))
      (cond
        ((null? methods) (arg-count-fail))
        ((= 1 (length methods-same)) (car methods-same))
        ((> (length methods-same) 1) (method-conflict-fail))
        ((null? assignable) (no-method-fail))
        ((= 1 (length assignable)) (car assignable))
        ((= (car (car assignable-count))
            (car (cadr assignable-count))) (method-conflict-fail))
        (else (car assignable)))))

  ;module-has-binding?: scheme-record string (-> void) -> void
  ;module-has-binding raises an exception when variable is not defined in mod-ref
  (define (module-has-binding? mod-ref variable fail)
    (let ((var (string->symbol (java-name->scheme variable))))
      (or (memq var (scheme-record-provides mod-ref))
          (let ((mod-syntax (datum->syntax-object #f
                                                  `(module m mzscheme
                                                     (require ,(generate-require-spec (java-name->scheme (scheme-record-name mod-ref))
                                                                                      (scheme-record-path mod-ref)))
                                                     ,var)
                                                  #f)))
            (with-handlers ((exn? (lambda (e) (fail))))
              (expand mod-syntax))
            (set-scheme-record-provides! mod-ref (cons var (scheme-record-provides mod-ref))))
                                    
          #;(let ((old-namespace (current-namespace)))
           (current-namespace (make-namespace))
           (namespace-require (generate-require-spec (java-name->scheme (scheme-record-name mod-ref))
                                                     (scheme-record-path mod-ref)))
           (begin
             (namespace-variable-value var #t  (lambda () 
                                                 (current-namespace old-namespace)
                                                 (fail)))
             (set-scheme-record-provides! mod-ref (cons var (scheme-record-provides mod-ref)))
             (current-namespace old-namespace))))))
          
  ;generate-require-spec: string (list string) -> (U string (list symbol string+))
  (define (generate-require-spec name path)
    (let ((mod (string-append name ".ss")))
      (cond
        ((null? path) mod)
        ((equal? (car path) "lib")  `(lib ,mod ,@(cdr path)))
        (else `(file ,(build-path (apply build-path path) mod))))))
  
  ;java-name->scheme: string -> string
  (define (java-name->scheme name)
    (cond
      ((regexp-match "[a-zA-Z0-9]*To[A-Z0-9]*" name)
       (java-name->scheme (regexp-replace "To" name "->")))
      ((regexp-match "[a-zA-Z0-9]+P$" name)
       (java-name->scheme (regexp-replace "P$" name "?")))
      ((regexp-match "[a-zA-Z0-9]+Set$" name)
       (java-name->scheme (regexp-replace "Set$" name "!")))
      ((regexp-match "[a-zA-Z0-9]+Obj$" name)
       (java-name->scheme (regexp-replace "Obj%" name "%")))
      ((regexp-match "[a-z0-9]+->[A-Z]" name) =>
       (lambda (substring)
         (let ((char (car (regexp-match "[A-Z]" (car substring)))))
           (java-name->scheme (regexp-replace (string-append "->" char) name
                                              (string-append "->" (string (char-downcase (car (string->list char))))))))))
      ((regexp-match "[a-z0-9]+[A-Z]" name) =>
       (lambda (substring)
         (let ((char (car (string->list (car (regexp-match "[A-Z]" (car substring))))))
               (remainder (car (regexp-match "[a-z0-9]+" (car substring)))))
           (java-name->scheme (regexp-replace (car substring) name 
                                              (string-append remainder "-" (string (char-downcase char))))))))
      (else name)))

  
                  
;                                          
;             ;                ;;          
;                             ;            
;     ;;;                     ;            
;       ;                     ;            
;       ;   ;;;     ; ;;;   ;;;;;    ;;;;  
;       ;     ;     ;;   ;    ;     ;;  ;; 
;       ;     ;     ;    ;    ;     ;    ; 
;       ;     ;     ;    ;    ;     ;    ; 
;       ;     ;     ;    ;    ;     ;    ; 
;   ;  ;;     ;     ;    ;    ;     ;;  ;; 
;    ;;;    ;;;;;   ;    ;    ;      ;;;;  
;                                          

  
  (define type-version "version3")
  (define type-length 10)
  
  ;; read-record: path -> (U class-record #f)
  (define (read-record filename)
    (letrec ((parse-class/iface
              (lambda (input)
                (and (= (length input) type-length)
                     (equal? type-version (list-ref input 9))
                     (make-class-record (list-ref input 1)
                                        (list-ref input 2)
                                        (symbol=? 'class (car input))
                                        (list-ref input 3)
                                        (map parse-field (list-ref input 4))
                                        (map parse-method (list-ref input 5))
                                        (map parse-inner (list-ref input 6))
                                        (list-ref input 7)
                                        (list-ref input 8)))))
             (parse-field
              (lambda (input)
                (make-field-record (car input)
                                   (cadr input)
                                   #f
                                   (caddr input)
                                   (parse-type (cadddr input)))))
             (parse-method
              (lambda (input)
                (make-method-record (car input)
                                    (cadr input)
                                    (parse-type (caddr input))
                                    (map parse-type (cadddr input))
                                    (map parse-type (list-ref input 4))
                                    #f
                                    (list-ref input 5))))
             (parse-inner
              (lambda (input)
                (make-inner-record (car input)
                                   (cadr input)
                                   (symbol=? 'class (caddr input)))))
             (parse-type
              (lambda (input)
                (cond
                  ((symbol? input) input)
                  ((number? (car input)) 
                   (make-array-type (parse-type (cadr input)) (car input)))
                  (else
                   (make-ref-type (car input) (cdr input)))))))
      (parse-class/iface (call-with-input-file filename read))))
  
  ;; write-record: class-record port->
  (define (write-record rec port)
    (letrec ((record->list
              (lambda (r)
                (list
                 (if (class-record-class? r)
                     'class
                     'interface)
                 (class-record-name r)
                 (class-record-modifiers r)
                 (class-record-object? r)
                 (map field->list (class-record-fields r))
                 (map method->list (filter (compose not method-record-override) (class-record-methods r)))
                 (map inner->list (class-record-inners r))
                 (class-record-parents r)
                 (class-record-ifaces r)
                 type-version)))
             (field->list
              (lambda (f)
                (list
                 (field-record-name f)
                 (field-record-modifiers f)
                 (field-record-class f)
                 (type->list (field-record-type f)))))
             (method->list
              (lambda (m)
                (list
                 (method-record-name m)
                 (method-record-modifiers m)
                 (type->list (method-record-rtype m))
                 (map type->list (method-record-atypes m))
                 (map type->list (method-record-throws m))
                 (method-record-class m))))
             (inner->list
              (lambda (i)
                (list (inner-record-name i)
                      (inner-record-modifiers i)
                      (if (inner-record-class? i) 'class 'interface))))
             (type->list
              (lambda (t)
                (cond
                  ((symbol? t) t)
                  ((ref-type? t) (cons (ref-type-class/iface t) (ref-type-path t)))
                  ((array-type? t)
                   (list (array-type-dim t) (type->list (array-type-type t))))))))
      (pretty-print (record->list rec) port)))
  )
