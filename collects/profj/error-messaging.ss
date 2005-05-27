#cs
(module error-messaging mzscheme
  
  (require "ast.ss")
  (require "types.ss")
  
  (provide make-error-pass get-expected type->ext-name id->ext-name 
           get-call-type method-name->ext-name path->ext name->path
           statement->ext-name)
  
  ;make-error: 'a string 'a src -> void
  (define (make-error-pass parm)
    (lambda (kind message so src)
      (raise-syntax-error kind message (make-so so src parm))))
  
  ;make-so: symbol src (-> location) -> syntax-object
  (define (make-so id src parm)
    (datum->syntax-object #f id (build-src-list src parm)))
  
  ;build-src-list: src (-> location) -> (U bool (list loc int int int int))
  (define (build-src-list src parm)
    (if (not src)
        src
        (if (and (= (src-line src) 0)
                 (= (src-col src) 0)
                 (= (src-pos src) 0)
                 (= (src-span src) 0))
            #f
            (list (or (src-file src) (parm)) (src-line src) (src-col src) (src-pos src) (src-span src)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Message helpers
  
  ;get-expected: symbol-> string
  (define (get-expected e)
    (case e
      ((bool) 'boolean)
      ((int) "int, short, byte or char")
      ((num) "double, float, long, int, short, byte or char")
      (else "dummy")))
  
 ;type->ext-name: type -> symbol
  (define (type->ext-name t)
    (string->symbol
     (cond 
       ((ref-type? t) (ref-type-class/iface t))
       ((array-type? t) 
        (format "~a~a" (type->ext-name (array-type-type t))
                (let ((dims ""))
                  (let loop ((d (array-type-dim t)))
                    (if (= d 0)
                        dims
                        (begin (set! dims (string-append dims "[]"))
                               (loop (sub1 d))))))))
       (else (format "~a" t)))))
  
  ;id->ext-name: id -> symbol
  (define (id->ext-name id)
    (string->symbol (if (special-name? id)
                        (special-name-name id)
                        (id-string id))))
  
  ;get-call-type: type -> string
  (define (get-call-type t)
    (cond
      ((eq? t 'super) "the current super class")
      ((not t) "this class")
      (else (type->ext-name t))))
  
  ;make-parm-string: (list type) -> string
  (define (make-parm-string parms)
    (if (null? parms)
        ""
        (let ((parm-str (apply string-append
                          (map 
                           (lambda (p) (format "~a " (type->ext-name p))) parms))))
          (substring parm-str 0 (sub1 (string-length parm-str))))))
  
  ;method-name->ext-name: string (list type) -> symbol
  (define (method-name->ext-name name parms)
    (string->symbol (format "~a(~a)" name (make-parm-string parms))))

  ;path->ext: (list string) -> string
  (define (path->ext path)
    (apply string-append
           (append (map (lambda (a) (string-append a "."))
                        (cdr path))
                   (list (car path)))))
  
  ;name->path: name -> (list string)
  (define (name->path n)
    (cons (id-string (name-id n)) (map id-string (name-path n))))
  
  ;statement->ext-name: statement -> symbol
  (define (statement->ext-name s)
    (cond
      ((ifS? s) 'if)
      ((return? s) 'return)
      ((call? s) 
       (let ((method (call-method-name s)))
         (string->symbol (if (special-name? method)
                             (special-name-name method)
                             (id-string method)))))))

  )