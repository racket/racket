(module tenv mzscheme
  
  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "boundmap.ss" "syntax")
           (lib "contract.ss")  
           "ast.ss"
           "parameters.ss"
           "readerr.ss")
  
  (provide (struct tenv:entry  (stx))
           (struct tenv:init   (name type optional?))
           (struct tenv:member (stx name type))
           (struct tenv:type   (supers members inherited))
           (struct tenv:class  (sub-type impls inits final? super))
           (struct tenv:mixin  (arg-type sub-type impls inits
                                         withs final?))
           (struct tenv:value  (type)))
  
  (define-struct tenv:entry (stx) #f)

  (define-struct tenv:init (name type optional?) #f)
  
  (define-struct tenv:member (stx name type) #f)
 
  ;; members will be a hashtable from member names to types
  ;; -- if I ever get around to it
  (define-struct (tenv:type  tenv:entry) (supers members inherited) #f)
  (define-struct (tenv:class tenv:entry) (sub-type impls inits final? super) #f)
  (define-struct (tenv:mixin tenv:entry) (arg-type sub-type impls inits
                                           withs final?) #f)
  ;; this is for top-level function and value bindings
  (define-struct (tenv:value tenv:entry) (type) #f)
  
  (define builtin-list
    (list (cons #'error         (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-bot #f)))
          (cons #'printString   (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-tuple #f '())))
          (cons #'printLine     (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-tuple #f '())))
          (cons #'readChar      (make-honu:type-func #f
                                  (make-honu:type-tuple #f '())
                                  (make-honu:type-prim #f 'char)))
          (cons #'readLine      (make-honu:type-func #f
                                  (make-honu:type-tuple #f '())
                                  (make-honu:type-prim #f 'string)))
          (cons #'intToString   (make-honu:type-func #f
                                  (make-honu:type-prim #f 'int)
                                  (make-honu:type-prim #f 'string)))
          (cons #'floatToString (make-honu:type-func #f
                                  (make-honu:type-prim #f 'float)
                                  (make-honu:type-prim #f 'string)))
          (cons #'charToString  (make-honu:type-func #f
                                  (make-honu:type-prim #f 'char)
                                  (make-honu:type-prim #f 'string)))
          (cons #'stringToInt   (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-prim #f 'int)))
          (cons #'stringToFloat (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-prim #f 'float)))
          (cons #'strlen        (make-honu:type-func #f
                                  (make-honu:type-prim #f 'string)
                                  (make-honu:type-prim #f 'int)))
          (cons #'substr        (make-honu:type-func #f
                                  (make-honu:type-tuple #f
                                    (list (make-honu:type-prim #f 'string)
                                          (make-honu:type-prim #f 'int)
                                          (make-honu:type-prim #f 'int)))
                                  (make-honu:type-prim #f 'string)))
          (cons #'charAt        (make-honu:type-func #f
                                  (make-honu:type-tuple #f
                                    (list (make-honu:type-prim #f 'string)
                                          (make-honu:type-prim #f 'int)))
                                  (make-honu:type-prim #f 'char)))
          (cons #'sqrt          (make-honu:type-func #f
                                  (make-honu:type-prim #f 'float)
                                  (make-honu:type-prim #f 'float)))
          ))
 
  (provide tenv?)
  (define tenv? bound-identifier-mapping?)

  (provide/contract [printable-key (identifier? . -> . symbol?)]
                    [tenv-key=?    (identifier? identifier? . -> . any)]
                    [tenv-key<?    (identifier? identifier? . -> . any)]
                    [tenv-map      (tenv?
                                    (identifier? tenv:entry? . -> . any)
                                    . -> .
                                    list?)]
                    [tenv-for-each (tenv?
                                    (identifier? tenv:entry? . -> . void?)
                                    . -> .
                                    void?)])
  (define printable-key syntax-e)
  (define tenv-key=? bound-identifier=?)
  (define (tenv-key<? k1 k2)
    (string<? (symbol->string (syntax-e k1))
              (symbol->string (syntax-e k2))))
  (define tenv-map bound-identifier-mapping-map)
  (define tenv-for-each bound-identifier-mapping-for-each)

  (provide/contract [empty-tenv (-> tenv?)]
                    [get-builtin-lenv (-> tenv?)]
                    [extend-tenv (identifier? tenv:entry? . -> . void?)]
                    [extend-lenv (identifier? tenv:value? . -> . void?)]
                    [extend-tenv-without-checking (identifier? tenv:entry? . -> . void?)])
  
  (define (empty-tenv) (make-bound-identifier-mapping))
  (define (get-builtin-lenv)
    (create-tenv (map car builtin-list)
                 (map (lambda (p)
                        (make-tenv:value (car p) (cdr p))) builtin-list)))
  (define (extend-tenv key val)
    (extend-tenv/checks key val (current-type-environment)))
  (define (extend-lenv key val)
    (extend-tenv/checks key val (current-lexical-environment)))
  (define (extend-tenv-without-checking key val)
    (extend-tenv/no-checks key val (current-type-environment)))

  (define (extend-tenv/checks key val tenv)
    (if (bound-identifier-mapping-get tenv key (lambda () #f))
        (if (eqv? (string-ref (symbol->string (printable-key key)) 0) #\$)
            (raise-read-error-with-stx
             (format "~a already bound by a subclass or substruct"
                     (substring (symbol->string (printable-key key)) 1))
             key)
            (raise-read-error-with-stx
             (format "~a already bound by top-level definition" (printable-key key))
             key))
        (bound-identifier-mapping-put! tenv key val)))
  (define (extend-tenv/no-checks key val tenv)
    (bound-identifier-mapping-put! tenv key val))
  (define (create-tenv keys vals)
    (let ((table (empty-tenv)))
      (begin (for-each (lambda (k v)
                         (extend-tenv/checks k v table))
                       keys vals)
             table)))

  ;; only use this if you a) don't want an error or b) don't know what you should get.
  (provide/contract [get-tenv-entry   (identifier? . -> . (union tenv:entry? false/c))]
                    [get-lenv-entry   (identifier? . -> . (union tenv:entry? false/c))])
  (define (get-tenv-entry key)
    (bound-identifier-mapping-get (current-type-environment) key (lambda () #f)))
  (define (get-lenv-entry key)
    (bound-identifier-mapping-get (current-lexical-environment) key (lambda () #f)))
  
  (provide/contract [get-type-entry   ((union honu:type-iface?
                                              honu:type-iface-top?) . -> . tenv:type?)]
                    [get-class-entry  (identifier?                  . -> . tenv:class?)]
                    [get-mixin-entry  (identifier?                  . -> . tenv:mixin?)]
                    [get-member-type  ((union honu:type-iface?
                                              honu:type-iface-top?)
                                       identifier?                  . -> . honu:type?)]
                    [get-value-entry  (identifier?                  . -> . tenv:value?)])
  (define (get-type-entry type)
    (if (honu:type-iface-top? type)
      (make-tenv:type #f (list) (list) (list))
      (let* ([name (honu:type-iface-name type)]
             [entry (get-tenv-entry name)])
        (cond
          [(not entry)
           (raise-read-error-with-stx
            (format "No type defined with name ~a" (printable-key name))
            name)]
          [(not (tenv:type? entry))
           (raise-read-error-with-stx
            (format "Definition of ~a is not a type" (printable-key name))
            name)]
          [else entry]))))
  (define (get-class-entry name)
    (let ([entry (get-tenv-entry name)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No class defined with name ~a" (printable-key name))
          name)]
        [(not (tenv:class? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a class" (printable-key name))
          name)]
        [else entry])))
  (define (get-mixin-entry name)
    (let ([entry (get-tenv-entry name)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No mixin defined with name ~a" (printable-key name))
          name)]
        [(not (tenv:mixin? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a mixin" (printable-key name))
          name)]
        [else entry])))
  (define (get-member-type type name)
    (let* ([entry (get-type-entry type)]
           [mtype (find (lambda (m)
                          (tenv-key=? (tenv:member-name m) name))
                        (append (tenv:type-members entry)
                                (tenv:type-inherited entry)))])
      (if mtype
          (tenv:member-type mtype)
          (raise-read-error-with-stx
           (format "No member named ~a found for type ~a"
                   (printable-key name)
                   (if (honu:type-iface-top? type)
                       'Any
                       (printable-key (honu:type-iface-name type))))
           name))))
  (define (get-value-entry name)
    (let ([entry (get-lenv-entry name)])
      (cond
        [(not entry)
         (raise-read-error-with-stx
          (format "No function or top-level binding defined with name ~a" (printable-key name))
          name)]
        [(not (tenv:value? entry))
         (raise-read-error-with-stx
          (format "Definition of ~a is not a function definition or value binding" (printable-key name))
          name)]
        [else entry])))
  
  (provide wrap-lenv extend-fenv)
  
  (define (wrap-lenv) (wrap-as-function (current-lexical-environment)))
  (define (wrap-as-function tenv)
    (lambda (name)
      (let ([entry (bound-identifier-mapping-get tenv name (lambda () #f))])
        (if entry (tenv:value-type entry) #f))))
  
  (define (extend-fenv key value fenv)
    (lambda (name)
      (if (tenv-key=? key name)
          value
          (fenv name))))

  )
