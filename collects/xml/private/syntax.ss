(module syntax mzscheme
  (provide syntax-structs@)
  (require (lib "unitsig.ss")
           "sig.ss")
  
  ; to make error-raising functions named like structure mutators
  (define-syntax (struct! stx)
    (syntax-case stx ()
      [(struct-src name (field ...))
       (with-syntax ([struct:name (datum->syntax-object
                                   (syntax name)
                                   (string->symbol (string-append "struct:" (symbol->string (syntax-object->datum (syntax name))))))]
                     [(setter-name ...)
                      (let ([struct-name
                             (symbol->string (syntax-object->datum (syntax name)))])
                        (map (lambda (field-name)
                               (datum->syntax-object
                                field-name
                                (string->symbol
                                 (string-append
                                  "set-"
                                  struct-name
                                  "-"
                                  (symbol->string (syntax-object->datum field-name))
                                  "!"))))
                             (syntax->list (syntax (field ...)))))])
         (syntax
          (begin
            (define struct:name void)
            (define (setter-name s v)
              (error (quote setter-name) "cannot mutate XML syntax"))
            ...)))]))
  
  (define syntax-structs@
    (unit/sig xml-structs^
      (import)
      
      ; The locations from the two sets of structures shouldn't mingle, so I'm
      ; re-defining the location structure.  Maybe this is not a good idea, but I
      ; think it's okay.
      (define-struct location (line char offset))
      (define-struct source (start stop))
      
      ; make-document : prolog element ? -> document
      (define (make-document p e ?) e)
      
      ; make-prolog : ? #f -> prolog
      (define (make-prolog ? ??) #f)
      
      ; make-element : src src sym (listof attribute) (listof content) -> element
      (define (make-element from to name attrs content)
        (wrap (list* name attrs content) from to))
      
      ; make-pcdata : src src str -> pcdata
      (define (make-pcdata from to x)
        (wrap x from to))
      
      ; make-entity : src src (U sym num) -> entity
      (define (make-entity from to entity)
        (wrap entity from to))
      
      ; make-comment : str -> comment
      ; There is no syntax object representation for comments
      (define (make-comment x) #f)
      
      ; make-pi : src src sym str -> pi
      ; There's not really a syntax object representation for pi's either
      (define (make-pi from to name val) #f)
      
      ; make-attribute : src src sym str -> attribute
      (define (make-attribute from to name val)
        (wrap (list name val) from to))
      
      (define (make-document-type . x) #f)
      (define (make-external-dtd . x) #f)
      (define (make-external-dtd/public . x) #f)
      (define (make-external-dtd/system . x) #f)
      
      ; wrap : tst src src -> syntax
      (define (wrap x from to)
        (datum->syntax-object #f x (position from to)))
      
      ; position : src src -> (list #f nat nat nat nat)
      (define (position from to)
        (let ([start-offset (location-offset from)])
          (list #f (location-line from) (location-char from) start-offset
                (- (location-offset to) start-offset))))
      
      ; : syntax -> syntax
      (define (attribute-name a) (car (syntax->list a)))
      (define (attribute-value a) (cadr (syntax->list a)))
      
      ; : syntax -> syntax
      (define (element-name e) (car (syntax->list e)))
      (define (element-attributes e) (cadr (syntax->list e)))
      (define (element-content e) (cddr (syntax->list e)))
      
      (define (entity-text e) (syntax-e e))
      
      (define (pcdata-string x) (syntax-e x))
      
      (define (comment-text c)
        (error 'comment-text "expected a syntax representation of an XML comment, received ~a" c))
      ; conflate documents with their root elements
      (define (document-element d) d)
      ; more here - spoof document pieces better?
      (define (document-misc d) null)
      (define (document-prolog d) null)
      
      (define (document-type-external dtd)
        (error 'document-type-external "expected a dtd, given ~a" dtd))
      
      (define (document-type-inlined dtd)
        (error 'document-type-inlined "expected a dtd, given ~a" dtd))
      
      (define (document-type-name dtd)
        (error 'document-type-name "expected a dtd, given ~a" dtd))
      
      (define (external-dtd-system x)
        (error 'external-dtd-system "expected an external dtd, given ~a" x))
      
      (define (external-dtd/public-public x)
        (error 'external-dtd/public-public "expected an external dtd, given ~a" x))
      
      (define (pi-instruction x)
        (error 'pi-instruction "expected a pi, given ~a" x))
      
      (define (pi-target-name x)
        (error 'pi-target-name "expected a pi, given ~a" x))
      
      (define (prolog-dtd x)
        (error 'prolog-dtd "expected a prolog, given ~a" x))
      
      (define (prolog-misc x)
        (error 'prolog-misc "expected a prolog, given ~a" x))
      
      (define (prolog-misc2 x)
        (error 'prolog-misc2 "expected a prolog, given ~a" x))
      
      ; : tst -> bool
      (define (attribute? a)
        (and (syntax? a)
             (let ([x (syntax-object->datum a)])
               (and (pair? x) (symbol? (car x))
                    (pair? (cdr x)) (string? (cadr x))
                    (null? (cddr x))))))
      
      
      ; : tst -> bool
      (define (comment? x) #f)
      
      ; : tst -> bool
      (define (content? x)
        (and (syntax? x)
             (or (string? (syntax-object->datum x))
                 (element? x))))
      
      ; : tst -> bool
      (define (element? x)
        (and (syntax? x)
             (let ([e (syntax-e x)])
               (and (pair? e) (symbol? (car e))
                    (pair? (cdr e)) (list? (cadr e))
                    (andmap attribute? (cadr e))
                    (list? (cddr e))
                    (andmap content? (cddr e))))))
      
      ; : tst -> bool
      (define document? element?)
      
      ; : tst -> bool
      (define (document-type? x) #f)
      
      ; : tst -> bool
      (define (external-dtd/public? x) #f)
      (define (external-dtd/system? x) #f)
      (define (external-dtd? x) #f)
      
      (define (prolog? x) #f)
      (define (pi? x) #f)
      
      ; : tst -> bool
      (define (pcdata? x)
        (and (syntax? x) (string (syntax-e x))))
      
      ; : tst -> bool
      (define (entity? x)
        (and (syntax? x) (let ([r (syntax-e x)]) (or (symbol? r) (number? r)))))
      
      ;(struct! location (line char offset))
      (struct! document (prolog element misc))
      (struct! comment (text))
      (struct! prolog (misc dtd misc2))
      (struct! document-type (name external inlined))
      (struct! external-dtd (system))
      (struct! external-dtd/public (public))
      (struct! external-dtd/system ())
      (struct! element (name attributes content))
      (struct! attribute (name value))
      (struct! pi (target-name instruction))
      ;(struct! source (start stop))
      (struct! pcdata (string))
      (struct! entity (text))
      
      )))
