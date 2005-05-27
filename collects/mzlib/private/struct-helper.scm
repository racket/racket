(module struct-helper mzscheme
  (require (lib "list.ss"))
  (require-for-template mzscheme)
  (provide (all-defined))
  
  (define-struct field-decl (field ref mut posn immutable? auto?) (make-inspector))
  
  (define (sym+ . items)
    (define (->string x)
      (cond [(string? x) x]
            [(symbol? x) (symbol->string x)]
            [(identifier? x) (symbol->string (syntax-e x))]))
    (string->symbol (apply string-append (map ->string items))))
  
  (define (identifier/tf? stx)
    (or (identifier? stx) 
        (not stx) 
        (eq? (syntax-e stx) #t)
        (eq? (syntax-e stx) #f)))
  
  (define (id/tf stx stx2)
    (cond [(identifier? stx)
           stx]
          [(eq? (syntax-e stx) #t)
           stx2]
          [else #f]))
  
  (define (mk-parse-field-decl name-id)
    (define (parse-field-decl stx)
      (syntax-case stx ()
        [(field (flag ...) ref mut)
         (and (identifier? #'field)
              (identifier/tf? #'ref)
              (identifier/tf? #'mut)
              (andmap identifier? (syntax->list #'(flag ...))))
         (let ((flags (syntax-object->datum #'(flag ...))))
           (make-field-decl
            (id/tf #'field #f)
            (id/tf #'ref (datum->syntax-object name-id (sym+ name-id '- #'field)))
            (id/tf #'mut (datum->syntax-object name-id (sym+ 'set- name-id '- #'field '!)))
            #f
            (memq 'immutable flags)
            (memq 'auto flags)))]
        [(field (flag ...) ref)
         (parse-field-decl #'(field (flag ...) ref #t))]
        [(field (flag ...))
         (parse-field-decl
          #`(field 
             (flag ...)
             #t
             #t))]
        [field
         (identifier? #'field)
         (parse-field-decl
          #`(field () #t #t))]))
    (lambda (stx)
      (let ((r (parse-field-decl stx)))
        #;(printf "parse-field-decl returned ~s~n" r)
        r)))
  
  (define-struct decl:super (super struct:super))
  (define-struct decl:auto (value))
  (define-struct decl:property (key value))
  (define-struct decl:inspector (value))
  (define-struct decl:procedure-field (field))
  (define-struct decl:procedure (value))
  (define-struct decl:guard (value))
  (define-struct decl:option (value))
  
  (define (fetch-struct:super type)
    (let ((struct-info (syntax-local-value type)))
      (car struct-info)))
  
  (define (parse-decl stx)
    (syntax-case stx (super struct:super
                            auto-value property inspector transparent
                            procedure procedure-field guard
                            omit-define-values
                            omit-static-info
                            clone
                            replace
                            )
      [(super type)
       (identifier? #'type)
       (make-decl:super #'type (fetch-struct:super #'type))]
      [(struct:super value)
       (make-decl:super #f #'value)]
      [(auto-value value)
       (make-decl:auto #'value)]
      [(property key value)
       (make-decl:property #'key #'value)]
      [(inspector value)
       (make-decl:inspector #'value)]
      [transparent
       (make-decl:inspector #'(make-inspector))]
      [(procedure proc)
       (make-decl:procedure #'proc)]
      [(procedure-field field)
       (identifier? #'field)
       (make-decl:procedure-field #'field)]
      [(guard proc)
       (make-decl:guard #'proc)]
      [omit-define-values
       (make-decl:option 'omit-define-values)]
      [omit-static-info
       (make-decl:option 'omit-static-info)]
      [clone
       (make-decl:option 'include-clone)]
      [replace
       (make-decl:option 'include-replace)]))
  
  (define-struct info (type super auto-k auto-v 
                            props insp proc-spec imm-k-list guard
                            ref-fields ref-posns ref-names
                            mut-fields mut-posns mut-names
                            options fdecls))
  (define (make-null-info type)
    (make-info type #f 0 #f
               '() #f #f '() #f
               '() '() '()
               '() '() '()
               '() '()))
  (define (create-info type decls field-decls)
    (let ((info (make-null-info type)))
      (let loop ((fdecls field-decls) (posn 0) (first-auto #f))
        (if (pair? fdecls)
            (let ((fdecl (car fdecls)))
              (set-field-decl-posn! fdecl posn)
              (when (and first-auto (not (field-decl-auto? fdecl)))
                (raise-syntax-error 'define-struct* 
                                    "non-auto field came after auto field"
                                    (field-decl-field fdecl)))
              (when (field-decl-ref fdecl)
                (set-info-ref-fields! info 
                                      (cons (field-decl-field fdecl) (info-ref-fields info)))
                (set-info-ref-posns! info 
                                     (cons posn (info-ref-posns info)))
                (set-info-ref-names! info 
                                     (cons (field-decl-ref fdecl) (info-ref-names info))))
              (when (field-decl-mut fdecl)
                (set-info-mut-fields! info 
                                      (cons (field-decl-field fdecl) (info-mut-fields info)))
                (set-info-mut-posns! info 
                                     (cons posn (info-mut-posns info)))
                (set-info-mut-names! info 
                                     (cons (field-decl-mut fdecl) (info-mut-names info))))
              (loop (cdr fdecls)
                    (add1 posn)
                    (or first-auto (if (field-decl-auto? fdecl) posn #f))))
            (begin (set-info-auto-k! info 
                                     (if first-auto (- posn first-auto) 0)))))
      (set-info-ref-fields! info (reverse (info-ref-fields info)))
      (set-info-ref-posns! info (reverse (info-ref-posns info)))
      (set-info-ref-names! info (reverse (info-ref-names info)))
      (set-info-mut-fields! info (reverse (info-mut-fields info)))
      (set-info-mut-posns! info (reverse (info-mut-posns info)))
      (set-info-mut-names! info (reverse (info-mut-names info)))
      (set-info-fdecls! info field-decls)
      (for-each 
       (lambda (decl)
         (cond [(decl:super? decl) (set-info-super! info decl)]
               [(decl:auto? decl) (set-info-auto-v! info (decl:auto-value decl))]
               [(decl:property? decl)
                (set-info-props! info (cons (cons (decl:property-key decl)
                                                  (decl:property-value decl))
                                            (info-props info)))]
               [(decl:inspector? decl)
                (set-info-insp! info (decl:inspector-value decl))]
               [(decl:procedure? decl)
                (set-info-proc-spec! info (decl:procedure-value decl))]
               [(decl:procedure-field? decl)
                (set-info-proc-spec! 
                 info
                 (let loop ((fields (map field-decl-field field-decls)) (i 0))
                   (cond
                     [(null? fields)
                      (raise-syntax-error 'define-struct*
                                          "procedure-field not in field set"
                                          (decl:procedure-field-field decl))]
                     [(module-identifier=? (decl:procedure-field-field decl)
                                           (car fields))
                      i]
                     [else (loop (cdr fields) (add1 i))])))]
               [(decl:guard? decl)
                (set-info-guard! info (decl:guard-value decl))]
               [(decl:option? decl)
                (set-info-options! info (cons (decl:option-value decl)
                                              (info-options info)))]
               ))
       decls)
      (when (and (info-include-replacers? info) (pair? (info-auto-fields info)))
        (error 'define-struct* "cannot define replacers with auto-fields"))
      info))
  
  (define (info-init-fields info)
    (filter (lambda (fdecl) (not (field-decl-auto? fdecl)))
            (info-fdecls info)))
  (define (info-auto-fields info)
    (filter (lambda (fdecl) (field-decl-auto? fdecl))
            (info-fdecls info)))
  
  (define (info-include-define-values? info)
    (not (memq 'omit-define-values (info-options info))))
  
  (define (info-include-static-info? info)
    (not (memq 'omit-static-info (info-options info))))
  (define (info-include-replacers? info)
    (memq 'include-replace (info-options info)))
  (define (info-include-clone? info)
    (memq 'include-clone (info-options info)))
  
  (define (info-include-x-ref? info)
    #f)
  (define (info-include-x-set!? info)
    #f)
  
  
  (define (info-name:struct-record info)
    (let ((type (info-type info)))
      (datum->syntax-object type (sym+ 'struct: type))))
  (define (info-name:constructor info)
    (let ((type (info-type info)))
      (datum->syntax-object type (sym+ 'make- type))))
  (define (info-name:predicate info)
    (let ((type (info-type info)))
      (datum->syntax-object type (sym+ type '?))))
  (define (info-defined-names info)
    (let ((type (info-type info)))
      (append (list (info-name:struct-record info)
                    (info-name:constructor info)
                    (info-name:predicate info))
              (info-ref-names info)
              (info-mut-names info))))
  
  )
