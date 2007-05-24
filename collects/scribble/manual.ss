
(module manual mzscheme
  (require "decode.ss"
           "struct.ss"
           "scheme.ss"
           "config.ss"
           "basic.ss"
           (lib "string.ss")
           (lib "kw.ss")
           (lib "list.ss")
           (lib "class.ss"))

  (provide (all-from "basic.ss"))

  (provide PLaneT)
  (define PLaneT "PLaneT")

  (define-code schemeblock0 to-paragraph)
  (define-code schemeblock (to-paragraph/prefix (hspace 2) 
                                                (hspace 2)))
  (define-code SCHEMEBLOCK (to-paragraph/prefix (hspace 2) 
                                                (hspace 2))
    UNSYNTAX)
  (define-code SCHEMEBLOCK0 to-paragraph UNSYNTAX)
  (define-code schemeinput (to-paragraph/prefix (make-element
                                                 #f
                                                 (list
                                                  (hspace 2)
                                                  (make-element 'tt (list "> " ))))
                                                (hspace 4)))

  (define-syntax (schememod stx)
    (syntax-case stx ()
      [(_ lang rest ...)
       (with-syntax ([modtag (datum->syntax-object
                              #'here
                              '(unsyntax (schemefont "#module "))
                              #'lang)])
         #'(schemeblock modtag lang rest ...))]))

  (define (to-element/result s)
    (make-element "schemeresult" (list (to-element/no-color s))))
  (define (to-element/id s)
    (make-element "schemesymbol" (list (to-element/no-color s))))

  (define-code scheme to-element unsyntax (lambda (ctx s v) s))
  (define-code schemeresult to-element/result unsyntax (lambda (ctx s v) s))
  (define-code schemeid to-element/id unsyntax (lambda (ctx s v) s))
  (define-code schememodname to-element unsyntax (lambda (ctx s v) s))

  (define (litchar . strs)
    (unless (andmap string? strs)
      (raise-type-error 'litchar "strings" strs))
    (let ([s (apply string-append
                    (map (lambda (s) (if (string=? s "\n") " " s))
                         strs))])
      (let ([spaces (regexp-match-positions #rx"^ *" s)])
        (make-element "schemeinput" 
                      (list (hspace (cdar spaces))
                            (make-element 'tt (list (substring s (cdar spaces)))))))))

  (define (verbatim s)
    (let ([strs (regexp-split #rx"\n" s)])
      (make-table
       #f
       (map (lambda (s)
              (list (make-flow (list (make-paragraph 
                                      (let ([spaces (cdar (regexp-match-positions #rx"^ *" s))])
                                        (list 
                                         (hspace spaces)
                                         (make-element 'tt (list (substring s spaces))))))))))
            strs))))

  (provide schemeblock SCHEMEBLOCK
           schemeblock0 SCHEMEBLOCK0
           schemeinput
           schememod
           scheme schemeresult schemeid schememodname
           litchar
           verbatim)

  (provide onscreen menuitem defterm
           schemefont schemevalfont schemeresultfont schemeidfont 
           schemeparenfont schemekeywordfont
           file exec
           link procedure
           idefterm)

  (define/kw (onscreen #:body str)
    (make-element 'sf (decode-content str)))
  (define (menuitem menu item)
    (make-element 'sf (list menu "|" item)))
  (define/kw (defterm #:body str)
    (make-element 'italic (decode-content str)))
  (define/kw (idefterm #:body str)
    (let ([c (decode-content str)])
      (make-element 'italic c)))
  (define/kw (schemefont #:body str)
    (apply tt str))
  (define/kw (schemevalfont #:body str)
    (make-element "schemevalue" (decode-content str)))
  (define/kw (schemeresultfont #:body str)
    (make-element "schemeresult" (decode-content str)))
  (define/kw (schemeidfont #:body str)
    (make-element "schemesymbol" (decode-content str)))
  (define/kw (schemeparenfont #:body str)
    (make-element "schemeparen" (decode-content str)))
  (define/kw (schemekeywordfont #:body str)
    (make-element "schemekeyword" (decode-content str)))
  (define/kw (file #:body str)
    (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
  (define/kw (exec #:body str)
    (make-element 'tt (decode-content str)))
  (define/kw (procedure #:body str)
    (make-element "schemeresult" (append (list "#<procedure:") (decode-content str) (list ">"))))

  (define/kw (link url #:body str)
    (make-element (make-target-url url) (decode-content str)))

  (provide t)
  (define/kw (t #:body str)
    (decode-paragraph str))

  (provide schememodule)
  (define-syntax (schememodule stx)
    (syntax-rules ()
      [(_ body ...)
       (code body ...)]))

  ;; ----------------------------------------

  (provide defproc defproc* defstruct defthing defform 
           specsubform specsubform/inline
           var svar void-const)

  (define (void-const)
    "void")

  (define dots0
    (make-element #f (list "...")))
  (define dots1
    (make-element #f (list "..."  (superscript "+"))))

  (define-syntax defproc 
    (syntax-rules ()
      [(_ s-exp result desc ...)
       (*defproc '[s-exp] '[result] (lambda () (list desc ...)))]))
  (define-syntax defproc* 
    (syntax-rules ()
      [(_ [[s-exp result] ...] desc ...)
       (*defproc '[s-exp ...] '[result ...] (lambda () (list desc ...)))]))
  (define-syntax defstruct
    (syntax-rules ()
      [(_ name fields desc ...)
       (*defstruct 'name 'fields (lambda () (list desc ...)))]))
  (define-syntax (defform stx)
    (syntax-case stx ()
      [(_ spec desc ...)
       (with-syntax ([new-spec
                      (syntax-case #'spec ()
                        [(name . rest)
                         (datum->syntax-object #'spec
                                               (cons
                                                (datum->syntax-object #'here
                                                                      '(unsyntax x)
                                                                      #'name)
                                                #'rest)
                                               #'spec)])])
         #'(*defform 'spec (lambda (x) (schemeblock0 new-spec)) (lambda () (list desc ...))))]))
  (define-syntax specsubform
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec (lambda () (schemeblock0 spec)) (lambda () (list desc ...)))]))
  (define-syntax specsubform/inline
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec #f (lambda () (list desc ...)))]))
  (define-syntax defthing
    (syntax-rules ()
      [(_ id result desc ...)
       (*defthing 'id 'result (lambda () (list desc ...)))]))
  (define-syntax var
    (syntax-rules ()
      [(_ id) (*var 'id)]))
  (define-syntax svar
    (syntax-rules ()
      [(_ id) (*var 'id)]))

  (define (*defproc prototypes results content-thunk)
    (let ([spacer (hspace 1)]
          [has-optional? (lambda (arg)
                           (and (pair? arg)
                                ((length arg) . > . (if (keyword? (car arg))
                                                        2
                                                        3))))]
          [arg->elem (lambda (v)
                       (cond
                        [(pair? v)
                         (if (keyword? (car v))
                             (make-element #f (list (to-element (car v))
                                                    (hspace 1)
                                                    (to-element (cadr v))))
                             (to-element (car v)))]
                        [(eq? v '...1)
                         dots1]
                        [(eq? v '...0)
                         dots0]
                        [else v]))])
      (parameterize ([current-variable-list
                      (map (lambda (i)
                             (and (pair? i)
                                  (car i)))
                           (apply append (map cdr prototypes)))])        
        (make-splice
         (cons
          (make-table
           'boxed
           (apply 
            append
            (map 
             (lambda (prototype result first?)
               (append
                (list
                 (list (make-flow
                        (list
                         (make-paragraph
                          (list
                           (let-values ([(required optional more-required)
                                         (let loop ([a (cdr prototype)][r-accum null])
                                           (if (or (null? a)
                                                   (and (has-optional? (car a))))
                                               (let ([req (reverse r-accum)])
                                                 (let loop ([a a][o-accum null])
                                                   (if (or (null? a)
                                                           (not (has-optional? (car a))))
                                                       (values req (reverse o-accum) a)
                                                       (loop (cdr a) (cons (car a) o-accum)))))
                                               (loop (cdr a) (cons (car a) r-accum))))])
                             (to-element (append
                                          (list (if first?
                                                    (make-target-element
                                                     #f
                                                     (list (to-element (car prototype)))
                                                     (register-scheme-definition (car prototype)))
                                                    (to-element (car prototype))))
                                          (map arg->elem required)
                                          (if (null? optional)
                                              null
                                              (list
                                               (to-element
                                                (syntax-property
                                                 (syntax-ize (map arg->elem optional) 0)
                                                 'paren-shape
                                                 #\?))))
                                          (map arg->elem more-required))))
                           (hspace 2)
                           'rarr
                           (hspace 2)
                           (to-element result)))))))
                (apply append
                       (map (lambda (v)
                              (cond
                               [(pair? v)
                                (list
                                 (list
                                  (make-flow
                                   (list
                                    (let ([v (if (keyword? (car v)) 
                                                 (cdr v)
                                                 v)])
                                      (make-paragraph (append
                                                       (list
                                                        (hspace 2)
                                                        (arg->elem v))
                                                       (list
                                                        spacer
                                                        ":"
                                                        spacer
                                                        (to-element (cadr v)))
                                                       (if (has-optional? v)
                                                           (list spacer
                                                                 "="
                                                                 spacer
                                                                 (to-element (caddr v)))
                                                           null))))))))]
                               [else null]))
                            (cdr prototype)))))
             prototypes
             results
             (cons #t (map (lambda (x) #f) (cdr prototypes))))))
          (content-thunk))))))

  (define (make-target-element* content wrappers)
    (if (null? wrappers)
        content
        (make-target-element*
         (make-target-element
          #f
          (list content)
          (register-scheme-definition (string->symbol
                                       (apply string-append
                                              (map symbol->string (car wrappers))))))
         (cdr wrappers))))

  (define (*defstruct name fields content-thunk)
    (define spacer (hspace 1))
    (make-splice
     (cons
      (make-table
       'boxed
       (cons
        (list (make-flow 
               (list
                (make-paragraph
                 (list
                  (to-element
                   `(struct ,(make-target-element*
                              (to-element name)
                              (let ([name (if (pair? name)
                                              (car name)
                                              name)])
                                (list* (list name)
                                       (list name '?)
                                       (list 'make- name)
                                       (append
                                        (map (lambda (f)
                                               (list name '- (car f)))
                                             fields)
                                        (map (lambda (f)
                                               (list 'set- name '- (car f) '!))
                                             fields)))))
                            ,(map car fields))))))))
        (map (lambda (v)
               (cond
                [(pair? v)
                 (list
                  (make-flow
                   (list
                    (make-paragraph (append
                                     (list
                                      (hspace 2)
                                      (to-element (car v)))
                                     (list
                                      spacer
                                      ":"
                                      spacer
                                      (to-element (cadr v))))))))]
                [else null]))
             fields)))
      (content-thunk))))

  (define (*defthing name result-contract content-thunk)
    (define spacer (hspace 1))
    (make-splice
     (cons
      (make-table
       'boxed
       (list
        (list (make-flow 
               (list
                (make-paragraph
                 (list (make-target-element
                        #f
                        (list (to-element name))
                        (register-scheme-definition name))
                       spacer ":" spacer
                       (to-element result-contract))))))))
      (content-thunk))))

  (define (*defform form form-proc content-thunk)
    (parameterize ([current-variable-list
                    (let loop ([form (cdr form)])
                      (cond
                       [(symbol? form) (list form)]
                       [(pair? form) (append (loop (car form))
                                             (loop (cdr form)))]
                       [else null]))])
      (make-splice
       (cons
        (make-table
         'boxed
         (list
          (list (make-flow 
                 (list
                  ((or form-proc
                       (lambda (x)
                         (make-paragraph
                          (list
                           (to-element
                            `(,x
                              . ,(cdr form)))))))
                   (make-target-element
                    #f
                    (list (to-element (car form)))
                    (register-scheme-form-definition (car form)))))))))
        (content-thunk)))))

  (define (*specsubform form form-thunk content-thunk)
    (parameterize ([current-variable-list
                    (let loop ([form form])
                      (cond
                       [(symbol? form) (list form)]
                       [(pair? form) (append (loop (car form))
                                             (loop (cdr form)))]
                       [else null]))])
      (make-splice
       (cons
        (if form-thunk
            (form-thunk)
            (to-element form))
        (content-thunk)))))

  (define (*var id)
    (to-element (*var-sym id)))

  (define (*var-sym id)
    (string->symbol (format "_~a" id)))

  ;; ----------------------------------------

  (provide centerline)
  (define/kw (centerline #:body s)
    (make-table 'centered (list (list (make-flow (list (decode-paragraph s)))))))

  (provide commandline)
  (define/kw (commandline #:body s)
    (make-paragraph (list (hspace 2) (apply tt s))))
           

  (define (secref s)
    (make-link-element #f null `(part ,s)))
  (define/kw (seclink tag #:body s)
    (make-link-element #f (decode-content s) `(part ,tag)))
  (define/kw (*schemelink id #:body s)
    (make-link-element #f (decode-content s) (register-scheme-definition id)))
  (define-syntax schemelink
    (syntax-rules ()
      [(_ id . content) (*schemelink 'id . content)]))
  (provide secref seclink schemelink)

  (define/kw (pidefterm #:body s)
    (let ([c (apply defterm s)])
      (index (string-append (content->string (element-content c)) "s") 
             c)))
  (provide pidefterm)
  
  ;; ----------------------------------------

  (provide math)
  (define/kw (math #:body s)
    (let ([c (decode-content s)])
      (make-element #f (apply append
                              (map (lambda (i)
                                     (let loop ([i i])
                                       (cond
                                        [(string? i)
                                         (let ([m (regexp-match #rx"^(.*)([()])(.*)$" i)])
                                           (if m
                                               (append (loop (cadr m))
                                                       (list (caddr m))
                                                       (loop (cadddr m)))
                                               (list (make-element 'italic (list i)))))]
                                        [else (list i)])))
                                   c)))))

  ;; ----------------------------------------
  )
