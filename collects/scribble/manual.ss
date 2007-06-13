
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
                                                (hspace 2)
                                                ""))
  (define-code SCHEMEBLOCK (to-paragraph/prefix (hspace 2) 
                                                (hspace 2)
                                                "")
    UNSYNTAX)
  (define-code SCHEMEBLOCK0 to-paragraph UNSYNTAX)
  (define-code schemeinput (to-paragraph/prefix (make-element
                                                 #f
                                                 (list
                                                  (hspace 2)
                                                  (make-element 'tt (list "> " ))))
                                                (hspace 4)
                                                ""))

  (define-syntax (schememod stx)
    (syntax-case stx ()
      [(_ lang rest ...)
       (with-syntax ([modtag (datum->syntax-object
                              #'here
                              `(unsyntax (schemefont ,(format "#module ~a" (syntax-e #'lang))))
                              #'lang)])
         #'(schemeblock modtag rest ...))]))

  (define (to-element/result s)
    (make-element "schemeresult" (list (to-element/no-color s))))
  (define (to-element/id s)
    (make-element "schemesymbol" (list (to-element/no-color s))))

  (define (keep-s-expr ctx s v) s)
  (define (add-sq-prop s name val)
    (if (eq? name 'paren-shape)
        (make-shaped-parens s val)
        s))

  (define-code scheme to-element unsyntax keep-s-expr add-sq-prop)
  (define-code schemeresult to-element/result unsyntax keep-s-expr add-sq-prop)
  (define-code schemeid to-element/id unsyntax keep-s-expr add-sq-prop)
  (define-code schememodname to-element unsyntax keep-s-expr add-sq-prop)

  (define (litchar . strs)
    (unless (andmap string? strs)
      (raise-type-error 'litchar "strings" strs))
    (let ([s (apply string-append
                    (map (lambda (s) (if (string=? s "\n") " " s))
                         strs))])
      (let ([spaces (regexp-match-positions #rx"^ *" s)]
            [end-spaces (regexp-match-positions #rx" *$" s)])
        (make-element "schemeinput" 
                      (list (hspace (cdar spaces))
                            (make-element #f (list (substring s (cdar spaces) (caar end-spaces))))
                            (hspace (- (cdar end-spaces) (caar end-spaces))))))))

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
           schemeparenfont schemekeywordfont schememetafont
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
  (define/kw (schememetafont #:body str)
    (make-element "schememeta" (decode-content str)))
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

  (provide margin-note)
  
  (define (margin-note . c)
    (make-styled-paragraph (list (make-element "refcontent"
                                               c))
                           "refpara"))

  ;; ----------------------------------------

  (provide defproc defproc* defstruct defthing defform defform* defform/subs defform*/subs defform/none
           specform specform/subs 
           specsubform specspecsubform specsubform/inline
           schemegrammar
           var svar void-const undefined-const)

  (define void-const
    (schemeresultfont "#<void>"))
  (define undefined-const
    (schemeresultfont "#<undefined>"))

  (define dots0
    (make-element "schememeta" (list "...")))
  (define dots1
    (make-element "schememeta" (list "...+")))

  (define-syntax (arg-contract stx)
    (syntax-case stx (... ...+)
      [(_ [id contract])
       (identifier? #'id)
       #'(schemeblock0 contract)]
      [(_ [id contract val])
       (identifier? #'id)
       #'(schemeblock0 contract)]
      [(_ [kw id contract])
       (and (keyword? (syntax-e #'kw))
            (identifier? #'id))
       #'(schemeblock0 contract)]
      [(_ [kw id contract val])
       (and (keyword? (syntax-e #'kw))
            (identifier? #'id))
       #'(schemeblock0 contract)]
      [(_ (... ...))
       #'#f]
      [(_ (... ...+))
       #'#f]
      [(_ arg)
       (raise-syntax-error
        'defproc
        "bad argument form"
        #'arg)]))

  (define-syntax defproc 
    (syntax-rules ()
      [(_ (id arg ...) result desc ...)
       (*defproc '[(id arg ...)]
                 (list (list (lambda () (arg-contract arg)) ...))
                 (list (lambda () (schemeblock0 result))) 
                 (lambda () (list desc ...)))]))
  (define-syntax defproc* 
    (syntax-rules ()
      [(_ [[(id arg ...) result] ...] desc ...)
       (*defproc '[(id arg ...) ...] 
                 (list (list (lambda () (arg-contract arg)) ...) ...)
                 (list (lambda () (schemeblock0 result)) ...)
                 (lambda () (list desc ...)))]))
  (define-syntax defstruct
    (syntax-rules ()
      [(_ name fields desc ...)
       (*defstruct 'name 'fields (lambda () (list desc ...)))]))
  (define-syntax (defform*/subs stx)
    (syntax-case stx ()
      [(_ [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
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
         #'(*defforms #t
                      '(spec spec1 ...) 
                      (list (lambda (x) (schemeblock0 new-spec))
                            (lambda (ignored) (schemeblock0 spec1)) ...)
                      '((non-term-id non-term-form ...) ...)
                      (list (list (lambda () (scheme non-term-id))
                                  (lambda () (schemeblock0 non-term-form))
                                  ...)
                            ...)
                      (lambda () (list desc ...))))]))
  (define-syntax (defform* stx)
    (syntax-case stx ()
      [(_ [spec ...] desc ...) #'(defform*/subs [spec ...] () desc ...)]))
  (define-syntax (defform stx)
    (syntax-case stx ()
      [(_ spec desc ...) #'(defform*/subs [spec] () desc ...)]))
  (define-syntax (defform/subs stx)
    (syntax-case stx ()
      [(_ spec subs desc ...) #'(defform*/subs [spec] subs desc ...)]))
  (define-syntax (defform/none stx)
    (syntax-case stx ()
      [(_ spec desc ...)
       #'(*defforms #f 
                    '(spec) (list (lambda (ignored) (schemeblock0 spec))) 
                    null null
                    (lambda () (list desc ...)))]))
  (define-syntax specsubform
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec #f (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]))
  (define-syntax specspecsubform
    (syntax-rules ()
      [(_ spec desc ...)
       (make-blockquote "leftindent" (list (specsubform spec desc ...)))]))
  (define-syntax specform
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec #t (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]))
  (define-syntax specform/subs
    (syntax-rules ()
      [(_ spec ([non-term-id non-term-form ...] ...) desc ...)
       (*specsubform 'spec #t 
                     (lambda () (schemeblock0 spec)) 
                     '((non-term-id non-term-form ...) ...)
                     (list (list (lambda () (scheme non-term-id))
                                 (lambda () (schemeblock0 non-term-form))
                                 ...)
                           ...)
                     (lambda () (list desc ...)))]))
  (define-syntax specsubform/inline
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec #f #f null null (lambda () (list desc ...)))]))
  (define-syntax defthing
    (syntax-rules ()
      [(_ id result desc ...)
       (*defthing 'id 'result (lambda () (list desc ...)))]))
  (define-syntax schemegrammar
    (syntax-rules ()
      [(_ #:literals (lit ...) id clause ...) (*schemegrammar '(lit ...) 
                                                              '(id clause ...)
                                                              (lambda () (list (scheme id) (schemeblock0 clause) ...)))]
      [(_ id clause ...) (schemegrammar #:literals () id clause ...)]))
  (define-syntax var
    (syntax-rules ()
      [(_ id) (*var 'id)]))
  (define-syntax svar
    (syntax-rules ()
      [(_ id) (*var 'id)]))

  (define (make-table-if-necessary style content)
    (if (= 1 (length content))
        (let ([paras (apply append (map flow-paragraphs (car content)))])
          (if (andmap paragraph? paras)
              (list (make-paragraph (apply append (map paragraph-content paras))))
              (list (make-table style content))))
        (list (make-table style content))))

  (define (*defproc prototypes arg-contractss result-contracts content-thunk)
    (let ([spacer (hspace 1)]
          [has-optional? (lambda (arg)
                           (and (pair? arg)
                                ((length arg) . > . (if (keyword? (car arg))
                                                        3
                                                        2))))]
          [to-flow (lambda (e)
                     (make-flow (list (make-paragraph (list e)))))]
          [arg->elem (lambda (v)
                       (cond
                        [(pair? v)
                         (if (keyword? (car v))
                             (make-element #f (list (to-element (car v))
                                                    (hspace 1)
                                                    (to-element (cadr v))))
                             (to-element (car v)))]
                        [(eq? v '...+)
                         dots1]
                        [(eq? v '...)
                         dots0]
                        [else v]))])
      (parameterize ([current-variable-list
                      (map (lambda (i)
                             (and (pair? i)
                                  (if (keyword? (car i))
                                      (cadr i)
                                      (car i))))
                           (apply append (map cdr prototypes)))])
        (make-splice
         (cons
          (make-table
           'boxed
           (apply 
            append
            (map 
             (lambda (prototype arg-contracts result-contract first?)
               (append
                (list
                 (list (make-flow
                        (make-table-if-necessary
                         "prototype"
                         (list
                          (list
                           (to-flow
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
                                           (map arg->elem more-required)))))
                           (to-flow spacer)
                           (to-flow 'rarr)
                           (to-flow spacer)
                           (make-flow (list (result-contract)))))))))
                (apply append
                       (map (lambda (v arg-contract)
                              (cond
                               [(pair? v)
                                (list
                                 (list
                                  (make-flow
                                   (make-table-if-necessary
                                    "argcontract"
                                    (list
                                     (let ([v (if (keyword? (car v)) 
                                                  (cdr v)
                                                  v)])
                                       (append
                                        (list
                                         (to-flow (hspace 2))
                                         (to-flow (arg->elem v))
                                         (to-flow spacer)
                                         (to-flow ":")
                                         (to-flow spacer)
                                         (make-flow (list (arg-contract))))
                                        (if (has-optional? v)
                                            (list (to-flow spacer)
                                                  (to-flow "=")
                                                  (to-flow spacer)
                                                  (to-flow (to-element (caddr v))))
                                            null))))))))]
                               [else null]))
                            (cdr prototype)
                            arg-contracts))))
             prototypes
             arg-contractss
             result-contracts
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
                   `(,(schemeparenfont "struct")
                     ,(make-target-element*
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

  (define (meta-symbol? s) (memq s '(... ...+ ?)))

  (define (*defforms kw? forms form-procs subs sub-procs content-thunk)
    (parameterize ([current-variable-list
                    (apply 
                     append
                     (map (lambda (form)
                            (let loop ([form (cons (if kw? (cdr form) form)
                                                   subs)])
                              (cond
                               [(symbol? form) (if (meta-symbol? form)
                                                   null
                                                   (list form))]
                               [(pair? form) (append (loop (car form))
                                                     (loop (cdr form)))]
                               [else null])))
                          forms))]
                     [current-meta-list '(... ...+)])
      (make-splice
       (cons
        (make-table
         'boxed
         (append
          (map (lambda (form form-proc)
                 (list
                  (make-flow 
                   (list
                    ((or form-proc
                         (lambda (x)
                           (make-paragraph
                            (list
                             (to-element
                              `(,x
                                . ,(cdr form)))))))
                     (and kw?
                          (eq? form (car forms))
                          (make-target-element
                           #f
                           (list (to-element (car form)))
                           (register-scheme-form-definition (car form)))))))))
               forms form-procs)
          (apply
           append
           (map (lambda (sub)
                  (list (list (make-flow (list (make-paragraph (list (tt 'nbsp))))))
                        (list (make-flow (list (apply *schemerawgrammar 
                                                      (map (lambda (f) (f)) sub)))))))
                sub-procs))))
        (content-thunk)))))

  (define (*specsubform form has-kw? form-thunk subs sub-procs content-thunk)
    (parameterize ([current-variable-list
                    (append (let loop ([form (cons (if has-kw? (cdr form) form)
                                                   subs)])
                              (cond
                               [(symbol? form) (if (meta-symbol? form)
                                                   null
                                                   (list form))]
                               [(pair? form) (append (loop (car form))
                                                     (loop (cdr form)))]
                               [else null]))
                            (current-variable-list))]
                   [current-meta-list '(... ...+)])
      (make-blockquote
       "leftindent"
       (cons
        (make-table
         'boxed
         (cons
          (list
           (make-flow
            (list
             (if form-thunk
                 (form-thunk)
                 (make-paragraph (list (to-element form)))))))
          (apply
           append
           (map (lambda (sub)
                  (list (list (make-flow (list (make-paragraph (list (tt 'nbsp))))))
                        (list (make-flow (list (apply *schemerawgrammar 
                                                      (map (lambda (f) (f)) sub)))))))
                sub-procs))))
        (flow-paragraphs (decode-flow (content-thunk)))))))

  (define (*schemerawgrammar nonterm clause1 . clauses)
    (make-table
     '((valignment baseline baseline baseline baseline baseline)
       (alignment left left center left left))
     (let ([empty-line (make-flow (list (make-paragraph (list (tt 'nbsp)))))]
           [to-flow (lambda (i) (make-flow (list (make-paragraph (list i)))))])
       (cons
        (list (to-flow nonterm)
              empty-line
              (to-flow "=")
              empty-line
              (make-flow (list clause1)))
        (map (lambda (clause)
               (list empty-line
                     empty-line
                     (to-flow "|")
                     empty-line
                     (make-flow (list clause))))
             clauses)))))

  (define (*schemegrammar lits s-expr clauses-thunk)
    (parameterize ([current-variable-list
                    (let loop ([form s-expr])
                      (cond
                       [(symbol? form) (if (memq form lits)
                                           null
                                           (list form))]
                       [(pair? form) (append (loop (car form))
                                             (loop (cdr form)))]
                       [else null]))])
      (apply *schemerawgrammar (clauses-thunk))))

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

  (provide where-is-one-of
           is-one-of)

  (define (where-is-one-of id)
    (make-element #f (list "where " id " is one of")))

  (define (is-one-of id)
    (make-element #f (list id " is one of")))
  
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

  (provide cite)

  (define/kw (cite #:key key title author location date)
    "[...]"
    #;
    (make-bibliography-element
     #f
     (list "[...]")
     key
     (list (string-append
            (content->string (list author))
            ", "
            (content->string (list title))))
     (list (make-element #f (list author
                                  ", "
                                  title
                                  ", "
                                  date
                                  ". "
                                  location
                                  ".")))))

  ;; ----------------------------------------
  )
