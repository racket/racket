
(module manual (lib "lang.ss" "big")
  (require "decode.ss"
           "struct.ss"
           "scheme.ss"
           "config.ss"
           "basic.ss"
           (lib "string.ss")
           (lib "list.ss")
           (lib "class.ss"))

  (provide (all-from "basic.ss"))

  (provide PLaneT)
  (define PLaneT "PLaneT")

  (provide etc)
  (define etc "etc.") ; so we can fix the latex space, one day

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

  (define (keep-s-expr ctx s v) 
    (if (symbol? s)
        (make-just-context s ctx)
        s))
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
        (make-element
         "schemeinputbg"
         (list (hspace (cdar spaces))
               (make-element "schemeinput" (list (substring s (cdar spaces) (caar end-spaces))))
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
           schemeparenfont schemekeywordfont schememetafont schememodfont
           file exec envvar Flag DFlag
           link procedure
           idefterm)

  (define (onscreen . str)
    (make-element 'sf (decode-content str)))
  (define (menuitem menu item)
    (make-element 'sf (list menu "|" item)))
  (define (defterm . str)
    (make-element 'italic (decode-content str)))
  (define (idefterm . str)
    (let ([c (decode-content str)])
      (make-element 'italic c)))
  (define (schemefont . str)
    (apply tt str))
  (define (schemevalfont . str)
    (make-element "schemevalue" (decode-content str)))
  (define (schemeresultfont . str)
    (make-element "schemeresult" (decode-content str)))
  (define (schemeidfont . str)
    (make-element "schemesymbol" (decode-content str)))
  (define (schemeparenfont . str)
    (make-element "schemeparen" (decode-content str)))
  (define (schememetafont . str)
    (make-element "schememeta" (decode-content str)))
  (define (schememodfont . str)
    (make-element "schememod" (decode-content str)))
  (define (schemekeywordfont . str)
    (make-element "schemekeyword" (decode-content str)))
  (define (file . str)
    (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
  (define (exec . str)
    (make-element 'tt (decode-content str)))
  (define (Flag . str)
    (make-element 'tt (cons "-" (decode-content str))))
  (define (DFlag . str)
    (make-element 'tt (cons "--" (decode-content str))))
  (define (envvar . str)
    (make-element 'tt (decode-content str)))
  (define (procedure . str)
    (make-element "schemeresult" (append (list "#<procedure:") (decode-content str) (list ">"))))

  (define (link url . str)
    (make-element (make-target-url url) (decode-content str)))

  (provide t)
  (define (t . str)
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

  (provide deftech tech techlink)

  (define (*tech make-elem style s)
    (let* ([c (decode-content s)]
           [s (regexp-replace* #px"[-\\s]+" 
                               (regexp-replace 
                                #rx"s$" 
                                (regexp-replace 
                                 #rx"ies$" 
                                 (string-foldcase (content->string c))
                                 "y")
                                "")
                               " ")])
      (make-elem style
                 c
                 (format "tech-term:~a" s))))

  (define (deftech . s)
    (*tech make-target-element #f (list (apply defterm s))))

  (define (tech . s)
    (*tech make-link-element "techlink" s))

  (define (techlink . s)
    (*tech make-link-element #f s))

  ;; ----------------------------------------

  (provide defproc defproc* defstruct defthing defparam defboolparam
           defform defform* defform/subs defform*/subs defform/none
           defidform
           specform specform/subs 
           specsubform specsubform/subs specspecsubform specspecsubform/subs specsubform/inline
           schemegrammar schemegrammar*
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
       (*defproc (list (quote-syntax id))
                 '[(id arg ...)]
                 (list (list (lambda () (arg-contract arg)) ...))
                 (list (lambda () (schemeblock0 result))) 
                 (lambda () (list desc ...)))]))
  (define-syntax defproc* 
    (syntax-rules ()
      [(_ [[(id arg ...) result] ...] desc ...)
       (*defproc (list (quote-syntax id) ...)
                 '[(id arg ...) ...] 
                 (list (list (lambda () (arg-contract arg)) ...) ...)
                 (list (lambda () (schemeblock0 result)) ...)
                 (lambda () (list desc ...)))]))
  (define-syntax defstruct
    (syntax-rules ()
      [(_ name fields #:immutable #:inspector #f desc ...)
       (**defstruct name fields #t #t desc ...)]
      [(_ name fields #:immutable desc ...)
       (**defstruct name fields #t #f desc ...)]
      [(_ name fields #:inspector #f desc ...)
       (**defstruct name fields #f #t desc ...)]
      [(_ name fields desc ...)
       (**defstruct name fields #f #f desc ...)]))
  (define-syntax **defstruct
    (syntax-rules ()
      [(_ name ([field field-contract] ...) immutable? transparent? desc ...)
       (*defstruct (quote-syntax name) 'name 
                   '([field field-contract] ...) (list (lambda () (schemeblock0 field-contract)) ...)
                   #t #t (lambda () (list desc ...)))]))
  (define-syntax (defform*/subs stx)
    (syntax-case stx ()
      [(_ #:literals (lit ...) [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
       (with-syntax ([new-spec
                      (syntax-case #'spec ()
                        [(name . rest)
                         (datum->syntax-object #'spec
                                               (cons
                                                (datum->syntax-object #'here
                                                                      '(unsyntax x)
                                                                      #'name)
                                                #'rest)
                                               #'spec)])]
                     [spec-id
                      (syntax-case #'spec ()
                        [(name . rest) #'name])])
         #'(*defforms (quote-syntax spec-id) '(lit ...)
                      '(spec spec1 ...) 
                      (list (lambda (x) (schemeblock0 new-spec))
                            (lambda (ignored) (schemeblock0 spec1)) ...)
                      '((non-term-id non-term-form ...) ...)
                      (list (list (lambda () (scheme non-term-id))
                                  (lambda () (schemeblock0 non-term-form))
                                  ...)
                            ...)
                      (lambda () (list desc ...))))]
      [(fm [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
       #'(fm #:literals () [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)]))
  (define-syntax (defform* stx)
    (syntax-case stx ()
      [(_ #:literals lits [spec ...] desc ...) #'(defform*/subs #:literals lits [spec ...] () desc ...)]
      [(_ [spec ...] desc ...) #'(defform*/subs [spec ...] () desc ...)]))
  (define-syntax (defform stx)
    (syntax-case stx ()
      [(_ spec desc ...) #'(defform*/subs [spec] () desc ...)]))
  (define-syntax (defform/subs stx)
    (syntax-case stx ()
      [(_ #:literals lits spec subs desc ...) #'(defform*/subs #:literals lits [spec] subs desc ...)]
      [(_ spec subs desc ...) #'(defform*/subs [spec] subs desc ...)]))
  (define-syntax (defform/none stx)
    (syntax-case stx ()
      [(_ spec desc ...)
       #'(*defforms #f null
                    '(spec) (list (lambda (ignored) (schemeblock0 spec))) 
                    null null
                    (lambda () (list desc ...)))]))
  (define-syntax (defidform stx)
    (syntax-case stx ()
      [(_ spec-id desc ...)
       #'(*defforms (quote-syntax spec-id) null
                    '(spec-id)
                    (list (lambda (x) (make-paragraph (list x))))
                    null
                    null
                    (lambda () (list desc ...)))]))
  (define-syntax specsubform
    (syntax-rules ()
      [(_ #:literals (lit ...) spec desc ...)
       (*specsubform 'spec #f '(lit ...) (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]
      [(_ spec desc ...)
       (*specsubform 'spec #f null (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]))
  (define-syntax specsubform/subs
    (syntax-rules ()
      [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...) desc ...)
       (*specsubform 'spec #f '(lit ...) (lambda () (schemeblock0 spec)) 
                     '((non-term-id non-term-form ...) ...)
                     (list (list (lambda () (scheme non-term-id))
                                 (lambda () (schemeblock0 non-term-form))
                                 ...)
                           ...)
                     (lambda () (list desc ...)))]
      [(_ spec subs desc ...)
       (specsubform/subs #:literals () spec subs desc ...)]))
  (define-syntax specspecsubform
    (syntax-rules ()
      [(_ spec desc ...)
       (make-blockquote "leftindent" (list (specsubform spec desc ...)))]))
  (define-syntax specspecsubform/subs
    (syntax-rules ()
      [(_ spec subs desc ...)
       (make-blockquote "leftindent" (list (specsubform/subs spec subs desc ...)))]))
  (define-syntax specform
    (syntax-rules ()
      [(_ #:literals (lit ...) spec desc ...)
       (*specsubform 'spec #t '(lit ...) (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]
      [(_ spec desc ...)
       (*specsubform 'spec #t null (lambda () (schemeblock0 spec)) null null (lambda () (list desc ...)))]))
  (define-syntax specform/subs
    (syntax-rules ()
      [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...) desc ...)
       (*specsubform 'spec #t 
                     '(lit ...)
                     (lambda () (schemeblock0 spec)) 
                     '((non-term-id non-term-form ...) ...)
                     (list (list (lambda () (scheme non-term-id))
                                 (lambda () (schemeblock0 non-term-form))
                                 ...)
                           ...)
                     (lambda () (list desc ...)))]
      [(_ spec ([non-term-id non-term-form ...] ...) desc ...)
       (specform/subs #:literals () spec ([non-term-id non-term-form ...] ...) desc ...)]))
  (define-syntax specsubform/inline
    (syntax-rules ()
      [(_ spec desc ...)
       (*specsubform 'spec #f null #f null null (lambda () (list desc ...)))]))
  (define-syntax defthing
    (syntax-rules ()
      [(_ id result desc ...)
       (*defthing (quote-syntax id) 'id 'result (lambda () (list desc ...)))]))
  (define-syntax defparam
    (syntax-rules ()
      [(_ id arg contract desc ...)
       (defproc* ([(id) contract] [(id [arg contract]) void?]) desc ...)]))
  (define-syntax defboolparam
    (syntax-rules ()
      [(_ id arg desc ...)
       (defproc* ([(id) boolean?] [(id [arg any/c]) void?]) desc ...)]))
  (define-syntax schemegrammar
    (syntax-rules ()
      [(_ #:literals (lit ...) id clause ...) (*schemegrammar '(lit ...) 
                                                              '(id clause ...)
                                                              (lambda () (list (list (scheme id) (schemeblock0 clause) ...))))]
      [(_ id clause ...) (schemegrammar #:literals () id clause ...)]))
  (define-syntax schemegrammar*
    (syntax-rules ()
      [(_ #:literals (lit ...) [id clause ...] ...) (*schemegrammar '(lit ...) 
                                                                    '(id ... clause ... ...)
                                                                    (lambda () 
                                                                      (list
                                                                       (list (scheme id) (schemeblock0 clause) ...) ...)))]
      [(_ [id clause ...] ...) (schemegrammar #:literals () [id clause ...] ...)]))
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

  (define max-proto-width 65)

  (define (*defproc stx-ids prototypes arg-contractss result-contracts content-thunk)
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
                        [else v]))]
          [prototype-size (lambda (s first-combine next-combine)
                            (let loop ([s s][combine first-combine])
                              (if (null? s)
                                  0
                                  (combine
                                   (loop (cdr s) next-combine)
                                   (cond
                                    [(symbol? (car s)) (string-length (symbol->string (car s)))]
                                    [(pair? (car s)) 
                                     (if (keyword? (caar s))
                                         (+ (string-length (keyword->string (caar s)))
                                            3
                                            (string-length (symbol->string (cadar s))))
                                         (string-length (symbol->string (caar s))))]
                                    [else 0])))))])
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
             (lambda (stx-id prototype arg-contracts result-contract first?)
               (let*-values ([(required optional more-required)
                              (let loop ([a (cdr prototype)][r-accum null])
                                (if (or (null? a)
                                        (and (has-optional? (car a))))
                                    (let ([req (reverse r-accum)])
                                      (let loop ([a a][o-accum null])
                                        (if (or (null? a)
                                                (and (not (has-optional? (car a)))
                                                     ;; A repeat after an optional argument is
                                                     ;; effectively optional:
                                                     (not (memq (car a) '(...)))
                                                     (or (null? (cdr a))
                                                         (not (memq (cadr a) '(...))))))
                                            (values req (reverse o-accum) a)
                                            (loop (cdr a) (cons (car a) o-accum)))))
                                    (loop (cdr a) (cons (car a) r-accum))))]
                             [(tagged) (if first?
                                           (make-target-element
                                            #f
                                            (list (to-element (make-just-context (car prototype)
                                                                                 stx-id)))
                                            (register-scheme-definition stx-id))
                                           (to-element (make-just-context (car prototype)
                                                                          stx-id)))]
                             [(flat-size) (prototype-size prototype + +)]
                             [(short?) (or (flat-size . < . 40)
                                           ((length prototype) . < . 3))]
                             [(res) (result-contract)]
                             [(result-next-line?) ((+ (if short? 
                                                          flat-size
                                                          (prototype-size prototype + max))
                                                      (flow-element-width res))
                                                   . >= . (- max-proto-width 7))]
                             [(end) (list (to-flow spacer)
                                          (to-flow 'rarr)
                                          (to-flow spacer)
                                          (make-flow (list res)))]
                             [(opt-cnt) (length optional)])
                 (append
                  (list
                   (list (make-flow
                          (if short?
                              (make-table-if-necessary
                               "prototype"
                               (list
                                (cons
                                 (to-flow
                                  (to-element (append
                                               (list tagged)
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
                                 (if result-next-line?
                                     null
                                     end))))
                              (let ([not-end
                                     (if result-next-line?
                                         (list (to-flow spacer))
                                         (list (to-flow spacer)
                                               (to-flow spacer)
                                               (to-flow spacer)
                                               (to-flow spacer)))])
                                (list
                                 (make-table
                                  "prototype"
                                  (cons
                                   (list* (to-flow (make-element
                                                    #f
                                                    (list
                                                     (schemeparenfont "(")
                                                     tagged)))
                                          (cond
                                           [(null? required)
                                            (to-flow (make-element #f (list spacer "[")))]
                                           [else
                                            (to-flow spacer)])
                                          (to-flow
                                           (if (null? required)
                                               (arg->elem (car optional))
                                               (arg->elem (car required))))
                                          not-end)
                                   (let loop ([args (cdr (append required optional more-required))]
                                              [req (sub1 (length required))])
                                     (if (null? args)
                                         null
                                         (let ([dots-next? (or (and (pair? (cdr args))
                                                                    (or (eq? (cadr args) '...)
                                                                        (eq? (cadr args) '...+))))])
                                           (cons (list* (to-flow spacer)
                                                        (if (zero? req)
                                                            (to-flow (make-element #f (list spacer "[")))
                                                            (to-flow spacer))
                                                        (let ([a (arg->elem (car args))]
                                                              [next (if dots-next?
                                                                        (make-element #f (list (hspace 1)
                                                                                               (arg->elem (cadr args))))
                                                                        "")])
                                                          (to-flow 
                                                           (cond
                                                            [(null? ((if dots-next? cddr cdr) args))
                                                             (if (or (null? optional)
                                                                     (not (null? more-required)))
                                                                 (make-element 
                                                                  #f 
                                                                  (list a next (schemeparenfont ")")))
                                                                 (make-element 
                                                                  #f 
                                                                  (list a next "]" (schemeparenfont ")"))))]
                                                            [(and (pair? more-required)
                                                                  (= (- 1 req) (length optional)))
                                                             (make-element #f (list a next "]"))]
                                                            [(equal? next "") a]
                                                            [else
                                                             (make-element #f (list a next))])))
                                                        (if (and (null? ((if dots-next? cddr cdr) args))
                                                                 (not result-next-line?))
                                                            end
                                                            not-end))
                                               (loop ((if dots-next? cddr cdr) args) (sub1 req))))))))))))))
                  (if result-next-line?
                      (list (list (make-flow (make-table-if-necessary
                                              "prototype" 
                                              (list end)))))
                      null)
                  (apply append
                         (map (lambda (v arg-contract)
                                (cond
                                 [(pair? v)
                                  (let* ([v (if (keyword? (car v)) 
                                                (cdr v)
                                                v)]
                                         [arg-cont (arg-contract)]
                                         [base-len (+ 5 (string-length (symbol->string (car v)))
                                                      (flow-element-width arg-cont))]
                                         [def-len (if (has-optional? v)
                                                      (string-length (format "~a" (caddr v)))
                                                      0)]
                                         [base-list
                                          (list
                                           (to-flow (hspace 2))
                                           (to-flow (arg->elem v))
                                           (to-flow spacer)
                                           (to-flow ":")
                                           (to-flow spacer)
                                           (make-flow (list arg-cont)))])
                                    (list
                                     (list
                                      (make-flow
                                       (if (and (has-optional? v)
                                                ((+ base-len 3 def-len) . >= . max-proto-width))
                                           (list
                                            (make-table
                                             "argcontract"
                                             (list
                                              base-list
                                              (list
                                               (to-flow spacer)
                                               (to-flow spacer)
                                               (to-flow spacer)
                                               (to-flow "=")
                                               (to-flow spacer)
                                               (to-flow (to-element (caddr v)))))))
                                           (make-table-if-necessary
                                            "argcontract"
                                            (list
                                             (append
                                              base-list
                                              (if (and (has-optional? v)
                                                       ((+ base-len 3 def-len) . < . max-proto-width))
                                                  (list (to-flow spacer)
                                                        (to-flow "=")
                                                        (to-flow spacer)
                                                        (to-flow (to-element (caddr v))))
                                                  null)))))))))]
                                 [else null]))
                              (cdr prototype)
                              arg-contracts)))))
             stx-ids
             prototypes
             arg-contractss
             result-contracts
             (cons #t (map (lambda (x) #f) (cdr prototypes))))))
          (content-thunk))))))

  (define (make-target-element* stx-id content wrappers)
    (if (null? wrappers)
        content
        (make-target-element*
         stx-id
         (make-target-element
          #f
          (list content)
          (register-scheme-definition 
           (datum->syntax-object stx-id
                                 (string->symbol
                                  (apply string-append
                                         (map symbol->string (car wrappers)))))))
         (cdr wrappers))))

  (define (*defstruct stx-id name fields field-contracts immutable? transparent? content-thunk)
    (define spacer (hspace 1))
    (define to-flow (lambda (e) (make-flow (list (make-paragraph (list e))))))
    (make-splice
     (cons
      (make-table
       'boxed
       (cons
        (list (make-flow 
               (list
                (let* ([the-name 
                       (make-target-element*
                        stx-id
                        (to-element (if (pair? name)
                                        (map (lambda (x)
                                               (make-just-context x stx-id))
                                             name)
                                        stx-id))
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
                                  (if immutable?
                                      null
                                      (map (lambda (f)
                                             (list 'set- name '- (car f) '!))
                                           fields))))))]
                      [short-width (apply +
                                          (length fields)
                                          8
                                          (map (lambda (s)
                                                 (string-length (symbol->string s)))
                                               (append (if (pair? name)
                                                           name
                                                           (list name))
                                                     (map car fields))))])
                  (if (and (short-width . < . max-proto-width)
                           (not immutable?)
                           (not transparent?))
                      (make-paragraph
                       (list
                        (to-element
                         `(,(schemeparenfont "struct")
                           ,the-name
                           ,(map car fields)))))
                      (make-table
                       #f
                       (append
                        (list
                         (list (to-flow (schemeparenfont "(struct"))
                               (to-flow spacer)
                               (to-flow the-name)
                               (if (or (null? fields)
                                       (short-width . < . max-proto-width))
                                   (to-flow spacer)
                                   (to-flow (make-element #f
                                                          (list spacer
                                                                (schemeparenfont "(")))))
                               (to-flow (if (or (null? fields)
                                                (short-width . < . max-proto-width))
                                            (to-element (map car fields))
                                            (to-element (caar fields))))))
                        (if (short-width . < . max-proto-width)
                            null
                            (let loop ([fields fields])
                              (if (null? fields)
                                  null
                                  (cons (let ([fld (car fields)])
                                          (list (to-flow spacer)
                                                (to-flow spacer)
                                                (to-flow spacer)
                                                (to-flow spacer)
                                                (to-flow
                                                 (let ([e (to-element (car fld))])
                                                   (if (null? (cdr fields))
                                                       (make-element 
                                                        #f 
                                                        (list e 
                                                              (schemeparenfont 
                                                               (if (and (not immutable?)
                                                                        (not transparent?))
                                                                   "))"
                                                                   ")"))))
                                                       e)))))
                                        (loop (cdr fields))))))
                        (cond
                         [(and immutable? transparent?)
                          (list
                           (list (to-flow spacer)
                                 (to-flow spacer)
                                 (to-flow (to-element '#:immutable))
                                 'cont
                                 'cont)
                           (list (to-flow spacer)
                                 (to-flow spacer)
                                 (to-flow (make-element
                                           #f
                                           (list (to-element '#:inspector)
                                                 spacer
                                                 (to-element #f)
                                                 (schemeparenfont ")"))))
                                 'cont
                                 'cont))]
                         [immutable?
                          (list
                           (list (to-flow spacer)
                                 (to-flow spacer)
                                 (to-flow (make-element
                                           #f
                                           (list (to-element '#:immutable)
                                                 (schemeparenfont ")"))))
                                 'cont
                                 'cont))]
                         [transparent?
                          (list
                           (list (to-flow spacer)
                                 (to-flow spacer)
                                 (to-flow (make-element
                                           #f
                                           (list (to-element '#:inspector)
                                                 spacer
                                                 (to-element #f)
                                                 (schemeparenfont ")"))))
                                 'cont
                                 'cont))]
                         [else null]))))))))
        (map (lambda (v field-contract)
               (cond
                [(pair? v)
                 (list
                  (make-flow
                   (make-table-if-necessary
                    #f
                    (list
                     (list (to-flow (hspace 2))
                           (to-flow (to-element (car v)))
                           (to-flow spacer)
                           (to-flow ":")
                           (to-flow spacer)
                           (make-flow (list (field-contract))))))))]
                [else null]))
             fields field-contracts)))
      (content-thunk))))

  (define (*defthing stx-id name result-contract content-thunk)
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
                        (register-scheme-definition stx-id))
                       spacer ":" spacer
                       (to-element result-contract))))))))
      (content-thunk))))

  (define (meta-symbol? s) (memq s '(... ...+ ?)))

  (define (*defforms kw-id lits forms form-procs subs sub-procs content-thunk)
    (parameterize ([current-variable-list
                    (apply 
                     append
                     (map (lambda (form)
                            (let loop ([form (cons (if kw-id 
                                                       (if (pair? form)
                                                           (cdr form) 
                                                           null)
                                                       form)
                                                   subs)])
                              (cond
                               [(symbol? form) (if (or (meta-symbol? form)
                                                       (memq form lits))
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
                     (and kw-id
                          (eq? form (car forms))
                          (make-target-element
                           #f
                           (list (to-element (make-just-context (if (pair? form)
                                                                    (car form) 
                                                                    form)
                                                                kw-id)))
                           (register-scheme-form-definition kw-id))))))))
               forms form-procs)
          (if (null? sub-procs)
              null
              (list (list (make-flow (list (make-paragraph (list (tt 'nbsp))))))
                    (list (make-flow (list (let ([l (map (lambda (sub)
                                                           (map (lambda (f) (f)) sub))
                                                         sub-procs)])
                                             (*schemerawgrammars
                                              "specgrammar"
                                              (map car l)
                                              (map cdr l))))))))))
        (content-thunk)))))
  
  (define (*specsubform form has-kw? lits form-thunk subs sub-procs content-thunk)
    (parameterize ([current-variable-list
                    (append (let loop ([form (cons (if has-kw? (cdr form) form)
                                                   subs)])
                              (cond
                               [(symbol? form) (if (or (meta-symbol? form)
                                                       (memq form lits))
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
          (if (null? sub-procs)
              null
              (list (list (make-flow (list (make-paragraph (list (tt 'nbsp))))))
                    (list (make-flow (list (let ([l (map (lambda (sub)
                                                           (map (lambda (f) (f)) sub))
                                                         sub-procs)])
                                             (*schemerawgrammars
                                              "specgrammar"
                                              (map car l)
                                              (map cdr l))))))))))
        (flow-paragraphs (decode-flow (content-thunk)))))))

  (define (*schemerawgrammars style nonterms clauseses)
    (make-table
     `((valignment baseline baseline baseline baseline baseline)
       (alignment right left center left left)
       (style ,style))
     (let ([empty-line (make-flow (list (make-paragraph (list (tt 'nbsp)))))]
           [to-flow (lambda (i) (make-flow (list (make-paragraph (list i)))))])
       (cdr
        (apply append
               (map
                (lambda (nonterm clauses)
                  (list*
                   (list empty-line empty-line empty-line empty-line empty-line)
                   (list (to-flow nonterm)
                         empty-line
                         (to-flow "=")
                         empty-line
                         (make-flow (list (car clauses))))
                   (map (lambda (clause)
                          (list empty-line
                                empty-line
                                (to-flow "|")
                                empty-line
                                (make-flow (list clause))))
                        (cdr clauses))))
                nonterms clauseses))))))

  (define (*schemerawgrammar style nonterm clause1 . clauses)
    (*schemerawgrammars style (list nonterm) (list (cons clause1 clauses))))

  (define (*schemegrammar lits s-expr clauseses-thunk)
    (parameterize ([current-variable-list
                    (let loop ([form s-expr])
                      (cond
                       [(symbol? form) (if (memq form lits)
                                           null
                                           (list form))]
                       [(pair? form) (append (loop (car form))
                                             (loop (cdr form)))]
                       [else null]))])
      (let ([l (clauseses-thunk)])
        (*schemerawgrammars #f (map car l) (map cdr l)))))

  (define (*var id)
    (to-element (*var-sym id)))

  (define (*var-sym id)
    (string->symbol (format "_~a" id)))

  ;; ----------------------------------------

  (provide centerline)
  (define (centerline . s)
    (make-table 'centered (list (list (make-flow (list (decode-paragraph s)))))))

  (provide commandline)
  (define (commandline . s)
    (make-paragraph (list (hspace 2) (apply tt s))))
           
  (define (elemtag t . body)
    (make-target-element #f (decode-content body) t))
  (define (elemref t . body)
    (make-link-element #f (decode-content body) t))
  (provide elemtag elemref)

  (define (secref s)
    (make-link-element #f null `(part ,s)))
  (define (seclink tag . s)
    (make-link-element #f (decode-content s) `(part ,tag)))
  (define (*schemelink stx-id id . s)
    (make-link-element #f (decode-content s) (register-scheme-definition stx-id)))
  (define-syntax schemelink
    (syntax-rules ()
      [(_ id . content) (*schemelink (quote-syntax id) 'id . content)]))
  (provide secref seclink schemelink)

  (define (pidefterm . s)
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
  (define (math . s)
    (let ([c (decode-content s)])
      (make-element #f (apply append
                              (map (lambda (i)
                                     (let loop ([i i])
                                       (cond
                                        [(string? i)
                                         (cond
                                          [(regexp-match #rx"^(.*)([()0-9])(.*)$" i)
                                           => (lambda (m)
                                                (append (loop (cadr m))
                                                        (list (caddr m))
                                                        (loop (cadddr m))))]
                                          [else
                                           (list (make-element 'italic (list i)))])]
                                        [(eq? i 'rsquo) (list 'prime)]
                                        [else (list i)])))
                                   c)))))

  ;; ----------------------------------------

  (provide cite)

  (define (cite #:key key #:title title #:author author #:location location #:date date)
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
