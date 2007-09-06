
(module manual (lib "lang.ss" "big")
  (require "decode.ss"
           "struct.ss"
           "scheme.ss"
           "config.ss"
           "basic.ss"
           (lib "string.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "stxparam.ss")
           (lib "serialize.ss"))
  (require-for-syntax (lib "stxparam.ss"))
  (require-for-label (lib "lang.ss" "big")
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

  (define-syntax (keep-s-expr stx)
    (syntax-case stx ()
      [(_ ctx s srcloc)
       (let ([sv (syntax-e #'s)])
         (if (or (number? sv)
                 (boolean? sv)
                 (and (pair? sv)
                      (identifier? (car sv))
                      (module-identifier=? #'cons (car sv))))
             ;; We know that the context is irrelvant
             #'s
             ;; Context may be relevant:
             #'(*keep-s-expr s ctx)))]))
  (define (*keep-s-expr s ctx)
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
                                      (let loop ([s s])
                                        (let ([spaces (regexp-match-positions #rx"(?:^| ) +" s)])
                                          (if spaces
                                              (append
                                               (loop (substring s 0 (caar spaces)))
                                               (list (hspace (- (cdar spaces) (caar spaces))))
                                               (loop (substring s (cdar spaces))))
                                              (list (make-element 'tt (list s)))))))))))
            strs))))

  (define-syntax indexed-scheme
    (syntax-rules ()
      [(_ x) (add-scheme-index 'x (scheme x))]))

  (define (add-scheme-index s e)
    (let ([k (cond
              [(and (pair? s)
                    (eq? (car s) 'quote))
               (format "~s" (cadr s))]
              [(string? s) s]
              [else (format "~s" s)])])
      (index* (list k) (list e) e)))

  (provide schemeblock SCHEMEBLOCK
           schemeblock0 SCHEMEBLOCK0
           schemeinput
           schememod
           scheme schemeresult schemeid schememodname
           indexed-scheme
           litchar
           verbatim)

  (provide onscreen menuitem defterm
           schemefont schemevalfont schemeresultfont schemeidfont 
           schemeparenfont schemekeywordfont schememetafont schememodfont
           file exec envvar Flag DFlag
           indexed-file indexed-envvar
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
  (define (indexed-file . str)
    (let* ([f (apply file str)]
           [s (element->string f)])
      (index* (list (substring s 1 (sub1 (string-length s)))) (list f) f)))
  (define (exec . str)
    (make-element 'tt (decode-content str)))
  (define (Flag . str)
    (make-element 'no-break (list (make-element 'tt (cons "-" (decode-content str))))))
  (define (DFlag . str)
    (make-element 'no-break (list (make-element 'tt (cons "--" (decode-content str))))))
  (define (envvar . str)
    (make-element 'tt (decode-content str)))
  (define (indexed-envvar . str)
    (let* ([f (apply envvar str)]
           [s (element->string f)])
      (index* (list s) (list f) f)))
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

  (provide method xmethod (rename method ::))

  (define-syntax method
    (syntax-rules ()
      [(_ a b)
       (*method 'b (quote-syntax a))]))

  (define-syntax xmethod
    (syntax-rules ()
      [(_ a b)
       (elem (method a b) " in " (scheme a))]))

  (define (*method sym id)
    (**method sym (register-scheme-definition id #t)))

  (define (**method sym tag)
    (make-element
     "schemesymbol"
     (list (make-link-element
            "schemevaluelink"
            (list (symbol->string sym))
            (method-tag tag sym)))))

  (define (method-tag vtag sym)
    (list 'meth
          (format "~a::~a" (cadr vtag) sym)))

  ;; ----------------------------------------

  (provide margin-note)
  
  (define (margin-note . c)
    (make-styled-paragraph (list (make-element "refcontent"
                                               c))
                           "refpara"))

  ;; ----------------------------------------

  (provide deftech tech techlink)

  (define (*tech make-elem style doc s)
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
                 (list 'tech (doc-prefix doc s)))))

  (define (deftech . s)
    (let* ([e (apply defterm s)]
           [t (*tech make-target-element #f #f (list e))])
      (make-index-element #f
                          (list t)
                          (target-element-tag t)
                          (list (element->string e))
                          (list e))))

  (define (tech #:doc [doc #f] . s)
    (*tech make-link-element "techlink" doc s))

  (define (techlink #:doc [doc #f] . s)
    (*tech make-link-element #f doc s))

  ;; ----------------------------------------

  (provide declare-exporting
           defproc defproc* defstruct defthing defparam defboolparam
           defform defform* defform/subs defform*/subs defform/none
           defidform
           specform specform/subs 
           specsubform specsubform/subs specspecsubform specspecsubform/subs specsubform/inline
           schemegrammar schemegrammar*
           var svar void-const undefined-const)

  (define-syntax declare-exporting
    (syntax-rules ()
      [(_ lib ...) (*declare-exporting '(lib ...))]))

  (define (*declare-exporting libs)
    (make-part-collect-decl
     (make-collect-element #f
                           null
                           (lambda (ri)
                             (collect-put! ri '(exporting-libraries #f)libs)))))

  (define-syntax (quote-syntax/loc stx)
    (syntax-case stx ()
      [(_ id) 
       (with-syntax ([loc
                      (let ([s #'id])
                        (list (syntax-source s)
                              (syntax-line s)
                              (syntax-column s)
                              (syntax-position s)
                              (syntax-span s)))])
         #'(let ([s (quote-syntax id)])
             (datum->syntax-object s
                                   (syntax-e s)
                                   'loc
                                   s)))]))

  (define void-const
    (schemeresultfont "#<void>"))
  (define undefined-const
    (schemeresultfont "#<undefined>"))

  (define dots0
    (make-element "schememeta" (list "...")))
  (define dots1
    (make-element "schememeta" (list "...+")))

  (define-syntax (arg-contract stx)
    (syntax-case stx (... ...+ _...superclass-args...)
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
      [(_ _...superclass-args...)
       #'#f]
      [(_ arg)
       (raise-syntax-error
        'defproc
        "bad argument form"
        #'arg)]))

  (define-syntax defproc 
    (syntax-rules ()
      [(_ (id arg ...) result desc ...)
       (defproc* [[(id arg ...) result]] desc ...)]))
  (define-syntax defproc*
    (syntax-rules ()
      [(_ [[(id arg ...) result] ...] desc ...)
       (defproc* #:mode procedure #:within #f [[(id arg ...) result] ...] desc ...)]
      [(_ #:mode m #:within cl [[(id arg ...) result] ...] desc ...)
       (*defproc 'm (quote-syntax/loc cl)
                 (list (quote-syntax/loc id) ...)
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
       (*defstruct (quote-syntax/loc name) 'name 
                   '([field field-contract] ...) (list (lambda () (schemeblock0 field-contract)) ...)
                   immutable? transparent? (lambda () (list desc ...)))]))
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
         #'(*defforms (quote-syntax/loc spec-id) '(lit ...)
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
      [(_ #:literals (lit ...) spec desc ...) #'(defform*/subs #:literals (lit ...) [spec] () desc ...)]
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
       #'(*defforms (quote-syntax/loc spec-id) null
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
       (*defthing (quote-syntax/loc id) 'id (quote-syntax result) (lambda () (list desc ...)))]))
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

  (define (name-this-object type-sym)
    (to-element
     (string->symbol
      (regexp-replace
       #rx"(%|<%>|-mixin)$"
       (format "_a~a-~s" 
               (if (member
                    (string-ref (symbol->string type-sym) 0)
                    '(#\a #\e #\i #\o #\u))
                   "n"
                   "")
               type-sym)
       ""))))

  (define (annote-exporting-library e)
    (make-delayed-element
     (lambda (render p ri)
       (let ([from (resolve-get/tentative p ri '(exporting-libraries #f))])
         (if (and from
                  (pair? from))
             (list (make-hover-element
                    #f
                    (list e)
                    (string-append
                     "Provided from: "
                     (let loop ([from from])
                       (if (null? (cdr from))
                           (format "~s" (car from))
                           (format "~s, ~a"
                                   (car from)
                                   (loop (cdr from))))))))
             (list e))))
     (lambda () e)
     (lambda () e)))

  (define (*defproc mode within-id
                    stx-ids prototypes arg-contractss result-contracts content-thunk)
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
                             (if (eq? mode 'new)
                                 (make-element #f (list (schemeparenfont "[")
                                                        (schemeidfont (keyword->string (car v)))
                                                        (hspace 1)
                                                        (to-element (cadr v))
                                                        (schemeparenfont "]")))
                                 (make-element #f (list (to-element (car v))
                                                        (hspace 1)
                                                        (to-element (cadr v)))))
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
                                         (+ (if (eq? mode 'new) 2 0)
                                            (string-length (keyword->string (caar s)))
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
                             [(tagged) (cond
                                        [(eq? mode 'new)
                                         (make-element #f
                                                       (list (scheme new)
                                                             (hspace 1)
                                                             (to-element within-id)))]
                                        [(eq? mode 'make)
                                         (make-element #f
                                                       (list (scheme make-object)
                                                             (hspace 1)
                                                             (to-element within-id)))]
                                        [(eq? mode 'send)
                                         (make-element #f
                                                       (list (scheme send)
                                                             (hspace 1)
                                                             (name-this-object (syntax-e within-id))
                                                             (hspace 1)
                                                             (if first?
                                                                 (let* ([mname (car prototype)]
                                                                        [ctag (register-scheme-definition within-id #t)]
                                                                        [tag (method-tag ctag mname)]
                                                                        [content (list (*method mname within-id))])
                                                                   (if tag
                                                                       (make-toc-target-element
                                                                        #f
                                                                        (list (make-index-element #f
                                                                                                  content
                                                                                                  tag
                                                                                                  (list (symbol->string mname))
                                                                                                  content))
                                                                        tag)
                                                                       (car content)))
                                                                 (*method (car prototype) within-id))))]
                                        [else
                                         (if first?
                                             (let ([tag (register-scheme-definition stx-id #t)]
                                                   [content (list 
                                                             (annote-exporting-library
                                                              (to-element (make-just-context (car prototype)
                                                                                             stx-id))))])
                                               (if tag
                                                   (make-toc-target-element
                                                    #f
                                                    (list (make-index-element #f
                                                                              content
                                                                              tag
                                                                              (list (symbol->string (car prototype)))
                                                                              content))
                                                    tag)
                                                   (car content)))
                                             (annote-exporting-library
                                              (to-element (make-just-context (car prototype)
                                                                             stx-id))))])]
                             [(flat-size) (+ (prototype-size (cdr prototype) + +)
                                             (element-width tagged))]
                             [(short?) (or (flat-size . < . 40)
                                           ((length prototype) . < . 3))]
                             [(res) (result-contract)]
                             [(result-next-line?) ((+ (if short? 
                                                          flat-size
                                                          (+ (prototype-size (cdr prototype) max max)
                                                             (element-width tagged)))
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
             (let loop ([ps prototypes][accum null])
               (cond
                [(null? ps) null]
                [(ormap (lambda (a) (eq? (caar ps) a)) accum)
                 (cons #f (loop (cdr ps) accum))]
                [else
                 (cons #t (loop (cdr ps)
                                (cons (caar ps) accum)))])))))
          (content-thunk))))))

  (define (make-target-element* inner-make-target-element stx-id content wrappers)
    (if (null? wrappers)
        content
        (make-target-element*
         make-target-element
         stx-id
         (let* ([name
                 (apply string-append
                        (map symbol->string (car wrappers)))]
                [tag 
                 (register-scheme-definition 
                  (datum->syntax-object stx-id
                                        (string->symbol
                                         name))
                  #t)])
           (if tag
               (inner-make-target-element
                #f
                (list
                 (make-index-element #f
                                     (list content)
                                     tag
                                     (list name)
                                     (list (schemeidfont (make-element "schemevaluelink" (list name))))))
                tag)
               content))
         (cdr wrappers))))

  (define (*defstruct stx-id name fields field-contracts immutable? transparent? content-thunk)
    (define spacer (hspace 1))
    (define to-flow (lambda (e) (make-flow (list (make-paragraph (list e))))))
    (define (field-name f) (if (pair? (car f))
                               (caar f)
                               (car f)))
    (define (field-view f) (if (pair? (car f))
                               (make-shaped-parens (car f) #\[)
                               (car f)))
    (make-splice
     (cons
      (make-table
       'boxed
       (cons
        (list (make-flow 
               (list
                (let* ([the-name
                        (let ([just-name
                               (make-target-element*
                                make-toc-target-element
                                stx-id
                                (annote-exporting-library
                                 (to-element (if (pair? name)
                                                 (make-just-context (car name) stx-id)
                                                 stx-id)))
                                (let ([name (if (pair? name)
                                                (car name)
                                                name)])
                                  (list* (list name)
                                         (list name '?)
                                         (list 'make- name)
                                         (append
                                          (map (lambda (f)
                                                 (list name '- (field-name f)))
                                               fields)
                                          (if immutable?
                                              null
                                              (filter 
                                               values
                                               (map (lambda (f)
                                                      (if (and (pair? (car f))
                                                               (memq '#:immutable (car f)))
                                                          #f
                                                          (list 'set- name '- (field-name f) '!)))
                                                    fields)))))))])
                          (if (pair? name)
                              (to-element (list just-name
                                                (make-just-context (cadr name) stx-id)))
                              just-name))]
                       [short-width (apply +
                                           (length fields)
                                           8
                                           (append
                                            (map (lambda (s)
                                                   (string-length (symbol->string s)))
                                                 (append (if (pair? name)
                                                             name
                                                             (list name))
                                                         (map field-name fields)))
                                            (map (lambda (f)
                                                   (if (pair? (car f))
                                                       (+ 3 2 (string-length (keyword->string (cadar f))))
                                                       0))
                                                 fields)))])
                  (if (and (short-width . < . max-proto-width)
                           (not immutable?)
                           (not transparent?))
                      (make-paragraph
                       (list
                        (to-element
                         `(,(schemeparenfont "struct")
                           ,the-name
                           ,(map field-view fields)))))
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
                                            (to-element (map field-view fields))
                                            (to-element (field-view (car fields)))))))
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
                                                 (let ([e (to-element (field-view fld))])
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
                           (to-flow (to-element (field-name v)))
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
                 (list (let ([tag (register-scheme-definition stx-id #t)]
                             [content (list (annote-exporting-library
                                             (to-element (make-just-context name stx-id))))])
                         (if tag
                             (make-toc-target-element
                              #f
                              (list (make-index-element #f
                                                        content
                                                        tag
                                                        (list (symbol->string name))
                                                        content))
                              tag)
                             (car content)))
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
                              `(,x . ,(cdr form)))))))
                     (and kw-id
                          (eq? form (car forms))
                          (let ([tag (register-scheme-definition kw-id #t)]
                                [stag (register-scheme-form-definition kw-id)]
                                [content (list (annote-exporting-library
                                                (to-element (make-just-context (if (pair? form)
                                                                                   (car form) 
                                                                                   form)
                                                                               kw-id))))])
                            (if tag
                                (make-target-element
                                 #f
                                 (list
                                  (make-toc-target-element
                                   #f
                                   (if kw-id
                                       (list (make-index-element #f
                                                                 content
                                                                 tag
                                                                 (list (symbol->string (syntax-e kw-id)))
                                                                 content))
                                       content)
                                   stag))
                                 tag)
                                (car content)))))))))
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
    (make-target-element #f (decode-content body) `(elem ,t)))
  (define (elemref t . body)
    (make-link-element #f (decode-content body) `(elem ,t)))
  (provide elemtag elemref)

  (define (doc-prefix doc s)
    (if doc
        (format "~a:~a" 
                (module-path-prefix->string doc)
                s)
        s))

  (define (secref s #:doc [doc #f])
    (make-link-element #f null `(part ,(doc-prefix doc s))))
  (define (seclink tag #:doc [doc #f] . s)
    (make-link-element #f (decode-content s) `(part ,(doc-prefix doc tag))))
  (define (*schemelink stx-id id . s)
    (make-link-element #f (decode-content s) (or (register-scheme-definition stx-id)
                                                 (format "--UNDEFINED:~a--" (syntax-e stx-id)))))
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

  (provide defclass
           defclass/title
           definterface
           definterface/title
           defconstructor
           defconstructor/make
           defconstructor*/make
           defconstructor/auto-super
           defmethod
           defmethod*
           methspec
           methimpl
           this-obj)

  (define-syntax-parameter current-class #f)

  (define-struct decl (name super intfs mk-head body))
  (define-struct constructor (def))
  (define-struct meth (name mode desc def))
  (define-struct spec (def))
  (define-struct impl (def))

  (define-serializable-struct cls/intf (name-element super intfs methods))

  (define (make-inherited-table r d ri decl)
    (let* ([start (let ([key (register-scheme-definition (decl-name decl))])
                    (list (cons key (lookup-cls/intf d ri key))))]
           [supers (cdr
                    (let loop ([supers start][accum null])
                      (cond
                       [(null? supers) (reverse accum)]
                       [(memq (car supers) accum)
                        (loop (cdr supers) accum)]
                       [else
                        (let ([super (car supers)])
                          (loop (append (map (lambda (i)
                                               (cons i (lookup-cls/intf d ri i)))
                                             (reverse (cls/intf-intfs (cdr super))))
                                        (let ([s (cls/intf-super (cdr super))])
                                          (if s
                                              (list (cons s (lookup-cls/intf d ri s)))
                                              null))
                                        (cdr supers))
                                (cons super accum)))])))]
           [ht (let ([ht (make-hash-table)])
                 (for-each (lambda (i)
                             (when (meth? i) 
                               (hash-table-put! ht (meth-name i) #t)))
                           (decl-body decl))
                 ht)]
           [inh (apply
                 append
                 (map (lambda (super)
                        (let ([inh (filter
                                    values
                                    (map
                                     (lambda (k)
                                       (if (hash-table-get ht k #f)
                                           #f
                                           (begin
                                             (hash-table-put! ht k #t)
                                             (cons (symbol->string k)
                                                   (**method k (car super))))))
                                     (cls/intf-methods (cdr super))))])
                          (if (null? inh)
                              null
                              (cons
                               (make-element #f (list (make-element "inheritedlbl" '("from "))
                                                      (cls/intf-name-element (cdr super))))
                               (map cdr (sort inh
                                              (lambda (a b)
                                                (string<? (car a) (car b)))))))))
                      supers))])
      (if (null? inh)
          (make-auxiliary-table "inherited" null)
          (make-auxiliary-table
           "inherited"
           (map (lambda (i)
                  (list (make-flow (list (make-paragraph (list i))))))
                (cons (make-element "inheritedlbl" '("Inherited methods:")) inh))))))

  (define (make-decl-collect decl)
    (make-part-collect-decl
     (make-collect-element
      #f null
      (lambda (ci)
        (let ([tag (register-scheme-definition (decl-name decl))])
          (collect-put! ci 
                        `(cls/intf ,tag)
                        (make-cls/intf
                         (make-element
                          "schemesymbol"
                          (list (make-link-element
                                 "schemevaluelink"
                                 (list (symbol->string (syntax-e (decl-name decl))))
                                 tag)))
                         (and (decl-super decl)
                              (not (module-label-identifier=? #'object% (decl-super decl)))
                              (register-scheme-definition (decl-super decl)))
                         (map register-scheme-definition (decl-intfs decl))
                         (map (lambda (m)
                                (meth-name m))
                              (filter meth? (decl-body decl))))))))))

  (define (build-body decl body)
    (append
     (map (lambda (i)
            (cond
             [(constructor? i) ((constructor-def i))]
             [(meth? i)
              ((meth-def i) (meth-desc i))]
             [else i]))
          body)
     (list
      (make-delayed-flow-element
       (lambda (r d ri)
         (make-inherited-table r d ri decl))))))

  (define (*include-class/title decl)
    (make-splice
     (list* (title #:style 'hidden (to-element (decl-name decl)))
            (make-decl-collect decl)
            (build-body decl
                        (append
                         ((decl-mk-head decl) #t)
                         (decl-body decl))))))

  (define (*include-class decl)
    (make-splice
     (cons
      (make-decl-collect decl)
      (append
       ((decl-mk-head decl) #f)
       (list
        (make-blockquote 
         "leftindent"
         (flow-paragraphs
          (decode-flow
           (build-body decl (decl-body decl))))))))))

  (define (*class-doc stx-id super intfs whole-page?)
    (let ([spacer (hspace 1)])
      (make-table
       'boxed
       (append
        (list
         (list (make-flow 
                (list
                 (make-paragraph
                  (list (let ([tag (register-scheme-definition stx-id)]
                              [content (list (annote-exporting-library (to-element stx-id)))])
                          (if tag
                              ((if whole-page? 
                                   make-page-target-element
                                   make-toc-target-element)
                               #f
                               (if whole-page?
                                   content ; title is already an index entry
                                   (list (make-index-element #f
                                                             content
                                                             tag
                                                             (list (symbol->string (syntax-e stx-id)))
                                                             content)))
                               tag)
                              (car content)))
                        spacer ":" spacer
                        (if super
                            (scheme class?)
                            (scheme interface?))))))))
        (if super
            (list
             (list (make-flow
                    (list
                     (t (hspace 2) "superclass:" spacer (to-element super))))))
            null)
        (if (null? intfs)
            null
            (list
             (list
              (make-flow
               (list
                (make-table #f
                            (cons
                             (list (make-flow (list (make-paragraph (list (hspace 2) 
                                                                          (if super
                                                                              "implements:" 
                                                                              "extends:")
                                                                          spacer))))
                                   (make-flow (list (make-paragraph (list (to-element (car intfs)))))))
                             (map (lambda (i)
                                    (list (make-flow (list (make-paragraph (list spacer))))
                                          (make-flow (list (make-paragraph (list (to-element i)))))))
                                  (cdr intfs)))))))))))))

  (define-syntax *defclass
    (syntax-rules ()
      [(_ *include-class name super (intf ...) body ...)
       (*include-class
         (syntax-parameterize ([current-class (quote-syntax name)])
           (make-decl (quote-syntax/loc name)
                      (quote-syntax/loc super)
                      (list (quote-syntax/loc intf) ...)
                      (lambda (whole-page?)
                        (list
                         (*class-doc (quote-syntax/loc name)
                                     (quote-syntax super)
                                     (list (quote-syntax intf) ...)
                                     whole-page?)))
                      (list body ...))))]))

  (define-syntax defclass
    (syntax-rules ()
      [(_ name super (intf ...) body ...)
       (*defclass *include-class name super (intf ...) body ...)]))

  (define-syntax defclass/title
    (syntax-rules ()
      [(_ name super (intf ...) body ...)
       (*defclass *include-class/title name super (intf ...) body ...)]))

  (define-syntax *definterface
    (syntax-rules ()
      [(_ *include-class name (intf ...) body ...)
       (*include-class
        (syntax-parameterize ([current-class (quote-syntax name)])
          (make-decl (quote-syntax/loc name)
                     #f
                     (list (quote-syntax/loc intf) ...)
                     (lambda (whole-page?)
                       (list
                        (*class-doc (quote-syntax/loc name)
                                    #f
                                    (list (quote-syntax intf) ...)
                                    whole-page?)))
                     (list body ...))))]))
  
  (define-syntax definterface
    (syntax-rules ()
      [(_ name (intf ...) body ...)
       (*definterface *include-class name (intf ...) body ...)]))

  (define-syntax definterface/title
    (syntax-rules ()
      [(_ name (intf ...) body ...)
       (*definterface *include-class/title name (intf ...) body ...)]))

  (define-syntax (defconstructor*/* stx)
    (syntax-case stx ()
      [(_ mode ((arg ...) ...) desc ...)
       (let ([n (syntax-parameter-value #'current-class)])
         (with-syntax ([name n]
                       [result (datum->syntax-object #f
                                                     (list
                                                      (datum->syntax-object #'is-a?/c
                                                                            'is-a?/c
                                                                            (list 'src 1 1 2 1))
                                                      (datum->syntax-object n
                                                                            (syntax-e n)
                                                                            (list 'src 1 3 4 1)))
                                                     (list 'src 1 0 1 5))]
                       [(((kw ...) ...) ...) (map (lambda (ids)
                                                    (map (lambda (arg)
                                                           (if (and (pair? (syntax-e arg))
                                                                    (eq? (syntax-e #'mode) 'new))
                                                               (list (string->keyword (symbol->string 
                                                                                       (syntax-e 
                                                                                        (car (syntax-e arg))))))
                                                               null))
                                                         (syntax->list ids)))
                                                  (syntax->list #'((arg ...) ...)))])
           #'(make-constructor (lambda ()
                                 (defproc* #:mode mode #:within name [[(make [kw ... . arg] ...) result] ...]
                                   desc ...)))))]))

  (define-syntax (defconstructor stx)
    (syntax-case stx ()
      [(_ ([id . arg-rest] ...) desc ...)
       #'(defconstructor*/* new (([id . arg-rest] ...)) desc ...)]))

  (define-syntax (defconstructor/make stx)
    (syntax-case stx ()
      [(_ ([id . arg-rest] ...) desc ...)
       #'(defconstructor*/* make (([id . arg-rest] ...)) desc ...)]))

  (define-syntax (defconstructor*/make stx)
    (syntax-case stx ()
      [(_ (([id . arg-rest] ...) ...) desc ...)
       #'(defconstructor*/* make (([id . arg-rest] ...) ...) desc ...)]))

  (define-syntax (defconstructor/auto-super stx)
    (syntax-case stx ()
      [(_ ([id . arg-rest] ...) desc ...)
       #'(defconstructor*/* new (([id . arg-rest] ... _...superclass-args...)) desc ...)]))

  (define-syntax (defmethod* stx)
    (syntax-case stx ()
      [(_ #:mode mode ([(name arg ...) result-type] ...) desc ...)
       (with-syntax ([cname (syntax-parameter-value #'current-class)]
                     [name1 (car (syntax->list #'(name ...)))])
         (with-syntax ([(extra ...) (case (syntax-e #'mode)
                                      [(pubment)
                                       #'((t "Refine this method with " (scheme augment) "."))]
                                      [(override extend augment)
                                       #'((t (case (syntax-e #'mode)
                                               [(override) "Overrides "]
                                               [(extend) "Extends "]
                                               [(augment) "Augments "])
                                             (*xmethod/super (quote-syntax/loc cname) 'name1) "."))]
                                      [else
                                       null])])
           #'(make-meth 'name1
                        'mode
                        (lambda () (make-splice (apply
                                                 append
                                                 (map (lambda (f)
                                                        (cond
                                                         [(impl? f) ((impl-def f))]
                                                         [(spec? f) ((spec-def f))]
                                                         [else (list f)]))
                                                      (list extra ... desc ...)))))
                        (lambda (desc-splice)
                          (defproc* #:mode send #:within cname ([(name arg ...) result-type] ...)
                            (desc-splice))))))]
      [(_ ([(name arg ...) result-type] ...) desc ...)
       #'(defmethod* #:mode public ([(name arg ...) result-type] ...) desc ...)]))

  (define-syntax defmethod
    (syntax-rules ()
      [(_ #:mode mode (name arg ...) result-type desc ...)
       (defmethod* #:mode mode ([(name arg ...) result-type]) desc ...)]
      [(_ (name arg ...) result-type desc ...)
       (defmethod #:mode public (name arg ...) result-type desc ...)]))
  
  (define-syntax methimpl
    (syntax-rules ()
      [(_ body ...) (make-impl (lambda () (list (italic "Default implementation:") body ...)))]))
  
  (define-syntax methspec
    (syntax-rules ()
      [(_ body ...) (make-spec (lambda () (list (italic "Specification:") body ...)))]))

  (define (*this-obj cname)
    (name-this-object cname))

  (define-syntax (this-obj stx)
    (syntax-case stx ()
      [(_) 
       (with-syntax ([cname (syntax-parameter-value #'current-class)])
         #'(*this-obj 'cname))]))

  (define (*xmethod/super cname name)
    (let ([get
           (lambda (d ri key)
             (let ([v (lookup-cls/intf d ri key)])
               (if v
                   (cons (cls/intf-super v)
                         (cls/intf-intfs v))
                   null)))])
      (make-delayed-element
       (lambda (r d ri)
         (let loop ([search (get d ri (register-scheme-definition cname))])
           (cond
            [(null? search)
             (make-element #f "<method not found>")]
            [(not (car search))
             (loop (cdr search))]
            [else
             (let ([v (lookup-cls/intf d ri (car search))])
               (if v
                   (if (member name (cls/intf-methods v))
                       (list
                        (make-element #f
                                      (list (**method name (car search))
                                            " in " 
                                            (cls/intf-name-element v))))
                       (loop (append (cdr search) (get d ri (car search)))))
                   (loop (cdr search))))])))
       (lambda () (format "~a in ~a" (syntax-e cname) name))
       (lambda () (format "~a in ~a" (syntax-e cname) name)))))

  (define (lookup-cls/intf d ri name)
    (let ([v (resolve-get d ri `(cls/intf ,name))])
      (or v
          (make-cls/intf "unknown"
                         #f
                         null
                         null))))

  ;; ----------------------------------------
  )
