#lang scheme/base
(require "decode.ss"
         "struct.ss"
         "scheme.ss"
         "search.ss"
         "config.ss"
         "basic.ss"
         "manual-struct.ss"
         scheme/string
         scheme/list
         scheme/class
         scheme/stxparam
         scheme/serialize
         setup/main-collects
         (for-syntax scheme/base)
         (for-label scheme/base
                    scheme/class))

(provide (all-from-out "basic.ss")
         unsyntax)

(provide PLaneT)
(define PLaneT "PLaneT")

(provide etc)
(define etc "etc.") ; so we can fix the latex space, one day

(define (to-flow e)
  (make-flow (list (make-paragraph (list e)))))
(define spacer (hspace 1))
(define flow-spacer (to-flow spacer))
(define flow-empty-line (to-flow (tt 'nbsp)))
(define (make-openers n)
  (schemeparenfont
   (case n [(1) "("] [(0) ""] [(2) "(("] [else (make-string n #\()])))
(define (make-closers n)
  (schemeparenfont
   (case n [(1) ")"] [(0) ""] [(2) "))"] [else (make-string n #\()])))

(define-code schemeblock0 to-paragraph)
(define-code schemeblock (to-paragraph/prefix (hspace 2) (hspace 2) ""))
(define-code SCHEMEBLOCK (to-paragraph/prefix (hspace 2) (hspace 2) "")
                         UNSYNTAX)
(define-code SCHEMEBLOCK0 to-paragraph UNSYNTAX)
(define interaction-prompt (make-element 'tt (list "> " )))
(define-code schemeinput
  (to-paragraph/prefix
   (make-element #f (list (hspace 2) interaction-prompt))
   (hspace 4)
   ""))

(define-syntax (schememod stx)
  (syntax-case stx ()
    [(_ #:file filename lang rest ...)
     (with-syntax ([modtag (datum->syntax
                            #'here
                            `(unsyntax (make-element
                                        #f
                                        (list (hash-lang)
                                              spacer
                                              (as-modname-link
                                               ',#'lang
                                               (to-element ',#'lang)))))
                            #'lang)]
                   [(file ...)
                    (if (syntax-e #'filename)
                        (list
                         (datum->syntax
                          #'filename
                          `(code:comment (unsyntax (t "In \"" ,(syntax-e #'filename) "\":")))
                          #'filename))
                        null)])
       (syntax/loc stx (schemeblock file ... modtag rest ...)))]
    [(_ lang rest ...)
     (syntax/loc stx (schememod #:file #f lang rest ...))]))

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
                    (free-identifier=? #'cons (car sv))))
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

(define-code schemeblockelem to-element)

(define-code scheme to-element unsyntax keep-s-expr add-sq-prop)
(define-code SCHEME to-element UNSYNTAX keep-s-expr add-sq-prop)
(define-code schemeresult to-element/result unsyntax keep-s-expr add-sq-prop)
(define-code schemeid to-element/id unsyntax keep-s-expr add-sq-prop)
(define-code *schememodname to-element unsyntax keep-s-expr add-sq-prop)

(define-syntax-rule (schememodname n)
  (as-modname-link 'n (*schememodname n)))

(define (as-modname-link s e)
  (if (symbol? s)
    (make-link-element "schememodlink"
                       (list e)
                       `(mod-path ,(symbol->string s)))
    e))

(define-syntax-rule (defmodule*/no-declare (name ...) . content)
  (*defmodule (list (schememodname name) ...)
              #f
              (list . content)))

(define-syntax defmodule*
  (syntax-rules ()
    [(_ (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodule*/no-declare (name ...) . content))]
    [(_ (name ...) . content)
     (defmodule* (name ...) #:use-sources () . content)]))

(define-syntax-rule (defmodule name . content)
  (defmodule* (name) . content))

(define-syntax-rule (defmodulelang*/no-declare (lang ...) . content)
  (*defmodule (list (schememodname lang) ...) #t (list . content)))

(define-syntax defmodulelang*
  (syntax-rules ()
    [(_ (name ...) #:use-sources (pname ...) . content)
     (begin (declare-exporting name ... #:use-sources (pname ...))
            (defmodulelang*/no-declare (name ...) . content))]
    [(_ (name ...) . content)
     (defmodulelang* (name ...) #:use-sources () . content)]))

(define-syntax-rule (defmodulelang lang . content)
  (defmodulelang* (lang) . content))

(define (*defmodule names lang? content)
  (make-splice
   (cons
    (make-table
     "defmodule"
     (map
      (lambda (name)
        (list
         (make-flow
          (list
           (make-paragraph
            (cons
             spacer
             (if lang?
               (list (hash-lang) spacer (make-defschememodname name))
               (list (scheme (require #,(make-defschememodname name)))))))))))
      names))
    (append (map (lambda (name)
                   (make-part-tag-decl `(mod-path ,(element->string name))))
                 names)
            (flow-paragraphs (decode-flow content))))))

(define (make-defschememodname mn)
  (let ([name-str (element->string mn)])
    (make-index-element #f
                        (list mn)
                        `(mod-path ,name-str)
                        (list name-str)
                        (list mn)
                        (make-module-path-index-desc))))

(define (litchar . strs)
  (unless (andmap string? strs)
    (raise-type-error 'litchar "strings" strs))
  (let ([s (string-append* (map (lambda (s) (regexp-replace* "\n" s " "))
                                strs))])
    (if (regexp-match? #rx"^ *$" s)
      (make-element "schemeinputbg" (list (hspace (string-length s))))
      (let ([^spaces (car (regexp-match-positions #rx"^ *" s))]
            [$spaces (car (regexp-match-positions #rx" *$" s))])
        (make-element
         "schemeinputbg"
         (list (hspace (cdr ^spaces))
               (make-element "schemeinput"
                             (list (substring s (cdr ^spaces) (car $spaces))))
               (hspace (- (cdr $spaces) (car $spaces)))))))))

(define (verbatim #:indent [i 0] s . more)
  (define indent
    (if (zero? i)
      values
      (let ([hs (hspace i)]) (lambda (x) (cons hs x)))))
  (define strs (regexp-split #rx"\n" (string-append* s more)))
  (define (str->elts str)
    (let ([spaces (regexp-match-positions #rx"(?:^| ) +" str)])
      (if spaces
        (list* (substring str 0 (caar spaces))
               (hspace (- (cdar spaces) (caar spaces)))
               (str->elts (substring str (cdar spaces))))
        (list (make-element 'tt (list str))))))
  (define (make-line str)
    (let* ([line (indent (str->elts str))]
           [line (list (make-element 'tt line))])
      (list (make-flow (list (make-paragraph line))))))
  (make-table #f (map make-line strs)))

(define-syntax-rule (indexed-scheme x)
  (add-scheme-index 'x (scheme x)))

(define (add-scheme-index s e)
  (let ([k (cond [(and (pair? s) (eq? (car s) 'quote)) (format "~s" (cadr s))]
                 [(string? s) s]
                 [else (format "~s" s)])])
    (index* (list k) (list e) e)))

(define-syntax-rule (define-/form id base)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ a)
       (with-syntax ([ellipses (datum->syntax #'a '(... ...))])
         #'(let ([ellipses #f])
             (base a)))])))

(define-/form schemeblock0/form schemeblock0)
(define-/form schemeblock/form schemeblock)
(define-/form scheme/form scheme)

(provide schemeblock SCHEMEBLOCK schemeblock/form
         schemeblock0 SCHEMEBLOCK0 schemeblock0/form
         schemeblockelem
         schemeinput
         schememod
         schemeerror
         scheme SCHEME scheme/form schemeresult schemeid schememodname
         defmodule defmodule* defmodulelang defmodulelang*
         defmodule*/no-declare defmodulelang*/no-declare
         indexed-scheme
         litchar
         verbatim)

(provide image image/plain onscreen menuitem defterm emph
         schemefont schemevalfont schemeresultfont schemeidfont schemevarfont
         schemeparenfont schemekeywordfont schememetafont schememodfont
         filepath exec envvar Flag DFlag PFlag DPFlag
         indexed-file indexed-envvar
         link procedure
         idefterm
         inset-flow)

;; String String *-> Element
;; an in-lined image, relative to the current directory
(define (image #:scale [scale 1.0] filename-relative-to-source . alt)
  (make-element (make-image-file filename-relative-to-source scale)
                (decode-content alt)))

(define (image/plain filename-relative-to-source . alt)
  (make-element (make-image-file filename-relative-to-source 1.0)
                (decode-content alt)))

(define (onscreen . str)
  (make-element 'sf (decode-content str)))
(define (menuitem menu item)
  (make-element 'sf (list menu "|" item)))
(define (emph . str)
  (make-element 'italic (decode-content str)))
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
(define (schemevarfont . str)
  (make-element "schemevariable" (decode-content str)))
(define (schemeparenfont . str)
  (make-element "schemeparen" (decode-content str)))
(define (schememetafont . str)
  (make-element "schememeta" (decode-content str)))
(define (schememodfont . str)
  (make-element "schememod" (decode-content str)))
(define (schemekeywordfont . str)
  (make-element "schemekeyword" (decode-content str)))
(define (filepath . str)
  (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
(define (indexed-file . str)
  (let* ([f (apply filepath str)]
         [s (element->string f)])
    (index* (list (clean-up-index-string
                   (substring s 1 (sub1 (string-length s)))))
            (list f)
            f)))
(define (exec . str)
  (if (andmap string? str)
    (make-element 'tt str)
    (make-element #f (map (lambda (s)
                            (if (string? s)
                              (make-element 'tt (list s))
                              s))
                          str))))
(define (Flag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "-" (decode-content str))))))
(define (DFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "--" (decode-content str))))))
(define (PFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "+" (decode-content str))))))
(define (DPFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "++" (decode-content str))))))
(define (envvar . str)
  (make-element 'tt (decode-content str)))
(define (indexed-envvar . str)
  (let* ([f (apply envvar str)]
         [s (element->string f)])
    (index* (list s) (list f) f)))
(define (procedure . str)
  (make-element "schemeresult" `("#<procedure:" ,@(decode-content str) ">")))

(define (link url
              #:underline? [underline? #t]
              #:style [style (if underline? #f "plainlink")]
              . str)
  (make-element (make-target-url url style)
                (decode-content str)))

(define (schemeerror . str)
  (make-element "schemeerror" (decode-content str)))

(provide t)
(define (t . str)
  (decode-paragraph str))

(define (inset-flow . c)
  (make-blockquote "insetpara" (flow-paragraphs (decode-flow c))))

;; ----------------------------------------

(define (gen-absolute-tag)
  `(abs ,(make-generated-tag)))

(define-struct sig (id))

(define (definition-site name stx-id form?)
  (let ([sig (current-signature)])
    (if sig
      (*sig-elem (sig-id sig) name)
      (annote-exporting-library
       (to-element (make-just-context name stx-id))))))

(define checkers (make-hash))

(define (libs->taglet id libs source-libs)
  (let ([lib
         (or (ormap (lambda (lib)
                      (let ([checker
                             (hash-ref
                              checkers lib
                              (lambda ()
                                (let ([ns (make-base-empty-namespace)])
                                  (parameterize ([current-namespace ns])
                                    (namespace-require `(for-label ,lib)))
                                  (let ([checker
                                         (lambda (id)
                                           (parameterize ([current-namespace
                                                           ns])
                                             (free-label-identifier=?
                                              (namespace-syntax-introduce
                                               (datum->syntax
                                                #f
                                                (syntax-e id)))
                                              id)))])
                                    (hash-set! checkers lib checker)
                                    checker))))])
                        (and (checker id) lib)))
                    (or source-libs null))
             (and (pair? libs) (car libs)))])
    (and lib (module-path-index->taglet
              (module-path-index-join lib #f)))))

(define (id-to-target-maker id dep?)
  (*id-to-target-maker 'def id dep?))

(define (id-to-form-target-maker id dep?)
  (*id-to-target-maker 'form id dep?))

(define (*id-to-target-maker sym id dep?)
  (let ([sig (current-signature)])
    (lambda (content mk)
      (make-part-relative-element
       (lambda (ci)
         (let ([e (ormap (lambda (p)
                           (ormap (lambda (e)
                                    (and (exporting-libraries? e) e))
                                  (part-to-collect p)))
                         (collect-info-parents ci))])
           (unless e
             ;; Call raise-syntax-error to capture error message:
             (with-handlers ([exn:fail:syntax?
                              (lambda (exn)
                                (fprintf (current-error-port)
                                         "~a\n" (exn-message exn)))])
               (raise-syntax-error
                'WARNING
                "no declared exporting libraries for definition" id)))
           (if e
             (let* ([lib-taglet (libs->taglet
                                 (if sig (sig-id sig) id)
                                 (exporting-libraries-libs e)
                                 (exporting-libraries-source-libs e))]
                    [tag (intern-taglet
                          (list (if sig
                                  (case sym
                                    [(def) 'sig-val]
                                    [(form) 'sig-def])
                                  sym)
                                `(,lib-taglet
                                  ,@(if sig (list (syntax-e (sig-id sig))) null)
                                  ,(syntax-e id))))])
               (if (or sig (not dep?))
                 (list (mk tag))
                 (list (make-target-element
                        #f
                        (list (mk tag))
                        (intern-taglet
                         `(dep ,(list lib-taglet (syntax-e id))))))))
             content)))
       (lambda () (car content))
       (lambda () (car content))))))

(define (make-binding-redirect-elements mod-path redirects)
  (let ([taglet (module-path-index->taglet 
                 (module-path-index-join mod-path #f))])
    (make-element
     #f
     (map
      (lambda (redirect)
        (let ([id (car redirect)]
              [form? (cadr redirect)]
              [path (caddr redirect)]
              [anchor (cadddr redirect)])
          (let ([make-one
                 (lambda (kind)
                   (make-redirect-target-element
                    #f
                    null
                    (intern-taglet (list kind (list taglet id)))
                    path
                    anchor))])
            (make-element
             #f
             (list (make-one (if form? 'form 'def))
                   (make-one 'dep)
                   (make-index-element #f
                                       null
                                       (list (if form? 'form 'def)
                                             (list taglet id))
                                       (list (symbol->string id))
                                       (list
                                        (make-element
                                         "schemesymbol"
                                         (list
                                          (make-element
                                           (if form?
                                             "schemesyntaxlink"
                                             "schemevaluelink")
                                           (list (symbol->string id))))))
                                       ((if form?
                                          make-form-index-desc
                                          make-procedure-index-desc)
                                        id
                                        (list mod-path))))))))
      redirects))))

(provide make-binding-redirect-elements)

(define current-signature (make-parameter #f))

(define-syntax-rule (sigelem sig elem)
  (*sig-elem (quote-syntax sig) 'elem))

(define (*sig-elem sig elem)
  (let ([s (to-element/no-color elem)])
    (make-delayed-element
     (lambda (renderer sec ri)
       (let* ([tag (find-scheme-tag sec ri sig #f)]
              [taglet (and tag (append (cadr tag) (list elem)))]
              [vtag (and tag `(sig-val ,taglet))]
              [stag (and tag `(sig-form ,taglet))]
              [sd (and stag (resolve-get/tentative sec ri stag))])
         (list
          (make-element
           "schemesymbol"
           (list
            (cond [sd (make-link-element "schemesyntaxlink" (list s) stag)]
                  [vtag (make-link-element "schemevaluelink" (list s) vtag)]
                  [else s]))))))
     (lambda () s)
     (lambda () s))))

(provide sigelem)

;; ----------------------------------------

(provide method xmethod (rename-out [method ::]))

(define-syntax-rule (method a b)
  (*method 'b (quote-syntax a)))

(define-syntax-rule (xmethod a b)
  (elem (method a b) " in " (scheme a)))

(define (*method sym id)
  (**method sym id))

(define (**method sym id/tag)
  (let ([content (list (symbol->string sym))])
    ((if (identifier? id/tag)
       (lambda (c mk)
         (make-delayed-element
          (lambda (ren p ri)
            (let ([tag (find-scheme-tag p ri id/tag #f)])
              (if tag (list (mk tag)) content)))
          (lambda () (car content))
          (lambda () (car content))))
       (lambda (c mk) (mk id/tag)))
     content
     (lambda (tag)
       (make-element "schemesymbol"
                     (list (make-link-element "schemevaluelink" content
                                              (method-tag tag sym))))))))

(define (method-tag vtag sym)
  (list 'meth (list (cadr vtag) sym)))

;; ----------------------------------------

(provide margin-note)

(define (margin-note . c)
  (make-styled-paragraph
   (list (make-element "refcolumn"
                       (list (make-element "refcontent" (decode-content c)))))
   "refpara"))

;; ----------------------------------------

(provide deftech tech techlink)

(define (*tech make-elem style doc s)
  (let* ([c (decode-content s)]
         [s (string-foldcase (content->string c))]
         [s (regexp-replace #rx"ies$" s "y")]
         [s (regexp-replace #rx"s$" s "")]
         [s (regexp-replace* #px"[-\\s]+" s " ")])
    (make-elem style c (list 'tech (doc-prefix doc s)))))

(define (deftech . s)
  (let* ([e (apply defterm s)]
         [t (*tech make-target-element #f #f (list e))])
    (make-index-element #f
                        (list t)
                        (target-element-tag t)
                        (list (clean-up-index-string (element->string e)))
                        (list e)
                        'tech)))

(define (tech #:doc [doc #f] . s)
  (*tech make-link-element "techlink" doc s))

(define (techlink #:doc [doc #f] . s)
  (*tech make-link-element #f doc s))

;; ----------------------------------------

(provide declare-exporting
         deftogether
         defproc defproc* defstruct
         defthing defthing* defthing/proc
         defparam defparam* defboolparam
         defform defform* defform/subs defform*/subs defform/none
         defidform
         specform specform/subs
         specsubform specsubform/subs specspecsubform specspecsubform/subs
         specsubform/inline
         defsubform defsubform*
         schemegrammar schemegrammar*
         var svar void-const undefined-const)

(define-syntax (declare-exporting stx)
  (syntax-case stx ()
    [(_ lib ... #:use-sources (plib ...))
     (let ([libs (syntax->list #'(lib ... plib ...))])
       (for ([l libs])
         (unless (module-path? (syntax->datum l))
           (raise-syntax-error #f "not a module path" stx l)))
       (when (null? libs)
         (raise-syntax-error #f "need at least one module path" stx))
       #'(*declare-exporting '(lib ...) '(plib ...)))]
    [(_ lib ...) #'(*declare-exporting '(lib ...) '())]))

(define-struct (exporting-libraries element) (libs source-libs))

(define (*declare-exporting libs source-libs)
  (make-splice
   (list
    (make-part-collect-decl
     (make-collect-element
      #f null
      (lambda (ri) (collect-put! ri '(exporting-libraries #f) libs))))
    (make-part-collect-decl
     (make-exporting-libraries #f null (and (pair? libs) libs) source-libs)))))

(define-syntax (quote-syntax/loc stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([loc (let ([s #'id])
                          (vector (syntax-source s)
                                  (syntax-line s)
                                  (syntax-column s)
                                  (syntax-position s)
                                  (syntax-span s)))])
       #'(let ([s (*quote-syntax/loc id)])
           (datum->syntax s (syntax-e s) 'loc s)))]))

(define-syntax *quote-syntax/loc
  (syntax-rules ()
    [(_ (sub ...)) (datum->syntax #f (list (quote-syntax/loc sub) ...))]
    [(_ id) (quote-syntax id)]))

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
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(schemeblock0 contract)]
    [(_ [kw id contract val])
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(schemeblock0 contract)]
    [(_ (... ...)) #'#f]
    [(_ (... ...+)) #'#f]
    [(_ _...superclass-args...) #'#f]
    [(_ arg) (raise-syntax-error 'defproc "bad argument form" #'arg)]))

(define-syntax (arg-default stx)
  (syntax-case stx (... ...+ _...superclass-args...)
    [(_ [id contract])
     (identifier? #'id)
     #'#f]
    [(_ [id contract val])
     (identifier? #'id)
     #'(schemeblock0 val)]
    [(_ [kw id contract])
     (keyword? (syntax-e #'kw))
     #'#f]
    [(_ [kw id contract val])
     (keyword? (syntax-e #'kw))
     #'(schemeblock0 val)]
    [_ #'#f]))

(define-syntax (extract-proc-id stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     #`(quote-syntax/loc id)]
    [(_ (proto arg ...))
     #'(extract-proc-id proto)]
    [(_ thing) (raise-syntax-error 'defproc "bad prototype" #'thing)]))

(define-syntax (arg-contracts stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-contract arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-contracts proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (arg-defaults stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-default arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-defaults proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (result-contract stx)
  (syntax-case stx (values)
    [(_ (values c ...))
     #'(list (schemeblock0 c) ...)]
    [(_ c)
     (if (string? (syntax-e #'c))
       (raise-syntax-error 'defproc
                           "expected a result contract, found a string" #'c)
       #'(schemeblock0 c))]))

(define-syntax-rule (defproc (id arg ...) result desc ...)
  (defproc* [[(id arg ...) result]] desc ...))
(define-syntax defproc*
  (syntax-rules ()
    [(_ [[proto result] ...] desc ...)
     (defproc* #:mode procedure #:within #f [[proto result] ...] desc ...)]
    [(_ #:mode m #:within cl [[proto result] ...] desc ...)
     (*defproc 'm (quote-syntax/loc cl)
               (list (extract-proc-id proto) ...)
               '[proto ...]
               (list (arg-contracts proto) ...)
               (list (arg-defaults proto) ...)
               (list (lambda () (result-contract result)) ...)
               (lambda () (list desc ...)))]))
(define-syntax defstruct
  (syntax-rules ()
    [(_ name fields #:mutable #:inspector #f desc ...)
     (**defstruct name fields #f #t desc ...)]
    [(_ name fields #:mutable #:transparent desc ...)
     (**defstruct name fields #f #t desc ...)]
    [(_ name fields #:mutable desc ...)
     (**defstruct name fields #f #f desc ...)]
    [(_ name fields #:inspector #f desc ...)
     (**defstruct name fields #t #t desc ...)]
    [(_ name fields #:transparent desc ...)
     (**defstruct name fields #t #t desc ...)]
    [(_ name fields desc ...)
     (**defstruct name fields #t #f desc ...)]))
(define-syntax-rule (**defstruct name ([field field-contract] ...) immutable?
                                 transparent? desc ...)
  (*defstruct (quote-syntax/loc name) 'name
              '([field field-contract] ...)
              (list (lambda () (schemeblock0 field-contract)) ...)
              immutable? transparent? (lambda () (list desc ...))))
(define-syntax (defform*/subs stx)
  (syntax-case stx ()
    [(_ #:id defined-id #:literals (lit ...) [spec spec1 ...]
        ([non-term-id non-term-form ...] ...)
        desc ...)
     (with-syntax ([new-spec
                    (let loop ([spec #'spec])
                      (if (and (identifier? spec)
                               (free-identifier=? spec #'defined-id))
                        (datum->syntax #'here '(unsyntax x) spec spec)
                        (syntax-case spec ()
                          [(a . b)
                           (datum->syntax spec
                                          (cons (loop #'a) (loop #'b))
                                          spec
                                          spec)]
                          [_ spec])))])
       #'(*defforms (quote-syntax/loc defined-id) '(lit ...)
                    '(spec spec1 ...)
                    (list (lambda (x) (schemeblock0/form new-spec))
                          (lambda (ignored) (schemeblock0/form spec1)) ...)
                    '((non-term-id non-term-form ...) ...)
                    (list (list (lambda () (scheme non-term-id))
                                (lambda () (schemeblock0/form non-term-form))
                                ...)
                          ...)
                    (lambda () (list desc ...))))]
    [(fm #:id id [spec spec1 ...] ([non-term-id non-term-form ...] ...)
         desc ...)
     #'(fm #:id id #:literals () [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           desc ...)]
    [(fm #:literals lits [(spec-id . spec-rest) spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (with-syntax ([(_ _ _ [spec . _] . _) stx])
       #'(fm #:id spec-id #:literals lits [spec spec1 ...]
             ([non-term-id non-term-form ...] ...)
             desc ...))]
    [(fm [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
     #'(fm #:literals () [spec spec1 ...] ([non-term-id non-term-form ...] ...)
           desc ...)]))
(define-syntax (defform* stx)
  (syntax-case stx ()
    [(_ #:id id #:literals lits [spec ...] desc ...)
     #'(defform*/subs #:id id #:literals lits [spec ...] () desc ...)]
    [(_ #:literals lits [spec ...] desc ...)
     #'(defform*/subs #:literals lits [spec ...] () desc ...)]
    [(_ [spec ...] desc ...)
     #'(defform*/subs [spec ...] () desc ...)]))
(define-syntax (defform stx)
  (syntax-case stx ()
    [(_ #:id id #:literals (lit ...) spec desc ...)
     #'(defform*/subs #:id id #:literals (lit ...) [spec] () desc ...)]
    [(_ #:id id spec desc ...)
     #'(defform*/subs #:id id #:literals () [spec] () desc ...)]
    [(_ #:literals (lit ...) spec desc ...)
     #'(defform*/subs #:literals (lit ...) [spec] () desc ...)]
    [(_ spec desc ...)
     #'(defform*/subs [spec] () desc ...)]))
(define-syntax (defform/subs stx)
  (syntax-case stx ()
    [(_ #:id id #:literals lits spec subs desc ...)
     #'(defform*/subs #:id id #:literals lits [spec] subs desc ...)]
    [(_ #:id id spec subs desc ...)
     #'(defform*/subs #:id id #:literals () [spec] subs desc ...)]
    [(_ #:literals lits spec subs desc ...)
     #'(defform*/subs #:literals lits [spec] subs desc ...)]
    [(_ spec subs desc ...)
     #'(defform*/subs [spec] subs desc ...)]))
(define-syntax (defform/none stx)
  (syntax-case stx ()
    [(_ #:literals (lit ...) spec desc ...)
     #'(*defforms #f '(lit ...)
                  '(spec) (list (lambda (ignored) (schemeblock0/form spec)))
                  null null
                  (lambda () (list desc ...)))]
    [(_ spec desc ...)
     #'(defform/none #:literals () spec desc ...)]))
(define-syntax (defidform stx)
  (syntax-case stx ()
    [(_ spec-id desc ...)
     #'(*defforms (quote-syntax/loc spec-id) null
                  '(spec-id)
                  (list (lambda (x) (make-paragraph (list x))))
                  null
                  null
                  (lambda () (list desc ...)))]))
(define-syntax (defsubform stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform . rest))]))
(define-syntax (defsubform* stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform* . rest))]))
(define-syntax specsubform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (*specsubform 'spec #f '(lit ...) (lambda () (schemeblock0/form spec))
                   null null (lambda () (list desc ...)))]
    [(_ spec desc ...)
     (*specsubform 'spec #f null (lambda () (schemeblock0/form spec))
                   null null (lambda () (list desc ...)))]))
(define-syntax specsubform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (*specsubform 'spec #f '(lit ...) (lambda () (schemeblock0/form spec))
                   '((non-term-id non-term-form ...) ...)
                   (list (list (lambda () (scheme non-term-id))
                               (lambda () (schemeblock0/form non-term-form))
                               ...)
                         ...)
                   (lambda () (list desc ...)))]
    [(_ spec subs desc ...)
     (specsubform/subs #:literals () spec subs desc ...)]))
(define-syntax-rule (specspecsubform spec desc ...)
  (make-blockquote "leftindent" (list (specsubform spec desc ...))))
(define-syntax-rule (specspecsubform/subs spec subs desc ...)
  (make-blockquote "leftindent" (list (specsubform/subs spec subs desc ...))))
(define-syntax specform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (*specsubform 'spec #t '(lit ...) (lambda () (schemeblock0/form spec))
                   null null (lambda () (list desc ...)))]
    [(_ spec desc ...)
     (*specsubform 'spec #t null (lambda () (schemeblock0/form spec))
                   null null (lambda () (list desc ...)))]))
(define-syntax specform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (*specsubform 'spec #t
                   '(lit ...)
                   (lambda () (schemeblock0/form spec))
                   '((non-term-id non-term-form ...) ...)
                   (list (list (lambda () (scheme non-term-id))
                               (lambda () (schemeblock0/form non-term-form))
                               ...)
                         ...)
                   (lambda () (list desc ...)))]
    [(_ spec ([non-term-id non-term-form ...] ...) desc ...)
     (specform/subs #:literals () spec ([non-term-id non-term-form ...] ...)
                    desc ...)]))
(define-syntax-rule (specsubform/inline spec desc ...)
  (*specsubform 'spec #f null #f null null (lambda () (list desc ...))))
(define-syntax-rule (defthing id result desc ...)
  (*defthing (list (quote-syntax/loc id)) (list 'id) #f
             (list (schemeblock0 result))
             (lambda () (list desc ...))))
(define-syntax-rule (defthing* ([id result] ...) desc ...)
  (*defthing (list (quote-syntax/loc id) ...) (list 'id ...) #f
             (list (schemeblock0 result) ...)
             (lambda () (list desc ...))))
(define-syntax-rule (defparam id arg contract desc ...)
  (defproc* ([(id) contract] [(id [arg contract]) void?]) desc ...))
(define-syntax-rule (defparam* id arg in-contract out-contract desc ...)
  (defproc* ([(id) out-contract] [(id [arg in-contract]) void?]) desc ...))
(define-syntax-rule (defboolparam id arg desc ...)
  (defproc* ([(id) boolean?] [(id [arg any/c]) void?]) desc ...))
(define-syntax schemegrammar
  (syntax-rules ()
    [(_ #:literals (lit ...) id clause ...)
     (*schemegrammar '(lit ...)
                     '(id clause ...)
                     (lambda ()
                       (list (list (scheme id)
                                   (schemeblock0/form clause) ...))))]
    [(_ id clause ...) (schemegrammar #:literals () id clause ...)]))
(define-syntax schemegrammar*
  (syntax-rules ()
    [(_ #:literals (lit ...) [id clause ...] ...)
     (*schemegrammar '(lit ...)
                     '(id ... clause ... ...)
                     (lambda ()
                       (list (list (scheme id) (schemeblock0/form clause) ...)
                             ...)))]
    [(_ [id clause ...] ...)
     (schemegrammar #:literals () [id clause ...] ...)]))
(define-syntax-rule (var id)
  (*var 'id))
(define-syntax-rule (svar id)
  (*var 'id))

(define (defthing/proc id contract descs)
  (*defthing (list id) (list (syntax-e id)) #f (list contract)
             (lambda () descs)))

(define (into-blockquote s)
  (make-blockquote "leftindent"
                   (if (splice? s)
                     (flow-paragraphs (decode-flow (splice-run s)))
                     (list s))))

(define (make-table-if-necessary style content)
  (if (= 1 (length content))
    (let ([paras (append-map flow-paragraphs (car content))])
      (if (andmap paragraph? paras)
        (list (make-paragraph (append-map paragraph-content paras)))
        (list (make-table style content))))
    (list (make-table style content))))

(define max-proto-width 65)

(define (name-this-object type-sym)
  (to-element
   (string->symbol
    (regexp-replace
     #rx"(%|<%>|-mixin)$"
     (format "_a~a-~s"
             (if (member (string-ref (symbol->string type-sym) 0)
                         '(#\a #\e #\i #\o #\u))
               "n"
               "")
             type-sym)
     ""))))

(define (annote-exporting-library e)
  (make-delayed-element
   (lambda (render p ri)
     (let ([from (resolve-get/tentative p ri '(exporting-libraries #f))])
       (if (and from (pair? from))
         (list (make-hover-element
                #f
                (list e)
                (intern-taglet
                 (string-append
                  "Provided from: "
                  (let loop ([from from])
                    (if (null? (cdr from))
                      (format "~s" (car from))
                      (format "~s, ~a" (car from) (loop (cdr from)))))))))
         (list e))))
   (lambda () e)
   (lambda () e)))

(define (get-exporting-libraries render p ri)
  (resolve-get/tentative p ri '(exporting-libraries #f)))

(define (with-exporting-libraries proc)
  (make-delayed-index-desc
   (lambda (render part ri)
     (proc (or (get-exporting-libraries render part ri) null)))))

(define-struct (box-splice splice) (var-list))

(define (*deftogether boxes body-thunk)
  (make-splice
   (cons
    (make-table
     'boxed
     (map
      (lambda (box)
        (unless (and (box-splice? box)
                     (= 1 (length (splice-run box)))
                     (table? (car (splice-run box)))
                     (eq? 'boxed (table-style (car (splice-run box)))))
          (error 'deftogether
                 "element is not a boxing splice containing a single table: ~e"
                 box))
        (list (make-flow (list (make-table
                                "together"
                                (table-flowss (car (splice-run box))))))))
      boxes))
    (parameterize ([current-variable-list
                    (append-map box-splice-var-list boxes)])
      (body-thunk)))))

(define-syntax-rule (deftogether (box ...) . body)
  (*deftogether (list box ...) (lambda () (list . body))))

(define-struct arg
  (special? kw id optional? starts-optional? ends-optional? num-closers))

(define (*defproc mode within-id
                  stx-ids prototypes arg-contractss arg-valss result-contracts
                  content-thunk)
  (define ((arg->elem show-opt-start?) arg)
    (let* ([e (cond [(not (arg-special? arg))
                     (if (arg-kw arg)
                       (if (eq? mode 'new)
                         (make-element
                          #f (list (schemeparenfont "[")
                                   (schemeidfont (keyword->string (arg-kw arg)))
                                   spacer
                                   (to-element (arg-id arg))
                                   (schemeparenfont "]")))
                         (make-element
                          #f (list (to-element (arg-kw arg))
                                   spacer
                                   (to-element (arg-id arg)))))
                       (to-element (arg-id arg)))]
                    [(eq? (arg-id arg) '...+) dots1]
                    [(eq? (arg-id arg) '...) dots0]
                    [else (to-element (arg-id arg))])]
           [e (if (arg-ends-optional? arg)
                (make-element #f (list e "]"))
                e)]
           [e (if (zero? (arg-num-closers arg))
                e
                (make-element
                 #f (list e (make-closers (arg-num-closers arg)))))])
      (if (and show-opt-start? (arg-starts-optional? arg))
        (make-element #f (list "[" e))
        e)))
  (define (prototype-depth p)
    (let loop ([p (car p)])
      (if (symbol? p) 0 (+ 1 (loop (car p))))))
  (define (prototype-args p)
    (define (parse-arg v in-optional? depth next-optional? next-special-dots?)
      (let* ([id (if (pair? v) ((if (keyword? (car v)) cadr car) v) v)]
             [kw (and (pair? v) (keyword? (car v)) (car v))]
             [default? (and (pair? v) (pair? ((if kw cdddr cddr) v)))])
        (make-arg (symbol? v) kw id default?
                  (and default? (not in-optional?))
                  (or (and (not default?)
                           in-optional?) ; => must be special
                      (and default?
                           (not next-optional?)
                           (not next-special-dots?)))
                  depth)))
    (let loop ([p p] [last-depth 0])
      (append
       (if (symbol? (car p))
         null
         (loop (car p) (add1 last-depth)))
       (let loop ([p (cdr p)][in-optional? #f])
         (cond
           [(null? p) null]
           [(null? (cdr p))
            (list (parse-arg (car p) in-optional? last-depth #f #f))]
           [else
            (let ([a (parse-arg
                      (car p)
                      in-optional?
                      0
                      (let ([v (cadr p)])
                        (and (pair? v)
                             (not
                              (null? ((if (keyword? (car v)) cdddr cddr) v)))))
                      (and (not (pair? (cadr p)))
                           (not (eq? '_...superclass-args... (cadr p)))))])
              (cons a (loop (cdr p)
                            (and (arg-optional? a)
                                 (not (arg-ends-optional? a))))))])))))
  (define (prototype-size args first-combine next-combine special-combine?)
    (let loop ([s args] [combine first-combine])
      (if (null? s)
        0
        (combine
         (loop (cdr s) next-combine)
         (let ([a (car s)])
           (+ (arg-num-closers a)
              (if (arg-special? a)
                (string-length (symbol->string (arg-id a)))
                (+ (if (arg-kw a)
                     (+ (if (eq? mode 'new) 2 0)
                        (string-length (keyword->string (arg-kw a)))
                        3
                        (string-length (symbol->string (arg-id a))))
                     (string-length (symbol->string (arg-id a))))
                   (if (and special-combine?
                            (pair? (cdr s))
                            (arg-special? (cadr s))
                            (not (eq? '_...superclass-args...
                                      (arg-id (cadr s)))))
                     (+ 1 (string-length (symbol->string (arg-id (cadr s)))))
                     0)))))))))
  (define (extract-id p)
    (let loop ([p p])
      (if (symbol? (car p)) (car p) (loop (car p)))))
  (define (do-one stx-id prototype args arg-contracts arg-vals result-contract
                  first?)
    (define tagged
      (cond
        [(eq? mode 'new)
         (make-element #f (list (scheme new) spacer (to-element within-id)))]
        [(eq? mode 'make)
         (make-element
          #f (list (scheme make-object) spacer (to-element within-id)))]
        [(eq? mode 'send)
         (make-element
          #f
          (list (scheme send) spacer
                (name-this-object (syntax-e within-id)) spacer
                (if first?
                  (let* ([mname (extract-id prototype)]
                         [target-maker (id-to-target-maker within-id #f)]
                         [content (list (*method mname within-id))])
                    (if target-maker
                      (target-maker
                       content
                       (lambda (ctag)
                         (let ([tag (method-tag ctag mname)])
                           (make-toc-target-element
                            #f
                            (list (make-index-element
                                   #f
                                   content
                                   tag
                                   (list (symbol->string mname))
                                   content
                                   (with-exporting-libraries
                                    (lambda (libs)
                                      (make-method-index-desc
                                       (syntax-e within-id)
                                       libs mname ctag)))))
                            tag))))
                      (car content)))
                  (*method (extract-id prototype) within-id))))]
        [first?
         (let ([target-maker (id-to-target-maker stx-id #t)]
               [content (list (definition-site (extract-id prototype)
                                               stx-id #f))])
           (if target-maker
             (target-maker
              content
              (lambda (tag)
                (make-toc-target-element
                 #f
                 (list (make-index-element
                        #f content tag
                        (list (symbol->string (extract-id prototype)))
                        content
                        (with-exporting-libraries
                         (lambda (libs)
                           (make-procedure-index-desc (extract-id prototype)
                                                      libs)))))
                 tag)))
             (car content)))]
        [else
         (annote-exporting-library
          (let ([sig (current-signature)])
            (if sig
              (*sig-elem (sig-id sig) (extract-id prototype))
              (to-element (make-just-context (extract-id prototype)
                                             stx-id)))))]))
    (define flat-size (+ (prototype-size args + + #f)
                         (prototype-depth prototype)
                         (element-width tagged)))
    (define short? (or (flat-size . < . 40) ((length args) . < . 2)))
    (define res
      (let ([res (result-contract)])
        (if (list? res)
          ;; multiple results
          (if (null? res)
            'nbsp
            (let ([w (apply + (map block-width res))])
              (if (or (ormap table? res) (w . > . 40))
                (make-table
                 #f (map (lambda (fe) (list (make-flow (list fe)))) res))
                (make-table
                 #f
                 (list (let loop ([res res])
                         (if (null? (cdr res))
                           (list (make-flow (list (car res))))
                           (list* (make-flow (list (car res)))
                                  flow-spacer
                                  (loop (cdr res))))))))))
          res)))
    (define tagged+arg-width (+ (prototype-size args max max #t)
                                (prototype-depth prototype)
                                (element-width tagged)))
    (define result-next-line?
      ((+ (if short? flat-size tagged+arg-width) (block-width res))
       . >= . (- max-proto-width 7)))
    (define end (list flow-spacer (to-flow 'rarr)
                      flow-spacer (make-flow (list res))))
    (append
     (list
      (list
       (make-flow
        (if short?
          ;; The single-line case:
          (make-table-if-necessary
           "prototype"
           (list
            (cons
             (to-flow
              (make-element
               #f
               `(,(make-openers (add1 (prototype-depth prototype)))
                 ,tagged
                 ,@(if (null? args)
                     (list (make-closers (prototype-depth prototype)))
                     (append-map (lambda (arg)
                                   (list spacer ((arg->elem #t) arg)))
                                 args))
                 ,(schemeparenfont ")"))))
             (if result-next-line? null end))))
          ;; The multi-line case:
          (let ([not-end (if result-next-line?
                           (list flow-spacer)
                           (list flow-spacer flow-spacer
                                 flow-spacer flow-spacer))]
                [one-ok? (tagged+arg-width . < . 60)])
            (list
             (make-table
              "prototype"
              (cons
               (cons
                (to-flow
                 (make-element
                  #f
                  (list
                   (make-openers (add1 (prototype-depth prototype)))
                   tagged)))
                (if one-ok?
                  (list*
                   (if (arg-starts-optional? (car args))
                     (to-flow (make-element #f (list spacer "[")))
                     flow-spacer)
                   (to-flow ((arg->elem #f) (car args)))
                   not-end)
                  (list* 'cont 'cont not-end)))
               (let loop ([args (if one-ok? (cdr args) args)])
                 (if (null? args)
                   null
                   (let ([dots-next?
                          (or (and (pair? (cdr args))
                                   (arg-special? (cadr args))
                                   (not (eq? '_...superclass-args...
                                             (arg-id (cadr args))))))])
                     (cons
                      (list*
                       flow-spacer
                       (if (arg-starts-optional? (car args))
                         (to-flow (make-element #f (list spacer "[")))
                         flow-spacer)
                       (let ([a ((arg->elem #f) (car args))]
                             [next (if dots-next?
                                     (make-element
                                      #f (list spacer
                                               ((arg->elem #f)
                                                (cadr args))))
                                     "")])
                         (to-flow
                          (cond
                            [(null? ((if dots-next? cddr cdr) args))
                             (make-element
                              #f
                              (list a next (schemeparenfont ")")))]
                            [(equal? next "") a]
                            [else
                             (make-element #f (list a next))])))
                       (if (and (null? ((if dots-next? cddr cdr) args))
                                (not result-next-line?))
                         end
                         not-end))
                      (loop ((if dots-next? cddr cdr)
                             args))))))))))))))
     (if result-next-line?
       (list (list (make-flow (make-table-if-necessary "prototype"
                                                       (list end)))))
       null)
     (append-map
      (lambda (arg arg-contract arg-val)
        (cond
          [(not (arg-special? arg))
           (let* ([arg-cont (arg-contract)]
                  [base-len (+ 5 (string-length (symbol->string (arg-id arg)))
                               (block-width arg-cont))]
                  [arg-val (and arg-val (arg-val))]
                  [def-len (if (arg-optional? arg) (block-width arg-val) 0)]
                  [base-list
                   (list (to-flow (hspace 2))
                         (to-flow (to-element (arg-id arg)))
                         flow-spacer
                         (to-flow ":")
                         flow-spacer
                         (make-flow (list arg-cont)))])
             (list
              (list
               (make-flow
                (if (and (arg-optional? arg)
                         ((+ base-len 3 def-len) . >= . max-proto-width))
                  (list
                   (make-table
                    "argcontract"
                    (list base-list (list flow-spacer flow-spacer flow-spacer
                                          (to-flow "=") flow-spacer
                                          (make-flow (list arg-val))))))
                  (make-table-if-necessary
                   "argcontract"
                   (list
                    (append
                     base-list
                     (if (and (arg-optional? arg)
                              ((+ base-len 3 def-len) . < . max-proto-width))
                       (list flow-spacer (to-flow "=") flow-spacer
                             (make-flow (list arg-val)))
                       null)))))))))]
          [else null]))
      args
      arg-contracts
      arg-vals)))
  (define all-args (map prototype-args prototypes))
  (define var-list
    (filter-map (lambda (a) (and (not (arg-special? a)) (arg-id a)))
                (append* all-args)))
  (parameterize ([current-variable-list var-list])
    (make-box-splice
     (cons
      (make-table
       'boxed
       (append-map
        do-one
        stx-ids prototypes all-args arg-contractss arg-valss result-contracts
        (let loop ([ps prototypes] [accum null])
          (cond [(null? ps) null]
                [(ormap (lambda (a) (eq? (extract-id (car ps)) a)) accum)
                 (cons #f (loop (cdr ps) accum))]
                [else (cons #t (loop (cdr ps)
                                     (cons (extract-id (car ps)) accum)))]))))
      (content-thunk))
     var-list)))

(define (make-target-element* inner-make-target-element stx-id content wrappers)
  (if (null? wrappers)
    content
    (make-target-element*
     make-target-element
     stx-id
     (let* ([name (string-append* (map symbol->string (cdar wrappers)))]
            [target-maker
             (id-to-target-maker (datum->syntax stx-id (string->symbol name))
                                 #t)])
       (if target-maker
         (target-maker
          (list content)
          (lambda (tag)
            (inner-make-target-element
             #f
             (list
              (make-index-element
               #f
               (list content)
               tag
               (list name)
               (list (schemeidfont (make-element "schemevaluelink"
                                                 (list name))))
               (with-exporting-libraries
                (lambda (libs)
                  (let ([name (string->symbol name)])
                    (if (eq? 'info (caar wrappers))
                      (make-struct-index-desc name libs)
                      (make-procedure-index-desc name libs)))))))
             tag)))
         content))
     (cdr wrappers))))

(define (*defstruct stx-id name fields field-contracts immutable? transparent?
                    content-thunk)
  (define (field-name f) ((if (pair? (car f)) caar car) f))
  (define (field-view f)
    (if (pair? (car f)) (make-shaped-parens (car f) #\[) (car f)))
  (make-box-splice
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
                              (if (pair? name)
                                (car (syntax-e stx-id))
                                stx-id)
                              (annote-exporting-library
                               (to-element
                                (if (pair? name)
                                  (make-just-context (car name)
                                                     (car (syntax-e stx-id)))
                                  stx-id)))
                              (let ([name (if (pair? name) (car name) name)])
                                (list* (list 'info name)
                                       (list 'type 'struct: name)
                                       (list 'predicate name '?)
                                       (list 'constructor 'make- name)
                                       (append
                                        (map (lambda (f)
                                               (list 'accessor name '-
                                                     (field-name f)))
                                             fields)
                                        (filter-map
                                         (lambda (f)
                                           (if (or (not immutable?)
                                                   (and (pair? (car f))
                                                        (memq '#:mutable
                                                              (car f))))
                                             (list 'mutator 'set- name '-
                                                   (field-name f) '!)
                                             #f))
                                         fields)))))])
                        (if (pair? name)
                          (to-element (list just-name
                                            (make-just-context
                                             (cadr name)
                                             (cadr (syntax-e stx-id)))))
                          just-name))]
                     [short-width
                      (apply + (length fields) 8
                             (append
                              (map (lambda (s)
                                     (string-length (symbol->string s)))
                                   (append (if (pair? name) name (list name))
                                           (map field-name fields)))
                              (map (lambda (f)
                                     (if (pair? (car f))
                                       (+ 3 2 (string-length (keyword->string
                                                              (cadar f))))
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
                           flow-spacer
                           (to-flow the-name)
                           (if (or (null? fields)
                                   (short-width . < . max-proto-width))
                             flow-spacer
                             (to-flow (make-element
                                       #f (list spacer (schemeparenfont "(")))))
                           (to-flow (if (or (null? fields)
                                            (short-width . < . max-proto-width))
                                      (make-element
                                       #f (list (to-element (map field-view
                                                                 fields))
                                                (schemeparenfont ")")))
                                      (to-element (field-view (car fields)))))))
                    (if (short-width . < . max-proto-width)
                      null
                      (let loop ([fields (if (null? fields)
                                           fields (cdr fields))])
                        (if (null? fields)
                          null
                          (cons
                           (let ([fld (car fields)])
                             (list flow-spacer flow-spacer
                                   flow-spacer flow-spacer
                                   (to-flow
                                    (let ([e (to-element (field-view fld))])
                                      (if (null? (cdr fields))
                                        (make-element
                                         #f
                                         (list e (schemeparenfont
                                                  (if (and immutable?
                                                           (not transparent?))
                                                    "))" ")"))))
                                        e)))))
                           (loop (cdr fields))))))
                    (cond
                      [(and (not immutable?) transparent?)
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (to-element '#:mutable))
                              'cont
                              'cont)
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (to-element '#:transparent)
                                              (schemeparenfont ")"))))
                              'cont
                              'cont))]
                      [(not immutable?)
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (to-element '#:mutable)
                                              (schemeparenfont ")"))))
                              'cont
                              'cont))]
                      [transparent?
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (to-element '#:transparent)
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
                   "argcontract"
                   (list (list (to-flow (hspace 2))
                               (to-flow (to-element (field-name v)))
                               flow-spacer
                               (to-flow ":")
                               flow-spacer
                               (make-flow (list (field-contract))))))))]
               [else null]))
           fields field-contracts)))
    (content-thunk))
   null))

(define (*defthing stx-ids names form? result-contracts content-thunk)
  (make-box-splice
   (cons
    (make-table
     'boxed
     (map
      (lambda (stx-id name result-contract)
        (list
         (make-flow
          (make-table-if-necessary
           "argcontract"
           (list
            (list
             (make-flow
              (list
               (make-paragraph
                (list
                 (let ([target-maker
                        ((if form? id-to-form-target-maker id-to-target-maker)
                         stx-id #t)]
                       [content (list (definition-site name stx-id form?))])
                   (if target-maker
                     (target-maker
                      content
                      (lambda (tag)
                        (make-toc-target-element
                         #f
                         (list
                          (make-index-element
                           #f
                           content
                           tag
                           (list (symbol->string name))
                           content
                           (with-exporting-libraries
                            (lambda (libs) (make-thing-index-desc name libs)))))
                         tag)))
                     (car content)))
                 spacer ":" spacer))))
             (make-flow (list (if (block? result-contract)
                                result-contract
                                (make-paragraph (list result-contract)))))))))))
      stx-ids names result-contracts))
    (content-thunk))
   null))

(define (meta-symbol? s) (memq s '(... ...+ ?)))

(define (*defforms kw-id lits forms form-procs subs sub-procs content-thunk)
  (define var-list
    (let loop ([form (cons forms subs)])
      (cond [(symbol? form)
             (if (or (meta-symbol? form)
                     (and kw-id (eq? form (syntax-e kw-id)))
                     (memq form lits))
               null
               (list form))]
            [(pair? form) (append (loop (car form)) (loop (cdr form)))]
            [else null])))
  (parameterize ([current-variable-list var-list]
                 [current-meta-list '(... ...+)])
    (make-box-splice
     (cons
      (make-table
       'boxed
       (append
        (map
         (lambda (form form-proc)
           (list
            (make-flow
             (list
              ((or form-proc
                   (lambda (x)
                     (make-paragraph
                      (list (to-element `(,x . ,(cdr form)))))))
               (and kw-id
                    (eq? form (car forms))
                    (let ([target-maker (id-to-form-target-maker kw-id #t)]
                          [content (list (definition-site (syntax-e kw-id)
                                                          kw-id #t))])
                      (if target-maker
                        (target-maker
                         content
                         (lambda (tag)
                           (make-toc-target-element
                            #f
                            (if kw-id
                              (list (make-index-element
                                     #f content tag
                                     (list (symbol->string (syntax-e kw-id)))
                                     content
                                     (with-exporting-libraries
                                      (lambda (libs)
                                        (make-form-index-desc (syntax-e kw-id)
                                                              libs)))))
                              content)
                            tag)))
                        (car content)))))))))
         forms form-procs)
        (if (null? sub-procs)
          null
          (list (list flow-empty-line)
                (list (make-flow
                       (list (let ([l (map (lambda (sub)
                                             (map (lambda (f) (f)) sub))
                                           sub-procs)])
                               (*schemerawgrammars "specgrammar"
                                                   (map car l)
                                                   (map cdr l))))))))))
      (content-thunk))
     var-list)))

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
          (list (list flow-empty-line)
                (list (make-flow
                       (list (let ([l (map (lambda (sub)
                                             (map (lambda (f) (f)) sub))
                                           sub-procs)])
                               (*schemerawgrammars "specgrammar"
                                                   (map car l)
                                                   (map cdr l))))))))))
      (flow-paragraphs (decode-flow (content-thunk)))))))

(define (*schemerawgrammars style nonterms clauseses)
  (make-table
   `((valignment baseline baseline baseline baseline baseline)
     (alignment right left center left left)
     (style ,style))
   (cdr
    (append-map
     (lambda (nonterm clauses)
       (list*
        (list flow-empty-line flow-empty-line flow-empty-line
              flow-empty-line flow-empty-line)
        (list (to-flow nonterm) flow-empty-line (to-flow "=") flow-empty-line
              (make-flow (list (car clauses))))
        (map (lambda (clause)
               (list flow-empty-line flow-empty-line
                     (to-flow "|") flow-empty-line
                     (make-flow (list clause))))
             (cdr clauses))))
     nonterms clauseses))))

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
      (*schemerawgrammars #f
                          (map (lambda (x)
                                 (make-element #f
                                               (list (hspace 2)
                                                     (car x))))
                               l)
                          (map cdr l)))))

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
  (make-paragraph (cons (hspace 2) (map (lambda (s)
                                          (if (string? s)
                                            (make-element 'tt (list s))
                                            s))
                                        s))))

(define (elemtag t . body)
  (make-target-element #f (decode-content body) `(elem ,t)))
(define (elemref t . body)
  (make-link-element #f (decode-content body) `(elem ,t)))
(provide elemtag elemref)

(define (doc-prefix doc s)
  (if doc (list (module-path-prefix->string doc) s) s))

(define (secref s #:underline? [u? #t] #:doc [doc #f])
  (make-link-element (if u? #f "plainlink") null `(part ,(doc-prefix doc s))))
(define (seclink tag #:underline? [u? #t] #:doc [doc #f] . s)
  (make-link-element (if u? #f "plainlink") (decode-content s)
                     `(part ,(doc-prefix doc tag))))

(define (other-manual #:underline? [u? #t] doc)
  (secref #:doc doc #:underline? u? "top"))

(define (*schemelink stx-id id . s)
  (let ([content (decode-content s)])
    (make-delayed-element
     (lambda (r p ri)
       (list
        (make-link-element
         #f
         content
         (or (find-scheme-tag p ri stx-id #f)
             `(undef ,(format "--UNDEFINED:~a--" (syntax-e stx-id)))))))
     (lambda () content)
     (lambda () content))))

(define-syntax-rule (schemelink id . content)
  (*schemelink (quote-syntax id) 'id . content))
(provide secref seclink schemelink other-manual)

(define (pidefterm . s)
  (let ([c (apply defterm s)])
    (index (string-append (content->string (element-content c)) "s")
           c)))
(provide pidefterm)

(provide hash-lang)
(define (hash-lang)
  (make-link-element
   "schememodlink"
   (list (schememodfont "#lang"))
   `(part ,(doc-prefix '(lib "scribblings/guide/guide.scrbl") "hash-lang"))))

;; ----------------------------------------

(provide math)
(define (math . s)
  (let ([c (decode-content s)])
    (make-element
     #f
     (append-map
      (lambda (i)
        (let loop ([i i])
          (cond
            [(string? i)
             (cond
               [(regexp-match #px"^(.*)_([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'subscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)\\^([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'superscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)([()0-9{}\\[\\]\u03C0])(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (caddr m))
                             (loop (cadddr m))))]
               [else
                (list (make-element 'italic (list i)))])]
            [(eq? i 'rsquo) (list 'prime)]
            [else (list i)])))
      c))))

;; ----------------------------------------

(provide cite
         bib-entry
         (rename-out [a-bib-entry? bib-entry?])
         bibliography)

(define (cite key . keys)
  (make-element
   #f
   (list "["
         (let loop ([keys (cons key keys)])
           (if (null? (cdr keys))
             (make-link-element
              #f
              (list (car keys))
              `(cite ,(car keys)))
             (make-element
              #f
              (list (loop (list (car keys)))
                    ", "
                    (loop (cdr keys))))))
         "]")))

(define-struct a-bib-entry (key val))

(define (bib-entry #:key key
                   #:title title
                   #:is-book? [is-book? #f]
                   #:author [author #f]
                   #:location [location #f]
                   #:date [date #f]
                   #:url [url #f])
  (make-a-bib-entry
   key
   (make-element
    #f
    (append
     (if author `(,@(decode-content (list author)) ", ") null)
     (if is-book? null '(ldquo))
     (if is-book?
       (list (italic title))
       (decode-content (list title)))
     (if location '(",") '("."))
     (if is-book? null '(rdquo))
     (if location
       `(" " ,@(decode-content (list location)) ,(if date "," "."))
       null)
     (if date `(" " ,@(decode-content (list date)) ".") null)
     (if url `(" " ,(link url (tt url))) null)))))

(define (bibliography #:tag [tag "doc-bibliography"] . citations)
  (make-unnumbered-part
   #f
   `((part ,tag))
   '("Bibliography")
   '()
   null
   (make-flow
    (list
     (make-table
      "bibliography"
      (map (lambda (c)
             (let ([key (a-bib-entry-key c)]
                   [val (a-bib-entry-val c)])
               (list
                (to-flow (make-target-element #f `("[" ,key "]") `(cite ,key)))
                flow-spacer
                (to-flow val))))
           citations))))
   null))

;; ----------------------------------------

(provide defclass
         defclass/title
         definterface
         definterface/title
         defmixin
         defmixin/title
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

(define-struct decl (name super app-mixins intfs ranges mk-head body))
(define-struct constructor (def))
(define-struct meth (names mode desc def))
(define-struct spec (def))
(define-struct impl (def))

(define (id-info id)
  (let ([b (identifier-label-binding id)])
    (if b
      (list (caddr b)
            (list-ref b 3)
            (list-ref b 4)
            (list-ref b 5)
            (list-ref b 6))
      (error 'scribble "no class/interface/mixin information for identifier: ~e"
             id))))

(define-serializable-struct cls/intf
  (name-element app-mixins super intfs methods))

(define (make-inherited-table r d ri decl)
  (define start
    (let ([key (find-scheme-tag d ri (decl-name decl) #f)])
      (if key (list (cons key (lookup-cls/intf d ri key))) null)))
  (define supers
    (if (null? start)
      null
      (cdr
       (let loop ([supers start][accum null])
         (cond
           [(null? supers) (reverse accum)]
           [(memq (car supers) accum)
            (loop (cdr supers) accum)]
           [else
            (let ([super (car supers)])
              (loop (append (filter-map
                             (lambda (i)
                               (let ([key (find-scheme-tag d ri i #f)])
                                 (and key
                                      (cons key (lookup-cls/intf d ri key)))))
                             (append
                              (reverse (cls/intf-intfs (cdr super)))
                              (if (cls/intf-super (cdr super))
                                (list (cls/intf-super (cdr super)))
                                null)
                              (reverse (cls/intf-app-mixins (cdr super)))))
                            (cdr supers))
                    (cons super accum)))])))))
  (define ht
    (let ([ht (make-hasheq)])
      (for* ([i (decl-body decl)]
             #:when (meth? i)
             [name (meth-names i)])
        (hash-set! ht name #t))
      ht))
  (define inh
    (append-map
     (lambda (super)
       (let ([inh (filter-map
                   (lambda (k)
                     (if (hash-ref ht k #f)
                       #f
                       (begin (hash-set! ht k #t)
                              (cons (symbol->string k)
                                    (**method k (car super))))))
                   (cls/intf-methods (cdr super)))])
         (if (null? inh)
           null
           (cons (make-element #f (list (make-element "inheritedlbl" '("from "))
                                        (cls/intf-name-element (cdr super))))
                 (map cdr (sort inh string<? #:key car))))))
     supers))
  (if (null? inh)
    (make-auxiliary-table "inherited" null)
    (make-auxiliary-table
     "inherited"
     (map (lambda (i) (list (to-flow i)))
          (cons (make-element "inheritedlbl" '("Inherited methods:")) inh)))))

(define (make-decl-collect decl)
  (make-part-collect-decl
   ((id-to-target-maker (decl-name decl) #f)
    (list "ignored")
    (lambda (tag)
      (make-collect-element
       #f null
       (lambda (ci)
         (collect-put!
          ci
          `(cls/intf ,(cadr tag))
          (make-cls/intf
           (make-element
            "schemesymbol"
            (list (make-link-element
                   "schemevaluelink"
                   (list (symbol->string (syntax-e (decl-name decl))))
                   tag)))
           (map id-info (decl-app-mixins decl))
           (and (decl-super decl)
                (not (free-label-identifier=? (quote-syntax object%)
                                              (decl-super decl)))
                (id-info (decl-super decl)))
           (map id-info (decl-intfs decl))
           (append-map (lambda (m)
                         (let loop ([l (meth-names m)])
                           (cond [(null? l) null]
                                 [(memq (car l) (cdr l)) (loop (cdr l))]
                                 [else (cons (car l) (loop (cdr l)))])))
                       (filter meth? (decl-body decl)))))))))))

(define (build-body decl body)
  `(,@(map (lambda (i)
             (cond [(constructor? i) ((constructor-def i))]
                   [(meth? i) ((meth-def i) (meth-desc i))]
                   [else i]))
           body)
    ,(make-delayed-block (lambda (r d ri) (make-inherited-table r d ri decl)))))

(define (*include-class/title decl)
  (make-splice
   (list* (title #:style 'hidden (to-element (decl-name decl)))
          (make-decl-collect decl)
          (build-body decl (append ((decl-mk-head decl) #t)
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
        (decode-flow (build-body decl (decl-body decl))))))))))

(define (*class-doc kind stx-id super intfs ranges whole-page? make-index-desc)
  (make-table
   'boxed
   (append
    (list
     (list (make-flow
            (list
             (make-paragraph
              (list (let ([target-maker (id-to-target-maker stx-id #t)]
                          [content (list (annote-exporting-library
                                          (to-element stx-id)))])
                      (if target-maker
                        (target-maker
                         content
                         (lambda (tag)
                           ((if whole-page?
                              make-page-target-element
                              make-toc-target-element)
                            #f
                            (list
                             (make-index-element
                              #f content tag
                              (list (symbol->string (syntax-e stx-id)))
                              content
                              (with-exporting-libraries
                               (lambda (libs)
                                 (make-index-desc (syntax-e stx-id) libs)))))
                            tag)))
                        (car content)))
                    spacer ":" spacer
                    (case kind
                      [(class) (scheme class?)]
                      [(interface) (scheme interface?)]
                      [(mixin) (schemeblockelem (class? . -> . class?))])))))))
    (if super
      (list
       (list (make-flow
              (list (t (hspace 2) "superclass:" spacer (to-element super))))))
      null)
    (let ([show-intfs
           (lambda (intfs range?)
             (if (null? intfs)
               null
               (list
                (list
                 (make-flow
                  (list
                   (make-table
                    #f
                    (cons
                     (list (make-flow
                            (list (make-paragraph
                                   (list (hspace 2)
                                         (case kind
                                           [(interface) "implements:"]
                                           [(class) "extends:"]
                                           [(mixin)
                                            (if range?
                                              "result implements:"
                                              "argument extends/implements:")])
                                         spacer))))
                           (to-flow (to-element (car intfs))))
                     (map (lambda (i)
                            (list flow-spacer (to-flow (to-element i))))
                          (cdr intfs))))))))))])
      (append (show-intfs intfs #f) (show-intfs ranges #t))))))

(define-syntax extract-super
  (syntax-rules ()
    [(_ (mixin base)) (extract-super base)]
    [(_ super) (quote-syntax/loc super)]))

(define-syntax extract-app-mixins
  (syntax-rules ()
    [(_ (mixin base)) (cons (quote-syntax/loc mixin) (extract-app-mixins base))]
    [(_ super) null]))

(define (flatten-splices l)
  (let loop ([l l])
    (cond [(null? l) null]
          [(splice? (car l)) (append (splice-run (car l)) (loop (cdr l)))]
          [else (cons (car l) (loop (cdr l)))])))

(define-syntax-rule (*defclass *include-class name super (intf ...) body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                (extract-super super)
                (extract-app-mixins super)
                (list (quote-syntax/loc intf) ...)
                null
                (lambda (whole-page?)
                  (list (*class-doc 'class
                                    (quote-syntax/loc name)
                                    (quote-syntax/loc super)
                                    (list (quote-syntax intf) ...)
                                    null
                                    whole-page?
                                    make-class-index-desc)))
                (flatten-splices (list body ...))))))

(define-syntax-rule (defclass name super (intf ...) body ...)
  (*defclass *include-class name super (intf ...) body ...))

(define-syntax-rule (defclass/title name super (intf ...) body ...)
  (*defclass *include-class/title name super (intf ...) body ...))

(define-syntax-rule (*definterface *include-class name (intf ...) body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                #f
                null
                (list (quote-syntax/loc intf) ...)
                null
                (lambda (whole-page?)
                  (list
                   (*class-doc 'interface
                               (quote-syntax/loc name)
                               #f
                               (list (quote-syntax intf) ...)
                               null
                               whole-page?
                               make-interface-index-desc)))
                (list body ...)))))

(define-syntax-rule (definterface name (intf ...) body ...)
  (*definterface *include-class name (intf ...) body ...))

(define-syntax-rule (definterface/title name (intf ...) body ...)
  (*definterface *include-class/title name (intf ...) body ...))

(define-syntax-rule (*defmixin *include-class name (domain ...) (range ...)
                               body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                #f
                null
                (list (quote-syntax/loc domain) ...)
                (list (quote-syntax/loc range) ...)
                (lambda (whole-page?)
                  (list
                   (*class-doc 'mixin
                               (quote-syntax/loc name)
                               #f
                               (list (quote-syntax domain) ...)
                               (list (quote-syntax range) ...)
                               whole-page?
                               make-mixin-index-desc)))
                (list body ...)))))

(define-syntax-rule (defmixin name (domain ...) (range ...) body ...)
  (*defmixin *include-class name (domain ...) (range ...) body ...))

(define-syntax-rule (defmixin/title name (domain ...) (range ...) body ...)
  (*defmixin *include-class/title name (domain ...) (range ...) body ...))

(define-syntax (defconstructor*/* stx)
  (syntax-case stx ()
    [(_ mode ((arg ...) ...) desc ...)
     (let ([n (syntax-parameter-value #'current-class)])
       (with-syntax ([name n]
                     [result
                      (datum->syntax
                       #f
                       (list
                        (datum->syntax #'is-a?/c 'is-a?/c (list 'src 1 1 2 1))
                        (datum->syntax n (syntax-e n) (list 'src 1 3 4 1)))
                       (list 'src 1 0 1 5))]
                     [(((kw ...) ...) ...)
                      (map (lambda (ids)
                             (map (lambda (arg)
                                    (if (and (pair? (syntax-e arg))
                                             (eq? (syntax-e #'mode) 'new))
                                      (list (string->keyword
                                             (symbol->string
                                              (syntax-e
                                               (car (syntax-e arg))))))
                                      null))
                                  (syntax->list ids)))
                           (syntax->list #'((arg ...) ...)))])
         #'(make-constructor (lambda ()
                               (defproc* #:mode mode #:within name
                                 [[(make [kw ... . arg] ...) result] ...]
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
     #'(defconstructor*/* new (([id . arg-rest] ... _...superclass-args...))
         desc ...)]))

(define-syntax (defmethod* stx)
  (syntax-case stx ()
    [(_ #:mode mode ([(name arg ...) result-type] ...) desc ...)
     (with-syntax ([cname (syntax-parameter-value #'current-class)]
                   [name1 (car (syntax->list #'(name ...)))])
       (with-syntax ([(extra ...)
                      (case (syntax-e #'mode)
                        [(pubment)
                         #'((t "Refine this method with "
                               (scheme augment) "."))]
                        [(override extend augment)
                         #'((t (case (syntax-e #'mode)
                                 [(override) "Overrides "]
                                 [(extend) "Extends "]
                                 [(augment) "Augments "])
                               (*xmethod/super (quote-syntax/loc cname) 'name1)
                               "."))]
                        [else null])])
         #'(make-meth '(name ...)
                      'mode
                      (lambda ()
                        (make-splice
                         (append-map (lambda (f)
                                       (cond [(impl? f) ((impl-def f))]
                                             [(spec? f) ((spec-def f))]
                                             [else (list f)]))
                                     (list extra ... desc ...))))
                      (lambda (desc-splice)
                        (defproc* #:mode send #:within cname
                          ([(name arg ...) result-type] ...)
                          (desc-splice))))))]
    [(_ ([(name arg ...) result-type] ...) desc ...)
     #'(defmethod* #:mode public ([(name arg ...) result-type] ...) desc ...)]))

(define-syntax defmethod
  (syntax-rules ()
    [(_ #:mode mode (name arg ...) result-type desc ...)
     (defmethod* #:mode mode ([(name arg ...) result-type]) desc ...)]
    [(_ (name arg ...) result-type desc ...)
     (defmethod #:mode public (name arg ...) result-type desc ...)]))

(define-syntax-rule (methimpl body ...)
  (make-impl (lambda () (list (italic "Default implementation:") body ...))))

(define-syntax-rule (methspec body ...)
  (make-spec (lambda () (list (italic "Specification:") body ...))))

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
           (if key
             (let ([v (lookup-cls/intf d ri key)])
               (if v
                 (append (cls/intf-app-mixins v)
                         (cons (cls/intf-super v)
                               (cls/intf-intfs v)))
                 null))
             null))])
    (make-delayed-element
     (lambda (r d ri)
       (let loop ([search (get d ri (find-scheme-tag d ri cname #f))])
         (cond
           [(null? search)
            (list (make-element #f '("<method not found>")))]
           [(not (car search))
            (loop (cdr search))]
           [else
            (let* ([a-key (find-scheme-tag d ri (car search) #f)]
                   [v (and a-key (lookup-cls/intf d ri a-key))])
              (if v
                (if (member name (cls/intf-methods v))
                  (list
                   (make-element #f
                                 (list (**method name a-key)
                                       " in "
                                       (cls/intf-name-element v))))
                  (loop (append (cdr search)
                                (get d ri (find-scheme-tag d ri (car search)
                                                           #f)))))
                (loop (cdr search))))])))
     (lambda () (format "~a in ~a" (syntax-e cname) name))
     (lambda () (format "~a in ~a" (syntax-e cname) name)))))

(define (lookup-cls/intf d ri tag)
  (let ([v (resolve-get d ri `(cls/intf ,(cadr tag)))])
    (or v (make-cls/intf "unknown" null #f null null))))

;; ----------------------------------------

(provide defsignature
         defsignature/splice
         signature-desc)

(define-syntax-rule (defsignature name (super ...) body ...)
  (*defsignature (quote-syntax name)
                 (list (quote-syntax super) ...)
                 (lambda () (list body ...))
                 #t))

(define-syntax-rule (defsignature/splice name (super ...) body ...)
  (*defsignature (quote-syntax name)
                 (list (quote-syntax super) ...)
                 (lambda () (list body ...))
                 #f))

(define-struct sig-desc (in))
(define (signature-desc . l)
  (make-sig-desc l))

(define (*defsignature stx-id supers body-thunk indent?)
  (*defthing
   (list stx-id)
   (list (syntax-e stx-id))
   #t
   (list (make-element #f '("signature")))
   (lambda ()
     (define in
       (parameterize ([current-signature (make-sig stx-id)]) (body-thunk)))
     (if indent?
       (let-values ([(pre-body post-body)
                     (let loop ([in in][pre-accum null])
                       (cond [(null? in) (values (reverse pre-accum) null)]
                             [(whitespace? (car in))
                              (loop (cdr in) (cons (car in) pre-accum))]
                             [(sig-desc? (car in))
                              (loop (cdr in)
                                    (append (reverse (sig-desc-in (car in)))
                                            pre-accum))]
                             [else (values (reverse pre-accum) in)]))])
         `(,@pre-body
           ,(make-blockquote
             "leftindent"
             (flow-paragraphs (decode-flow post-body)))))
       in))))

;; ----------------------------------------
