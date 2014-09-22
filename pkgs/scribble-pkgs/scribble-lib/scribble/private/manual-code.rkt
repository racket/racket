#lang racket/base
(require syntax/strip-context
         syntax-color/module-lexer
         syntax-color/lexer-contract
         "../racket.rkt"
         "../base.rkt"
         "manual-scheme.rkt"
         "manual-style.rkt"
         scribble/core
         (for-syntax racket/base
                     syntax/parse))

(provide codeblock
         codeblock0
         typeset-code
         code)

(define-for-syntax (do-codeblock stx)
  (syntax-parse stx
    [(_ (~seq (~or (~optional (~seq #:expand expand-expr:expr)
                              #:defaults ([expand-expr #'#f])
                              #:name "#:expand keyword")
                   (~optional (~seq #:indent indent-expr:expr)
                              #:defaults ([indent-expr #'0])
                              #:name "#:expand keyword")
                   (~optional (~seq #:keep-lang-line? keep-lang-line?-expr:expr)
                              #:defaults ([keep-lang-line?-expr #'#t])
                              #:name "#:keep-lang-line? keyword")
                   (~optional (~seq #:context context-expr:expr)
                              #:name "#:context keyword")
                   (~optional (~seq #:line-numbers line-numbers:expr)
                              #:defaults ([line-numbers #'#f])
                              #:name "#:line-numbers keyword")
                   (~optional (~seq #:line-number-sep line-number-sep:expr)
                              #:defaults ([line-number-sep #'1])
                              #:name "#:line-number-sep keyword"))
              ...)
        str ...)
     #`(typeset-code str ...
                     #:expand expand-expr
                     #:keep-lang-line? keep-lang-line?-expr
                     #:indent indent-expr
                     #:context #,(if (attribute context-expr)
                                     #'context-expr
                                     (or
                                      (let ([v #'(str ...)])
                                        (and (pair? (syntax-e v))
                                             #`#'#,(car (syntax-e v))))
                                      #'#f))
                     #:line-numbers line-numbers
                     #:line-number-sep line-number-sep)]))

(define-syntax (codeblock stx) #`(code-inset #,(do-codeblock stx)))
(define-syntax (codeblock0 stx) (do-codeblock stx))

(define (typeset-code #:context [context #f]
                      #:expand [expand #f]
                      #:indent [indent 2]
                      #:keep-lang-line? [keep-lang-line? #t]
                      #:line-numbers [line-numbers #f]
                      #:line-number-sep [line-number-sep 1]
                      #:block? [block? #t]
                      . strs)
  (define-values (tokens bstr) (get-tokens strs context expand))
  (define default-color meta-color)
  ((if block? table (lambda (style lines) (make-element #f lines)))
   block-color
   ((if keep-lang-line? values cdr) ; FIXME: #lang can span lines
    (list->lines
     indent
     #:line-numbers line-numbers
     #:line-number-sep line-number-sep
     #:block? block?
     (let loop ([pos 0]
                [tokens tokens])
       (cond
         [(null? tokens) (split-lines default-color (substring bstr pos))]
         [(eq? (caar tokens) 'white-space) (loop pos (cdr tokens))]
         [(= pos (cadar tokens))
          (append (let ([style (caar tokens)]
                        [get-str (lambda ()
                                   (substring bstr (cadar tokens) (caddar tokens)))])
                    (cond
                      [(symbol? style)
                       (let ([scribble-style
                              (case style
                                [(symbol) symbol-color]
                                [(parenthesis hash-colon-keyword) paren-color]
                                [(constant string) value-color]
                                [(comment) comment-color]
                                [else default-color])])
                         (split-lines scribble-style (get-str)))]
                      [(procedure? style)
                       (list (style (get-str)))]
                      [else (list style)]))
                  (loop (caddar tokens) (cdr tokens)))]
         [(> pos (cadar tokens))
          (loop pos (cdr tokens))]
         [else (append
                (split-lines default-color (substring bstr pos (cadar tokens)))
                (loop (cadar tokens) tokens))]))))))

;; (listof string) boolean boolean -> tokens string
;; tokens is a
;; (cons metadata (listof (list T natural natural natural)))
;; T being a symbol returned as a token type from the languages lexer
;;   OR a function created by get-tokens
;; the first number being the start position
;; the second being the end position
;; the third 0 if T is a symbol, and 1 if its a function
;; the tokens are sorted by the start end end positions
(define (get-tokens strs context expand)
  (let* ([xstr (apply string-append strs)]
         [bstr (regexp-replace* #rx"(?m:^$)" xstr "\xA0")]
         [in (open-input-string bstr)])
    (port-count-lines! in)
    (let* ([tokens
            (let loop ([mode #f])
              (let-values ([(lexeme type data start end backup-delta mode)
                            (module-lexer in 0 mode)])
                (if (equal? type 'eof)
                    null
                    (cons (list type (sub1 start) (sub1 end) 0)
                          (loop (if (dont-stop? mode)
                                    (dont-stop-val mode)
                                    mode))))))]
           [e (parameterize ([read-accept-reader #t])
                ((or expand 
                     (lambda (stx) 
                       (if context
                           (replace-context context stx)
                           stx)))
                 (let ([p (open-input-string bstr)])
                   (port-count-lines! p)
                   (let loop ()
                     (let ([v (read-syntax 'prog p)])
                       (cond
                        [expand v]
                        [(eof-object? v) null]
                        [else (datum->syntax #f (cons v (loop)) v v)]))))))]
           [ids (let loop ([e e])
                  (cond
                   [(and (identifier? e)
                         (syntax-original? e))
                    (let ([pos (sub1 (syntax-position e))])
                      (list (list (lambda (str)
                                    (to-element (syntax-property
                                                 e
                                                 'display-string
                                                 str)
                                                #:escapes? #f))
                                  pos
                                  (+ pos (syntax-span e))
                                  1)))]
                   [(syntax? e) (append (loop (syntax-e e))
                                        (loop (or (syntax-property e 'origin)
                                                  null))
                                        (loop (or (syntax-property e 'disappeared-use)
                                                  null)))]
                   [(pair? e) (append (loop (car e)) (loop (cdr e)))]
                   [else null]))]
           [link-mod (lambda (mp-stx priority #:orig? [always-orig? #f])
                       (if (or always-orig?
                               (syntax-original? mp-stx))
                           (let ([mp (syntax->datum mp-stx)]
                                 [pos (sub1 (syntax-position mp-stx))])
                             (list (list (racketmodname #,mp)
                                         pos
                                         (+ pos (syntax-span mp-stx))
                                         priority)))
                           null))]
           ;; This makes sense when `expand' actually expands, and
           ;; probably not otherwise:
           [mods (let loop ([e e])
                   (syntax-case e (module #%require begin)
                     [(module name lang (mod-beg form ...))
                      (apply append
                             (link-mod #'lang 2)
                             (map loop (syntax->list #'(form ...))))]
                     [(#%require spec ...)
                      (apply append
                             (map (lambda (spec)
                                    ;; Need to add support for renaming forms, etc.:
                                    (if (module-path? (syntax->datum spec))
                                        (link-mod spec 2)
                                        null))
                                  (syntax->list #'(spec ...))))]
                     [(begin form ...)
                      (apply append
                             (map loop (syntax->list #'(form ...))))]
                     [else null]))]
           [has-hash-lang? (regexp-match? #rx"^#lang " bstr)]
           [language (if has-hash-lang?
                         (let ([m (regexp-match #rx"^#lang ([-0-9a-zA-Z/._+]+)" bstr)])
                           (if m
                               (link-mod
                                #:orig? #t
                                (datum->syntax #f
                                               (string->symbol (cadr m))
                                               (vector 'in 1 6 7 (string-length (cadr m))))
                                3)
                               null))
                         null)]
           [tokens (sort (append ids
                                 mods
                                 language
                                 (filter (lambda (x) (not (eq? (car x) 'symbol)))
                                         (if has-hash-lang?
                                             ;; Drop #lang entry:
                                             (cdr tokens)
                                             tokens)))
                         (lambda (a b)
                           (or (< (cadr a) (cadr b))
                               (and (= (cadr a) (cadr b))
                                    (> (cadddr a) (cadddr b))))))])
      (values tokens bstr))))

(define (typeset-code-line context expand lang-line . strs)
  (typeset-code
   #:context context
   #:expand expand
   #:keep-lang-line? (not lang-line)
   #:block? #f
   #:indent 0
   (let ([s (regexp-replace* #px"(?:\\s*(?:\r|\n|\r\n)\\s*)+" (apply string-append strs) " ")])
     (if lang-line
         (string-append "#lang " lang-line "\n" s)
         s))))

(define-syntax (code stx)
  (syntax-parse stx
    [(_ (~seq (~or (~optional (~seq #:expand expand-expr:expr)
                              #:defaults ([expand-expr #'#f])
                              #:name "#:expand keyword")
                   (~optional (~seq #:context context-expr:expr)
                              #:name "#:context keyword")
                   (~optional (~seq #:lang lang-line-expr:expr)
                              #:defaults ([lang-line-expr #'#f])
                              #:name "#:lang-line keyword"))
              ...)
        str ...)
     #`(typeset-code-line #,(if (attribute context-expr)
                                #'context-expr
                                (or
                                 (let ([v #'(str ...)])
                                   (and (pair? (syntax-e v))
                                        #`#'#,(car (syntax-e v))))
                                 #'#f))
                          expand-expr
                          lang-line-expr
                          str ...)]))

(define (split-lines style s)
  (cond
   [(regexp-match-positions #rx"(?:\r\n|\r|\n)" s)
    => (lambda (m)
         (append (split-lines style (substring s 0 (caar m)))
                 (list 'newline)
                 (split-lines style (substring s (cdar m)))))]
   [(regexp-match-positions #rx" +" s)
    => (lambda (m)
         (append (split-lines style (substring s 0 (caar m)))
                 (list (hspace (- (cdar m) (caar m))))
                 (split-lines style (substring s (cdar m)))))]
   [else (list (element style s))]))

(define omitable (make-style #f '(omitable)))

(define (list->lines indent-amt l 
                     #:line-numbers line-numbers
                     #:line-number-sep line-number-sep
                     #:block? block?)
  (define indent-elem (if (zero? indent-amt)
                          ""
                          (hspace indent-amt)))
  ;(list of any) delim -> (list of (list of any))
  (define (break-list lst delim)
    (let loop ([l lst] [n null] [c null])
      (cond
       [(null? l) (reverse (if (null? c) n (cons (reverse c) n)))]
       [(eq? delim (car l)) (loop (cdr l) (cons (reverse c) n) null)]
       [else (loop (cdr l) n (cons (car l) c) )])))

  (define lines (break-list l 'newline))
  (define line-cnt (length lines))
  (define line-cntl (string-length (format "~a" (+ line-cnt (or line-numbers 0)))))

  (define (prepend-line-number n r)
    (define ln (format "~a" n))
    (define lnl (string-length ln))
    (define diff (- line-cntl lnl))
    (define l1 (list (tt ln) (hspace line-number-sep)))
    (cons (make-element 'smaller 
                        (make-element 'smaller  
                                      (if (not (zero? diff))
                                          (cons (hspace diff) l1)
                                          l1)))
          r))

  (define (make-line accum-line line-number)
    (define rest (cons indent-elem accum-line))
    (list ((if block? paragraph (lambda (s e) e))
           omitable 
           (if line-numbers
               (prepend-line-number line-number rest)
               rest))))

  (for/list ([l (break-list l 'newline)]
             [i (in-naturals (or line-numbers 1))])
    (make-line l i)))


;; ----------------------------------------

(module+ test
  (require racket/list
           racket/match
           tests/eli-tester)

  (define (tokens strs)
    (define-values (toks _) (get-tokens strs #f #f))
    (for/list ([tok (rest toks)])
      (match tok
        [(list _ start end 1)
         (list 'function start end 1)]
        [_ tok])))

  (define (make-test-result  lst)
    (define-values (res _)
      (for/fold ([result null] [count 12])
                ([p lst])
        (define next (+ count (second p)))
        (define r (if (eq? (first p) 'function) 1 0))
        (values
         (cons (list (first p) count next r) result)
         next)))
    (cons `(function 6 12 1) (reverse res)))

  (test
   (tokens (list "#lang racket\n1"))
   => `((function 6 12 1) (white-space 12 13 0) (constant 13 14 0))
   (tokens (list "#lang racket\n" "(+ 1 2)"))
   => (make-test-result
       '((white-space 1)
         (parenthesis 1) (function 1)
         (white-space 1) (constant 1) (white-space 1) (constant 1)
         (parenthesis 1)))
   (tokens (list "#lang racket\n(apply x (list y))"))
   => (make-test-result
       '((white-space 1)
         (parenthesis 1)
         (function 5) (white-space 1);apply
         (function 1) (white-space 1);x
         (parenthesis 1)
         (function 4) (white-space 1) (function 1);list y
         (parenthesis 1)
         (parenthesis 1)))))
