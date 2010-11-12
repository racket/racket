#lang racket/base
(require syntax/strip-context
         syntax-color/module-lexer
         "../racket.rkt"
         "../core.rkt"
         "../base.rkt"
         "manual-scheme.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide codeblock
         typeset-code)

(define-syntax (codeblock stx)
  (syntax-parse stx
    [(_ (~seq (~or (~optional (~seq #:expand expand-expr:expr)
                              #:defaults ([expand-expr #'#f])
                              #:name "#:expand keyword")
                   (~optional (~seq #:indent indent-expr:expr)
                              #:defaults ([indent-expr #'2])
                              #:name "#:expand keyword")
                   (~optional (~seq #:keep-lang-line? keep-lang-line?-expr:expr)
                              #:defaults ([keep-lang-line?-expr #'#t])
                              #:name "#:keep-lang-line? keyword")
                   (~optional (~seq #:context context-expr:expr)
                              #:name "#:context keyword"))
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
                                      #'#f)))]))

(define (typeset-code #:context [context #f]
                      #:expand [expand #f]
                      #:indent [indent 2]
                      #:keep-lang-line? [keep-lang-line? #t]
                      . strs)
  (let* ([str (apply string-append strs)]
         [bstr (string->bytes/utf-8 (regexp-replace* #rx"(?m:^$)" str "\xA0"))]
         [in (open-input-bytes bstr)])
    (let* ([tokens
            (let loop ([mode #f])
              (let-values ([(lexeme type data start end backup-delta mode)
                            (module-lexer in 0 mode)])
                (if (eof-object? lexeme)
                    null
                    (cons (list type (sub1 start) (sub1 end) 0)
                          (loop mode)))))]
           [substring* (lambda (bstr start [end (bytes-length bstr)])
                         (bytes->string/utf-8 (subbytes bstr start end)))]
           [e (parameterize ([read-accept-reader #t])
                ((or expand 
                     (lambda (stx) 
                       (if context
                           (replace-context context stx)
                           stx)))
                 (read-syntax 'prog (open-input-bytes bstr))))]
           [ids (let loop ([e e])
                  (cond
                   [(and (identifier? e)
                         (syntax-original? e))
                    (let ([pos (sub1 (syntax-position e))])
                      (list (list (to-element e)
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
                   (syntax-case e (module require begin)
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
           [language (if (regexp-match? #rx"^#lang " bstr)
                         (let ([m (regexp-match #rx"^#lang ([-a-zA-Z/._+]+)" bstr)])
                           (if m
                               (link-mod
                                #:orig? #t
                                (datum->syntax #f
                                               (string->symbol (bytes->string/utf-8 (cadr m)))
                                               (vector 'in 1 6 7 (bytes-length (cadr m))))
                                3)
                               null))
                         null)]
           [tokens (sort (append ids
                                 mods
                                 language
                                 (filter (lambda (x) (not (eq? (car x) 'symbol)))
                                         ;; Drop #lang entry:
                                         (cdr tokens)))
                         (lambda (a b)
                           (or (< (cadr a) (cadr b))
                               (and (= (cadr a) (cadr b))
                                    (> (cadddr a) (cadddr b))))))]
           [default-color meta-color])
      (table
       block-color
       ((if keep-lang-line? values cdr) ; FIXME: #lang can span lines
        (list->lines
         indent
         (let loop ([pos 0]
                    [tokens tokens])
           (cond
            [(null? tokens) (split-lines default-color (substring* bstr pos))]
            [(eq? (caar tokens) 'white-space) (loop pos (cdr tokens))]
            [(= pos (cadar tokens))
             (append (let ([style (caar tokens)])
                       (if (symbol? style)
                           (let ([scribble-style
                                  (case style
                                    [(symbol) symbol-color]
                                    [(parenthesis) paren-color]
                                    [(constant string) value-color]
                                    [(comment) comment-color]
                                    [else default-color])])
                             (split-lines scribble-style
                                          (substring* bstr (cadar tokens) (caddar tokens))))
                           (list (caar tokens))))
                     (loop (caddar tokens) (cdr tokens)))]
            [(> pos (cadar tokens))
             (loop pos (cdr tokens))]
            [else (append
                   (split-lines default-color (substring* bstr pos (cadar tokens)))
                   (loop (cadar tokens) tokens))]))))))))


(define (split-lines style s)
  (cond
   [(regexp-match-positions #rx"(?:\r\n|\r|\n)" s)
    => (lambda (m)
         (list* (element style (substring s 0 (caar m)))
                'newline
                (split-lines style (substring s (cdar m)))))]
   [(regexp-match-positions #rx" +" s)
    => (lambda (m)
         (append (split-lines style (substring s 0 (caar m)))
                 (list (hspace (- (cdar m) (caar m))))
                 (split-lines style (substring s (cdar m)))))]
   [else (list (element style s))]))

(define omitable (make-style #f '(omitable)))

(define (list->lines indent-amt l)
  (define (make-line accum-line) (list (paragraph omitable 
                                                  (cons indent-elem
                                                        (reverse accum-line)))))
  (define indent-elem (hspace indent-amt))
  (let loop ([l l] [accum-line null])
    (cond
     [(null? l) (if (null? accum-line)
                    null
                    (list (make-line accum-line)))]
     [(eq? 'newline (car l))
      (cons (make-line accum-line)
            (loop (cdr l) null))]
     [else (loop (cdr l) (cons (car l) accum-line))])))
