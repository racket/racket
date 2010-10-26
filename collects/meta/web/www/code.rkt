#lang meta/web

(require "shared.rkt"
         syntax-color/module-lexer setup/xref scribble/xref)

(provide code)

(define doc-root "http://docs.racket-lang.org/")

(define expand-namespace (make-base-namespace))
(define xref (load-collections-xref))

(define (code . strs)
  (let* ([str (apply string-append strs)]
         [str (let ([N (- 6 (length (regexp-match-positions* "\n" str)))])
                (cond [(N . > . 0) (string-append str (make-string
                                                       N #\newline))]
                      [(N . < . 0) (error 'code "too many lines in example: ~e"
                                          str)]
                      [else str]))]
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
           [e (parameterize ([read-accept-reader #t]
                             [current-namespace expand-namespace])
                (expand (read-syntax 'prog (open-input-bytes bstr))))]
           [ids (let loop ([e e])
                  (cond
                   [(and (identifier? e)
                         (syntax-original? e))
                    (let ([pos (sub1 (syntax-position e))]
                          [b (identifier-binding e)])
                      (list (list (if (and (list? b)
                                           (let-values ([(name base) (module-path-index-split (car b))])
                                             (or name base)))
                                      (let ([tag (xref-binding->definition-tag xref e 0)])
                                        (if tag
                                            (cons (if (eq? (car tag) 'form)
                                                      'linkimportform
                                                      'linkimportid)
                                                  (let-values ([(p a) (xref-tag->path+anchor
                                                                       xref tag
                                                                       #:external-root-url doc-root)])
                                                    (if a (format "~a#~a" p a) p)))
                                            'importid))
                                      'id)
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
                           (let ([mp (syntax->datum mp-stx)])
                             (let-values ([(p a)
                                           (xref-tag->path+anchor
                                            xref
                                            `(mod-path ,(format "~s" mp))
                                            #:external-root-url doc-root)])
                               (if p
                                   (list (let ([pos (sub1 (syntax-position mp-stx))])
                                           (list (cons 'modpath
                                                       (if a (format "~a#~a" p a) p))
                                                 pos
                                                 (+ pos (syntax-span mp-stx))
                                                 priority)))
                                   null)))
                           null))]
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
                                    (> (cadddr a) (cadddr b))))))])
      (apply pre (let loop ([pos 0]
                            [tokens tokens])
                   (cond
                    [(null? tokens) (list (substring* bstr pos))]
                    [(eq? (caar tokens) 'white-space) (loop pos (cdr tokens))]
                    [(= pos (cadar tokens))
                     (cons (let ([style (caar tokens)]
                                 [s (substring* bstr (cadar tokens) (caddar tokens))])
                             (if (pair? style)
                                 (a href: (cdr style) class: (format "code~a" (car style)) s)
                                 (span class: (format "code~a" style) s)))
                           (loop (caddar tokens) (cdr tokens)))]
                    [(> pos (cadar tokens))
                     (loop pos (cdr tokens))]
                    [else (cons
                           (substring* bstr pos (cadar tokens))
                           (loop (cadar tokens) tokens))]))))))
