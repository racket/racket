#lang plt-web

(require syntax-color/module-lexer setup/xref 
         scribble/xref scribble/tag
         (only-in "../stubs/docs.rkt" docs-path))

(provide code)

(define expand-namespace (make-base-namespace))
(define xref (load-collections-xref))

(define (code . strs)
  (define doc-root (docs-path))
  (define str
    (let* ([str (string-append* strs)]
           [N (- 6 (length (regexp-match-positions* "\n" str)))])
      (cond [(N . > . 0) (string-append str (make-string N #\newline))]
            [(N . < . 0) (error 'code "too many lines in example: ~e" str)]
            [else str])))
  (define bstr (string->bytes/utf-8 (regexp-replace* #rx"(?m:^$)" str "\xA0")))
  (define in (open-input-bytes bstr))
  (define (substring* bstr start [end (bytes-length bstr)])
    (bytes->string/utf-8 (subbytes bstr start end)))
  (define e
    (parameterize ([read-accept-reader #t] [current-namespace expand-namespace])
      (expand (read-syntax 'prog (open-input-bytes bstr)))))
  (define ids
    (let loop ([e e])
      (cond
        [(and (identifier? e) (syntax-original? e))
         (define pos (sub1 (syntax-position e)))
         (define b (identifier-binding e))
         (define imp?
           (and (list? b)
                (let-values ([(name base) (module-path-index-split (car b))])
                  (or name base))))
         (define tag (and imp? (xref-binding->definition-tag xref e 0)))
         (list (list (cond [(not imp?) 'id]
                           [(not tag) 'importid]
                           [else (define-values [p a]
                                   (xref-tag->path+anchor
                                    xref tag #:external-root-url doc-root))
                                 (cons (if (eq? (car tag) 'form)
                                         'linkimportform 'linkimportid)
                                       (if a (format "~a#~a" p a) p))])
                     pos (+ pos (syntax-span e)) 1))]
        [(syntax? e)
         (append (loop (syntax-e e))
                 (loop (or (syntax-property e 'origin) null))
                 (loop (or (syntax-property e 'disappeared-use) null)))]
        [(pair? e) (append (loop (car e)) (loop (cdr e)))]
        [else null])))
  (define (link-mod mp-stx priority #:orig? [always-orig? #f])
    (if (or always-orig? (syntax-original? mp-stx))
      (let ([mp (syntax->datum mp-stx)])
        (define-values [p a]
          (xref-tag->path+anchor xref 
                                 (make-module-language-tag mp)
                                 #:external-root-url doc-root))
        (if p
          (list (let ([pos (sub1 (syntax-position mp-stx))])
                  (list (cons 'modpath (if a (format "~a#~a" p a) p))
                        pos (+ pos (syntax-span mp-stx)) priority)))
          null))
      null))
  (define mods
    (let loop ([e e])
      (syntax-case e (module #%require begin)
        [(module name lang (mod-beg form ...))
         (append* (link-mod #'lang 2) (map loop (syntax->list #'(form ...))))]
        [(#%require spec ...)
         (append-map (λ (spec)
                       ;; Need to add support for renaming forms, etc.:
                       (if (module-path? (syntax->datum spec))
                         (link-mod spec 2)
                         null))
                     (syntax->list #'(spec ...)))]
        [(begin form ...) (append-map loop (syntax->list #'(form ...)))]
        [else null])))
  (define language
    (let ([m (regexp-match #rx"^#lang ([-a-zA-Z/._+]+)" bstr)])
      (if m
        (link-mod #:orig? #t
                  (datum->syntax #f
                                 (string->symbol (bytes->string/utf-8 (cadr m)))
                                 (vector 'in 1 6 7 (bytes-length (cadr m))))
                  3)
        null)))
  (define raw-tokens
    (let loop ([mode #f])
      (define-values [lexeme type data start end backup-delta mode*]
        (module-lexer in 0 mode))
      (if (eof-object? lexeme)
        null
        (cons (list type (sub1 start) (sub1 end) 0) (loop mode*)))))
  (define tokens
    (sort (append ids mods language
                  (filter (λ (x) (not (eq? (car x) 'symbol)))
                          ;; Drop #lang entry:
                          (cdr raw-tokens)))
          (λ (a b) (or (< (cadr a) (cadr b))
                       (and (= (cadr a) (cadr b))
                            (> (cadddr a) (cadddr b)))))))
  (let loop ([pos 0] [tokens tokens])
    (cond
      [(null? tokens) (list (substring* bstr pos))]
      [(eq? (caar tokens) 'white-space) (loop pos (cdr tokens))]
      [(= pos (cadar tokens))
       (define style (caar tokens))
       (define s     (substring* bstr (cadar tokens) (caddar tokens)))
       (cons (if (pair? style)
               (a href: (cdr style) class: @list{code@(car style)}
                  rel: 'nofollow s)
               (span class: @list{code@style} s))
             (loop (caddar tokens) (cdr tokens)))]
      [(> pos (cadar tokens)) (loop pos (cdr tokens))]
      [else (cons (substring* bstr pos (cadar tokens))
                  (loop (cadar tokens) tokens))])))
