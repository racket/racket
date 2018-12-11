#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "coerce.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "closer.rkt"
         "consume.rkt"
         "location.rkt"
         "accum-string.rkt"
         "error.rkt"
         "indentation.rkt"
         "parameter.rkt"
         "primitive-parameter.rkt"
         "special-comment.rkt"
         "sequence.rkt"
         "vector.rkt"
         "struct.rkt"
         "graph.rkt"
         "hash.rkt"
         "symbol-or-number.rkt"
         "string.rkt"
         "char.rkt"
         "quote.rkt"
         "constant.rkt"
         "box.rkt"
         "regexp.rkt"
         "extension.rkt"
         "language.rkt"
         "number.rkt")

(provide read
         read-language

         current-readtable
         make-readtable
         readtable?
         readtable-mapping

         string->number

         (all-from-out "primitive-parameter.rkt")
         (all-from-out "special-comment.rkt"))

;; This is not the `read` to be exposed from `racket/base`, but a
;; general entry to point implement `read` and variants like
;; `read-syntax` and `read/recursive`. To support syntax objects, the
;; caller should provide the `dynamic-require`, `read-compiled`,
;; `module-declared?`, and `corece` functions, even when implementing
;; a plain `read`, since those might be needed by a
;; `read-syntax/recursive`.
(define (read in
              #:wrap [wrap #f]
              #:init-c [init-c #f]
              #:next-readtable [next-readtable (current-readtable)]
              #:readtable [readtable next-readtable]
              #:recursive? [recursive? #f]
              #:local-graph? [local-graph? #f] ; ignored unless `recursive?`
              #:source [source #f]
              #:for-syntax? [for-syntax? #f]
              #:read-compiled [read-compiled #f]       ; see "config.rkt"
              #:call-with-root-namespace [call-with-root-namespace #f] ; see "config.rkt"
              #:dynamic-require [dynamic-require #f]   ; see "config.rkt"
              #:module-declared? [module-declared? #f] ; see "config.rkt"
              #:coerce [coerce #f]                     ; see "config.rkt"
              #:coerce-key [coerce-key #f]             ; see "config.rkt"
              #:keep-comment? [keep-comment? recursive?])
  (define config
    (cond
     [(and recursive?
           (current-read-config))
      => (lambda (config)
           (read-config-update config
                               #:for-syntax? for-syntax?
                               #:wrap wrap
                               #:readtable readtable
                               #:next-readtable next-readtable
                               #:reset-graph? local-graph?
                               #:keep-comment? keep-comment?))]
     [else
      (make-read-config #:readtable readtable
                        #:next-readtable next-readtable
                        #:source source
                        #:for-syntax? for-syntax?
                        #:wrap wrap
                        #:read-compiled read-compiled
                        #:call-with-root-namespace call-with-root-namespace
                        #:dynamic-require dynamic-require
                        #:module-declared? module-declared?
                        #:coerce coerce
                        #:coerce-key coerce-key
                        #:keep-comment? keep-comment?)]))
  (define v (read-one init-c in config))
  (cond
   [(and (or (not recursive?) local-graph?)
         (read-config-state-graph (read-config-st config)))
    (catch-and-reraise-as-reader
     #f config
     (make-reader-graph v))]
   [(and recursive?
         (not local-graph?)
         (not for-syntax?)
         (not (eof-object? v))
         (not (special-comment? v)))
    (get-graph-hash config) ; to trigger placeholder resolution
    v]
   [else v]))

(define (read-language in fail-k
                       #:for-syntax? [for-syntax? #f]
                       #:wrap [wrap #f]
                       #:read-compiled [read-compiled #f]
                       #:dynamic-require [dynamic-require #f]
                       #:call-with-root-namespace [call-with-root-namespace #f]
                       #:module-declared? [module-declared? #f]
                       #:coerce [coerce #f]
                       #:coerce-key [coerce-key #f])
  (define config (make-read-config #:readtable #f
                                   #:next-readtable #f
                                   #:for-syntax? for-syntax?
                                   #:wrap wrap
                                   #:read-compiled read-compiled
                                   #:call-with-root-namespace call-with-root-namespace
                                   #:dynamic-require dynamic-require
                                   #:module-declared? module-declared?
                                   #:coerce coerce
                                   #:coerce-key coerce-key))
  (define l-config (override-parameter read-accept-reader config #f))
  (read-language/get-info read-undotted in config fail-k))

;; ----------------------------------------
;; The top-level reading layer that takes care of parsing into
;; `#%cdot`.

(define (read-one init-c in config)
  (cond
   [(not (check-parameter read-cdot config))
    ;; No parsing of `.` as `#%dot`
    (read-undotted init-c in config)]
   [(check-parameter read-cdot config)
    ;; Look for `<something> . <something>`
    (define-values (line col pos) (port-next-location in))
    (define v (read-undotted init-c in config))
    (cond
     [(special-comment? v) v]
     [else
      (let loop ([v v])
        (define c (peek-char/special in config))
        (define ec (effective-char c config))
        (cond
         [(not (char? ec)) v]
         [(char-whitespace? ec)
          (consume-char in c)
          (loop v)]
         [(char=? ec #\.)
          (define-values (dot-line dot-col dot-pos) (port-next-location in))
          (consume-char in c)
          (define pos-config (reading-at config dot-line dot-col dot-pos))
          (define cdot (wrap '#%dot in pos-config #\.))
          (define post-v (read-undotted #f in config))
          (when (eof-object? post-v)
            (reader-error in pos-config #:due-to eof "expected a datum after cdot, found end-of-file"))
          (loop (wrap (list cdot v post-v) in (reading-at config line col pos) #\.))]
         [else v]))])]))

;; ----------------------------------------
;; The top-level reading layer within `#%dot` handling --- which is
;; the reader's main dispatch layer.

(define (read-undotted init-c in config)
  (define c (read-char/skip-whitespace-and-comments init-c read-one in config))
  (define-values (line col pos) (port-next-location* in c))
  (cond
   [(eof-object? c) eof]
   [(not (char? c))
    (define v (special-value c))
    (cond
     [(special-comment? v)
      (if (read-config-keep-comment? config)
          v
          (read-undotted #f in config))]
     [else (coerce v in (reading-at config line col pos))])]
   [(readtable-handler config c)
    => (lambda (handler)
         (define v (readtable-apply handler c in config line col pos))
         (retry-special-comment v in config))]
   [else
    ;; Map character via readtable:
    (define ec (effective-char c config))

    ;; Track indentation, unless it's a spurious closer:
    (when (not (char-closer? ec config))
      (track-indentation! config line col))
    (define r-config (reading-at (discard-comment config) line col pos))
    
    (define-syntax-rule (guard-legal e body ...)
      (cond
       [e body ...]
       [else (reader-error in r-config "illegal use of `~a`" c)]))
    
    ;; Dispatch on character:
    (case ec
      [(#\#)
       (read-dispatch c in r-config config)]
      [(#\')
       (read-quote read-one 'quote "quoting \"'\"" c in r-config)]
      [(#\`)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (read-quote read-one 'quasiquote "quasiquoting \"`\"" c in r-config))]
      [(#\,)
       (guard-legal
        (check-parameter read-accept-quasiquote config)
        (define c2 (peek-char/special in config))
        (if (eqv? c2 #\@)
            (begin
              (consume-char in c2)
              (read-quote read-one 'unquote-splicing "unquoting `,@`" c in r-config))
            (read-quote read-one 'unquote "unquoting `,`" c in r-config)))]
      [(#\()
       (wrap (read-unwrapped-sequence read-one ec #\( #\) in r-config #:shape-tag? #t) in r-config ec)]
      [(#\))
       (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config))]
      [(#\[)
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (wrap (read-unwrapped-sequence read-one ec #\[ #\] in r-config #:shape-tag? #t) in r-config ec))]
      [(#\])
       (guard-legal
        (or (check-parameter read-square-bracket-as-paren config)
            (check-parameter read-square-bracket-with-tag config))
        (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config)))]
      [(#\{)
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (wrap (read-unwrapped-sequence read-one ec #\{ #\} in r-config #:shape-tag? #t) in r-config ec))]
      [(#\})
       (guard-legal
        (or (check-parameter read-curly-brace-as-paren config)
            (check-parameter read-curly-brace-with-tag config))
        (reader-error in r-config "~a" (indentation-unexpected-closer-message ec c r-config)))]
      [(#\")
       (read-string in r-config)]
      [(#\|)
       (read-symbol-or-number c in r-config #:mode 'symbol)]
      [else
       (define v
         (read-symbol-or-number c in r-config
                                ;; Don't read as a number if the effective char
                                ;; is non-numeric:
                                #:mode (if (or (eq? c ec)
                                               (and ((char->integer ec) . < . 128)
                                                    (char-numeric? ec)))
                                           'symbol-or-number
                                           'symbol/indirect)))
       (retry-special-comment v in config)])]))

;; Dispatch on `#` character
(define (read-dispatch dispatch-c in config orig-config)
  (define c (read-char/special in config))
  (cond
   [(eof-object? c)
    (reader-error in config #:due-to c "bad syntax `~a`" dispatch-c)]
   [(not (char? c))
    (reader-error in config #:due-to c "bad syntax `~a`" dispatch-c)]
   [(readtable-dispatch-handler orig-config c)
    => (lambda (handler)
         (define line (read-config-line config))
         (define col (read-config-col config))
         (define pos (read-config-pos config))
         (define v (readtable-apply handler c in config line col pos))
         (retry-special-comment v in orig-config))]
   [else
    (define-syntax-rule (guard-legal e c body ...)
      (cond
       [e body ...]
       [else (bad-syntax-error in config (format "~a~a" dispatch-c c))]))
    (case c
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       ;; Vector, graph definition, or graph reference
       (read-vector-or-graph read-one dispatch-c c in config)]
      [(#\()
       (read-vector read-one #\( #\( #\) in config)]
      [(#\[)
       (guard-legal
        (check-parameter read-square-bracket-as-paren config)
        c
        (read-vector read-one #\[ #\[ #\] in config))]
      [(#\{)
       (guard-legal
        (check-parameter read-curly-brace-as-paren config)
        c
        (read-vector read-one #\{ #\{ #\} in config))]
      [(#\s)
       (read-struct read-one dispatch-c in config)]
      [(#\&)
       (read-box read-one dispatch-c in config)]
      [(#\')
       (read-quote read-one 'syntax "quoting #'" c in config)]
      [(#\`)
       (read-quote read-one 'quasisyntax "quasiquoting #`" c in config)]
      [(#\,)
       (define c2 (peek-char/special in config))
       (if (eqv? c2 #\@)
           (begin
             (consume-char in c2)
             (read-quote read-one 'unsyntax-splicing "unquoting #,@" c in config))
           (read-quote read-one 'unsyntax "unquoting #," c in config))]
      [(#\\)
       (read-character in config)]
      [(#\")
       (read-string in config #:mode '|byte string|)]
      [(#\<)
       (define c2 (peek-char/special in config))
       (cond
        [(eqv? #\< c2)
         (consume-char in #\<)
         (read-here-string in config)]
        [else
         (reader-error in config #:due-to c2 "bad syntax `~a<`" dispatch-c)])]
      [(#\%)
       (read-symbol-or-number c in config #:extra-prefix dispatch-c #:mode 'symbol)]
      [(#\:)
       (read-symbol-or-number #f in config #:mode 'keyword)]
      [(#\t #\T)
       (define c2 (peek-char/special in config))
       (cond
        [(char-delimiter? c2 config) (wrap #t in config c)]
        [else (read-delimited-constant c (char=? c #\t) '(#\r #\u #\e) #t in config)])]
      [(#\f #\F)
       (define c2 (peek-char/special in config))
       (cond
        [(char-delimiter? c2 config) (wrap #f in config c)]
        [(or (char=? c2 #\x) (char=? c2 #\l))
         (read-fixnum-or-flonum-vector read-one dispatch-c c c2 in config)]
        [else (read-delimited-constant c (char=? c #\f) '(#\a #\l #\s #\e) #f in config)])]
      [(#\e) (read-symbol-or-number #f in config #:mode "#e")]
      [(#\E) (read-symbol-or-number #f in config #:mode "#E")]
      [(#\i) (read-symbol-or-number #f in config #:mode "#i")]
      [(#\I) (read-symbol-or-number #f in config #:mode "#I")]
      [(#\d) (read-symbol-or-number #f in config #:mode "#d")]
      [(#\B) (read-symbol-or-number #f in config #:mode "#B")]
      [(#\o) (read-symbol-or-number #f in config #:mode "#o")]
      [(#\O) (read-symbol-or-number #f in config #:mode "#O")]
      [(#\D) (read-symbol-or-number #f in config #:mode "#D")]
      [(#\b) (read-symbol-or-number #f in config #:mode "#b")]
      [(#\x) (read-symbol-or-number #f in config #:mode "#x")]
      [(#\X) (read-symbol-or-number #f in config #:mode "#X")]
      [(#\c #\C)
       (define c2 (read-char/special in config))
       (case c2
         [(#\s #\S) (read-one #f in (override-parameter read-case-sensitive config #t))]
         [(#\i #\I) (read-one #f in (override-parameter read-case-sensitive config #f))]
         [else
          (reader-error in config #:due-to c2
                        "expected `s', `S`, `i`, or `I` after `~a~a`"
                        dispatch-c c)])]
      [(#\h #\H) (read-hash read-one dispatch-c c in config)]
      [(#\r)
       ;; Maybe regexp or `#reader`
       (define accum-str (accum-string-init! config))
       (accum-string-add! accum-str dispatch-c)
       (accum-string-add! accum-str c)
       (define c2 (read-char/special in config))
       (when (char? c2) (accum-string-add! accum-str c2))
       (case c2
         [(#\x) (read-regexp c accum-str in config)]
         [(#\e) (read-extension-reader read-one read-undotted dispatch-c in config)]
         [else
          (bad-syntax-error in config
                            #:due-to c2
                            (accum-string-get! accum-str config))])]
      [(#\p)
       ;; Maybe pregexp
       (define accum-str (accum-string-init! config))
       (accum-string-add! accum-str dispatch-c)
       (accum-string-add! accum-str c)
       (define c2 (read-char/special in config))
       (when (char? c2) (accum-string-add! accum-str c2))
       (case c2
         [(#\x) (read-regexp c accum-str in config)]
         [else (bad-syntax-error in config #:due-to c2
                                 (accum-string-get! accum-str config))])]
      [(#\l)
       ;; Maybe `#lang`
       (read-extension-lang read-undotted dispatch-c in config)]
      [(#\!)
       ;; Maybe `#lang`
       (read-extension-#! read-undotted dispatch-c in config)]
      [(#\~)
       ;; Compiled code
       (cond
        [(check-parameter read-accept-compiled config)
         (wrap ((read-config-read-compiled config) in) in config c)]
        [else
         (reader-error in config
                       "`~a~~` compiled expressions not enabled"
                       dispatch-c)])]
      [else
       (reader-error in config "bad syntax `~a~a`" dispatch-c c)])]))

(define (retry-special-comment v in config)
  (cond
   [(special-comment? v)
    (if (read-config-keep-comment? config)
        v
        (read-undotted #f in config))]
   [else v]))
