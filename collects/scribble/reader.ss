;; Implements the @-reader macro for embedding text in Scheme code.
(module reader mzscheme
  (require (lib "string.ss") (lib "kw.ss") (lib "readerr.ss" "syntax"))

  (define cmd-char #\@)

  (define bars-quoted   #rx#"^[ \t\r\n]*\\|([^|]*)\\|")
  (define open-attrs    #rx#"^[ \t\r\n]*[[][ \t\r\n]*")
  (define open-lines    #rx#"^[ \t\r\n]*[{](?:[ \t]*\r?\n[ \t]*)?") ; 1 newline
  (define open-lines*   '(#"^[ \t\r\n]*" #"(?:[ \t]*\r?\n[ \t]*)?"))
  (define open-lines-special ; a special ending expected: @foo{<{ ... }>} etc
    #rx#"^[ \t\r\n]*([|][^a-zA-Z0-9 \t\r\n@\\]*?[{])(?:[ \t]*\r?\n[ \t]*)?")
  (define open-attr/lines #rx#"^[ \t\r\n]*[[{][ \t\r\n]*")
  (define close-attrs   #rx#"^[ \t\r\n]*[]]")
  (define close-lines   #rx#"^(?:[ \t]*\r?\n[ \t]*)?[}]") ; swallow 1 newline
  (define close-lines*  '(#"^(?:[ \t]*\r?\n[ \t]*)?" #""))
  (define comment-start #rx#"^[ \t]*;")
  (define comment-line  #rx#"^[^\r\n]*\r?\n[ \t]*") ; like tex's `%' nl & space
  (define attr-sep      #rx#"^[ \t\r\n]*=[ \t\r\n]*")
  (define sub-start     #rx#"^[@]")
  (define line-item     #rx#"^(?:[^{}@\r\n]*[^\\{}@\r\n]|[\\]+[{}@])+")
  (define line-item* '(#"^(?:[^{}@\r\n]*[^\\{}@\r\n]|[\\]+(?:[@]|" #"))+"))
  (define end-of-line   #rx#"^([\\]+)?\r?\n[ \t]*") ; make \-eoln possible
  (define bar-pfx-remove  #rx#"^[|]")
  (define bslash-unquote  #rx#"[\\]([\\]*[{}@])")
  (define bslash-unquote* '(#"[\\]([\\]+(?:[@]|" #"))"))

  (define byte-pairs
    (map (lambda (b) (cons (bytes-ref b 0) (bytes-ref b 1)))
         '(#"()" #"[]" #"{}" #"<>")))

  (define make-spaces
    (let ([t (make-hash-table)])
      (lambda (n)
        (hash-table-get t n
          (lambda ()
            (let ([s (make-string n #\space)])
              (hash-table-put! t n s) s))))))

  (define ((dispatcher start-inside?) char inp source-name line-num col-num position)
    (define/kw (next-syntax readtable #:optional plain?)
      (let ([read (if plain? read-syntax read-syntax/recursive)])
        (parameterize ([current-readtable readtable])
          (let loop ()
            (let ([x (read source-name inp)])
              (if (special-comment? x) (loop) x))))))
    (define (cur-pos)
      (let-values ([(line col pos) (port-next-location inp)])
        pos))
    (define (span-from start)
      (and start (- (cur-pos) start)))
    (define (read-error msg . xs)
      (let-values ([(line col pos) (port-next-location inp)])
        (raise-read-error (apply format msg xs) source-name line col pos #f)))
    (define (read-from-bytes-exact-or-identifier bs)
      (let ([inp (open-input-bytes bs)]
            [default (lambda _ (string->symbol (bytes->string/utf-8 bs)))])
        (with-handlers ([void default])
          (let ([x (read inp)])
            ;; must match all -- otherwise: default
            (if (regexp-match #rx#"^[ \t\r\n]*$" inp) x (default))))))
    (define (reverse-bytes bytes)
      (define (rev-byte b)
        (cond [(assq b byte-pairs) => cdr]
              [else b]))
      (let* ([len (bytes-length bytes)] [r (make-bytes len)])
        (let loop ([i (sub1 len)])
          (when (<= 0 i)
            (bytes-set! r i (rev-byte (bytes-ref bytes (- len i 1))))
            (loop (sub1 i))))
        r))
    (define eol-token "\n")
    (define (get-attr)
      (if (regexp-match/fail-without-reading close-attrs inp) #f
          (let* ([fst (next-syntax
                       ;; hack: if we see an open paren or other nested
                       ;; constructs, use the usual readtable so a nested `='
                       ;; behaves correctly
                       (if (regexp-match-peek-positions
                            #rx#"^[ \t\r\n]*['`,]*[[({@]" inp)
                         at-readtable attr-readtable)
                       #t)]
                 [snd (and (symbol? (syntax-e fst))
                           (regexp-match/fail-without-reading attr-sep inp)
                           (next-syntax at-readtable))])
            (if snd
              (list (string->keyword (symbol->string (syntax-e fst))) snd)
              (list fst)))))
    (define (get-attrs)
      (and (regexp-match/fail-without-reading open-attrs inp)
           (let loop ([attrs '()])
             (let ([a (get-attr)])
               (if a (loop (append! (reverse! a) attrs)) (reverse! attrs))))))
    (define ((get-line eof-ok? open open-re close close-re item-re unquote-re level))
      (let-values ([(line col pos) (port-next-location inp)])
        (define (make-stx sexpr)
          (datum->syntax-object #f
            (if (bytes? sexpr) (bytes->string/utf-8 sexpr) sexpr)
            (list source-name line col pos (span-from pos))))
        (cond [(regexp-match/fail-without-reading close-re inp)
               => (lambda (m)
                    (let ([l (sub1 (unbox level))])
                      (set-box! level l)
                      (and (<= 0 l) (make-stx (car m)))))]
              [(regexp-match/fail-without-reading open-re inp)
               => (lambda (m)
                    (set-box! level (add1 (unbox level)))
                    (make-stx (car m)))]
              [(regexp-match-peek-positions sub-start inp)
               (read-syntax source-name inp)] ; include comment objs
              [(regexp-match/fail-without-reading end-of-line inp)
               => (lambda (m)
                    (if (cadr m) ; backslashes?
                      (list (make-stx (cadr m)) (make-stx eol-token))
                      (make-stx eol-token)))]
              [(regexp-match/fail-without-reading item-re inp)
               => (lambda (m)
                    (let* ([m (car m)]
                           [m (regexp-replace bar-pfx-remove m #"")]
                           [m (regexp-replace* unquote-re m #"\\1")])
                      (make-stx m)))]
              [(and (not (eq? item-re line-item))
                    (regexp-match/fail-without-reading #rx#"[{}]" inp))
               => (lambda (m)
                    (make-stx (car m)))]
              [(regexp-match/fail-without-reading #rx#"^$" inp)
               (if eof-ok?
                   #f
                   (read-error "missing `~a'" close))]
              [else (read-error "internal error")])))
    ;; adds stx (new syntax) to the list of stxs, merging it if both are
    ;; strings, except for newline markers
    (define (maybe-merge stx stxs)
      (if (and (pair? stxs) (syntax? stx) (syntax? (car stxs))
               (string? (syntax-e stx))
               (string? (syntax-e (car stxs)))
               (not (eq? eol-token (syntax-e stx)))
               (not (eq? eol-token (syntax-e (car stxs)))))
        (let ([fst (car stxs)])
          (cons (datum->syntax-object stx
                  (string-append (syntax-e fst) (syntax-e stx))
                  (list (syntax-source fst)
                        (syntax-line   fst)
                        (syntax-column fst)
                        (syntax-position fst)
                        (span-from (syntax-position fst))))
                (cdr stxs)))
        (cons stx stxs)))
    (define (add-indents stxs)
      (if (or (null? stxs)
              (not (andmap (lambda (s) (and (syntax-line s) (syntax-column s)))
                           stxs)))
        stxs
        (let ([mincol (apply min (map syntax-column stxs))])
          (let loop ([curline line-num] [stxs stxs] [r '()])
            (if (null? stxs)
              (reverse! r)
              (let* ([stx (car stxs)] [line (syntax-line stx)])
                (loop line (cdr stxs)
                      (if (and (< curline line) (< mincol (syntax-column stx)))
                        (list* stx
                               (datum->syntax-object stx
                                 (make-spaces (- (syntax-column stx) mincol))
                                 stx)
                               r)
                        (cons stx r)))))))))
    (define (get-lines inside?)
      (define get
        (cond [inside?
               (get-line #t "{" open-lines "}" close-lines
                         line-item bslash-unquote (box 0))]
              [(regexp-match/fail-without-reading open-lines-special inp)
               => (lambda (m)
                    (let* ([open     (cadr m)]
                           [close    (reverse-bytes open)]
                           [open-re  (regexp-quote open)]
                           [close-re (regexp-quote close)]
                           [either-re (bytes-append open-re #"|" close-re)]
                           [bre (lambda (pfx/sfx re)
                                  (byte-regexp
                                   (bytes-append (car pfx/sfx)
                                                 re
                                                 (cadr pfx/sfx))))])
                      (get-line #f open (bre open-lines* open-re)
                                close (bre close-lines* close-re)
                                (bre line-item* either-re)
                                (bre bslash-unquote* either-re)
                                (box 0))))]
              [(regexp-match/fail-without-reading open-lines inp)
               (get-line #f "{" open-lines "}" close-lines
                         line-item bslash-unquote (box 0))]
              [else #f]))
      (and get (let loop ([lines '()] [more '()])
                 (let-values ([(line more) (if (pair? more)
                                             (values (car more) (cdr more))
                                             (values (get) more))])
                   (cond [(not line) (add-indents (reverse! lines))]
                         ;; can happen from a sub @;{...} comment
                         [(special-comment? line) (loop lines more)]
                         [(list? line) (loop lines (append line more))]
                         [else (loop (maybe-merge line lines) more)])))))
    (define (get-rprefixes) ; return punctuation prefixes in reverse
      (cond
       [(regexp-match/fail-without-reading
         #rx#"^(?:[ \t\r\n]*(?:'|`|,@?))+" inp)
        => (lambda (m)
             ;; accumulate prefixes in reverse
             (let loop ([s (car m)] [r '()])
               (cond
                [(equal? #"" s) r]
                [(regexp-match #rx#"^[ \t\r\n]*('|`|,@?)(.*)$" s)
                 => (lambda (m)
                      (loop (caddr m)
                            (cons (let ([m (cadr m)])
                                    (cadr (or (assoc
                                               m '([#"'"  quote]
                                                   [#"`"  quasiquote]
                                                   [#","  unquote]
                                                   [#",@" unquote-splicing]))
                                              (error "something bad"))))
                                  r)))]
                [else (error "something bad happened")])))]
       [else '()]))
    (define (get-command) ; #f means no command
      (let-values ([(line col pos) (port-next-location inp)])
        (cond [(regexp-match-peek-positions open-attr/lines inp)
               (values #f #f)]
              [(regexp-match/fail-without-reading bars-quoted inp)
               => (lambda (m)
                    (values (datum->syntax-object #f
                              (read-from-bytes-exact-or-identifier (cadr m))
                              (list source-name line col pos (span-from pos)))
                            #t))]
              [else (values (next-syntax cmd-readtable) #f)])))
    (cond
     [start-inside?
      (datum->syntax-object #f 
                            (get-lines #t)
                            (list source-name line-num col-num position (span-from position)))]
     [(regexp-match/fail-without-reading comment-start inp)
      (if (regexp-match-peek-positions open-lines inp)
        (get-lines #f) (regexp-match comment-line inp))
      (make-special-comment #f)]
     [else
      (let* ([pfx   (get-rprefixes)]
             [bars? #f]
             [cmd   (let-values ([(cmd bs?) (get-command)])
                      (set! bars? bs?) cmd)] ; #f means no command
             [attrs (and (not bars?) (get-attrs))]
             [lines (and (not bars?) (get-lines #f))]
             [stx   (and (or attrs lines)
                         (append (or attrs '()) (or lines '())))]
             [stx   (or (and cmd stx (cons cmd stx)) ; all parts
                        stx  ; no cmd part => just a parenthesized expression
                        cmd  ; no attrs/lines => simple expression (no parens)
                        ;; impossible: either we saw []s or {}s, or we read a
                        ;; scheme expression
                        (error "something bad happened"))]
             [stx   (let loop ([pfx pfx] [stx stx])
                      (if (null? pfx) stx
                          (loop (cdr pfx) (list (car pfx) stx))))])
        (datum->syntax-object #f stx
          (list source-name line-num col-num position (span-from position))))]))

  (define at-readtable
    (make-readtable #f cmd-char 'terminating-macro (dispatcher #f)))

  ;; similar to plain Scheme, but with `|' as a terminating macro
  (define cmd-readtable
    (make-readtable at-readtable #\| 'terminating-macro
      (lambda (char inp source-name line-num col-num position)
        (let ([m (regexp-match #rx#"^([^|]*)\\|" inp)])
          (unless m
            (raise-read-error
             "unbalanced `|'" source-name line-num col-num position #f))
          (datum->syntax-object
            #f (string->symbol (bytes->string/utf-8 (cadr m)))
            (list source-name line-num col-num position
                  (add1 (bytes-length (car m)))))))))

  ;; similar to plain Scheme, but with `=' always parsed as a separate symbol
  (define attr-readtable
    (make-readtable at-readtable #\= 'terminating-macro
      (lambda (char inp source-name line-num col-num position)
        (datum->syntax-object
         #f (string->symbol (string char))
         (list source-name line-num col-num position 1)))))

  (provide use-at-readtable)
  (define (use-at-readtable)
    (port-count-lines! (current-input-port))
    (current-readtable at-readtable))

  (define default-src (gensym))
  (define (src-name src port)
    (if (eq? src default-src)
        (object-name port)
        src))

  (define/kw (*read #:optional [inp (current-input-port)])
    (parameterize ([current-readtable at-readtable])
      (read inp)))

  (define/kw (*read-syntax #:optional [src default-src] [port (current-input-port)])
    (parameterize ([current-readtable at-readtable])
      (read-syntax (src-name src port) port)))

  (define/kw (read-inside #:optional [inp (current-input-port)])
    (let-values ([(line col pos) (port-next-location inp)])
      (parameterize ([current-readtable at-readtable])
        (syntax-object->datum
         ((dispatcher #t) #f inp (object-name inp) line col pos)))))

  (define/kw (read-inside-syntax #:optional [src default-src] [port (current-input-port)])
    (let-values ([(line col pos) (port-next-location port)])
      (parameterize ([current-readtable at-readtable])
        ((dispatcher #t) #f port (src-name src port) line col pos))))

  (provide (rename *read read) (rename *read-syntax read-syntax)
           read-inside read-inside-syntax)

  )
