(module reader mzscheme
  (require (lib "string.ss") (lib "readerr.ss" "syntax"))

  (define cmd-char #\@)

  (define open-attrs    #rx#"^[ \t\r\n]*[[][ \t\r\n]*")
  (define open-lines    #rx#"^[ \t\r\n]*[{](?:[ \t]*\r?\n[ \t]*)?") ; 1 newline
  (define open-lines*   '(#"^[ \t\r\n]*" #"(?:[ \t]*\r?\n[ \t]*)?"))
  (define open-lines-special ; a special ending expected: @foo{<{ ... }>} etc
    #rx#"^[ \t\r\n]*([{][^a-zA-Z0-9 \t\r\n@$\\]*[{])(?:[ \t]*\r?\n[ \t]*)?")
  (define open-attr/lines #rx#"^[ \t\r\n]*[[{][ \t\r\n]*")
  (define close-attrs   #rx#"^[ \t\r\n]*[]]")
  (define close-lines   #rx#"^(?:[ \t]*\r?\n[ \t]*)?[}]") ; swallow 1 newline
  (define close-lines*  '(#"^(?:[ \t]*\r?\n[ \t]*)?" #""))
  (define comment-start #rx#"^[ \t]*;")
  (define comment-line  #rx#"^[^\r\n]*\r?\n[ \t]*") ; like tex's `%' nl & space
  (define attr-sep      #rx#"^[ \t\r\n]*=[ \t\r\n]*")
  (define scheme-start  #rx#"^[$]")
  (define scheme-start* #rx#"^[$][ \t\r\n]*{")
  (define scheme-end*   #rx#"^[ \t\r\n]*}")
  (define sub-start     #rx#"^[@]")
  (define line-item     #rx#"^(?:[^{}@$\r\n]*[^\\{}@$\r\n]|[\\]+[{}@$])+")
  (define line-item* '(#"^(?:[^{}@$\r\n]*[^\\{}@$\r\n]|[\\]+(?:[@$]|" #"))+"))
  (define end-of-line   #rx#"^([\\]+)?\r?\n[ \t]*") ; make \-eoln possible
  (define bar-pfx-remove  #rx#"^[|]")
  (define bslash-unquote  #rx#"[\\]([\\]*[{}@$])")
  (define bslash-unquote* '(#"[\\]([\\]+(?:[@$]|" #"))"))

  (define byte-pairs
    (map (lambda (b) (cons (bytes-ref b 0) (bytes-ref b 1)))
         '(#"()" #"[]" #"{}" #"<>")))

  (define attr-readtable
    (make-readtable #f #\= 'terminating-macro
      (lambda (char inp source-name line-num col-num position)
        (datum->syntax-object
         #f (string->symbol (string char))
         (list source-name line-num col-num position 1)))))

  (define (dispatcher char inp source-name line-num col-num position)
    (define (next-syntax . plain?)
      (let ([x ((if (and (pair? plain?) (car plain?))
                  read-syntax read-syntax/recursive)
                source-name inp)])
        (if (special-comment? x) (apply next-syntax plain?) x)))
    (define (cur-pos)
      (let-values ([(line col pos) (port-next-location inp)])
        pos))
    (define (span-from start)
      (and start (- (cur-pos) start)))
    (define (read-error msg . xs)
      (let-values ([(line col pos) (port-next-location inp)])
        (raise-read-error (apply format msg xs) source-name line col pos #f)))
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
          (let* ([fst (next-syntax #t)]
                 [snd (and (symbol? (syntax-e fst))
                           (regexp-match/fail-without-reading attr-sep inp)
                           (next-syntax))])
            (if snd
              (list (string->keyword (symbol->string (syntax-e fst))) snd)
              (list fst)))))
    (define (get-attrs)
      (and (regexp-match/fail-without-reading open-attrs inp)
           (parameterize ([current-readtable attr-readtable])
             (let loop ([attrs '()])
               (let ([a (get-attr)])
                 (if a
                   (loop (append! (reverse! a) attrs))
                   (reverse! attrs)))))))
    (define ((get-line open open-re close close-re item-re unquote-re level))
      (let-values ([(line col pos) (port-next-location inp)])
        (define (make-stx sexpr)
          (datum->syntax-object #f
            (if (bytes? sexpr) (bytes->string/utf-8 sexpr) sexpr)
            (list source-name line col pos (span-from pos))))
        (cond [(regexp-match/fail-without-reading close-re inp)
               ;; #f
               => (lambda (m)
                    (let ([l (sub1 (unbox level))])
                      (set-box! level l)
                      (and (<= 0 l) (make-stx (car m)))))]
              ;; [(regexp-match-peek-positions open-re inp)
              ;;  (read-error "unexpected `~a'" open)]
              [(regexp-match/fail-without-reading open-re inp)
               => (lambda (m)
                    (set-box! level (add1 (unbox level)))
                    (make-stx (car m)))]
              [(regexp-match/fail-without-reading scheme-start* inp)
               (let ([s (next-syntax)])
                 (if (regexp-match/fail-without-reading scheme-end* inp)
                   s (read-error "expected `}'")))]
              [(regexp-match/fail-without-reading scheme-start inp)
               (next-syntax)] ; read a real expression here
              [(regexp-match-peek-positions sub-start inp)
               (read-syntax/recursive source-name inp)] ; include comment objs
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
               (read-error "missing `~a'" close)]
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
    (define (get-lines)
      (define get
        (cond [(regexp-match/fail-without-reading open-lines-special inp)
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
                      (get-line open (bre open-lines* open-re)
                                close (bre close-lines* close-re)
                                (bre line-item* either-re)
                                (bre bslash-unquote* either-re)
                                (box 0))))]
              [(regexp-match/fail-without-reading open-lines inp)
               (get-line "{" open-lines "}" close-lines
                         line-item bslash-unquote (box 0))]
              [else #f]))
      (and get (let loop ([lines '()] [more '()])
                 (let-values ([(line more) (if (pair? more)
                                             (values (car more) (cdr more))
                                             (values (get) more))])
                   (cond [(not line) (reverse! lines)]
                         ;; can happen from a sub @;{...} comment
                         [(special-comment? line) (loop lines more)]
                         [(list? line) (loop lines (append line more))]
                         [else (loop (maybe-merge line lines) more)])))))
    (cond
     [(regexp-match/fail-without-reading comment-start inp)
      (if (regexp-match-peek-positions open-lines inp)
        (get-lines) (regexp-match comment-line inp))
      (make-special-comment #f)]
     [else
      (let* ([pfx (regexp-match/fail-without-reading
                   #rx#"^(?:[ \t\r\n]*(?:'|`|,@?))+" inp)]
             [pfx
              (if pfx
                ;; accumulate prefixes in reverse
                (let loop ([s (car pfx)] [r '()])
                  (cond
                   [(equal? #"" s) r]
                   [(regexp-match #rx#"^[ \t\r\n]*('|`|,@?)(.*)$" s)
                    => (lambda (m)
                         (loop
                          (caddr m)
                          (cons (let ([m (cadr m)])
                                  (cond [(equal? m #"'") 'quote]
                                        [(equal? m #"`") 'quasiquote]
                                        [(equal? m #",") 'unquote]
                                        [(equal? m #",@") 'unquote-splicing]
                                        [else (error "something bad")]))
                                r)))]
                   [else (error "something bad happened")]))
                '())]
             [cmd   (if (regexp-match-peek-positions open-attr/lines inp)
                      #f
                      (next-syntax))] ; never #f
             [attrs (get-attrs)]
             [lines (get-lines)]
             [stx   (append (or attrs '()) (or lines '()))]
             [stx   (if cmd (cons cmd stx) stx)]
             [stx   (let loop ([pfx pfx] [stx stx])
                      (if (null? pfx) stx
                          (loop (cdr pfx) (list (car pfx) stx))))])
        (datum->syntax-object #f stx
          (list source-name line-num col-num position (span-from position))))]))

  (define readtable
    (make-readtable #f cmd-char 'terminating-macro dispatcher))

  (provide use-at-readtable)
  (define (use-at-readtable) (current-readtable readtable))

  (define (*read inp)
    (parameterize ([current-readtable readtable])
      (read inp)))

  (define (*read-syntax src port)
    (parameterize ([current-readtable readtable])
      (read-syntax src port)))

  (provide (rename *read read) (rename *read-syntax read-syntax))

  )
