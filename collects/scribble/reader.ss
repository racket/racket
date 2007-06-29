;; ============================================================================
;; Implements the @-reader macro for embedding text in Scheme code.

(module reader mzscheme

  (require (lib "kw.ss") (lib "string.ss") (lib "readerr.ss" "syntax"))

  ;; --------------------------------------------------------------------------
  ;; utilities for syntax specifications below

  ;; regexps
  (define (px . args)
    (let* ([args (let loop ([xs args])
                   (if (list? xs) (apply append (map loop xs)) (list xs)))]
           [args (map (lambda (x)
                        (cond
                          [(bytes? x) x]
                          [(string? x) (string->bytes/utf-8 x)]
                          [(char? x) (regexp-quote (bytes (char->integer x)))]
                          [else (error 'reader "internal error [px]")]))
                      args)])
      (byte-pregexp (apply bytes-append args))))
  (define (^px . args) (px #"^" args))

  ;; reverses a byte string visually
  (define reverse-bytes
    (let ([pairs (let ([xs (bytes->list #"([{<")]
                       [ys (bytes->list #")]}>")])
                   (append (map cons xs ys) (map cons ys xs)))])
      (define (rev-byte b)
        (cond [(assq b pairs) => cdr]
              [else b]))
      (lambda (bs) (list->bytes (map rev-byte (reverse! (bytes->list bs)))))))

  ;; --------------------------------------------------------------------------
  ;; syntax

  ;; basic customization
  (define ch:command     #\@)
  (define ch:comment     #\;)
  (define ch:bar-quote   #\|)
  (define ch:command-quote #\\)
  (define ch:attrs-begin #\[)
  (define ch:attrs-end   #\])
  (define ch:lines-begin #\{)
  (define ch:lines-end   #\})

  (define str:lines-begin* #"\\|[^a-zA-Z0-9 \t\r\n\f@\\\177-\377{]*\\{")

  (define re:command       (^px ch:command
                                ;; the following identifies string escapes, see
                                ;; hoe it is used below
                                "("ch:bar-quote"?\")?"))
  (define re:whitespaces   (^px "\\s+"))
  (define re:comment-start (^px ch:comment))
  (define re:comment-line  (^px "[^\n]*\n[ \t]*")) ; like tex's `%'
  (define re:expr-escape   (^px ch:bar-quote))
  (define re:attrs-begin   (^px ch:attrs-begin))
  (define re:attrs-end     (^px ch:attrs-end))
  (define re:lines-begin   (^px ch:lines-begin))
  (define re:lines-begin*  (^px str:lines-begin*))
  (define re:lines-end     (^px ch:lines-end))
  (define str:end-of-line  "[ \t]*\r?\n[ \t]*") ; eat spaces on the next line
  (define re:end-of-line   (^px str:end-of-line))
  (define (re:line-item* bgn end)
    (^px "(.+?)(?:"bgn"|"end
                "|"ch:command-quote"*"ch:command
                "|"str:end-of-line")"))
  (define re:line-item (re:line-item* ch:lines-begin ch:lines-end))
  (define re:line-item-no-nests (^px "(.+?)(?:"ch:command-quote"*"ch:command
                                            "|"str:end-of-line")"))
  (define re:command-unquote (^px ch:command-quote
                                  "("ch:command-quote"*"ch:command")"))

  ;; --------------------------------------------------------------------------
  ;; utilities

  ;; like `regexp-match/fail-without-reading', without extras; the regexp that
  ;; is used must be anchored -- nothing is dropped
  (define (*regexp-match-peek-positions pattern input-port)
    (unless (and (byte-regexp? pattern)
                 (regexp-match? #rx#"^\\^" (object-name pattern)))
      (error 'reader "internal error [invalid bregexp] ~e" pattern))
    (regexp-match-peek-positions pattern input-port))
  ;; the following doesn't work -- must peek first
  ;; (define (*regexp-match-positions pattern input-port)
  ;;   (unless (and (byte-regexp? pattern)
  ;;                (regexp-match? #rx#"^\\^" (object-name pattern)))
  ;;     (error 'reader "internal error [invalid bregexp] ~e" pattern))
  ;;   (regexp-match-peek-positions pattern input-port))
  (define (*regexp-match pattern input-port)
    (let ([m (*regexp-match-peek-positions pattern input-port)])
      (and m (let ([s (read-bytes (cdar m) input-port)])
               (cons s (map (lambda (p) (and p (subbytes s (car p) (cdr p))))
                            (cdr m)))))))
  ;; like regexp-match, but returns the whole match
  (define (*regexp-match1 pattern input-port)
    (let ([m (*regexp-match-peek-positions pattern input-port)])
      (and m (read-bytes (cdar m) input-port))))

  ;; Skips whitespace characters, sensitive to the current readtable's
  ;; definition of whitespace; optimizes common spaces when possible
  (define skip-whitespace
    (let* ([plain-readtables  (make-hash-table 'weak)]
           [plain-spaces      " \t\n\r\f"]
           [plain-spaces-list (string->list " \t\n\r\f")]
           [plain-spaces-re   (^px "[" plain-spaces "]*")])
      (define (skip-plain-spaces port)
        ;; hack: according to the specs, this might consume more characters
        ;; than needed, but it works fine with a simple <ch>* regexp (because
        ;; it can always match an empty string)
        (*regexp-match-peek-positions plain-spaces-re port))
      (define (whitespace? ch rt)
        (if rt
          (let-values ([(like-ch/sym _1 _2) (readtable-mapping rt ch)])
            ;; if like-ch/sym is whitespace, then ch is whitespace
            (and (char? like-ch/sym) (char-whitespace? like-ch/sym)))
          ;; `char-whitespace?' is fine for the default readtable
          (char-whitespace? ch)))
      (define (plain-readtable? rt)
        (hash-table-get plain-readtables rt
          (lambda ()
            (let ([plain? (andmap (lambda (ch) (whitespace? ch rt))
                                  plain-spaces-list)])
              (hash-table-put! plain-readtables rt #t)
              rt))))
      (lambda (port)
        (let* ([rt (current-readtable)] [plain? (plain-readtable? rt)])
          (let loop ()
            (when plain? (skip-plain-spaces port))
            (let ([ch (peek-char port)])
              (unless (eof-object? ch)
                (when (whitespace? ch rt) (read-char port) (loop)))))))))

  ;; Wrappers for placeholders, to keep source information for them.  (MzScheme
  ;; provides nothing for them -- there's not even a predicate.  Hopefully, if
  ;; something is added it will use the same name, so there's a compiler error
  ;; here and this code is adapted.)
  (define-struct placeholder (p loc))
  (define (syntax/placeholder-line sp)
    (if (placeholder? sp) (cadr (placeholder-loc sp)) (syntax-line sp)))
  (define (syntax/placeholder-column sp)
    (if (placeholder? sp) (caddr (placeholder-loc sp)) (syntax-column sp)))
  (define (syntax/placeholder-strip sp)
    (if (placeholder? sp) (placeholder-p sp) sp))
  (define (datum->syntax-object/placeholder sp d)
    (if (placeholder? sp)
      ;; using the syntax for lexical context is not possible for placeholders,
      ;; but we don't need it since we're a reader
      (datum->syntax-object #f d (placeholder-loc sp))
      (datum->syntax-object sp d sp)))

  ;; make n spaces, cached for n
  (define make-spaces
    (let ([t (make-hash-table)])
      (lambda (n)
        (hash-table-get t n
          (lambda ()
            (let ([s (make-string n #\space)])
              (hash-table-put! t n s) s))))))

  ;; a unique eol string
  (define eol-token "\n")
  (define (eol-syntax? x) (and (syntax? x) (eq? eol-token (syntax-e x))))
  ;; sanity check, in case this property gets violated in the future
  (unless (eol-syntax? (datum->syntax-object #f eol-token))
    (error 'reader "internal error [invalid assumption]"))

  ;; --------------------------------------------------------------------------
  ;; main reader function for @ constructs

  (define ((dispatcher start-inside?)
           char inp source-name line-num col-num position)

    (define (read-error* line col pos span msg . xs)
      (let* ([eof? (and (eq? 'eof msg) (pair? xs))]
             [msg  (apply format (if eof? xs (cons msg xs)))]
             [msg  (if source-name
                     (format "~a (when reading ~a)" msg source-name)
                     msg)])
        ((if eof? raise-read-error raise-read-eof-error)
         msg source-name line col pos span)))
    (define (read-error msg . xs)
      (let-values ([(line col pos) (port-next-location inp)])
        (apply read-error* line col pos #f msg xs)))

    (define (*match  rx) (*regexp-match rx inp))
    (define (*match1 rx) (*regexp-match1 rx inp))
    ;; (define (*skip   rx) (*regexp-match-positions rx inp)) <- see above
    (define (*skip   rx) (*regexp-match1 rx inp))
    (define (*peek   rx) (*regexp-match-peek-positions rx inp))

    (define (cur-pos)
      (let-values ([(line col pos) (port-next-location inp)])
        pos))

    (define (span-from start)
      (and start (- (cur-pos) start)))

    (define (read-delimited-list end-re end-ch)
      (let loop ([r '()])
        (skip-whitespace inp)
        (if (*skip end-re)
          (reverse! r)
          (let ([x (read-syntax/recursive source-name inp)])
            (if (eof-object? x)
              (read-error 'eof "expected a '~a'" end-ch)
              (loop (if (special-comment? x) r (cons x r))))))))

    ;; adds indentation (as new syntaxes, not merged); if the first line was
    ;; not empty, then it is treated specially.  called with at least two items
    ;; (see below).
    (define (add-indents stxs 1st-eol?)
      (unless (andmap (lambda (x)
                        (and (or (syntax? x) (placeholder? x))
                             (syntax/placeholder-column x)
                             (syntax/placeholder-line x)))
                      stxs)
        ;; the reader always turns on line counting
        (read-error "internal error [add-indents] ~s" stxs))
      (let* ([mincol
              (let loop ([min #f] [stxs (if 1st-eol? stxs (cdr stxs))])
                (if (null? stxs)
                  (or min (error "internal error [add-indents]"))
                  (loop (if (eol-syntax? (car stxs))
                          min
                          (let ([c (syntax/placeholder-column (car stxs))])
                            (if (or (not min) (< c min)) c min)))
                        (cdr stxs))))]
             [mincol (if 1st-eol?
                       mincol
                       (min mincol (syntax/placeholder-column (car stxs))))])
        (let loop (;; no indentation for text on the first '{' line
                   [newline? 1st-eol?] [curline -1] [stxs stxs] [r '()])
          (if (null? stxs)
            (reverse! r)
            (let* ([stx  (car stxs)]
                   [line (syntax/placeholder-line stx)])
              (loop (eol-syntax? stx) line (cdr stxs)
                    (let ([stxcol (syntax/placeholder-column stx)]
                          [stx*   (syntax/placeholder-strip stx)])
                      (if (and newline? (< curline line) (< mincol stxcol))
                        (list* stx*
                               (datum->syntax-object/placeholder stx
                                 (make-spaces (- stxcol mincol)))
                               r)
                        (cons stx* r)))))))))

    ;; gets an accumulated (reversed) list of syntaxes, sorts things out
    ;; (remove prefix and suffix newlines, adds indentation if needed)
    (define (done-lines rlines)
      (cond
        [(andmap eol-syntax? rlines)
         ;; nothing to do (includes null, so the code below can assume a pair)
         (reverse! rlines)]
        [start-inside?
         ;; no newlines removed
         (add-indents (reverse! rlines) #t)] ; don't ignore the 1st line
        [else
         ;; strip off leading and trailing newlines
         (let* ([rlines   (if (eol-syntax? (car rlines)) (cdr rlines) rlines)]
                [lines    (reverse! rlines)]
                [1st-eol? (eol-syntax? (car lines))]
                [lines    (if 1st-eol? (cdr lines) lines)])
           (if (null? (cdr lines)) ; common case: one string
             (list (syntax/placeholder-strip (car lines)))
             (add-indents lines 1st-eol?)))]))

    ;; cons stx (new syntax) to the list of stxs, merging it if both are
    ;; strings, except for newline markers
    (define (maybe-merge stx stxs)
      (let* ([2nd  (and (syntax? stx) (syntax-e stx))]
             [stx0 (and (pair? stxs) (car stxs))]
             [1st  (and (syntax? stx0) (syntax-e stx0))])
        (if (and (string? 1st) (not (eq? eol-token 1st))
                 (string? 2nd) (not (eq? eol-token 2nd)))
          (cons (datum->syntax-object stx0
                  (string-append 1st 2nd)
                  (list (syntax-source stx0)
                        (syntax-line   stx0)
                        (syntax-column stx0)
                        (syntax-position stx0)
                        ;; this is called right after reading stx
                        (span-from (syntax-position stx0))))
                (cdr stxs))
          (cons stx stxs))))

    (define (get-lines* re:begin re:end re:item end-token)
      ;; re:begin, re:end, end-token can be false if start-inside? is #t
      (let loop ([lvl 0] [r '()])
        (let-values ([(line col pos) (port-next-location inp)])
          (define (make-stx sexpr)
            (datum->syntax-object #f
              (if (bytes? sexpr) (bytes->string/utf-8 sexpr) sexpr)
              (list source-name line col pos (span-from pos))))
          (cond [(and re:begin (*match1 re:begin))
                 => (lambda (m) (loop (add1 lvl) (maybe-merge (make-stx m) r)))]
                [(and re:end (*match1 re:end))
                 => (lambda (m)
                      (if (and (zero? lvl) (not start-inside?))
                        (done-lines r)
                        (loop (sub1 lvl) (maybe-merge (make-stx m) r))))]
                [(*skip re:end-of-line)
                 (loop lvl (cons (make-stx eol-token) r))] ; no merge needed
                [(*match re:command-unquote)
                 => (lambda (m)
                      (loop lvl (maybe-merge (make-stx (cadr m)) r)))]
                [(*peek re:command)
                 ;; read the next value, include comment objs, keep source
                 ;; location manually (see above)
                 => (lambda (m)
                      ;; if the command is a string escape, use `read-syntax',
                      ;; so that we don't get a placeholder, and we can merge
                      ;; the string to others
                      (let* ([reader (if (cadr m)
                                       read-syntax read-syntax/recursive)]
                             [x (reader source-name inp)])
                        (loop lvl
                              (cond [(special-comment? x) r]
                                    [(eof-object? x)
                                     (read-error 'eof "missing command")]
                                    [(syntax? x) (maybe-merge x r)]
                                    ;; otherwise it's a placeholder to wrap
                                    [else (cons (make-placeholder x ; no merge
                                                  (list source-name line col pos
                                                        (span-from pos)))
                                                r)]))))]
                ;; must be last, since it will always succeed with 1 char
                [(*peek re:item) ; don't read: regexp grabs the following text
                 => (lambda (m)
                      (loop lvl
                            (maybe-merge (make-stx (read-bytes (cdadr m) inp))
                                         r)))]
                [(*peek #rx#"^$")
                 (if end-token
                   (read-error 'eof "missing closing `~a'" end-token)
                   (done-lines r))]
                [else (read-error "internal error [get-lines*]")]))))

    (define (get-lines)
      (cond [(*skip re:lines-begin)
             (get-lines* re:lines-begin re:lines-end re:line-item ch:lines-end)]
            [(*match1 re:lines-begin*)
             => (lambda (bgn)
                  (let* ([end  (reverse-bytes bgn)]
                         [bgn* (regexp-quote bgn)]
                         [end* (regexp-quote end)])
                    (get-lines* (^px bgn*) (^px end*)
                                (re:line-item* bgn* end*)
                                end)))]
            [else #f]))

    (define (get-attrs)
      (and (*skip re:attrs-begin)
           (read-delimited-list re:attrs-end ch:attrs-end)))

    (define (get-escape-expr)
      (define-values (line col pos) (port-next-location inp))
      (and (*skip re:expr-escape)
           (begin
             (skip-whitespace inp)
             (begin0 (parameterize ([current-readtable command-readtable])
                       (let loop ()
                         (let ([expr
                                ;; should be `read-syntax/recursive', but see
                                ;; the next comment (this also means that we
                                ;; never get a special-comment)
                                (read-syntax source-name inp)])
                           (cond
                             [(special-comment? expr) (loop)]
                             [(eof-object? expr)
                              (read-error 'eof "missing escape expression")]
                             [else
                              ;; we need to use the proper source location,
                              ;; including the initial "@|" so if an escape is
                              ;; at the beginning of a line no bogus
                              ;; indentation is added later
                              (datum->syntax-object expr (syntax-e expr)
                                (list source-name line-num col-num position
                                      (span-from position)))]))))
               (skip-whitespace inp)
               (unless (*skip re:expr-escape)
                 (read-error* line col pos #f
                              "expecting a terminating '~a'" ch:bar-quote))))))

    ;; called only when we must see a command in the input
    (define (get-command)
      (define-values (line col pos) (port-next-location inp))
      (let ([cmd (parameterize ([current-readtable command-readtable])
                   (read-syntax/recursive source-name inp))])
        (cond [(special-comment? cmd)
               (read-error* line col pos (span-from pos)
                            "expecting a command expression, got a comment")]
              [(eof-object? cmd) (read-error 'eof "missing command")]
              [else cmd])))

    (define (get-rprefixes) ; return punctuation prefixes in reverse
      (let loop ([r '()])
        (let-values ([(line col pos) (port-next-location inp)])
          (cond
            [(*match1 #rx#"^(?:'|`|,@?)")
             => (lambda (m)
                  (let ([sym (cond
                               [(assoc m '([#"'"  quote]
                                           [#"`"  quasiquote]
                                           [#","  unquote]
                                           [#",@" unquote-splicing]))
                                => cadr]
                               [else (read-error "internal error [rpfxs]")])])
                    (loop (cons (datum->syntax-object #f sym
                                  (list source-name line col pos
                                        (span-from pos)))
                                r))))]
            [(*peek re:whitespaces)
             (read-error "unexpected whitespace after ~a" ch:command)]
            [else r]))))

    (cond
      [start-inside?
       (datum->syntax-object #f (get-lines* #f #f re:line-item-no-nests #f)
         (list source-name line-num col-num position (span-from position)))]
      [(*peek re:whitespaces)
       (read-error "unexpected whitespace after ~a" ch:command)]
      [(*skip re:comment-start)
       (unless (get-lines) (*skip re:comment-line))
       (make-special-comment #f)]
      [else
       (let*-values
           ([(rpfxs) (get-rprefixes)]
            [(cmd attrs lines)
             (cond
               ;; try get-lines first -- so @|{...}| is not used as a
               ;; simple expression escape, same for get-attrs
               [(get-lines) => (lambda (lines) (values #f #f lines))]
               [(get-attrs) => (lambda (attrs) (values #f attrs (get-lines)))]
               [(get-escape-expr) => (lambda (expr) (values expr #f #f))]
               [else (values (get-command) (get-attrs) (get-lines))])]
            [(stx) (and (or attrs lines)
                        (append (or attrs '()) (or lines '())))]
            [(stx) (or (and cmd stx (cons cmd stx)) ; all parts
                       stx  ; no cmd part => just a parenthesized expression
                       cmd  ; no attrs/lines => simple expression (no parens)
                       ;; impossible: either we saw []s or {}s, or we read a
                       ;; scheme expression
                       (read-error "internal error [dispatcher]"))]
            [(stx)
             ;; wrap the prefixes around the result
             (let loop ([rpfxs rpfxs] [stx stx])
               (if (null? rpfxs)
                 stx
                 (loop (cdr rpfxs) (list (car rpfxs) stx))))])
         (datum->syntax-object #f stx
           (list source-name line-num col-num position
                 (span-from position))))]))

  ;; --------------------------------------------------------------------------
  ;; readtables

  (define at-readtable
    (make-readtable #f ch:command 'terminating-macro (dispatcher #f)))

  (provide use-at-readtable)
  (define (use-at-readtable)
    (port-count-lines! (current-input-port))
    (current-readtable at-readtable))

  ;; similar to plain Scheme (scribble, actually), but with `|' as a
  ;; terminating macro (otherwise it behaves the same; the only difference is
  ;; that `a|b|c' is three symbols)
  (define command-readtable
    (make-readtable at-readtable #\| 'terminating-macro
      (lambda (char inp source-name line-num col-num position)
        (let ([m (*regexp-match #rx#"^([^|]*)\\|" inp)])
          (unless m
            (raise-read-error
             "unbalanced `|'" source-name line-num col-num position #f))
          (datum->syntax-object
            #f (string->symbol (bytes->string/utf-8 (cadr m)))
            (list source-name line-num col-num position
                  (add1 (bytes-length (car m)))))))))

  (define default-src (gensym 'scribble-reader))
  (define (src-name src port)
    (if (eq? src default-src) (object-name port) src))

  (define/kw (*read #:optional [inp (current-input-port)])
    (port-count-lines! inp)
    (parameterize ([current-readtable at-readtable])
      (read inp)))

  (define/kw (*read-syntax #:optional [src default-src]
                                      [inp (current-input-port)])
    (port-count-lines! inp)
    (parameterize ([current-readtable at-readtable])
      (read-syntax (src-name src inp) inp)))

  (define/kw (read-inside #:optional [inp (current-input-port)])
    (port-count-lines! inp)
    (let-values ([(line col pos) (port-next-location inp)])
      (parameterize ([current-readtable at-readtable])
        (syntax-object->datum
         ((dispatcher #t) #f inp (object-name inp) line col pos)))))

  (define/kw (read-inside-syntax #:optional [src default-src]
                                            [inp (current-input-port)])
    (port-count-lines! inp)
    (let-values ([(line col pos) (port-next-location inp)])
      (parameterize ([current-readtable at-readtable])
        ((dispatcher #t) #f inp (src-name src inp) line col pos))))

  (provide (rename *read read) (rename *read-syntax read-syntax)
           read-inside read-inside-syntax)

  )
