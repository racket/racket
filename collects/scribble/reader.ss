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
  (define ch:expr-escape #\|)
  (define ch:attrs-begin #\[)
  (define ch:attrs-end   #\])
  (define ch:lines-begin #\{)
  (define ch:lines-end   #\})

  (define str:lines-begin* #"\\|[^a-zA-Z0-9 \t\r\n\f@\\\177-\377{]*\\{")

  (define re:command       (^px ch:command
                                ;; the following identifies string and
                                ;; expression escapes, see how it is used below
                                "(?:(\")|("ch:expr-escape"))?"))
  (define re:whitespaces   (^px "\\s+"))
  (define re:comment-start (^px ch:comment))
  (define re:comment-line  (^px "[^\n]*\n[ \t]*")) ; like tex's `%'
  (define re:expr-escape   (^px ch:expr-escape))
  (define re:attrs-begin   (^px ch:attrs-begin))
  (define re:attrs-end     (^px ch:attrs-end))
  (define re:lines-begin   (^px ch:lines-begin))
  (define re:lines-begin*  (^px str:lines-begin*))
  (define re:lines-end     (^px ch:lines-end))
  (define str:end-of-line  "[ \t]*\r?\n[ \t]*") ; eat spaces on the next line
  (define re:end-of-line   (^px str:end-of-line))
  (define (re:line-item* bgn end)
    (^px "(.+?)(?:" (if bgn `(,bgn"|") "") (if end `(,end"|") "")
         ch:command"|"str:end-of-line"|$)"))
  (define re:line-item (re:line-item* ch:lines-begin ch:lines-end))
  (define re:line-item-no-nests (re:line-item* #f #f))

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
  ;; (Note: used to wrap special comment values too.)
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
             [loc  (cond [(and line col) (format "at ~a:~a" line col)]
                         [pos (format "at #~a" pos)]
                         [else #f])]
             [loc  (cond [(and source-name loc)
                          (format "when reading ~a ~a" source-name loc)]
                         [source-name (format "when reading ~a" source-name)]
                         [else loc])]
             [msg  (if loc (format "~a (~a)" msg loc) msg)])
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

    (define (read-delimited-list begin-re end-re end-ch tweak-locations)
      ;; when `tweak-locations' is not #f, it should be (src line col pos) for
      ;; the whole thing -- and we need to adjust the first item so it appears
      ;; from its beginning, and the last so it appears to go to its end (used
      ;; to make escape sequences not have bogus indentation added)
      (and (*skip begin-re)
           (let ([reader (if tweak-locations
                           ;; should always be `read-syntax/recursive', but
                           ;; then we don't get location information (in also
                           ;; means that we never get a special-comment)
                           read-syntax
                           read-syntax/recursive)])
             (let loop ([r '()])
               (skip-whitespace inp)
               (if (*skip end-re)
                 (cond [(null? r) r]
                       [(not tweak-locations) (reverse! r)]
                       [(null? (cdr r))
                        ;; make the single syntax span the whole thing
                        (list (datum->syntax-object (car r) (syntax-e (car r))
                                `(,@tweak-locations
                                  ,(span-from (cadddr tweak-locations)))))]
                       [else
                        (let* (;; make the last one span to the end
                               [last (car r)]
                               [last (datum->syntax-object last (syntax-e last)
                                       (list (syntax-source last)
                                             (syntax-line last)
                                             (syntax-column last)
                                             (syntax-position last)
                                             (span-from
                                              (syntax-position last))))]
                               [r (reverse! (cons last (cdr r)))]
                               ;; make the first go from the beginning
                               [fst (car r)]
                               [fst (datum->syntax-object fst (syntax-e fst)
                                      `(,@tweak-locations
                                        ,(let ([tw-pos (cadddr tweak-locations)]
                                               [1pos   (syntax-position fst)]
                                               [1span  (syntax-span fst)])
                                           (and tw-pos 1pos 1span
                                                (+ (- 1pos tw-pos) 1span)))))])
                          (cons fst (cdr r)))])
                 (let ([x (reader source-name inp)])
                   (if (eof-object? x)
                     (read-error 'eof "expected a '~a'" end-ch)
                     (loop (if (special-comment? x) r (cons x r))))))))))

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
                    (let* ([stxcol (syntax/placeholder-column stx)]
                           [stx*   (syntax/placeholder-strip stx)]
                           ;; add spaces
                           [r (if (and newline?
                                       (< curline line)
                                       (< mincol stxcol))
                                (cons (syntax-property
                                       (datum->syntax-object/placeholder stx
                                         (make-spaces (- stxcol mincol)))
                                       'scribble 'indentation)
                                      r)
                                r)]
                           ;; remove special-comments
                           [r (if (special-comment? stx*) r (cons stx* r))])
                      r)))))))

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
         ;; strip off leading and trailing newlines (must have at least one
         ;; non-newline item)
         (let* ([rlines   (if (eol-syntax? (car rlines)) (cdr rlines) rlines)]
                [lines    (reverse! rlines)]
                [1st-eol? (eol-syntax? (car lines))]
                [lines    (if 1st-eol? (cdr lines) lines)])
           (if (null? (cdr lines)) ; common case: one string
             (let ([line (syntax/placeholder-strip (car lines))])
               ;; note: we can get comment values
               (if (special-comment? line) '() (list line)))
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
          (cond
            [(and re:begin (*match1 re:begin))
             => (lambda (m) (loop (add1 lvl) (maybe-merge (make-stx m) r)))]
            [(and re:end (*match1 re:end))
             => (lambda (m)
                  (if (and (zero? lvl) (not start-inside?))
                    (done-lines r)
                    (loop (sub1 lvl) (maybe-merge (make-stx m) r))))]
            [(*match1 re:end-of-line)
             => (lambda (m)
                  (loop lvl (cons ; no merge needed
                             (syntax-property (make-stx eol-token)
                                              'scribble `(newline ,m))
                             r)))]
            [(*peek re:command)
             ;; read the next value, include comment objs, keep source location
             ;; manually (see above)
             => (lambda (m)
                  (let ([x (cond
                             [(cadr m)
                              ;; the command is a string escape, use
                              ;; `read-syntax', to not get a placeholder, so we
                              ;; can merge the string to others
                              (let ([x (read-syntax source-name inp)])
                                ;; adjust to not get bogus indentation
                                (make-stx (syntax-e x)))]
                             [(caddr m)
                              ;; it's an expression escape, get multiple
                              ;; expressions and put them all here
                              (read-bytes (caaddr m) inp)
                              (get-escape-expr #f line col pos)]
                             [else
                              ;; otherwise it's a plain read
                              (read-syntax/recursive source-name inp)])])
                    (loop
                     lvl
                     (cond
                       [(eof-object? x) (read-error 'eof "missing command")]
                       [(syntax? x) (maybe-merge x r)]
                       ;; escaped expressions (not empty: @||)
                       [(pair? x) (append! (reverse x) r)]
                       ;; a comment in the middle of a line disappears so
                       ;; strings next to it are merged
                       [(and (special-comment? x)
                             (not (and (pair? r) (eol-syntax? (car r)))))
                        r]
                       ;; otherwise it's a either null (@||) a comment (at the
                       ;; beginning of a line) or a placeholder: wrap to get
                       ;; source info for proper indentation; @|| is turned to
                       ;; a comment, which can be used to separate strings, or
                       ;; to make spaces meaningful
                       [else (let ([x (if (null? x)
                                        (make-special-comment #f)
                                        x)])
                               (cons (make-placeholder x ; no merge
                                       (list source-name line col pos
                                             (span-from pos)))
                                     r))]))))]
            ;; must be last, since it will always succeed with 1 char
            [(*peek re:item) ; don't read: regexp grabs the following text
             => (lambda (m)
                  (loop lvl (maybe-merge (make-stx (read-bytes (cdadr m) inp))
                                         r)))]
            [(*peek #rx#"^$")
             (if end-token
               (read-error 'eof "missing closing `~a'~a" end-token
                           (if (and line-num col-num)
                             (format " for command at ~a:~a" line-num col-num)
                             ""))
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
      (read-delimited-list re:attrs-begin re:attrs-end ch:attrs-end #f))

    (define (get-escape-expr single? line col pos)
      ;; single? means expect just one expression (or none, which is returned
      ;; as a special-comment)
      (let ([xs (parameterize ([current-readtable command-readtable])
                  ;; tweak source information to avoid bad indentation
                  (read-delimited-list
                   re:expr-escape re:expr-escape ch:expr-escape
                   (list source-name line col pos)))])
        (cond [(not xs) xs]
              [(not single?) xs]
              [(null? xs) (make-special-comment #f)]
              [(null? (cdr xs)) (car xs)]
              [else (read-error* line col pos (span-from pos)
                                 "too many escape expressions")])))

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
               [(get-escape-expr #t line-num col-num position)
                => (lambda (expr) (values expr #f #f))]
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
    (make-readtable #f ch:command 'non-terminating-macro (dispatcher #f)))

  (provide use-at-readtable)
  (define (use-at-readtable)
    (port-count-lines! (current-input-port))
    (current-readtable at-readtable))

  ;; similar to plain Scheme (scribble, actually), but with `@' and `|' as
  ;; terminating macro characters (otherwise it behaves the same; the only
  ;; difference is that `a|b|c' is three symbols and `@foo@bar' are two
  ;; @-forms)
  (define command-readtable
    (make-readtable at-readtable
      ch:command 'terminating-macro (dispatcher #f)
      #\| 'terminating-macro
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
