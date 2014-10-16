;; ============================================================================
;; Implements the @-reader macro for embedding text in Racket code.

#lang racket/base

(require syntax/readerr)

;; ----------------------------------------------------------------------------
;; utilities for syntax specifications below

;; regexps
(define (px . args)
  (let* ([args (let loop ([xs args])
                 (if (list? xs) (apply append (map loop xs)) (list xs)))]
         [args (map (lambda (x)
                      (cond [(bytes? x) x]
                            [(string? x) (string->bytes/utf-8 x)]
                            [(char? x) (regexp-quote (string->bytes/utf-8 (string x)))]
                            [(not x) #""]
                            [else (internal-error 'px)]))
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
    (lambda (bs) (list->bytes (map rev-byte (reverse (bytes->list bs)))))))

;; ----------------------------------------------------------------------------
;; syntax

;; basic syntax customization
(define ch:command      #\@)
(define ch:comment      #\;)
(define ch:expr-escape  #\|)
(define ch:datums-begin #\[)
(define ch:datums-end   #\])
(define ch:lines-begin  #\{)
(define ch:lines-end    #\})

(define str:lines-begin* #"(\\|[^a-zA-Z0-9 \t\r\n\f@\\\177-\377{]*)\\{")
(define str:end-of-line  "[ \t]*\r?\n[ \t]*") ; eat spaces on the next line

;; regexps based on the above (more in make-dispatcher)
(define re:whitespaces   (^px "\\s+"))
(define re:comment-start (^px ch:comment))
(define re:comment-line  (^px "[^\n]*(?:\n|$)[ \t]*")) ; like tex's `%'
(define re:expr-escape   (^px ch:expr-escape))
(define re:datums-begin  (^px ch:datums-begin))
(define re:datums-end    (^px ch:datums-end))
(define re:lines-begin   (^px ch:lines-begin))
(define re:lines-begin*  (^px str:lines-begin*))
(define re:lines-end     (^px ch:lines-end))
(define re:end-of-line   (^px str:end-of-line))

;; ----------------------------------------------------------------------------
;; utilities

(define (internal-error label)
  (error 'scribble-reader "internal error [~a]" label))

;; like `regexp-try-match', without extras; the regexp that is used
;; must be anchored -- nothing is dropped
(define (*regexp-match-peek-positions pattern input-port)
  #; ; sanity checks, not needed unless this file is edited
  (unless (and (byte-regexp? pattern)
               (regexp-match? #rx#"^\\^" (object-name pattern)))
    (internal-error 'invalid-bregexp))
  (regexp-match-peek-positions pattern input-port))
;; the following doesn't work -- must peek first
;; (define (*regexp-match-positions pattern input-port)
;;   #; ; sanity checks, not needed unless this file is edited
;;   (unless (and (byte-regexp? pattern)
;;                (regexp-match? #rx#"^\\^" (object-name pattern)))
;;     (internal-error 'invalid-bregexp))
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

;; Utility for readtable-based caches
(define (readtable-cached fun)
  (let ([cache (make-weak-hasheq)])
    (letrec ([readtable-cached
              (case-lambda
                [(rt) (hash-ref cache rt
                        (lambda ()
                          (let ([r (fun rt)])
                            (hash-set! cache rt r)
                            r)))]
                [() (readtable-cached (current-readtable))])])
      readtable-cached)))

;; Skips whitespace characters, sensitive to the current readtable's
;; definition of whitespace; optimizes common spaces when possible
(define skip-whitespace
  (let* ([plain-readtables  (make-weak-hasheq)]
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
    (define plain-readtable?
      (readtable-cached
       (lambda (rt)
         (andmap (lambda (ch) (whitespace? ch rt)) plain-spaces-list))))
    (lambda (port)
      (let* ([rt (current-readtable)] [plain? (plain-readtable? rt)])
        (let loop ()
          (when plain? (skip-plain-spaces port))
          (let ([ch (peek-char port)])
            (unless (eof-object? ch)
              (when (whitespace? ch rt) (read-char port) (loop)))))))))

;; make n spaces, cached for n
(define make-spaces
  (let ([t (make-hasheq)])
    (lambda (n)
      (hash-ref t n
        (lambda ()
          (let ([s (make-string n #\space)])
            (hash-set! t n s) s))))))

(define (bytes-width bs start)
  (let ([len (bytes-length bs)])
    (if (regexp-match? #rx"^ *$" bs start)
      (- (bytes-length bs) start)
      (let loop ([i start] [w 0])
        (if (= i len)
          w
          (loop (add1 i)
                (+ w (if (eq? 9 (bytes-ref bs i)) (- 8 (modulo w 8)) 1))))))))

;; A syntax object that has the "original?" property:
(define orig-stx (read-syntax #f (open-input-string "dummy")))

;; ----------------------------------------------------------------------------
;; main reader function for @ constructs

(define (dispatcher char inp source-name line-num col-num position
                    start-inside? command-readtable ch:command
                    re:command re:line-item* re:line-item
                    re:line-item-no-nests datum-readtable
                    syntax-post-processor)

  (define (read-error line col pos msg . xs)
    (let* ([eof? (and (eq? 'eof msg) (pair? xs))]
           [msg  (apply format (if eof? xs (cons msg xs)))])
      ((if eof? raise-read-error raise-read-eof-error)
       msg (or source-name (object-name inp)) line col pos (span-from pos))))
  (define (read-error* . xs)
    (apply read-error line-num col-num position xs))

  (define (read-stx) (read-syntax/recursive source-name inp))
  (define (read-stx/rt rt) (read-syntax/recursive source-name inp #f rt))
  ;; use this to avoid placeholders
  (define (read-stx*)
    ;; (read-syntax/recursive source-name inp #f (current-readtable) #f)
    (read-syntax source-name inp))

  (define (*match  rx) (*regexp-match rx inp))
  (define (*match1 rx) (*regexp-match1 rx inp))
  ;; (define (*skip   rx) (*regexp-match-positions rx inp)) ; <- see above
  (define (*skip   rx) (*regexp-match1 rx inp))
  (define (*peek   rx) (*regexp-match-peek-positions rx inp))

  (define (span-from start)
    (and start (let-values ([(line col pos) (port-next-location inp)])
                 (- pos start))))

  (define (read-delimited-list begin-re end-re end-ch)
    (let-values ([(line col pos) (port-next-location inp)])
      (and (*skip begin-re)
           (let loop ([r '()])
             (skip-whitespace inp)
             (if (*skip end-re)
               (reverse r)
               (let ([x (read-stx)])
                 (if (eof-object? x)
                   (read-error line col pos 'eof "expected a '~a'" end-ch)
                   (loop (if (special-comment? x) r (cons x r))))))))))

  ;; identifies newlines in text
  (define (eol-syntax? x)
    (let ([p (and (syntax? x) (syntax-property x 'scribble))])
      (and (pair? p) (eq? 'newline (car p)))))

  ;; gets an accumulated (reversed) list of syntaxes and column markers, and
  ;; sorts things out (remove prefix and suffix newlines, adds indentation if
  ;; needed)
  (define (done-items xs)
    ;; a column marker is either a non-negative integer N (saying the following
    ;; code came from at column N), or a negative integer -N (saying that the
    ;; following code came from column N but no need to add indentation at this
    ;; point because it is at the openning of a {...}); `get-lines*' is careful
    ;; not to include column markers before a newline or the end of the text,
    ;; and a -N marker can only come from the beginning of the text (and it's
    ;; never there if the text began with a newline)
    (if (andmap eol-syntax? xs)
      ;; nothing to do
      (reverse xs)
      (let ([mincol (let loop ([xs xs] [m #f])
                      (if (null? xs)
                        m
                        (let ([x (car xs)])
                          (loop (cdr xs)
                                (if (integer? x)
                                  (let ([x (abs x)]) (if (and m (< m x)) m x))
                                  m)))))])
        (let loop ([xs (if (and (not start-inside?) (eol-syntax? (car xs)))
                         (cdr xs) ; trim last eol
                         xs)]
                   [r '()])
          (if (or (null? xs)
                  (and (not start-inside?)
                       ;; trim first eol
                       (null? (cdr xs)) (eol-syntax? (car xs))))
            r
            (loop
             (cdr xs)
             (let ([x (car xs)])
               (cond [(integer? x)
                      (if (or (< x 0) (= x mincol))
                        r ; no indentation marker, or zero indentation
                        (let ([eol (cadr xs)]
                              [spaces (make-spaces (- x mincol))])
                          ;; markers always follow end-of-lines
                          (unless (eol-syntax? eol)
                            (internal-error 'done-items))
                          (cons (syntax-property
                                 (datum->syntax eol spaces eol)
                                 'scribble 'indentation)
                                r)))]
                     ;; can have special comment values from "@||"
                     [(special-comment? x) r]
                     [else (cons x r)]))))))))

  ;; cons stx (new syntax) to the list of stxs, merging it if both are
  ;; strings, except for newline markers
  (define (maybe-merge stx stxs)
    (let* ([2nd  (and (syntax? stx) (syntax-e stx))]
           [stx0 (and (pair? stxs) (car stxs))]
           [1st  (and (syntax? stx0) (syntax-e stx0))])
      (if (and (string? 1st) (not (eol-syntax? stx0))
               (string? 2nd) (not (eol-syntax? stx)))
        (cons (datum->syntax stx0
                (string-append 1st 2nd)
                (vector (syntax-source stx0)
                        (syntax-line   stx0)
                        (syntax-column stx0)
                        (syntax-position stx0)
                        ;; this is called right after reading stx
                        (span-from (syntax-position stx0)))
                stx0)
              (cdr stxs))
        (cons stx stxs))))

  ;; helper for `get-lines*' drop a column marker if the previous item was also
  ;; a newline (or the beginning)
  (define (maybe-drop-marker r)
    (if (and (pair? r) (integer? (car r))
             (or (null? (cdr r)) (eol-syntax? (cadr r))))
      (cdr r)
      r))

  (define (get-lines* re:begin re:end re:cmd-pfx re:item end-token)
    ;; re:begin, re:end, end-token can be false if start-inside? is #t;
    ;; re:cmd-pfx is a regexp when we do sub-@-reads only after a prefix
    (let loop ([lvl 0]
               [r (let-values ([(l c p) (port-next-location inp)])
                    ;; marker for the beginning of the text
                    (if c (list (- c)) '()))])
      ;; this loop collects lines etc for the body, and also puts in column
      ;; markers (integers) after newlines -- the result is handed off to
      ;; `done-items' to finish the job
      (define-values (line col pos) (port-next-location inp))
      (define (make-stx sexpr)
        (datum->syntax #f
          (if (bytes? sexpr) (bytes->string/utf-8 sexpr) sexpr)
          (vector source-name line col pos (span-from pos))
          orig-stx))
      (cond
        [(and re:begin (*match1 re:begin))
         => (lambda (m) (loop (add1 lvl) (maybe-merge (make-stx m) r)))]
        [(and re:end (*match1 re:end))
         => (lambda (m)
              (if (and (zero? lvl) (not start-inside?))
                ;; drop a marker if it's after a last eol item
                (done-items (maybe-drop-marker r))
                (loop (sub1 lvl) (maybe-merge (make-stx m) r))))]
        [(*match1 re:end-of-line)
         => (lambda (m)
              (let ([n (car (regexp-match-positions #rx#"\n" m))])
                (loop lvl (list* ; no merge needed
                           (bytes-width m (cdr n))
                           (syntax-property
                            (make-stx "\n")
                            'scribble `(newline ,(bytes->string/utf-8 m)))
                           (maybe-drop-marker r)))))]
        [(if re:cmd-pfx
           (and (*skip re:cmd-pfx) (*peek re:command))
           (*peek re:command))
         ;; read the next value
         => (lambda (m)
              (define x (cond [(cadr m)
                               ;; the command is a string escape, use
                               ;; `read-stx*' to not get a placeholder, so we
                               ;; can merge the string to others
                               (read-stx*)]
                              [(caddr m)
                               ;; it's an expression escape, get multiple
                               ;; expressions and put them all here
                               (read-bytes (caaddr m) inp)
                               (get-escape-expr #f)]
                              [else (read-stx)])) ; otherwise: a plain sub-read
              (loop lvl (cond [(eof-object? x)
                               ;; shouldn't happen -- the sub-read would
                               ;; raise an error
                               (internal-error 'get-lines*-sub-read)]
                              ;; throw away comments
                              [(special-comment? x) r]
                              ;; escaped expressions: no merge, and add a
                              ;; comment to prevent merges with later stuff
                              [(pair? x)
                               `(,(make-special-comment #f) ,@(reverse x) ,@r)]
                              [(null? x) (cons (make-special-comment #f) r)]
                              [else (maybe-merge x r)])))]
        ;; must be last, since it will always succeed with 1 char
        [(*peek re:item) ; don't read: regexp grabs the following text
         => (lambda (m)
              (loop lvl
                    (maybe-merge (make-stx (read-bytes (cdadr m) inp)) r)))]
        [(*peek #rx#"^$")
         (if end-token
           (read-error* 'eof "missing closing `~a'" end-token)
           (done-items r))]
        [else (internal-error 'get-lines*)])))

  (define (get-lines)
    (cond [(*skip re:lines-begin) (get-lines* re:lines-begin re:lines-end #f
                                              re:line-item ch:lines-end)]
          [(*match re:lines-begin*)
           => (lambda (m)
                (let* ([bgn (car m)]
                       [end  (reverse-bytes bgn)]
                       [bgn* (regexp-quote bgn)]
                       [end* (regexp-quote end)]
                       [cmd-pfx* (regexp-quote (cadr m))])
                  (get-lines* (^px bgn*) (^px end*)
                              (^px cmd-pfx* "(?=" ch:command ")")
                              (re:line-item* bgn* end* cmd-pfx*)
                              end)))]
          [else #f]))

  (define (get-datums)
    (parameterize ([current-readtable datum-readtable])
      (read-delimited-list re:datums-begin re:datums-end ch:datums-end)))

  (define (get-escape-expr single?)
    ;; single? means expect just one expression (or none, which is returned as
    ;; a special-comment)
    (let ([get (lambda ()
                 (parameterize ([current-readtable command-readtable])
                   (read-delimited-list re:expr-escape re:expr-escape
                                        ch:expr-escape)))])
      (if single?
        (let*-values ([(line col pos) (port-next-location inp)]
                      [(xs) (get)])
          (cond [(not xs) xs]
                [(or (null? xs) (not (null? (cdr xs))))
                 (read-error line col pos
                             "a ~a|...| form in Racket mode must have ~a"
                             ch:command
                             "exactly one escaped expression")]
                [else (car xs)]))
        (get))))

  ;; called only when we must see a command in the input
  (define (get-command)
    (let ([cmd (read-stx/rt command-readtable)])
      (cond [(special-comment? cmd)
             (read-error* "expecting a command expression, got a comment")]
            [(eof-object? cmd)
             (read-error* 'eof "missing command")]
            ;; we have a command: adjust its location to include the dispatch
            ;; character
            [else
             ;; (datum->syntax #f (syntax-e cmd)
             ;;   (vector (syntax-source cmd)
             ;;           (syntax-line cmd)
             ;;           (cond [(syntax-column cmd) => sub1] [else #f])
             ;;           (cond [(syntax-position cmd) => sub1] [else #f])
             ;;           (cond [(syntax-span cmd) => add1] [else #f]))
             ;;   orig-stx)
             ;; The reasoning for the above is that in `@foo' the `@' is part
             ;; of the syntax of the identifier, in a similar way to including
             ;; the double quotes in the position information for a string
             ;; syntax or the backslash in a mzscheme \foo identifier.  Another
             ;; feature of this is that there needs to be some way to know what
             ;; was the actual source of some syntax.  However, this is
             ;; problematic in two ways: (a) it can be confusing that
             ;; highlighting an identifier highlights the `@' too, and more
             ;; importantly, it makes `@|foo|' be treated differently than
             ;; `@foo'.  So we'll try to not do this adjusting.
             cmd])))

  (define (get-rprefixes) ; return punctuation prefixes in reverse
    (let loop ([r '()])
      (let-values ([(line col pos) (port-next-location inp)])
        (cond [(*match1 #rx#"^#?(?:'|`|,@?)")
               => (lambda (m)
                    (let ([sym (cond [(assoc m '([#"'"   quote]
                                                 [#"`"   quasiquote]
                                                 [#","   unquote]
                                                 [#",@"  unquote-splicing]
                                                 [#"#'"  syntax]
                                                 [#"#`"  quasisyntax]
                                                 [#"#,"  unsyntax]
                                                 [#"#,@" unsyntax-splicing]))
                                      => cadr]
                                     [else (internal-error 'get-rprefixes)])])
                      (loop (cons (datum->syntax #f sym
                                                 (vector source-name line col
                                                         pos (span-from pos))
                                                 orig-stx)
                                  r))))]
              [(*skip re:whitespaces)
               (read-error* "unexpected whitespace after ~a" ch:command)]
              [else r]))))

  (cond
    [start-inside?
     (datum->syntax #f (get-lines* #f #f #f re:line-item-no-nests #f)
       (vector source-name line-num col-num position (span-from position))
       orig-stx)]
    [(*skip re:whitespaces)
     (read-error* "unexpected whitespace after ~a" ch:command)]
    [(*skip re:comment-start)
     (unless (get-lines) (*skip re:comment-line))
     (make-special-comment #f)]
    [else
     (let*-values
         ([(rpfxs) (get-rprefixes)]
          [(cmd datums lines)
           (cond [(get-lines)
                  ;; try get-lines first -- so @|{...}| is not used as a simple
                  ;; expression escape, same for get-datums
                  => (lambda (lines) (values #f #f lines))]
                 [(get-datums)
                  => (lambda (datums) (values #f datums (get-lines)))]
                 [(get-escape-expr #t) => (lambda (expr) (values expr #f #f))]
                 [else (values (get-command) (get-datums) (get-lines))])]
          [(stx) (and (or datums lines)
                      (append (or datums '()) (or lines '())))]
          [(stx) (or (and cmd stx (cons cmd stx)) ; all parts
                     stx  ; no cmd part => just a parenthesized expression
                     cmd  ; no datums/lines => simple expression (no parens)
                     ;; impossible: either we saw []s or {}s, or we read a
                     ;; racket expression
                     (internal-error 'dispatcher))]
          [(stx) (let ([ds (and datums (length datums))]
                       [ls (and lines  (length lines))])
                   (syntax-property
                    (if (syntax? stx)
                      stx
                      (datum->syntax #f stx
                        (vector source-name line-num col-num position
                                (span-from position))
                        orig-stx))
                    'scribble (list 'form ds ls)))]
          [(stx) (syntax-post-processor stx)]
          [(stx)
           ;; wrap the prefixes around the result
           (let loop ([rpfxs rpfxs] [stx stx])
             (if (null? rpfxs)
               stx
               (loop (cdr rpfxs) (list (car rpfxs) stx))))])
       (datum->syntax #f stx (vector source-name line-num col-num position
                                     (span-from position))
                      orig-stx))]))

(define (make-dispatcher start-inside? ch:command
                         get-command-readtable get-datum-readtable
                         syntax-post-processor)
  (define re:command (^px ch:command
                          ;; the following identifies string and expression
                          ;; escapes, see how it is used above
                          "(?:(\")|("ch:expr-escape"))?"))
  (define (re:line-item* bgn end cmd-prefix)
    (^px "(.+?)(?:" (and bgn `(,bgn"|")) (and end `(,end"|"))
         cmd-prefix ch:command"|"str:end-of-line"|$)"))
  (define re:line-item (re:line-item* ch:lines-begin ch:lines-end #f))
  (define re:line-item-no-nests (and start-inside? (re:line-item* #f #f #f)))
  (lambda (char inp source-name line-num col-num position)
    (dispatcher char inp source-name line-num col-num position
                start-inside? (get-command-readtable) ch:command
                re:command re:line-item* re:line-item re:line-item-no-nests
                (get-datum-readtable) syntax-post-processor)))

;; ----------------------------------------------------------------------------
;; minor utilities for the below

(define default-src (gensym 'scribble-reader))
(define (src-name src port)
  (if (eq? src default-src) (object-name port) src))

(define-syntax-rule (named-lambda (name . args) . body)
  (let ([name (lambda args . body)]) name))

;; ----------------------------------------------------------------------------
;; readtable and reader

(provide make-at-readtable make-at-reader)

(define ((make-at-readtable-or-inside-reader inside-reader?)
         readtable command-char command-readtable datum-readtable syntax-post-processor)
  (define (get-cmd-rt)
    (if (readtable? cmd-rt)
        cmd-rt
        (cmd-rt)))
  (define (get-datum-rt)
    (if (eq? datum-rt 'dynamic)
        (current-readtable)
        datum-rt))
  (define dispatcher
    (make-dispatcher #f command-char get-cmd-rt get-datum-rt
                     syntax-post-processor))
  (define (make-inside-reader)
    (define dispatcher
      (make-dispatcher #t command-char get-cmd-rt get-datum-rt
                       syntax-post-processor))
    ;; use a name consistent with `make-at-reader'
    (named-lambda (at-read-syntax/inside [src default-src]
                                         [inp (current-input-port)])
      (define-values [line col pos] (port-next-location inp))
      (parameterize ([current-readtable at-rt])
        (dispatcher #f inp (src-name src inp) line col pos))))
  (define at-rt
    (make-readtable readtable command-char 'non-terminating-macro dispatcher))
  (define command-bar
    (lambda (char inp source-name line-num col-num position)
      (let ([m (*regexp-match #rx#"^([^|]*)\\|" inp)])
        (unless m
          (raise-read-error "unbalanced `|'" source-name
                            line-num col-num position #f))
        (datum->syntax
         #f (string->symbol (bytes->string/utf-8 (cadr m)))
         (vector source-name line-num col-num position
                 (add1 (bytes-length (car m))))
         orig-stx))))
  (define (make-cmd-rt command-readtable)
    ;; similar to plain Racket (scribble, actually), but with `@' as usual and
    ;; and `|' as a terminating macro characters (otherwise it behaves the
    ;; same; the only difference is that `a|b|c' is three symbols)
    (make-readtable command-readtable
      command-char 'non-terminating-macro dispatcher
      #\| 'terminating-macro command-bar))
  (define cmd-rt
    (if (eq? command-readtable 'dynamic)
        (readtable-cached make-cmd-rt)
        (make-cmd-rt command-readtable)))
  (define datum-rt
    (cond [(or (not datum-readtable) (readtable? datum-readtable))
           datum-readtable]
          [(eq? #t datum-readtable) at-rt]
          [(procedure? datum-readtable) (datum-readtable at-rt)]
          [(eq? datum-readtable 'dynamic) 'dynamic]
          [else (error 'make-at-readtable
                       "bad datum-readtable: ~e" datum-readtable)]))
  (if inside-reader? (make-inside-reader) at-rt))

(define (make-at-readtable
         #:readtable             [readtable (current-readtable)]
         #:command-char          [command-char ch:command]
         #:command-readtable     [command-readtable readtable]
         #:datum-readtable       [datum-readtable #t]
         #:syntax-post-processor [syntax-post-processor values])
  ((make-at-readtable-or-inside-reader #f)
   readtable command-char command-readtable datum-readtable syntax-post-processor))

(define (make-at-reader
         #:readtable             [readtable (current-readtable)]
         #:command-char          [command-char ch:command]
         #:datum-readtable       [datum-readtable #t]
         #:command-readtable     [command-readtable readtable]
         #:syntax-post-processor [syntax-post-processor values]
         #:syntax?               [syntax-reader? #t]
         #:inside?               [inside-reader? #f])
  (let ([r ((make-at-readtable-or-inside-reader inside-reader?)
            readtable command-char command-readtable datum-readtable syntax-post-processor)])
    ;; the result can be a readtable or a syntax reader, depending on inside?,
    ;; convert it now to the appropriate reader
    (if inside-reader?
      ;; if it's a function, then it already is a syntax reader, convert it to
      ;; a plain reader if needed (note: this only happens when r is a reader)
      (if syntax-reader?
        r
        (named-lambda (at-read/inside [in (current-input-port)])
          ;; can't be eof, since it returns a list of expressions (as a syntax)
          (syntax->datum (r (object-name in) in))))
      ;; if it's a readtable, then just wrap the standard functions
      (if syntax-reader?
        (named-lambda (at-read-syntax [src default-src]
                                      [inp (current-input-port)])
          (parameterize ([current-readtable r])
            (read-syntax src inp)))
        (named-lambda (at-read [inp (current-input-port)])
          (parameterize ([current-readtable r])
            (let ([r (read-syntax (object-name inp) inp)])
              ;; it might be eof
              (if (syntax? r) (syntax->datum r) r))))))))

(provide use-at-readtable)
(define use-at-readtable
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (port-count-lines! (current-input-port))
     (current-readtable
      (keyword-apply make-at-readtable kws kw-args rest)))))

;; utilities for below
(define make-default-at-readtable
  (readtable-cached (lambda (rt) (make-at-readtable #:readtable rt
                                                    #:command-readtable 'dynamic
                                                    #:datum-readtable 'dynamic))))
(define make-default-at-reader/inside
  (readtable-cached
   (lambda (rt) (make-at-reader #:inside? #t #:readtable rt
                                #:command-readtable 'dynamic
                                #:datum-readtable 'dynamic))))

;; ----------------------------------------------------------------------------
;; readers

(provide (rename-out [*read read] [*read-syntax read-syntax]))
(define (*read [inp (current-input-port)])
  (parameterize ([current-readtable (make-default-at-readtable)])
    (read inp)))
(define (*read-syntax [src default-src] [inp (current-input-port)])
  (parameterize ([current-readtable (make-default-at-readtable)])
    (read-syntax (src-name src inp) inp)))

(provide read-inside read-syntax-inside)
(define (read-inside [inp (current-input-port)])
  (syntax->datum ((make-default-at-reader/inside) default-src inp)))
(define (read-syntax-inside [src default-src] [inp (current-input-port)] 
                            #:command-char [command-char ch:command])
  (((readtable-cached
     (lambda (rt) (make-at-reader #:inside? #t #:command-char command-char #:readtable rt))))
   src inp))
