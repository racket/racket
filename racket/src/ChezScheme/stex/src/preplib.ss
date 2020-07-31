;;; preplib.ss
;;;
;;; Copyright (c) 1998-2016 R. Kent Dybvig and Oscar Waddell
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(library (preplib)
  (export current-ifile genlab-prefix genlab-counters
    state-case read-alpha-command read-command
    command-symbol? read-back-slash read-open-brace read-close-brace
    read-bracketed-text read-optional-arg push-ifile pop-ifile input-error
    unexpected-eof unexpected-command genlab read-integer read-def-pattern
    read-args expand-template suppress-white-space parse-index global-def
    set-def! get-def conditional? populate-source-directories anchored-filename? find-filename
    open-input-file)

  (import (except (chezscheme) open-input-file))

  (define current-ifile (make-parameter #f))
  (define genlab-prefix (make-parameter #f))
  (define genlab-counters (make-parameter #f))

  (define-syntax state-case
    (syntax-rules ()
      [(_ (var exp) c1 c2 ...)
       (identifier? (syntax var))
       (let ([var exp]) (state-case-help var c1 c2 ...))]))

  (define-syntax state-case-help
    (syntax-rules (else)
      [(_ var (else e1 e2 ...)) (begin e1 e2 ...)]
      [(_ var ((k ...) e1 e2 ...) c ...)
       (if (or (state-case-test var k) ...)
           (begin e1 e2 ...)
           (state-case-help var c ...))]))

  (define-syntax state-case-test
    (syntax-rules (eof -)
      [(_ var eof)
       (eof-object? var)]
      [(_ var (char1 - char2))
       (and (char? var) (char<=? char1 var char2))]
      [(_ var char)
       (and (char? var) (char=? var char))]))

  ; doesn't allow @ even in document class or style files.  this won't
  ; work for us anyway because we use character-based rather than
  ; token-based substitution, so macros that insert @ symbols into
  ; their output won't work outside of the original context
  (define read-alpha-command
    ; return symbol representing command; assume \ already seen and scan
    ; maximal string of alphabetic chars, e.g., \scheme => symbol scheme
    ; returns || when no command is recognized
    (let ([buf (open-output-string)])
      (lambda (ip)
        (state-case (c (peek-char ip))
          [((#\a - #\z) (#\A - #\Z))
           (let loop ()
             (write-char (read-char ip) buf)
             (state-case (c (peek-char ip))
               [((#\a - #\z) (#\A - #\Z)) (loop)]
               [else (string->symbol (get-output-string buf))]))]
          [else '||]))))

  (define read-command
    ; like read-alpha-command, but allows single nonalphabetic char
    ; commands, e.g., \' => |'|
    (let ([buf (open-output-string)])
      (lambda (ip)
        (state-case (c (peek-char ip))
          [((#\a - #\z) (#\A - #\Z))
           (let loop ()
             (write-char (read-char ip) buf)
             (state-case (c (peek-char ip))
               [((#\a - #\z) (#\A - #\Z)) (loop)]
               [else (string->symbol (get-output-string buf))]))]
          [(eof) '||]
          [else (read-char ip) (string->symbol (string c))]))))

  (define command-symbol?
    (lambda (cmd) ; true iff command is one character, nonalpabetic
      (let ([s (symbol->string cmd)])
        (and (fx= (string-length s) 1)
             (state-case (c (string-ref s 0))
               [((#\a - #\z) (#\A - #\Z)) #f]
               [else #t])))))

  (define read-back-slash
    (lambda (ip)
      (if (eqv? (peek-char ip) #\\)
          (read-char ip)
          (input-error "back slash expected"))))

  (define read-open-brace
    (lambda (ip)
      (if (eqv? (peek-char ip) #\{)
          (read-char ip)
          (input-error "open brace expected"))))

  (define read-close-brace
    (lambda (ip)
      (if (eqv? (peek-char ip) #\})
          (read-char ip)
          (input-error "close brace expected"))))

  (define read-bracketed-text
    (let ([buf (open-output-string)])
      (case-lambda
        [(ip) (read-open-brace ip) (read-bracketed-text ip 1)]
        [(ip depth)
         (state-case (c (read-char ip))
           [(#\}) (if (= depth 1)
                      (get-output-string buf)
                      (begin (write-char #\} buf)
                        (read-bracketed-text ip (- depth 1))))]
           [(#\{) (write-char #\{ buf) (read-bracketed-text ip (+ depth 1))]
           [(eof) (input-error "file ended within bracketed text")]
           [else (write-char c buf) (read-bracketed-text ip depth)])])))

  (define read-optional-arg
    (let ([buf (open-output-string)])
      (lambda (ip)
        (state-case (c (peek-char ip))
          [(#\[)
           (read-char ip)
           (let loop ([depth 0])
             (state-case (c (read-char ip))
               [(#\]) (if (= depth 0)
                          (get-output-string buf)
                          (begin (write-char c buf) (loop depth)))]
               [(#\{) (write-char c buf) (loop (+ depth 1))]
               [(#\}) (write-char c buf) (loop (- depth 1))]
               [(eof) (input-error "file ended within optional argument")]
               [else (write-char c buf) (loop depth)]))]
          [else #f]))))

  (define push-ifile
    (lambda (ip ifiles)
      (current-ifile ip)
      (cons ip ifiles)))

  (define pop-ifile
    (lambda (ifiles)
      (let ([ifiles (cdr ifiles)])
        (current-ifile (and (not (null? ifiles)) (car ifiles)))
        ifiles)))

  (define input-error
    (lambda (msg . args)
      (define file-coordinates
        (lambda (ip)
          (let ([n (file-position ip)])
            (file-position ip 0)
            (let f ([n n] [line 1] [char 1] [return? #f])
              (if (= n 0)
                  (values line char)
                  (state-case (c (read-char ip))
                    [(#\newline) (f (- n 1) (if return? line (+ line 1)) 1 #f)]
                    [(#\return) (f (- n 1) (+ line 1) 1 #t)]
                    [(eof) (values line char)]
                    [else (f (- n 1) line (+ char 1) #f)]))))))
      (let ([ip (current-ifile)])
        (call-with-values (lambda () (file-coordinates ip))
          (lambda (line char)
            (errorf #f "~a on line ~d, character ~d of ~s"
              (apply format msg args)
              line char
              (port-name ip)))))))

  (define unexpected-eof
    (lambda (where)
      (input-error "unexpected end-of-input ~a" where)))

  (define unexpected-command
    (lambda (cmd)
      (input-error "unexpected command '\\~a'" cmd)))

  (define genlab
    (lambda ()
      (define next-count
        (lambda (fn)
          (cond
            [(assoc fn (genlab-counters)) =>
             (lambda (a)
               (let ([n (+ (cdr a) 1)])
                 (set-cdr! a n)
                 n))]
            [else
             (genlab-counters (cons (cons fn 0) (genlab-counters)))
              0])))
      (let ([name (path-root (port-name (current-ifile)))])
        (string->symbol
          (format "~a:~a~d" name (genlab-prefix) (next-count name))))))

  (define read-integer ; return integer or #f if none found
    (lambda (ip)
      (string->number
        (list->string
          (let loop ()
            (state-case (c (peek-char ip))
              [((#\0 - #\9)) (read-char ip) (cons c (loop))]
              [else '()]))))))

  (define read-def-pattern
    (lambda (ip)
      (let loop ([i 1])
        (state-case (c (peek-char ip))
          [(#\{) '()]
          [(#\#)
           (read-char ip)
           (state-case (c1 (peek-char ip))
             [(#\#) (read-char ip) (list* c1 c (loop i))]
             [else
              (let ([n (read-integer ip)])
                (if (eq? n i)
                    (cons n (loop (+ i 1)))
                    (input-error "invalid \\def argument specifier")))])]
          [(eof) (unexpected-eof "after \\def")]
          [else (read-char ip) (cons c (loop i))]))))

  (define read-args
    (lambda (ip pattern cmd)
      (define read-arg
        (lambda (ip cmd)
          (state-case (c (read-char ip))
            [(#\\) (format "\\~a" (read-command ip))]
            [(#\{) (read-bracketed-text ip 1)]
            [(eof) (unexpected-eof (format "reading ~a arguments" cmd))]
            [else (string c)])))
      (let loop ([pattern pattern])
        (if (null? pattern)
            '()
            (let ([x (car pattern)])
              (cond
                [(integer? x)
                 (let ([arg (read-arg ip cmd)])
                   (cons arg (loop (cdr pattern))))]
                [(string? x)
                 (let ([arg (read-optional-arg ip)])
                   (cons (or arg x) (loop (cdr pattern))))]
                [(eqv? x #\space)
                 (suppress-white-space ip)
                 (loop (cdr pattern))]
                [(eqv? (read-char ip) x) (loop (cdr pattern))]
                [else (input-error "~a use does not match pattern" cmd)]))))))

  (define expand-template
    (let ([buf (open-output-string)])
      (lambda (template args cmd)
        (let ([sip (open-input-string template)])
          (let loop ()
            (state-case (c (read-char sip))
              [(#\\)
               (write-char c buf)
               (state-case (c (peek-char sip))
                 [(#\#) (read-char sip) (write-char c buf)]
                 [else (void)])
               (loop)]
              [(#\#)
               (state-case (c (peek-char sip))
                 [(#\#) (read-char sip) (write-char #\# buf)]
                 [else (let ([n (read-integer sip)])
                         (let ([n (and n (- n 1))])
                           (unless (and n (< -1 n (length args)))
                             (input-error "invalid argument specifier in ~a template" cmd))
                           (display (list-ref args n) buf)))])
               (loop)]
              [(eof) (get-output-string buf)]
              [else (write-char c buf) (loop)]))))))

  (define (suppress-white-space ip)
    (state-case (c (peek-char ip))
      [(#\space #\tab #\newline) (read-char ip) (suppress-white-space ip)]
      [(#\%)
       (read-char ip)
       (let loop ()
         (state-case (c (read-char ip))
           [(eof #\newline) (void)]
           [else (loop)]))]
      [else (void)]))

  (define parse-index
    (let ([buf (open-output-string)])
      ; if proper-nesting? is true, the characters ", @, !, and | lose their
      ; special meaning within nested groups.
      (lambda (ip proper-nesting?)
        (define nested-group
          (lambda (depth)
            (state-case (c (read-char ip))
              [(#\{)
               (write-char c buf)
               (nested-group (+ depth 1))]
              [(#\})
               (write-char c buf)
               (unless (= depth 0) (nested-group (- depth 1)))]
              [(#\@ #\! #\|)
               (if proper-nesting?
                   (write-char c buf)
                   (input-error "unquoted ~c within nested group in index entry" c))
               (nested-group depth)]
              [(#\")
               (if proper-nesting?
                   (write-char c buf)
                   (state-case (c (read-char ip))
                     [(eof) (input-error "file ended within \\index{}")]
                     [else (write-char c buf)]))
               (nested-group depth)]
              [(#\")
               (write-char c buf)
               (unless proper-nesting?
                 (state-case (c (peek-char ip))
                   [(#\") (read-char ip) (write-char c buf)]
                   [else (void)]))
               (nested-group depth)]
              [else (write-char c buf) (nested-group depth)])))
        (define before@
          (lambda (ls)
            ; ls is list of levels seen so far
            (state-case (c (read-char ip))
              [(#\})
               (let ([s (get-output-string buf)])
                 (values (reverse (cons (cons #f s) ls)) ""))]
              [(#\{)
               (write-char c buf)
               (nested-group 0)
               (before@ ls)]
              [(#\|)
               (let ([s (get-output-string buf)])
                 (values (reverse (cons (cons #f s) ls))
                   (read-bracketed-text ip 1)))]
              [(#\@) (after@ ls (get-output-string buf))]
              [(#\!)
               (let ([s (get-output-string buf)])
                 (before@ (cons (cons #f s) ls)))]
              [(#\")
               (state-case (c (read-char ip))
                 [(eof) (input-error "file ended within \\index{}")]
                 [else
                  (write-char c buf)
                  (before@ ls)])]
              [(#\\)
               (write-char c buf)
               (state-case (c (peek-char ip))
                 [(#\") (read-char ip) (write-char c buf)]
                 [else (void)])
               (before@ ls)]
              [(eof) (input-error "file ended within \\index{}")]
              [else (write-char c buf) (before@ ls)])))
        (define after@
          (lambda (ls sort-key)
            ; ls is list of levels seen so far
            ; sort-key is sort key part of current level
            (state-case (c (read-char ip))
              [(#\})
               (let ([s (get-output-string buf)])
                 (values (reverse (cons (cons sort-key s) ls)) ""))]
              [(#\{)
               (write-char c buf)
               (nested-group 0)
               (after@ ls sort-key)]
              [(#\|)
               (let ([s (get-output-string buf)])
                 (values (reverse (cons (cons sort-key s) ls))
                   (read-bracketed-text ip 1)))]
              [(#\@) (input-error "at sign seen after at sign in \\index{}")]
              [(#\!)
               (let ([s (get-output-string buf)])
                 (before@ (cons (cons sort-key s) ls)))]
              [(#\")
               (state-case (c (read-char ip))
                 [(eof) (input-error "file ended within \\index{}")]
                 [else
                  ; leave out quote; reinsert later
                  (write-char c buf)
                  (after@ ls sort-key)])]
              [(#\\)
               (write-char c buf)
               (state-case (c (peek-char ip))
                 [(#\") (read-char ip) (write-char c buf)]
                 [else (void)])
               (after@ ls sort-key)]
              [(eof) (input-error "file ended within \\index{}")]
              [else (write-char c buf) (after@ ls sort-key)])))
        (before@ '()))))

  ;; support for definitions
  (define-syntax global-def
    (syntax-rules ()
      [(_ name expr)
       (set-def! 'name '() #f expr)]))

  (define set-def!
    (lambda (cmd env conditional? proc)
      (if (null? env)
          (putprop cmd 'def (cons conditional? proc))
          (set-car! env (cons (list* cmd conditional? proc) (car env))))))

  (module (get-def conditional?)
    (define lookup-env
      (lambda (cmd env)
        (cond
          [(null? env) (getprop cmd 'def '(#f . #f))]
          [(assq cmd (car env)) => cdr]
          [else (lookup-env cmd (cdr env))])))

    (define get-def
      (lambda (cmd env)
        (cdr (lookup-env cmd env))))

    (define conditional?
      (lambda (cmd env)
        (car (lookup-env cmd env)))))

  (define (populate-source-directories)
    (let ([inputs (or (getenv "TEXINPUTS") "")])
      (unless (equal? inputs "")
        (let ([ip (open-input-string inputs)] [op (open-output-string)])
          (source-directories
            (let loop ([ls '()])
              (let ([c (read-char ip)])
                (case c
                  [(#\:) (loop (cons (get-output-string op) ls))]
                  [(#!eof) (append (reverse ls) (source-directories))]
                  [else (write-char c op) (loop ls)]))))))))

  (define anchored-filename?
    (lambda (s)
      (and (> (string-length s) 0)
           (memv (string-ref s 0) '(#\/ #\.)))))

  (define find-filename
    (lambda (fn)
      (if (anchored-filename? fn)
          fn
          (ormap
            (lambda (p)
              (let ([path (string-append p "/" fn)])
                (and (file-exists? path) path)))
            (source-directories)))))

  (define open-input-file
    (lambda (fn . flags)
      (import scheme)
      (let ([path (find-filename fn)])
        (unless path
          (errorf #f
            (if (anchored-filename? fn)
                "unable to find file ~a"
                "unable to find file ~a in search path")
            fn))
        (apply open-input-file path flags))))
  )
