#! /usr/bin/scheme --program

;;; scheme-prep.ss
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

;;; primitive commands

;;; \genlab
;;; \generated ... \endgenerated  ; inject and process string returned by enclosed Scheme code
;;; \hindex
;;; \index
;;; \raw
;;; \scheme
;;; \schemeinit ... \endschemeinit
;;; \schemedisplay ... \endschemedisplay
;;; \schemedisplay[number-lines]  ... \endschemedisplay ; insert line numbers
;;; \schemeverbatim ... \endschemeverbatim
;;; \schlbrace
;;; \schrbrace
;;; \transcript ... \endtranscript
;;; \transcript[endtranscript] ... \endtranscript
;;; \var
;;; \xdef
;;; \xedef

;;; \schemeoutput[mode]{filename} ; divert schemedisplay code to file
;;; \schemeoutput{}               ; closes the open scheme output file

;;; commands inserted into the output

;;; \$, \&, \%, \#, \\
;;; \dots
;;; \endschemedisplay
;;; \hindex
;;; \is
;;; \label
;;; \null
;;; \schemeindent
;;; \scheme
;;; \schemeblankline
;;; \schemedisplay
;;; \schemelinestart      ; relies on defn of \schemeindent=
;;; \schatsign
;;; \schbackslash
;;; \schcarat
;;; \schdot
;;; \schlbrace
;;; \schrbrace
;;; \schtilde
;;; \schunderscore
;;; \si
;;;    The transerr, etc. commands require an empty argument so that
;;;    we can put them in a \raw{} in the schemedisplay we generate for
;;;    the transcript and still keep both htmlprep and latex happy with
;;;    the result.  html-prep would also accept {\transin}, but latex
;;;    was not happy with that.
;;; \transerr{} ... \endtranserr{}   ; transcript error msg typesetting
;;; \transin{} ... \endtransin{}     ; transcript user input typesetting
;;; \transout{} ... \endtransout{}   ; transcript program output typesetting
;;; \var
;;; \vdots
;;; any undefined input command
;;; anything else inserted by \raw

;;; Valid within \scheme{...}
;;;   \dots
;;;   \raw{<text>}
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \
;;;   all other characters besides } and newline

;;; Valid within \schemedisplay ... \endschemedisplay
;;;   \dots
;;;   \raw{<text>}
;;;   \var{<text>}
;;;   \var{<text>_<subscript>}
;;;   \vdots
;;;   \
;;;   all other characters besides }
;;;   \label{text} may appear immediately after \schemedisplay

;;;   index and hindex entries follow makeindex 2.13 syntax except that
;;;   the special characters !, @, |, ", and \ are freely allowed within
;;;   embeded {, } pairs in the "actual entry" portion of a level (the
;;;   portion following @, if any).  a modified version of makeindex 2.13
;;;   that supports this extension (with the -d [allow delimited special
;;;   chars] flag) is available here as well.
;;;
;;;   in \index input, we also allow \scheme{...} to appear, with embedded
;;;   special characters.

;;; \index syntax
;;;   \index{levels}
;;;   \index{levels|pageformat}
;;;   levels --> level
;;;          --> level!levels
;;;   level  --> keyentry                     (key and entry same)
;;;   level  --> key@entry                    (separate key and entry)
;;;
;;;   Valid within keyentry
;;;     \scheme{...}

;;; If multiple filenames are supplied on the command line, each file is
;;; processed in the same runtime image.  Thus an \xdef in an earlier file
;;; persists through the processing of subsequent files.

#!chezscheme
(import (except (chezscheme) open-input-file) (dsm) (preplib) (script))

(define use-interaction-window (make-parameter #f))
(define interaction-window-width (make-parameter 28))

(define copy-through-newline
  (lambda (ip op)
    (state-case (c (read-char ip))
      [(#\newline) (write-char c op)]
      [(eof) (void)]
      [else (write-char c op) (copy-through-newline ip op)])))

(define dovar
  (lambda (ip op)
    (read-open-brace ip)
    (display "\\var{" op)
    (let f ()
      (state-case (c (read-char ip))
        [(#\}) (write-char c op)]
        [(#\') (display "$'$" op) (f)]
        [(#\_)
         (display "$_" op)
         (state-case (c (read-char ip))
           [(eof) (input-error "unexpected eof in \\var{}")]
           [(#\{) (fprintf op "{~a}" (read-bracketed-text ip 1))]
           [else (write-char c op)])
         (write-char #\$ op)
         (f)]
        [(eof) (input-error "unexpected eof in \\var{}")]
        [else (write-char c op) (f)]))))
 
(define (sscheme ip op) ; within \scheme
  ; unlike schemedisplay, does not allow { or }.  { and } must be expressed
  ; as \schlbrace and \schrbrace
  (state-case (c (read-char ip))
    [(#\{) (input-error "unexpected { within \\scheme{}")]
    [(#\}) (void)]
    [(#\\)
     ; use read-alpha-command instead of read-command to avoid
     ; improper handling of special characters that follow a slash,
     ; including }
     (let ([cmd (read-alpha-command ip)])
       (case cmd
         [(dots)
          (display "{\\dots}" op)
          (sscheme ip op)]
         [(raw)
          (display (read-bracketed-text ip) op)
          (sscheme ip op)]
         [(var)
          (dovar ip op)
          (sscheme ip op)]
         [(schlbrace)
          (display "\\schlbrace" op)
          (sscheme ip op)]
         [(schrbrace)
          (display "\\schrbrace" op)
          (sscheme ip op)]
         [else ; assume random \ possibly followed by alphabetic chars
          (fprintf op "{\\schbackslash}~a" cmd)
          (sscheme ip op)]))]
    [(#\.) (display "{\\schdot}" op) (sscheme ip op)]
    [(#\~) (display "{\\schtilde}" op) (sscheme ip op)]
    [(#\^) (display "{\\schcarat}" op) (sscheme ip op)]
    [(#\@) (display "{\\schatsign}" op) (sscheme ip op)]
    [(#\_) (display "{\\schunderscore}" op) (sscheme ip op)]
    [(#\space) (display "~" op) (sscheme ip op)]
    [(#\$ #\& #\% #\#) (fprintf op "\\~c" c) (sscheme ip op)]
    [(#\newline) (input-error "line ended within \\scheme{}")]
    [(eof) (input-error "file ended within \\scheme{}")]
    [else (write-char c op) (sscheme ip op)]))

(define sschemedisplay
  (P lambda (number-lines? labels) ; within a schemedisplay
    ; number-lines?
    ;   Flag is true if we want the lines numbered.
    ;
    ; labels
    ;   A list of labels to be attached to this schemedisplay.
    ;   We insert them after the first newline in the schemedisplay
    ;   (or before the \endschemedisplay if no newline) so that the
    ;   label is attached to something other than whitespace (this
    ;   prevents labels from referring to the preceeding page when the
    ;   start of a schemedisplay falls right on a page break).
    (define print-line-start
      (lambda (line op)
        (if number-lines?
            (fprintf op "\\schemelinestartnumbered{~s}~%" line)
            (fprintf op "\\schemelinestart~%"))))
    (fprintf op "\\schemelinestart~%")
    (when sout (newline sout))
    (let loop ([n? #f] [line 0] [labels labels])
      ; n? is true if we've just passed a new line within the display; we use
      ;   it to determine whether to insert \\\schemelinestart.  (We insert
      ;   \schemelinestart after \\ to prevent \\ from sucking up ensuing
      ;   whitespace---including spaces used for indentation on the following
      ;   line.)
      (state-case (c (read-char ip))
        [(#\\)
         ; use read-alpha-command instead of read-alpha-command to avoid
         ; improper handling of special characters that follow a slash
         (let ([cmd (read-alpha-command ip)])
           (case cmd
             [(endschemedisplay)
              (unless (null? labels)
                (for-each (lambda (l) (fprintf op "\\label{~a}" l)) labels))
              (display "\\endschemedisplay" op)]
             [else
              (when n? (display "\\\\\n" op) (print-line-start line op))
              (case cmd
                [(dots)
                 (display "{\\dots}" op)
                 (loop #f line labels)]
                [(var)
                 (dovar ip op)
                 (loop #f line labels)]
                [(raw)
                 (fprintf op "~a" (read-bracketed-text ip))
                 (loop #f line labels)]
                [(vdots)
                 (display "{\\vdots}" op)
                 (loop #f line labels)]
                [else ; assume random \ possibly followed by alphabetic chars
                 (fprintf op "{\\schbackslash}~a" cmd)
                 (when sout (fprintf sout "\\~a" cmd))
                 (loop #f line labels)])]))]
        [else
         (when n? (display "\\\\\n" op) (print-line-start line op))
         (when sout (write-char c sout))
         (state-case (c c)
           [(#\;)
            ; convert ;=> into \is and ;== into \si
            (state-case (c (peek-char ip))
              [(#\=)
               (read-char ip)
               (when sout (write-char c sout))
               (state-case (c (peek-char ip))
                 [(#\>)
                  (when sout (write-char c sout))
                  (read-char ip)
                  (display "\\is" op)
                  (loop #f line labels)]
                 [(#\=)
                  (when sout (write-char c sout))
                  (read-char ip)
                  (display "\\si" op)
                  (loop #f line labels)]
                 [else
                  (when sout (write-char c sout))
                  (display ";=" op)
                  (loop #f line labels)])]
              [(#\-)                   ; should abstract this
               (read-char ip)
               (when sout (write-char c sout))
               (state-case (c (peek-char ip))
                 [(#\>)
                  (when sout (write-char c sout))
                  (read-char ip)
                  (display "\\becomes" op)   ; would prefer something else here
                  (loop #f line labels)]
                 [else
                  (when sout (write-char c sout))
                  (display ";-" op)
                  (loop #f line labels)])]
              [else
               (write-char #\; op)
               (loop #f line labels)])]
           ;; Convert space to ~ since \obeyspaces doesn't seem to work for us
           ;; when \schemedisplay appears within a macro (see f2002/quiz02.stex).
           [(#\space) (display "~" op) (loop #f line labels)]
           [(#\.)
            (display "{\\schdot}" op)
            (loop #f line labels)]
           [(#\~)
            (display "{\\schtilde}" op)
            (loop #f line labels)]
           [(#\^)
            (display "{\\schcarat}" op)
            (loop #f line labels)]
           [(#\@)
            (display "{\\schatsign}" op)
            (loop #f line labels)]
           [(#\_)
            (display "{\\schunderscore}" op)
            (loop #f line labels)]
           [(#\{)
            (display "{\\schlbrace}" op)
            (loop #f line labels)]
           [(#\})
            (display "{\\schrbrace}" op)
            (loop #f line labels)]
           [(#\$ #\& #\% #\#)
            (fprintf op "\\~c" c)
            (loop #f line labels)]
           [(#\newline)
            (unless (null? labels)
              (for-each (lambda (l) (fprintf op "\\label{~a}" l)) labels))
            (state-case (c (peek-char ip))
              [(#\newline)
               (read-char ip)
               (fprintf op "~%\\schemeblankline")
               (print-line-start line op)
               (when sout (newline sout))
               (loop #f (fx+ line 2) '())]
              [else
               (loop #t (fx+ line 1) '())])]
           [(eof) (errorf #f "file ended within schemedisplay")]
           [else (write-char c op) (loop #f line labels)])]))))

(define (sindex ip op)
  ; 1. read entire contents of \index{} form, w/o intepreting \scheme{...}
  ;    separate at !s into 1 or more levels plus page format
  ;    after |, if present; separate levels at @ into sort key
  ;    and text, if present
  ; 2. for each level,
  ;    a. compute output sort key
  ;       - if input sort key is given, use it
  ;       - otherwise use stripped version of input text
  ;       - insert quotes where needed
  ;    b. preprocess input text to produce output text
  ;       - expand \scheme{...}
  ;       - insert quotes where needed
  ; 3. produce output
  ;    a. print \index{
  ;    b. for each level, if output text is same as output sort key,
  ;          print <output text>.
  ;       otherwise print <output text>@<output sort key>
  ;    c. separate levels with !
  ;    d. print |<page format> if present in input
  ;    e. print }
  (define strip-sort-key
    ; presently strips only \scheme{ and matching }
    (let ([buf (open-output-string)])
      (lambda (ip)
        (state-case (c (read-char ip))
          [(#\\)
           (let ([cmd (read-command ip)])
             (case cmd
               [(scheme)
                (read-open-brace ip)
                (display (read-bracketed-text ip 1) buf)
                (strip-sort-key ip)]
               [else (unexpected-command cmd)]))]
          [(eof) (get-output-string buf)]
          [else (write-char c buf) (strip-sort-key ip)]))))
  (call-with-values
    (lambda () (parse-index ip #t))
    (lambda (levels page-format)
      (let ([keys (map (lambda (s) (insert-quotes (open-input-string s) #f))
                       (map (lambda (level)
                              (if (car level)
                                  (car level)
                                  ; strip text to create sort key
                                  (strip-sort-key
                                    (open-input-string (cdr level)))))
                            levels))]
            [texts (map (lambda (level)
                          (insert-quotes
                            (open-input-string
                              (expand-entry (open-input-string (cdr level))))
                          #f))
                        levels)])
        (let f ([keys keys] [texts texts] [delim #\{])
          (unless (null? keys)
            (write-char delim op)
            (let ([key (car keys)] [text (car texts)])
              (if (string=? key text)
                  (display text op)
                  (fprintf op "~a@~a" key text)))
            (f (cdr keys) (cdr texts) #\!)))
        (unless (string=? page-format "") (fprintf op "|~a" page-format))
        (write-char #\} op)))))

(define expand-entry
  ; expands \scheme{} forms
  (let ([buf (open-output-string)])
    (lambda (ip)
      (state-case (c (read-char ip))
        [(#\\)
         (let ([cmd (read-command ip)])
           (case cmd
             [(scheme)
              (read-open-brace ip)
              (display "\\scheme{" buf)
              (sscheme ip buf)
              (write-char #\} buf)
              (expand-entry ip)]
             [else
              (fprintf buf "\\~a" cmd)
              (expand-entry ip)]))]
        [(eof) (get-output-string buf)]
        [else (write-char c buf) (expand-entry ip)]))))

(define insert-quotes
  (let ([buf (open-output-string)])
    ; if proper-nesting? is true, the characters ", @, !, and | lose their
    ; special meaning within nested groups.
    (lambda (ip proper-nesting?)
      (let loop ()
        (state-case (c (read-char ip))
          [(#\" #\@ #\! #\|)
           (write-char #\" buf)
           (write-char c buf)
           (loop)]
          [(#\\)
           (state-case (c (peek-char ip))
             [(#\@ #\! #\|) (write-char #\" buf)]
             [else (void)])
           (write-char c buf)
           (loop)]
          [(#\{)
           (if proper-nesting?
               (fprintf buf "{~a}" (read-bracketed-text ip 1))
               (write-char c buf))
           (loop)]
          [(eof) (get-output-string buf)]
          [else (write-char c buf) (loop)])))))

(define-syntactic-monad P
  ip                   ; current input port
  op                   ; current output port
  ips                  ; input port stack; does not include ip
  ops                  ; output port stack; does not include op
  ifiles               ; stack of input files [(cons ip ips) w/o string ports]
  eofconts             ; stack of continuations to call on eof
  sout                 ; output port for code within schemedisplay or #f
)
 
(define process-string
  (P lambda (s k)
    (P s0
      ([ip (open-input-string s)]
       [ips (cons ip ips)]
       [eofconts (cons (P lambda ()
                         (P k
                           ([op (car ops)] [ops (cdr ops)])
                           (get-output-string op)))
                       eofconts)]
       [op (open-output-string)]
       [ops (cons op ops)]))))

(define s0
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\\)
       (let ([cmd (read-command ip)])
         (cond
           [(get-def cmd '()) =>
            (lambda (proc)
              (unless (or (command-symbol? cmd)
                          (eq? cmd 'schemedisplay)
                          (eq? cmd 'schemeinit)
                          (eq? cmd 'transcript))
                (suppress-white-space ip))
              (P proc))]
           [else (fprintf op "\\~a" cmd) (P s0)]))]
      [(#\%)
       (write-char #\% op)
       (copy-through-newline ip op)
       (P s0)]
      [(eof)
       (close-port ip)
       (if (null? ips)
           (when sout (close-port sout))
           (P (car eofconts)
             ([ip (car ips)]
              [ips (cdr ips)]
              [ifiles (if (eq? ip (car ifiles)) (pop-ifile ifiles) ifiles)]
              [eofconts (cdr eofconts)])))]
      [else (write-char c op) (P s0)])))

;--------------------------------------------------------------------------

(define go
  (lambda (fn)
    (let ([ip (open-input-file (format "~a.stex" fn))])
      (let ([op (open-output-file (format "~a.tex" fn) 'replace)])
        (fprintf op "%%% DO NOT EDIT THIS FILE~%")
        (fprintf op "%%% Edit the .stex version instead~%~%")
        ; preplib parameters
        (parameterize ([current-ifile #f]
                       [genlab-prefix "s"]
                       [genlab-counters '()])
          (P s0
            ([ip ip]
             [op op]
             [ips '()]
             [ops '()]
             [ifiles (push-ifile ip '())]
             [eofconts (list s0)]
             [sout #f]))
          (close-port op))))))

(global-def genlab
  (P lambda ()
    (display (genlab) op)
    (P s0)))

(global-def hindex
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (lab)
        (fprintf op "\\hindex{~a}" lab)
        (read-open-brace ip)
        (sindex ip op)
        (P s0)))))

(global-def index
  (P lambda ()
    (let ([lab (genlab)])
      (fprintf op "\\label{~a}\\hindex{~a}" lab lab))
    (read-open-brace ip)
    (sindex ip op)
    (P s0)))

(global-def raw
  (P lambda ()
    (display (read-bracketed-text ip) op)
    (P s0)))

(global-def scheme
  (P lambda ()
    (read-open-brace ip)
    (display "\\scheme{" op)
    (sscheme ip op)
    (write-char #\} op)
    (P s0)))

(global-def schemedisplay
  (P lambda ()
    (display "\\schemedisplay\n" op)
    (let loop ([options '()] [labels '()])
      ; currently a bit sloppy on the grammar
      (state-case (c (read-char ip))
        [(#\\)
        ; use read-alpha-command instead of read-command to avoid
        ; improper handling of special characters that follow a slash
         (let ([cmd (read-alpha-command ip)])
           (case cmd
             [(label)
              (loop options (cons (read-bracketed-text ip) labels))]
             [(raw)
              (fprintf op "\\raw{~a}~%" (read-bracketed-text ip))
              (loop options labels)]
             [else
              (input-error "invalid command \\~s following \\schemedisplay" cmd)]))]
        [(#\newline)
         (P sschemedisplay () (memq 'number-lines options) labels)]
        [(#\[)
         (unread-char c ip)
         (let ([opt (read-optional-arg ip)])
           (if (equal? opt "number-lines")
               (loop (cons 'number-lines options) labels)
               (input-error "unexpected optional argument ~a to \\schemedisplay" opt)))]
        [else (input-error "expected newline after \\schemedisplay")]))
    (P s0)))

(global-def schemeoutput
  (P lambda ()
    (let* ([mode (if (equal? (read-optional-arg ip) "append") 'append 'replace)]
           [filename (read-bracketed-text ip)])
       (when sout (close-port sout))
       (if (eq? filename "")
           (P s0 ([sout #f]))
           (P s0 ([sout (open-output-file filename mode)]))))))

(global-def var
  (P lambda ()
    (display "\\scheme{" op)
    (dovar ip op)
    (write-char #\} op)
    (P s0)))

(global-def def
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\def syntax")])]
           [pattern (read-def-pattern ip)]
           [template (read-bracketed-text ip)])
      (fprintf op "\\def\\~a" cmd)
      (for-each
        (lambda (x)
          (cond
            [(char? x) (write-char x op)]
            [(number? x) (fprintf op "#~a" x)]
            [else (errorf #f "unexpected parsed pattern element ~s" x)]))
        pattern)
      (fprintf op "{~a}" template)
      (P s0))))

(global-def xdef
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\xdef syntax")])]
           [pattern (read-def-pattern ip)]
           [template (read-bracketed-text ip)])
      (set-def! cmd '() #f
        (P lambda ()
          (P s0
            ([ip (open-input-string
                   (expand-template template
                     (read-args ip pattern cmd) cmd))]
             [ips (cons ip ips)]
             [eofconts (cons s0 eofconts)]))))
      (P s0))))

(global-def xedef
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\xedef syntax")])]
           [pattern (read-def-pattern ip)]
           [template (read-bracketed-text ip)])
      (P process-string () template
        (P lambda (template)
          (set-def! cmd '() #f
            (P lambda ()
              (P s0
                ([ip (open-input-string
                       (expand-template template
                         (read-args ip pattern cmd) cmd))]
                 [ips (cons ip ips)]
                 [eofconts (cons s0 eofconts)]))))
          (P s0))))))

(global-def schemeverbatim
  (P lambda ()
    (display "\\begin{verbatim}\n" op)
    (let f ()
      (state-case (c (read-char ip))
        [(#\\) 
         (let ([cmd (read-command ip)])
           (case cmd
             [(endschemeverbatim) (void)]
             [else (write-char c op) (display cmd op) (f)]))]
        [(eof) (unexpected-eof "after \\schemeverbatim")]
        [else (write-char c op) (f)]))
    (display "\\end{verbatim}\n" op)
    (P s0)))

(global-def timestamp
  (P lambda ()
    (display (date-and-time) op)
    (P s0)))

;--------------------------------------------------------------------------
(let ()

  (define-syntax define-enumeration
    (lambda (x)
      (define iota
        (case-lambda
          [(n) (iota 0 n)]
          [(i n) (if (= i n) '() (cons i (iota (+ i 1) n)))]))
      (syntax-case x ()
        [(_ name (enum ...))
         (and (identifier? #'name) (andmap identifier? #'(enum ...)))
         (with-syntax ([len (length #'(enum ...))])
           (with-syntax ([(i ...) (iota #'len)])
             #'(begin (define enum i) ... (define name len))))])))

  (define-record tag (markup entity))
  
  (define make-tag-port
    (lambda (ip)
     ; To preserve the indentation of Scheme expressions in the .stex source,
     ; we have to add the leading whitespace the user would have entered to
     ; compensate for the indentation---due to the prompt---of the first line.
     ; We compute the current indentation level when writing non-echo output.
     ; We indent subsequent input lines after we read the first line that
     ; follows some output.
      (define-enumeration num-modes (writing reading indenting))
      (define mode writing)
      (define indent 0)
      (define adjust-indent  ; called by output routine
        (lambda (x)
          (define do-char
            (lambda (c)
              (set! indent (if (char=? c #\newline) 0 (fx+ indent 1)))))
          ; shouldn't do this if we're printing on behalf of the repl
          (begin ; unless (use-interaction-window)
            (unless (fx= mode writing) (set! indent 0))
            (set! mode writing)
            (if (char? x)
                (do-char x)
                (let ([len (string-length x)])
                  (do ([i 0 (fx+ i 1)]) ((fx= i len))
                    (do-char (string-ref x i))))))))
      (define whitespace?
        (lambda (x)
          (if (char? x)
              (char-whitespace? x)
              (let loop ([i (fx- (string-length x) 1)])
                (or (fx< i 0)
                    (and (char-whitespace? (string-ref x i))
                         (loop (fx- i 1))))))))
      (let ([line '()] [bline '()] [buffer '()] [ip ip])
        (define (handler msg . args)
          (record-case (cons msg args)
            [char-ready? (p) 
             (critical-section
               (or (not (null? line)) (char-ready? ip)))]
            [peek-char (p)
             (define read-line
               (lambda ()
                 (when (fx= mode writing) (set! mode reading))
                 (when (fx= mode indenting)
                   (unless (fx= indent 0)
                     (set! buffer
                       (cons (make-tag #f (make-string indent #\space))
                             buffer))))
                 (let loop ()
                   (let ([c (read-char ip)])
                     (if (eof-object? c)
                         '()
                         (let ([x (make-tag #f c)])
                           (set! buffer (cons x buffer))
                           (if (eqv? c #\newline)
                               (begin (set! mode indenting) (list x))
                               (cons x (loop)))))))))
             (critical-section
               (if (null? line)
                   (let ([old bline])
                     (set! line (read-line))
                     (set! bline '())
                     (if (null? line)
                         (begin
                           ; When the last expression in a transcript calls read
                           ; and then prints something in the transcript window,
                           ; we have to be sure that the newline character that
                           ; sent the line of buffered text is properly tagged as
                           ; having been read via the transcript input port since
                           ; read simply peeks at the whitespace character and the
                           ; subsequent repl read (returning #!eof) will usurp the
                           ; newline unless we patch it up here.
                           (when (and (> (length old) 1)
                                      (eqv? (tag-entity (car old)) #\newline)
                                      (eq? (tag-markup (cadr old)) 'transin))
                             (set-tag-markup! (car old) 'transin))
                           #!eof)
                         (tag-entity (car line))))
                   (tag-entity (car line))))]
            [unread-char (c p)
             (critical-section
               (when (null? bline)
                 (errorf 'unread-char "too many consecutive unreads from ~s" p))
               (let ([x (car bline)])
                 (set! bline (cdr bline))
                 (set-tag-markup! x #f)
                 (set-tag-entity! x c)
                 (set! line (cons x line))))]
            [clear-input-port (p)
             (critical-section
               (set! line '())
               (set! bline '()))]
            [clear-output-port (p) (void)]
            [close-port (p) (mark-port-closed! p)]
            [flush-output-port (p) (void)]
            [file-position (p . pos)
             (if (null? pos)
                 (most-negative-fixnum)
                 (errorf 'transcript-port "cannot reposition"))]
            [port-name (p) "tag-port"]
            ; intentionally missing block-read and read-char
            [tag-read-char (markup p)
             (critical-section
               (let ([c (peek-char p)])
                 (unless (null? line)
                   (let ([x (car line)])
                     (set! line (cdr line))
                     (set-tag-markup! x markup)
                     (set! bline (cons x bline))))
                 c))]
            [tag-write-entity (markup x p)
             (critical-section
               (adjust-indent x)
               (set! buffer (cons (make-tag markup x) buffer)))]
            [tag-get-repl-interaction-strings ()
             (let ([repl (open-output-string)] [intr (open-output-string)])
               (define maxcols (interaction-window-width))
               (define col 0)
               (define col-display
                 (lambda (entity p)
                   (define col-write-char
                     (lambda (c p)
                       (cond
                         [(char=? c #\newline) (set! col 0)]
                         [(= col maxcols) (newline p) (set! col 0)])
                       (write-char c p)
                       (set! col (+ col 1))))
                   (if (char? entity)
                       (col-write-char entity p)
                       (for-each (lambda (c) (col-write-char c p)) (string->list entity)))))
               (define choose-port
                 (lambda (mkup)
                   (case mkup
                     [(#f transerr traceout) repl]
                     [(transin transout) intr]
                     [else (errorf 'scheme-prep.ss "unexpected markup ~s" mkup)])))
               (define reclaim-first-newline
                 ; the newline that we entered after the expression in the repl window should
                 ; be charged to the repl window, not the interaction window.
                 ; (misattributed as being read by transin since the repl's read just peeks at
                 ;  the newline, and subsequent read during eval phase claims the newline)
                 (lambda (ls)
                   (let f ([ls ls])
                     (if (null? ls)
                         '()
                         (let ([x (car ls)])
                           (case (tag-markup x)
                             [(transin) (when (eqv? (tag-entity x) #\newline) (set-tag-markup! x #f))]
                             [(transout) (void)]
                             [else (f (cdr ls))]))))
                   ls))
               (let loop ([ls (reclaim-first-newline (reverse buffer))] [markup #f])
                 (if (null? ls)
                     (begin
                       (set! buffer '())
                       (when markup (fprintf (choose-port markup) "\\raw{\\end~s{}}" markup))
                       (values
                         (get-output-string repl)
                         (let ([x (get-output-string intr)])
                           (if (eq? x "") #f x))))
                     (let ([x (car ls)])
                       (let ([new (tag-markup x)] [entity (tag-entity x)])
                         (unless (eq? new markup)
                           (when markup (fprintf (choose-port markup) "\\raw{\\end~s{}}" markup))
                           (when new (fprintf (choose-port new) "\\raw{\\~s{}}" new)))
                         (let ([p (choose-port new)])
                           (if (eq? p repl)
                               (display entity p)
                               (col-display entity p)))
                         (loop (cdr ls) new))))))]
            [tag-get-output-string ()
             (let ([op (open-output-string)])
               (let loop ([ls (reverse buffer)] [markup #f])
                 (if (null? ls)
                     (begin
                       (set! buffer '())
                       (when markup (fprintf op "\\raw{\\end~s{}}" markup))
                       (get-output-string op))
                     (let ([x (car ls)])
                       (let ([new (tag-markup x)] [entity (tag-entity x)])
                         (unless (eq? new markup)
                           (when markup (fprintf op "\\raw{\\end~s{}}" markup))
                           (when new (fprintf op "\\raw{\\~s{}}" new)))
                         (display entity op)
                         (loop (cdr ls) new))))))]
            [delete-last-line-if-unmarked ()
             ; clean up the "\n> \n" at the end of the transcript
             ; by deleting everything from the last newline upto, but
             ; not including the second newline.
             (let trim ([ls buffer] [n? #f])
               (if (or (null? ls) (tag-markup (car ls)))
                   (set! buffer ls)
                   (let foo ([first (tag-entity (car ls))])
                     (cond
                       [(string? first)
                        (let scan ([i (- (string-length first) 1)] [n? n?])
                          (cond
                            [(fx< i 0) (trim (cdr ls) n?)]
                            [(char=? (string-ref first i) #\newline)
                             (if (not n?)
                                 (scan (fx- i 1) #t)
                                 (begin
                                   (set-tag-entity!
                                     (car ls)
                                     (substring first 0 i))
                                   (set! buffer ls)))]
                            [else (scan (fx- i 1) n?)]))]
                       [(char=? first #\newline)
                        (if (not n?)
                            (trim (cdr ls) #t)
                            (set! buffer (cdr ls)))]
                       [else (trim (cdr ls) n?)]))))
             ; trim the last newline at the end of the transcript
             ; since transcript env will insert some vspace of its own
             (unless (null? buffer)
               (let ([first (car buffer)])
                 (when (eqv? (tag-entity first) #\newline)
                   (set! buffer (cdr buffer)))))]
            [else (errorf 'tag-port "operation ~s not handled" msg)]))
        (make-input/output-port handler "" ""))))
  
  (define make-markup-port
    (lambda (tag-p imarkup omarkup)
      (define (handler msg . args)
        (record-case (cons msg args)
          [block-read (p str cnt)
           (critical-section
             (let ([c (peek-char p)])
               (if (eof-object? c)
                   c
                   (if (= cnt 0)
                       0
                       (begin
                         (read-char p)
                         (string-set! str 0 c)
                         1)))))]
          [char-ready? (p) (char-ready? tag-p)]
          [clear-input-port (p) (clear-input-port tag-p)]
          [clear-output-port (p) (clear-output-port tag-p)]
          [close-port (p) (mark-port-closed! p) (close-port tag-p)]
          [flush-output-port (p) (flush-output-port tag-p)]
          [file-position (p . pos)
           (if (null? pos)
               (most-negative-fixnum)
               (errorf 'transcript-port "cannot reposition"))]
          [port-name (p) "markup"]
          [peek-char (p) (peek-char tag-p)]
          [read-char (p) ((port-handler tag-p) 'tag-read-char imarkup tag-p)]
          [unread-char (c p) (unread-char c tag-p)]
          [write-char (c p) ((port-handler tag-p) 'tag-write-entity omarkup c tag-p)]
          [block-write (p str cnt)
           ((port-handler tag-p)
              'tag-write-entity
              omarkup
              (substring str 0 cnt)
              tag-p)
           cnt]
          [else (errorf 'markup-port "operation ~s not handled" msg)]))
      (make-input/output-port handler "" "")))

  (define scheme-transcript
    (P lambda (console-input labels)
      (let ([tp (make-tag-port (open-input-string console-input))])
        (let ([normal (make-markup-port tp #f #f)]
              [trace  (make-markup-port tp #f 'traceout)]
              [effect (make-markup-port tp 'transin 'transout)]
              [errors (make-markup-port tp #f 'transerr)]
              [handler (port-handler tp)])
          (handler 'tag-write-entity #f "\\schemedisplay" tp)
          (for-each
            (lambda (l)
              (handler 'tag-write-entity #f (format "\\label{~a}" l) tp))
            labels)
          (handler 'tag-write-entity #f "\n" tp)
          (parameterize ([console-input-port normal]
                         [console-output-port normal]
                         [console-error-port normal]
                         [current-input-port effect]
                         [current-output-port effect]
                         [current-error-port effect]
                         [trace-output-port trace])
            (new-cafe
              (lambda (x)
                (with-exception-handler default-exception-handler
                  (lambda ()
                    (eval x))))))
          ; We could do this with string bashing.  I modified the port.
          (handler 'delete-last-line-if-unmarked tp)
          (handler 'tag-write-entity #f "\\endschemedisplay\n" tp)
          (if (use-interaction-window)
              (let-values ([(repl interaction) (handler 'tag-get-repl-interaction-strings tp)])
                (P s0
                  ([ip
                    (if (not interaction)
                        (open-input-string repl)
                        (open-input-string
                          (string-append
                            "\\startrepl{}" repl
                            "\\endrepl{}\\startinteraction{}\\schemedisplay\n"
                            interaction
                            "\\endschemedisplay\\endinteraction{}")))]
                   [ips (cons ip ips)]
                   [eofconts (cons s0 eofconts)])))
              (let ([transcript (handler 'tag-get-output-string tp)])
                (P s0
                  ([ip (open-input-string transcript)]
                   [ips (cons ip ips)]
                   [eofconts (cons s0 eofconts)]))))))))

   (define make-string-evaluator
     (lambda (start-mark evaluator)
       (P lambda ()
         (define text-upto
           (let ([op (open-output-string)])
             (lambda (endmark)
               (state-case (c (read-char ip))
                 [(#\\)
                  (let ([mk (read-alpha-command ip)])
                    (if (eq? mk endmark)
                        (get-output-string op)
                        (begin
                          (write-char #\\ op)
                          (display mk op)
                          (text-upto endmark))))]
                 [(eof) (unexpected-eof (format "after \\~a" start-mark))]
                 [else
                  (write-char c op)
                  (text-upto endmark)]))))
         (let loop ([endmark #f] [labels '()])
           (state-case (c (read-char ip))
             [(#\[) ;]
              (when endmark
                (input-error "expected newline after \\~a[~a]" start-mark endmark))
              (unread-char c ip)
              (let ([ls (string->list (read-optional-arg ip))])
                (when (null? ls)
                  (input-error "empty optional argument to \\~a" start-mark))
                (when (not (char=? (car ls) #\\))
                  (input-error "\\~a terminator must begin with \\" start-mark))
                (let ([rest (cdr ls)])
                  (when (or (null? rest) (not (andmap char-alphabetic? (cdr ls))))
                    (input-error
                      "\\~a terminator must be \\ followed by alphabetic character(s)" start-mark))
                  (loop (string->symbol (list->string rest)) labels)))]
             [(#\\)
              ; currently a bit sloppy on the grammar
              ; could require all labels one after the other
              ; use read-alpha-command instead of read-command to avoid
              ; improper handling of special characters that follow a slash
               (let ([cmd (read-alpha-command ip)])
                 (case cmd
                   [(label)
                    (loop endmark (cons (read-bracketed-text ip) labels))]
                   [else
                    (input-error "invalid command \\~s following \\~a" cmd start-mark)]))]
             [(#\newline)
              (let ([endmark
                     (or endmark
                         (string->symbol (string-append "end" start-mark)))])
                (P evaluator () (text-upto endmark) endmark (reverse labels)))]
             [(#\space #\tab) (loop endmark labels)]
             [else
              (let ([txt (format "expected newline after \\~a" start-mark)])
                (input-error
                  (if endmark
                      (string-append txt (format "[~a]" endmark))
                      txt)))])))))

  (global-def schemeinit
    ; whitespace has not suppressed when we are invoked
    (make-string-evaluator "schemeinit"
      (P lambda (s endmark labels)
        (unless (null? labels) (input-error "what on earth do you mean putting \\label after \\schemeinit ???"))
        ; We could perhaps make this easier by not trying to share the
        ; make-string-evaluator code with \transcript and instead let
        ; the code simply read from the true ip instead, so that input-error
        ; does its job without our assistance.  Unfortunately, I just thought
        ; of that and don't have the heart to do any more than document the
        ; possibility.
        (let ([real-abort-handler (abort-handler)])
          (let ([sip (open-input-string s)])
            ; nesting is not an issue.
            (let f ()
              ; scan ahead to first non-whitespace character, if any
              (let skip-whitespace ()
                (let ([c (peek-char sip)])
                  (when (and (char? c) (char-whitespace? c))
                    (read-char sip)
                    (skip-whitespace))))
              ; record the current file position so we adjust the true
              ; file-position based on the string-input-port's file position
              ; if we run into an error
              (let ([last-sip-pos (file-position sip)])
                (define scout ; be prepared
                  (lambda (thunk)
                    (parameterize
                      ([abort-handler
                        (lambda args
                          (let ([end (file-position ip)]
                                [endmark-len
                                 (+ 1
                                    (string-length (symbol->string endmark)))])
                            (file-position
                              ip
                              (- end
                                 endmark-len
                                 (- (string-length s) last-sip-pos))))
                          (parameterize ([abort-handler real-abort-handler])
                            (input-error
                              "previous error occurred within \\schemeinit")))])
                      (thunk))))
                (let ([x (scout (lambda () (read sip)))])
                  (unless (eof-object? x)
                    (scout (lambda () (eval x)))
                    (f)))))))
        (P s0 ()))))

  (global-def generated
    (P lambda ()
      (let ([sop (open-output-string)])
        (let loop ()
          (state-case (c (peek-char ip))
            [(#\space #\tab #\newline) (read-char ip) (loop)]
            [(#\\)
             (read-char ip)
             (let ([cmd (read-alpha-command ip)])
               (case cmd
                 [(endgenerated)
                  (P s0 ([ip (open-input-string (get-output-string sop))]
                         [ips (cons ip ips)]
                         [eofconts (cons s0 eofconts)]))]
                 [else (input-error "unexpected command \\~s following \\generated" cmd)]))]
            [else
             (parameterize ([current-output-port sop])
               (eval (read ip))) ; should protect against errors
             (loop)])))))

  (global-def transcript
    ; whitespace has not suppressed when we are invoked
    (make-string-evaluator "transcript"
      (P lambda (s endmark labels)
        (P scheme-transcript () s labels))))

  (global-def useinteractionwindow
    (P lambda ()
      (use-interaction-window #t)
      (P s0 ())))

  (global-def enduseinteractionwindow
    (P lambda ()
      (use-interaction-window #f)
      (P s0 ())))

  (global-def interactionwindowwidth
    (P lambda ()
      (let ([s (read-bracketed-text ip)])
        (let ([n (string->number s)])
          (unless n (errorf #f "expected numeric argument to \\interactionwindowwidth"))
          ; should be more clever here to make it work in bgroup ... egroup fashion
          (interaction-window-width n)
          (P s0 ())))))

)

(populate-source-directories)

(command-line-case (command-line)
  [((keyword --help)) (usage)]
  [(filename* ...)
   (for-each go
     (let ([found (find-filename "scheme-prep.tex")])
       (if found
           (cons found filename*)
           filename*)))])
