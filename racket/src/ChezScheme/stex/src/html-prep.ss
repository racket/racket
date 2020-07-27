#! /usr/bin/scheme --program

;;; html-prep.ss
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

;;; TODO
;;; * fix paragraph handling
;;; * verify compliance of generated documents
;;; * resolve multicolumn bug --- see ./multicolumn-bug.stex
;;;    - I commented out lots of stuff that Kent added and it seems to
;;;      work for the one example I care about at the moment (right now
;;;      I'm just trying to get Assignment 14 out by hook or by crook)
;;; * move stuff now unique to html-prep.ss from preplib.ss
;;; * add ability to insert arbitrary content into header
;;; * update documentation

;;; html-prep now consults the TEXINPUTS environment variable to
;;; locate .hcls files.

;;; primitive commands (latex standard first, then extensions)
;;;   &
;;;   \&
;;;   ~
;;;   -- (en-dash)
;;;   --- (em-dash)
;;;   \'e, \'o
;;;   \"{<letter>}
;;;   \c{<letter>}
;;;   \\[ignored]
;;;   \<space>
;;;   $ ... $                                 (see below)
;;;   { ... }                                 (groups)
;;;   \begingroup ... \endgroup
;;;   \bgroup ... \egroup
;;;   %<comment>
;;;   \[ ... \]                               (see below)
;;;   \begin{eqnarray*} ... \end{eqnarray*}   (see below)
;;;   \begin{divertoutput}[i] ... \end{divertoutput}
;;;      i selects a currently opened file:
;;;          0 first file opened, 1 second file, ...
;;;          or -1 most recent, -2 next most recent, ...
;;;   \begin{verbatim} ... \end{verbatim}
;;;   \bf
;;;   \bibitem
;;;   \cite{...}
;;;   \emph{...}
;;;   \epsfbox{...}
;;;   \genlab
;;;   \hindex{label}{stuff}                   (see below)
;;;   \include{filename}
;;;   \input{filename}
;;;   \index{stuff}                           (see below)
;;;   \it
;;;   \label{...}
;;;   \pagebreak[n]
;;;   \pageref[class]{...}
;;;   \ref[class]{...}
;;;   \usepackage{...}   ; implicit .hsty suffix
;;;   \vskip                                  (remainder of line ignored!)
;;;   \def
;;;   \newenvironment
;;;   \renewenvironment
;;;   \newif
;;;   \newcounter
;;;   \setcounter
;;;   \addtocounter
;;;   \stepcounter
;;;   \tt
;;;   \closehtmlfile
;;;   \closerawfile
;;;   \currentoutputfile
;;;   \headerstuff{stuff}                    (raw stuff between <head></head>
;;;   \documenttitle[formats]{title}
;;;   \hpageref[class]{label}{text}          (pageref link around text)
;;;   \href[class]{label}{text}              (ref link around text)
;;;   \hardspaces
;;;   \rawinput{filename}                    (inserts raw html from filename)
;;;   \makeindex
;;;   \openhtmlfile
;;;   \openrawfile
;;;   \raw{...}
;;;   \geq{...}
;;;   \leq{...}
;;;   \neq{...}
;;;   \equiv{...}
;;;   \LARGE
;;;   \Large
;;;   \large
;;;   \small
;;;   \footnotesize
;;;   \tiny

;;; Within $ ... $, \[ ... \], \begin{eqnarray*} ... \end{eqnarray*}
;;;   user-defined macros are not presently expanded

;;; \index and \hindex syntax
;;;   \index{levels} or \index{levels|pageformat}
;;;   \hindex{label}{levels} or \hindex{label}{levels|pageformat}
;;;   levels --> level
;;;          --> level!levels
;;;   level  --> keyentry                     (key and entry same)
;;;   level  --> key@entry                    (separate key and entry)

;;;   index and hindex entries follow makeindex 2.13 syntax except that
;;;   the special characters !, @, |, ", and \ are freely allowed within
;;;   embeded {, } pairs in the "actual entry" portion of a level (the
;;;   portion following @, if any).  a modified version of makeindex 2.13
;;;   that supports this extension (with the -d [allow delimited special
;;;   chars] flag) is available here as well.

#!chezscheme
(import (except (chezscheme) open-input-file) (dsm) (preplib) (script))

(define math-directory (make-parameter "math"))

(define push-ofile
  (lambda (op ofiles)
    (current-ofile op)
    (cons op ofiles)))

(define pop-ofile
  (lambda (ofiles)
    (let ([ofiles (cdr ofiles)])
      (current-ofile (and (not (null? ofiles)) (car ofiles)))
      ofiles)))

(define get-counter-value
  (lambda (count)
    (getprop count 'counter #f)))

(define set-counter-value!
  (lambda (count value)
    (putprop count 'counter value)))

(define add-subcounter!
  (lambda (counter subcounter)
    (putprop counter 'subcounters
      (cons subcounter (getprop counter 'subcounters '())))))

(define subcounters
  (lambda (counter)
    (getprop counter 'subcounters '())))

(define tex-file-name
  (lambda (fn)
    (if (or (string=? (format "~a.tex" (path-root fn)) fn)
            (file-exists? fn))
        fn
        (format "~a.tex" fn))))

(define open-html-file
  (lambda (ip title)
    (define next-count
      (lambda (root)
        (cond
          [(assoc root (output-file-counters)) =>
           (lambda (a)
             (let ([n (+ (cdr a) 1)])
               (set-cdr! a n)
               n))]
          [else
           (output-file-counters
             (cons (cons root 0) (output-file-counters)))
           0])))
    ; generate sequence of names foo.html, foo_1.html, foo_2.html, ...
    (let ([op (let ([fn (let ([root (path-root (port-name (current-ifile)))])
                          (let ([n (next-count root)])
                            (if (= n 0)
                                (format "~a.html" root)
                                (format "~a_~d.html" root n))))])
                (open-output-file fn 'replace))])
      (fprintf op "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n  \"http://www.w3.org/TR/html4/loose.dtd\">")
      (fprintf op "<!-- DO NOT EDIT THIS FILE-->~%")
      (fprintf op "<!-- Edit the .tex version instead-->~%~%")
      (fprintf op "<html>~%")
      (fprintf op "<head>~%<title>~a</title>~%" title)
      (when (header-stuff) (display (header-stuff) op))
      (when (style-sheet)
        (fprintf op "<link href=\"~a\" rel=\"stylesheet\" type=\"text/css\">\n"
          (style-sheet)))
      (fprintf op "</head>~%<body>~%")
      op)))

(define close-html-port
  (lambda (p)
    (fprintf p "</body>~%")
    (fprintf p "</html>~%")
    (close-output-port p)))

(define get-cite
  (lambda (key)
    (or (getprop key 'cite)
        (begin
           (warningf #f "bib entry ~a not found" key)
           "???"))))

(define put-label!
  (lambda (name type value)
    (putprop name type value)))

(define get-label
  (lambda (name type)
    (or (getprop name type)
        (begin
          (warningf #f "label (~a) ~a undefined" type name)
          "???"))))

(define read-aux-file
  (lambda (fn)
    (let ([ip (open-input-file fn)])
      (parameterize ([current-ifile ip])
        (let loop ([newline? #f])
          (state-case (c (read-char ip))
            [(#\newline) (loop #t)]
            [(#\\)
             (when newline?
               (let ([cmd (read-command ip)])
                 (case cmd
                   [(|@|)
                    (let ([cmd (read-command ip)])
                      (case cmd
                        [(input)
                         (read-aux-file (read-bracketed-text ip))]))]
                   [(bibcite)
                    (let* ([key (read-bracketed-text ip)]
                           [cite (read-bracketed-text ip)])
                      (putprop (string->symbol key) 'cite cite))]
                   [(newlabel)
                    (let* ([name (read-bracketed-text ip)]
                           [ref (begin (read-open-brace ip)
                                       (read-bracketed-text ip))]
                           [pageref (read-bracketed-text ip)])
                      (put-label! (string->symbol name) 'pageref pageref))])))
             (loop #f)]
            [(eof) (close-port ip)]
            [else (loop #f)]))))))

;; structures
(define-structure (index-entry url keys texts pageno pageformat prefix))

(define s$
  (lambda (ip op) ; within $...$
    (let ([s (let ([buf (open-output-string)])
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\$) (get-output-string buf)]
                   [(eof) (unexpected-eof "within $ ... $")]
                   [(#\newline) (write-char #\space buf) (loop)]
                   [else (write-char c buf) (loop)])))])
      (emit-math s op))))

(define (scomment ip op hard-spaces)
  (state-case (c (read-char ip))
    [(eof #\newline)
     (unless hard-spaces
       (let eat-spaces ()
         (state-case (c (peek-char ip))
           [(#\space #\tab) (read-char ip) (eat-spaces)]
           [else (void)])))]
    [else (scomment ip op hard-spaces)]))

(define seqnarray*
  (lambda (ip op) ; within \begin{eqnarray*} ... \end{eqnarray*}
    (let ([s (let ([buf (open-output-string)])
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\\)
                    (let ([cmd (read-command ip)])
                      (case cmd
                        [(end)
                         (read-open-brace ip)
                         (if (equal? (read-bracketed-text ip 1) "eqnarray*")
                             (get-output-string buf)
                             (input-error "expected \\end{eqnarray*}"))]
                        [else
                         (fprintf buf "\\~a" cmd)
                         (loop)]))]
                   [(eof) (unexpected-eof "within eqnarray*")]
                   [else (write-char c buf) (loop)])))])
      (punt-to-latex (format "\\begin{eqnarray*}~a\\end{eqnarray*}" s) op))))

(define smathdisplay
  (lambda (ip op) ; within \[ ... \]
    (let ([s (let ([buf (open-output-string)])
               (let loop ()
                 (state-case (c (read-char ip))
                   [(#\\)
                    (let ([cmd (read-command ip)])
                      (case cmd ;[
                        [(|]|) (get-output-string buf)]
                        [else
                         (fprintf buf "\\~a" cmd)
                         (loop)]))]
                   [(eof) (unexpected-eof "within \\[ ... \\]")]
                   [else (write-char c buf) (loop)])))])
      (emit-math s op))))

(define emit-math
  (lambda (s op)
    (cond
      [(htmlmath (open-input-string s) #f) =>
       (lambda (html) (display html op))]
      [else (punt-to-latex (format "$~a$" s) op)])))

(define htmlmath
  (lambda (ip compact?)
    (let ([buf (open-output-string)])
      (let loop ([unary? #t])
        (state-case (c (read-char ip))
          [(#\\)
           (let ([cmd (read-command ip)])
             (case cmd
               [(|{|)
                (fprintf buf "~a" cmd)
                (loop #t)]
               [(|}|)
                (fprintf buf "~a" cmd)
                (loop #f)]
               [(|,| |;|)
                (loop unary?)]
               [(cdot)
                (fprintf buf (if (or compact? unary?) "&middot;" " &middot; "))
                (loop #t)]
               [(dots)
                (fprintf buf "...")
                (loop #f)]
               [(gt)
                (fprintf buf (if (or compact? unary?) "&gt;" " &gt; "))
                (loop #t)]
               [(lt)
                (fprintf buf (if (or compact? unary?) "&lt;" " &lt; "))
                (loop #t)]
               [(ge geq)
                (fprintf buf (if (or compact? unary?) "&ge;" " &ge; "))
                (loop #t)]
               [(le leq)
                (fprintf buf (if (or compact? unary?) "&le;" " &le; "))
                (loop #t)]
               [(ne neq)
                (fprintf buf (if (or compact? unary?) "&ne;" " &ne; "))
                (loop #t)]
               [(equiv)
                (fprintf buf (if (or compact? unary?) "&equiv;" " &equiv; "))
                (loop #t)]
               [(log)
                (fprintf buf "log")
                (loop #t)]
               [(pm)
                (fprintf buf (if (or compact? unary?) "&plusmn;" " &plusmn; "))
                (loop #t)]
               [(div)
                (fprintf buf (if (or compact? unary?) "&divide;" " &divide; "))
                (loop #t)]
               [(times)
                (fprintf buf (if (or compact? unary?) "&times;" " &times; "))
                (loop #t)]
               [else #f]))]
          [(#\{) (loop unary?)]
          [(#\}) (loop unary?)]
          [(#\_)
           (state-case (c (read-char ip))
             [(#\{)
              (cond
                [(htmlmath (open-input-string (read-bracketed-text ip 1)) #t) =>
                 (lambda (html)
                   (fprintf buf "<sub>~a</sub>" html)
                   (loop #f))]
                [else #f])]
             [(eof) #f]
             [else (fprintf buf "<sub>~a</sub>" c) (loop #f)])]
          [(#\^)
           (state-case (c (read-char ip))
             [(#\{)
              (cond
                [(htmlmath (open-input-string (read-bracketed-text ip 1)) #t) =>
                 (lambda (html)
                   (fprintf buf "<sup>~a</sup>" html)
                   (loop #f))]
                [else #f])]
             [(eof) #f]
             [else (fprintf buf "<sup>~a</sup>" c) (loop #f)])]
          [((#\a - #\z) (#\A - #\Z))
           (fprintf buf "<i>")
           (let loop ([c c])
             (write-char c buf)
             (state-case (c (peek-char ip))
               [((#\a - #\z) (#\A - #\Z)) (read-char ip) (loop c)]
               [else (void)]))
           (fprintf buf "</i>")
           (loop #f)]
          [(#\<) (fprintf buf (if (or compact? unary?) "&lt;" " &lt; ")) (loop #t)]
          [(#\>) (fprintf buf (if (or compact? unary?) "&gt;" " &gt; ")) (loop #t)]
          [(#\= #\+ #\-)
           (fprintf buf (if (or compact? unary?) "~c" " ~c ") c)
           (loop #t)]
          [(#\/ #\( #\[ #\,) (write-char c buf) (loop #t)]
          [((#\0 - #\9) #\. #\) #\] #\! #\| #\')
           (write-char c buf) (loop #f)]
          [(#\space) (loop unary?)]
          [(#\newline) (write-char c buf) (loop #t)]
          [(eof) (get-output-string buf)]
          [else (input-error "unexpected character ~s in math mode" c)])))))

(define punt-to-latex
  (lambda (s op)
    (define latex-header
"\\documentclass[12pt]{article}
\\input mathmacros   % Kent wanted it moved back up here for some reason.
\\begin{document}
\\pagestyle{empty}
%\\input mathmacros  % I had moved it for a good reason that I don't recall.
")
    (define latex-trailer
"
\\end{document}
")
    (cond
      [(assoc s (latex-cache)) =>
       (lambda (a)
         (fprintf op "<img src=\"~a\" alt=\"<graphic>\">" (cdr a)))]
      [else
       (let* ([fn (math-file-name)]
              [texfn (format "~a.tex" fn)]
              [giffn (format "~a.gif" fn)])
         (fprintf op "<img src=\"~a\" alt=\"<graphic>\">" giffn)
         (latex-cache (cons (cons s (format "~a" giffn)) (latex-cache)))
        ; don't rewrite file unless different to avoid need to remake gif file
         (let ([s (format "~a~a~a" latex-header s latex-trailer)])
           (unless (guard (c [else #f])
                     (equal? s (call-with-port (open-input-file texfn) get-string-all)))
             (call-with-port (open-output-file texfn 'replace)
               (lambda (texop) (display s texop))))))])))

(define math-file-name
  (let ([seq -1])
    (lambda ()
      (set! seq (+ seq 1))
      (format "~a/~d" (math-directory) seq))))

(define haux-put-label
  (lambda (label type url)
    (write `(putprop ',label ',type ,url) (haux-op))
    (newline (haux-op))))

(define current-ofile-name
  (lambda ()
    (if (current-ofile)
        (port-name (current-ofile))
        "nofile")))

(define slabel
  (case-lambda
    [(label text) (slabel label text (gensym))]
    [(label text tag)
     (let ([url (format "~a#~a" (current-ofile-name) tag)])
       (haux-put-label label 'pageref-url url)
       (when (current-ref-label)
         (haux-put-label label 'ref (car (current-ref-label)))
         (haux-put-label label 'ref-url (cdr (current-ref-label)))))
     (format "<a name=\"~a\">~a</a>" tag text)]))

(define sindex
  ; 1. read contents of \index{} form
  ;    separate at !s into 1 or more levels plus page format after |, if
  ;    present; separate levels at @ into sort key and text, if present
  ;    recognize quoted/escaped characters in the input
  ; 2. look up pageno & url corresponding
  ; 3. cons entry on to index-entries
  (lambda (ip op lab)
    (call-with-values
      (lambda () (parse-index ip #f))
      (lambda (levels page-format)
        (let ([keys (map (lambda (level) (or (car level) (cdr level)))
                         levels)]
              [texts (map cdr levels)]
              [pageno (get-label lab 'pageref)]
              [url (get-label lab 'pageref-url)])
          (index-entries
            (cons (make-index-entry url keys texts pageno page-format "")
                  (index-entries))))))))

(define smakeindex
  ; insert indexspace between letters?
  ; links per starting letter?
  (lambda (op)
    (define print-page
      (lambda (entry)
        (let ([pageno (format "\\raw{<a class=index href=\"~a\">}~a~a\\raw{</a>}"
                        (index-entry-url entry)
                        (index-entry-prefix entry)
                        (index-entry-pageno entry))]
              [pageformat (index-entry-pageformat entry)])
          (if (string=? pageformat "")
              (fprintf op ", ~a" pageno)
              (fprintf op ", \\~a{~a}" pageformat pageno)))))
    (define see?
      (lambda (pageformat)
        (and (fx> (string-length pageformat) 3)
             (string=? (substring pageformat 0 3) "see"))))
    (define remove-dups
      ; remove dup even if url is different, which it usually will be.
      ; to avoid entries like "begin, 51, 51".  if they're on the same
      ; page in the printed version, they'll be close enough in the
      ; electronic version.  remove duplicate "see" pageformat entries
      ; regardless of the page and url.
      (lambda (ls)
        (if (null? ls)
            ls
            (let f ([ls (cdr ls)] [prev (car ls)])
              (if (null? ls)
                  (list prev)
                  (let ([x (car ls)])
                    (if (let ([xpageformat (index-entry-pageformat x)])
                          (and (see? xpageformat)
                               (string=? (index-entry-pageformat prev) xpageformat)))
                        (f (cdr ls) prev)
                        (if (and (equal? (index-entry-texts x)
                                         (index-entry-texts prev))
                                 (string=? (index-entry-prefix x)
                                           (index-entry-prefix prev))
                                 (string=? (index-entry-pageno x)
                                           (index-entry-pageno prev)))
                            (if (string=? (index-entry-pageformat x)
                                          (index-entry-pageformat prev))
                                (f (cdr ls) prev)
                                (if (string=? (index-entry-pageformat prev) "")
                                    (f (cdr ls) x)
                                    (if (string=? (index-entry-pageformat x) "")
                                        (f (cdr ls) prev)
                                        (errorf #f
                                          "conflicting page formats for ~s and ~s"
                                          x prev))))
                            (cons prev (f (cdr ls) x))))))))))
    (define print-item
      (lambda (entry texts level)
        (let f ([texts texts] [level level])
          (if (null? texts)
              (print-page entry)
              (begin
                (fprintf op "~%  \\")
                (do ([i level (- i 1)]) ((= i 0)) (display "sub" op))
                (fprintf op "item ~a" (car texts))
                (f (cdr texts) (+ level 1)))))))
    (define see-cmp
      (lambda (xpageformat ypageformat)
        (if (see? xpageformat)
            (if (see? ypageformat)
                (str-cmp xpageformat ypageformat)
                '>)
            (if (see? ypageformat)
                '<
                '=))))
    (guard (c [else #f])
      (let ([seed-entries (call-with-port (open-input-file "in.hidx") read)])
        (index-entries (append seed-entries (index-entries)))))
    (parameterize ([print-vector-length #f])
      (let ([out.hidx (open-output-file "out.hidx" 'replace)])
        (fprintf out.hidx "(~%") ;)
        (for-each
          (lambda (x) (fprintf out.hidx "~s~%" x))
          (index-entries)) ;(
        (fprintf out.hidx ")~%")))
    (fprintf op "\\begin{theindex}~%")
    (let ([ls (sort (lambda (x y)
                      ; sort based on each key.  if the keys compare equal,
                      ; sort based on text.  if the text is equal, sort based
                      ; on the remaining keys and texts.  for matching
                      ; keys and texts, push "see" formats to the back, then
                      ; sort on prefix, pageno, and url
                      (let f ([xkeys (index-entry-keys x)]
                              [xtexts (index-entry-texts x)]
                              [ykeys (index-entry-keys y)]
                              [ytexts (index-entry-texts y)])
                        (cond
                          [(null? xkeys)
                           (or (not (null? ykeys))
                               (case (see-cmp (index-entry-pageformat x) (index-entry-pageformat y))
                                 [(<) #t]
                                 [(>) #f]
                                 [else
                                  (case (str-cmp (index-entry-prefix x)
                                                 (index-entry-prefix y))
                                    [(<) #t]
                                    [(>) #f]
                                    [else
                                     (let ([nx (pageno->number (index-entry-pageno x))]
                                           [ny (pageno->number (index-entry-pageno y))])
                                       (cond
                                         [(< nx ny) #t]
                                         [(> nx ny) #f]
                                         [else 
                                          (str-cmp (index-entry-url x)
                                                   (index-entry-url y))]))])]))]
                          [(null? ykeys) #f]
                          [else
                           (case (str-cmp (car xkeys) (car ykeys))
                             [(<) #t]
                             [(>) #f]
                             [else
                              ; separate different texts with same keys
                              (case (str-cmp (car xtexts) (car ytexts))
                                [(<) #t]
                                [(>) #f]
                                [else (f (cdr xkeys) (cdr xtexts)
                                         (cdr ykeys) (cdr ytexts))])])])))
                    (index-entries))])
      (let loop ([ls (remove-dups ls)] [last-texts '()])
        (unless (null? ls)
          (let* ([entry (car ls)] [texts (index-entry-texts entry)])
            (let f ([texts texts] [last-texts last-texts] [level 0])
              (if (null? last-texts)
                  (if (null? texts)
                      (print-page entry)
                      (print-item entry texts level))
                  (if (eq? (str-cmp (car texts) (car last-texts)) '=)
                      (f (cdr texts) (cdr last-texts) (+ level 1))
                      (print-item entry texts level))))
            (loop (cdr ls) texts)))))
    (fprintf op "\\end{theindex}~%")))

(define pageno->number
  (lambda (s)
    (let ([sip (open-input-string s)])
      (let loop ([a -10000])
        (state-case (c (read-char sip))
          [(#\i)
           (state-case (c (peek-char sip))
             [(#\v) (read-char sip) (loop (+ a 4))]
             [(#\x) (read-char sip) (loop (+ a 9))]
             [else (loop (+ a 1))])]
          [(#\v)
           (state-case (c (peek-char sip))
             [(#\x) (read-char sip) (loop (+ a 5))]
             [else (loop (+ a 5))])]
          [(#\x) (loop (+ a 10))]
          [(eof) a]
          [else (or (string->number s) -1)])))))

(define char-table
  (let ([s (make-string 256 #\nul)])
    (define fill!
      (lambda (i ls)
        (unless (null? ls)
          (for-each
            (lambda (c) (string-set! s (char->integer c) (integer->char i)))
            (if (char? (car ls)) (list (car ls)) (car ls)))
          (fill! (+ i 1) (cdr ls)))))
    (fill! 1 '(
       #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.  #\/ #\:
       #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~
       #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
       (#\A #\a) (#\B #\b) (#\C #\c) (#\D #\d) (#\E #\e) (#\F #\f)
       (#\G #\g) (#\H #\h) (#\I #\i) (#\J #\j) (#\K #\k) (#\L #\l)
       (#\M #\m) (#\N #\n) (#\O #\o) (#\P #\p) (#\Q #\q) (#\R #\r)
       (#\S #\s) (#\T #\t) (#\U #\u) (#\V #\v) (#\W #\w) (#\X #\x)
       (#\Y #\y) (#\Z #\z)))
    s))

(define char-cvt
  ; place all non-alphabetic characters up front; commonize upper and
  ; lower case characters
  (lambda (c)
    (string-ref char-table (char->integer c))))

(define str-cmp
  ; returns <, =, > if x<y, =y, >y
  (lambda (s1 s2)
    (let ([n1 (string-length s1)] [n2 (string-length s2)])
      (let f ([i 0])
        (if (fx= i n2)
            (if (fx= i n1) '= '>)
            (if (fx= i n1)
                '<
                (let ([c1 (char-cvt (string-ref s1 i))]
                      [c2 (char-cvt (string-ref s2 i))])
                  (if (char<? c1 c2)
                      '<
                      (if (char=? c1 c2) (f (fx+ i 1)) '>)))))))))

(define push-undo
  (lambda (p undos)
    (cons (cons p (car undos)) (cdr undos))))

(define-syntactic-monad P
  ip                   ; current input port
  op                   ; current output port
  def-env              ; definition environment
  pending              ; stack of pending latex "environments"
  groups               ; stack of pending groups
  ips                  ; input port stack; does not include ip
  ops                  ; output port stack; does not include op
  ifiles               ; stack of input files [(cons ip ips) w/o string ports]
  ofiles               ; stack of output files [(cons op ops) w/o string ports]
  rawfiles             ; assocation list of name, raw output-file pairs
  hard-spaces          ; if #t, treat spaces as ~
  eofconts             ; stack of continuations to call on eof
  undos                ; stack of lists of undo procs to call when group ends
  column               ; current column in current table
  columns              ; stack of current column in pending tables
  colfmt               ; column format for current table
  colfmts              ; stack of column formats for pending tables
  convert-quotes       ; if #t, converts `` and '' to " and -- to -
)

(define sinclude
  (P lambda (fn)
    (let ([new-ip
           (guard (c [else (warningf #f "cannot open ~a" fn) #f])
             (open-input-file fn))])
      (if new-ip
          (P s0
            ([ip new-ip]
             [ips (cons ip ips)]
             [eofconts (cons s0 eofconts)]
             [ifiles (push-ifile new-ip ifiles)]))
          (P s0)))))

(define sbegingroup
  (P lambda (g)
    (P s0
      ([groups (cons g groups)]
       [def-env (cons '() def-env)]
       [undos (cons '() undos)]))))

(define sundo
  (lambda (ls)
    (if (null? ls)
        s0
        (P lambda ()
          (P (car ls) () (sundo (cdr ls)))))))

(define sendgroup
  (P lambda (g)
    (unless (eq? (car groups) g)
      (input-error "unmatched ~a"
        (if (eq? g 'bgroup) "close brace or egroup" "endgroup")))
    (P (sundo (car undos))
      ([groups (cdr groups)]
       [def-env (cdr def-env)]
       [undos (cdr undos)]))))

(define snewcommand
  (P lambda (cmd)
    (define iota
      (lambda (i n)
        (if (> i n) '() (cons i (iota (+ i 1) n)))))
    (let* ([argcnt-str (read-optional-arg ip)]
           [argcnt (if argcnt-str (string->number argcnt-str) 0)]
           [opt (read-optional-arg ip)]
           [template (read-bracketed-text ip)])
      (unless (and argcnt (<= (if opt 1 0) argcnt 9))
        (input-error "invalid argument count ~a" argcnt-str))
      (let ([pattern (if opt (cons opt (iota 2 argcnt)) (iota 1 argcnt))])
        (set-def! cmd def-env #f
          (P lambda ()
            (P s0
              ([ip (open-input-string
                     (expand-template template
                       (read-args ip pattern cmd) cmd))]
               [ips (cons ip ips)]
               [eofconts (cons s0 eofconts)]))))))
    (P s0)))

(define snewenvironment
  (P lambda (cmd)
    (define iota
      (lambda (i n)
        (if (> i n) '() (cons i (iota (+ i 1) n)))))
    (let* ([argcnt-str (read-optional-arg ip)]
           [argcnt (if argcnt-str (string->number argcnt-str) 0)]
           [opt (read-optional-arg ip)]
           [b (begin (suppress-white-space ip) (read-bracketed-text ip))]
           [e (begin (suppress-white-space ip) (read-bracketed-text ip))])
      (unless (and argcnt (<= (if opt 1 0) argcnt 9))
        (input-error "invalid argument count ~a" argcnt-str))
      (let ([pattern (if opt (cons opt (iota 2 argcnt)) (iota 1 argcnt))])
        (set-def! cmd def-env #f
          (P lambda ()
            (P s0
              ([ip (open-input-string
                     (expand-template b
                       (read-args ip pattern cmd) cmd))]
               [ips (cons ip ips)]
               [eofconts (cons s0 eofconts)]
               [pending (cons cmd pending)])))))
      (let ([endcmd (string->symbol (format "end~a" cmd))])
        (set-def! endcmd def-env #f
          (P lambda ()
            (P s0 ([ip (open-input-string e)]
                   [ips (cons ip ips)]
                   [eofconts (cons (P lambda ()
                                     (check-pending (car pending) cmd)
                                     (P s0 ([pending (cdr pending)])))
                                   eofconts)]))))))
    (P s0)))

(define check-pending
  (lambda (expected actual)
    (unless (eq? expected actual)
      (input-error "expected \\end{~a}, got \\end{~a} or \\end~a"
        expected
        actual
        actual))))

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

(define new-conditional
  (lambda (ifcmd def-env default cmdtrue cmdfalse)
    (define scan-if
      (let ([buf (open-output-string)])
        (lambda (ip def-env hard-spaces)
          (let loop ([depth 0] [then-part #f])
            (state-case (c (read-char ip))
              [(#\\)
               (let ([cmd (read-command ip)])
                 (cond
                   [(conditional? cmd def-env)
                    (fprintf buf "\\~a" cmd)
                    (loop (+ depth 1) then-part)]
                   [(and (= depth 0) (eq? cmd 'else))
                    (if then-part
                        (input-error "extra \\else found")
                        (loop depth (get-output-string buf)))]
                   [(eq? cmd 'fi)
                    (if (= depth 0)
                        (if then-part
                            (values then-part (get-output-string buf))
                            (values (get-output-string buf) ""))
                        (begin
                          (fprintf buf "\\~a" cmd)
                          (loop (- depth 1) then-part)))]
                   [else
                    (fprintf buf "\\~a" cmd)
                    (loop depth then-part)]))]
              [(#\%)
               (scomment ip buf hard-spaces)
               (loop depth then-part)]
              [(#\{)
               (fprintf buf "{~a}" (read-bracketed-text ip 1))
               (loop depth then-part)]
              [(#\}) (input-error "unmatched } within \\if ... \\fi")]
              [(eof) (input-error
                       "unexpected end-of-file within \\if ... \\fi")]
              [else (write-char c buf) (loop depth then-part)])))))
    (let ([cell (cons default (void))])
      (set-def! ifcmd def-env #t
        (P lambda ()
          (call-with-values
            (lambda () (scan-if ip def-env hard-spaces))
            (lambda (then-part else-part)
              (let ([part (if (car cell) then-part else-part)])
                (P s0 ([ip (open-input-string part)]
                       [ips (cons ip ips)]
                       [eofconts (cons s0 eofconts)])))))))
      (when cmdtrue
        (set-def! cmdtrue def-env #f
          (P lambda ()
            (set-car! cell #t)
            (P s0))))
      (when cmdfalse
        (set-def! cmdfalse def-env #f
          (P lambda ()
            (set-car! cell #f)
            (P s0)))))))

(define (sraw ip op)
  (state-case (c (read-char ip))
    [(#\\)
     (state-case (c2 (peek-char ip))
       [(#\%)
        (write-char (read-char ip) op)]
       [else (write-char #\\ op)])
     (sraw ip op)]
    [(eof) (void)]
    [else (write-char c op) (sraw ip op)]))

(define snewif
  (lambda (ip def-env)
    (state-case (c (read-char ip))
      [(#\\)
       (let ([ifcmd (read-command ip)])
         (let ([ifcmd-str (symbol->string ifcmd)])
           (unless (and (> (string-length ifcmd-str) 2)
                        (string=? (substring ifcmd-str 0 2) "if"))
             (input-error "invalid conditional name ~a" ifcmd))
           (let ([cmd (substring ifcmd-str 2 (string-length ifcmd-str))])
             (new-conditional ifcmd def-env #f
               (string->symbol (string-append cmd "true"))
               (string->symbol (string-append cmd "false"))))))]
      [else (input-error "unexpected character following \\newif")])))

(define-syntax numbering-command
  (syntax-rules ()
    [(_ who fmt)
     (global-def who (snumber 'who fmt))]))

(define snumber
  (lambda (who fmt)
    (P lambda ()
      (let* ([counter (string->symbol (read-bracketed-text ip))]
             [value (get-counter-value counter)])
        (unless value
          (input-error "unknown counter in \\~a{~a}" who counter))
        (display (fmt (get-counter-value counter)) op))
      (P s0))))

(define s0
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\\)
       (let ([cmd (read-command ip)])
         (cond
           [(get-def cmd def-env) =>
            (lambda (proc)
              (unless (command-symbol? cmd)
                (if hard-spaces
                    (let loop () ; squeeze out only empty lines
                      (state-case (c (peek-char ip))
                        [(#\newline) (read-char ip) (loop)]
                        [(#\%) (read-char ip) (scomment ip op hard-spaces) (loop)]
                        [else (void)]))
                    (suppress-white-space ip)))
              (P proc))]
           [else (unexpected-command cmd)]))]
      [(#\space #\tab)
       (if hard-spaces (display "&nbsp;" op) (write-char c op))
       (P s0)]
      [(#\~)
       (display "&nbsp;" op)
       (P s0)]
      [(#\-)
       (if convert-quotes
           ; emit - for -- and --- for ---.  IE4 and older versions of netscape
           ; can't handle the proper HTML en-dash and em-dash encodings.
           (if (eqv? (peek-char ip) #\-)
               (begin
                 (read-char ip)
                 (if (eqv? (peek-char ip) #\-)
                     (begin
                       (read-char ip)
                       (display "---" op)) ; should be &#8212;
                     (display "-" op))) ; should be &#8211;
               (write-char c op))
           (write-char c op))
       (P s0)]
      [(#\<) (fprintf op "&lt;") (P s0)]
      [(#\>) (fprintf op "&gt;") (P s0)]
      [(#\$)
       (s$ ip op)
       (P s0)]
      [(#\%) (scomment ip op hard-spaces) (P s0)]
      [(#\{) (P sbegingroup () 'group)]
      [(#\}) (P sendgroup () 'group)]
      [(#\` #\')
       (if (and convert-quotes (eqv? (peek-char ip) c))
           (begin (read-char ip) (write-char #\" op))
           (write-char c op))
       (P s0)]
      [(#\newline)
       (write-char c op)
       (when (let loop ([par? #f]) ; insert par for multiple blank lines
               (state-case (c (peek-char ip))
                 [(#\newline) (read-char ip) (write-char c op) (loop #t)]
                 [(#\space #\tab)
                  (if hard-spaces
                      par?
                      (begin (read-char ip) (write-char c op) (loop par?)))]
                 [(#\%) (read-char ip) (scomment ip op hard-spaces) (loop par?)]
                 [else par?]))
         (fprintf op "<p>~%"))
       (P s0)]
      [(#\&) (P sampersand ())]
      [(eof)
       (close-input-port ip)
       (if (null? ips)
           (void)
           (P (car eofconts)
             ([ip (car ips)]
              [ips (cdr ips)]
              [ifiles (if (eq? ip (car ifiles)) (pop-ifile ifiles) ifiles)]
              [eofconts (cdr eofconts)])))]
      [else (write-char c op) (P s0)])))

;-----------------------------------------------------------------------
; rudimentary table support

(define-record table (col-format border?))

(define silenced? #f)

(define parse-col-format
  (lambda (s)
    (let ([ip (open-input-string s)])
      (let loop ([ls '()] [border? #f])
        (state-case (c (read-char ip))
          [(#\|)
(unless silenced?
  (set! silenced? #t)
  (warningf 'tabular "support for rules in HTML tables is imprecise"))
           (loop ls #t)]
          [(#\c) (loop (cons " align=\"center\"" ls) border?)]
          [(#\l) (loop (cons " align=\"left\"" ls) border?)]
          [(#\r) (loop (cons " align=\"right\"" ls) border?)]
          [(#\@)
(warningf 'tabular "ignoring @{~a} for now" (read-bracketed-text ip))
           (loop ls border?)]
          [(eof) (make-table (list->vector (reverse ls)) border?)]
          [else (input-error (format "unexpected column format specifier ~a" c))])))))

(define emit-td
  (P lambda (k)
    (fprintf (car ops) "<TD nowrap~a>~a</TD>"
      (vector-ref (table-col-format colfmt) column)
      (get-output-string op))
    (case (car pending)
      [(multicolumn)
       (P k ([pending (cdr pending)]
             [colfmt (car colfmts)]
             [colfmts (cdr colfmts)]))]
      [(tabular) (P k ())]
      [else (errorf 'html-prep.ss "emit-td somehow invoked with invalid pending ~s" (car pending))])))

(define sampersand
  (P lambda ()
    (case (car pending)
      [(tabular multicolumn)
       (P emit-td ()
         (P lambda ()
           (let ([column (+ column 1)])
             (when (>= column (vector-length (table-col-format colfmt)))
               (unread-char #\& ip)
               (input-error "Extra alignment character &"))
             (P s0 ([op (open-output-string)] [column column])))))]
      [else (input-error "Misplaced or extra alignment tab character &")])))

(define scr
  (P lambda ()
    (read-optional-arg ip) ; waste bracketed skip amount, if any
    (case (car pending)
      [(tabular multicolumn)
       (P emit-td ()
         (P lambda ()
           (display "</TR><TR>" (car ops))
           (P s0 ([column 0]))))]
      [else
       (display "<br>\n" op)
       (P s0)])))

;-----------------------------------------------------------------------

(define header-stuff (make-parameter #f))
(define style-sheet (make-parameter #f))
(define current-ofile (make-parameter #f)) ; for slabel
(define current-ref-label (make-parameter #f))
(define document-title (make-parameter #f))
(define index-entries (make-parameter #f))
(define latex-cache (make-parameter #f))
(define output-file-counters (make-parameter #f))
(define haux-op (make-parameter #f))
(define jobname (make-parameter #f))

(define go
  (lambda (fn)
    (define bit-sink
      (let ()
        (define make-bit-sink-port
          (lambda ()
            (define handler
              (lambda (msg . args)
                (record-case (cons msg args)
                  [block-write (p s n) (void)]
                  [clear-output-port (p) (set-port-output-index! p 0)]
                  [close-port (p)
                   (set-port-output-size! p 0)
                   (mark-port-closed! p)]
                  [flush-output-port (p) (set-port-output-index! p 0)]
                  [port-name (p) "bit-sink port"]
                  [write-char (c p) (set-port-output-index! p 0)]
                  [else (errorf 'bit-sink-port "operation ~s not handled"
                               msg)])))
             (let ([len 1024])
               (let ([p (make-output-port handler (make-string len))])
                 p))))
        (make-bit-sink-port)))
    (jobname fn)
    (let ([ip (open-input-file (tex-file-name fn))])
      ; preplib parameters
      (parameterize ([current-ifile #f]
                     [genlab-prefix "h"]
                     [genlab-counters '()])
        ; local parameters
        (parameterize ([header-stuff #f]
                       [style-sheet #f]
                       [current-ofile #f]
                       [current-ref-label #f]
                       [document-title "Untitled Document"]
                       [index-entries '()]
                       [latex-cache '()]
                       [output-file-counters '()]
                       [haux-op bit-sink])
          (P s0
            ([ip ip]
             [op bit-sink]
             [def-env '()]
             [pending '(top)]
             [groups '(top)]
             [ips '()]
             [ops '()]
             [ifiles (push-ifile ip '())]
             [ofiles '()]
             [rawfiles '()]
             [hard-spaces #f]
             [eofconts (list s0)]
             [undos (list '())]
             [column #f]   ; need flow-sensitive static analysis
             [columns '()]
             [colfmt #f]   ; need flow-sensitive static analysis
             [colfmts '()]
             [convert-quotes #t])))))))

(global-def def
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\def syntax")])]
           [pattern (read-def-pattern ip)]
           [template (read-bracketed-text ip)])
      (set-def! cmd def-env #f
        (P lambda ()
          (P s0
            ([ip (open-input-string
                   (expand-template template
                     (read-args ip pattern cmd) cmd))]
             [ips (cons ip ips)]
             [eofconts (cons s0 eofconts)]))))
      (P s0))))

;\let\foo=

(global-def let
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\let syntax")])]
           [rhs (state-case (c (read-char ip))
                  [(#\=) (state-case (c (read-char ip))
                           [(#\\) (read-command ip)]
                           [else (input-error "invalid \\let right-hand side")])]
                  [else (input-error "expected = after \\let")])])
      (set-def! cmd def-env #f (get-def rhs def-env))
      (P s0))))

(global-def edef
  (P lambda ()
    (let* ([cmd (state-case (c (read-char ip))
                  [(#\\) (read-command ip)]
                  [else (input-error "invalid \\def syntax")])]
           [pattern (read-def-pattern ip)]
           [template (read-bracketed-text ip)])
      (P process-string () template
        (P lambda (template)
          (set-def! cmd def-env #f
            (P lambda ()
              (P s0
                ([ip (open-input-string
                       (expand-template template
                         (read-args ip pattern cmd) cmd))]
                 [ips (cons ip ips)]
                 [eofconts (cons s0 eofconts)]))))
          (P s0))))))

(global-def newcommand
  (P lambda ()
    (read-open-brace ip)
    (read-back-slash ip)
    (let ([cmd (read-command ip)])
      (read-close-brace ip)
      (when (get-def cmd def-env)
        (input-error "\\newcommand: \\~a already defined" cmd))
      (P snewcommand () cmd))))

(global-def renewcommand
  (P lambda ()
    (read-open-brace ip)
    (read-back-slash ip)
    (let ([cmd (read-command ip)])
      (read-close-brace ip)
      (unless (get-def cmd def-env)
        (input-error "\\renewcommand: \\~a undefined" cmd))
      (P snewcommand () cmd))))

(global-def newenvironment
  (P lambda ()
    (let ([cmd (string->symbol (read-bracketed-text ip))])
      (when (get-def cmd def-env)
        (input-error "\\newenvironment: \\~a already defined" cmd))
      (P snewenvironment () cmd))))

(global-def renewenvironment
  (P lambda ()
    (let ([cmd (string->symbol (read-bracketed-text ip))])
      (unless (get-def cmd def-env)
        (input-error "\\renewenvironment: \\~a undefined" cmd))
      (P snewenvironment () cmd))))

(global-def begin
  (P lambda ()
    (let ([cmd (string->symbol (read-bracketed-text ip))])
      (cond
        [(get-def cmd def-env) => (lambda (proc) (P proc))]
        [else (input-error "undefined command \\begin{~a}" cmd)]))))

(global-def end
  (P lambda ()
    (let* ([cmd (string->symbol (read-bracketed-text ip))]
           [endcmd (string->symbol (format "end~a" cmd))])
      (cond
        [(get-def endcmd def-env) => (lambda (proc) (P proc))]
        [else (input-error "undefined command \\end{~a}" cmd)]))))

(global-def eqnarray* ; no endeqnarray*---we finish the job here
  (P lambda ()
    (fprintf op "<p>~%")
    (seqnarray* ip op)
    (fprintf op "<p>~%")
    (P s0 ())))

(global-def divertoutput
  (P lambda ()
    (let* ([level-str (or (read-optional-arg ip) "0")]
           [level (let ([i (string->number level-str)])
                    (and (fixnum? i)
                         (let ([n (length ofiles)])
                           (and (fixnum? i)
                                (if (fx< -1 i n)
                                    (fx- n i 1)
                                    (and (fx<= (- n) i -1)
                                         (fx- -1 i)))))))])
      (cond
        [level (P s0 ([op (list-ref ofiles level)] [ops (cons op ops)]))]
        [(assoc level-str rawfiles) =>
         (lambda (a)
           (P s0 ([op (cdr a)] [ops (cons op ops)])))]
        [else (input-error (format "invalid divertoutput file ~a" level-str))]))))

(global-def enddivertoutput
  (P lambda ()
    (P s0 ([op (car ops)] [ops (cdr ops)]))))

(global-def begingroup
  (P lambda ()
    (P sbegingroup () 'begingroup)))

(global-def endgroup
  (P lambda ()
    (P sendgroup () 'begingroup)))

(global-def bgroup
  (P lambda ()
    (P sbegingroup () 'bgroup)))

(global-def egroup
  (P lambda ()
    (P sendgroup () 'bgroup)))

(global-def |[| ;]
  (P lambda ()
    (fprintf op "<p>~%")
    (smathdisplay ip op)
    (fprintf op "<p>~%")
    (P s0)))

(global-def raw
  (P lambda ()
    (sraw (open-input-string (read-bracketed-text ip)) op)
    (P s0)))

(global-def jobname
  (P lambda ()
    (display (jobname) op)
    (P s0)))

(global-def newif
  (P lambda ()
    (snewif ip def-env)
    (P s0)))

(numbering-command arabic (lambda (n) n))
(numbering-command alph
  (lambda (n)
    (when (> n 26)
      (input-error "counter value ~a too large for \\alph" n))
    (string-ref "abcdefghijklmnopqrstuvwxyz" (- n 1))))
(numbering-command Alph
  (lambda (n)
    (when (> n 26)
      (input-error "counter value ~a too large for \\Alph" n))
    (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- n 1))))

(global-def newcounter
  (P lambda ()
    (let* ([name-str (read-bracketed-text ip)]
           [counter (string->symbol name-str)]
           [within (read-optional-arg ip)])
      (when (get-counter-value counter)
        (input-error "newcounter of existing counter ~a" counter))
      (when within
        (let ([within (string->symbol within)])
          (unless (get-counter-value within)
            (input-error "newcounter of ~a within unknown counter ~a"
              counter within))
          (add-subcounter! within counter)))
      (set-counter-value! counter 0)
      (set-def! (string->symbol (string-append "the" name-str)) def-env #f
        (P lambda ()
          (display (get-counter-value counter) op)
          (P s0))))
    (P s0)))

(global-def setcounter
  (P lambda ()
    (let* ([counter (string->symbol (read-bracketed-text ip))]
           [num-str (read-bracketed-text ip)]
           [old-value (get-counter-value counter)]
           [new-value (string->number num-str)])
      (unless old-value
        (input-error "setcounter of unknown counter ~a" counter))
      (unless new-value
        (input-error "invalid setcounter value ~a" num-str))
      (set-counter-value! counter new-value))
    (P s0)))

(global-def addtocounter
  (P lambda ()
    (let* ([counter (string->symbol (read-bracketed-text ip))]
           [num-str (read-bracketed-text ip)]
           [old-value (get-counter-value counter)]
           [incr (string->number num-str)])
      (unless old-value
        (input-error "addtocounter of unknown counter ~a" counter))
      (unless incr
        (input-error "invalid addtocounter increment ~a" num-str))
      (set-counter-value! counter (+ old-value incr)))
    (P s0)))

(global-def stepcounter
  (P lambda ()
    (let* ([counter (string->symbol (read-bracketed-text ip))]
           [old-value (get-counter-value counter)])
      (unless old-value
        (input-error "\\stepcounter of unknown counter ~a" counter))
      (set-counter-value! counter (+ old-value 1))
      (for-each
        (lambda (x) (set-counter-value! x 0))
        (subcounters counter)))
    (P s0)))

(global-def refstepcounter
  (P lambda ()
    (let* ([counter (string->symbol (read-bracketed-text ip))]
           [old-value (get-counter-value counter)])
      (unless old-value
        (input-error "\\refstepcounter of unknown counter ~a" counter))
      (set-counter-value! counter (+ old-value 1))
      (for-each
        (lambda (x) (set-counter-value! x 0))
        (subcounters counter))
      (P process-string () (format "\\the~a" counter)
        (P lambda (s)
          (let ([tag (gensym)])
            (current-ref-label
              (cons s (format "~a#~a" (current-ofile-name) tag)))
            (fprintf op "<a name=\"~a\"></a>" tag))
          (P s0))))))

(global-def pagebreak
  (P lambda ()
    (read-optional-arg ip) ; ignore [...]
    (P s0)))

(global-def verbatim ; no endverbatim---we finish the job here
  (P lambda ()
    (define escape-char
      (lambda (c)
        (case c
          [(#\space #\tab #\newline #\return) (write-char c op)]
          [(#\<) (fprintf op "&lt;")]
          [(#\>) (fprintf op "&gt;")]
          [(#\&) (fprintf op "&amp;")]
          [else (write-char c op)])))
    (display "<pre>" op)
    (let loop ()
      (state-case (c (read-char ip))
        [(#\\)
         (let ([cmd (read-command ip)])
           (case cmd
             [(end)
              (state-case (c (read-char ip))
                [(#\{)
                 (let ([what (read-alpha-command ip)])
                   (if (and (eq? what 'verbatim) (eqv? (peek-char ip) #\}))
                       (read-char ip)
                       (begin
                         (fprintf op "\\end{~a" what)
                         (loop))))]
                [(eof) (unexpected-eof "within verbatim environment")]
                [else (fprintf op "\\end") (escape-char c) (loop)])]
             [else (fprintf op "\\~a" cmd) (loop)]))]
        [(eof) (unexpected-eof "within verbatim environment")]
        [else (escape-char c) (loop)]))
    (fprintf op "</pre>~%")
    (P s0 ())))

(global-def |'|
  (P lambda ()
    (state-case (c (read-char ip))
      [(#\e) (fprintf op "&eacute;")]
      [(#\o) (fprintf op "&oacute;")]
      [else (input-error "invalid \\' command \\'~a" c)])
    (P s0)))

(global-def |"|      ; \"{<letter>}
  (P lambda ()
    (let ([arg (read-bracketed-text ip)])
      (unless (= (string-length arg) 1)
        (input-error "invalid \\\" argument ~s" arg))
      (let ([c (string-ref arg 0)])
        (case c
          [(#\a #\e #\i #\o #\u #\y #\A #\E #\I #\O #\U)
           (fprintf op "&~auml;" c)]
          [else (input-error "invalid \\\" command \\\"{~a}" c)])))
    (P s0)))

(global-def |c|      ; \c{<letter>}
  (P lambda ()
    (let ([arg (read-bracketed-text ip)])
      (unless (= (string-length arg) 1)
        (input-error "invalid \\c argument ~s" arg))
      (let ([c (string-ref arg 0)])
        (case c
          [(#\c #\C) (fprintf op "&~acedil;" c)]
          [else (input-error "invalid \\c command \\c{~a}" c)])))
    (P s0)))

(global-def ss
  (P lambda ()
    (fprintf op "&szlig;")
    (P s0)))

(global-def vskip
  (P lambda ()
    ; it's a pain to parse tex amounts, so we choose to ignore
    ; everything up to the next line break instead...watch out!
    (let ([op (open-output-string)])
      (let f ()
        (state-case (c (read-char ip))
          [(#\newline eof)
           (warningf 'vskip "discarded text: ~a" (get-output-string op))
           (P s0)]
          [else (write-char c op) (f)])))))

(global-def large
  (P lambda ()
    (fprintf op "<span style=\"font-size: large\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def Large
  (P lambda ()
    (fprintf op "<span style=\"font-size: x-large\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def LARGE
  (P lambda ()
    (fprintf op "<span style=\"font-size: xx-large\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def small
  (P lambda ()
    (fprintf op "<span style=\"font-size: small\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def footnotesize
  (P lambda ()
    (fprintf op "<span style=\"font-size: x-small\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def tiny
  (P lambda ()
    (fprintf op "<span style=\"font-size: xx-small\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def normalsize
  (P lambda ()
    (fprintf op "<span style=\"font-size: medium\">")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</span>") (P next))
                undos)]))))

(global-def tt
  (P lambda ()
    (fprintf op "<tt>")
    (P s0
      ([convert-quotes #f]
       [undos (push-undo
                (P lambda (next)
                  (fprintf op "</tt>")
                  (P next ([convert-quotes #t])))
                undos)]))))

(global-def bf
  (P lambda ()
    (fprintf op "<b>")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</b>") (P next))
                undos)]))))

(global-def it
  (P lambda ()
    (fprintf op "<i>")
    (P s0
      ([undos (push-undo
                (P lambda (next) (fprintf op "</i>") (P next))
                undos)]))))

(global-def hardspaces
  (P lambda ()
    (let ([old-hs hard-spaces])
      (P s0
        ([hard-spaces #t]
         [undos (push-undo
                  (P lambda (next) (P next ([hard-spaces old-hs])))
                  undos)])))))

(global-def include
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (fn)
        (P sinclude () (format "~a.tex" fn))))))

(global-def input
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (fn)
        (P sinclude () (tex-file-name fn))))))

(global-def rawinput
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (fn)
        (call-with-port (guard (c [else (warningf #f "cannot open ~a" fn) #f])
                          (open-input-file fn))
          (lambda (raw-ip)
            (let loop ()
              (let ([c (read-char raw-ip)])
                (unless (eof-object? c)
                  (write-char c op)
                  (loop))))))
        (P s0)))))

(global-def label
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (s)
        (display (slabel (string->symbol s) "" s) op)
        (P s0)))))

(global-def href
  (P lambda ()
    (let ([class (read-optional-arg ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (lab)
          (P process-string () (read-bracketed-text ip)
            (P lambda (text)
              (let ([name (string->symbol lab)])
                (fprintf op "<a ~@[class=~a ~]href=\"~a\">~a</a>"
                  class
                  (get-label name 'ref-url)
                  text)
                (P s0)))))))))

(global-def hpageref
  (P lambda ()
    (let ([class (read-optional-arg ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (lab)
          (P process-string () (read-bracketed-text ip)
            (P lambda (text)
              (let ([name (string->symbol lab)])
                (fprintf op "<a ~@[class=~a ~]href=\"~a\">~a</a>"
                  class
                  (get-label name 'pageref-url)
                  text)
                (P s0)))))))))

(global-def ref
  (P lambda ()
    (let ([class (read-optional-arg ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (s)
          (let ([name (string->symbol s)])
            (fprintf op "<a ~@[class=~a ~]href=\"~a\">~a</a>"
              class
              (get-label name 'ref-url)
              (get-label name 'ref)))
          (P s0))))))

(global-def pageref
  (P lambda ()
    (let ([class (read-optional-arg ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (s)
          (let ([name (string->symbol s)])
            (fprintf op "<a ~@[class=~a ~]href=\"~a\">~a</a>"
              class
              (get-label name 'pageref-url)
              (get-label name 'pageref)))
          (P s0))))))

(global-def cite
  (P lambda ()
    (write-char #\[ op)
    (let ([keys (let ([sip (open-input-string (read-bracketed-text ip))]
                      [buf (open-output-string)])
                  (let loop ()
                    (state-case (c (read-char sip))
                      [(#\,)
                       (let ([key (get-output-string buf)])
                         (cons key (loop)))]
                      [(eof)
                       (list (get-output-string buf))]
                      [else
                       (write-char c buf)
                       (loop)])))])
      (do ([keys keys (cdr keys)] [sep "" ","])
          ((null? keys) (write-char #\] op))
          (let ([key (string->symbol (car keys))])
            (fprintf op "~a<a class=citation href=\"~a\">~a</a>"
              sep (get-label key 'pageref-url) (get-cite key)))))
    (P s0)))

(global-def epsfbox
  (P lambda ()
    (fprintf op "<p>~%")
    (punt-to-latex (format "\\input{epsf.sty}\\epsfbox{~a}" (read-bracketed-text ip)) op)
    (fprintf op "<p>~%")
    (P s0)))

(global-def bibitem
  (P lambda ()
    (let ([key (string->symbol (read-bracketed-text ip))])
      (fprintf op "<p>[~a] " (slabel key (get-cite key))))
    (P s0)))

(global-def openhtmlfile
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (title)
        (let ([new-op (open-html-file (car ifiles) title)])
          (P s0
            ([op new-op]
             [ops (cons op ops)]
             [ofiles (push-ofile new-op ofiles)])))))))

(global-def closehtmlfile
  (P lambda ()
    (unless (and (not (null? ofiles)) (eq? op (car ofiles)))
      (input-error "invalid context for \\closehtmlfile"))
    (close-html-port op)
    (P s0 ([op (car ops)] [ops (cdr ops)] [ofiles (pop-ofile ofiles)]))))

(global-def openrawfile
  (P lambda ()
    (let ([name (read-bracketed-text ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (path)
          (P s0 ([rawfiles (cons (cons name (open-output-file path 'replace))
                                 rawfiles)])))))))

(global-def closerawfile
  (P lambda ()
    (let ([name (read-bracketed-text ip)])
      (cond
        [(assoc name rawfiles) =>
         (lambda (a)
           (close-output-port (cdr a))
           (P s0 ([rawfiles (remq a rawfiles)])))]
        [else (input-error "unrecognized raw file" name)]))))

(global-def genlab
  (P lambda ()
    (display (genlab) op)
    (P s0)))

(global-def hindex
  (P lambda ()
    (P process-string () (read-bracketed-text ip)
      (P lambda (s)
        (read-open-brace ip)
        (sindex ip op (string->symbol s))
        (P s0)))))

(global-def index
  (P lambda ()
    (let ([lab (genlab)])
      (display (slabel lab "") op)
      (read-open-brace ip)
      (sindex ip op lab)
      (P s0))))

(global-def makeindex
  (P lambda ()
    (let ([buf (open-output-string)])
      (smakeindex buf)
      (P s0 ([ip (open-input-string (get-output-string buf))]
             [ips (cons ip ips)]
             [eofconts (cons s0 eofconts)])))))

(global-def documentclass
  (P lambda ()
    (read-optional-arg ip)
    (P sinclude ()
      (with-source-path 'documentclass
        (format "~a.hcls" (read-bracketed-text ip))
        (lambda (x)
(printf "using ~a~%" x)
 x)))))

(global-def document
  (P lambda ()
    (let ([root (path-root (port-name (car ifiles)))])
      (let ([auxfn (format "~a.aux" root)] [hauxfn (format "~a.haux" root)])
        (guard (c [else (warningf #f "missing or incomplete aux file")])
          (read-aux-file (format "~a.aux" root)))
        (guard (c [else (warningf #f "missing or incomplete haux file")])
          (load hauxfn))
        (haux-op (open-output-file hauxfn 'replace))))
    (P s0
      ([ip (open-input-string
             (format "\\openhtmlfile{\\raw{~a}}" (document-title)))]
       [ips (cons ip ips)]
       [eofconts (cons s0 eofconts)]))))

(global-def enddocument
  (P lambda ()
    (for-each close-output-port (map cdr rawfiles))
    (P s0
      ([ip (open-input-string "\\closehtmlfile")]
       [ips (cons ip ips)]
       [eofconts (cons s0 eofconts)]))))

(global-def headerstuff
  (P lambda ()
    (header-stuff (read-bracketed-text ip))
    (P s0)))

(global-def documenttitle
  (P lambda ()
    (let ([fmt (read-optional-arg ip)])
      (P process-string () (read-bracketed-text ip)
        (P lambda (title)
          (global-def thetitle (P lambda () (display title op) (P s0)))
          (style-sheet fmt)
          (document-title title)
          (P s0))))))

(global-def |{|
  (P lambda ()
    (display "{" op)
    (P s0)))

(global-def |}|
  (P lambda ()
    (display "}" op)
    (P s0)))

(global-def | |
  (P lambda ()
    (display (if hard-spaces "&nbsp;" #\space) op)
    (P s0)))

(global-def |\| scr)

(global-def usepackage
  (P lambda ()
    (let ([filename (string-append (read-bracketed-text ip) ".hsty")])
      (P sinclude ()
        (or (ormap
              (lambda (p)
                (let ([path (string-append p "/" filename)])
                  (and (file-exists? path) path)))
              (source-directories))
            (input-error
              (format "hprep style file ~s not found in TEXINPUTS"
                filename)))))))

(global-def year
  (P lambda ()
     (let* ([s (date-and-time)] [len (string-length s)])
       (display (substring s (- len 4) len) op))
     (P s0)))

(global-def url
  (P lambda ()
    (define display-url
      (lambda (s op)
        (let ([n (string-length s)])
          (let loop ([i 0] [escape? #f])
            (unless (fx= i n)
              (loop (fx+ i 1)
                    (let ([c (string-ref s i)])
                      (or (and (not escape?) (char=? c #\\))
                          (begin (write-char c op) #f)))))))))
    (display-url (read-bracketed-text ip) op)
    (P s0)))

(let ()
  (global-def tabular
    (P lambda ()
      (let ([s (read-bracketed-text ip)])
        (let ([col-format (parse-col-format s)])
            (display
              (if (table-border? col-format)
                  "<TABLE border=\"1\"><TR>"
                  "<TABLE><TR>")
              op)
            (P s0 ([op (open-output-string)]
                   [ops (cons op ops)]
                   [column 0]                        ; could collapse these
                   [columns (cons column columns)]
                   [colfmt col-format]
                   [colfmts (cons colfmt colfmts)]
                   [pending (cons 'tabular pending)]))))))
  (global-def endtabular
    (P lambda ()
      (P emit-td ()
        (P lambda ()
          (check-pending (car pending) 'tabular)
          (display "</TR></TABLE>" (car ops))
          (P s0 ([op (car ops)]
                 [ops (cdr ops)]
                 [column (car columns)]
                 [columns (cdr columns)]
                 [colfmt (car colfmts)]
                 [colfmts (cdr colfmts)]
                 [pending (cdr pending)]))))))
  (global-def multicolumn
    (P lambda ()
      (let ([span
             (or (string->number (read-bracketed-text ip))
                 (input-error "number expected"))]
            [v (table-col-format colfmt)])
        (unless (integer? span)
          (input-error "invalid \\multicolumn span"))
        (unless (<= 1 span (- (vector-length v) column))
          (input-error
            (format "\\multicolumn span ~s out of range for ~s column table"
              span (vector-length v))))
        (let* ([s (read-bracketed-text ip)]
               [fmt (parse-col-format s)])
          (unless (= 1 (vector-length (table-col-format fmt)))
            (input-error (format "invalid \\multicolumn format ~a" s)))
          (P s0 ([ip (open-input-string (read-bracketed-text ip))]
                 [ips (cons ip ips)]
                 [colfmt
                  (let ([newsize (- (vector-length v) span -1)])
                    (let ([new (make-vector newsize)])
                      (do ([i 0 (+ i 1)])
                          ((= i newsize) (make-table new (table-border? colfmt)))
                          (vector-set! new i
                            (cond
                              [(< i column) (vector-ref v i)]
                              [(= i column)
                               (format " colspan=\"~a\"~a" span
                                 (vector-ref (table-col-format fmt) 0))]
                              [else (vector-ref v (+ i span -1))])))))]
                 [colfmts (cons colfmt colfmts)]
                 [pending (cons 'multicolumn pending)]
                 [eofconts (cons s0 eofconts)]))))))

)

(populate-source-directories)

(command-line-case (command-line)
  [((keyword --help)) (usage)]
  [((flags [--mathdir mathdir $ (math-directory mathdir)])
    filename* ...)
   (for-each go
     (let ([found (find-filename "html-prep.tex")])
       (if found
           (cons found filename*)
           filename*)))])
