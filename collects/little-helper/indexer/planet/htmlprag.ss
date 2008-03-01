;;; Original: htmlprag 1.3
;;; Header and footer modified to be able to run in PLT Schee v4xx.

;; THIS FILE IS GENERATED

(module htmlprag r5rs
  (#%require (only mzscheme 
                   error
                   get-output-string 
                   open-input-string
                   open-output-string ))
  
;;; @Package     HtmlPrag
;;; @Subtitle    Pragmatic Parsing and Emitting of HTML using SXML and SHTML
;;; @HomePage    http://www.neilvandyke.org/htmlprag/
;;; @Author      Neil W. Van Dyke
;;; @AuthorEmail neil@@neilvandyke.org
;;; @Version     0.16
;;; @Date        2005-12-18

;; $Id: htmlprag.scm,v 1.385 2005/12/19 03:28:28 neil Exp $

;;; @legal
;;; Copyright @copyright{} 2003 - 2005 Neil W. Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at your option) any
;;; later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/copyleft/lesser.html} for details.  For
;;; other license options and consulting, contact the author.
;;; @end legal

(define-syntax %htmlprag:testeez
  (syntax-rules () ((_ x ...)
                    ;; (testeez x ...)
                    (error "Tests disabled.")
                    )))

;;; @section Introduction

;;; HtmlPrag provides permissive HTML parsing and emitting capability to Scheme
;;; programs.  The parser is useful for software agent extraction of
;;; information from Web pages, for programmatically transforming HTML files,
;;; and for implementing interactive Web browsers.  HtmlPrag emits ``SHTML,''
;;; which is an encoding of HTML in
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/SXML.html, SXML}, so that
;;; conventional HTML may be processed with XML tools such as
;;; @uref{http://pair.com/lisovsky/query/sxpath/, SXPath}.  Like Oleg
;;; Kiselyov's @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#HTML-parser,
;;; SSAX-based HTML parser}, HtmlPrag provides a permissive tokenizer, but also
;;; attempts to recover structure.  HtmlPrag also includes procedures for
;;; encoding SHTML in HTML syntax.
;;;
;;; The HtmlPrag parsing behavior is permissive in that it accepts erroneous
;;; HTML, handling several classes of HTML syntax errors gracefully, without
;;; yielding a parse error.  This is crucial for parsing arbitrary real-world
;;; Web pages, since many pages actually contain syntax errors that would
;;; defeat a strict or validating parser.  HtmlPrag's handling of errors is
;;; intended to generally emulate popular Web browsers' interpretation of the
;;; structure of erroneous HTML.  We euphemistically term this kind of parse
;;; ``pragmatic.''
;;;
;;; HtmlPrag also has some support for XHTML, although XML namespace qualifiers
;;; are currently accepted but stripped from the resulting SHTML.  Note that
;;; valid XHTML input is of course better handled by a validating XML parser
;;; like Kiselyov's
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-parser, SSAX}.
;;;
;;; HtmlPrag requires R5RS, SRFI-6, and SRFI-23.

;; The following bindings are used internally by HtmlPrag for portability, with
;; the intention that packagings of HtmlPrag use more efficient procedures for
;; the particular Scheme implementation.  This is waiting on universal support
;; of SRFI-0.

;; @defproc %htmlprag:a2c num
;;
;; Returns the character with ASCII value @var{num}.  In most Scheme
;; implementations, this is the same as @code{integer->char}.  Two exceptions
;; are Scheme 48 0.57 and Scsh 0.6.3, for which the user must manually edit
;; file @code{htmlprag.scm} to bind this variable to @code{ascii->char}.  A
;; future version of HtmlPrag will automatically use @code{ascii->char} where
;; available.

(define %htmlprag:a2c integer->char)

;; @defproc %htmlprag:append! a b
;;
;; Returns a concatenation of lists @var{a} and @var{b}, modifying the tail of
;; @var{a} to point to the head of @var{b} if both lists are non-null.  A
;; future version should use the more general @code{append!} where available.

(define (%htmlprag:append! a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else      (let loop  ((sub a))
                     (if (null? (cdr sub))
                         (begin (set-cdr! sub b)
                                a)
                         (loop (cdr sub)))))))

;; @defproc %htmlprag:reverse!ok lst
;;
;; Returns a reversed list @var{lst}, possibly destructive.  A future version
;; will use @code{reverse!} where available, and @code{reverse} elsewhere.

(define %htmlprag:reverse!ok reverse)

;; @defproc %htmlprag:down str
;;
;; Returns a string that is equivalent to @var{str} with all characters mapped
;; to lowercase, as if by @code{char-downcase}, without mutating @var{str}.  A
;; future version should use the Scheme implementation's native nondestructive
;; procedure where available.

;; @defproc %htmlprag:error proc-str msg obj
;;
;; For Bigloo, this is changed to:
;;
;; @lisp
;; (define %htmlprag:error error)
;; @end lisp

;; TODO: Maybe go back to requiring a SRFI-23 "error".

(define-syntax %htmlprag:error
  (syntax-rules ()
    ((_ p m o) (error (string-append p " : " m) o))
    ;; ((_ p m o) (error p m o))))
    ))

(define (%htmlprag:down s)
  (list->string (map char-downcase (string->list s))))

;; @defproc %htmlprag:down!ok str
;;
;; Returns a string that is equivalent to @var{str} with all characters mapped
;; to lowercase, as if by @code{char-downcase}, possibly mutating @var{str}.
;; A future version should use the Scheme implementation's native destructive
;; or nondestructive procedure where available.

(define %htmlprag:down!ok %htmlprag:down)

;; @defproc %htmlprag:gosc os
;;
;; One-shot version of the conventional @code{get-output-string}.  The result
;; of any subsequent attempt to write to the port or get the output string is
;; undefined.  This may or may not free up resources.

(define (%htmlprag:gosc os)
  (let ((str (get-output-string os)))
    ;; Note: By default, we don't call close-output-port, since at least one
    ;; tested Scheme implementation barfs on that.
    ;;
    ;; (close-output-port os)
    str))

;;; @section SHTML and SXML

;;; SHTML is a variant of SXML, with two minor but useful extensions:
;;;
;;; @enumerate
;;;
;;; @item
;;; The SXML keyword symbols, such as @code{*TOP*}, are defined to be in all
;;; uppercase, regardless of the case-sensitivity of the reader of the hosting
;;; Scheme implementation in any context.  This avoids several pitfalls.
;;;
;;; @item
;;; Since not all character entity references used in HTML can be converted to
;;; Scheme characters in all R5RS Scheme implementations, nor represented in
;;; conventional text files or other common external text formats to which one
;;; might wish to write SHTML, SHTML adds a special @code{&} syntax for
;;; non-ASCII (or non-Extended-ASCII) characters.  The syntax is @code{(&
;;; @var{val})}, where @var{val} is a symbol or string naming with the symbolic
;;; name of the character, or an integer with the numeric value of the
;;; character.
;;;
;;; @end enumerate

;;; @defvar  shtml-comment-symbol
;;; @defvarx shtml-decl-symbol
;;; @defvarx shtml-empty-symbol
;;; @defvarx shtml-end-symbol
;;; @defvarx shtml-entity-symbol
;;; @defvarx shtml-pi-symbol
;;; @defvarx shtml-start-symbol
;;; @defvarx shtml-text-symbol
;;; @defvarx shtml-top-symbol
;;;
;;; These variables are bound to the following case-sensitive symbols used in
;;; SHTML, respectively: @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*},
;;; @code{*END*}, @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*},
;;; and @code{*TOP*}.  These can be used in lieu of the literal symbols in
;;; programs read by a case-insensitive Scheme reader.@footnote{Scheme
;;; implementators who have not yet made @code{read} case-sensitive by default
;;; are encouraged to do so.}

(define shtml-comment-symbol (string->symbol "*COMMENT*"))
(define shtml-decl-symbol    (string->symbol "*DECL*"))
(define shtml-empty-symbol   (string->symbol "*EMPTY*"))
(define shtml-end-symbol     (string->symbol "*END*"))
(define shtml-entity-symbol  (string->symbol "*ENTITY*"))
(define shtml-pi-symbol      (string->symbol "*PI*"))
(define shtml-start-symbol   (string->symbol "*START*"))
(define shtml-text-symbol    (string->symbol "*TEXT*"))
(define shtml-top-symbol     (string->symbol "*TOP*"))

;;; @defvar  shtml-named-char-id
;;; @defvarx shtml-numeric-char-id
;;;
;;; These variables are bound to the SHTML entity public identifier strings
;;; used in SHTML @code{*ENTITY*} named and numeric character entity
;;; references.

(define shtml-named-char-id   "shtml-named-char")
(define shtml-numeric-char-id "shtml-numeric-char")

;;; @defproc make-shtml-entity val
;;;
;;; Yields an SHTML character entity reference for @var{val}.  For example:
;;;
;;; @lisp
;;; (make-shtml-entity "rArr")                  @result{} (& rArr)
;;; (make-shtml-entity (string->symbol "rArr")) @result{} (& rArr)
;;; (make-shtml-entity 151)                     @result{} (& 151)
;;; @end lisp

(define (make-shtml-entity val)
  (list '& (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else (%htmlprag:error "make-shtml-entity"
                                        "invalid SHTML entity value:"
                                        val)))))

;; TODO:
;;
;; (define (shtml-entity? x)
;;   (and (shtml-entity-value entity) #t))

;;; @defproc shtml-entity-value obj
;;;
;;; Yields the value for the SHTML entity @var{obj}, or @code{#f} if @var{obj}
;;; is not a recognized entity.  Values of named entities are symbols, and
;;; values of numeric entities are numbers.  An error may raised if @var{obj}
;;; is an entity with system ID inconsistent with its public ID.  For example:
;;;
;;; @lisp
;;; (define (f s) (shtml-entity-value (cadr (html->shtml s))))
;;; (f "&nbsp;")  @result{} nbsp
;;; (f "&#2000;") @result{} 2000
;;; @end lisp

(define (shtml-entity-value entity)
  (cond ((not (pair? entity)) #f)
        ((null? (cdr entity)) #f)
        ((eqv? (car entity) '&)
         ;; TODO: Error-check for extraneous list members?
         (let ((val (cadr entity)))
           (cond ((symbol?  val) val)
                 ((integer? val) val)
                 ((string?  val) (string->symbol val))
                 (else           #f))))
        ((eqv? (car entity) shtml-entity-symbol)
         (if (null? (cddr entity))
             #f
             (let ((public-id (list-ref entity 1))
                   (system-id (list-ref entity 2)))
               ;; TODO: Error-check for extraneous list members?
               (cond ((equal? public-id shtml-named-char-id)
                      (string->symbol system-id))
                     ((equal? public-id shtml-numeric-char-id)
                      (string->number system-id))
                     (else #f)))))
        (else #f)))

;;; @section Tokenizing

;;; The tokenizer is used by the higher-level structural parser, but can also
;;; be called directly for debugging purposes or unusual applications.  Some of
;;; the list structure of tokens, such as for start tag tokens, is mutated and
;;; incorporated into the SHTML list structure emitted by the parser.

;; TODO: Document the token format.

;;; @defproc make-html-tokenizer in normalized?
;;;
;;; Constructs an HTML tokenizer procedure on input port @var{in}.  If boolean
;;; @var{normalized?} is true, then tokens will be in a format conducive to use
;;; with a parser emitting normalized SXML.  Each call to the resulting
;;; procedure yields a successive token from the input.  When the tokens have
;;; been exhausted, the procedure returns the null list.  For example:
;;;
;;; @lisp
;;; (define input (open-input-string "<a href=\"foo\">bar</a>"))
;;; (define next  (make-html-tokenizer input #f))
;;; (next) @result{} (a (@@ (href "foo")))
;;; (next) @result{} "bar"
;;; (next) @result{} (*END* a)
;;; (next) @result{} ()
;;; (next) @result{} ()
;;; @end lisp

(define make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;; text tokens with single space characters (except for when in `pre' and
  ;; verbatim elements).  The parser will introduce new contiguous whitespace
  ;; (e.g., when text tokens are concatenated, invalid end tags are removed,
  ;; whitespace is irrelevant between certain elements), but then the parser
  ;; only has to worry about the first and last character of each string.
  ;; Perhaps the text tokens should have both leading and trailing whitespace
  ;; stripped, and contain flags for whether or not leading and trailing
  ;; whitespace occurred.
  (letrec ((no-token '())

           ;; TODO: Maybe make these three variables options.

           (verbatim-to-eof-elems '(plaintext))

           (verbatim-pair-elems '(script server style xmp))

           (ws-chars (list #\space
                           (%htmlprag:a2c 9)
                           (%htmlprag:a2c 10)
                           (%htmlprag:a2c 11)
                           (%htmlprag:a2c 12)
                           (%htmlprag:a2c 13)))

           (gosc/string-or-false
            (lambda (os)
              (let ((s (%htmlprag:gosc os)))
                (if (string=? s "") #f s))))

           (gosc/symbol-or-false
            (lambda (os)
              (let ((s (gosc/string-or-false os)))
                (if s (string->symbol s) #f))))
           )
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;; be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;; isn't "make-html-tokenizer"...
                              (%htmlprag:error "make-html-tokenizer"
                                               "already unread:"
                                               c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (%htmlprag:error
                                   "make-html-tokenizer"
                                   "pushback full:"
                                   c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))

           ;; TODO: These procedures are a temporary convenience for
           ;; enumerating the pertinent character classes, with an eye towards
           ;; removing redundant tests of character class.  These procedures
           ;; should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lsquare?  (lambda () (eqv? c #\[)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                             #\A #\B #\C #\D #\E #\F))))

           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))

           (if-read-chars
            (lambda (match-chars yes-thunk no-proc)
              (let loop ((chars       match-chars)
                         (match-count 0))
                (if (null? chars)
                    (yes-thunk)
                    (begin (read-c)
                           (if (eqv? c (car chars))
                               (begin (loop (cdr chars) (+ 1 match-count)))
                               (begin (unread-c)
                                      (no-proc match-chars match-count))))))))

           (write-chars-count
            (lambda (chars count port)
              (let loop ((chars chars)
                         (count count))
                (or (zero? count)
                    (begin (write-char (car chars) port)
                           (loop (cdr chars)
                                 (- count 1)))))))

           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons '@ attrs))))))

           (make-empty-token
            (lambda (name ns attrs)
              (cons shtml-empty-symbol
                    (make-start-token name ns attrs))))

           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list shtml-end-symbol
                        name
                        (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list shtml-end-symbol name)
                      (list shtml-end-symbol
                            name
                            (cons '@ attrs))))))

           (make-comment-token
            (lambda (str) (list shtml-comment-symbol str)))

           (make-decl-token
            (lambda (parts) (cons shtml-decl-symbol parts)))

           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;; this can break SXML tools.  Have to validate this afterwards if
            ;; "verbatim-safe?".  Also check for "@" and maybe "@@".  Check
            ;; qname parsing code, especially for verbatim mode.  This is
            ;; important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (%htmlprag:gosc os)
                                              ns))
                               (set! os #f)))
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (%htmlprag:reverse!ok ns))))
                      (local (if os (%htmlprag:gosc os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;; any characters!
                      (cons ns local)
                      ;; Note: We represent "xml:" and "xmlns:" syntax as
                      ;; normal qnames, for lack of something better to do with
                      ;; them when we don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;; since HTML is usually case-insensitive.  If XML
                      ;; namespaces are used, we might wish to keep local names
                      ;; case-sensitive.
                      (if local
                          (if ns
                              (if (or (string=? ns "xml")
                                      (string=? ns "xmlns"))
                                  (string->symbol (string-append ns ":" local))
                                  (cons ns
                                        (string->symbol
                                         (%htmlprag:down!ok
                                          local))))
                              (string->symbol
                               (%htmlprag:down!ok local)))
                          (if ns
                              (string->symbol
                               (%htmlprag:down!ok ns))
                              ;; TODO: Ensure in rest of code that returning #f
                              ;; as a name here is OK.
                              #f)))))))

           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name))))
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;; compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;; two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))

           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name)))
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?)
                                               ;;(c-slash?)
                                               )
                                           (unread-c))
                                          ;; Note: We can treat a slash in an
                                          ;; unquoted attribute value as a
                                          ;; value constituent because the
                                          ;; slash is specially-handled only
                                          ;; for XHTML, and XHTML attribute
                                          ;; values must always be quoted.  We
                                          ;; could do lookahead for "/>", but
                                          ;; that wouldn't let us parse HTML
                                          ;; "<a href=/>" correctly, so this is
                                          ;; an easier and more correct way to
                                          ;; do things.
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (%htmlprag:gosc val)
                                   (symbol->string name)))
                    (if val
                        (list name (%htmlprag:gosc val))
                        (list name))))))

           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;; variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus)            'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus)              'end-minus-minus)
                                 ((end-minus-minus) (write-char #\- os) state)
                                 (else (%htmlprag:error
                                        "make-html-tokenizer"
                                        "invalid state:"
                                        state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (%htmlprag:gosc os)))))

           (scan-possible-cdata
            (lambda ()
              ;; Read "<!" and current character is "[", so try to read the
              ;; rest of the CDATA start delimeter.
              (if-read-chars
               '(#\C #\D #\A #\T #\A #\[)
               (lambda ()
                 ;; Successfully read CDATA section start delimiter, so read
                 ;; the section.
                 (scan-cdata))
               (lambda (chars count)
                 ;; Did not read rest of CDATA section start delimiter, so
                 ;; return a string for what we did read.
                 (let ((os (open-output-string)))
                   (display "<![" os)
                   (write-chars-count chars count os)
                   (%htmlprag:gosc os))))))

           (scan-cdata
            (lambda ()
              (let ((os (open-output-string)))
                (let loop ()
                  (if-read-chars
                   '(#\] #\] #\>)
                   (lambda () (%htmlprag:gosc os))
                   (lambda (chars count)
                     (if (zero? count)
                         (if (eof-object? c)
                             (%htmlprag:gosc os)
                             (begin (write-char c os)
                                    (read-c)
                                    (loop)))
                         (begin (write-char #\] os)
                                (if (= count 2)
                                    (push-c #\]))
                                (loop)))))))))

           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (gosc/symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list shtml-pi-symbol
                      name
                      (%htmlprag:gosc val)))))

           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so, use
            ;; whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted))
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (%htmlprag:gosc part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;; into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))

           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                     (let ((name (open-output-string)))
                       (write-char c name)
                       (let loop ()
                         (read-c)
                         (cond ((c-eof?)   #f)
                               ((c-alpha?) (write-char c name) (loop))
                               ((c-semi?)  #f)
                               (else       (unread-c))))
                       (set! name (%htmlprag:gosc name))
                       ;; TODO: Make the entity map an option.
                       (let ((pair (assoc name '(("amp"  . "&")
                                                 ("apos" . "'")
                                                 ("gt"   . ">")
                                                 ("lt"   . "<")
                                                 ("quot" . "\"")))))
                         (if pair
                             (cdr pair)
                             (make-shtml-entity name)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (%htmlprag:gosc num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (<= 32 n 126)
                                 ;; (and (<= 32 n 255) (not (= n 127)))
                                 (string (%htmlprag:a2c n))
                                 (make-shtml-entity n))))))
                    (else (unread-c) "&"))))

           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             ((c-bang?)
                              (read-c)
                              (if (c-lsquare?)
                                  (scan-possible-cdata)
                                  (let loop ()
                                    (cond ((c-eof?)   no-token)
                                          ((c-ws?)    (read-c) (loop))
                                          ((c-minus?) (scan-comment))
                                          (else       (unread-c)
                                                      (scan-decl))))))
                             (else (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (%htmlprag:gosc os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))

           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (%htmlprag:gosc os)))))

           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (%htmlprag:down
                                                       local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os)))
                                            (if local
                                                (display local os))
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (gosc/string-or-false os) (nexttok))))))

           (nexttok #f))

        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

;;; @defproc tokenize-html in normalized?
;;;
;;; Returns a list of tokens from input port @var{in}, normalizing according to
;;; boolean @var{normalized?}.  This is probably most useful as a debugging
;;; convenience.  For example:
;;;
;;; @lisp
;;; (tokenize-html (open-input-string "<a href=\"foo\">bar</a>") #f)
;;; @result{} ((a (@@ (href "foo"))) "bar" (*END* a))
;;; @end lisp

(define (tokenize-html in normalized?)
  (let ((next-tok (make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

;;; @defproc shtml-token-kind token
;;;
;;; Returns a symbol indicating the kind of tokenizer @var{token}:
;;; @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*}, @code{*END*},
;;; @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*}.
;;; This is used by higher-level parsing code.  For example:
;;;
;;; @lisp
;;; (map shtml-token-kind
;;;      (tokenize-html (open-input-string "<a<b>><c</</c") #f))
;;; @result{} (*START* *START* *TEXT* *START* *END* *END*)
;;; @end lisp

(define (shtml-token-kind token)
  (cond ((string? token) shtml-text-symbol)
        ((list?   token)
         (let ((s (list-ref token 0)))
           (if (memq s `(,shtml-comment-symbol
                         ,shtml-decl-symbol
                         ,shtml-empty-symbol
                         ,shtml-end-symbol
                         ,shtml-entity-symbol
                         ,shtml-pi-symbol))
               s
               shtml-start-symbol)))
        (else (%htmlprag:error "shtml-token-kind"
                               "unrecognized token kind:"
                               token))))

;;; @section Parsing

;;; Most applications will call a parser procedure such as
;;; @code{html->shtml} rather than calling the tokenizer directly.

;; @defvar %htmlprag:empty-elements
;;
;; List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.

;; TODO: Document exactly which elements these are, after we make the new
;; parameterized parser constructor.

(define %htmlprag:empty-elements
  '(& area base br frame hr img input isindex keygen link meta object param
      spacer wbr))

;;; @defproc parse-html/tokenizer tokenizer normalized?
;;;
;;; Emits a parse tree like @code{html->shtml} and related procedures, except
;;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;;; input port.  This procedure is used internally, and generally should not be
;;; called directly.

(define parse-html/tokenizer
  ;; TODO: Document the algorithm, then see if rewriting as idiomatic Scheme
  ;; can make it more clear.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.  This might also be an
            ;; acceptable way to parse old HTML that uses the `p' element as a
            ;; paragraph terminator.
            %htmlprag:empty-elements)
           (parent-constraints
            ;; TODO: Maybe make this an option.
            '((area     . (map))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (body td th))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      ;; Example `begs' value:
      ;;
      ;; ( ((head ...) . ( (title ...)                         ))
      ;;   ((html ...) . ( (head  ...) (*COMMENT* ...)         ))
      ;;   (#f         . ( (html  ...) (*DECL*    doctype ...) )) )
      (let ((begs (list (cons #f '()))))
        (letrec ((add-to-current-beg
                  (lambda (tok)
                    (set-cdr! (car begs) (cons tok (cdr (car begs))))))
                 (finish-all-begs
                  (lambda ()
                    (let ((toplist #f))
                      (map (lambda (beg) (set! toplist (finish-beg beg)))
                           begs)
                      toplist)))
                 (finish-beg
                  (lambda (beg)
                    (let ((start-tok (car beg)))
                      (if start-tok
                          (%htmlprag:append!
                           (car beg)
                           (%htmlprag:reverse!ok (cdr beg)))
                          (%htmlprag:reverse!ok (cdr beg))))))
                 (finish-begs-to
                  (lambda (name lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((eqv? name (start-tag-name starttag))
                             (set! begs (cdr lst))
                             (finish-beg top)
                             #t)
                            (else (if (finish-begs-to name (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f))))))
                 (finish-begs-upto
                  (lambda (parents lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((memq (start-tag-name starttag) parents)
                             (set! begs lst)
                             #t)
                            (else (if (finish-begs-upto parents (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f)))))))
          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs)
                  (let ((kind (shtml-token-kind tok)))
                    (cond ((memv kind `(,shtml-comment-symbol
                                        ,shtml-decl-symbol
                                        ,shtml-entity-symbol
                                        ,shtml-pi-symbol
                                        ,shtml-text-symbol))
                           (add-to-current-beg tok))
                          ((eqv? kind shtml-start-symbol)
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             (and cell (finish-begs-upto (cons 'div (cdr cell))
                                                         begs))
                             (add-to-current-beg tok)
                             (or (memq name empty-elements)
                                 (set! begs (cons (cons tok '()) begs)))))
                          ((eqv? kind shtml-empty-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-to-current-beg (cdr tok)))
                          ((eqv? kind shtml-end-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-to name begs)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (car (car begs))
                                      (begin (finish-beg (car begs))
                                             (set! begs (cdr begs)))))))
                          (else (%htmlprag:error "parse-html/tokenizer"
                                                 "unknown tag kind:"
                                                 kind)))
                    (loop))))))))))

;; @defproc %htmlprag:parse-html input normalized? top?
;;
;; This procedure is now used internally by @code{html->shtml} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.

(define (%htmlprag:parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (parse-html/tokenizer
            (make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (%htmlprag:error
                          "%htmlprag:parse-html"
                          "invalid input type:"
                          input)))
             normalized?)
            normalized?))))
    (if top?
        (cons shtml-top-symbol (parse))
        (parse))))

;;; @defproc  html->sxml-0nf input
;;; @defprocx html->sxml-1nf input
;;; @defprocx html->sxml-2nf input
;;; @defprocx html->sxml     input
;;; @defprocx html->shtml    input
;;;
;;; Permissively parse HTML from @var{input}, which is either an input port or
;;; a string, and emit an SHTML equivalent or approximation.  To borrow and
;;; slightly modify an example from Kiselyov's discussion of his HTML parser:
;;;
;;; @lisp
;;; (html->shtml
;;;  "<html><head><title></title><title>whatever</title></head><body>
;;; <a href=\"url\">link</a><p align=center><ul compact style=\"aa\">
;;; <p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened</i>
;;; still &lt; bold </b></body><P> But not done yet...")
;;; @result{}
;;; (*TOP* (html (head (title) (title "whatever"))
;;;              (body "\n"
;;;                    (a (@@ (href "url")) "link")
;;;                    (p (@@ (align "center"))
;;;                       (ul (@@ (compact) (style "aa")) "\n"))
;;;                    (p "BLah"
;;;                       (*COMMENT* " comment <comment> ")
;;;                       " "
;;;                       (i " italic " (b " bold " (tt " ened")))
;;;                       "\n"
;;;                       "still < bold "))
;;;              (p " But not done yet...")))
;;; @end lisp
;;;
;;; Note that in the emitted SHTML the text token @code{"still < bold"} is
;;; @emph{not} inside the @code{b} element, which represents an unfortunate
;;; failure to emulate all the quirks-handling behavior of some popular Web
;;; browsers.
;;;
;;; The procedures @code{html->sxml-@var{n}nf} for @var{n} 0 through 2
;;; correspond to 0th through 2nd normal forms of SXML as specified in SXML,
;;; and indicate the minimal requirements of the emitted SXML.
;;;
;;; @code{html->sxml} and @code{html->shtml} are currently aliases for
;;; @code{html->sxml-0nf}, and can be used in scripts and interactively, when
;;; terseness is important and any normal form of SXML would suffice.

(define (html->sxml-0nf input) (%htmlprag:parse-html input #f #t))
(define (html->sxml-1nf input) (%htmlprag:parse-html input #f #t))
(define (html->sxml-2nf input) (%htmlprag:parse-html input #t #t))

(define html->sxml  html->sxml-0nf)
(define html->shtml html->sxml-0nf)

;;; @section Emitting HTML

;;; Two procedures encoding the SHTML representation as conventional HTML,
;;; @code{write-shtml-as-html} and @code{shtml->html}.  These are perhaps most
;;; useful for emitting the result of parsed and transformed input HTML.  They
;;; can also be used for emitting HTML from generated or handwritten SHTML.

;;; @defproc write-shtml-as-html shtml [out [foreign-filter]]
;;;
;;; Writes a conventional HTML transliteration of the SHTML @var{shtml} to
;;; output port @var{out}.  If @var{out} is not specified, the default is the
;;; current output port.  HTML elements of types that are always empty are
;;; written using HTML4-compatible XHTML tag syntax.
;;;
;;; If @var{foreign-filter} is specified, it is a procedure of two argument
;;; that is applied to any non-SHTML (``foreign'') object encountered in
;;; @var{shtml}, and should yield SHTML.  The first argument is the object, and
;;; the second argument is a boolean for whether or not the object is part of
;;; an attribute value.
;;;
;;; No inter-tag whitespace or line breaks not explicit in @var{shtml} is
;;; emitted.  The @var{shtml} should normally include a newline at the end of
;;; the document.  For example:
;;;
;;; @lisp
;;; (write-shtml-as-html
;;;  '((html (head (title "My Title"))
;;;          (body (@@ (bgcolor "white"))
;;;                (h1 "My Heading")
;;;                (p "This is a paragraph.")
;;;                (p "This is another paragraph.")))))
;;; @print{} <html><head><title>My Title</title></head><body bgcolor="whi
;;; @print{} te"><h1>My Heading</h1><p>This is a paragraph.</p><p>This is
;;; @print{}  another paragraph.</p></body></html>
;;; @end lisp

(define (%htmlprag:write-shtml-as-html/fixed shtml out foreign-filter)
  (letrec
      ((write-shtml-text
        (lambda (str out)
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (case c
                                      ;; ((#\") "&quot;")
                                      ((#\&) "&amp;")
                                      ((#\<) "&lt;")
                                      ((#\>) "&gt;")
                                      (else c)))
                                  out)
                         (loop (+ 1 i))))))))
       (write-dquote-ampified
        (lambda (str out)
          ;; TODO: If we emit "&quot;", we really should parse it, and HTML
          ;; 4.01 says we should, but anachronisms in HTML create the potential
          ;; for nasty mutilation of URI in attribute values.
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (if (eqv? c #\") "&quot;" c))
                                  out)
                         (loop (+ 1 i))))))))
       (do-thing
        (lambda (thing)
          (cond ((string? thing) (write-shtml-text thing out))
                ((list? thing)   (if (not (null? thing))
                                     (do-list-thing thing)))
                (else (do-thing (foreign-filter thing #f))))))
       (do-list-thing
        (lambda (thing)
          (let ((head (car thing)))
            (cond ((symbol? head)
                   ;; Head is a symbol, so...
                   (cond ((eq? head shtml-comment-symbol)
                          ;; TODO: Make sure the comment text doesn't contain a
                          ;; comment end sequence.
                          (display "<!-- " out)
                          (let ((text (car (cdr thing))))
                            (if (string? text)
                                ;; TODO: Enforce whitespace safety without
                                ;; padding unnecessarily.
                                ;;
                                ;; (let ((len (string-length text)))
                                ;; (if (= len 0)
                                ;; (display #\space out)
                                ;; (begin (if (not (eqv?
                                ;; (string-ref text 0)
                                ;; #\space))
                                (display text out)
                                (%htmlprag:error
                                 "write-shtml-as-html"
                                 "invalid SHTML comment text:"
                                 thing)))
                          (or (null? (cdr (cdr thing)))
                              (%htmlprag:error
                               "write-shtml-as-html"
                               "invalid SHTML comment body:"
                               thing))
                          (display " -->" out))
                         ((eq? head shtml-decl-symbol)
                          (let ((head (car (cdr thing))))
                            (display "<!" out)
                            (display (symbol->string head) out)
                            (for-each
                             (lambda (n)
                               (cond ((symbol? n)
                                      (display #\space out)
                                      (display (symbol->string n) out))
                                     ((string? n)
                                      (display " \"" out)
                                      (write-dquote-ampified n out)
                                      (display #\" out))
                                     (else (%htmlprag:error
                                            "write-shtml-as-html"
                                            "invalid SHTML decl:"
                                            thing))))
                             (cdr (cdr thing)))
                            (display #\> out)))
                         ((eq? head shtml-pi-symbol)
                          (display "<?" out)
                          (display (symbol->string (car (cdr thing))) out)
                          (display #\space out)
                          (display (car (cdr (cdr thing))) out)
                          ;; TODO: Error-check that no more rest of PI.
                          (display "?>" out))
                         ((eq? head shtml-top-symbol)
                          (for-each do-thing (cdr thing)))
                         ((eq? head shtml-empty-symbol)
                          #f)
                         ((eq? head '@)
                          (%htmlprag:error
                           "write-shtml-as-html"
                           "illegal position of SHTML attributes:"
                           thing))
                         ((or (eq? head '&) (eq? head shtml-entity-symbol))
                          (let ((val (shtml-entity-value thing)))
                            (if val
                                (begin (write-char     #\& out)
                                       (if (integer? val)
                                           (write-char #\# out))
                                       (display        val out)
                                       (write-char     #\; out))
                                (%htmlprag:error
                                 "write-shtml-as-html"
                                 "invalid SHTML entity reference:"
                                 thing))))
                         ((memq head `(,shtml-end-symbol
                                       ,shtml-start-symbol
                                       ,shtml-text-symbol))
                          (%htmlprag:error "write-shtml-as-html"
                                           "invalid SHTML symbol:"
                                           head))
                         (else
                          (display #\< out)
                          (display head out)
                          (let* ((rest   (cdr thing)))
                            (if (not (null? rest))
                                (let ((second (car rest)))
                                  (and (list? second)
                                       (not (null? second))
                                       (eq? (car second)
                                            '@)
                                       (begin (for-each do-attr (cdr second))
                                              (set! rest (cdr rest))))))
                            (if (memq head
                                      %htmlprag:empty-elements)
                                ;; TODO: Error-check to make sure the element
                                ;; has no content other than attributes.  We
                                ;; have to test for cases like: (br (@) ()
                                ;; (()))
                                (display " />" out)
                                (begin (display #\> out)
                                       (for-each do-thing rest)
                                       (display "</" out)
                                       (display (symbol->string head) out)
                                       (display #\> out)))))))
                  ;; ((or (list? head) (string? head))
                  ;;
                  ;; Head is a list or string, which might occur as the result
                  ;; of an SXML transform, so we'll cope.
                  (else
                   ;; Head is not a symbol, which might occur as the result of
                   ;; an SXML transform, so we'll cope.
                   (for-each do-thing thing))
                  ;;(else
                  ;; ;; Head is NOT a symbol, list, or string, so error.
                  ;; (%htmlprag:error "write-shtml-as-html"
                  ;;                          "invalid SHTML list:"
                  ;;                          thing))
                  ))))
       (write-attr-val-dquoted
        (lambda (str out)
          (display #\" out)
          (display str out)
          (display #\" out)))
       (write-attr-val-squoted
        (lambda (str out)
          (display #\' out)
          (display str out)
          (display #\' out)))
       (write-attr-val-dquoted-and-amped
        (lambda (str out)
          (display #\" out)
          (write-dquote-ampified str out)
          (display #\" out)))
       (write-attr-val
        (lambda (str out)
          (let ((len (string-length str)))
            (let find-dquote-and-squote ((i 0))
              (if (= i len)
                  (write-attr-val-dquoted str out)
                  (let ((c (string-ref str i)))
                    (cond ((eqv? c #\")
                           (let find-squote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-squoted str out)
                                 (if (eqv? (string-ref str i) #\')
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-squote (+ 1 i))))))
                          ((eqv? c #\')
                           (let find-dquote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-dquoted str out)
                                 (if (eqv? (string-ref str i) #\")
                                     (write-attr-val-dquoted-and-amped str
                                                                       out)
                                     (find-dquote (+ 1 i))))))
                          (else (find-dquote-and-squote (+ 1 i))))))))))

       (collect-and-write-attr-val
        ;; TODO: Take another look at this.
        (lambda (lst out)
          (let ((os #f))
            (let do-list ((lst lst))
              (for-each
               (lambda (thing)
                 (let do-thing ((thing thing))
                   (cond ((string? thing)
                          (or os (set! os (open-output-string)))
                          (display thing os))
                         ((list? thing)
                          (do-list thing))
                         ((eq? thing #t)
                          #f)
                         (else
                          (do-thing (foreign-filter thing #t))))))
               lst))
            (if os
                (begin
                  (display #\= out)
                  (write-attr-val (%htmlprag:gosc os) out))))))

       (do-attr
        (lambda (attr)
          (or (list? attr)
              (%htmlprag:error "write-shtml-as-html"
                               "invalid SHTML attribute:"
                               attr))
          (if (not (null? attr))
              (let ((name (car attr)))
                (or (symbol? name)
                    (%htmlprag:error
                     "write-shtml-as-html"
                     "invalid name in SHTML attribute:"
                     attr))
                (if (not (eq? name '@))
                    (begin
                      (display #\space out)
                      (display name    out)
                      (collect-and-write-attr-val (cdr attr) out)

                      )))))))
    (do-thing shtml)
    (if #f #f)))

(define write-shtml-as-html
  (letrec ((error-foreign-filter
            (lambda (foreign-object in-attribute-value?)
              (%htmlprag:error
               "write-shtml-as-html"
               (if in-attribute-value?
                   "unhandled foreign object in shtml attribute value:"
                   "unhandled foreign object in shtml:")
               foreign-object))))
    (lambda (shtml . rest)
      (case (length rest)
        ((0) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (current-output-port)
              error-foreign-filter))
        ((1) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (car rest)
              error-foreign-filter))
        ((2) (%htmlprag:write-shtml-as-html/fixed
              shtml
              (car rest)
              (cadr rest)))
        (else
         (%htmlprag:error "write-shtml-as-html"
                          "extraneous arguments:"
                          (cddr rest)))))))

;;; @defproc shtml->html shtml
;;;
;;; Yields an HTML encoding of SHTML @var{shtml} as a string.  For example:
;;;
;;; @lisp
;;; (shtml->html
;;;  (html->shtml
;;;   "<P>This is<br<b<I>bold </foo>italic</ b > text.</p>"))
;;; @result{} "<p>This is<br /><b><i>bold italic</i></b> text.</p>"
;;; @end lisp
;;;
;;; Note that, since this procedure constructs a string, it should normally
;;; only be used when the HTML is relatively small.  When encoding HTML
;;; documents of conventional size and larger, @code{write-shtml-as-html} is
;;; much more efficient.

(define (shtml->html shtml)
  (let ((os (open-output-string)))
    (write-shtml-as-html shtml os)
    (%htmlprag:gosc os)))

;;; @section Tests

;;; The HtmlPrag test suite can be enabled by editing the source code file and
;;; loading @uref{http://www.neilvandyke.org/testeez/, Testeez}.

(define (%htmlprag:test)
  (%htmlprag:testeez
   "HtmlPrag"

   (test-define "" lf (string (%htmlprag:a2c 10)))

   (test/equal "" (html->shtml "<a>>") `(,shtml-top-symbol (a ">")))
   (test/equal "" (html->shtml "<a<>") `(,shtml-top-symbol (a "<" ">")))

   (test/equal "" (html->shtml "<>")      `(,shtml-top-symbol "<" ">"))
   (test/equal "" (html->shtml "< >")     `(,shtml-top-symbol "<" ">"))
   (test/equal "" (html->shtml "< a>")    `(,shtml-top-symbol (a)))
   (test/equal "" (html->shtml "< a / >") `(,shtml-top-symbol (a)))

   (test/equal "" (html->shtml "<a<")  `(,shtml-top-symbol (a "<")))
   (test/equal "" (html->shtml "<a<b") `(,shtml-top-symbol (a (b))))

   (test/equal "" (html->shtml "><a>") `(,shtml-top-symbol ">" (a)))

   (test/equal "" (html->shtml "</>") `(,shtml-top-symbol))

   (test/equal "" (html->shtml "<\">") `(,shtml-top-symbol "<" "\"" ">"))

   (test/equal ""
               (html->shtml (string-append "<a>xxx<plaintext>aaa" lf
                                           "bbb" lf
                                           "c<c<c"))
               `(,shtml-top-symbol
                 (a "xxx" (plaintext ,(string-append "aaa" lf)
                                     ,(string-append "bbb" lf)
                                     "c<c<c"))))

   (test/equal ""
               (html->shtml "aaa<!-- xxx -->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx ")   "bbb"))

   (test/equal ""
               (html->shtml "aaa<! -- xxx -->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx ")   "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx --->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx -")  "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx ---->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx --") "bbb"))

   (test/equal ""
               (html->shtml "aaa<!-- xxx -y-->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol " xxx -y") "bbb"))

   (test/equal ""
               (html->shtml "aaa<!----->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol "-")       "bbb"))

   (test/equal ""
               (html->shtml "aaa<!---->bbb")
               `(,shtml-top-symbol
                 "aaa" (,shtml-comment-symbol "")        "bbb"))

   (test/equal ""
               (html->shtml "aaa<!--->bbb")
               `(,shtml-top-symbol "aaa" (,shtml-comment-symbol "->bbb")))

   (test/equal "" (html->shtml "<hr>")   `(,shtml-top-symbol (hr)))
   (test/equal "" (html->shtml "<hr/>")  `(,shtml-top-symbol (hr)))
   (test/equal "" (html->shtml "<hr />") `(,shtml-top-symbol (hr)))

   (test/equal ""
               (html->shtml "<hr noshade>")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade/>")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade />")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade / >")
               `(,shtml-top-symbol (hr (@ (noshade)))))
   (test/equal ""
               (html->shtml "<hr noshade=1 />")
               `(,shtml-top-symbol (hr (@ (noshade "1")))))
   (test/equal ""
               (html->shtml "<hr noshade=1/>")
               `(,shtml-top-symbol (hr (@ (noshade "1/")))))

   (test/equal ""
               (html->shtml "<q>aaa<p/>bbb</q>ccc</p>ddd")
               `(,shtml-top-symbol (q "aaa" (p) "bbb") "ccc" "ddd"))

   (test/equal "" (html->shtml "&lt;") `(,shtml-top-symbol "<"))
   (test/equal "" (html->shtml "&gt;") `(,shtml-top-symbol ">"))

   (test/equal ""
               (html->shtml "Gilbert &amp; Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))
   (test/equal ""
               (html->shtml "Gilbert &amp Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))
   (test/equal ""
               (html->shtml "Gilbert & Sullivan")
               `(,shtml-top-symbol "Gilbert & Sullivan"))

   (test/equal ""
               (html->shtml "Copyright &copy; Foo")
               `(,shtml-top-symbol "Copyright "
                                   (& ,(string->symbol "copy"))
                                   " Foo"))
   (test/equal ""
               (html->shtml "aaa&copy;bbb")
               `(,shtml-top-symbol
                 "aaa" (& ,(string->symbol "copy")) "bbb"))
   (test/equal ""
               (html->shtml "aaa&copy")
               `(,shtml-top-symbol
                 "aaa" (& ,(string->symbol "copy"))))

   (test/equal "" (html->shtml "&#42;")  `(,shtml-top-symbol "*"))
   (test/equal "" (html->shtml "&#42")   `(,shtml-top-symbol "*"))
   (test/equal "" (html->shtml "&#42x")  `(,shtml-top-symbol "*x"))
   (test/equal "" (html->shtml "&#151")  `(,shtml-top-symbol
                                           (& 151)
                                           ;; ,(string (%htmlprag:a2c 151))
                                           ))
   (test/equal "" (html->shtml "&#1000") `(,shtml-top-symbol (& 1000)))
   (test/equal "" (html->shtml "&#x42")  `(,shtml-top-symbol "B"))
   (test/equal "" (html->shtml "&#xA2")  `(,shtml-top-symbol
                                           (& 162)
                                           ;; ,(string (%htmlprag:a2c 162))
                                           ))
   (test/equal "" (html->shtml "&#xFF")  `(,shtml-top-symbol
                                           (& 255)
                                           ;; ,(string (%htmlprag:a2c 255))
                                           ))
   (test/equal "" (html->shtml "&#x100") `(,shtml-top-symbol (& 256)))
   (test/equal "" (html->shtml "&#X42")  `(,shtml-top-symbol "B"))
   (test/equal "" (html->shtml "&42;")   `(,shtml-top-symbol "&42;"))

   (test/equal ""
               (html->shtml (string-append "aaa&copy;bbb&amp;ccc&lt;ddd&&gt;"
                                           "eee&#42;fff&#1000;ggg&#x5a;hhh"))
               `(,shtml-top-symbol
                 "aaa"
                 (& ,(string->symbol "copy"))
                 "bbb&ccc<ddd&>eee*fff"
                 (& 1000)
                 "gggZhhh"))

   (test/equal ""
               (html->shtml
                (string-append
                 "<IMG src=\"http://e.e/aw/pics/listings/"
                 "ebayLogo_38x16.gif\" border=0 width=\"38\" height=\"16\" "
                 "HSPACE=5 VSPACE=0\">2</FONT>"))
               `(,shtml-top-symbol
                 (img (@
                       (src
                        "http://e.e/aw/pics/listings/ebayLogo_38x16.gif")
                       (border "0") (width "38") (height "16")
                       (hspace "5") (vspace "0")))
                 "2"))

   (test/equal ""
               (html->shtml "<aaa bbb=ccc\"ddd>eee")
               `(,shtml-top-symbol (aaa (@ (bbb "ccc") (ddd)) "eee")))
   (test/equal ""
               (html->shtml "<aaa bbb=ccc \"ddd>eee")
               `(,shtml-top-symbol (aaa (@ (bbb "ccc") (ddd)) "eee")))

   (test/equal ""
               (html->shtml
                (string-append
                 "<HTML><Head><Title>My Title</Title></Head>"
                 "<Body BGColor=\"white\" Foo=42>"
                 "This is a <B><I>bold-italic</B></I> test of </Erk>"
                 "broken HTML.<br>Yes it is.</Body></HTML>"))
               `(,shtml-top-symbol
                 (html (head (title "My Title"))
                       (body (@ (bgcolor "white") (foo "42"))
                             "This is a "
                             (b (i "bold-italic"))
                             " test of "
                             "broken HTML."
                             (br)
                             "Yes it is."))))

   (test/equal ""
               (html->shtml
                (string-append
                 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
                 " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
               `(,shtml-top-symbol
                 (,shtml-decl-symbol
                  ,(string->symbol "DOCTYPE")
                  html
                  ,(string->symbol "PUBLIC")
                  "-//W3C//DTD XHTML 1.0 Strict//EN"
                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")))

   (test/equal ""
               (html->shtml
                (string-append
                 "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
                 "xml:lang=\"en\" "
                 "lang=\"en\">"))
               `(,shtml-top-symbol
                 (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                          (xml:lang "en") (lang "en")))))

   (test/equal
    ""
    (html->shtml
     (string-append
      "<html:html xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
      "<html:head><html:title>Frobnostication</html:title></html:head>"
      "<html:body><html:p>Moved to <html:a href=\"http://frob.com\">"
      "here.</html:a></html:p></html:body></html:html>"))
    `(,shtml-top-symbol
      (html (@ (xmlns:html "http://www.w3.org/TR/REC-html40"))
            (head (title "Frobnostication"))
            (body (p "Moved to "
                     (a (@ (href "http://frob.com"))
                        "here."))))))

   (test/equal ""
               (html->shtml
                (string-append
                 "<RESERVATION xmlns:HTML=\"http://www.w3.org/TR/REC-html40\">"
                 "<NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
                 "<SEAT CLASS=\"Y\" HTML:CLASS=\"largeMonotype\">33B</SEAT>"
                 "<HTML:A HREF=\"/cgi-bin/ResStatus\">Check Status</HTML:A>"
                 "<DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>"))
               `(,shtml-top-symbol
                 (reservation (@ (,(string->symbol "xmlns:HTML")
                                  "http://www.w3.org/TR/REC-html40"))
                              (name (@ (class "largeSansSerif"))
                                    "Layman, A")
                              (seat (@ (class "Y") (class "largeMonotype"))
                                    "33B")
                              (a (@ (href "/cgi-bin/ResStatus"))
                                 "Check Status")
                              (departure "1997-05-24T07:55:00+1"))))

   (test/equal
    ""
    (html->shtml
     (string-append
      "<html><head><title></title><title>whatever</title></head><body>"
      "<a href=\"url\">link</a><p align=center><ul compact style=\"aa\">"
      "<p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened </i>"
      " still &lt; bold </b></body><P> But not done yet..."))
    `(,shtml-top-symbol
      (html (head (title) (title "whatever"))
            (body (a (@ (href "url")) "link")
                  (p (@ (align "center"))
                     (ul (@ (compact) (style "aa"))))
                  (p "BLah"
                     (,shtml-comment-symbol " comment <comment> ")
                     " "
                     (i " italic " (b " bold " (tt " ened ")))
                     " still < bold "))
            (p " But not done yet..."))))

   (test/equal ""
               (html->shtml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
               `(,shtml-top-symbol
                 (,shtml-pi-symbol xml "version=\"1.0\" encoding=\"UTF-8\"")))

   (test/equal ""
               (html->shtml "<?php php_info(); ?>")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ")))
   (test/equal ""
               (html->shtml "<?php php_info(); ?")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ?")))
   (test/equal ""
               (html->shtml "<?php php_info(); ")
               `(,shtml-top-symbol (,shtml-pi-symbol php "php_info(); ")))

   (test/equal ""
               (html->shtml "<?foo bar ? baz > blort ?>")
               `(,shtml-top-symbol
                 (,shtml-pi-symbol foo "bar ? baz > blort ")))

   (test/equal ""
               (html->shtml "<?foo b?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "b") "x"))
   (test/equal ""
               (html->shtml "<?foo ?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?foo ?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?foo?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol foo "")  "x"))
   (test/equal ""
               (html->shtml "<?f?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol f   "")  "x"))
   (test/equal ""
               (html->shtml "<??>x")
               `(,shtml-top-symbol (,shtml-pi-symbol #f  "")  "x"))
   (test/equal ""
               (html->shtml "<?>x")
               `(,shtml-top-symbol (,shtml-pi-symbol #f  ">x")))

   (test/equal ""
               (html->shtml "<foo bar=\"baz\">blort")
               `(,shtml-top-symbol (foo (@ (bar "baz")) "blort")))
   (test/equal ""
               (html->shtml "<foo bar='baz'>blort")
               `(,shtml-top-symbol (foo (@ (bar "baz")) "blort")))
   (test/equal ""
               (html->shtml "<foo bar=\"baz'>blort")
               `(,shtml-top-symbol (foo (@ (bar "baz'>blort")))))
   (test/equal ""
               (html->shtml "<foo bar='baz\">blort")
               `(,shtml-top-symbol (foo (@ (bar "baz\">blort")))))

   (test/equal ""
               (html->shtml (string-append "<p>A</p>"
                                           "<script>line0 <" lf
                                           "line1" lf
                                           "<line2></script>"
                                           "<p>B</p>"))
               `(,shtml-top-symbol (p "A")
                                   (script ,(string-append "line0 <" lf)
                                           ,(string-append "line1"   lf)
                                           "<line2>")
                                   (p "B")))

   (test/equal ""
               (html->shtml "<xmp>a<b>c</XMP>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<XMP>a<b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<xmp>a<b>c</foo:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</foo:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))
   (test/equal ""
               (html->shtml "<foo:xmp>a<b>c</bar:xmp>d")
               `(,shtml-top-symbol (xmp "a<b>c") "d"))

   (test/equal ""
               (html->shtml "<xmp>a</b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b>c")     "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b >c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b >c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</ b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</ b>c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</ b >c</xmp>d")
               `(,shtml-top-symbol (xmp "a</ b >c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:x>c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b::x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b::x>c")  "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:::x>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:::x>c") "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b:>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b:>c")    "d"))
   (test/equal ""
               (html->shtml "<xmp>a</b::>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</b::>c")   "d"))
   (test/equal ""
               (html->shtml "<xmp>a</xmp:b>c</xmp>d")
               `(,shtml-top-symbol (xmp "a</xmp:b>c") "d"))

   (test-define "expected output for next two tests"
                expected
                `(,shtml-top-symbol (p "real1")
                                    ,lf
                                    (xmp ,lf
                                         ,(string-append "alpha"       lf)
                                         ,(string-append "<P>fake</P>" lf)
                                         ,(string-append "bravo"       lf))
                                    (p "real2")))

   (test/equal ""
               (html->shtml (string-append "<P>real1</P>" lf
                                           "<XMP>"        lf
                                           "alpha"        lf
                                           "<P>fake</P>"  lf
                                           "bravo"        lf
                                           "</XMP "       lf
                                           "<P>real2</P>"))
               expected)

   (test/equal ""
               (html->shtml (string-append "<P>real1</P>" lf
                                           "<XMP>"        lf
                                           "alpha"        lf
                                           "<P>fake</P>"  lf
                                           "bravo"        lf
                                           "</XMP"        lf
                                           "<P>real2</P>"))
               expected)

   (test/equal ""
               (html->shtml "<xmp>a</xmp>x")
               `(,shtml-top-symbol (xmp "a")   "x"))
   (test/equal ""
               (html->shtml (string-append "<xmp>a" lf "</xmp>x"))
               `(,shtml-top-symbol (xmp ,(string-append "a" lf)) "x"))
   (test/equal ""
               (html->shtml "<xmp></xmp>x")
               `(,shtml-top-symbol (xmp)       "x"))

   (test/equal ""
               (html->shtml "<xmp>a</xmp") `(,shtml-top-symbol (xmp "a")))
   (test/equal ""
               (html->shtml "<xmp>a</xm")  `(,shtml-top-symbol (xmp "a</xm")))
   (test/equal ""
               (html->shtml "<xmp>a</x")   `(,shtml-top-symbol (xmp "a</x")))
   (test/equal ""
               (html->shtml "<xmp>a</")    `(,shtml-top-symbol (xmp "a</")))
   (test/equal ""
               (html->shtml "<xmp>a<")     `(,shtml-top-symbol (xmp "a<")))
   (test/equal ""
               (html->shtml "<xmp>a")      `(,shtml-top-symbol (xmp "a")))
   (test/equal ""
               (html->shtml "<xmp>")       `(,shtml-top-symbol (xmp)))
   (test/equal ""
               (html->shtml "<xmp")        `(,shtml-top-symbol (xmp)))

   (test/equal ""
               (html->shtml "<xmp x=42 ")
               `(,shtml-top-symbol (xmp (@ (x "42")))))
   (test/equal ""
               (html->shtml "<xmp x= ")   `(,shtml-top-symbol (xmp (@ (x)))))
   (test/equal ""
               (html->shtml "<xmp x ")    `(,shtml-top-symbol (xmp (@ (x)))))
   (test/equal ""
               (html->shtml "<xmp x")     `(,shtml-top-symbol (xmp (@ (x)))))

   (test/equal ""
               (html->shtml "<script>xxx")
               `(,shtml-top-symbol (script "xxx")))
   (test/equal ""
               (html->shtml "<script/>xxx")
               `(,shtml-top-symbol (script) "xxx"))

   (test/equal ""
               (html->shtml "<html xml:lang=\"en\" lang=\"en\">")
               `(,shtml-top-symbol (html (@ (xml:lang "en") (lang "en")))))

   (test/equal ""
               (html->shtml "<a href=/foo.html>")
               `(,shtml-top-symbol (a (@ (href "/foo.html")))))
   (test/equal ""
               (html->shtml "<a href=/>foo.html")
               `(,shtml-top-symbol (a (@ (href "/")) "foo.html")))

   ;; TODO: Add verbatim-pair cases with attributes in the end tag.

   (test/equal ""
               (shtml->html '(p))            "<p></p>")
   (test/equal ""
               (shtml->html '(p "CONTENT"))  "<p>CONTENT</p>")
   (test/equal ""
               (shtml->html '(br))           "<br />")
   (test/equal ""
               (shtml->html '(br "CONTENT")) "<br />")

   (test/equal ""
               (shtml->html `(hr (@ (clear "all"))))
               "<hr clear=\"all\" />")

   (test/equal ""
               (shtml->html `(hr (@ (noshade))))
               "<hr noshade />")
   (test/equal ""
               (shtml->html `(hr (@ (noshade #t))))
               "<hr noshade />") ;; TODO: Maybe lose this test.
   (test/equal ""
               (shtml->html `(hr (@ (noshade "noshade"))))
               "<hr noshade=\"noshade\" />")

   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbbccc"))))
               "<hr aaa=\"bbbccc\" />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb'ccc"))))
               "<hr aaa=\"bbb'ccc\" />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb\"ccc"))))
               "<hr aaa='bbb\"ccc' />")
   (test/equal ""
               (shtml->html `(hr (@ (aaa "bbb\"ccc'ddd"))))
               "<hr aaa=\"bbb&quot;ccc'ddd\" />")

   (test/equal "" (shtml->html '(& "copy"))                   "&copy;")
   (test/equal "" (shtml->html '(& "rArr"))                   "&rArr;")
   (test/equal "" (shtml->html `(& ,(string->symbol "rArr"))) "&rArr;")
   (test/equal "" (shtml->html '(& 151))                      "&#151;")

   (test/equal ""
               (html->shtml "&copy;")
               `(,shtml-top-symbol (& ,(string->symbol "copy"))))
   (test/equal ""
               (html->shtml "&rArr;")
               `(,shtml-top-symbol (& ,(string->symbol "rArr"))))
   (test/equal ""
               (html->shtml "&#151;")
               `(,shtml-top-symbol
                 (& 151)
                 ;; ,(string (%htmlprag:a2c 151))
                 ))

   (test/equal ""
               (html->shtml "&#999;")
               `(,shtml-top-symbol (& 999)))

   (test/equal ""
               (shtml->html
                `(,shtml-pi-symbol xml "version=\"1.0\" encoding=\"UTF-8\""))
               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

   (test/equal ""
               (shtml->html
                `(,shtml-decl-symbol
                  ,(string->symbol "DOCTYPE")
                  html
                  ,(string->symbol "PUBLIC")
                  "-//W3C//DTD XHTML 1.0 Strict//EN"
                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))
               (string-append
                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
                " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

   (test/equal ""
               (shtml-entity-value '(*ENTITY* "shtml-named-char" "rArr"))
               (string->symbol "rArr"))

   (test/equal ""
               (shtml-entity-value '(& "rArr"))
               (string->symbol "rArr"))

   (test/equal ""
               (shtml-entity-value `(& ,(string->symbol "rArr")))
               (string->symbol "rArr"))

   (test/equal ""
               (html->shtml "xxx<![CDATA[abc]]>yyy")
               `(,shtml-top-symbol "xxx" "abc" "yyy"))

   (test/equal ""
               (html->shtml "xxx<![CDATA[ab]c]]>yyy")
               `(,shtml-top-symbol "xxx" "ab]c" "yyy"))

   (test/equal ""
               (html->shtml "xxx<![CDATA[ab]]c]]>yyy")
               `(,shtml-top-symbol "xxx" "ab]]c" "yyy"))

   (test/equal ""
               (html->shtml "xxx<![CDATA[]]]>yyy")
               `(,shtml-top-symbol "xxx" "]" "yyy"))

   (test/equal ""
               (html->shtml "xxx<![CDATAyyy")
               `(,shtml-top-symbol "xxx" "<![CDATA" "yyy"))

   (test/equal "parent constraints with div"
               (html->shtml "<html><div><p>P1</p><p>P2</p></div><p>P3</p>")
               `(,shtml-top-symbol (html (div (p "P1")
                                              (p "P2"))
                                         (p "P3"))))

   (test/equal "we no longer convert character references above 126 to string"
               (html->shtml "&#151;")
               `(,shtml-top-symbol (& 151)))

   ;; TODO: Write more test cases for HTML encoding.

   ;; TODO: Write test cases for foreign-filter of HTML writing.

   ;; TODO: Write test cases for attribute values that aren't simple strings.

   ;; TODO: Document this.
   ;;
   ;; (define html-1 "<myelem myattr=\"&\">")
   ;; (define shtml   (html->shtml html-1))
   ;; shtml
   ;; (define html-2 (shtml->html shtml))
   ;; html-2

   ))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.16 --- 2005-12-18
;;; Documentation fix.
;;;
;;; @item Version 0.15 --- 2005-12-18
;;; In the HTML parent element constraints that are used for structure
;;; recovery, @code{div} is now always permitted as a parent, as a stopgap
;;; measure until substantial time can be spent reworking the algorithm to
;;; better support @code{div} (bug reported by Corey Sweeney and Jepri).  Also
;;; no longer convert to Scheme character any HTML numeric character reference
;;; with value above 126, to avoid Unicode problem with PLT 299/300 (bug
;;; reported by Corey Sweeney).
;;;
;;; @item Version 0.14 --- 2005-06-16
;;; XML CDATA sections are now tokenized.  Thanks to Alejandro Forero Cuervo
;;; for suggesting this feature.  The deprecated procedures @code{sxml->html}
;;; and @code{write-sxml-html} have been removed.  Minor documentation changes.
;;;
;;; @item Version 0.13 --- 2005-02-23
;;; HtmlPrag now requires @code{syntax-rules}, and a reader that can read
;;; @code{@@} as a symbol.  SHTML now has a special @code{&} element for
;;; character entities, and it is emitted by the parser rather than the old
;;; @code{*ENTITY*} kludge.  @code{shtml-entity-value} supports both the new
;;; and the old character entity representations.  @code{shtml-entity-value}
;;; now yields @code{#f} on invalid SHTML entity, rather than raising an error.
;;; @code{write-shtml-as-html} now has a third argument, @code{foreign-filter}.
;;; @code{write-shtml-as-html} now emits SHTML @code{&} entity references.
;;; Changed @code{shtml-named-char-id} and @code{shtml-numeric-char-id}, as
;;; previously warned.  Testeez is now used for the test suite.  Test procedure
;;; is now the internal @code{%htmlprag:test}.  Documentation changes.
;;; Notably, much documentation about using HtmlPrag under various particular
;;; Scheme implementations has been removed.
;;;
;;; @item Version 0.12 --- 2004-07-12
;;; Forward-slash in an unquoted attribute value is now considered a value
;;; constituent rather than an unconsumed terminator of the value (thanks to
;;; Maurice Davis for reporting and a suggested fix).  @code{xml:} is now
;;; preserved as a namespace qualifier (thanks to Peter Barabas for
;;; reporting).  Output port term of @code{write-shtml-as-html} is now
;;; optional.  Began documenting loading for particular implementation-specific
;;; packagings.
;;;
;;; @item Version 0.11 --- 2004-05-13
;;; To reduce likely namespace collisions with SXML tools, and in anticipation
;;; of a forthcoming set of new features, introduced the concept of ``SHTML,''
;;; which will be elaborated upon in a future version of HtmlPrag.  Renamed
;;; @code{sxml-@var{x}-symbol} to @code{shtml-@var{x}-symbol},
;;; @code{sxml-html-@var{x}} to @code{shtml-@var{x}}, and
;;; @code{sxml-token-kind} to @code{shtml-token-kind}.  @code{html->shtml},
;;; @code{shtml->html}, and @code{write-shtml-as-html} have been added as
;;; names.  Considered deprecated but still defined (see the ``Deprecated''
;;; section of this documentation) are @code{sxml->html} and
;;; @code{write-sxml-html}.  The growing pains should now be all but over.
;;; Internally, @code{htmlprag-internal:error} introduced for Bigloo
;;; portability.  SISC returned to the test list; thanks to Scott G.  Miller
;;; for his help.  Fixed a new character @code{eq?}  bug, thanks to SISC.
;;;
;;; @item Version 0.10 --- 2004-05-11
;;; All public identifiers have been renamed to drop the ``@code{htmlprag:}''
;;; prefix.  The portability identifiers have been renamed to begin with an
;;; @code{htmlprag-internal:} prefix, are now considered strictly
;;; internal-use-only, and have otherwise been changed.  @code{parse-html} and
;;; @code{always-empty-html-elements} are no longer public.
;;; @code{test-htmlprag} now tests @code{html->sxml} rather than
;;; @code{parse-html}.  SISC temporarily removed from the test list, until an
;;; open source Java that works correctly is found.
;;;
;;; @item Version 0.9 --- 2004-05-07
;;; HTML encoding procedures added.  Added
;;; @code{htmlprag:sxml-html-entity-value}.  Upper-case @code{X} in hexadecimal
;;; character entities is now parsed, in addition to lower-case @code{x}.
;;; Added @code{htmlprag:always-empty-html-elements}.  Added additional
;;; portability bindings.  Added more test cases.
;;;
;;; @item Version 0.8 --- 2004-04-27
;;; Entity references (symbolic, decimal numeric, hexadecimal numeric) are now
;;; parsed into @code{*ENTITY*} SXML.  SXML symbols like @code{*TOP*} are now
;;; always upper-case, regardless of the Scheme implementation.  Identifiers
;;; such as @code{htmlprag:sxml-top-symbol} are bound to the upper-case
;;; symbols.  Procedures @code{htmlprag:html->sxml-0nf},
;;; @code{htmlprag:html->sxml-1nf}, and @code{htmlprag:html->sxml-2nf} have
;;; been added.  @code{htmlprag:html->sxml} now an alias for
;;; @code{htmlprag:html->sxml-0nf}.  @code{htmlprag:parse} has been refashioned
;;; as @code{htmlprag:parse-html} and should no longer be directly.  A number
;;; of identifiers have been renamed to be more appropriate when the
;;; @code{htmlprag:} prefix is dropped in some implementation-specific
;;; packagings of HtmlPrag: @code{htmlprag:make-tokenizer} to
;;; @code{htmlprag:make-html-tokenizer}, @code{htmlprag:parse/tokenizer} to
;;; @code{htmlprag:parse-html/tokenizer}, @code{htmlprag:html->token-list} to
;;; @code{htmlprag:tokenize-html}, @code{htmlprag:token-kind} to
;;; @code{htmlprag:sxml-token-kind}, and @code{htmlprag:test} to
;;; @code{htmlprag:test-htmlprag}.  Verbatim elements with empty-element tag
;;; syntax are handled correctly.  New versions of Bigloo and RScheme tested.
;;;
;;; @item Version 0.7 --- 2004-03-10
;;; Verbatim pair elements like @code{script} and @code{xmp} are now parsed
;;; correctly.  Two Scheme implementations have temporarily been dropped from
;;; regression testing: Kawa, due to a Java bytecode verifier error likely due
;;; to a Java installation problem on the test machine; and SXM 1.1, due to
;;; hitting a limit on the number of literals late in the test suite code.
;;; Tested newer versions of Bigloo, Chicken, Gauche, Guile, MIT Scheme, PLT
;;; MzScheme, RScheme, SISC, and STklos.  RScheme no longer requires the
;;; ``@code{(define get-output-string close-output-port)}'' workaround.
;;;
;;; @item Version 0.6 --- 2003-07-03
;;; Fixed uses of @code{eq?} in character comparisons, thanks to Scott G.
;;; Miller.  Added @code{htmlprag:html->normalized-sxml} and
;;; @code{htmlprag:html->nonnormalized-sxml}.  Started to add
;;; @code{close-output-port} to uses of output strings, then reverted due to
;;; bug in one of the supported dialects.  Tested newer versions of Bigloo,
;;; Gauche, PLT MzScheme, RScheme.
;;;
;;; @item Version 0.5 --- 2003-02-26
;;; Removed uses of @code{call-with-values}.  Re-ordered top-level definitions,
;;; for portability.  Now tests under Kawa 1.6.99, RScheme 0.7.3.2, Scheme 48
;;; 0.57, SISC 1.7.4, STklos 0.54, and SXM 1.1.
;;;
;;; @item Version 0.4 --- 2003-02-19
;;; Apostrophe-quoted element attribute values are now handled.  A bug that
;;; incorrectly assumed left-to-right term evaluation order has been fixed
;;; (thanks to MIT Scheme for confronting us with this).  Now also tests OK
;;; under Gauche 0.6.6 and MIT Scheme 7.7.1.  Portability improvement for
;;; implementations (e.g., RScheme 0.7.3.2.b6, Stalin 0.9) that cannot read
;;; @code{@@} as a symbol (although those implementations tend to present other
;;; portability issues, as yet unresolved).
;;;
;;; @item Version 0.3 --- 2003-02-05
;;; A test suite with 66 cases has been added, and necessary changes have been
;;; made for the suite to pass on five popular Scheme implementations.  XML
;;; processing instructions are now parsed.  Parent constraints have been added
;;; for @code{colgroup}, @code{tbody}, and @code{thead} elements.  Erroneous
;;; input, including invalid hexadecimal entity reference syntax and extraneous
;;; double quotes in element tags, is now parsed better.
;;; @code{htmlprag:token-kind} emits symbols more consistent with SXML.
;;;
;;; @item Version 0.2 --- 2003-02-02
;;; Portability improvements.
;;;
;;; @item Version 0.1 --- 2003-01-31
;;; Dusted off old Guile-specific code from April 2001, converted to emit SXML,
;;; mostly ported to R5RS and SRFI-6, added some XHTML support and
;;; documentation.  A little preliminary testing has been done, and the package
;;; is already useful for some applications, but this release should be
;;; considered a preview to invite comments.
;;;
;;; @end table

(#%provide (all-defined)))
