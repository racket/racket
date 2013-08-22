#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scriblib/render-cond
          scribble/core
          scribble/html-properties
          (for-syntax scheme/base)
          (for-label scheme/base
                     racket/stream))

@(define-syntax (srfi stx)
  (syntax-case stx ()
   [(_ num #:subdir subdir? . title)
    (with-syntax ([srfi/n (string->symbol (format "srfi/~a" (syntax-e #'num)))])
      #'(begin
          (section #:tag (format "srfi-~a" num)
                   #:style 'unnumbered
                   (format "SRFI ~a: " num)
                   . title)
          (defmodule srfi/n)
          "Original specification: "
          (let* ([label (format "SRFI ~a" num)]
                 [sub (if subdir? (format "srfi-~a/" num) "")]
                 [url (λ (b) (format "~a/srfi-std/~asrfi-~a.html" b sub num))])
            (cond-element
              [(or latex text) @link[(url "http://docs.racket-lang.org") label]]
              [else @link[(url ".") label]]))))]
   [(_ num . title) #'(srfi num #:subdir #f . title)]))

@;{ The `lst' argument is a list of
       (list sym syntactic-form? html-anchor) }
@(define (redirect n lst #:subdir [subdir? #f])
   (let ([file (if subdir?
                 (format "srfi-~a/srfi-~a.html" n n)
                 (format "srfi-~a.html" n))]
         [mod-path (string->symbol (format "srfi/~a" n))])
     (make-binding-redirect-elements mod-path
       (map (lambda (b)
              (list (car b) (cadr b)
                    (build-path "srfi-std" file)
                    (caddr b)))
            lst))))

@(define in-core
   (case-lambda
     [() (in-core ".")]
     [(k) @elem{This SRFI's bindings are also available in
                @racketmodname[racket/base]@|k|}]))

@(begin
  (define-syntax-rule (def-mz mz-if)
    (begin
      (require (for-label mzscheme))
      (define mz-if (racket if))))
  (def-mz mz-if))

@(define srfi-std (style #f (list (install-resource "srfi-std"))))

@; ----------------------------------------------------------------------

@title{SRFIs: Libraries}

The @link[#:style srfi-std "http://srfi.schemers.org/"]{Scheme Requests for
Implementation} (a.k.a. @deftech{SRFI}) process allows individual
members of the Scheme community to propose libraries and extensions to
be supported by multiple Scheme implementations.

Racket is distributed with implementations of many SRFIs, most of
which can be implemented as libraries. To import the bindings of SRFI
@math{n}, use

@racketblock[
(require @#,elem{@racketidfont{srfi/}@math{n}})
]

This document lists the SRFIs that are supported by Racket and
provides a link to the original SRFI specification (which is also
distributed as part of Racket's documentation).

@table-of-contents[]

@; ----------------------------------------

@srfi[1]{List Library}

This SRFI works with pairs and lists as in @racketmodname[racket],
which are immutable, so it does not export @racketidfont{set-car!} and
@racketidfont{set-cdr!}. The other provided bindings that end in
@racketidfont{!} are equivalent to the corresponding bindings without
@racketidfont{!}. Functions that are documented in the SRFI in bold
(but not bold italic) correspond to @racketmodname[racket] functions,
while the others are distinct from same-named @racketmodname[racket]
functions.

@redirect[1 '(
 (cons #f "cons")
 (list #f "list")
 (xcons #f "xcons")
 (cons* #f "cons*")
 (make-list #f "make-list")
 (list-tabulate #f "list-tabulate")
 (list-copy #f "list-copy")
 (circular-list #f "circular-list")
 (iota #f "iota")
 (pair? #f "pair-p")
 (null? #f "null-p")
 (proper-list? #f "proper-list-p")
 (circular-list? #f "circular-list-p")
 (dotted-list? #f "dotted-list-p")
 (not-pair? #f "not-pair-p")
 (null-list? #f "null-list-p")
 (car #f "car")
 (cdr #f "cdr")
 (cddadr #f "cddadr")
 (cddddr #f "cddddr")
 (list-ref #f "list-ref")
 (first #f "first")
 (second #f "second")
 (third #f "third")
 (fourth #f "fourth")
 (fifth #f "fifth")
 (sixth #f "sixth")
 (seventh #f "seventh")
 (eighth #f "eighth")
 (ninth #f "ninth")
 (tenth #f "tenth")
 (car+cdr #f "car+cdr")
 (take #f "take")
 (drop #f "drop")
 (take-right #f "take-right")
 (drop-right #f "drop-right")
 (take! #f "take!")
 (drop-right! #f "drop-right!")
 (split-at #f "split-at")
 (split-at! #f "split-at!")
 (last #f "last")
 (last-pair #f "last-pair")
 (length #f "length")
 (length+ #f "length+")
 (append #f "append")
 (append! #f "append!")
 (concatenate #f "concatenate")
 (concatenate! #f "concatenate!")
 (reverse #f "reverse")
 (reverse! #f "srfi-1.html")
 (append-reverse #f "append-reverse")
 (append-reverse! #f "append-reverse!")
 (zip #f "zip")
 (unzip1 #f "unzip1")
 (unzip2 #f "unzip2")
 (unzip3 #f "unzip3")
 (unzip4 #f "unzip4")
 (unzip5 #f "unzip5")
 (count #f "count")
 (fold #f "fold")
 (fold-right #f "fold-right")
 (pair-fold #f "pair-fold")
 (pair-fold-right #f "pair-fold-right")
 (reduce #f "reduce")
 (reduce-right #f "reduce-right")
 (unfold #f "unfold")
 (unfold-right #f "unfold-right")
 (map #f "map")
 (for-each #f "srfi-1.html")
 (append-map #f "append-map")
 (append-map! #f "append-map!")
 (map! #f "map!")
 (map-in-order #f "map-in-order")
 (pair-for-each #f "pair-for-each")
 (filter-map #f "filter-map")
 (filter #f "filter")
 (filter! #f "filter!")
 (partition! #f "partition!")
 (remove! #f "remove!")
 (remove #f "remove")
 (partition #f "partition")
 (find #f "find")
 (find-tail #f "find-tail")
 (take-while #f "take-while")
 (take-while! #f "take-while!")
 (drop-while #f "drop-while")
 (span #f "span")
 (span! #f "span!")
 (break #f "break")
 (break! #f "break!")
 (any #f "any")
 (every #f "every")
 (list-index #f "list-index")
 (member #f "member")
 (memq #f "memq")
 (memv #f "memv")
 (delete #f "delete")
 (delete-duplicates #f "delete-duplicates")
 (delete! #f "delete!")
 (delete-duplicates! #f "delete-duplicates!")
 (assoc #f "assoc")
 (assq #f "assq")
 (assv #f "assv")
 (alist-cons #f "alist-cons")
 (alist-copy #f "alist-copy")
 (alist-delete #f "alist-delete")
 (alist-delete! #f "alist-delete!")
 (lset #f "lset")
 (lset= #f "lset=")
 (lset-adjoin #f "lset-adjoin")
 (lset-union #f "lset-union")
 (lset-intersection #f "lset-intersection")
 (lset-intersection! #f "lset-intersection!")
 (lset-union! #f "lset-union!")
 (lset-difference! #f "lset-difference!")
 (lset-xor! #f "lset-xor!")
 (lset-diff+intersection! #f "lset-diff+intersection!")
 (lset-difference #f "lset-difference")
 (lset-xor #f "lset-xor")
 (lset-diff+intersection #f "lset-diff+intersection")
)]

@; ----------------------------------------

@srfi[2]{AND-LET*: an AND with local bindings...}

@redirect[2 '(
 (and-let* #t "and-let")
)]

@; ----------------------------------------

@srfi[4]{Homogeneous numeric vector datatypes}

@redirect[4 '(
 (s8vector #f "s8vector")
 (u8vector #f "s8vector")
 (s16vector #f "s8vector")
 (u16vector #f "u8vector")
 (s32vector #f "s16vector")
 (u32vector #f "u16vector")
 (s64vector #f "s64vector")
 (u64vector #f "u64vector")
 (f32vector #f "f32vector")
 (f64vector #f "f64vector")
)]

This SRFI's reader and printer syntax is not supported. The bindings
are also available from @racketmodname[scheme/foreign].

@; ----------------------------------------

@srfi[5]{A compatible let form with signatures and rest arguments}

@redirect[5 '(
 (let #t "unnamed")
)]

@; ----------------------------------------

@srfi[6]{Basic String Ports}

@redirect[6 '(
 (open-input-string #f "open-input-string")
 (open-output-string #f "open-output-string")
 (get-output-string #f "get-output-string")
)]

@in-core{}

@; ----------------------------------------

@srfi[7]{Feature-based program configuration language}

@redirect[7 '(
 (program #t "program")
)]

@; ----------------------------------------

@srfi[8]{RECEIVE: Binding to multiple values}

@redirect[8 '(
 (receive #t "receive")
)]

@; ----------------------------------------

@srfi[9]{Defining Record Types}

@redirect[9 '(
 (define-record-type #t "define-record-type")
)]

@; ----------------------------------------

@srfi[11]{Syntax for receiving multiple values}

@redirect[11 '(
 (let-values #t "let-values")
 (let*-values #t "let*-values")
)]

@in-core{, but without support for dotted ``rest'' bindings.}

@; ----------------------------------------

@srfi[13]{String Libraries}

@redirect[13 '(
 (string? #f "string-p")
 (string-null? #f "string-null-p")
 (string-every #f "string-every")
 (string-any #f "string-any")
 (make-string #f "make-string")
 (string #f "string")
 (string-tabulate #f "string-tabulate")
 (string->list #f "string2list")
 (list->string #f "list2string")
 (reverse-list->string #f "reverse-list2string")
 (string-join #f "string-join")
 (string-length #f "string-length")
 (string-ref #f "string-ref")
 (string-copy #f "string-copy")
 (substring/shared #f "substring/shared")
 (string-copy! #f "string-copy!")
 (string-take #f "string-take")
 (string-drop #f "string-drop")
 (string-take-right #f "string-take-right")
 (string-drop-right #f "string-drop-right")
 (string-pad #f "string-pad")
 (string-pad-right #f "string-pad-right")
 (string-trim #f "string-trim")
 (string-trim-right #f "string-trim-right")
 (string-trim-both #f "string-trim-both")
 (string-set! #f "string-set!")
 (string-fill! #f "string-fill!")
 (string-compare #f "string-compare")
 (string-compare-ci #f "string-compare-ci")
 (string= #f "string=")
 (string<> #f "string<>")
 (string< #f "string<")
 (string> #f "string>")
 (string<= #f "string<=")
 (string>= #f "string>=")
 (string-ci= #f "string-ci=")
 (string-ci<> #f "string-ci<>")
 (string-ci< #f "string-ci<")
 (string-ci> #f "string-ci>")
 (string-ci<= #f "string-ci<=")
 (string-ci>= #f "string-ci>=")
 (string-hash #f "string-hash")
 (string-hash-ci #f "string-hash-ci")
 (string-prefix-length #f "string-prefix-length")
 (string-suffix-length #f "string-suffix-length")
 (string-prefix-length-ci #f "string-prefix-length-ci")
 (string-suffix-length-ci #f "string-suffix-length-ci")
 (string-prefix? #f "string-prefix-p")
 (string-suffix? #f "string-suffix-p")
 (string-prefix-ci? #f "string-prefix-ci-p")
 (string-suffix-ci? #f "string-suffix-ci-p")
 (string-index #f "string-index")
 (string-index-right #f "string-index-right")
 (string-skip #f "string-skip")
 (string-skip-right #f "string-skip-right")
 (string-count #f "string-count")
 (string-contains #f "string-contains")
 (string-contains-ci #f "string-contains-ci")
 (string-titlecase #f "string-titlecase")
 (string-titlecase! #f "string-titlecase!")
 (string-upcase #f "string-upcase")
 (string-upcase! #f "string-upcase!")
 (string-downcase #f "string-downcase")
 (string-downcase! #f "string-downcase!")
 (string-reverse #f "string-reverse")
 (string-reverse! #f "string-reverse!")
 (string-append #f "string-append")
 (string-concatenate #f "string-concatenate")
 (string-concatenate/shared #f "string-concatenate/shared")
 (string-append/shared #f "string-append/shared")
 (string-concatenate-reverse #f "string-concatenate-reverse")
 (string-concatenate-reverse/shared #f "string-concatenate-reverse/shared")
 (string-map #f "string-map")
 (string-map! #f "string-map!")
 (string-fold #f "string-fold")
 (string-fold-right #f "string-fold-right")
 (string-unfold #f "string-unfold")
 (string-unfold-right #f "string-unfold-right")
 (string-for-each #f "string-for-each")
 (string-for-each-index #f "string-for-each-index")
 (xsubstring #f "xsubstring")
 (string-xcopy! #f "string-xcopy!")
 (string-replace #f "string-replace")
 (string-tokenize #f "string-tokenize")
 (string-filter #f "string-filter")
 (string-delete #f "string-delete")
 (string-parse-start+end #f "string-parse-start+end")
 (string-parse-final-start+end #f "string-parse-final-start+end")
 (let-string-start+end #f "let-string-start+end")
 (check-substring-spec #f "check-substring-spec")
 (substring-spec-ok? #f "substring-spec-ok-p")
 (make-kmp-restart-vector #f "make-kmp-restart-vector")
 (kmp-step #f "kmp-step")
 (string-kmp-partial-search #f "string-kmp-partial-search")
)]

@; ----------------------------------------

@srfi[14]{Character-set Library}

@redirect[14 '(
 (char-set? #f "char-set-p")
 (char-set= #f "char-set=")
 (char-set<= #f "char-set<=")
 (char-set-hash #f "char-set-hash")
 (char-set-cursor #f "char-set-cursor")
 (char-set-ref #f "char-set-ref")
 (char-set-cursor-next #f "char-set-cursor-next")
 (end-of-char-set? #f "end-of-char-set-p")
 (char-set-fold #f "char-set-fold")
 (char-set-unfold #f "char-set-unfold")
 (char-set-unfold! #f "char-set-unfold!")
 (char-set-for-each #f "char-set-for-each")
 (char-set-map #f "char-set-map")
 (char-set-copy #f "char-set-copy")
 (char-set #f "char-set")
 (list->char-set #f "list->char-set")
 (list->char-set! #f "list->char-set!")
 (string->char-set #f "string->char-set")
 (string->char-set! #f "string->char-set!")
 (char-set-filter #f "char-set-filter")
 (char-set-filter! #f "char-set-filter!")
 (ucs-range->char-set #f "ucs-range->char-set")
 (ucs-range->char-set! #f "ucs-range->char-set!")
 (->char-set #f "->char-set")
 (char-set->list #f "char-set->list")
 (char-set->string #f "char-set->string")
 (char-set-size #f "char-set-size")
 (char-set-count #f "char-set-count")
 (char-set-every #f "char-set-every")
 (char-set-any #f "char-set-any")
 (char-set-contains? #f "char-set-contains-p")
 (char-set-adjoin #f "char-set-adjoin")
 (char-set-delete #f "char-set-delete")
 (char-set-adjoin! #f "char-set-adjoin!")
 (char-set-delete! #f "char-set-delete!")
 (char-set-complement #f "char-set-complement")
 (char-set-union #f "char-set-union")
 (char-set-intersection #f "char-set-intersection")
 (char-set-difference #f "char-set-difference")
 (char-set-xor #f "char-set-xor")
 (char-set-diff+intersection #f "char-set-diff+intersection")
 (char-set-complement! #f "char-set-complement!")
 (char-set-union! #f "char-set-union!")
 (char-set-intersection! #f "char-set-intersection!")
 (char-set-difference! #f "char-set-difference!")
 (char-set-xor! #f "char-set-xor!")
 (char-set-diff+intersection! #f "char-set-diff+intersection!")
 (char-set:lower-case #f "char-set:lower-case")
 (char-set:upper-case #f "char-set:upper-case")
 (char-set:title-case #f "char-set:title-case")
 (char-set:letter #f "char-set:letter")
 (char-set:digit #f "char-set:digit")
 (char-set:letter+digit #f "char-set:letter+digit")
 (char-set:graphic #f "char-set:graphic")
 (char-set:printing #f "char-set:printing")
 (char-set:whitespace #f "char-set:whitespace")
 (char-set:iso-control #f "char-set:iso-control")
 (char-set:punctuation #f "char-set:punctuation")
 (char-set:symbol #f "char-set:symbol")
 (char-set:hex-digit #f "char-set:hex-digit")
 (char-set:blank #f "char-set:blank")
 (char-set:ascii #f "char-set:ascii")
 (char-set:empty #f "char-set:empty")
 (char-set:full #f "char-set:full")
)]

@; ----------------------------------------

@srfi[16]{Syntax for procedures of variable arity}

@redirect[16 '(
 (case-lambda #t "case-lambda")
)]

@in-core{}

@; ----------------------------------------

@srfi[17]{Generalized set!}

@redirect[17 '(
 (set! #t "set!")
 (getter-with-setter #f "getter-with-setter")
)]

@; ----------------------------------------

@srfi[19]{Time Data Types and Procedures}

@redirect[19 '(
 (time-duration #f "")
 (time-monotonic #f "time-monotonic")
 (time-process #f "time-process")
 (time-tai #f "time-tai")
 (time-thread #f "time-thread")
 (time-utc #f "time-utc")
 (current-date #f "")
 (current-julian-day #f "current-julian-day")
 (current-modified-julian-day #f "current-modified-julian-day")
 (current-time #f "current-time")
 (time-resolution #f "time-resolution")
 (make-time #f "make-time")
 (time? #f "time-p")
 (time-type #f "time-type")
 (time-nanosecond #f "time-nanosecond")
 (time-second #f "time-second")
 (set-time-type! #f "set-time-type!")
 (set-time-nanosecond! #f "set-time-nanosecond!")
 (set-time-second! #f "set-time-second!")
 (copy-time #f "copy-time")
 (time<=? #f "time<=-p")
 (time<? #f "time<-p")
 (time=? #f "time=-p")
 (time>=? #f "time>=-p")
 (time>? #f "time>-p")
 (time-difference #f "time-difference")
 (time-difference! #f "time-difference!")
 (add-duration #f "add-duration")
 (add-duration! #f "add-duration!")
 (subtract-duration #f "subtract-duration")
 (subtract-duration! #f "subtract-duration!")
 (make-date #f "make-date")
 (date? #f "date-p")
 (date-nanosecond #f "date-nanosecond")
 (date-second #f "date-second")
 (date-minute #f "date-minute")
 (date-hour #f "date-hour")
 (date-day #f "date-day")
 (date-month #f "date-month")
 (date-year #f "date-year")
 (date-zone-offset #f "date-zone-offset")
 (date-year-day #f "date-year-day")
 (date-week-day #f "date-week-day")
 (date-week-number #f "date-week-number")
 (date->julian-day #f "date->julian-day")
 (date->modified-julian-day #f "date->modified-julian-day")
 (date->time-monotonic #f "date->time-monotonic")
 (date->time-tai #f "date->time-tai")
 (date->time-utc #f "date->time-utc")
 (julian-day->date #f "julian-day->date")
 (julian-day->time-monotonic #f "julian-day->time-monotonic")
 (julian-day->time-tai #f "julian-day->time-tai")
 (julian-day->time-utc #f "julian-day->time-utc")
 (modified-julian-day->date #f "modified-julian-day->date")
 (modified-julian-day->time-monotonic #f "modified-julian-day->time-monotonic")
 (modified-julian-day->time-tai #f "modified-julian-day->time-tai")
 (modified-julian-day->time-utc #f "modified-julian-day->time-utc")
 (time-monotonic->date #f "time-monotonic->date")
 (time-monotonic->julian-day #f "time-monotonic->julian-day")
 (time-monotonic->modified-julian-day #f "time-monotonic->modified-julian-day")
 (time-monotonic->time-tai #f "time-monotonic->time-tai")
 (time-monotonic->time-tai! #f "time-monotonic->time-tai!")
 (time-monotonic->time-utc #f "time-monotonic->time-utc")
 (time-monotonic->time-utc! #f "time-monotonic->time-utc!")
 (time-tai->date #f "time-tai->date")
 (time-tai->julian-day #f "time-tai->julian-day")
 (time-tai->modified-julian-day #f "time-tai->modified-julian-day")
 (time-tai->time-monotonic #f "time-tai->time-monotonic")
 (time-tai->time-monotonic! #f "time-tai->time-monotonic!")
 (time-tai->time-utc #f "time-tai->time-utc")
 (time-tai->time-utc! #f "time-tai->time-utc!")
 (time-utc->date #f "time-utc->date")
 (time-utc->julian-day #f "time-utc->julian-day")
 (time-utc->modified-julian-day #f "time-utc->modified-julian-day")
 (time-utc->time-monotonic #f "time-utc->time-monotonic")
 (time-utc->time-monotonic! #f "time-utc->time-monotonic!")
 (time-utc->time-tai #f "time-utc->time-tai")
 (time-utc->time-tai! #f "time-utc->time-tai!")
 (date->string #f "date->string")
 (string->date #f "string->date")
)]

The date structure produced by this SRFI library is identical
to the one provided by @racketmodname[racket/base] in most cases
(see @racket[date]).

For backwards compatibility, when an invalid date field value is
provided to the SRFI constructor, the constructor will produce a lax
date structure. A lax date structure is @emph{not} compatible with
functions from @racketmodname[racket/base] or
@racketmodname[racket/date]. SRFI functions such as
@racket[string->date] may return a lax date structure depending on the
format string.

@(define srfi-19-eval (make-base-eval))
@(srfi-19-eval '(require srfi/19))

@defproc[(lax-date? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[_v] is a lax date structure. Otherwise
returns @racket[#f].

@examples[#:eval srfi-19-eval
  (lax-date? (make-date 0 19 10 10 14 "bogus" "bogus" 0))
  (lax-date? (make-date 0 19 10 10 14 1 2013 0))
  (lax-date? (string->date "10:21:00" "~H:~M:~S"))
]}

@(close-eval srfi-19-eval)

@; ----------------------------------------

@srfi[23]{Error reporting mechanism}

@redirect[23 '(
 (error #f "error")
)]

@in-core{}

@; ----------------------------------------

@srfi[25]{Multi-dimensional Array Primitives}

@redirect[25 '(
 (array? #f "array-p")
 (make-array #f "make-array")
 (shape #f "shape")
 (array #f "array")
 (array-rank #f "array-rank")
 (array-start #f "array-start")
 (array-end #f "array-end")
 (array-ref #f "array-ref")
 (array-set! #f "array-set!")
 (share-array #f "share-array")
)]

@; ----------------------------------------

@srfi[26]{Notation for Specializing Parameters without Currying}

@redirect[26 '(
 (cut #t "cut")
 (cute #t "cute")
)]


@; ----------------------------------------

@srfi[27]{Sources of Random Bits}

@redirect[27 '(
 (random-integer #f "random-integer")
 (random-real #f "random-real")
 (default-random-source #f "default-random-source")
 (make-random-source #f "make-random-source")
 (random-source? #f "random-source-p")
 (random-source-state-ref #f "random-source-state-ref")
 (random-source-state-ref! #f "random-source-state-ref")
 (random-source-randomize! #f "random-source-randomize!")
 (random-source-pseudo-randomize! #f "random-source-pseudo-randomize!")
 (random-source-make-integers #f "random-source-make-integers")
 (random-source-make-reals #f "random-source-make-reals")
)]

@; ----------------------------------------

@srfi[28]{Basic Format Strings}

@redirect[28 '(
 (format #f "format")
)]

@in-core{}

@; ----------------------------------------

@srfi[29]{Localization}

@redirect[29 '(
 (current-language #f "current-language")
 (current-country #f "current-country")
 (current-locale-details #f "current-locale-details")
 (declare-bundle! #f "declare-bundle!")
 (store-bundle #f "store-bundle")
 (load-bundle! #f "load-bundle!")
 (localized-template #f "localized-template")
)]

@; ----------------------------------------

@srfi[30]{Nested Multi-line Comments}

This SRFI's syntax is part of Racket's default reader.

@; ----------------------------------------

@srfi[31]{A special form rec for recursive evaluation}

@redirect[31 '(
 (rec #t "rec")
)]

@; ----------------------------------------

@srfi[34]{Exception Handling for Programs}

@redirect[34 '(
 (with-exception-handler #f "with-exception-handler")
 (guard #t "guard")
 (raise #f "raise")
)]

@; ----------------------------------------

@srfi[35]{Conditions}

@; ----------------------------------------

@srfi[38]{External Representation for Data With Shared Structure}

@redirect[38 '(
 (write-with-shared-structure #f "write-with-shared-structure")
 (read-with-shared-structure #f "read-with-shared-structure")
)]

This SRFI's syntax is part of Racket's default reader and printer.

@; ----------------------------------------

@srfi[39]{Parameter objects}

@redirect[39 '(
 (make-parameter #f "make-parameter")
 (parameterize #t "parameterize")
)]

@in-core{}

@; ----------------------------------------

@srfi[40]{A Library of Streams}

@redirect[40 '(
 (stream-cons #t "stream-cons")
 (stream? #f "stream?")
 (stream-null? #f "stream-null?")
 (stream-car #f "stream-car")
 (stream-cdr #f "stream-cdr")
 (stream-delay #t "stream-delay")
 (stream-null #f "stream-null")
 (stream #f "stream")
 (stream-unfoldn #f "stream-unfoldn")
 (stream-map #f "stream-map")
 (stream-for-each #f "stream-for-each")
 (stream-filter #f "stream-filter")
)]

Superceded by @racketmodname[srfi/41].

@; ----------------------------------------

@srfi[41 #:subdir #t]{Streams}

The @racket[stream-cons] operation from @racketmodname[srfi/41] is the
same as from @racketmodname[racket/stream].

@redirect[41 #:subdir #t '(
 (stream-null #f "stream-null")
 (stream-cons #t "stream-cons")
 (stream? #f "stream?")
 (stream-null? #f "stream-null?")
 (stream-pair? #f "stream-pair?")
 (stream-car #f "stream-car")
 (stream-cdr #f "stream-cdr")
 (stream-lambda #t "stream-lambda")
 (define-stream #t "define-stream")
 (list->stream #f "list-to-stream")
 (port->stream #f "port-to-stream")
 (stream #t "stream")
 (stream->list #f "stream-to-list")
 (stream-append #f "stream-append")
 (stream-concat #f "stream-concat")
 (stream-constant #f "stream-constant")
 (stream-drop #f "stream-drop")
 (stream-drop-while #f "stream-drop-while")
 (stream-filter #f "stream-filter")
 (stream-fold #f "stream-fold")
 (stream-for-each #f "stream-for-each")
 (stream-from #f "stream-from")
 (stream-iterate #f "stream-iterate")
 (stream-length #f "stream-length")
 (stream-let #t "stream-let")
 (stream-map #f "stream-map")
 (stream-match #t "stream-match")
 (stream-of #t "stream-of")
 (stream-range #f "stream-range")
 (stream-ref #f "stream-ref")
 (stream-reverse #f "stream-reverse")
 (stream-scan #f "stream-scan")
 (stream-take #f "stream-take")
 (stream-take-while #f "stream-take-while")
 (stream-unfold #f "stream-unfold")
 (stream-zip #f "stream-zip")
)]

@; ----------------------------------------

@srfi[42]{Eager Comprehensions}

@redirect[42 '(
 (do-ec #t "do-ec")
 (list-ec #t "list-ec")
 (append-ec #t "append-ec")
 (string-ec #t "string-ec")
 (string-append-ec #t "string-append-ec")
 (vector-ec #t "vector-ec")
 (vector-of-length-ec #t "vector-of-length-ec")
 (sum-ec #t "sum-ec")
 (product-ec #t "product-ec")
 (min-ec #t "min-ec")
 (max-ec #t "max-ec")
 (any?-ec #t "any?-ec")
 (every?-ec #t "every?-ec")
 (first-ec #t "first-ec")
 (last-ec #t "last-ec")
 (fold-ec #t "fold-ec")
 (fold3-ec #t "fold3-ec")
 (generator #t "generator")
 (if #t "if")
 (not #t "not")
 (and #t "and")
 (or #t "or")
 (begin #t "begin")
 (nested #t "nested")
 (: #t ":")
 (:list #t ":list")
 (:string #t ":string")
 (:vector #t ":vector")
 (:integers #t ":integers")
 (:range #t ":range")
 (:real-range #t ":real-range")
 (:char-range #t ":char-range")
 (:port #t ":port")
 (:dispatched #t "")
 (:generator-proc #t ":generator-proc")
 (:do #t ":do")
 (:let #t ":let")
 (:parallel #t ":parallel")
 (:while #t ":while")
 (:until #t ":until")
)]

Forms that syntactically detect @racket[if] recognize both @racket[if]
from @racketmodname[scheme/base] and @mz-if from
@racketmodname[mzscheme].

@; ----------------------------------------

@srfi[43]{Vector Library}

@redirect[43 '(
 (make-vector #f "make-vector")
 (vector #f "vector")
 (vector-unfold #f "vector-unfold")
 (vector-unfold-right #f "vector-unfold-right")
 (vector-copy #f "vector-copy")
 (vector-reverse-copy #f "vector-reverse-copy")
 (vector-append #f "vector-append")
 (vector-concatenate #f "vector-concatenate")
 (vector? #f "vector-p")
 (vector-empty? #f "vector-empty?")
 (vector= #f "vector-eq")
 (vector-ref #f "vector-ref")
 (vector-length #f "vector-length")
 (vector-fold #f "vector-fold")
 (vector-fold-right #f "vector-fold-right")
 (vector-map #f "vector-map")
 (vector-map! #f "vector-map-bang")
 (vector-for-each #f "vector-for-each")
 (vector-count #f "vector-count")
 (vector-index #f "vector-index")
 (vector-index-right #f "vector-index-right")
 (vector-skip #f "vector-skip")
 (vector-skip-right #f "vector-skip-right")
 (vector-binary-search #f "vector-binary-search")
 (vector-any #f "vector-any")
 (vector-every #f "vector-every")
 (vector-set! #f "vector-set-bang")
 (vector-swap! #f "vector-swap-bang")
 (vector-fill! #f "vector-fill-bang")
 (vector-reverse! #f "vector-reverse-bang")
 (vector-copy! #f "vector-copy-bang")
 (vector-reverse-copy! #f "vector-reverse-copy-bang")
 (vector->list #f "vector->list")
 (reverse-vector->list #f "reverse-vector->list")
 (list->vector #f "list->vector")
 (reverse-list->vector #f "reverse-list->vector")
)]

@; ----------------------------------------

@srfi[45]{Primitives for Expressing Iterative Lazy Algorithms}

@redirect[45 '(
 (delay #t "delay")
 (lazy #t "lazy")
 (force #f "force")
 (eager #f "eager")
)]

Additional binding:

@defproc[(promise? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a promise, @racket[#f] otherwise.}

@; ----------------------------------------

@srfi[48]{Intermediate Format Strings}

@redirect[48 '(
 (format #f "format")
)]

@; ----------------------------------------

@srfi[54]{Formatting}

@; ----------------------------------------

@srfi[57]{Records}

@; ----------------------------------------

@srfi[59]{Vicinity}

@redirect[59 '(
 (program-vicinity #f "program-vicinity")
 (library-vicinity #f "library-vicinity")
 (implementation-vicinity #f "implementation-vicinity")
 (user-vicinity #f "user-vicinity")
 (home-vicinity #f "home-vicinity")
 (in-vicinity #f "in-vicinity")
 (sub-vicinity #f "sub-vicinity")
 (make-vicinity #f "make-vicinity")
 (pathname->vicinity #f "pathname-to-vicinity")
 (vicinity:suffix? #f "vicinity:suffix-p")
)]

@; ----------------------------------------

@srfi[60]{Integers as Bits}

@; ----------------------------------------

@srfi[61]{A more general cond clause}

@; ----------------------------------------

@; no actual library for this
@section[#:tag "srfi-62" #:style 'unnumbered]{SRFI 62: S-expression comments}

Original specification: @link["../srfi-std/srfi-62.html"]{SRFI 62}

This SRFI's syntax is part of Racket's default reader (no
@racket[require] is needed).

@; ----------------------------------------

@srfi[63]{Homogeneous and Heterogeneous Arrays}

@; ----------------------------------------

@srfi[64]{A Scheme API for test suites}

@; ----------------------------------------

@srfi[66]{Octet Vectors}

@; ----------------------------------------

@srfi[67 #:subdir #t]{Compare Procedures}

@; ----------------------------------------

@srfi[69]{Basic hash tables}

@; ----------------------------------------

@srfi[71]{Extended LET-syntax for multiple values}

@; ----------------------------------------

@srfi[74]{Octet-Addressed Binary Blocks}

@; ----------------------------------------

@srfi[78]{Lightweight testing}

@; ----------------------------------------

@srfi[86]{MU & NU simulating VALUES & CALL-WITH-VALUES...}

@; ----------------------------------------

@srfi[87]{=> in case clauses}

@; ----------------------------------------

@srfi[98]{An interface to access environment variables}

@redirect[98 '((get-environment-variable #f "get-environment-variable")
               (get-environment-variables #f "get-environment-variables"))]

@; ----------------------------------------

@index-section[]
