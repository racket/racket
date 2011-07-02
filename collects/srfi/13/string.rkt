;;;
;;; <string.rkt> ---- SRFI 13 port to PLT Scheme
;;; Revised by Chongkai Zhu, based on the orgianl port
;;;
;;; Here is the copyright notice, and licence from the original source:

;;; Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; Copyright (c) 1998, 1999, 2000 Olin Shivers. All rights reserved.
;;; Copyright details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The prefix/suffix and comparison routines in this code had (extremely
;;; distant) origins in MIT Scheme's string lib, and was substantially
;;; reworked by Olin Shivers (shivers@ai.mit.edu) 9/98. As such, it is
;;; covered by MIT Scheme's open source copyright. See below for details.
;;;
;;; The KMP string-search code was influenced by implementations written
;;; by Stephen Bevan, Brian Dehneyer and Will Fitzgerald. However, this
;;; version was written from scratch by myself.
;;;
;;; The remainder of this code was written from scratch by myself for scsh.
;;; The scsh copyright is a BSD-style open source copyright. See below for
;;; details.
;;;     -Olin Shivers

;; Olin Shivers verified that he is fine with redistributing this code
;; under the LGPL.  (Verified personally by Eli Barzilay.)

;;; MIT Scheme copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy and modify this software, to
;;; redistribute either the original software or a modified version, and
;;; to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright notice
;;; in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation of
;;; this software will be error-free, and MIT is under no obligation to
;;; provide any services, by way of maintenance, update, or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case.

;;; Scsh copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Are you still here? Cool, keep reading it gets better:

#lang mzscheme

(require srfi/optional
         srfi/8/receive
         srfi/14/char-set
         mzlib/etc ; for opt-lambda (instead of let-optionals*)
         )
(provide
 ;; String procedures:
 string-map string-map!
 string-fold string-unfold
 string-fold-right string-unfold-right
 string-tabulate string-for-each string-for-each-index
 string-every string-any
 string-hash string-hash-ci
 string-compare string-compare-ci
 string=    string<    string>    string<=    string>=    string<>
 string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>
 s:string-downcase  s:string-upcase  s:string-titlecase
 string-downcase! string-upcase! string-titlecase!
 string-take string-take-right
 string-drop string-drop-right
 string-pad string-pad-right
 string-trim string-trim-right string-trim-both
 string-filter string-delete
 string-index string-index-right
 string-skip  string-skip-right
 string-count
 string-prefix-length string-prefix-length-ci
 string-suffix-length string-suffix-length-ci
 string-prefix? string-prefix-ci?
 string-suffix? string-suffix-ci?
 string-contains string-contains-ci
 substring/shared
 string-reverse string-reverse! reverse-list->string
 string-concatenate string-concatenate/shared string-concatenate-reverse
 string-concatenate-reverse/shared
 string-append/shared
 xsubstring string-xcopy!
 string-null?
 string-join
 string-tokenize
 string-replace

 s:string-fill!
 s:string->list
 ;; R5RS re-exports:
 ;; string-copy
 ;; string? make-string string-length string-ref string-set!
 ;; R5RS re-exports (also defined here but commented-out):
 ;; string string-append list->string

 ;; Low level routines:
 make-kmp-restart-vector string-kmp-partial-search kmp-step
 string-parse-start+end
 string-parse-final-start+end
 let-string-start+end
 check-substring-spec
 substring-spec-ok?
 )


;; Support for START/END substring specs
;;
;; This macro parses optional start/end arguments from arg lists, defaulting
;; them to 0/(string-length s), and checks them for correctness.

(define-syntax let-string-start+end
  (syntax-rules ()
    ((let-string-start+end (start end) proc s-exp args-exp body ...)
     (receive (start end) (string-parse-final-start+end proc s-exp args-exp)
       body ...))
    ((let-string-start+end (start end rest) proc s-exp args-exp body ...)
     (receive (rest start end) (string-parse-start+end proc s-exp args-exp)
       body ...))))

;; This one parses out a *pair* of final start/end indices.
;; Not exported; for internal use.
(define-syntax let-string-start+end2
  (syntax-rules ()
    ((l-s-s+e2 (start1 end1 start2 end2) proc s1 s2 args body ...)
     (let ((procv proc))        ; Make sure PROC is only evaluated once.
       (let-string-start+end (start1 end1 rest) procv s1 args
         (let-string-start+end (start2 end2) procv s2 rest
           body ...))))))


;; Returns three values: rest start end

(define (string-parse-start+end proc s args)
  (if (not (string? s)) (error proc "Non-string value ~a" s))
  (let ((slen (string-length s)))
    (if (pair? args)

      (let ((start (car args))
            (args (cdr args)))
        (if (and (integer? start) (exact? start) (>= start 0))
          (receive (end args)
                   (if (pair? args)
                     (let ((end (car args))
                           (args (cdr args)))
                       (if (and (integer? end) (exact? end) (<= end slen))
                         (values end args)
                         (error proc "Illegal substring END spec" end s)))
                     (values slen args))
            (if (<= start end)
              (values args start end)
              (error proc "Illegal substring START/END spec" start end s)))
          (error proc "Illegal substring START spec" start s)))

      (values '() 0 slen))))

(define (string-parse-final-start+end proc s args)
  (receive (rest start end) (string-parse-start+end proc s args)
    (if (pair? rest)
      (error proc "Extra arguments to procedure" rest)
      (values start end))))

(define (substring-spec-ok? s start end)
  (and (string? s)
       (integer? start)
       (exact? start)
       (integer? end)
       (exact? end)
       (<= 0 start)
       (<= start end)
       (<= end (string-length s))))

(define (check-substring-spec proc s start end)
  (unless (substring-spec-ok? s start end)
    (error "Illegal substring spec." proc s start end)))


;; Defined by R5RS, so commented out here.
;(define (string . chars)
;  (let* ((len (length chars))
;         (ans (make-string len)))
;    (do ((i 0 (+ i 1))
;        (chars chars (cdr chars)))
;       ((>= i len))
;      (string-set! ans i (car chars)))
;    ans))
;
;(define (string . chars) (string-unfold null? car cdr chars))



;; substring/shared S START [END]
;; string-copy      S [START END]
;;

;; All this goop is just arg parsing & checking surrounding a call to the
;; actual primitive, %SUBSTRING/SHARED.

(define (substring/shared s start . maybe-end)
  (check-arg string? s 'substring/shared)
  (let ((slen (string-length s)))
    (check-arg (lambda (start) (and (integer? start) (exact? start) (<= 0 start)))
               start 'substring/shared)
    (%substring/shared s start
                       (:optional maybe-end slen
                                  (lambda (end)
                                    (and (integer? end)
                                         (exact? end)
                                         (<= start end)
                                         (<= end slen)))))))

;; Split out so that other routines in this library can avoid arg-parsing
;; overhead for END parameter.
(define (%substring/shared s start end)
  (if (and (zero? start) (= end (string-length s)))
    s
    (substring s start end)))

;; string-copy exists in PLT Scheme, so, we'll use that,
;; hopefully faster implementation.
;; (define (string-copy s . maybe-start+end)
;;   (let-string-start+end (start end) string-copy s maybe-start+end
;;     (substring s start end)))

;; This library uses the R5RS SUBSTRING, but doesn't export it.
;; Here is a definition, just for completeness.
;; (define (substring s start end)
;;   (check-substring-spec substring s start end)
;;   (let* ((slen (- end start))
;;          (ans (make-string slen)))
;;     (do ((i 0 (+ i 1))
;;          (j start (+ j 1)))
;;         ((>= i slen) ans)
;;       (string-set! ans i (string-ref s j)))))

;; Basic iterators and other higher-order abstractions
;; (string-map proc s [start end])
;; (string-map! proc s [start end])
;; (string-fold kons knil s [start end])
;; (string-fold-right kons knil s [start end])
;; (string-unfold       p f g seed [base make-final])
;; (string-unfold-right p f g seed [base make-final])
;; (string-for-each       proc s [start end])
;; (string-for-each-index proc s [start end])
;; (string-every char-set/char/pred s [start end])
;; (string-any   char-set/char/pred s [start end])
;; (string-tabulate proc len)
;;
;; You want compiler support for high-level transforms on fold and unfold ops.
;; You'd at least like a lot of inlining for clients of these procedures.
;; Don't hold your breath.

(define (string-map proc s . maybe-start+end)
  (check-arg procedure? proc 'string-map)
  (let-string-start+end (start end) 'string-map s maybe-start+end
    (%string-map proc s start end)))

(define (%string-map proc s start end) ; Internal utility
  (let* ((len (- end start))
         (ans (make-string len)))
    (do ((i (- end 1) (- i 1))
         (j (- len 1) (- j 1)))
        ((< j 0))
      (string-set! ans j (proc (string-ref s i))))
    ans))

(define (string-map! proc s . maybe-start+end)
  (check-arg procedure? proc 'string-map!)
  (let-string-start+end (start end) 'string-map! s maybe-start+end
    (%string-map! proc s start end)))

(define (%string-map! proc s start end)
  (do ((i (- end 1) (- i 1)))
      ((< i start))
    (string-set! s i (proc (string-ref s i)))))

(define (string-fold kons knil s . maybe-start+end)
  (check-arg procedure? kons 'string-fold)
  (let-string-start+end (start end) 'string-fold s maybe-start+end
    (let lp ((v knil) (i start))
      (if (< i end)
        (lp (kons (string-ref s i) v) (+ i 1))
        v))))

(define (string-fold-right kons knil s . maybe-start+end)
  (check-arg procedure? kons 'string-fold-right)
  (let-string-start+end (start end) 'string-fold-right s maybe-start+end
    (let lp ((v knil) (i (- end 1)))
      (if (>= i start)
        (lp (kons (string-ref s i) v) (- i 1))
        v))))

;; (string-unfold p f g seed [base make-final])
;; This is the fundamental constructor for strings.
;; - G is used to generate a series of "seed" values from the initial seed:
;;     SEED, (G SEED), (G^2 SEED), (G^3 SEED), ...
;; - P tells us when to stop -- when it returns true when applied to one
;;   of these seed values.
;; - F maps each seed value to the corresponding character
;;   in the result string. These chars are assembled into the
;;   string in a left-to-right order.
;; - BASE is the optional initial/leftmost portion of the constructed string;
;;   it defaults to the empty string "".
;; - MAKE-FINAL is applied to the terminal seed value (on which P returns
;;   true) to produce the final/rightmost portion of the constructed string.
;;   It defaults to (LAMBDA (X) "").
;;
;; In other words, the following (simple, inefficient) definition holds:
;; (define (string-unfold p f g seed base make-final)
;;   (string-append base
;;                  (let recur ((seed seed))
;;                    (if (p seed) (make-final seed)
;;                        (string-append (string (f seed))
;;                                       (recur (g seed)))))))
;;
;; STRING-UNFOLD is a fairly powerful constructor -- you can use it to
;; reverse a string, copy a string, convert a list to a string, read
;; a port into a string, and so forth. Examples:
;; (port->string port) =
;;   (string-unfold (compose eof-object? peek-char)
;;                  read-char values port)
;;
;; (list->string lis) = (string-unfold null? car cdr lis)
;;
;; (tabulate-string f size) = (string-unfold (lambda (i) (= i size)) f add1 0)

;; A problem with the following simple formulation is that it pushes one
;; stack frame for every char in the result string -- an issue if you are
;; using it to read a 100kchar string. So we don't use it -- but I include
;; it to give a clear, straightforward description of what the function
;; does.

;(define (string-unfold p f g seed base make-final)
;  (let ((ans (let recur ((seed seed) (i (string-length base)))
;               (if (p seed)
;                   (let* ((final (make-final seed))
;                          (ans (make-string (+ i (string-length final)))))
;                     (string-copy! ans i final)
;                     ans)
;
;                   (let* ((c (f seed))
;                          (s (recur (g seed) (+ i 1))))
;                     (string-set! s i c)
;                     s)))))
;    (string-copy! ans 0 base)
;    ans))

;; The strategy is to allocate a series of chunks into which we stash the
;; chars as we generate them. Chunk size goes up in powers of two starting
;; with 40 and levelling out at 4k, i.e.
;;     40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
;; This should work pretty well for short strings, 1-line (80 char) strings,
;; and longer ones. When done, we allocate an answer string and copy the
;; chars over from the chunk buffers.

(define string-unfold
  (opt-lambda (p f g seed (base "") (make-final (lambda (x) "")))
    (check-arg procedure? p 'string-unfold)
    (check-arg procedure? f 'string-unfold)
    (check-arg procedure? g 'string-unfold)
    (check-arg string? base 'string-unfold)
    (check-arg procedure? make-final 'string-unfold)
    (let lp ((chunks '())             ; Previously filled chunks
             (nchars 0)               ; Number of chars in CHUNKS
             (chunk (make-string 40)) ; Current chunk into which we write
             (chunk-len 40)
             (i 0)                    ; Number of chars written into CHUNK
             (seed seed))
      (let lp2 ((i i) (seed seed))
        (if (not (p seed))
          (let ((c (f seed))
                (seed (g seed)))
            (if (< i chunk-len)
              (begin (string-set! chunk i c)
                     (lp2 (+ i 1) seed))

              (let* ((nchars2 (+ chunk-len nchars))
                     (chunk-len2 (min 4096 nchars2))
                     (new-chunk (make-string chunk-len2)))
                (string-set! new-chunk 0 c)
                (lp (cons chunk chunks) (+ nchars chunk-len)
                    new-chunk chunk-len2 1 seed))))

          ;; We're done. Make the answer string & install the bits.
          (let* ((final (make-final seed))
                 (flen (string-length final))
                 (base-len (string-length base))
                 (j (+ base-len nchars i))
                 (ans (make-string (+ j flen))))
            (%string-copy! ans j final 0 flen) ; Install FINAL.
            (let ((j (- j i)))
              (%string-copy! ans j chunk 0 i) ; Install CHUNK[0,I).
              (let lp ((j j) (chunks chunks)) ; Install CHUNKS.
                (if (pair? chunks)
                  (let* ((chunk  (car chunks))
                         (chunks (cdr chunks))
                         (chunk-len (string-length chunk))
                         (j (- j chunk-len)))
                    (%string-copy! ans j chunk 0 chunk-len)
                    (lp j chunks)))))
            (%string-copy! ans 0 base 0 base-len) ; Install BASE.
            ans))))))

(define string-unfold-right
  (opt-lambda (p f g seed (base "") (make-final (lambda (x) "")))
    (check-arg string? base 'string-unfold-right)
    (check-arg procedure? make-final 'string-unfold-right)
    (let lp ((chunks '())             ; Previously filled chunks
             (nchars 0)               ; Number of chars in CHUNKS
             (chunk (make-string 40)) ; Current chunk into which we write
             (chunk-len 40)
             (i 40)                   ; Number of chars available in CHUNK
             (seed seed))
      (let lp2 ((i i) (seed seed))    ; Fill up CHUNK from right
        (if (not (p seed))            ; to left.
          (let ((c (f seed))
                (seed (g seed)))
            (if (> i 0)
              (let ((i (- i 1)))
                (string-set! chunk i c)
                (lp2 i seed))

              (let* ((nchars2 (+ chunk-len nchars))
                     (chunk-len2 (min 4096 nchars2))
                     (new-chunk (make-string chunk-len2))
                     (i (- chunk-len2 1)))
                (string-set! new-chunk i c)
                (lp (cons chunk chunks) (+ nchars chunk-len)
                    new-chunk chunk-len2 i seed))))

          ;; We're done. Make the answer string & install the bits.
          (let* ((final (make-final seed))
                 (flen (string-length final))
                 (base-len (string-length base))
                 (chunk-used (- chunk-len i))
                 (j (+ base-len nchars chunk-used))
                 (ans (make-string (+ j flen))))
            (%string-copy! ans 0 final 0 flen) ; Install FINAL.
            (%string-copy! ans flen chunk i chunk-len) ; Install CHUNK[I,).
            (let lp ((j (+ flen chunk-used)) ; Install CHUNKS.
                     (chunks chunks))
              (if (pair? chunks)
                (let* ((chunk  (car chunks))
                       (chunks (cdr chunks))
                       (chunk-len (string-length chunk)))
                  (%string-copy! ans j chunk 0 chunk-len)
                  (lp (+ j chunk-len) chunks))
                (%string-copy! ans j base 0 base-len))) ; Install BASE.
            ans))))))


(define (string-for-each proc s . maybe-start+end)
  (check-arg procedure? proc 'string-for-each)
  (let-string-start+end (start end) 'string-for-each s maybe-start+end
    (let lp ((i start))
      (when (< i end)
        (proc (string-ref s i))
        (lp (+ i 1))))))

(define (string-for-each-index proc s . maybe-start+end)
  (check-arg procedure? proc 'string-for-each-index)
  (let-string-start+end (start end) 'string-for-each-index s maybe-start+end
    (let lp ((i start))
      (when (< i end) (proc i) (lp (+ i 1))))))

(define (string-every criterion s . maybe-start+end)
  (let-string-start+end (start end) 'string-every s maybe-start+end
    (cond ((char? criterion)
           (let lp ((i start))
             (or (>= i end)
                 (and (char=? criterion (string-ref s i))
                      (lp (+ i 1))))))

          ((char-set? criterion)
           (let lp ((i start))
             (or (>= i end)
                 (and (char-set-contains? criterion (string-ref s i))
                      (lp (+ i 1))))))

          ((procedure? criterion) ; Slightly funky loop so that
           (or (= start end) ; final (PRED S[END-1]) call
               (let lp ((i start)) ; is a tail call.
                 (let ((c (string-ref s i))
                       (i1 (+ i 1)))
                   (if (= i1 end) (criterion c) ; Tail call.
                       (and (criterion c) (lp i1)))))))

          (else (error 'string-every "Second param is neither char-set, char, or predicate procedure: ~a" criterion)))))


(define (string-any criterion s . maybe-start+end)
  (let-string-start+end (start end) 'string-any s maybe-start+end
    (cond ((char? criterion)
           (let lp ((i start))
             (and (< i end)
                  (or (char=? criterion (string-ref s i))
                      (lp (+ i 1))))))

          ((char-set? criterion)
           (let lp ((i start))
             (and (< i end)
                  (or (char-set-contains? criterion (string-ref s i))
                      (lp (+ i 1))))))

          ((procedure? criterion) ; Slightly funky loop so that
           (and (< start end) ; final (PRED S[END-1]) call
                (let lp ((i start)) ; is a tail call.
                  (let ((c (string-ref s i))
                        (i1 (+ i 1)))
                    (if (= i1 end) (criterion c) ; Tail call
                        (or (criterion c) (lp i1)))))))

          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-any criterion)))))


(define (string-tabulate proc len)
  (check-arg procedure? proc 'string-tabulate)
  (check-arg (lambda (val) (and (integer? val) (exact? val) (<= 0 val)))
             len 'string-tabulate)
  (let ((s (make-string len)))
    (do ((i (- len 1) (- i 1)))
        ((< i 0))
      (string-set! s i (proc i)))
    s))



;; string-prefix-length[-ci] s1 s2 [start1 end1 start2 end2]
;; string-suffix-length[-ci] s1 s2 [start1 end1 start2 end2]
;;
;; Find the length of the common prefix/suffix.
;; It is not required that the two substrings passed be of equal length.
;; This was microcode in MIT Scheme -- a very tightly bummed primitive.
;; %STRING-PREFIX-LENGTH is the core routine of all string-comparisons,
;; so should be as tense as possible.

(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
         (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2)) ; EQ fast path
      delta

      (let lp ((i start1) (j start2)) ; Regular path
        (if (or (>= i end1)
                (not (char=? (string-ref s1 i)
                             (string-ref s2 j))))
          (- i start1)
          (lp (+ i 1) (+ j 1)))))))

(define (%string-suffix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
         (start1 (- end1 delta)))

    (if (and (eq? s1 s2) (= end1 end2)) ; EQ fast path
      delta

      (let lp ((i (- end1 1)) (j (- end2 1))) ; Regular path
        (if (or (< i start1)
                (not (char=? (string-ref s1 i)
                             (string-ref s2 j))))
          (- (- end1 i) 1)
          (lp (- i 1) (- j 1)))))))

(define (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
         (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2)) ; EQ fast path
      delta

      (let lp ((i start1) (j start2)) ; Regular path
        (if (or (>= i end1)
                (not (char-ci=? (string-ref s1 i)
                                (string-ref s2 j))))
          (- i start1)
          (lp (+ i 1) (+ j 1)))))))

(define (%string-suffix-length-ci s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
         (start1 (- end1 delta)))

    (if (and (eq? s1 s2) (= end1 end2)) ; EQ fast path
      delta

      (let lp ((i (- end1 1)) (j (- end2 1))) ; Regular path
        (if (or (< i start1)
                (not (char-ci=? (string-ref s1 i)
                                (string-ref s2 j))))
          (- (- end1 i) 1)
          (lp (- i 1) (- j 1)))))))


(define (string-prefix-length s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-prefix-length s1 s2 maybe-starts+ends
    (%string-prefix-length s1 start1 end1 s2 start2 end2)))

(define (string-suffix-length s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-suffix-length s1 s2 maybe-starts+ends
    (%string-suffix-length s1 start1 end1 s2 start2 end2)))

(define (string-prefix-length-ci s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-prefix-length-ci s1 s2 maybe-starts+ends
    (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)))

(define (string-suffix-length-ci s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-suffix-length-ci s1 s2 maybe-starts+ends
    (%string-suffix-length-ci s1 start1 end1 s2 start2 end2)))


;; string-prefix?    s1 s2 [start1 end1 start2 end2]
;; string-suffix?    s1 s2 [start1 end1 start2 end2]
;; string-prefix-ci? s1 s2 [start1 end1 start2 end2]
;; string-suffix-ci? s1 s2 [start1 end1 start2 end2]
;;
;; These are all simple derivatives of the previous counting funs.

(define (string-prefix? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-prefix? s1 s2 maybe-starts+ends
    (%string-prefix? s1 start1 end1 s2 start2 end2)))

(define (string-suffix? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-suffix? s1 s2 maybe-starts+ends
    (%string-suffix? s1 start1 end1 s2 start2 end2)))

(define (string-prefix-ci? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-prefix-ci? s1 s2 maybe-starts+ends
    (%string-prefix-ci? s1 start1 end1 s2 start2 end2)))

(define (string-suffix-ci? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-suffix-ci? s1 s2 maybe-starts+ends
    (%string-suffix-ci? s1 start1 end1 s2 start2 end2)))


;; Here are the internal routines that do the real work.

(define (%string-prefix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))    ; Quick check
         (= (%string-prefix-length s1 start1 end1
                                   s2 start2 end2)
            len1))))

(define (%string-suffix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))    ; Quick check
         (= len1 (%string-suffix-length s1 start1 end1
                                        s2 start2 end2)))))

(define (%string-prefix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))    ; Quick check
         (= len1 (%string-prefix-length-ci s1 start1 end1
                                           s2 start2 end2)))))

(define (%string-suffix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))    ; Quick check
         (= len1 (%string-suffix-length-ci s1 start1 end1
                                           s2 start2 end2)))))


;; string-compare    s1 s2 proc< proc= proc> [start1 end1 start2 end2]
;; string-compare-ci s1 s2 proc< proc= proc> [start1 end1 start2 end2]
;;
;; Primitive string-comparison functions.
;; Continuation order is different from MIT Scheme.
;; Continuations are applied to s1's mismatch index;
;; in the case of equality, this is END1.

(define (%string-compare s1 start1 end1 s2 start2 end2
                         proc< proc= proc>)
  (let ((size1 (- end1 start1))
        (size2 (- end2 start2)))
    (let ((match (%string-prefix-length s1 start1 end1 s2 start2 end2)))
      (if (= match size1)
        ((if (= match size2) proc= proc<) end1)
        ((if (= match size2)
           proc>
           (if (char<? (string-ref s1 (+ start1 match))
                       (string-ref s2 (+ start2 match)))
             proc< proc>))
         (+ match start1))))))

(define (%string-compare-ci s1 start1 end1 s2 start2 end2
                            proc< proc= proc>)
  (let ((size1 (- end1 start1))
        (size2 (- end2 start2)))
    (let ((match (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)))
      (if (= match size1)
        ((if (= match size2) proc= proc<) end1)
        ((if (= match size2) proc>
             (if (char-ci<? (string-ref s1 (+ start1 match))
                            (string-ref s2 (+ start2 match)))
               proc< proc>))
         (+ start1 match))))))

(define (string-compare s1 s2 proc< proc= proc> . maybe-starts+ends)
  (check-arg procedure? proc< 'string-compare)
  (check-arg procedure? proc= 'string-compare)
  (check-arg procedure? proc> 'string-compare)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-compare s1 s2 maybe-starts+ends
    (%string-compare s1 start1 end1 s2 start2 end2 proc< proc= proc>)))

(define (string-compare-ci s1 s2 proc< proc= proc> . maybe-starts+ends)
  (check-arg procedure? proc< 'string-compare-ci)
  (check-arg procedure? proc= 'string-compare-ci)
  (check-arg procedure? proc> 'string-compare-ci)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-compare-ci s1 s2 maybe-starts+ends
    (%string-compare-ci s1 start1 end1 s2 start2 end2 proc< proc= proc>)))



  ;; string=          string<>          string-ci=          string-ci<>
  ;; string<          string>           string-ci<          string-ci>
  ;; string<=         string>=          string-ci<=         string-ci>=
  ;;
  ;; Simple definitions in terms of the previous comparison funs.
  ;; I sure hope the %STRING-COMPARE calls get integrated.

(define (string= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string= s1 s2 maybe-starts+ends
    (and (= (- end1 start1) (- end2 start2)) ; Quick filter
         (or (and (eq? s1 s2) (= start1 start2)) ; Fast path
             (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                              (lambda (i) #f)
                              values
                              (lambda (i) #f))))))

(define (string<> s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string<> s1 s2 maybe-starts+ends
    (or (not (= (- end1 start1) (- end2 start2))) ; Fast path
        (and (not (and (eq? s1 s2) (= start1 start2))) ; Quick filter
             (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                              values
                              (lambda (i) #f)
                              values)))))

(define (string< s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string< s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (< end1 end2)
      (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                       values
                       (lambda (i) #f)
                       (lambda (i) #f)))))

(define (string> s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string> s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (> end1 end2)

      (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                       (lambda (i) #f)
                       (lambda (i) #f)
                       values))))

(define (string<= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string<= s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (<= end1 end2)
      (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                       values
                       values
                       (lambda (i) #f)))))

(define (string>= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string>= s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (>= end1 end2)
      (%string-compare s1 start1 end1 s2 start2 end2 ; Real test
                       (lambda (i) #f)
                       values
                       values))))

(define (string-ci= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci= s1 s2 maybe-starts+ends
    (and (= (- end1 start1) (- end2 start2)) ; Quick filter
         (or (and (eq? s1 s2) (= start1 start2)) ; Fast path
             (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                                 (lambda (i) #f)
                                 values
                                 (lambda (i) #f))))))

(define (string-ci<> s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci<> s1 s2 maybe-starts+ends
    (or (not (= (- end1 start1) (- end2 start2))) ; Fast path
        (and (not (and (eq? s1 s2) (= start1 start2))) ; Quick filter
             (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                                 values
                                 (lambda (i) #f)
                                 values)))))

(define (string-ci< s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci< s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (< end1 end2)
      (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                          values
                          (lambda (i) #f)
                          (lambda (i) #f)))))

(define (string-ci> s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci> s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (> end1 end2)
      (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                          (lambda (i) #f)
                          (lambda (i) #f)
                          values))))

(define (string-ci<= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci<= s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (<= end1 end2)
      (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                          values
                          values
                          (lambda (i) #f)))))

(define (string-ci>= s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2)
                         'string-ci>= s1 s2 maybe-starts+ends
    (if (and (eq? s1 s2) (= start1 start2)) ; Fast path
      (>= end1 end2)
      (%string-compare-ci s1 start1 end1 s2 start2 end2 ; Real test
                          (lambda (i) #f)
                          values
                          values))))


;; Hash

(define string-hash
  (opt-lambda (s (bound 0) . rest)
    (check-arg (lambda (x)
                 (and (integer? x)
                      (exact? x)
                      (<= 0 x)))
               bound 'string-hash)
    ((lambda (r)
       (if (zero? bound)
         r
         (modulo r bound)))
     (equal-hash-code
      (if (null? rest)
        s
        (apply substring/shared s rest))))))

(define string-hash-ci
  (opt-lambda (s (bound 0) . rest)
    (check-arg (lambda (x)
                 (and (integer? x)
                      (exact? x)
                      (<= 0 x)))
               bound 'string-hash-ci)
    ((lambda (r)
       (if (zero? bound)
         r
         (modulo r bound)))
     (equal-hash-code
      (string-downcase
       (if (null? rest)
         s
         (apply substring/shared s rest)))))))

;; Case hacking
;;
;; string-upcase  s [start end]
;; string-upcase! s [start end]
;; string-downcase  s [start end]
;; string-downcase! s [start end]
;;
;; string-titlecase  s [start end]
;; string-titlecase! s [start end]
;;   Capitalize every contiguous alpha sequence: capitalise
;;   first char, lowercase rest.

(define (s:string-upcase  s . maybe-start+end)
  (let-string-start+end (start end) 'string-upcase s maybe-start+end
    (%string-map char-upcase s start end)))

(define (string-upcase! s . maybe-start+end)
  (let-string-start+end (start end) 'string-upcase! s maybe-start+end
    (%string-map! char-upcase s start end)))

(define (s:string-downcase  s . maybe-start+end)
  (let-string-start+end (start end) 'string-downcase s maybe-start+end
    (%string-map char-downcase s start end)))

(define (string-downcase! s . maybe-start+end)
  (let-string-start+end (start end) 'string-downcase! s maybe-start+end
    (%string-map! char-downcase s start end)))

(define (char-cased? c)
  (not (char=? (char-downcase c) (char-upcase c))))

(define (%string-titlecase! s start end)
  (let lp ((i start))
    (cond ((string-index s char-cased? i end) =>
           (lambda (i)
             (string-set! s i (char-titlecase (string-ref s i)))
             (let ((i1 (+ i 1)))
               (cond ((string-skip s char-cased? i1 end) =>
                      (lambda (j)
                        (string-downcase! s i1 j)
                        (lp (+ j 1))))
                     (else (string-downcase! s i1 end)))))))))

(define (string-titlecase! s . maybe-start+end)
  (let-string-start+end (start end) 'string-titlecase! s maybe-start+end
    (%string-titlecase! s start end)))

(define (s:string-titlecase s . maybe-start+end)
  (let-string-start+end (start end) 'string-titlecase! s maybe-start+end
    (let ((ans (substring s start end)))
      (%string-titlecase! ans 0 (- end start))
      ans)))


;; Cutting & pasting strings
;;
;; string-take string nchars
;; string-drop string nchars
;;
;; string-take-right string nchars
;; string-drop-right string nchars
;;
;; string-pad string k [char start end]
;; string-pad-right string k [char start end]
;;
;; string-trim       string [char/char-set/pred start end]
;; string-trim-right string [char/char-set/pred start end]
;; string-trim-both  string [char/char-set/pred start end]
;;
;; These trimmers invert the char-set meaning from MIT Scheme -- you
;; say what you want to trim.

(define (string-take s n)
  (check-arg string? s 'string-take)
  (check-arg (lambda (val) (and (integer? n) (exact? n)
                                (<= 0 n (string-length s))))
             n 'string-take)
  (%substring/shared s 0 n))

(define (string-take-right s n)
  (check-arg string? s 'string-take-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
               n 'string-take-right)
    (%substring/shared s (- len n) len)))

(define (string-drop s n)
  (check-arg string? s 'string-drop)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
               n 'string-drop)
    (%substring/shared s n len)))

(define (string-drop-right s n)
  (check-arg string? s 'string-drop-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
               n 'string-drop-right)
    (%substring/shared s 0 (- len n))))


(define string-trim
  (opt-lambda (s (criterion char-set:whitespace) . rest)
    (let-string-start+end (start end) 'string-trim s rest
      (cond ((string-skip s criterion start end)
             => (lambda (i) (%substring/shared s i end)))
            (else "")))))

(define string-trim-right
  (opt-lambda (s (criterion char-set:whitespace) . rest)
    (let-string-start+end (start end) 'string-trim-right s rest
      (cond ((string-skip-right s criterion start end)
             => (lambda (i) (%substring/shared s 0 (+ 1 i))))
            (else "")))))

(define string-trim-both
  (opt-lambda (s (criterion char-set:whitespace) . rest)
    (let-string-start+end (start end) 'string-trim-both s rest
      (cond ((string-skip s criterion start end)
             => (lambda (i)
                  (%substring/shared s i (+ 1 (string-skip-right s criterion i end)))))
            (else "")))))

(define string-pad-right
  (opt-lambda (s n (char #\space) . rest)
    (check-arg char? char 'string-pad-right)
    (let-string-start+end (start end) 'string-pad-right s rest
      (check-arg (lambda (n) (and (integer? n) (exact? n) (<= 0 n)))
                 n 'string-pad-right)
      (let ((len (- end start)))
        (if (<= n len)
          (%substring/shared s start (+ start n))
          (let ((ans (make-string n char)))
            (%string-copy! ans 0 s start end)
            ans))))))

(define string-pad
  (opt-lambda (s n (char #\space) . rest)
    (check-arg char? char 'string-pad)
    (let-string-start+end (start end) 'string-pad s rest
      (check-arg (lambda (n) (and (integer? n) (exact? n) (<= 0 n)))
                 n 'string-pad)
      (let ((len (- end start)))
        (if (<= n len)
          (%substring/shared s (- end n) end)
          (let ((ans (make-string n char)))
            (%string-copy! ans (- n len) s start end)
            ans))))))



;; Filtering strings
;;
;; string-delete char/char-set/pred string [start end]
;; string-filter char/char-set/pred string [start end]
;;
;; If the criterion is a char or char-set, we scan the string twice with
;;   string-fold -- once to determine the length of the result string,
;;   and once to do the filtered copy.
;; If the criterion is a predicate, we don't do this double-scan strategy,
;;   because the predicate might have side-effects or be very expensive to
;;   compute. So we preallocate a temp buffer pessimistically, and only do
;;   one scan over S. This is likely to be faster and more space-efficient
;;   than consing a list.

(define (string-delete criterion s . maybe-start+end)
  (let-string-start+end (start end) 'string-delete s maybe-start+end
    (if (procedure? criterion)
      (let* ((slen (- end start))
             (temp (make-string slen))
             (ans-len (string-fold (lambda (c i)
                                     (if (criterion c) i
                                         (begin (string-set! temp i c)
                                                (+ i 1))))
                                   0 s start end)))
        (if (= ans-len slen) temp (substring temp 0 ans-len)))

      (let* ((cset (cond ((char-set? criterion) criterion)
                         ((char? criterion) (char-set criterion))
                         (else (error "string-delete criterion not predicate, char or char-set" criterion))))
             (len (string-fold (lambda (c i)
                                 (if (char-set-contains? cset c) i (+ i 1)))
                               0 s start end))
             (ans (make-string len)))
        (string-fold (lambda (c i)
                       (if (char-set-contains? cset c)
                         i
                         (begin (string-set! ans i c) (+ i 1))))
                     0 s start end)
        ans))))

(define (string-filter criterion s . maybe-start+end)
  (let-string-start+end (start end) 'string-filter s maybe-start+end
    (if (procedure? criterion)
      (let* ((slen (- end start))
             (temp (make-string slen))
             (ans-len (string-fold (lambda (c i)
                                     (if (criterion c)
                                       (begin (string-set! temp i c)
                                              (+ i 1))
                                       i))
                                   0 s start end)))
        (if (= ans-len slen) temp (substring temp 0 ans-len)))

      (let* ((cset (cond ((char-set? criterion) criterion)
                         ((char? criterion) (char-set criterion))
                         (else (error "string-delete criterion not predicate, char or char-set" criterion))))

             (len (string-fold (lambda (c i)
                                 (if (char-set-contains? cset c) (+ i 1) i))
                               0 s start end))
             (ans (make-string len)))
        (string-fold (lambda (c i)
                       (if (char-set-contains? cset c)
                         (begin (string-set! ans i c) (+ i 1))
                         i))
                     0 s start end)
        ans))))


;; String search
;;
;; string-index       string char/char-set/pred [start end]
;; string-index-right string char/char-set/pred [start end]
;; string-skip        string char/char-set/pred [start end]
;; string-skip-right  string char/char-set/pred [start end]
;; string-count       string char/char-set/pred [start end]
;;     There's a lot of replicated code here for efficiency.
;;     For example, the char/char-set/pred discrimination has
;;     been lifted above the inner loop of each proc.

(define (string-index str criterion . maybe-start+end)
  (let-string-start+end (start end) 'string-index str maybe-start+end
    (cond ((char? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char=? criterion (string-ref str i)) i
                      (lp (+ i 1))))))
          ((char-set? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char-set-contains? criterion (string-ref str i)) i
                      (lp (+ i 1))))))
          ((procedure? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (criterion (string-ref str i)) i
                      (lp (+ i 1))))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-index criterion)))))

(define (string-index-right str criterion . maybe-start+end)
  (let-string-start+end (start end) 'string-index-right str maybe-start+end
    (cond ((char? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char=? criterion (string-ref str i)) i
                      (lp (- i 1))))))
          ((char-set? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char-set-contains? criterion (string-ref str i)) i
                      (lp (- i 1))))))
          ((procedure? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (criterion (string-ref str i)) i
                      (lp (- i 1))))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-index-right criterion)))))

(define (string-skip str criterion . maybe-start+end)
  (let-string-start+end (start end) 'string-skip str maybe-start+end
    (cond ((char? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char=? criterion (string-ref str i))
                    (lp (+ i 1))
                    i))))
          ((char-set? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (char-set-contains? criterion (string-ref str i))
                    (lp (+ i 1))
                    i))))
          ((procedure? criterion)
           (let lp ((i start))
             (and (< i end)
                  (if (criterion (string-ref str i)) (lp (+ i 1))
                      i))))
          (else (error "Second param is neither char-set, char, or predicate procedure."
                       string-skip criterion)))))

(define (string-skip-right str criterion . maybe-start+end)
  (let-string-start+end (start end) 'string-skip-right str maybe-start+end
    (cond ((char? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char=? criterion (string-ref str i))
                    (lp (- i 1))
                    i))))
          ((char-set? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (char-set-contains? criterion (string-ref str i))
                    (lp (- i 1))
                    i))))
          ((procedure? criterion)
           (let lp ((i (- end 1)))
             (and (>= i start)
                  (if (criterion (string-ref str i)) (lp (- i 1))
                      i))))
          (else (error "CRITERION param is neither char-set or char."
                       string-skip-right criterion)))))


(define (string-count s criterion . maybe-start+end)
  (let-string-start+end (start end) 'string-count s maybe-start+end
    (cond ((char? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (char=? criterion (string-ref s i))
                           (+ count 1)
                           count)))
               ((>= i end) count)))

          ((char-set? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (char-set-contains? criterion (string-ref s i))
                           (+ count 1)
                           count)))
               ((>= i end) count)))

          ((procedure? criterion)
           (do ((i start (+ i 1))
                (count 0 (if (criterion (string-ref s i)) (+ count 1) count)))
               ((>= i end) count)))

          (else (error "CRITERION param is neither char-set or char."
                       string-count criterion)))))



;;
;; string-fill! string char [start end]
;;
;; string-copy! to tstart from [fstart fend]
;;    Guaranteed to work, even if s1 eq s2.

(define (s:string-fill! s char . maybe-start+end)
  (check-arg char? char 'string-fill!)
  (let-string-start+end (start end) 'string-fill! s maybe-start+end
    (do ((i (- end 1) (- i 1)))
        ((< i start))
      (string-set! s i char))))

;; Library-internal routine
(define (%string-copy! to tstart from fstart fend)
  (string-copy! to tstart from fstart fend))


;; Returns starting-position in STRING or #f if not true.
;; This implementation is slow & simple. It is useful as a "spec" or for
;; comparison testing with fancier implementations.
;; See below for fast KMP version.

;;(define (string-contains string substring . maybe-starts+ends)
;;  (let-string-start+end2 (start1 end1 start2 end2)
;;                         'string-contains string substring maybe-starts+ends
;;    (let* ((len (- end2 start2))
;;          (i-bound (- end1 len)))
;;      (let lp ((i start1))
;;       (and (< i i-bound)
;;            (if (string= string substring i (+ i len) start2 end2)
;;                i
;;                (lp (+ i 1))))))))


;; Searching for an occurrence of a substring
;;

(define (string-contains text pattern . maybe-starts+ends)
  (let-string-start+end2 (t-start t-end p-start p-end)
                         'string-contains text pattern maybe-starts+ends
    (%kmp-search pattern text char=? p-start p-end t-start t-end)))

(define (string-contains-ci text pattern . maybe-starts+ends)
  (let-string-start+end2 (t-start t-end p-start p-end)
                         'string-contains-ci text pattern maybe-starts+ends
    (%kmp-search pattern text char-ci=? p-start p-end t-start t-end)))


;; Knuth-Morris-Pratt string searching
;;
;; See
;;     "Fast pattern matching in strings"
;;     SIAM J. Computing 6(2):323-350 1977
;;     D. E. Knuth, J. H. Morris and V. R. Pratt
;; also described in
;;     "Pattern matching in strings"
;;     Alfred V. Aho
;;     Formal Language Theory - Perspectives and Open Problems
;;     Ronald V. Brook (editor)
;; This algorithm is O(m + n) where m and n are the
;; lengths of the pattern and string respectively

;; KMP search source[start,end) for PATTERN. Return starting index of
;; leftmost match or #f.

(define (%kmp-search pattern text c= p-start p-end t-start t-end)
  (let ((plen (- p-end p-start))
        (rv (make-kmp-restart-vector pattern c= p-start p-end)))

    ;; The search loop. TJ & PJ are redundant state.
    (let lp ((ti t-start) (pi 0)
             (tj (- t-end t-start)) ; (- tlen ti) -- how many chars left.
             (pj plen))        ; (- plen pi) -- how many chars left.

      (if (= pi plen)
        (- ti plen)                 ; Win.
        (and (<= pj tj)             ; Lose.
             (if (c= (string-ref text ti) ; Search.
                     (string-ref pattern (+ p-start pi)))
               (lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1)) ; Advance.

               (let ((pi (vector-ref rv pi))) ; Retreat.
                 (if (= pi -1)
                   (lp (+ ti 1) 0  (- tj 1) plen) ; Punt.
                   (lp ti       pi tj       (- plen pi))))))))))

;; (make-kmp-restart-vector pattern [c= start end]) -> integer-vector
;;
;; Compute the KMP restart vector RV for string PATTERN.  If
;; we have matched chars 0..i-1 of PATTERN against a search string S, and
;; PATTERN[i] doesn't match S[k], then reset i := RV[i], and try again to
;; match S[k].  If RV[i] = -1, then punt S[k] completely, and move on to
;; S[k+1] and PATTERN[0] -- no possible match of PAT[0..i] contains S[k].
;;
;; In other words, if you have matched the first i chars of PATTERN, but
;; the i+1'th char doesn't match, RV[i] tells you what the next-longest
;; prefix of PATTERN is that you have matched.
;;
;; - C= (default CHAR=?) is used to compare characters for equality.
;;   Pass in CHAR-CI=? for case-folded string search.
;;
;; - START & END restrict the pattern to the indicated substring; the
;;   returned vector will be of length END - START. The numbers stored
;;   in the vector will be values in the range [0,END-START) -- that is,
;;   they are valid indices into the restart vector; you have to add START
;;   to them to use them as indices into PATTERN.
;;
;; I've split this out as a separate function in case other constant-string
;; searchers might want to use it.
;;
;; E.g.:
;;    a b d  a b x
;; #(-1 0 0 -1 1 2)

(define (make-kmp-restart-vector pattern . maybe-c=+start+end)
  (let-optionals* maybe-c=+start+end
                  ((c= char=? (procedure? c=))
                   ((start end) (lambda (args)
                                  (string-parse-start+end make-kmp-restart-vector
                                                          pattern args))))
                  (let* ((rvlen (- end start))
                         (rv (make-vector rvlen -1)))
                    (if (> rvlen 0)
                      (let ((rvlen-1 (- rvlen 1))
                            (c0 (string-ref pattern start)))

                        ;; Here's the main loop. We have set rv[0] ... rv[i].
                        ;; K = I + START -- it is the corresponding index into PATTERN.
                        (let lp1 ((i 0) (j -1) (k start))
                          (if (< i rvlen-1)
                            ;; lp2 invariant:
                            ;;   pat[(k-j) .. k-1] matches pat[start .. start+j-1]
                            ;;   or j = -1.
                            (let lp2 ((j j))
                              (cond ((= j -1)
                                     (let ((i1 (+ 1 i)))
                                       (if (not (c= (string-ref pattern (+ k 1)) c0))
                                         (vector-set! rv i1 0))
                                       (lp1 i1 0 (+ k 1))))
                                    ;; pat[(k-j) .. k] matches pat[start..start+j].
                                    ((c= (string-ref pattern k) (string-ref pattern (+ j start)))
                                     (let* ((i1 (+ 1 i))
                                            (j1 (+ 1 j)))
                                       (vector-set! rv i1 j1)
                                       (lp1 i1 j1 (+ k 1))))

                                    (else (lp2 (vector-ref rv j)))))))))
                    rv)))


;; We've matched I chars from PAT. C is the next char from the search string.
;; Return the new I after handling C.
;;
;; The pattern is (VECTOR-LENGTH RV) chars long, beginning at index PAT-START
;; in PAT (PAT-START is usually 0). The I chars of the pattern we've matched
;; are
;;     PAT[PAT-START .. PAT-START + I].
;;
;; It's *not* an oversight that there is no friendly error checking or
;; defaulting of arguments. This is a low-level, inner-loop procedure
;; that we want integrated/inlined into the point of call.

(define (kmp-step pat rv c i c= p-start)
  (let lp ((i i))
    (if (c= c (string-ref pat (+ i p-start))) ; Match =>
      (+ i 1)                       ;   Done.
      (let ((i (vector-ref rv i)))  ; Back up in PAT.
        (if (= i -1) 0              ; Can't back up further.
            (lp i))))))             ; Keep trying for match.

;; Zip through S[start,end), looking for a match of PAT. Assume we've
;; already matched the first I chars of PAT when we commence at S[start].
;; - <0:  If we find a match *ending* at index J, return -J.
;; - >=0: If we get to the end of the S[start,end) span without finding
;;   a complete match, return the number of chars from PAT we'd matched
;;   when we ran off the end.
;;
;; This is useful for searching *across* buffers -- that is, when your
;; input comes in chunks of text. We hand-integrate the KMP-STEP loop
;; for speed.

(define string-kmp-partial-search
  (opt-lambda (pat rv s i (c= char=?) (p-start 0) . start+end)
    (check-arg procedure? c= 'string-kmp-partial-search)
    (check-arg vector? rv 'string-kmp-partial-search)
    (check-arg (lambda (x)
                 (and (integer? x) (exact? x) (<= 0 x))) p-start 'string-kmp-partial-search)

    (let-values ([(rest s-start s-end) (string-parse-start+end 'string-kmp-partial-search
                                                               s start+end)])
      (let ((patlen (vector-length rv)))
        (check-arg (lambda (i) (and (integer? i) (exact? i) (<= 0 i) (< i patlen)))
                   i 'string-kmp-partial-search)

        ;; Enough prelude. Here's the actual code.
        (let lp ((si s-start)         ; An index into S.
                 (vi i))              ; An index into RV.
          (cond ((= vi patlen) (- si)) ; Win.
                ((= si s-end) vi)     ; Ran off the end.
                (else                 ; Match s[si] & loop.
                 (let ((c (string-ref s si)))
                   (lp (+ si 1)
                       (let lp2 ((vi vi)) ; This is just KMP-STEP.
                         (if (c= c (string-ref pat (+ vi p-start)))
                           (+ vi 1)
                           (let ((vi (vector-ref rv vi)))
                             (if (= vi -1) 0
                                 (lp2 vi))))))))))))))


;; Misc
;;
;; (string-null? s)
;; (string-reverse  s [start end])
;; (string-reverse! s [start end])
;; (reverse-list->string clist)
;; (s:string->list s [start end])

(define (string-null? s) (zero? (string-length s)))

(define (string-reverse s . maybe-start+end)
  (let-string-start+end (start end) 'string-reverse s maybe-start+end
    (let* ((len (- end start))
           (ans (make-string len)))
      (do ((i start (+ i 1))
           (j (- len 1) (- j 1)))
          ((< j 0))
        (string-set! ans j (string-ref s i)))
      ans)))

(define (string-reverse! s . maybe-start+end)
  (let-string-start+end (start end) 'string-reverse! s maybe-start+end
    (do ((i (- end 1) (- i 1))
         (j start (+ j 1)))
        ((<= i j))
      (let ((ci (string-ref s i)))
        (string-set! s i (string-ref s j))
        (string-set! s j ci)))))


(define (reverse-list->string clist)
  (let* ((len (length clist))
         (s (make-string len)))
    (do ((i (- len 1) (- i 1))
         (clist clist (cdr clist)))
        ((not (pair? clist)))
      (string-set! s i (car clist)))
    s))


;; (define (s:string->list s . maybe-start+end)
;;  (apply string-fold-right cons '() s maybe-start+end))

(define (s:string->list s . maybe-start+end)
  (let-string-start+end (start end) 'string->list s maybe-start+end
    (do ((i (- end 1) (- i 1))
         (ans '() (cons (string-ref s i) ans)))
        ((< i start) ans))))

;; Defined by R5RS, so commented out here.
;;(define (list->string lis) (string-unfold null? car cdr lis))


;; string-concatenate        string-list -> string
;; string-concatenate/shared string-list -> string
;; string-append/shared s ... -> string
;;
;; STRING-APPEND/SHARED has license to return a string that shares storage
;; with any of its arguments. In particular, if there is only one non-empty
;; string amongst its parameters, it is permitted to return that string as
;; its result. STRING-APPEND, by contrast, always allocates new storage.
;;
;; STRING-CONCATENATE & STRING-CONCATENATE/SHARED are passed a list of
;; strings, which they concatenate into a result string. STRING-CONCATENATE
;; always allocates a fresh string; STRING-CONCATENATE/SHARED may (or may
;; not) return a result that shares storage with any of its arguments. In
;; particular, if it is applied to a singleton list, it is permitted to
;; return the car of that list as its value.

(define (string-append/shared . strings) (string-concatenate/shared strings))

(define (string-concatenate/shared strings)
  (let lp ((strings strings) (nchars 0) (first #f))
    (cond ((pair? strings)            ; Scan the args, add up total
           (let* ((string  (car strings)) ; length, remember 1st
                  (tail (cdr strings)) ; non-empty string.
                  (slen (string-length string)))
             (if (zero? slen)
               (lp tail nchars first)
               (lp tail (+ nchars slen) (or first strings)))))

          ((zero? nchars) "")

          ;; Just one non-empty string! Return it.
          ((= nchars (string-length (car first))) (car first))

          (else (let ((ans (make-string nchars)))
                  (let lp ((strings first) (i 0))
                    (if (pair? strings)
                      (let* ((s (car strings))
                             (slen (string-length s)))
                        (%string-copy! ans i s 0 slen)
                        (lp (cdr strings) (+ i slen)))))
                  ans)))))


;; Alas, Scheme 48's APPLY blows up if you have many, many arguments.
;;(define (string-concatenate strings) (apply string-append strings))

;; Here it is written out. I avoid using REDUCE to add up string lengths
;; to avoid non-R5RS dependencies.
(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
                     (i 0 (+ i (string-length (car strings)))))
                    ((not (pair? strings)) i)))
         (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
        (let* ((s (car strings))
               (slen (string-length s)))
          (%string-copy! ans i s 0 slen)
          (lp (+ i slen) (cdr strings)))))
    ans))


;; Defined by R5RS, so commented out here.
;;(define (string-append . strings) (string-concatenate strings))

;; string-concatenate-reverse        string-list [final-string end] -> string
;; string-concatenate-reverse/shared string-list [final-string end] -> string
;;
;; Return
;;   (string-concatenate
;;     (reverse
;;       (cons (substring final-string 0 end) string-list)))

(define string-concatenate-reverse
  (opt-lambda (string-list (final "") (end (string-length final)))
    (check-arg string? final 'string-concatenate-reverse)
    (check-arg (lambda (x)
                 (and (integer? x) (exact? x) (<= 0 x (string-length final))))
               end 'string-concatenate-reverse)
    (let ((len (let lp ((sum 0) (lis string-list))
                 (if (pair? lis)
                   (lp (+ sum (string-length (car lis))) (cdr lis))
                   sum))))

      (%finish-string-concatenate-reverse len string-list final end))))

(define string-concatenate-reverse/shared
  (opt-lambda (string-list (final "") (end (string-length final)))
    (check-arg string? final 'string-concatenate-reverse/shared)
    (check-arg (lambda (x)
                 (and (integer? x) (exact? x) (<= 0 x (string-length final))))
               end 'string-concatenate-reverse/shared)
    ;; Add up the lengths of all the strings in STRING-LIST; also get a
    ;; pointer NZLIST into STRING-LIST showing where the first non-zero-length
    ;; string starts.
    (let lp ((len 0) (nzlist #f) (lis string-list))
      (if (pair? lis)
        (let ((slen (string-length (car lis))))
          (lp (+ len slen)
              (if (or nzlist (zero? slen)) nzlist lis)
              (cdr lis)))

        (cond ((zero? len) (substring/shared final 0 end))

              ;; LEN > 0, so NZLIST is non-empty.

              ((and (zero? end) (= len (string-length (car nzlist))))
               (car nzlist))

              (else (%finish-string-concatenate-reverse len nzlist final end)))))))

(define (%finish-string-concatenate-reverse len string-list final end)
  (let ((ans (make-string (+ end len))))
    (%string-copy! ans len final 0 end)
    (let lp ((i len) (lis string-list))
      (if (pair? lis)
        (let* ((s   (car lis))
               (lis (cdr lis))
               (slen (string-length s))
               (i (- i slen)))
          (%string-copy! ans i s 0 slen)
          (lp i lis))))
    ans))




;; string-replace s1 s2 start1 end1 [start2 end2] -> string
;;
;; Replace S1[START1,END1) with S2[START2,END2).

(define (string-replace s1 s2 start1 end1 . maybe-start+end)
  (check-substring-spec string-replace s1 start1 end1)
  (let-string-start+end (start2 end2) 'string-replace s2 maybe-start+end
    (let* ((slen1 (string-length s1))
           (sublen2 (- end2 start2))
           (alen (+ (- slen1 (- end1 start1)) sublen2))
           (ans (make-string alen)))
      (%string-copy! ans 0 s1 0 start1)
      (%string-copy! ans start1 s2 start2 end2)
      (%string-copy! ans (+ start1 sublen2) s1 end1 slen1)
      ans)))


;; string-tokenize s [token-set start end] -> list
;;
;; Break S up into a list of token strings, where a token is a maximal
;; non-empty contiguous sequence of chars belonging to TOKEN-SET.
;; (string-tokenize "hello, world") => ("hello," "world")

(define string-tokenize
  (opt-lambda (s (token-chars char-set:graphic) . rest)
    (check-arg char-set? token-chars 'string-tokenize)
    (let-string-start+end (start end) 'string-tokenize s rest
      (let lp ((i end) (ans '()))
        (cond ((and (< start i) (string-index-right s token-chars start i)) =>
               (lambda (tend-1)
                 (let ((tend (+ 1 tend-1)))
                   (cond ((string-skip-right s token-chars start tend-1) =>
                          (lambda (tstart-1)
                            (lp tstart-1
                                (cons (substring s (+ 1 tstart-1) tend)
                                      ans))))
                         (else (cons (substring s start tend) ans))))))
              (else ans))))))


;; xsubstring s from [to start end] -> string
;;
;; S is a string; START and END are optional arguments that demarcate
;; a substring of S, defaulting to 0 and the length of S (e.g., the whole
;; string). Replicate this substring up and down index space, in both the
;;  positive and negative directions. For example, if S = "abcdefg", START=3,
;; and END=6, then we have the conceptual bidirectionally-infinite string
;;     ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f ...
;;     ... -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 ...
;; XSUBSTRING returns the substring of this string beginning at index FROM,
;; and ending at TO (which defaults to FROM+(END-START)).
;;
;; You can use XSUBSTRING in many ways:
;; - To rotate a string left:  (xsubstring "abcdef" 2)  => "cdefab"
;; - To rotate a string right: (xsubstring "abcdef" -2) => "efabcd"
;; - To replicate a string:    (xsubstring "abc" 0 7) => "abcabca"
;;
;; Note that
;;   - The FROM/TO indices give a half-open range -- the characters from
;;     index FROM up to, but not including index TO.
;;   - The FROM/TO indices are not in terms of the index space for string S.
;;     They are in terms of the replicated index space of the substring
;;     defined by S, START, and END.
;;
;; It is an error if START=END -- although this is allowed by special
;; dispensation when FROM=TO.

(define (xsubstring s from . maybe-to+start+end)
  (check-arg (lambda (val) (and (integer? val) (exact? val)))
             from 'xsubstring)
  (receive (to start end)
           (if (pair? maybe-to+start+end)
             (let-string-start+end (start end) 'xsubstring s (cdr maybe-to+start+end)
               (let ((to (car maybe-to+start+end)))
                 (check-arg (lambda (val)
                              (and (integer? val)
                                   (exact? val)
                                   (<= from val)))
                            to 'xsubstring)
                 (values to start end)))
             (let ((slen (string-length (check-arg string? s 'xsubstring))))
               (values (+ from slen) 0 slen)))
    (let ((slen   (- end start))
          (anslen (- to  from)))
      (cond ((zero? anslen) "")
            ((zero? slen) (error xsubstring "Cannot replicate empty (sub)string ~a ~a ~a ~a ~a" s from to start end))

            ((= 1 slen)        ; Fast path for 1-char replication.
             (make-string anslen (string-ref s start)))

            ;; Selected text falls entirely within one span.
            ((= (floor (/ from slen)) (floor (/ to slen)))
             (substring s (+ start (modulo from slen))
                        (+ start (modulo to   slen))))

            ;; Selected text requires multiple spans.
            (else (let ((ans (make-string anslen)))
                    (%multispan-repcopy! ans 0 s from to start end)
                    ans))))))


;; string-xcopy! target tstart s sfrom [sto start end] -> unspecific
;;
;; Exactly the same as xsubstring, but the extracted text is written
;; into the string TARGET starting at index TSTART.
;; This operation is not defined if (EQ? TARGET S) -- you cannot copy
;; a string on top of itself.

(define (string-xcopy! target tstart s sfrom . maybe-sto+start+end)
  (check-arg (lambda (val) (and (integer? val) (exact? val)))
             sfrom 'string-xcopy!)
  (receive (sto start end)
           (if (pair? maybe-sto+start+end)
             (let-string-start+end (start end) 'string-xcopy! s (cdr maybe-sto+start+end)
               (let ((sto (car maybe-sto+start+end)))
                 (check-arg (lambda (val) (and (integer? val) (exact? val)))
                            sto 'string-xcopy!)
                 (values sto start end)))
             (let ((slen (string-length s)))
               (values (+ sfrom slen) 0 slen)))

    (let* ((tocopy (- sto sfrom))
           (tend (+ tstart tocopy))
           (slen (- end start)))
      (check-substring-spec string-xcopy! target tstart tend)
      (cond ((zero? tocopy))
            ((zero? slen) (error 'string-xcopy! "Cannot replicate empty (sub)string: ~a ~a ~a ~a ~a ~a ~a" target tstart s sfrom sto start end))

            ((= 1 slen)        ; Fast path for 1-char replication.
             (s:string-fill! target (string-ref s start) tstart tend))

            ;; Selected text falls entirely within one span.
            ((= (floor (/ sfrom slen)) (floor (/ sto slen)))
             (%string-copy! target tstart s
                            (+ start (modulo sfrom slen))
                            (+ start (modulo sto   slen))))

            ;; Multi-span copy.
            (else (%multispan-repcopy! target tstart s sfrom sto start end))))))

;; This is the core copying loop for XSUBSTRING and STRING-XCOPY!
;; Internal -- not exported, no careful arg checking.
(define (%multispan-repcopy! target tstart s sfrom sto start end)
  (let* ((slen (- end start))
         (i0 (+ start (modulo sfrom slen)))
         (total-chars (- sto sfrom)))

    ;; Copy the partial span @ the beginning
    (%string-copy! target tstart s i0 end)

    (let* ((ncopied (- end i0))       ; We've copied this many.
           (nleft (- total-chars ncopied)) ; # chars left to copy.
           (nspans (quotient nleft slen))) ; # whole spans to copy

      ;; Copy the whole spans in the middle.
      (do ((i (+ tstart ncopied) (+ i slen)) ; Current target index.
           (nspans nspans (- nspans 1))) ; # spans to copy
          ((zero? nspans)
           ;; Copy the partial-span @ the end & we're done.
           (%string-copy! target i s start (+ start (- total-chars (- i tstart)))))

        (%string-copy! target i s start end))))) ; Copy a whole span.



;; (string-join string-list [delimiter grammar]) => string
;;
;; Paste strings together using the delimiter string.
;;
;; (string-join '("foo" "bar" "baz") ":") => "foo:bar:baz"
;;
;; DELIMITER defaults to a single space " "
;; GRAMMAR is one of the symbols {prefix, infix, strict-infix, suffix}
;; and defaults to 'infix.
;;
;; I could rewrite this more efficiently -- precompute the length of the
;; answer string, then allocate & fill it in iteratively. Using
;; STRING-CONCATENATE is less efficient.

(define string-join
  (opt-lambda (strings (delim " ") (grammar 'infix))
    (check-arg string? delim 'string-join)
    (let ((buildit (lambda (lis final)
                     (let recur ((lis lis))
                       (if (pair? lis)
                         (cons delim (cons (car lis) (recur (cdr lis))))
                         final)))))

      (cond ((pair? strings)
             (string-concatenate
              (case grammar

                ((infix strict-infix)
                 (cons (car strings) (buildit (cdr strings) '())))

                ((prefix) (buildit strings '()))

                ((suffix)
                 (cons (car strings) (buildit (cdr strings) (list delim))))

                (else (error "Illegal join grammar"
                             grammar string-join)))))

            ((not (null? strings))
             (error "STRINGS parameter not list." strings string-join))

            ;; STRINGS is ()

            ((eq? grammar 'strict-infix)
             (error "Empty list cannot be joined with STRICT-INFIX grammar."
                    string-join))

            (else "")))))             ; Special-cased for infix grammar.

;; string.rkt ends here
