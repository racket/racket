#lang racket/base
(require racket/contract/base
         unstable/list
         unstable/contract
         racket/serialize)
(require unstable/bytes
         unstable/contract
         unstable/list
         unstable/net/url)
(provide
 (all-from-out
  unstable/bytes
  unstable/contract
  unstable/list
  unstable/net/url))

;; --

;; network-error: symbol string . values -> void
;; throws a formatted exn:fail:network
(define (network-error src fmt . args)
  (raise (make-exn:fail:network (format "~a: ~a" src (apply format fmt args))
                                (current-continuation-marks))))

;; exn->string : (or/c exn any) -> string
(define (exn->string exn)
  (if (exn? exn)
      (parameterize ([current-error-port (open-output-string)])
        ((error-display-handler) (exn-message exn) exn)
        (get-output-string (current-error-port)))
      (format "~s\n" exn)))

(provide/contract
 [network-error (->* [symbol? string?] [] #:rest list? void?)]
 [exn->string (-> any/c string?)])

;; --

; explode-path* : path? -> (listof path?)
(define (explode-path* p)
  (let loop ([p p] [r null])
    (cond 
      [(eq? 'relative p) r]
      [(not p) r]
      [else
       (let-values ([(base name dir?) (split-path p)])
         (loop base (list* name r)))])))
;; Eli: We already have `explode-path', this looks like it's doing the
;;   same thing, except a little less useful.

; strip-prefix-ups : (listof path-piece?) -> (listof path-piece?)
(define (strip-prefix-ups l)
  (define prefix? (box #t))
  (filter (lambda (p)
            (if (unbox prefix?)
                (if (eq? 'up p)
                    #f
                    (begin #t
                           (set-box! prefix? #f)))
                #t))
          l))
;; Eli: This is bad.  If I understand it correctly, this is what this
;;   *should* have been:
;;     (define (strip-prefix-ups l)
;;       (if (and (pair? l) (eq? 'up (car l))) (strip-prefix-ups (cdr l)) l))
;;   or even:
;;     (define (strip-prefix-ups l)
;;       (match l [(cons 'up l) (strip-prefix-ups l)] [_ l]))
;;   except that the above version manages to combine ugly and
;;   obfuscated code, redundant mutation, redundant code (why is it a
;;   box? why is there a (begin #t ...)?), and being extra slow.  Oh,
;;   and if this wasn't enough, there's exactly one place in the web
;;   server that uses it.

; path-without-base : path? path? -> (listof path-piece?)
(define (path-without-base base path)
  (define b (explode-path* base))
  (define p (explode-path* path))
  (if (list-prefix? b p)
      (list-tail p (length b))
      (error 'path-without-base "~a is not a prefix of ~a" base path)))
;; Eli: see my comment on `list-prefix?' -- it would make this trivial.
;;   Also, if you want to look for a useful utility to add, search the code for
;;   `relativize', which is a popular thing that gets written multiple times
;;   and would be nice to have as a library.  (But there are some differences
;;   between them, I think.)

;; build-path-unless-absolute : path-string? path-string? -> path?
(define (build-path-unless-absolute base path)
  (if (absolute-path? path)
      (build-path path)
      (build-path base path)))
;; Eli: This looks completely unnecessary.  I find the code much easier to
;;   understand than the long name.

(define (directory-part path)
  (let-values ([(base name must-be-dir) (split-path path)])
    (cond
      [(eq? 'relative base) (current-directory)]
      [(not base) (error 'directory-part "~a is a top-level directory" path)]
      [(path? base) base])))
;; Eli: There is now a `file-name-from-path', which suggests that the name for
;;   this should be `directory-name-from-path', but perhaps a new name is
;;   better for both.  Also, I find it questionable to return the current
;;   directory in the first case.

(provide/contract
 [explode-path* (path-string? . -> . (listof path-piece?))]
 [path-without-base (path-string? path-string? . -> . (listof path-piece?))]
 [strip-prefix-ups ((listof path-piece?) . -> . (listof path-piece?))] 
 [directory-part (path-string? . -> . path?)]
 [build-path-unless-absolute (path-string? path-string? . -> . path?)])

;; --

(define (read/string str)
  (define r (read (open-input-string str)))
  (cond [(eof-object? r) (raise-type-error 'read/string "nonempty string" str)]
        [else r]))

;; Eli: Same comments as `read/bytes'.

(define (write/string v)
  (define str (open-output-string))
  (write v str)
  (get-output-string str))
;; Eli: Same comments as `write/string', and worse -- this is the same as
;;   (format "~s" v)

; lowercase-symbol! : (or/c string bytes) -> symbol
(define (lowercase-symbol! s)
  (string->symbol
   (string-downcase
    (if (bytes? s)
        (bytes->string/utf-8 s)
        s))))
;; Eli: This doesn't make any sense at all.  Why is the `!' in the name?  Why
;;   does it accept bytes?  Why does a function in a "string" library accept
;;   bytes?  How can I guess that this creates a new symbol from that name?
;;   (Which makes me think that this is (compose string->symbol string-downcase
;;   symbol->string))

(provide/contract
 [lowercase-symbol! ((or/c string? bytes?) . -> . symbol?)]
 [read/string (string? . -> . serializable?)]
 [write/string (serializable? . -> . string?)])
