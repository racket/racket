#lang scheme/unit

(require "make-sig.rkt")

(import)
(export make^)

(define make-print-checking    (make-parameter #t))
(define make-print-dep-no-line (make-parameter #t))
(define make-print-reasons     (make-parameter #t))
(define make-notify-handler    (make-parameter void))

(define-struct line (targets      ; (list-of string)
                     dependencies ; (list-of string)
                     command))    ; (union thunk #f)
(define-struct (exn:fail:make exn:fail) (target orig-exn))

;; check-spec : TST -> (non-empty-list-of line)
;; throws an error on bad input
(define (spec->lines spec)
  (define (->strings xs)
    (map (lambda (x) (if (path? x) (path->string x) x)) xs))
  (define (err s p) (error 'make/proc "~a: ~e" s p))
  (unless (and (list? spec) (pair? spec))
    (err "specification is not a non-empty list" spec))
  (for/list ([line spec])
    (unless (and (list? line) (<= 2 (length line) 3))
      (err "line is not a list with 2 or 3 parts" line))
    (let* ([name (car line)]
           [tgts (if (list? name) name (list name))]
           [deps (cadr line)]
           [thunk (and (pair? (cddr line)) (caddr line))])
      (define (err s p) (error 'make/proc "~a: ~e for line: ~a" s p name))
      (unless (andmap path-string? tgts)
        (err "line does not start with a path/string or list of paths/strings"
             line))
      (unless (list? deps) (err "second part of line is not a list" deps))
      (for ([dep deps])
        (unless (path-string? dep)
          (err "dependency item is not a path/string" dep)))
      (unless (or (not thunk)
                  (and (procedure? thunk) (procedure-arity-includes? thunk 0)))
        (err "command part of line is not a thunk" thunk))
      (make-line (->strings tgts) (->strings deps) thunk))))

;; (union path-string (vector-of path-string) (list-of path-string))
;; -> (list-of string)
;; throws an error on bad input
(define (argv->args x)
  (let ([args (cond [(list? x) x]
                    [(vector? x) (vector->list x)]
                    [else (list x)])])
    (map (lambda (a)
           (cond [(string? a) a]
                 [(path? a) (path->string a)]
                 [else (raise-type-error
                        'make/proc "path/string or path/string vector or list"
                        x)]))
         args)))

;; path-date : path-string -> (union integer #f)
(define (path-date p)
  (and (or (directory-exists? p) (file-exists? p))
       (file-or-directory-modify-seconds p)))

;; make/proc :
;; spec (union path-string (vector-of path-string) (list-of path-string))
;; -> void
;; effect : make, according to spec and argv. See docs for details
(define (make/proc spec [argv '()])
  (define made null)
  (define lines (spec->lines spec))
  (define args (argv->args argv))
  (define (make-file s indent)
    (define line
      (findf (lambda (line)
               (ormap (lambda (s1) (string=? s s1)) (line-targets line)))
             lines))
    (define date (path-date s))
    (when (and (make-print-checking) (or line (make-print-dep-no-line)))
      (printf "make: ~achecking ~a\n" indent s)
      (flush-output))
    (if (not line)
      (unless date (error 'make "don't know how to make ~a" s))
      (let* ([deps (line-dependencies line)]
             [command (line-command line)]
             [indent+ (string-append indent " ")]
             [dep-dates (for/list ([d deps])
                          (make-file d indent+)
                          (or (path-date d)
                              (error 'make "dependancy ~a was not made\n" d)))]
             [reason (or (not date)
                         (ormap (lambda (dep ddate) (and (> ddate date) dep))
                                deps dep-dates))])
        (when (and reason command)
          (set! made (cons s made))
          ((make-notify-handler) s)
          (printf "make: ~amaking ~a~a\n"
                  (if (make-print-checking) indent "")
                  s
                  (cond [(not (make-print-reasons)) ""]
                        [(not date) (format " because ~a does not exist" s)]
                        [else (format " because ~a changed" reason)]))
          (flush-output)
          (with-handlers ([exn:fail?
                           (lambda (exn)
                             (raise (make-exn:fail:make
                                     (format "make: failed to make ~a; ~a"
                                             s (exn-message exn))
                                     (exn-continuation-marks exn)
                                     (line-targets line)
                                     exn)))])
            (command))))))
  (for ([f (if (null? args) (list (car (line-targets (car lines)))) args)])
    (make-file f ""))
  (for ([item (reverse made)]) (printf "make: made ~a\n" item))
  (flush-output))
