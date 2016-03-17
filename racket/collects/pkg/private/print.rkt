#lang racket/base
(require racket/format
         racket/match
         racket/string
         "params.rkt")

;; Output and error helpers

(provide (all-defined-out))

(define-logger pkg)

(define (pkg-error . rest)
  (apply (current-pkg-error) rest))

(define (format-list l)
  (if (null? l)
      " [none]"
      (apply string-append
             (for/list ([v (in-list l)])
               (format "\n   ~a" v)))))

(define (log-exn x what)
  (log-pkg-error (~a "failure ~a\n"
                         "  error: ~s")
                     what
                     (exn-message x)))

(define (printf/flush fmt . args)
  ;; For status reporting, flush immediately after printing
  (apply printf fmt args)
  (flush-output))

(define ((complain-about-source given-name) s reason)
  (pkg-error (~a "invalid package source;\n"
                 " ~a\n"
                 "  given: ~a~a")
             reason
             s
             (if given-name
                 (~a "\n  for package name: " given-name)
                 "")))

(define (ask question
             #:default-yes? [default-yes? #t])
  (let loop ()
    (printf question)
    (printf " [~a/~a/a/c/?] "
            (if default-yes? "Y" "y")
            (if default-yes? "n" "N"))
    (flush-output)
    (match (string-trim (read-line (current-input-port) 'any))
      [(or "y" "Y")
       'yes]
      [(or "n" "N")
       'no]
      [(or "a" "A")
       'always-yes]
      [(or "c" "C")
       'cancel]
      [""
       (if default-yes? 'yes 'no)]
      [x
       (eprintf "Invalid answer: ~a\n" x)
       (eprintf " Answer ~a`y' or `Y' for \"yes\", ~a`n' or `N' for \"no\", or\n"
                (if default-yes? "nothing or " "")
                (if default-yes? "" "nothing or "))
       (eprintf " `a' or `A' for \"yes for all\", or `c' or `C' for \"cancel\".\n")
       (loop)])))


(define (dry-run-explain dry-run?)
  (if dry-run?
      " (but not really)"
      ""))
