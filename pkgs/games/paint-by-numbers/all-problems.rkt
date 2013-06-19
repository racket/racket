#lang racket
(require racket/unit
         racket/include
         "problem.rkt"
         (for-syntax racket))

(define-signature paint-by-numbers:all-problems^ (problemss set-names))
(define-signature paint-by-numbers:problem-set^ (problems set-name))
(define-signature paint-by-numbers:problem^ ((struct problem (name rows cols solution) #:omit-constructor)))

(define-syntax (mk-units stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
         ([(unit-names ...)
           (let* ([prob-dir (collection-file-path "problems" "games" "paint-by-numbers")]
                  [files (call-with-input-file (build-path prob-dir "directory") read)])
             (for/list ([file files] #:when (file-exists? (build-path prob-dir file)))
               (define path-spec (string-append "problems" "/" file))
               path-spec))])
       #'(list (include unit-names) ...))]))

(define units (mk-units))

(define empty-unit
  (unit 
    (import paint-by-numbers:problem^)
    (export paint-by-numbers:all-problems^)
    (define problemss null)
    (define set-names null)))


(define (combine-units new-unit sofar)
  (compound-unit
    (import [p : paint-by-numbers:problem^])
    (export combine)
    (link [((new : paint-by-numbers:problem-set^)) new-unit p]
          [((old : paint-by-numbers:all-problems^)) sofar p]
          [((combine : paint-by-numbers:all-problems^))
           (unit 
             (import (prefix old: paint-by-numbers:all-problems^)
                     (prefix new: paint-by-numbers:problem-set^)
                     paint-by-numbers:problem^)
             (export paint-by-numbers:all-problems^)
             
             (define (expand-problem pbm)
               (make-problem (problem-name pbm)
                             (problem-rows pbm)
                             (problem-cols pbm)
                             (expand-solution (problem-solution pbm))))
             
             (define problemss 
               (if (null? new:problems)
                   old:problemss
                   (cons (map expand-problem new:problems) old:problemss)))
             (define set-names
               (if (null? new:problems)
                   old:set-names
                   (cons new:set-name old:set-names)))) new old p])))

;; expand-solution : (union #f (listof string[row])) -> 
;;                   (union #f (vectorof (vectorof (union 'on 'off 'unknown))))
(define (expand-solution sol)
  (and sol (apply vector (map expand-row sol))))
;; expand-row : string -> (vectorof (union 'on 'off 'unknown))
(define (expand-row str)
  (list->vector (map expand-char (string->list str))))
;; expand-char : char -> (union 'on 'off 'unknown)
(define (expand-char c)
  (case c
    [(#\x) 'on]
    [(#\space) 'off]
    [(#\U) 'unknown]))

(provide-signature-elements paint-by-numbers:all-problems^)

(define-values/invoke-unit 
  (foldr combine-units empty-unit units)
  (import paint-by-numbers:problem^)
  (export paint-by-numbers:all-problems^))
