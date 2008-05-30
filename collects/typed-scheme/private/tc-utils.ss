#lang scheme/base
(provide (all-defined-out))
(require "syntax-traversal.ss" (for-syntax scheme/base) scheme/match)

;; a parameter representing the original location of the syntax being currently checked
(define current-orig-stx (make-parameter #'here))
(define orig-module-stx (make-parameter #f))
(define expanded-module-stx (make-parameter #f))

(define (stringify l [between " "])
  (define (intersperse v l)
    (cond [(null? l) null]
          [(null? (cdr l)) l]
          [else (cons (car l) (cons v (intersperse v (cdr l))))]))
  (apply string-append (intersperse between (map (lambda (s) (format "~a" s)) l))))

;; helper function, not currently used
(define (find-origin stx)
  (cond [(syntax-property stx 'origin) => (lambda (orig)
                                            (let ([r (reverse orig)])
                                              (let loop ([r (reverse orig)])
                                                (if (null? r) #f
                                                    (if (syntax-source (car r)) (car r)
                                                        (loop (cdr r)))))))]
        [else #f]))

;; do we print the fully-expanded syntax in error messages?
(define print-syntax? (make-parameter #f))


(define check-unreachable-code? (make-parameter #f))

(define (locate-stx stx)
  (define omodule (orig-module-stx))
  (define emodule (expanded-module-stx))
  ;(printf "orig: ~a~n" (syntax-object->datum omodule))
  ;(printf "exp: ~a~n" (syntax-object->datum emodule))
  ;(printf "stx (locate): ~a~n" (syntax-object->datum stx))
  (if (and (not (print-syntax?)) omodule emodule stx)
      (look-for-in-orig omodule emodule stx)
      stx))

(define (raise-typecheck-error msg stxs)
  (raise (make-exn:fail:syntax (string-append "typecheck: " msg)
                               (current-continuation-marks)
                               stxs)))

(define delayed-errors null)

(define-struct err (msg stx) #:prefab)

(define (report-all-errors)
  (define (reset!) (set! delayed-errors null))
  (match (reverse delayed-errors)
    [(list) (void)]
    [(list (struct err (msg stx)))
     (reset!)
     (raise-typecheck-error msg stx)]
    [l
     (let ([stxs
            (for/list ([e (reverse delayed-errors)])
              (sync (thread (lambda () (raise-typecheck-error (err-msg e) (err-stx e)))))
              (err-stx e))])
       (reset!)
       (unless (null? stxs)
         (raise-typecheck-error "Errors encountered" (apply append stxs))))]))

(define delay-errors? (make-parameter #t))

(define (tc-error/delayed msg #:stx [stx (current-orig-stx)] . rest)
  (if (delay-errors?)
      (set! delayed-errors (cons (make-err (apply format msg rest) (list (locate-stx stx))) delayed-errors))
      (raise-typecheck-error (apply format msg rest) (list (locate-stx stx)))))

;; produce a type error, using the current syntax
(define (tc-error msg . rest)  
  (raise-typecheck-error (apply format msg rest) (list (locate-stx (current-orig-stx)))))

;; produce a type error, given a particular syntax
(define (tc-error/stx stx msg . rest)
  (parameterize ([current-orig-stx stx])
    (apply tc-error msg rest)))

;; check two identifiers to see if they have the same name
(define (symbolic-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

;; parameter for currently-defined type aliases
;; this is used only for printing type names
(define current-type-names (make-parameter (lambda () '())))

;; error for unbound variables
(define (lookup-fail e) (tc-error "unbound identifier ~a" e))  


;; for reporting internal errors in the type checker
(define-struct (exn:fail:tc exn:fail) ())

;; raise an internal error - typechecker bug!
(define (int-err msg . args) 
  (raise (make-exn:fail:tc (string-append "Internal Typechecker Error: " (apply format msg args))
                           (current-continuation-marks))))

(define-syntax (nyi stx)
  (syntax-case stx ()
    [(_ str)
     (quasisyntax/loc stx (int-err "~a: not yet implemented: ~a" str #,(syntax/loc stx (this-expression-file-name))))]
    [(_) (syntax/loc stx (nyi ""))]))


;; are we currently expanding in a typed module (or top-level form)?
(define typed-context? (box #f))

;; what type names have been referred to in this module?
(define type-name-references (make-parameter '()))

(define (add-type-name-reference t)
  (type-name-references (cons t (type-name-references))))

