#lang racket/base

#|
This file is for utilities that are only useful for Typed Racket, but
don't depend on any other portion of the system
|#

(require syntax/source-syntax "disappeared-use.rkt"
         racket/match racket/promise racket/string
         syntax/parse (for-syntax racket/base syntax/parse)
         (only-in unstable/sequence in-slice))

(provide ;; parameters
         current-orig-stx
         orig-module-stx
         expanded-module-stx
         print-syntax?
         warn-unreachable?
         delay-errors?
         current-type-names

         stringify
         locate-stx
         warn-unreachable

         reset-errors!
         report-first-error
         report-all-errors
         error-at-stx-loc?
         tc-error/fields
         tc-error/delayed
         tc-error
         tc-error/stx
         int-err

         typed-context?
         make-env
         id-from?
         id-from

         (all-from-out "disappeared-use.rkt"))

;; a parameter representing the original location of the syntax being
;; currently checked
(define current-orig-stx (make-parameter #'here))
(define orig-module-stx (make-parameter #f))
(define expanded-module-stx (make-parameter #f))

(define (stringify l [between " "])
  (define (intersperse v l)
    (cond [(null? l) null]
          [(null? (cdr l)) l]
          [else (cons (car l) (cons v (intersperse v (cdr l))))]))
  (apply string-append
         (intersperse between (map (lambda (s) (format "~a" s)) l))))

;; do we print the fully-expanded syntax in error messages?
(define print-syntax? (make-parameter #f))


(define warn-unreachable? (make-parameter #t))

(define (warn-unreachable e)
  (let ([l (current-logger)]
        [stx (locate-stx e)])
    (when (and (warn-unreachable?)
               (log-level? l 'warning)
               (and (syntax-transforming?)
                    (syntax-original? (syntax-local-introduce e)))
               #;(and (orig-module-stx)
                      (eq? (debugf syntax-source-module e)
                           (debugf syntax-source-module (orig-module-stx))))
               #;(syntax-source-module stx))
      (log-message l 'warning
                   (format "Typed Racket has detected unreachable code: ~.s"
                           (locate-stx e))
                   e))))

(define locate-stx
  ;; this hash handles using `locate-stx` even when orig/expand change
  (let ([recover-table (make-hash)])
    (lambda (stx)
      (define omodule (orig-module-stx))
      (define emodule (expanded-module-stx))
      (cond [(and (not (print-syntax?)) omodule emodule stx)
	     (define recover
	       (hash-ref! recover-table (cons omodule emodule)
			  (lambda () (recover-source-syntax omodule emodule))))
	     (or (recover stx) stx)]
	    [else stx]))))

(define (raise-typecheck-error msg stxs)
  (if (null? (cdr stxs))
      (raise-syntax-error (string->symbol "Type Checker") msg (car stxs))
      (raise-syntax-error (string->symbol "Type Checker") msg #f #f stxs)))

(define delayed-errors null)

(define-struct err (msg stx) #:prefab)

(define-values (save-errors! restore-errors!)
  (let ([v (box #f)])
    (values (lambda () (set-box! v delayed-errors))
            (lambda () (set! delayed-errors (unbox v))))))

(define (reset-errors!) (set! delayed-errors null))

(define (report-first-error)
  (match (reverse delayed-errors)
    [(list) (void)]
    [(cons (struct err (msg stx)) _)
     (reset-errors!)
     (raise-typecheck-error msg stx)]))

(define (report-all-errors)
  (match (reverse delayed-errors)
    [(list) (void)]
    ;; if there's only one, we don't need multiple-error handling
    [(list (struct err (msg stx)))
     (reset-errors!)
     (raise-typecheck-error msg stx)]
    [l
     (let ([stxs
            (for/list ([e (in-list l)])
              (with-handlers ([exn:fail:syntax?
                               (λ (e) ((error-display-handler) (exn-message e) e))])
                (raise-typecheck-error (err-msg e) (err-stx e)))
              (err-stx e))])
       (reset-errors!)
       (unless (null? stxs)
         (raise-typecheck-error (format "Summary: ~a errors encountered"
                                        (length stxs))
                                (apply append stxs))))]))

;; Returns #t if there's a type error recorded at the same position as
;; the given syntax object. Does not return a useful result if the
;; source, position, or span are #f.
(define (error-at-stx-loc? stx)
  (for/or ([an-err (in-list delayed-errors)])
    (match-define (struct err (_ stxes)) an-err)
    (define stx* (and (not (null? stxes)) (car stxes)))
    (and stx*
         (equal? (syntax-source stx*) (syntax-source stx))
         (= (syntax-position stx*) (syntax-position stx))
         (= (syntax-span stx*) (syntax-span stx)))))

;; send type errors to DrRacket's tooltip system
(define-logger online-check-syntax)
(define (log-type-error msg stxes)
  (define stx (and (not (null? stxes)) (car stxes)))
  (when (and stx
             (syntax-position stx)
             (syntax-span stx))
    (define tooltip-info
      ;; see type-table.rkt for why we do this
      (if (or (not (pair? (syntax-e stx)))
              (let ([fst (car (syntax-e stx))])
                (and (identifier? fst)
                     (free-identifier=? fst #'quote))))
          (list (vector stx
                        (sub1 (syntax-position stx))
                        (+ (sub1 (syntax-position stx)) (syntax-span stx))
                        msg))
          (list (vector stx
                        (sub1 (syntax-position stx))
                        (syntax-position stx)
                        msg)
                (vector stx
                        (sub1 (+ (sub1 (syntax-position stx))
                                 (syntax-span stx)))
                        (+ (sub1 (syntax-position stx))
                           (syntax-span stx))
                        msg))))
    (log-message online-check-syntax-logger
                 'info
                 "TR's type error tooltip syntaxes; this message is ignored"
                 (list (syntax-property #'(void) 'mouse-over-tooltips tooltip-info)))))

(define delay-errors? (make-parameter #f))

(define (tc-error/delayed msg #:stx [stx* (current-orig-stx)] . rest)
  (let ([stx (locate-stx stx*)])
    (unless (syntax? stx)
      (int-err "erroneous syntax was not a syntax object: ~a ~a"
               stx (syntax->datum stx*)))
    (if (delay-errors?)
        (set! delayed-errors (cons (make-err (apply format msg rest)
                                             (list stx))
                                   delayed-errors))
        (raise-typecheck-error (apply format msg rest) (list stx)))))

;; Produce a type error using modern Racket error syntax.
;; Avoid using format directives in the `msg`, `more`, and `field`
;; strings in the rest argument (may cause unexpected errors)
(define (tc-error/fields msg
                         #:more [more #f]
                         #:stx [stx (current-orig-stx)]
                         #:delayed? [delayed? #f]
                         . rst)
  (unless (even? (length rst))
    (raise-argument-error
     'tc-error/fields
     "alternating fields and values"
     rst))
  (define-values (field-strs vals)
    (for/fold ([field-strs null] [vals null])
              ([field+value (in-slice 2 rst)])
      (match-define (list field value) field+value)
      (define field-strs*
        (cons (format "  ~a: ~~a" field) field-strs))
      (values field-strs* (cons value vals))))
  (define more-msg (if more (string-append ";\n " more "\n") "\n"))
  (define all-fields (string-join (reverse field-strs) "\n"))
  (define final-msg
    (string-trim #:left? #f (string-append msg more-msg all-fields)))
  (if delayed?
      (apply tc-error/delayed #:stx stx final-msg (reverse vals))
      (apply tc-error/stx stx final-msg (reverse vals))))

;; produce a type error, using the current syntax
(define (tc-error msg . rest)
  (let* ([ostx (current-orig-stx)]
         [ostxs (if (list? ostx) ostx (list ostx))]
         [stxs (map locate-stx ostxs)])
    ;; If this isn't original syntax, then we can get some pretty bogus error
    ;; messages.  Note that this is from a macro expansion, so that introduced
    ;; vars and such don't confuse the user.
    (cond
     [(or (not (orig-module-stx))
          (for/and ([s (in-list ostxs)])
            (eq? (syntax-source s) (syntax-source (orig-module-stx)))))
      (raise-typecheck-error (apply format msg rest) stxs)]
     [else (raise-typecheck-error
            (apply format (string-append "Error in macro expansion -- " msg)
                   rest)
            stxs)])))

;; produce a type error, given a particular syntax
(define (tc-error/stx stx msg . rest)
  (parameterize ([current-orig-stx stx])
    (apply tc-error msg rest)))

;; parameter for currently-defined type aliases
;; this is used only for printing type names
(define current-type-names (make-parameter (lazy '())))

;; for reporting internal errors in the type checker
(define-struct (exn:fail:tc exn:fail) ())

;; raise an internal error - typechecker bug!
(define (int-err msg . args)
  (raise (make-exn:fail:tc
          (string-append
           "Internal Typechecker Error: "
           (apply format msg args)
           (format "\nwhile typechecking:\n~a\noriginally:\n~a"
                   (syntax->datum (current-orig-stx))
                   (syntax->datum (locate-stx (current-orig-stx)))))
          (current-continuation-marks))))

;; are we currently expanding in a typed module (or top-level form)?
(define typed-context? (box #f))

;; environment constructor
(define-syntax (make-env stx)
  (define-syntax-class spec
    #:transparent
    #:attributes (ty id)
    (pattern [nm:identifier ~! ty]
             #:fail-when (and (not (list? (identifier-template-binding #'nm))) #'nm)
             "not a bound identifier"
             #:with id #'(quote-syntax nm))
    (pattern [e:expr ty]
             #:with id #'e))
  (syntax-parse stx
    [(_ e:spec ...)
     #'(list (list e.id e.ty) ...)]))

;; id: identifier
;; sym: a symbol
;; mod: a quoted require spec like 'racket/base
;; is id the name sym defined in mod?
(define (id-from? id sym mod)
  (and (eq? (syntax-e id) sym)
       (eq? (module-path-index-resolve (syntax-source-module id))
            ((current-module-name-resolver) mod #f #f #f))))

(define-syntax-class (id-from sym mod)
  (pattern i:id
           #:fail-unless (id-from? #'i sym mod) #f))
