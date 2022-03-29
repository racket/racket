#lang racket/base
(require (only-in '#%linklet
                  primitive-table
                  primitive-in-category?)
         racket/cmdline
         "../../schemify/schemify.rkt"
         "../../cify/literal.rkt"
         "../../schemify/known.rkt"
         "../../schemify/lift.rkt"
         "../../schemify/reinfer-name.rkt"
         "../../schemify/wrap.rkt"
         "../../schemify/match.rkt"
         "../../cify/main.rkt"
         "help-startup.rkt")

(define version-line (format "/* version: ~a */" (version)))

(define debug? #f)

(define-values (dest src vers deps)
  (command-line
   #:args (dest-file src-file vers-file . dep)
   (values dest-file src-file vers-file dep)))

(define content (get-linklet src))
(define version-comparisons (get-version-comparisons vers))
                       
(define l (cdddr content))

(define (arity->mask a)
  (cond
   [(exact-nonnegative-integer? a)
    (arithmetic-shift 1 a)]
   [(arity-at-least? a)
    (bitwise-xor -1 (sub1 (arithmetic-shift 1 (arity-at-least-value a))))]
   [(list? a)
    (let loop ([mask 0] [l a])
      (cond
       [(null? l) mask]
       [else
        (let ([a (car l)])
          (cond
           [(or (exact-nonnegative-integer? a)
                (arity-at-least? a))
            (loop (bitwise-ior mask (arity->mask a)) (cdr l))]
           [else #f]))]))]
   [else #f]))

(define prim-knowns
  (for*/hash ([table-name '(#%linklet #%kernel
                                      #%paramz #%unsafe #%foreign
                                      #%futures #%place
                                      #%flfxnum #%extfl #%network)]
              [(name v) (in-hash (primitive-table table-name))])
    (values name
            (cond
              [(procedure? v)
               (define arity-mask (arity->mask (procedure-arity v)))
               (cond
                 [(primitive-in-category? name 'omitable)
                  (known-procedure/succeeds arity-mask)]
                 [else
                  (known-procedure arity-mask)])]
              [else
               a-known-constant]))))

(printf "Serializable...\n")
(define-values (bodys/literals-extracted literals)
  (time (extract-literals l)))

;; Startup code reuses names to keep it compact; make
;; te names unique again
(define bodys/re-uniqued
  (cdr (re-unique `(begin . ,bodys/literals-extracted))))

(printf "Schemify...\n")
(define body
  (time
   (schemify-body (recognize-inferred-names bodys/re-uniqued) prim-knowns #hasheq() #hasheq() #hasheq()
                  'cify
                  ;; unsafe mode:
                  #t
                  ;; no prompts:
                  #t
                  ;; no explicit unnamed:
                  #f)))

(printf "Lift...\n")
(define lifted-body
  (time
   (lift-in-schemified-body body)))

(define converted-body
  (append (for/list ([p (in-list literals)])
            (cons 'define p))
          lifted-body))

;; Convert 'inferred-name properties back to `(lambda <formals> (begin 'name <expr>))` form
(define (restore-inferred-names e)
  (cond
    [(wrap? e)
     (cond
       [(wrap-property e 'inferred-name)
        => (lambda (name)
             (match e
               [`(lambda ,formals ,expr)
                `(lambda ,formals (begin ',name ,(restore-inferred-names expr)))]
               [`(case-lambda [,formals ,expr] . ,rest)
                `(case-lambda [,formals (begin ',name ,(restore-inferred-names expr))]
                              . ,(restore-inferred-names rest))]
               [`,_
                (restore-inferred-names (unwrap e))]))]
       [else
        (restore-inferred-names (unwrap e))])]
    [(not (pair? e)) e]
    [else (cons (restore-inferred-names (car e))
                (restore-inferred-names (cdr e)))]))

(cify dest (caddr content) `(begin . ,(restore-inferred-names converted-body)) prim-knowns
      #:debug? debug?
      #:preamble (append (list version-line
                               (format "#if 0 ~a" version-comparisons)
                               "#include \"startup.inc\""
                               "#else")
                         (if debug?
                             (list "# define c_VALIDATE_DEBUG")
                             (list))
                         (list "# include \"startup-glue.inc\""))
      #:postamble (list (format "#endif")))
