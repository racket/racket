#lang racket/base
(require (for-syntax racket/base
                     racket/private/sc))
(provide define/with-syntax

         current-recorded-disappeared-uses
         with-disappeared-uses
         syntax-local-value/record
         record-disappeared-uses

         format-symbol
         format-id

         current-syntax-context
         wrong-syntax

         generate-temporary
         internal-definition-context-apply
         syntax-local-eval
         with-syntax*)

;; == Defining pattern variables ==

(define-syntax (define/with-syntax stx)
  (syntax-case stx ()
    [(define/with-syntax pattern rhs)
     (let* ([pvar-env (get-match-vars #'define/with-syntax
                                      stx
                                      #'pattern
                                      '())]
            [depthmap (for/list ([x pvar-env])
                        (let loop ([x x] [d 0])
                          (if (pair? x)
                              (loop (car x) (add1 d))
                              (cons x d))))]
            [pvars (map car depthmap)]
            [depths (map cdr depthmap)]
            [mark (make-syntax-introducer)])
       (with-syntax ([(pvar ...) pvars]
                     [(depth ...) depths]
                     [(valvar ...) (generate-temporaries pvars)])
         #'(begin (define-values (valvar ...)
                    (with-syntax ([pattern rhs])
                      (values (pvar-value pvar) ...)))
                  (define-syntax pvar
                    (make-syntax-mapping 'depth (quote-syntax valvar)))
                  ...)))]))
;; Ryan: alternative name: define/syntax-pattern ??

;; auxiliary macro
(define-syntax (pvar-value stx)
  (syntax-case stx ()
    [(_ pvar)
     (identifier? #'pvar)
     (let ([mapping (syntax-local-value #'pvar)])
       (unless (syntax-pattern-variable? mapping)
         (raise-syntax-error #f "not a pattern variable" #'pvar))
       (syntax-mapping-valvar mapping))]))


;; == Disappeared uses ==

(define current-recorded-disappeared-uses (make-parameter #f))

(define-syntax-rule (with-disappeared-uses stx-expr)
  (let-values ([(stx disappeared-uses)
                (parameterize ((current-recorded-disappeared-uses null))
                  (let ([result stx-expr])
                    (values result (current-recorded-disappeared-uses))))])
    (syntax-property stx
                     'disappeared-use
                     (append (or (syntax-property stx 'disappeared-use) null)
                             disappeared-uses))))

(define (syntax-local-value/record id pred)
  (unless (identifier? id)
    (raise-argument-error 'syntax-local-value/record
                          "identifier?"
                          0 id pred))
  (unless (and (procedure? pred)
               (procedure-arity-includes? pred 1))
    (raise-argument-error 'syntax-local-value/record
                          "(-> any/c boolean?)"
                          1 id pred))
  (let ([value (syntax-local-value id (lambda () #f))])
    (and (pred value)
         (begin (record-disappeared-uses (list id))
                value))))

(define (record-disappeared-uses ids)
  (unless (and (list? ids) (andmap identifier? ids))
    (raise-argument-error 'record-disappeared-uses
                          "(listof identifier?)"
                          ids))
  (let ([uses (current-recorded-disappeared-uses)])
    (when uses
      (current-recorded-disappeared-uses 
       (append
        (if (syntax-transforming?)
            (map syntax-local-introduce ids)
            ids)
        uses)))))


;; == Identifier formatting ==

(define (format-id lctx
                   #:source [src #f]
                   #:props [props #f]
                   #:cert [cert #f]
                   fmt . args)
  (define (convert x) (->atom x 'format-id))
  (check-restricted-format-string 'format-id fmt)
  (let* ([args (map convert args)]
         [str (apply format fmt args)]
         [sym (string->symbol str)])
    (datum->syntax lctx sym src props cert)))
;; Eli: This looks very *useful*, but I'd like to see it more convenient to
;;   "preserve everything".  Maybe add a keyword argument that when #t makes
;;   all the others use values lctx, and when syntax makes the others use that
;;   syntax?
;;   Finally, if you get to add this, then another useful utility in the same
;;   spirit is one that concatenates symbols and/or strings and/or identifiers
;;   into a new identifier.  I considered something like that, which expects a
;;   single syntax among its inputs, and will use it for the context etc, or
;;   throw an error if there's more or less than 1.

(define (format-symbol fmt . args)
  (define (convert x) (->atom x 'format-symbol))
  (check-restricted-format-string 'format-symbol fmt)
  (let ([args (map convert args)])
    (string->symbol (apply format fmt args))))

(define (restricted-format-string? fmt)
  (regexp-match? #rx"^(?:[^~]|~[aAn~%])*$" fmt))

(define (check-restricted-format-string who fmt)
  (unless (restricted-format-string? fmt)
    (raise-arguments-error who
                           (format "format string should have ~a placeholders"
                                   fmt)
                           "format string" fmt)))

(define (->atom x err)
  (cond [(string? x) x]
        [(symbol? x) x]
        [(identifier? x) (syntax-e x)]
        [(keyword? x) (keyword->string x)]
        [(number? x) x]
	[(char? x) x]
        [else (raise-argument-error err
                                    "(or/c string? symbol? identifier? keyword? char? number?)"
                                    x)]))


;; == Error reporting ==

(define current-syntax-context
  (make-parameter #f
                  (lambda (new-value)
                    (unless (or (syntax? new-value) (eq? new-value #f))
                      (raise-argument-error 'current-syntax-context
                                            "(or/c syntax? #f)"
                                            new-value))
                    new-value)))

(define (wrong-syntax stx #:extra [extras null] format-string . args)
  (unless (or (eq? stx #f) (syntax? stx))
    (raise-argument-error 'wrong-syntax "(or/c syntax? #f)" 0 (list* stx format-string args)))
  (let* ([ctx (current-syntax-context)]
         [blame (and (syntax? ctx) (syntax-property ctx 'report-error-as))])
    (raise-syntax-error (if (symbol? blame) blame #f)
                        (apply format format-string args)
                        ctx
                        stx
                        extras)))
;; Eli: The `report-error-as' thing seems arbitrary to me.


;; == Other utilities ==

;; generate-temporary : any -> identifier
(define (generate-temporary [stx 'g])
  (car (generate-temporaries (list stx))))

;; Applies the renaming of intdefs to stx.
(define (internal-definition-context-apply intdefs stx)
  (let ([qastx (local-expand #`(quote #,stx) 'expression (list #'quote) intdefs)])
    (with-syntax ([(q astx) qastx]) #'astx)))

(define (syntax-local-eval stx [intdef0 #f])
  (let* ([name (generate-temporary)]
         [intdefs (syntax-local-make-definition-context intdef0)])
    (syntax-local-bind-syntaxes (list name)
                                #`(call-with-values (lambda () #,stx) list)
                                intdefs)
    (internal-definition-context-seal intdefs)
    (apply values
           (syntax-local-value (internal-definition-context-apply intdefs name)
                               #f intdefs))))

(define-syntax (with-syntax* stx)
  (syntax-case stx ()
    [(_ (cl) body ...) #'(with-syntax (cl) body ...)]
    [(_ (cl cls ...) body ...)
     #'(with-syntax (cl) (with-syntax* (cls ...) body ...))]))
