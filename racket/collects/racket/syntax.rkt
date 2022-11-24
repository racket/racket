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
            [depthmap (for/list ([x (in-list pvar-env)])
                        (let loop ([x x] [d 0])
                          (if (pair? x)
                              (loop (car x) (add1 d))
                              (cons x d))))]
            [pvars (map car depthmap)]
            [depths (map cdr depthmap)])
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

(define current-recorded-disappeared-uses (make-parameter #f #f 'current-recorded-disappeared-uses))

(define-syntax-rule (with-disappeared-uses body-expr ... stx-expr)
  (let-values ([(stx disappeared-uses)
                (parameterize ((current-recorded-disappeared-uses null))
                  (let ([result (let () body-expr ... stx-expr)])
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

(define (record-disappeared-uses ids [intro? (syntax-transforming?)])
  (cond
    [(identifier? ids) (record-disappeared-uses (list ids) intro?)]
    [(and (list? ids) (andmap identifier? ids))
     (let ([uses (current-recorded-disappeared-uses)])
       (when uses
         (current-recorded-disappeared-uses 
          (append
           (if intro?
               (map syntax-local-introduce ids)
               ids)
           uses))))]
    [else (raise-argument-error 'record-disappeared-uses
                                "(or/c identifier? (listof identifier?))"
                                ids)]))


;; == Identifier formatting ==

(define (format-id lctx
                   #:source [src #f]
                   #:props [props #f]
                   #:cert [cert #f]
                   #:subs? [subs? #f]
                   #:subs-intro [subs-intro (default-intro)]
                   fmt . args)
  (unless (or (syntax? lctx) (eq? lctx #f))
    (apply raise-argument-error 'format-id "(or/c syntax? #f)" 0 lctx fmt args))
  (check-restricted-format-string 'format-id fmt)
  (define arg-strs (map (lambda (a) (->string a 'format-id)) args))
  (define str (apply format fmt arg-strs))
  (define id (datum->syntax lctx (string->symbol str) src props))
  (cond [subs?
         (syntax-property id 'sub-range-binders
                          (make-subs 'format-id id fmt args arg-strs subs-intro))]
        [else id]))
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
  (define (convert x) (->string x 'format-symbol))
  (check-restricted-format-string 'format-symbol fmt)
  (let ([args (map convert args)])
    (string->symbol (apply format fmt args))))

(define (restricted-format-string? fmt)
  (regexp-match? #rx"^(?:[^~]|~[aAn~%])*$" fmt))

(define (check-restricted-format-string who fmt)
  (unless (restricted-format-string? fmt)
    (raise-arguments-error who
                           "format string should have only ~a placeholders"
                           "format string" fmt)))

(define (make-subs who id fmt args arg-strs intro)
  (define seglens (restricted-format-string-segment-lengths fmt))
  (for/fold ([len 0] [subs null] #:result subs) ;; len is total length so far
            ([arg (in-list args)] [arg-str (in-list arg-strs)] [seglen (in-list seglens)])
    (define len* (+ len seglen))
    (values (+ len* (string-length arg-str))
            (cond [(identifier? arg)
                   (cons (make-subrange (intro id) (intro arg)
                                        len* (string-length arg-str))
                         subs)]
                  [else subs]))))

(define (make-subrange new-id old-id start-in-new-id old-id-len)
  (vector-immutable new-id start-in-new-id old-id-len 0.5 0.5
                    old-id 0 old-id-len 0.5 0.5))

(define (restricted-format-string-segment-lengths fmt)
  ;; Returns (list p1 p2 ...) s.t. the Nth placeholder follows pN characters
  ;; generated from the format string since the previous placeholder.
  ;; Example: for "~ax~~ayz~aw~a", want '(0 5 1).
  ;; PRE: fmt is restricted-format-string.
  (let loop ([start 0] [since-last 0])
    (cond [(regexp-match-positions #rx"~." fmt start)
           => (lambda (p)
                (let ([m-start (caar p)] [m-end (cdar p)])
                  (case (string-ref fmt (add1 m-start))
                    [(#\a #\A)
                     (cons (+ since-last (- m-start start)) (loop m-end 0))]
                    [else ;; "~[^aA]" produces 1 char
                     (loop (+ since-last (- m-start start) 1))])))]
          [else null])))

(define (default-intro)
  (if (syntax-transforming?) syntax-local-introduce values))

(define (->string orig err)
  (let loop ([x orig])
    (cond
      [(syntax? x) (loop (syntax-e x))]
      [(string? x) x]
      [(symbol? x) (symbol->string x)]
      [(keyword? x) (keyword->string x)]
      [(number? x) (number->string x)]
      [(char? x) (string x)]
      [else (raise-argument-error
             err
             (string-append "(or/c string? symbol? keyword? char? number?\n"
                            "      (syntax/c (or/c string? symbol? keyword? char? number?)))")
             orig)])))

;; == Error reporting ==

(define current-syntax-context
  (make-parameter #f
                  (lambda (new-value)
                    (unless (or (syntax? new-value) (eq? new-value #f))
                      (raise-argument-error 'current-syntax-context
                                            "(or/c syntax? #f)"
                                            new-value))
                    new-value)
                  'current-syntax-context))

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

;; Included for backwards compatibility.
(define (internal-definition-context-apply intdefs stx)
  ; The old implementation of internal-definition-context-apply implicitly converted its stx argument
  ; to syntax, which some things seem to (possibly unintentionally) rely on, so replicate that
  ; behavior here:
  (internal-definition-context-introduce intdefs (datum->syntax #f stx) 'add))

(define (syntax-local-eval stx [intdefs #f])
  #;
  (unless (syntax? stx)
    (raise-argument-error 'syntax-local-eval
                          "syntax?"
                          0 stx intdefs))
  (unless (or (internal-definition-context? intdefs)
              (not intdefs)
              (and (list? intdefs) (andmap internal-definition-context? intdefs)))
    (raise-argument-error 'syntax-local-eval
                          (string-append
                           "(or/c internal-definition-context?\n"
                           "      #f\n"
                           "      (listof internal-definition-context?))")
                          1 stx intdefs))
  
  (let* ([name (generate-temporary)]
         [intdef (syntax-local-make-definition-context)]
         [all-intdefs (cond
                        [(internal-definition-context? intdefs) (list intdef intdefs)]
                        [(not intdefs)   (list intdef)]
                        [(list? intdefs) (cons intdef intdefs)])])
    (syntax-local-bind-syntaxes (list name)
                                #`(call-with-values (lambda () #,stx) list)
                                intdef
                                all-intdefs)
    (apply values
           (syntax-local-value (for/fold ([name name]) ([intdef all-intdefs])
                                 (internal-definition-context-introduce intdef name 'add))
                               #f
                               intdef))))

(define-syntax (with-syntax* stx)
  (syntax-case stx ()
    [(_ () body ...) (syntax/loc stx (let () body ...))]
    [(_ (cl) body ...) (syntax/loc stx (with-syntax (cl) body ...))]
    [(_ (cl cls ...) body ...)
     (with-syntax ([with-syntax/rest (syntax/loc stx (with-syntax* (cls ...) body ...))])
       (syntax/loc stx (with-syntax (cl) with-syntax/rest)))]))
