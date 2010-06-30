#lang racket/base
;; owner: ryanc (and cce, where noted)
(require syntax/kerncase
         syntax/stx
         unstable/struct
         unstable/srcloc
         (for-syntax racket/base
                     racket/private/sc)
         (for-template racket/base unstable/private/expand))

(provide unwrap-syntax

         define-pattern-variable
         define/with-syntax

         with-temporaries
         generate-temporary
         generate-n-temporaries

         current-recorded-disappeared-uses
         with-disappeared-uses
         syntax-local-value/record
         record-disappeared-uses

         format-symbol
         format-id

         current-syntax-context
         wrong-syntax

         internal-definition-context-apply
         syntax-local-eval

         with-syntax*
         syntax-map

         ;; by cce:

         to-syntax
         to-datum

         syntax-source-file-name
         syntax-source-directory

         trampoline-transformer
         quote-transformer
         redirect-transformer
         head-expand

         syntax-list

         )

;; Unwrapping syntax

;; unwrap-syntax : any #:stop (any -> boolean) -> any
(define (unwrap-syntax stx #:stop [stop-at (lambda (x) #f)])
  (let loop ([x stx])
    (cond [(stop-at x) x]
          [(syntax? x) (loop (syntax-e x))]
          [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
          [(vector? x) (apply vector-immutable (loop (vector->list x)))]
          [(box? x) (box-immutable (loop (unbox x)))]
          [(prefab-struct-key x)
           => (lambda (key)
                (apply make-prefab-struct key
                       (loop (struct->list x))))]
          [else x])))
;; Eli: Is there any difference between this (with the default) and
;;   `syntax->datum'?  If not, then maybe add the optional (or keyword) to
;;   there instead?
;; Ryan: syntax->datum errors if its arg is not syntax.

;; Defining pattern variables

(define-syntax-rule (define-pattern-variable name expr)
  (begin (define var expr)
         (define-syntax name (make-syntax-mapping '0 (quote-syntax var)))))

;; Statics and disappeared uses

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
  (let ([value (syntax-local-value id (lambda () #f))])
    (and (pred value)
         (begin (record-disappeared-uses (list id))
                value))))

(define (record-disappeared-uses ids)
  (let ([uses (current-recorded-disappeared-uses)])
    (when uses
      (current-recorded-disappeared-uses (append ids uses)))))

;; Generating temporaries

;; with-temporaries
(define-syntax-rule (with-temporaries (temp-name ...) . body)
  (with-syntax ([(temp-name ...) (generate-temporaries (quote-syntax (temp-name ...)))])
    . body))
;; Eli: +1 to this, not sure about the next two

;; generate-temporary : any -> identifier
(define (generate-temporary [stx 'g])
  (car (generate-temporaries (list stx))))

;; generate-n-temporaries : exact-nonnegative-integer -> (listof identifier)
(define (generate-n-temporaries n)
  (generate-temporaries
   (for/list ([i (in-range n)])
     (string->symbol (format "g~sx" i)))))

;; Symbol Formatting

(define (format-symbol fmt . args)
  (define (convert x) (->atom x 'format-symbol))
  (check-restricted-format-string 'format-symbol fmt)
  (let ([args (map convert args)])
    (string->symbol (apply format fmt args))))

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

(define (restricted-format-string? fmt)
  (regexp-match? #rx"^(?:[^~]|~[aAn~%])*$" fmt))

(define (check-restricted-format-string who fmt)
  (unless (restricted-format-string? fmt)
    (raise-type-error who
                      "format string using only ~a placeholders"
                      fmt)))

(define (->atom x err)
  (cond [(string? x) x]
        [(symbol? x) x]
        [(identifier? x) (syntax-e x)]
        [(keyword? x) (keyword->string x)]
        [(number? x) x]
	[(char? x) x]
        [else (raise-type-error err
                                "string, symbol, identifier, keyword, character, or number"
                                x)]))

;; Error reporting

(define current-syntax-context (make-parameter #f))

(define (wrong-syntax stx #:extra [extras null] format-string . args)
  (unless (or (eq? stx #f) (syntax? stx))
    (raise-type-error 'wrong-syntax "syntax or #f" 0 (list* stx format-string args)))
  (let* ([ctx (current-syntax-context)]
         [blame (and (syntax? ctx) (syntax-property ctx 'report-error-as))])
    (raise-syntax-error (if (symbol? blame) blame #f)
                        (apply format format-string args)
                        ctx
                        stx
                        extras)))
;; Eli: The `report-error-as' thing seems arbitrary to me.

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


(define-syntax (with-syntax* stx)
  (syntax-case stx ()
    [(_ (cl) body ...) #'(with-syntax (cl) body ...)]
    [(_ (cl cls ...) body ...)
     #'(with-syntax (cl) (with-syntax* (cls ...) body ...))]))

(define (syntax-map f . stxls)
  (apply map f (map syntax->list stxls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  By Carl Eastlund, below
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Pattern Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (syntax-list template ...)
  (syntax->list (syntax (template ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Conversions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (to-syntax datum
                   #:stx [stx #f]
                   #:src [src stx]
                   #:ctxt [ctxt stx]
                   #:prop [prop stx]
                   #:cert [cert stx])
  (datum->syntax ctxt
                 datum
                 (if (srcloc? src) (build-source-location-list src) src)
                 prop
                 cert))

;; Slightly different from unwrap-syntax,
;; in that it doesn't traverse anything that isn't immediately syntax.
;; At some point we should pick one of the other or a combination,
;; both is probably overkill.
(define (to-datum v)
  (if (syntax? v) (syntax->datum v) v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Syntax Locations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-source-directory stx)
  (let* ([source (syntax-source stx)])
    (and (path-string? source)
         (let-values ([(base file dir?) (split-path source)])
           (and (path? base)
                (path->complete-path base
                                     (or (current-load-relative-directory)
                                         (current-directory))))))))

(define (syntax-source-file-name stx)
  (let* ([f (syntax-source stx)])
    (and (path-string? f)
         (let-values ([(base file dir?) (split-path f)]) file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Transformer Patterns
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((redirect-transformer id) stx)
  (cond
   [(identifier? stx) id]
   [(and (stx-pair? stx) (identifier? (stx-car stx)))
    (to-syntax (cons id (stx-cdr stx)) #:stx stx)]
   [else
    (wrong-syntax
     stx
     "expected an identifier (alone or in application position); cannot redirect to ~a"
     (syntax-e id))]))

(define (head-expand stx [stop-ids null] [intdef-ctx #f])
  (local-expand stx
                (syntax-local-context)
                (append stop-ids (kernel-form-identifier-list))
                intdef-ctx))
;; Ryan: added intdef-ctx optional arg

(define (quote-transformer datum)
  #`(quasiquote
     #,(let loop ([datum datum])
         (cond
          [(syntax? datum) #`(unquote (quote-syntax #,datum))]
          [(pair? datum) #`#,(cons (loop (car datum)) (loop (cdr datum)))]
          [(vector? datum)
           #`#,(apply vector-immutable (map loop (vector->list datum)))]
          [(box? datum) #`#,(box (loop (unbox datum)))]
          [(hash? datum)
           #`#,((cond [(hash-eqv? datum) make-immutable-hasheqv]
                      [(hash-eq? datum) make-immutable-hasheq]
                      [else make-immutable-hash])
                (hash-map datum (lambda (k v) (cons k (loop v)))))]
          [(prefab-struct-key datum) =>
           (lambda (key)
             #`#,(apply make-prefab-struct
                        key
                        (for/list ([i (in-vector (struct->vector datum) 1)])
                          (loop i))))]
          [else #`#,datum]))))

(define trampoline-prompt-tag
  (make-continuation-prompt-tag 'trampoline))

(define ((trampoline-transformer transform) stx)

  (define intro (make-syntax-introducer))

  (define (body)
    (syntax-local-introduce
     (intro
      (transform (trampoline-evaluator intro)
                 intro
                 (intro (syntax-local-introduce stx))))))

  (call-with-continuation-prompt body trampoline-prompt-tag))

(define ((trampoline-evaluator intro) stx)

  (define ((wrap continue))
    (call-with-continuation-prompt continue trampoline-prompt-tag))

  (define ((expander continue))
    #`(begin #,(syntax-local-introduce (intro stx))
             (#%trampoline #,(wrap continue))))

  (define (body continue)
    (abort-current-continuation trampoline-prompt-tag (expander continue)))

  (call-with-composable-continuation body trampoline-prompt-tag)
  (void))
