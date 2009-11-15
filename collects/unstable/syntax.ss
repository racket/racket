#lang scheme/base
;; owner: ryanc
(require syntax/kerncase
         syntax/stx
         unstable/struct
         (for-syntax scheme/base
                     scheme/private/sc))

(provide unwrap-syntax

         define-pattern-variable

         with-temporaries
         generate-temporary
         generate-n-temporaries

         current-caught-disappeared-uses
         with-catching-disappeared-uses
         with-disappeared-uses
         syntax-local-value/catch
         record-disappeared-uses

         format-symbol
         format-id

         current-syntax-context
         wrong-syntax)

;; Unwrapping syntax

;; unwrap-syntax : any #:stop-at (any -> boolean) -> any
(define (unwrap-syntax stx #:stop-at [stop-at (lambda (x) #f)])
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

;; Defining pattern variables

(define-syntax-rule (define-pattern-variable name expr)
  (begin (define var expr)
         (define-syntax name (make-syntax-mapping '0 (quote-syntax var)))))

;; Statics and disappeared uses

(define current-caught-disappeared-uses (make-parameter #f))

(define-syntax-rule (with-catching-disappeared-uses . body)
  (parameterize ((current-caught-disappeared-uses null))
    (let ([result (let () . body)])
      (values result (current-caught-disappeared-uses)))))

(define-syntax-rule (with-disappeared-uses stx-expr)
  (let-values ([(stx disappeared-uses)
                (with-catching-disappeared-uses stx-expr)])
    (syntax-property stx
                     'disappeared-use
                     (append (or (syntax-property stx 'disappeared-use) null)
                             disappeared-uses))))

(define (syntax-local-value/catch id pred)
  (let ([value (syntax-local-value id (lambda () #f))])
    (and (pred value)
         (begin (record-disappeared-uses (list id))
                value))))

(define (record-disappeared-uses ids)
  (let ([uses (current-caught-disappeared-uses)])
    (when uses
      (current-caught-disappeared-uses (append ids uses)))))

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

#|
(define (id-append #:source [src #f]
                   #:props [props #f]
                   #:cert [cert #f]
                   . args)
  (define stxs (filter syntax? args))
  (define lctx
    (cond [(and (pair? stxs) (null? (cdr stxs)))
           (car stxs)]
          [(error 'id-append "expected exactly one identifier in arguments: ~e" args)]))
  (define (convert x) (->atom x 'id-append))
  (define sym (string->symbol (apply string-append (map convert args))))
  (datum->syntax lctx sym src props cert))
|#

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
        [else (raise-type-error err
                                "string, symbol, identifier, keyword, or number"
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
