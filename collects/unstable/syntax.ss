#lang scheme/base
;; owner: ryanc
(require syntax/kerncase
         syntax/stx
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
                       (loop (cdr (vector->list (struct->vector x))))))]
          [else x])))

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
  (let ([args (for/list ([arg args]) (if (syntax? arg) (syntax->datum arg) arg))])
    (string->symbol (apply format fmt args))))

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
