#lang racket/base
;; owner: ryanc (and cce and stamourv, where noted)
(require racket/syntax
         syntax/kerncase
         syntax/stx
         unstable/struct
         unstable/srcloc
         (for-syntax racket/base)
         (for-template racket/base unstable/private/expand))

(provide unwrap-syntax
         with-temporaries
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

         ;; by stamourv:
         
         format-unique-id

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

;; Generating temporaries

;; with-temporaries
(define-syntax-rule (with-temporaries (temp-name ...) . body)
  (with-syntax ([(temp-name ...) (generate-temporaries (quote-syntax (temp-name ...)))])
    . body))
;; Eli: +1 to this, not sure about the next two

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

(define (format-unique-id lctx
                          #:source [src #f]
                          #:props [props #f]
                          #:cert [cert #f]
                          fmt . args)
  ((make-syntax-introducer) (apply format-id
                                   lctx #:source src #:props props #:cert cert
                                   fmt args)))
