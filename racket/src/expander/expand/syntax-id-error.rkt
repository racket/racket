#lang racket/base
(require "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/error.rkt"
         "context.rkt"
         "../syntax/debug.rkt")

(provide raise-ambiguous-error
         syntax-debug-info-string)

(define (raise-ambiguous-error id ctx)
  (raise-syntax-error #f
                      "identifier's binding is ambiguous"
                      id #f null
                      (syntax-debug-info-string id ctx)))

;; ----------------------------------------

(define (syntax-debug-info-string s ctx)
  (define info (syntax-debug-info s (expand-context-phase ctx) #f))
  (cond
   [(not (or (pair? (hash-ref info 'bindings null))
             (for*/or ([fb-info (in-list (hash-ref info 'fallbacks null))])
               (pair? (hash-ref fb-info 'bindings null)))))
    ;; Don't show context if there's no binding to compare it to
    ""]
   [else
    (define relevant-scope-sets
      (let loop ([info info] [layer 0])
        (apply
         append
         (cons (hash-ref info 'context)
               (for/list ([b (in-list (hash-ref info 'bindings null))])
                 (hash-ref b 'context)))
         (let ([fallbacks (hash-ref info 'fallbacks null)])
           (for/list ([fallback (in-list fallbacks)]
                      [layer (in-naturals (add1 layer))])
             (loop fallback layer))))))
    (define common-scopes
      (if (null? relevant-scope-sets)
          (set)
          (for/fold ([s (list->set (car relevant-scope-sets))]) ([l (in-list relevant-scope-sets)])
            (set-intersect s (list->set l)))))
    (string-append
     (let loop ([info info] [layer 0])
       (string-append
        "\n  context" (layer->string layer) "...:"
        (describe-context (hash-ref info 'context) common-scopes)
        (apply string-append
               (for/list ([b (in-list (sort (hash-ref info 'bindings null)
                                            ;; Order matches before non-matches:
                                            (lambda (a b)
                                              (and (hash-ref a 'match? #f)
                                                   (not (hash-ref b 'match? #f))))))])
                 (string-append
                  "\n  " (if (hash-ref b 'match? #f) "matching" "other") " binding" (layer->string layer) "...:"
                  "\n   " (if (hash-ref b 'local #f)
                              "local"
                              (format "~a" (hash-ref b 'module #f)))
                  (describe-context (hash-ref b 'context) common-scopes))))
        (let ([fallbacks (hash-ref info 'fallbacks null)])
          (apply
           string-append
           (for/list ([fallback (in-list fallbacks)]
                      [layer (in-naturals (add1 layer))])
             (loop fallback layer))))))
     (if (set-empty? common-scopes)
         ""
         (string-append
          "\n  common scopes...:"
          ;; Get scopes from the original context to keep them in the right order
          (describe-context (for/list ([s (in-list (hash-ref info 'context))]
                                       #:when (set-member? common-scopes s))
                              s)
                            (set)))))]))

(define (describe-context scopes common-scopes)
  (define strs
    (let loop ([strs null] [scopes (if (set-empty? common-scopes)
                                       scopes
                                       (append
                                        (for/list ([s (in-list scopes)]
                                                   #:when (not (set-member? common-scopes s)))
                                          s)
                                        (list "[common scopes]")))])
      (cond
       [(null? scopes) (reverse strs)]
       [else
        (define str (format " ~a" (car scopes)))
        (if (and (pair? strs)
                 ((+ (string-length str) (string-length (car strs))) . < . 72))
            (loop (cons (string-append (car strs) str)
                        (cdr strs))
                  (cdr scopes))
            (loop (cons str strs)
                  (cdr scopes)))])))
  (cond
   [(null? strs) "\n   [empty]"]
   [else
    (apply string-append (for/list ([str (in-list strs)])
                           (string-append "\n  " str)))]))

(define (layer->string layer)
  (if (zero? layer)
      ""
      (format " at layer ~a" layer)))
