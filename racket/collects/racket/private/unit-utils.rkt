#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/boundmap
                     "unit-compiletime.rkt"
                     "unit-syntax.rkt")
         racket/contract/base)

(provide (for-syntax check-duplicate-sigs
                     check-unit-ie-sigs
                     iota
                     tagged-info->keys
                     get-member-bindings))

(provide equal-hash-table
         unit-export)

(define-for-syntax (iota n)
  (let loop ((n n)
             (acc null))
    (cond
      ((= n 0) acc)
      (else (loop (sub1 n) (cons (sub1 n) acc))))))

(define-syntax-rule (equal-hash-table [k v] ...)
  (make-immutable-hash (list (cons k v) ...)))

;; get-member-bindings : bound-identifier-mapping? signature-ie? syntax? any/c -> (listof syntax?)
(define-for-syntax (get-member-bindings member-table sig pos bind?)
  (for/list ([int-id (in-list (map car (signature-vars sig)))]
             [ext-id (in-list (map cdr (signature-vars sig)))]
             [ctc (in-list (signature-ctcs sig))])

    (define (add-ctc name-id val-expr)
      (if ctc
          (with-syntax ([ctc* (syntax-property ctc 'inferred-name name-id)])
            #`(let ([v/c (#,val-expr)])
                (contract ctc* (car v/c) (cdr v/c) #,pos
                          (quote #,name-id) (quote-syntax #,name-id))))
          #`(#,val-expr)))

    #`[#,(if bind? ext-id int-id)
       (make-set!-transformer
        (λ (stx)
          (syntax-case stx (set!)
            [x
             (identifier? #'x)
             (quote-syntax
              #,(add-ctc int-id (bound-identifier-mapping-get
                                 member-table
                                 int-id)))]
            [(x . y)
             (quasisyntax
              (#,(quote-syntax
                  #,(add-ctc int-id (bound-identifier-mapping-get
                                     member-table
                                     int-id)))
               . y))])))]))

(define-syntax (unit-export stx)
  (syntax-case stx ()
    ((_ ((esig ...) elocs) ...)
     (with-syntax ((((kv ...) ...)
                    (map 
                     (lambda (esigs eloc)
                       (map
                        (lambda (esig) #`(#,esig #,eloc))
                        (syntax->list esigs)))
                     (syntax->list #'((esig ...) ...))
                     (syntax->list #'(elocs ...)))))
       #'(equal-hash-table kv ... ...)))))

;; check-duplicate-sigs : (listof (cons symbol siginfo)) (listof syntax-object)
;;                        (listof (cons symbol siginfo)) (listof syntax-object) ->
(define-for-syntax (check-duplicate-sigs tagged-siginfos sources tagged-deps dsources)
  (define import-idx (make-hash))
  (for-each
   (lambda (tinfo s)
     (define key (cons (car tinfo)
                       (car (siginfo-ctime-ids (cdr tinfo)))))
     (when (hash-ref import-idx key #f)
       (raise-stx-err "duplicate import signature" s))
     (hash-set! import-idx key #t))
   tagged-siginfos
   sources)
  (for-each
   (lambda (dep s)
     (unless (hash-ref import-idx
                       (cons (car dep)
                             (car (siginfo-ctime-ids (cdr dep))))
                       #f)
       (raise-stx-err "initialization dependency on unknown import" s)))
   tagged-deps
   dsources))
  
(define-for-syntax (check-unit-ie-sigs import-sigs export-sigs)
  (define import-names (append-map signature-ie-int-names import-sigs))
  (define export-names (append-map signature-ie-int-names export-sigs))

  (let ([dup (check-duplicate-identifier import-names)])
    (when dup
      (raise-stx-err
       (format "~a is imported by multiple signatures" (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier export-names)])
    (when dup
      (raise-stx-err (format "~a is exported by multiple signatures"
                             (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier  (append import-names export-names))])
    (when dup
      (raise-stx-err (format "import ~a is exported" (syntax-e dup))))))

;; tagged-info->keys : (cons (or symbol #f) siginfo) -> (listof syntax-object)
(define-for-syntax (tagged-info->keys tagged-info)
  (define tag (car tagged-info))
  (map (lambda (rid) 
         (build-key tag (syntax-local-introduce rid)))
       (siginfo-rtime-ids (cdr tagged-info))))

