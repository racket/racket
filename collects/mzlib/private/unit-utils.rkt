#lang racket/base

(require (for-syntax racket/base
                     syntax/boundmap
                     "unit-compiletime.rkt"
                     "unit-syntax.rkt")
         racket/contract/base)

(provide (for-syntax build-key
                     check-duplicate-sigs
                     check-unit-ie-sigs
                     iota
                     process-unit-import
                     process-unit-export
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

(define-for-syntax (get-member-bindings member-table sig pos)
  (for/list ([i (in-list (map car (car sig)))]
             [c (in-list (cadddr sig))])
    (let ([add-ctc
             (λ (v stx)
                 (if c
                     (with-syntax ([c-stx (syntax-property c 'inferred-name v)])
                       #`(let ([v/c (#,stx)])
                           (contract c-stx (car v/c) (cdr v/c) #,pos
                                     (quote #,v) (quote-syntax #,v))))
                     #`(#,stx)))])
      #`[#,i
         (make-set!-transformer
          (λ (stx)
            (syntax-case stx (set!)
              [x
               (identifier? #'x)
               #'#,(add-ctc i (bound-identifier-mapping-get
                               member-table
                               i))]
              [(x . y)
               #'(#,(add-ctc i (bound-identifier-mapping-get
                                member-table
                                i)) . y)])))])))

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
  (let ([dup (check-duplicate-identifier
              (apply append (map sig-int-names import-sigs)))])
    (when dup
      (raise-stx-err 
       (format "~a is imported by multiple signatures" (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier
              (apply append (map sig-int-names export-sigs)))])
    (when dup
      (raise-stx-err (format "~a is exported by multiple signatures"
                             (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier 
              (append
               (apply append (map sig-int-names import-sigs))
               (apply append (map sig-int-names export-sigs))))])
    (when dup
      (raise-stx-err (format "import ~a is exported" (syntax-e dup))))))

(define-for-syntax (process-unit-import/export process)
  (lambda (s)
    (define x1 (syntax->list s))
    (define x2 (map process x1))
    (values x1 x2 (map car x2) (map cadr x2) (map caddr x2))))

(define-for-syntax process-unit-import
  (process-unit-import/export process-tagged-import))

(define-for-syntax process-unit-export
  (process-unit-import/export process-tagged-export))
  
;; build-key : (or symbol #f) identifier -> syntax-object
(define-for-syntax (build-key tag i)
  (if tag
      #`(cons '#,tag #,i)
      i))

;; tagged-info->keys : (cons (or symbol #f) siginfo) -> (listof syntax-object)
(define-for-syntax (tagged-info->keys tagged-info)
  (define tag (car tagged-info))
  (map (lambda (rid) 
         (build-key tag (syntax-local-introduce rid)))
       (siginfo-rtime-ids (cdr tagged-info))))

