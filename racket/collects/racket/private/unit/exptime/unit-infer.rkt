#lang racket/base

(require "import-export.rkt"
         "signature.rkt"
         "util.rkt"
         (for-template racket/base
                       "../keywords.rkt"
                       "../runtime.rkt"))

(provide (struct-out unit-info)
         (struct-out link-record)
         unprocess-link-record-bind unprocess-link-record-use
         lookup-def-unit
         complete-exports complete-imports)

;; (make-unit-info identifier (listof (cons symbol identifier)) (listof (cons symbol identifier)) identifier boolean)
(define-struct unit-info (unit-id import-sig-ids export-sig-ids deps orig-binder contracted?)
  #:property prop:procedure
  (lambda (struct stx) 
    (with-syntax ((u (syntax-local-introduce (unit-info-unit-id struct))))
      (syntax-case stx (set!)
        ((set! x y)
         (if (unit-info-contracted? struct)
             (raise-syntax-error 'set!
                                 "cannot set! a contracted unit"
                                 stx
                                 (syntax x))
             #`(begin 
                 #,(syntax/loc #'y (check-unit y 'set!))
                 #,(syntax/loc #'y (check-sigs y (unit-import-sigs u) (unit-export-sigs u) 'set!))
                 (set! u y))))
        ((_ . y)
         (syntax/loc stx (u . y)))
        (x
         (identifier? #'x)
         (quasisyntax/loc stx (values u))))))) ;; The apparently superfluous values is so the certificates aren't
;; too permissive

(define (lookup-def-unit id)
  (let ((u (lookup id "unknown unit definition")))
    (unless (unit-info? u)
      (raise-stx-err "not a unit definition" id))
    u))


;; A link-record is
;; (make-link-record (or symbol #f) (or identifier #f) identifier siginfo)
(define-struct link-record (tag linkid sigid siginfo))

;; complete-exports : (listof link-record) (listof link-record) -> (listof link-record)
;; The export-bindings should not contain two bindings that are related as subsignatures.
(define (complete-exports unit-exports given-bindings)
  (define binding-table (make-hash))
  (define used-binding-table (make-hash))
  
  (check-duplicate-subs 
   (map (λ (ts) (cons (link-record-tag ts) (link-record-siginfo ts))) given-bindings)
   (map link-record-sigid given-bindings))
  
  (for-each 
   (λ (b)
     (hash-set! binding-table 
                (cons (link-record-tag b)
                      (car (siginfo-ctime-ids (link-record-siginfo b))))
                (link-record-linkid b)))
   given-bindings)
  
  (begin0
    (map
     (λ (export)
       (define r
         (ormap 
          (λ (ctime-id)
            (define key (cons (link-record-tag export) ctime-id))
            (define used (hash-ref used-binding-table key (λ () #f)))
            (when used
              (raise-stx-err "this export is supplied multiple times by the given unit" used))
            (let ([r (hash-ref binding-table key (λ () #f))])
              (when r
                (hash-set! used-binding-table key r))
              r))
          (siginfo-ctime-ids (link-record-siginfo export))))
       (make-link-record
        (link-record-tag export)
        (cond
          [r r]
          [else (car (generate-temporaries (list (link-record-linkid export))))])
        (link-record-sigid export)
        (link-record-siginfo export)))
     unit-exports)
    
    (hash-for-each
     binding-table
     (λ (k v)
       (unless (hash-ref used-binding-table k (λ () #f))
         (raise-stx-err "this export is not supplied by the given unit" v))))))

(define (name-form n) (syntax->datum n))

;; complete-imports : (hash-tableof symbol (or identifier 'duplicate))
;;                    (listof link-record)
;;                    (listof (list symbol identifier siginfo)) ->
;;                    (listof (cons symbol identifier))
(define (complete-imports sig-table given-links unit-imports src)
  (define linked-sigs-table (make-hash))
  (for-each
   (λ (link)
     (define tag (link-record-tag link))
     (for-each
      (λ (cid)
        (define there? (hash-ref linked-sigs-table (cons tag cid) (λ () #f)))
        (hash-set! linked-sigs-table (cons tag cid) (if there? 'duplicate #t)))
      (siginfo-ctime-ids (link-record-siginfo link))))
   given-links)
  
  (append
   given-links
   (let loop ([unit-imports unit-imports])
     (cond
       [(null? unit-imports) null]
       [else
        (let* ([import (car unit-imports)]
               [ctime-ids (siginfo-ctime-ids (link-record-siginfo import))]
               [tag (link-record-tag import)]
               [there?
                (hash-ref linked-sigs-table
                          (cons tag (car ctime-ids))
                          (λ () #f))])
          (cond
            [(eq? 'duplicate there?)
             (raise-stx-err
              (if tag
                  (format "specified linkages satisfy (tag ~a ~a) import multiple times"
                          tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                  (format "specified linkages satisfy untagged ~a import multiple times"
                          (name-form (car (siginfo-names (link-record-siginfo import))))))
              src)]
            [there?
             (loop (cdr unit-imports))]
            [else
             (let ([there?2 (hash-ref sig-table
                                      (car ctime-ids)
                                      (λ () #f))])
               (cond
                 [(eq? 'duplicate there?2)
                  (raise-stx-err 
                   (if tag
                       (format "multiple linkages satisfy (tag ~a ~a) import"
                               tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                       (format "multiple linkages satisfy untagged ~a import"
                               (name-form (car (siginfo-names (link-record-siginfo import))))))
                   src)]
                 [there?2
                  (for-each
                   (λ (cid)
                     (hash-set! linked-sigs-table
                                (cons tag cid)
                                #t))
                   ctime-ids)
                  (cons (make-link-record (link-record-tag import)
                                          there?2
                                          (link-record-sigid import)
                                          (link-record-siginfo import))
                        (loop (cdr unit-imports)))]
                 [else
                  (raise-stx-err
                   (if tag
                       (format "no linkages satisfy (tag ~a ~a) import"
                               tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                       (format "no linkages satisfy untagged ~a import"
                               (name-form (car (siginfo-names (link-record-siginfo import))))))
                   src)]))]))]))))

(define (unprocess-link-record-bind lr)
  (if (link-record-tag lr)
      #`(#,(link-record-linkid lr) : (tag #,(link-record-tag lr) #,(link-record-sigid lr)))
      #`(#,(link-record-linkid lr) : #,(link-record-sigid lr))))

(define (unprocess-link-record-use lr)
  (if (link-record-tag lr)
      #`(tag #,(link-record-tag lr) #,(link-record-linkid lr))
      (link-record-linkid lr)))
