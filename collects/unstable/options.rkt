#lang racket


(provide option/c transfer-option exercise-option waive-option)


(require syntax/location 
         (for-syntax racket/provide-transform))


(struct info (val proj blame))

(define-values (impersonator-prop:proxy proxy? proxy-info) 
  (make-impersonator-property 'proxy))


(define (build-proxy val proj blame)
  (let ([proxy-info (info val proj blame)])
    (cond [(procedure? val)
           (chaperone-procedure
            val
            values
            impersonator-prop:proxy 
            proxy-info)]
          [(vector? val)
           (chaperone-vector
            val
            (λ (v i val) val)
            (λ (v i val) val)
            impersonator-prop:proxy 
            proxy-info)]
          [(hash? val)
           (chaperone-hash
            val
            (λ (h k) (values k (λ (h k v) v)))
            (λ (h k v) (values k v))
            (λ (h k) k)
            (λ (h k) k)
            impersonator-prop:proxy 
            proxy-info)]
          [(struct? val)
           (chaperone-hash
            val
            (λ (h k) (values k (λ (h k v) v)))
            (λ (h k v) (values k v))
            (λ (h k) k)
            (λ (h k) k)
            impersonator-prop:proxy 
            proxy-info)])))


(define (run-tester tester val orig-ctc blame here)
  (let ([indy-blame (blame-replace-negative blame here)]
        [proj (contract-projection orig-ctc)]
        [option-blame
         (λ (blame context)
           (blame-add-context blame context))])  
    (unless (tester ((proj 
                      (option-blame 
                       blame
                       (format "in option contract tester ~e" tester))) 
                     val))
      (raise-blame-error 
       (option-blame indy-blame (format "option contract tester ~e failure" tester)) 
       val
       ""))))

(struct option (orig-ctc tester here)
  #:property prop:contract
  (build-contract-property
   #:name 
   (λ (ctc) (contract-name (option-orig-ctc ctc)))
   #:first-order
   (λ (ctc)
     (λ (val)
       ((contract-first-order (option-orig-ctc ctc)) val)))
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (let  ([tester (option-tester ctc)]
                [orig-ctc (option-orig-ctc ctc)]
                [here (option-here ctc)])
           (unless (symbol? tester)
             (run-tester tester val orig-ctc blame here))
           (build-proxy val (contract-projection orig-ctc) blame)))))))

(define (build-option ctc here #:tester [tester 'dont-care])
  (option ctc tester here))

(define-syntax (option/c stx)
  (syntax-case stx ()
    [(_ con) 
     (syntax-property
      #`(build-option con (quote-module-name))
      'racket/contract:contract
      (vector (gensym 'option/c)
              (list (car (syntax-e stx)))
              '()))]
    [(_ con key tester) 
     (syntax-property
      #`(build-option con (quote-module-name) key tester)
      'racket/contract:contract
      (vector (gensym 'option/c)
              (list (car (syntax-e stx)))
              '()))]))

(struct transfer ()
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (let ([option-blame
                (blame-add-context 
                 blame
                 (format "option contract transfer failure: ~a does not have an option" val))]
               [pos-blame (blame-positive blame)]
               [neg-blame (blame-negative blame)])
           (cond [(proxy? val)
                  (let ((info (proxy-info val)))
                    (build-proxy
                     (info-val info)
                     (info-proj info)
                     (blame-update (info-blame info) pos-blame neg-blame)))]
                 [else (raise-blame-error option-blame val "")])))))))

(define-syntax transfer/c
  (syntax-id-rules ()
    [_ (transfer)]))

(define-for-syntax (fresh-names l)
  (map (lambda (x) (datum->syntax #f (gensym x))) (syntax->datum l)))

(define-syntax transfer-option
  (make-provide-pre-transformer
   (lambda (stx modes)
     (unless (or (null? modes)
                 (and (= 1 (length modes))
                      (zero? (car modes))))
       (raise-syntax-error #f
                           "allowed only in relative phase-level 0"
                           stx))
     (syntax-case stx ()
       [(_ id ... )
        (syntax-local-lift-module-end-declaration
         (with-syntax ([(new-id ...) (generate-temporaries #'(id ...))])
           #`(begin
               (begin (define new-id id) ...)
               (provide (contract-out [rename new-id id transfer/c] ...)))))])
     #`(combine-out))))

(define (exercise-option val)
  (cond [(proxy? val)
         (let ((info (proxy-info val)))
           (((info-proj info)
             (info-blame info))
            (info-val info)))]
        [else (error 'exercise-error "~a has no option to exercise" val)]))

(define (waive-option val)
  (cond [(proxy? val) (info-val (proxy-info val))]
        [else (error 'exercise-error "~a has no option to exercise" val)]))



