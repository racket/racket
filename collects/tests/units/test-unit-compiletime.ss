(require-for-syntax (lib "unit-compiletime.ss" "mzlib" "private")
                    (lib "unit-syntax.ss" "mzlib" "private"))
(require "test-harness.ss"
         (lib "unit-compiletime.ss" "mzlib" "private")
         (lib "unit-keywords.ss" "mzlib" "private")
         (lib "unit-syntax.ss" "mzlib" "private"))


;; split-requires
;; UNTESTED

;; build-siginfo + siginfo-subtype
(test #t (siginfo-subtype (make-siginfo '(n1 n2 n3) '(c1 c2 c3)  (syntax->list #'(r1 r2 r3)))
                          (make-siginfo '(n1 n2 n3) '(c1 c2 c3)  (syntax->list #'(r1 r2 r3)))))
(test #t (siginfo-subtype (make-siginfo '(n1 n2 n3) '(c1 c2 c3)  (syntax->list #'(r1 r2 r3)))
                          (make-siginfo '(n2 n3) '(c2 c3)  (syntax->list #'(r2 r3)))))
(test #f (siginfo-subtype (make-siginfo '(n2 n3) '(c2 c3)  (syntax->list #'(r2 r3)))
                          (make-siginfo '(n1 n2 n3) '(c1 c2 c3)  (syntax->list #'(r1 r2 r3)))))
      

;; signature-proc
(test-syntax-error "illegal use of signature name"
  (let ()
    (define-syntax x (make-signature 1 2 3 4))
    x))
(test-syntax-error "illegal use of signature name"
  (let ()
    (define-syntax x (make-signature 1 2 3 4))
    (x 1)))

;; signature-form-proc
(test-syntax-error "illegal use of signature form"
  (let ()
    (define-syntax x (make-signature-form 1))
    x))
(test-syntax-error "illegal use of signature form"
  (let ()
    (define-syntax x (make-signature-form 1))
    (x 1)))

;; unit-info-proc
(test '1
  (let ()
    (define x 1)
    (define-syntax y (make-unit-info #'x null null null))
    y))
(test 2
  (let ()
    (define x +)
    (define-syntax y (make-unit-info #'x null null null))
    (y 1 1)))
(test-runtime-error exn:fail:contract? "not a unit"
  (let ()
    (define x +)
    (define-syntax y (make-set!-transformer (make-unit-info #'x null null null)))
    (set! y 1)))

;; lookup-signature
(define-syntax (lookup-sig-mac stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ id)
       #`#,(signature-siginfo (lookup-signature #'id))))))
(test-syntax-error "lookup-signature: not id"
  (lookup-sig-mac 1))
(test-syntax-error "lookup-signature: unbound id"
  (lookup-sig-mac s))
(test-syntax-error "lookup-signature: not a sig"
  (lookup-sig-mac lookup-sig-mac))
(let ()
  (define-syntax x (make-signature 1 2 3 4))
  (test 1 (lookup-sig-mac x)))

;; process-import
(define-syntax (process-import-mac-sig stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ x)
       #`#'#,(caddr (process-tagged-import #'x))))))
(define-for-syntax si (make-siginfo '(test-sig) '(ct) (list #'rt)))
(define-syntax test-sig (make-signature si
                                        (list #'x #'y)
                                        (list (cons (list #'v1 #'v2) #'body1)
                                              (cons (list #'v3 #'v4) #'body2))
                                        (list (cons (list #'s1 #'s2) #'body3)
                                              (cons (list #'s3 #'s4) #'body4))))

(test stx-bound-id=? #'(((x . x) (y . y))
                        ((((v1 . v1) (v2 . v2)) . body1)
                         (((v3 . v3) (v4 . v4)) . body2))
                        ((((s1 . s1) (s2 . s2)) . body3)
                         (((s3 . s3) (s4 . s4)) . body4)))
  (process-import-mac-sig test-sig))
(let ()
  (define-syntax ts (make-signature 1 (list #'x) null null))
  (define x-stx #'x)
  (let ((x 1))
    (test stx-bound-id=? #`(((x . #,x-stx)) () ())
      (process-import-mac-sig ts))))
(let ()
  (define-syntax ts (make-signature 1 null (list (cons (list #'v1 #'v2) #'body1)) null))
  (define v2-stx #'v2)
  (let ((v2 1))
    (test stx-bound-id=? #`(() ((((v1 . v1) (v2 . #,v2-stx)) . body1)) ())
      (process-import-mac-sig ts))))
(let ()
  (define-syntax ts (make-signature 1 null null (list (cons (list #'s1 #'s2) #'body3))))
  (define s1-stx #'s1)
  (let ((s1 1))
    (test stx-bound-id=? #`(() () ((((s1 . #,s1-stx) (s2 . s2)) . body3)))
      (process-import-mac-sig ts))))
(let ((b 1))
  (define-syntax test-sig2 (make-signature 1 null (list (cons (list #'v5) #'b)) null))
  (test stx-bound-id=? #'(()
                          ((((v5 . v5)) . b))
                          ())
    (let ((b 1)) (process-import-mac-sig test-sig2))))
(let ((b 2))
  (define-syntax test-sig2 (make-signature 1 null (list (cons (list #'v5) #'b)) null))
  (test stx-bound-id=? #'(()
                          ((((v5 . v5)) . b))
                          ())
    (let ((b 3)) (process-import-mac-sig test-sig2))))

(test-syntax-error "process-import: only, id not in spec"
  (process-import-mac (only test-sig x z)))
(test stx-bound-id=? #'(((x . x) ((#f . y) . y))
                        (((((#f . v1) . v1) (v2 . v2)) . body1)
                         ((((#f . v3) . v3) ((#f  . v4) . v4)) . body2))
                        ((((s1 . s1) ((#f . s2) . s2)) . body3)
                         ((((#f . s3) . s3) ((#f . s4) . s4)) . body4)))
  (process-import-mac-sig (only test-sig x v2 s1)))

(test-syntax-error "process-import: except, id not in spec"
  (process-import-mac (except test-sig x z)))
(test stx-bound-id=? #'(((x . x) ((#f . y) . y))
                        (((((#f . v1) . v1) (v2 . v2)) . body1)
                         ((((#f . v3) . v3) ((#f  . v4) . v4)) . body2))
                        ((((s1 . s1) ((#f . s2) . s2)) . body3)
                         ((((#f . s3) . s3) ((#f . s4) . s4)) . body4)))
  (process-import-mac-sig (except test-sig y v1 v3 v4 s2 s3 s4)))

(test stx-bound-id=? #'(((u:x . x) (u:y . y))
                        ((((u:v1 . v1) (u:v2 . v2)) . body1)
                         (((u:v3 . v3) (u:v4 . v4)) . body2))
                        ((((u:s1 . s1) (u:s2 . s2)) . body3)
                         (((u:s3 . s3) (u:s4 . s4)) . body4)))
  (process-import-mac-sig (prefix u: test-sig)))

(test-syntax-error "process-import: rename clause id not in spec"
  (process-import-mac (rename test-sig (z a))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (y x))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (s1 v1))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (x v1))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (v3 x))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (s4 x))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (y s3))))
(test-syntax-error "process-import-mac: rename clash"
  (process-import-mac (rename test-sig (v2 s3))))
(test-syntax-error "process-import-mac: rename, duplicate"
  (process-import-mac (rename test-sig (a x) (b x))))
(test-syntax-error "process-import-mac: rename, duplicate"
  (process-import-mac (rename test-sig (a s1) (b s1))))
(test-syntax-error "process-import-mac: rename, duplicate"
  (process-import-mac (rename test-sig (a v2) (b v2))))
(test stx-bound-id=? #'(((x . x) (a . y))
                        ((((b . v1) (v2 . v2)) . body1)
                         (((v3 . v3) (v4 . v4)) . body2))
                        ((((s1 . s1) (c . s2)) . body3)
                         (((s3 . s3) (s4 . s4)) . body4)))
  (process-import-mac-sig (rename test-sig (a y) (b v1) (c s2))))

;; process-export
(define-syntax (process-export-mac stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ x)
       #`#'#,(caddr (process-tagged-export #'x))))))
                             
(test-syntax-error "process-export: malformed"
  (process-export-mac (x y)))
(test-syntax-error "process-export: dot"
  (process-export-mac (x . y)))
(test-syntax-error "process-export: not id"
  (process-export-mac 1))
(test stx-bound-id=?  #'(((x . x) (y . y))
                        ((((v1 . v1) (v2 . v2)) . body1)
                         (((v3 . v3) (v4 . v4)) . body2))
                        ((((s1 . s1) (s2 . s2)) . body3)
                         (((s3 . s3) (s4 . s4)) . body4)))
  (process-export-mac test-sig))
(let ()
  (define-syntax ts (make-signature 1 (list #'x) null null))
  (define x-stx #'x)
  (let ((x 1))
    (test stx-bound-id=? #`(((x . #,x-stx)) () ())
      (process-export-mac ts))))
(let ()
  (define-syntax ts (make-signature 1 null (list (cons (list #'v1 #'v2) #'body1)) null))
  (define v2-stx #'v2)
  (let ((v2 1))
    (test stx-bound-id=? #`(() ((((v1 . v1) (v2 . #,v2-stx)) . body1)) ())
      (process-export-mac ts))))
(let ()
  (define-syntax ts (make-signature 1 null null (list (cons (list #'s1 #'s2) #'body3))))
  (define s1-stx #'s1)
  (let ((s1 1))
    (test stx-bound-id=? #`(() () ((((s1 . #,s1-stx) (s2 . s2)) . body3)))
      (process-export-mac ts))))

(test stx-bound-id=?  #'(((u:x . x) (u:y . y))
                        ((((u:v1 . v1) (u:v2 . v2)) . body1)
                         (((u:v3 . v3) (u:v4 . v4)) . body2))
                        ((((u:s1 . s1) (u:s2 . s2)) . body3)
                         (((u:s3 . s3) (u:s4 . s4)) . body4)))
  (process-export-mac (prefix u: test-sig)))

(test-syntax-error "process-export: rename clause id not in spec"
  (process-export-mac (rename test-sig (z a))))
(test-syntax-error "process-export-mac: rename, duplicate"
  (process-export-mac (rename test-sig (a x) (b x))))
(test stx-bound-id=? #'(((x . x) (a . y))
                        ((((b . v1) (v2 . v2)) . body1)
                         (((v3 . v3) (v4 . v4)) . body2))
                        ((((s1 . s1) (c . s2)) . body3)
                         (((s3 . s3) (s4 . s4)) . body4)))
  (process-export-mac (rename test-sig (a y) (b v1) (c s2))))

;; 


(define-syntax (extract-sig-runtime-macro stx)
  (parameterize ((error-syntax stx))
    (syntax-case stx ()
      ((_ x)
       #`#'#,(syntax-local-introduce (car (siginfo-rtime-ids (signature-siginfo (lookup-signature #'x)))))))))

(test bound-identifier=? #'rt
  (extract-sig-runtime-macro test-sig))

;; Complete-exports
(define s1 (make-siginfo '(a1 a2) '(c1 c2) ()))
(define s2 (make-siginfo '(b1 b2) '(c3 c2) ()))
(define s3 (make-siginfo '(b1 b2) '(c3 c2) ()))
(define s4 (make-siginfo '(c1 c2) '(c5 c4) ()))
(define s5 (make-siginfo '(d) '(c4) ()))
(define e1 (make-link-record #f #f #'a s1))
(define e2 (make-link-record #f #f #'b s2))
(define e3 (make-link-record 't #f #'b s3))
(define e4 (make-link-record 't #f #'c s4))
(define e5 (make-link-record 't #f #'d s5))
(define unit-exports (list e1 e2 e3 e4))

(define (add-lnkid l lr)
  (make-link-record (link-record-tag lr) l (link-record-sigid lr) (link-record-siginfo lr)))

(define (lnk-comp lr1 lr2)
  (andmap
   (λ (lr1 lr2)
     (and (eq? (link-record-tag lr1) (link-record-tag lr2))
          (bound-identifier=? (link-record-sigid lr1) (link-record-sigid lr2))
          (eq? (link-record-siginfo lr1) (link-record-siginfo lr2))
          (if (and (link-record-linkid lr1) (link-record-linkid lr2))
              (equal? (link-record-linkid lr1) (link-record-linkid lr2))
           #t)))
   lr1
   lr2))
           
(test lnk-comp unit-exports
      (complete-exports unit-exports '()))

(test lnk-comp (map add-lnkid '(4 3 2 1) unit-exports)
      (complete-exports unit-exports (map add-lnkid '(1 2 3 4) (reverse unit-exports))))

(let ([r (complete-exports unit-exports (list (add-lnkid 1 e2) (add-lnkid 2 e3)))])
  (test lnk-comp unit-exports  r))

(let ([r (complete-exports unit-exports (list (add-lnkid 1 e5)))])
  (test lnk-comp unit-exports r))

(parameterize ([error-syntax #'complete-exports])
  
  (test-runtime-error exn:fail:syntax? "complete-exports: duplicate bindings"
                      (complete-exports unit-exports (list (add-lnkid 1 e2) (add-lnkid 2 e3) (add-lnkid 3 e2))))
  
  (test-runtime-error exn:fail:syntax? "complete-exports: duplicate bindings"
                      (complete-exports unit-exports (list (add-lnkid 1 e4) (add-lnkid 2 e5))))
  
  (test-runtime-error exn:fail:syntax? "complete-exports: invalid link"
                      (complete-exports unit-exports (list (make-link-record #f 1 #'z (make-siginfo 'z '(c9) ())))))
  
  (test-runtime-error exn:fail:syntax? "complete-exports: ambiguous links"
                      (complete-exports unit-exports (list (make-link-record #f 1 #'z (make-siginfo 'z '(c2) ()))))))
  

(define unit-imports (cons e5 unit-exports))

(define sig-table 
  (make-immutable-hash-table `((c1 . duplicate)
                               (c2 . 1)
                               (c3 . 2))))

(test lnk-comp `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1) ,(add-lnkid 2 e2) ,(add-lnkid 2 e3))
      (complete-imports sig-table `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1)) unit-imports #'stx))

(test lnk-comp `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1) ,(add-lnkid 5 e2) ,(add-lnkid 2 e3))
      (complete-imports sig-table `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1) ,(add-lnkid 5 e2)) unit-imports #'stx))

(test lnk-comp `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1) ,(add-lnkid 5 e3) ,(add-lnkid 2 e2))
      (complete-imports sig-table `(,(add-lnkid 3 e4) ,(add-lnkid 4 e1) ,(add-lnkid 5 e3)) unit-imports #'stx))

(parameterize ([error-syntax #'complete-imports])
  
  (test-runtime-error exn:fail:syntax? "complete-imports: ambiguous"
                      (complete-imports sig-table `(,(add-lnkid 3 e4)) unit-imports #'stx))
  
  (test-runtime-error exn:fail:syntax? "complete-imports: missing"
                      (complete-imports sig-table `(,(add-lnkid 4 e1)) unit-imports #'stx))
  
  (test-runtime-error exn:fail:syntax? "complete-imports: duplicate"
                      (complete-imports sig-table
                                        `(,(add-lnkid 4 e1) ,(add-lnkid 5 e2))
                                        `(,(make-link-record #f #f #'e (make-siginfo '(a2) '(c2) ())))
                                        #'stx))
  )
