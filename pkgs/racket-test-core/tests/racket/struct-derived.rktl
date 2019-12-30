(load-relative "loadtest.rktl")

(Section 'struct-derived)

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 ;; Check that struct/derived can be instantiated
                 (test (void) (lambda ()
                                (new-struct foobar (buzz bazz))
                                (void)))

                 ;; Confirm struct define-values exist
                 (let ()
                   (new-struct foobar (buzz bazz))
                   (test #t procedure? foobar)
                   (test #t struct-type? struct:foobar)
                   (test #t procedure? foobar?)
                   (test #t procedure? foobar-buzz)
                   (test #t procedure? foobar-bazz))

                 ;; Confirm struct define-values work
                 (let ()
                   (new-struct foobar (buzz bazz bizz))
                   (test #t foobar? (foobar 1 2 3))
                   (let ([val (foobar 1 2 3)])
                     (test 1 foobar-buzz val)
                     (test 2 foobar-bazz val)
                     (test 3 foobar-bizz val)))

                 ;; Confirm make-struct not available
                 (let ()
                   (new-struct foobar (buzz bazz))
                   (err/rt-test (make-foobar 0 1))))
               (void)))

;;; ------------------

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 ;; Check that parent-child relationship can be instantiated
                 (test (void) (lambda ()
                                (struct foobar (buzz bazz))
                                (new-struct barfoo foobar (bizz))
                                (void)))
                 ;; Confirm struct define-values exist
                 (let ()
                   (struct barfoo (buzz bazz))
                   (new-struct foobar barfoo (bizz))
                   (test #t procedure? foobar)
                   (test #t struct-type? struct:foobar)
                   (test #t procedure? foobar?)
                   (test #t procedure? foobar-bizz))

                 ;; Confirm struct define-values work
                 (let ()
                   (struct barfoo (buzz bazz))
                   (new-struct foobar barfoo (bizz))
                   (test #t foobar? (foobar 1 2 3))
                   
                   (let ([val (foobar 1 2 3)])
                     (test 1 barfoo-buzz val)
                     (test 2 barfoo-bazz val)
                     (test 3 foobar-bizz val))))
               (void)))

;; Confirm both forms of constructor-name work
(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name extra (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) #:constructor-name extra . rest))]))
                 
                 (let ()
                   (new-struct foobar 
                               barfoo
                               (buzz bazz bizz))
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 
                 (let ()
                   (new-struct foobar (buzz bazz bizz) #:constructor-name barfoo)
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name extra parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) #:constructor-name extra . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar 
                               barfoo
                               zap
                               (buzz))
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar 
                               zap
                               (buzz)
                               #:constructor-name barfoo)
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))))
               (void)))

;; Confirm both forms of extra-constructor-name work
(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name extra (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) #:extra-constructor-name extra . rest))]))
                 
                 (let ()
                   (new-struct foobar 
                               barfoo
                               (buzz bazz bizz))
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 
                 (let ()
                   (new-struct foobar (buzz bazz bizz) #:extra-constructor-name barfoo)
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))
                   (test #t (lambda ()
                              (foobar? (foobar 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name extra parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) #:extra-constructor-name extra . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar 
                               barfoo
                               zap
                               (buzz))
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))
                   (test #t (lambda ()
                              (foobar? (foobar 0 1 2))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar 
                               zap
                               (buzz)
                               #:extra-constructor-name barfoo)
                   (test #t (lambda ()
                              (foobar? (barfoo 0 1 2))))
                   (test #t (lambda ()
                              (foobar? (foobar 0 1 2))))))
               (void)))

;; Confirm both forms of mutable work
(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) #:mutable . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               (buzz bazz bizz))
                   (test 0 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (foobar 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               (buzz bazz bizz)
                               #:mutable)
                   (test 0 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (foobar 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) #:mutable . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               (buzz))
                   (test 2 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (foobar 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               (buzz)
                               #:mutable)
                   
                   (test 2 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (foobar 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (foobar 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

;; Check multiple params work

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name extra (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) #:mutable #:extra-constructor-name extra . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               barfoo
                               (buzz bazz bizz))
                   (test #t foobar? (barfoo 0 1 2))
                   (test 0 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (barfoo 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               (buzz bazz bizz)
                               #:mutable
                               #:extra-constructor-name barfoo)
                   (test #t foobar? (barfoo 0 1 2))
                   (test 0 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (barfoo 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent extra (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) #:mutable #:extra-constructor-name extra . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               barfoo
                               (buzz))
                   
                   (test #t foobar? (barfoo 0 1 2))
                   (test 2 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (barfoo 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               (buzz)
                               #:mutable
                               #:extra-constructor-name barfoo)
                   (test #t foobar? (barfoo 0 1 2))
                   (test 2 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (foobar-buzz val))))
                   (test (void) (lambda ()
                                  (let ([val (barfoo 0 1 2)])
                                    (set-foobar-buzz! val 3))))
                   (test 3 (lambda ()
                             (let ([val (barfoo 0 1 2)])
                               (set-foobar-buzz! val 3)
                               (foobar-buzz val))))))
               (void)))


;; Confirm both forms of omit-define-values work

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) #:omit-define-values . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               (buzz bazz bizz))
                   (test (void) (lambda ()
                                  (define foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define struct:foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar? 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar-buzz 1)
                                  (void)))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name (fields ...) . rest))]))
                 
                 (let ()
                   (new-struct foobar
                               (buzz bazz bizz)
                               #:omit-define-values)
                   (test (void) (lambda ()
                                  (define foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define struct:foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar? 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar-buzz 1)
                                  (void)))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) #:omit-define-values . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               (buzz))
                   (test (void) (lambda ()
                                  (define foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define struct:foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar? 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar-buzz 1)
                                  (void)))))
               (void)))

(test (void) (lambda ()
               (let ()
                 (define-syntax (new-struct stx)
                   (syntax-case stx ()
                     [(ds name parent (fields ...) . rest)
                      (with-syntax ([orig stx])
                        #'(struct/derived orig name parent (fields ...) . rest))]))
                 
                 (let ()
                   (struct zap (zip zup))
                   (new-struct foobar
                               zap
                               (buzz)
                               #:omit-define-values)
                   (test (void) (lambda ()
                                  (define foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define struct:foobar 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar? 1)
                                  (void)))
                   (test (void) (lambda ()
                                  (define foobar-buzz 1)
                                  (void)))))
               (void)))

;;; ------------------

(report-errs)
