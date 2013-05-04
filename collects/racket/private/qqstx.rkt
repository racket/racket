;;----------------------------------------------------------------------
;; #%qqstx : quasisyntax

(module qqstx '#%kernel
  (#%require "small-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt"
             (for-syntax '#%kernel "small-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt"))

  (#%provide quasisyntax
             quasisyntax/loc
             unsyntax
             unsyntax-splicing
             quasidatum
             undatum
             undatum-splicing)
  
  (define-syntaxes (unsyntax unsyntax-splicing)
    (let ([f (lambda (stx)
	       (raise-syntax-error
		#f
		"illegal outside of quasisyntax"
		stx))])
      (values f f)))

  (define-syntaxes (undatum undatum-splicing)
    (let ([f (lambda (stx)
	       (raise-syntax-error
		#f
		"illegal outside of quasidatum"
		stx))])
      (values f f)))

  (-define (check-splicing-list l ctx)
    (unless (stx-list? l)
      (raise-argument-error
       'unsyntax-splicing
       "syntax->list"
       l))
    (datum->syntax ctx l ctx))

  (-define (check-splicing-datum-list l ctx)
    (unless (list? l)
      (raise-argument-error
       'undatum-splicing
       "list?"
       l))
    l)

  (define-syntaxes (quasisyntax quasisyntax/loc quasidatum)
    (let* ([gen-qq
            (lambda (orig-stx body mk-final who unsyntax-id unsyntax-splicing-id quasisyntax-id 
                              with-syntax-id check-splicing-list-id)
              (let ([here-stx #'here])
                (let loop ([stx body]
                           [depth 0]
                           [same-k (lambda ()
                                     (datum->syntax 
                                      here-stx
                                      (mk-final body)
                                      orig-stx))]
                           [convert-k (lambda (body bindings)
                                        (datum->syntax 
                                         here-stx
                                         (list
                                          with-syntax-id
                                          bindings
                                          (mk-final body))
                                         orig-stx))])
                  (syntax-case stx ()
                    [(us x)
                     (and (identifier? #'us)
                          (free-identifier=? #'us unsyntax-id))
                     (if (zero? depth)
                         (let ([temp (car (generate-temporaries '(uq)))])
                           (convert-k temp (list (list temp (syntax x)))))
                         (loop (syntax x) (sub1 depth)
                               same-k
                               (lambda (v bindings)
                                 (convert-k (datum->syntax
                                             here-stx
                                             (list (stx-car stx) v)
                                             stx)
                                            bindings))))]
                    [us
                     (and (identifier? #'us)
                          (free-identifier=? #'us unsyntax-id))
                     (raise-syntax-error
                      #f
                      (format "misuse within ~a" who)
                      orig-stx
                      stx)]
                    [((us-s x) . rest)
                     (and (identifier? #'us-s)
                          (free-identifier=? #'us-s unsyntax-splicing-id))
                     (if (zero? depth)
                         (if (stx-null? (syntax rest))
                             (with-syntax ([temp (car (generate-temporaries '(uqs1)))])
                               (convert-k (datum->syntax
                                           stx
                                           (syntax temp)
                                           stx)
                                          (list #'[temp x])))
                             (let ([rest-done-k
                                    (lambda (rest-v bindings)
                                      (with-syntax ([temp (car (generate-temporaries '(uqs)))]
                                                    [ctx (datum->syntax #'x 'ctx #'x)])
                                        (convert-k (datum->syntax
                                                    stx
                                                    (list* (syntax temp)
                                                           (quote-syntax ...)
                                                           rest-v)
                                                    stx
                                                    stx)
                                                   (with-syntax ([check check-splicing-list-id])
                                                     (cons #'[(temp (... ...)) (check x (quote-syntax ctx))]
                                                           bindings)))))])
                               (loop (syntax rest) depth
                                     (lambda ()
                                       (rest-done-k (syntax rest) null))
                                     rest-done-k)))
                         (let ([mk-rest-done-k
                                (lambda (x-v x-bindings)
                                  (lambda (rest-v rest-bindings)
                                    (convert-k (datum->syntax
                                                stx
                                                (cons x-v rest-v)
                                                stx
                                                stx)
                                               (append x-bindings
                                                       rest-bindings))))])
                           (loop (syntax x) (sub1 depth)
                                 (lambda ()
                                   ;; x is unchanged.
                                   (loop (syntax rest) depth
                                         same-k
                                         (mk-rest-done-k (stx-car stx) null)))
                                 (lambda (x-v x-bindings)
                                   ;; x is generated by x-v
                                   (let ([rest-done-k (mk-rest-done-k 
                                                       (datum->syntax
                                                        (stx-car stx)
                                                        (list (stx-car (stx-car stx)) x-v)
                                                        (stx-car stx)
                                                        (stx-car stx))
                                                       x-bindings)])
                                     (loop (syntax rest) depth
                                           (lambda ()
                                             ;; rest is unchanged
                                             (rest-done-k (syntax rest) null))
                                           rest-done-k))))))]
                    [us-s
                     (and (identifier? #'us-s)
                          (free-identifier=? #'us-s unsyntax-splicing-id))
                     (raise-syntax-error
                      #f
                      "misuse within quasisyntax"
                      orig-stx
                      stx)]
                    [(qs x)
                     (and (identifier? #'qs)
                          (free-identifier=? #'qs quasisyntax-id))
                     (loop (syntax x) (add1 depth)
                           same-k
                           (lambda (v bindings)
                             (convert-k (datum->syntax
                                         stx
                                         (list (stx-car stx) v)
                                         stx
                                         stx)
                                        bindings)))]
                    [_
                     (cond
                      ;; We treat pairs specially so that we don't generate a lot
                      ;;  of syntax objects when the input syntax collapses a list
                      ;;  into a single syntax object.
                      [(pair? (syntax-e stx))
                       (let ploop ([l (syntax-e stx)]
                                   [same-k same-k]
                                   [convert-k (lambda (l bindings)
                                                (convert-k (datum->syntax
                                                            stx
                                                            l
                                                            stx
                                                            stx)
                                                           bindings))])
                         (cond
                          [(pair? l)
                           (if (let ([a (car l)])
                                 (or (and (identifier? a)
                                          (or (free-identifier=? a unsyntax-id)
                                              (free-identifier=? a quasisyntax-id)))
                                     (and (stx-pair? a)
                                          (let ([a (stx-car a)])
                                            (and (identifier? a)
                                                 (free-identifier=? a unsyntax-splicing-id))))))
                               ;; Found something important, like `unsyntax'; stop the special
                               ;; handling for pairs
                               (loop (datum->syntax #f l #f) depth
                                     same-k 
                                     convert-k)
                               ;; Normal special pair handling
                               (ploop (cdr l)
                                      (lambda ()
                                        ;; rest is the same
                                        (loop (car l) depth
                                              same-k
                                              (lambda (a a-bindings)
                                                (convert-k (cons (datum->syntax
                                                                  (car l)
                                                                  a
                                                                  (car l)
                                                                  (car l))
                                                                 (cdr l))
                                                           a-bindings))))
                                      (lambda (rest rest-bindings)
                                        (loop (car l) depth
                                              (lambda ()
                                                (convert-k (cons (car l) rest)
                                                           rest-bindings))
                                              (lambda (a a-bindings)
                                                (convert-k (cons (datum->syntax
                                                                  (car l)
                                                                  a
                                                                  (car l)
                                                                  (car l))
                                                                 rest)
                                                           (append a-bindings
                                                                   rest-bindings)))))))]
                          [(null? l) (same-k)]
                          [else (loop l depth same-k convert-k)]))]
                      [(vector? (syntax-e stx))
                       (loop (datum->syntax
                              stx
                              (vector->list (syntax-e stx))
                              stx)
                             depth
                             same-k
                             (lambda (v bindings)
                               (convert-k (datum->syntax
                                           stx
                                           (list->vector (syntax->list v))
                                           stx
                                           stx)
                                          bindings)))]
                      [(prefab-struct-key (syntax-e stx))
                       (let* ([d (syntax-e stx)]
                              [key (prefab-struct-key d)]
                              [fields (cdr (vector->list (struct->vector d)))])
                         (loop (datum->syntax stx fields stx)
                               depth
                               same-k
                               (lambda (v bindings)
                                 (let ([p (apply make-prefab-struct key (syntax->list v))])
                                   (convert-k (datum->syntax stx p stx stx) bindings)))))]
                      [else
                       (same-k)])]))))]
           [qq (lambda (orig-stx body mk-final)
                 (gen-qq orig-stx body mk-final
                         'quasisyntax
                         (quote-syntax unsyntax)
                         (quote-syntax unsyntax-splicing)
                         (quote-syntax quasisyntax)
                         (quote-syntax with-syntax)
                         (quote-syntax check-splicing-list)))])
      (values (lambda (orig-stx)
		(syntax-case orig-stx ()
		  [(_ stx) (qq orig-stx
			       (syntax stx) 
			       (lambda (body)
				 (list (quote-syntax syntax) body)))]))
	      (lambda (orig-stx)
		(syntax-case orig-stx ()
		  [(_ loc stx) (qq orig-stx
				   (syntax stx) 
				   (lambda (body)
				     (list (quote-syntax syntax/loc) 
					   (syntax loc)
					   body)))]))
               (lambda (orig-stx)
		(syntax-case orig-stx ()
		  [(_ stx) (gen-qq orig-stx
                                   (syntax stx) 
                                   (lambda (body)
                                     (list (quote-syntax datum) body))
                                   'quasidatum
                                   (quote-syntax undatum)
                                   (quote-syntax undatum-splicing)
                                   (quote-syntax quasidatum-splicing)
                                   (quote-syntax with-datum)
                                   (quote-syntax check-splicing-datum-list))]))))))
