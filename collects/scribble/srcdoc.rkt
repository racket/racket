#lang scheme/base
(require racket/contract/base
         (for-syntax scheme/base)
         "provide-doc-transform.rkt")

(provide require/doc
         provide/doc
         thing-doc
         parameter-doc
         proc-doc
         proc-doc/names)

(define-syntax-rule (require/doc spec ...)
  (void (quote-syntax (require/doc spec ...))))

(define-syntax (provide/doc stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ([forms (syntax->list #'(form ...))])
       (with-syntax ([((for-provide/contract for-docs id) ...)
                      (map (lambda (form)
                             (syntax-case form ()
                               [(id . _)
                                (identifier? #'id)
                                (let ([t (syntax-local-value #'id (lambda () #f))])
                                  (unless (provide/doc-transformer? t)
                                    (raise-syntax-error
                                     #f
                                     "not bound as a provide/doc transformer"
                                     stx
                                     #'id))
                                  (let* ([i (make-syntax-introducer)]
                                         [i2 (lambda (x) (syntax-local-introduce (i x)))])
                                    (let-values ([(p/c d req/d id)
                                                  ((provide/doc-transformer-proc t)
                                                   (i (syntax-local-introduce form)))])
                                      (list (i2 p/c) (list (i2 req/d) (i2 d) (i2 (quote-syntax tag))) (i2 id)))))]
                               [_
                                (raise-syntax-error
                                 #f
                                 "not a provide/doc sub-form"
                                 stx
                                 form)]))
                           forms)])
         (with-syntax ([(p/c ...)
                        (map (lambda (form f)
                               (quasisyntax/loc form
                                 (provide/contract #,f)))
                             forms
                             (syntax->list #'(for-provide/contract ...)))])
           #'(begin
               p/c ...
               (void (quote-syntax (provide/doc (for-docs id) ...)))))))]))

(define-for-syntax (remove->i-deps stx)
  (syntax-case stx ()
    [(id (id2 ...) ctc)
     #'(id ctc)]
    [(id ctc)
     #'(id ctc)]
    [else
     (error 'remove->i-deps "unknown thing ~s" stx)]))

(define-provide/doc-transformer proc-doc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract desc)
       (with-syntax ([(header result (body-stuff ...))
                      (syntax-case #'contract (->d ->i -> values)
                        [(->d (req ...) () (values [name res] ...))
                         #'((id req ...) (values res ...) ())]
                        [(->d (req ...) () #:pre-cond condition (values [name res] ...))
                         #'((id req ...) (values res ...) ((bold "Pre-condition: ") (racket condition) "\n" "\n"))]
                        [(->d (req ...) () [name res])
                         #'((id req ...) res ())]
                        [(->d (req ...) () #:pre-cond condition [name res])
                         #'((id req ...) res ((bold "Pre-condition: ")  (racket condition) "\n" "\n" ))]
                        [(->d (req ...) () #:rest rest rest-ctc [name res])
                         #'((id req ... [rest rest-ctc] (... ...)) res ())]
                        [(->d (req ...) (one more ...) whatever)
                         (raise-syntax-error
                          #f
                          (format "unsupported ->d contract form for ~a, optional arguments non-empty, must use proc-doc/names"
                                  (syntax->datum #'id))
                          stx
                          #'contract)]
                        [(->d whatever ...) 
                         (raise-syntax-error
                          #f
                          (format "unsupported ->d contract form for ~a" (syntax->datum #'id))
                          stx
                          #'contract)]
                        
                        [(->i (req ...) () (values ress ...))
                         (with-syntax ([(req ...) (map remove->i-deps (syntax->list #'(req ...)))]
                                       [([name res] ...) (map remove->i-deps (syntax->list #'(req ...)))])
                           #'((id req ...) (values res ...) ()))]
                        [(->i (req ...) () #:pre (pre-id ...) condition (values ress ...))
                         (with-syntax ([(req ...) (map remove->i-deps (syntax->list #'(req ...)))]
                                       [([name res] ...) (map remove->i-deps (syntax->list #'(req ...)))])
                           #'((id req ...) (values res ...) ((bold "Pre-condition: ") (racket condition) "\n" "\n")))]
                        [(->i (req ...) () res)
                         (with-syntax ([(req ...) (map remove->i-deps (syntax->list #'(req ...)))]
                                       [[name res] (remove->i-deps #'res)])
                           #'((id req ...) res ()))]
                        [(->i (req ...) () #:pre (pre-id ...) condition [name res])
                         (with-syntax ([(req ...) (map remove->i-deps (syntax->list #'(req ...)))]
                                       [[name res] (remove->i-deps #'res)])
                           #'((id req ...) res ((bold "Pre-condition: ")  (racket condition) "\n" "\n" )))]
                        [(->i (req ...) () #:rest rest res)
                         (with-syntax ([(req ...) (map remove->i-deps (syntax->list #'(req ...)))]
                                       [[name res] (remove->i-deps #'res)]
                                       [[name-t rest-ctc] (remove->i-deps #'rest)])
                           #'((id req ... [name-t rest-ctc] (... ...)) res ()))]
                        [(->i (req ...) (one more ...) whatever)
                         (raise-syntax-error
                          #f
                          (format "unsupported ->i contract form for ~a, optional arguments non-empty, must use proc-doc/names"
                                  (syntax->datum #'id))
                          stx
                          #'contract)]
                        [(->i whatever ...) 
                         (raise-syntax-error
                          #f
                          (format "unsupported ->i contract form for ~a" (syntax->datum #'id))
                          stx
                          #'contract)]
                        
                        [(-> result)
                         #'((id) result ())]
                        [(-> whatever ...) 
                         (raise-syntax-error
                          #f
                          (format "unsupported -> contract form for ~a, must use proc-doc/names if there are arguments"
                                  (syntax->datum #'id))
                          stx
                          #'contract)]
                        [(id whatever ...)
                         (raise-syntax-error
                          #f
                          (format "unsupported ~a contract form (unable to synthesize argument names)" (syntax->datum #'id))
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc header result body-stuff ... . desc)
          #'(scribble/manual)
          #'id))])))

(define-provide/doc-transformer proc-doc/names
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract names desc)
       (with-syntax ([header                      
                      (syntax-case #'(contract names) (->d -> ->* values case->)
                        [((-> ctcs ... result) (arg-names ...))
                         (begin
                           (unless (= (length (syntax->list #'(ctcs ...)))
                                      (length (syntax->list #'(arg-names ...))))
                             (raise-syntax-error #f "mismatched argument list and domain contract count" stx))
                           #'([(id (arg-names ctcs) ...) result]))]
                        
                        [((->* (mandatory ...) (optional ...) result) 
                          names)
                         (syntax-case #'names ()
                           [((mandatory-names ...)
                             ((optional-names optional-default) ...))
                            
                            (let ([build-mandatories/optionals
                                   (λ (names contracts extras)
                                     (let ([names-length (length names)]
                                           [contracts-length (length contracts)])
                                       (let loop ([contracts contracts]
                                                  [names names]
                                                  [extras extras])
                                         (cond
                                           [(and (null? names) (null? contracts)) '()]
                                           [(or (null? names) (null? contracts))
                                            (raise-syntax-error #f
                                                                (format "mismatched ~a argument list count and domain contract count (~a)"
                                                                        (if extras "optional" "mandatory")
                                                                        (if (null? names)
                                                                            "ran out of names"
                                                                            "ran out of contracts"))
                                                                stx)]
                                           [else
                                            (let ([fst-name (car names)]
                                                  [fst-ctc (car contracts)])
                                              (if (keyword? (syntax-e fst-ctc))
                                                  (begin
                                                    (unless (pair? (cdr contracts))
                                                      (raise-syntax-error #f
                                                                          "keyword not followed by a contract"
                                                                          stx))
                                                    (cons (if extras
                                                              (list fst-ctc fst-name (cadr contracts) (car extras))
                                                              (list fst-ctc fst-name (cadr contracts)))
                                                          (loop (cddr contracts)
                                                                (cdr names)
                                                                (if extras
                                                                    (cdr extras)
                                                                    extras))))
                                                  (cons (if extras 
                                                            (list fst-name fst-ctc (car extras))
                                                            (list fst-name fst-ctc))
                                                        (loop (cdr contracts) (cdr names) (if extras
                                                                                              (cdr extras)
                                                                                              extras)))))]))))])
                            
                              #`([(id #,@(build-mandatories/optionals (syntax->list #'(mandatory-names ...))
                                                                      (syntax->list #'(mandatory ...))
                                                                      #f)
                                      #,@(build-mandatories/optionals (syntax->list #'(optional-names ...))
                                                                      (syntax->list #'(optional ...))
                                                                      (syntax->list #'(optional-default ...))))
                                result]))]
                           [(mandatory-names optional-names)
                            (begin
                              (syntax-case #'mandatory-names ()
                                [(mandatory-names ...)
                                 (andmap identifier? (syntax->list #'(mandatory-names ...)))]
                                [x
                                 (raise-syntax-error #f "mandatory names should be a sequence of identifiers" 
                                                     stx 
                                                     #'mandatory-names)])
                              (syntax-case #'optional-names ()
                                [((x y) ...)
                                 (andmap identifier? (syntax->list #'(x ... y ...)))]
                                [((x y) ...)
                                 (for-each
                                  (λ (var) 
                                    (unless (identifier? var)
                                      (raise-syntax-error #f "expected an identifier in the optional names" stx var)))
                                  (syntax->list #'(x ... y ...)))]
                                [(a ...)
                                 (for-each
                                  (λ (a)
                                    (syntax-case stx ()
                                      [(x y) (void)]
                                      [other
                                       (raise-syntax-error #f "expected an sequence of two idenfiers" stx #'other)]))
                                  (syntax->list #'(a ...)))]))]
                           [x
                            (raise-syntax-error
                             #f
                             "expected two sequences, one of mandatory names and one of optionals"
                             stx
                             #'x)])]
                        [((case-> (-> doms ... rng) ...)
                          ((args ...) ...))
                         (begin
                           (unless (= (length (syntax->list #'((doms ...) ...)))
                                      (length (syntax->list #'((args ...) ...))))
                             (raise-syntax-error #f
                                                 "number of cases and number of arg lists do not have the same size"
                                                 stx))
                           (for-each
                            (λ (doms args)
                              (unless (= (length (syntax->list doms))
                                         (length (syntax->list args)))
                                (raise-syntax-error #f "mismatched case argument list and domain contract" stx)))
                            (syntax->list #'((doms ...) ...))
                            (syntax->list #'((args ...) ...)))
                           #'([(id (args doms) ...) rng] ...))]
                        [else
                         (raise-syntax-error
                          #f
                          "unsupported procedure contract form (no argument names)"
                          stx
                          #'contract)])])
         (values
          #'[id contract]
          #'(defproc* header . desc)
          #'((only-in scribble/manual defproc*))
          #'id))])))

(define-provide/doc-transformer parameter-doc
  (lambda (stx)
    (syntax-case stx (parameter/c)
      [(_ id (parameter/c contract) arg-id desc)
       (begin
         (unless (identifier? #'arg-id)
           (raise-syntax-error 'parameter/doc 
                               "expected an identifier"
                               stx
                               #'arg-id))
         (unless (identifier? #'id)
           (raise-syntax-error 'parameter/doc 
                               "expected an identifier"
                               stx
                               #'id))
         (values
          #'[id (parameter/c contract)]
          #'(defparam id arg-id contract . desc)
          #'((only-in scribble/manual defparam))
          #'id))])))

(define-provide/doc-transformer thing-doc
  (lambda (stx)
    (syntax-case stx ()
      [(_ id contract desc)
       (begin
         (unless (identifier? #'id)
           (raise-syntax-error 'parameter/doc 
                               "expected an identifier"
                               stx
                               #'id))
         (values
          #'[id contract]
          #'(defthing id contract . desc)
          #'((only-in scribble/manual defthing))
          #'id))])))
