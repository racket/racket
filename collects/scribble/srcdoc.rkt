#lang racket/base
(require racket/contract/base
         (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     syntax/private/modcollapse-noctc))

(provide for-doc require/doc
         provide/doc ; not needed anymore
         thing-doc
         parameter-doc
         proc-doc
         proc-doc/names
         generate-delayed-documents)

(begin-for-syntax
 (define requires null)
 (define doc-body null)
 (define generated? #f)
 (define delayed? #f)

 (define (add-requires!/decl specs)
   (unless delayed?
     (syntax-local-lift-module-end-declaration
      #`(begin-for-syntax (add-relative-requires! (#%variable-reference)
                                                  (quote-syntax #,specs)))))
   (add-requires! (syntax-local-introduce specs)))

 (define (add-relative-requires! varref specs)
   (define mpi (variable-reference->module-path-index varref))
   (define-values (name base) (module-path-index-split mpi))
   (if name
       (add-requires!
        (with-syntax ([(spec ...) specs]
                      [rel-to (collapse-module-path-index 
                               mpi
                               (build-path (or (current-load-relative-directory)
                                               (current-directory))
                                           "here.rkt"))])
          #'((relative-in rel-to spec) ...)))
       (add-requires! specs)))

 (define (add-requires! specs)
   (set! requires (cons specs requires)))
 
 (define (generate-doc-submodule!)
   (unless generated?
     (set! generated? #t)
     (syntax-local-lift-module-end-declaration #'(doc-submodule)))))

(define-syntax for-doc
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec ...)
        (add-requires!/decl #'(spec ...))])
     (values null null))))

(define-syntax (doc-submodule stx)
  (with-syntax ([((req ...) ...)
                 (map syntax-local-introduce (reverse requires))]
                [doc-body
                 (map (lambda (s) (syntax-local-introduce
                                   (syntax-shift-phase-level s #f)))
                      (reverse doc-body))])
    ;; This module will be required `for-template':
    (if delayed?
        ;; delayed mode: return syntax objects to drop into context:
        #'(begin-for-syntax
           (module* srcdoc #f
             (require (for-syntax racket/base syntax/quote))
             (begin-for-syntax
              (provide get-docs)
              (define (get-docs)
                (list (quote-syntax (req ... ...))
                      (quote-syntax/keep-srcloc doc-body))))))
        ;; normal mode: return an identifier that holds the document:
        (with-syntax ([((id d) ...) #'doc-body])
          #'(begin-for-syntax
             (module* srcdoc #f
               (require req ... ...)
               (define docs (list (cons 'id d) ...))
               (require (for-syntax racket/base))
               (begin-for-syntax
                (provide get-docs)
                (define (get-docs)
                  #'docs))))))))

(define-syntax (require/doc stx)
  (syntax-case stx ()
    [(_ spec ...)
     (add-requires!/decl #'(spec ...))
     #'(begin)]))

(define-for-syntax (do-provide/doc stx modes)
  (let ([forms (list stx)])
    (with-syntax ([((for-provide/contract (req ...) d id) ...)
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
                                   (list (i2 p/c) (i req/d) (i d) (i id)))))]
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
                              (contract-out #,f)))
                          forms
                          (syntax->list #'(for-provide/contract ...)))])
        (generate-doc-submodule!)
        (set! doc-body (append (reverse (syntax->list #'((id d) ...)))
                               doc-body))
        (set! requires (cons #'(req ... ...) requires))
        (pre-expand-export #'(combine-out p/c ...) modes)))))

(define-syntax-rule (provide/doc form ...)
  (provide form ...))

(define-for-syntax (remove->i-deps stx)
  (syntax-case stx ()
    [(id (id2 ...) ctc)
     #'(id ctc)]
    [(id ctc)
     #'(id ctc)]
    [else
     (error 'remove->i-deps "unknown thing ~s" stx)]))

(provide define-provide/doc-transformer
         (for-syntax
          provide/doc-transformer?
          provide/doc-transformer-proc))

(begin-for-syntax
 (define-struct provide/doc-transformer (proc)
   #:property 
   prop:provide-pre-transformer
   (lambda (self)
     (lambda (stx mode)
       (do-provide/doc stx mode)))))

(define-syntax-rule (define-provide/doc-transformer id rhs)
  (define-syntax id
    (make-provide/doc-transformer rhs)))


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
          #'(scribble/manual
             racket/base) ; for `...'
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

(define-syntax (generate-delayed-documents stx)
  (syntax-case stx () 
    [(_) 
     (begin
       (set! delayed? #t)
       #'(begin))]))
