(module contract mzscheme

  ;; Provides helpers for contract.ss

  (require (lib "contract.ss"))
  (require-for-template (lib "contract.ss"))

  ;; Macro definitions
  
  (define-syntax (define/p stx)
    (syntax-case stx ()
      [(_ (NAME . ARGS) BODY ...)
       #`(begin
           (define (NAME . ARGS) BODY ...)
           (provide NAME))]
      [(_ NAME BODY ...)
       #`(begin
           (define NAME BODY ...)
           (provide NAME))]
      ))

  (define-syntax (define/c stx)
    (syntax-case stx ()
      [(_ (NAME . ARGS) CONTRACT BODY ...)
       #`(begin
           (define (NAME . ARGS) BODY ...)
           (provide/contract [NAME CONTRACT]))]
      [(_ NAME CONTRACT BODY ...)
       #`(begin
           (define NAME BODY ...)
           (provide/contract [NAME CONTRACT]))]
      ))

  (define-syntax (define-structs/provide/contract stx)

    (define inspector-var
      (with-syntax ([(VAR) (generate-temporaries #'(inspector))]) #'VAR))

    (define (build-define-struct spec-stx)
      (syntax-case spec-stx ()
        [(NAME ([FIELD CONTRACT] ...) SPEC ...)
         #`(begin
             (define-struct NAME (FIELD ...) #,inspector-var)
             #,@(map build-define-struct (syntax->list #'(SPEC ...))))]))

    (define (build-provide/contract parent inherit)
      (lambda (spec-stx)
        (syntax-case spec-stx ()
          [(NAME ([FIELD CONTRACT] ...) SPEC ...)
           (with-syntax ([(CONTRACT-VAR ...) (generate-temporaries #'(CONTRACT ...))]
                         [(INHERIT ...) inherit])
             (with-syntax ([(CURRENT ...) #'(INHERIT ... [FIELD CONTRACT-VAR] ...)])
               #`(begin
                   (provide/contract
                    (struct #,(if parent #`(NAME #,parent) #'NAME) (CURRENT ...)))
                   #,@(map (build-provide/contract #'NAME #'(CURRENT ...))
                           (syntax->list #'(SPEC ...))))))])))
    
    (syntax-case stx ()
      [(_ INSPECTOR SPEC ...)
       (let* ([specs (syntax->list #'(SPEC ...))])
         #`(begin
             (define #,inspector-var INSPECTOR)
             #,@(map build-define-struct specs)
             #,@(map (build-provide/contract #f #'()) specs)))]))
  
  ;; Value definitions

  ;; unknown/c : FlatContract
  ;; Rejects everything; use to guarantee a visible error.
  (define unknown/c
    (not/c any/c))

  ;; maybe : [Union Predicate FlatContract] -> FlatContract
  ;; Accepts either a Value or #f.
  (define (maybe value/c)
    (union value/c false/c))

  ;; predicate : Contract
  ;; Accepts value predicates.
  (define predicate/c
    (any/c . -> . boolean?))

  ;; flat-contract-or-predicate/c : Contract
  ;; Accepts flat-contract arguments (flat contracts or predicates).
  (define flat-contract-or-predicate/c
    (union predicate/c flat-contract?))
  
  ;; Macro exports
  
  (provide
   define/p
   define/c
   define-structs/provide/contract
   )

  ;; Value exports

  (provide/contract
   [predicate/c contract?]
   [maybe (flat-contract-or-predicate/c . -> . flat-contract?)]
   [flat-contract-or-predicate/c flat-contract?]
   )

  )