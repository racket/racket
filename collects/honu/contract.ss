(module contract mzscheme

  ;; Provides helpers for contract.ss

  (require (lib "contract.ss"))
  (require-for-template (lib "contract.ss"))

  ;; Macro definitions
  
  (define-syntax (define/provide stx)
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

  (define-syntax (define/provide/contract stx)
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

  (define-syntax (define-structs stx)

    (define (build-define-struct struct-stx var-stx)
      (syntax-case struct-stx ()
        [(NAME (FIELD ...) STRUCT ...)
         #`(begin
             (define-struct NAME (FIELD ...) #,var-stx)
             #,(build-define-struct #'STRUCT var-stx) ...)]))
    
    (syntax-case stx ()
      [(_ INSPECTOR-EXPR STRUCT ...)
       (with-syntax ([(INSPECTOR-VAR) (generate-temporaries #'(INSPECTOR-EXPR))])
         #`(begin
             (define INSPECTOR-VAR INSPECTOR-EXPR)
             #,(build-define-struct #'STRUCT #'INSPECTOR-VAR) ...))]))
  
  (define-syntax (define-structs/provide stx)
    (syntax-case stx ()
      [(_ (NAME SUPER) (FIELD ...) REST ...)
       #`(begin
           (define-struct (NAME SUPER) (FIELD ...) REST ...)
           (provide (struct NAME (FIELD ...))))]
      [(_ NAME (FIELD ...) REST ...)
       #`(begin
           (define-struct NAME (FIELD ...) REST ...)
           (provide (struct NAME (FIELD ...))))]))

  (define-syntax (define-structs/provide/contract stx)
    (syntax-case stx ()
      [(_ (NAME SUPER) ([FIELD CONTRACT] ...) REST ...)
       #`(begin
           (define-struct (NAME SUPER) (FIELD ...) REST ...)
           (provide/contract (struct NAME ([FIELD CONTRACT] ...))))]
      [(_ NAME ([FIELD CONTRACT] ...) REST ...)
       #`(begin
           (define-struct NAME (FIELD ...) REST ...)
           (provide/contract (struct NAME ([FIELD CONTRACT] ...))))]))

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
   define/p/c
   define-struct/p
   define-struct/p/c
   )

  ;; Value exports

  (provide/contract
   [predicate/c contract?]
   [maybe (flat-contract-or-predicate/c . -> . flat-contract?)]
   [flat-contract-or-predicate/c flat-contract?]
   )

  )