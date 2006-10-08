
(module deriv-tokens mzscheme
  (require (lib "lex.ss" "parser-tools")
           "deriv.ss")
  (provide (all-defined))
  
  (define-tokens basic-tokens
    (visit                ; syntax
     resolve              ; identifier
     next                 ; .
     next-group           ; .
     enter-macro          ; syntax
     macro-pre-transform  ; syntax
     macro-post-transform ; syntax
     exit-macro           ; syntax
     enter-prim           ; syntax
     exit-prim            ; syntax
     return               ; syntax
     enter-block          ; syntaxes
     block->list          ; syntaxes
     block->letrec        ; syntax(es?)
     splice               ; syntaxes
     enter-list           ; syntaxes
     exit-list            ; syntaxes
     enter-check          ; syntax
     exit-check           ; syntax
     phase-up             ; .
     module-body          ; (list-of (cons syntax boolean))
     ...                  ; .
     EOF                  ; .
     syntax-error         ; exn
     lift-loop            ; syntax
     lift/let-loop        ; syntax
     module-lift-loop     ; syntaxes
     module-lift-end-loop ; syntaxes
     lift                 ; (cons syntax id)
     lift-statement       ; syntax
     enter-local          ; syntax
     local-pre            ; syntax
     local-post           ; syntax
     exit-local           ; syntax
     
     IMPOSSIBLE           ; useful for error-handling clauses that have no NoError counterpart
     ))
  
  (define-tokens renames-tokens
    (renames-lambda           ; (cons syntax syntax)
     renames-case-lambda      ; (cons syntax syntax)
     renames-let              ; (cons (listof syntax) syntax)
     renames-letrec-syntaxes  ; (cons (listof syntax) (cons (listof syntax) syntax))
     renames-block            ; (cons syntax syntax) ... different, contains both pre+post
     ))
  (define-tokens prim-tokens
    (prim-module prim-#%module-begin
     prim-define-syntaxes prim-define-values
     prim-if prim-wcm prim-begin prim-begin0 prim-#%app prim-lambda
     prim-case-lambda prim-let-values prim-let*-values prim-letrec-values 
     prim-letrec-syntaxes+values prim-#%datum prim-#%top prim-stop
     prim-quote prim-quote-syntax prim-require prim-require-for-syntax
     prim-require-for-template prim-provide
     prim-set!
     
     variable            ; (cons identifier identifier)
     ))
  
  ;; ** Signals to tokens
  
  (define signal-mapping
    `((EOF . EOF)
      (error . ,token-syntax-error)
      (0 . ,token-visit)
      (1 . ,token-resolve)
      (2 . ,token-return)
      (3 . ,token-next)
      (4 . ,token-enter-list)
      (5 . ,token-exit-list)
      (6 . ,token-enter-prim)
      (7 . ,token-exit-prim)
      (8 . ,token-enter-macro)
      (9 . ,token-exit-macro)
      (10 . ,token-enter-block)
      (11 . ,token-splice)
      (12 . ,token-block->list)
      (13 . ,token-next-group)
      (14 . ,token-block->letrec)
      #;(15 . renamer)
      (16 . ,token-renames-let)
      (17 . ,token-renames-lambda)
      (18 . ,token-renames-case-lambda)
      (19 . ,token-renames-letrec-syntaxes)
      (20 . phase-up)
      (21 . ,token-macro-pre-transform)
      (22 . ,token-macro-post-transform)
      (23 . ,token-module-body)
      (24 . ,token-renames-block)
      
      (100 . prim-stop)
      (101 . prim-module)
      (102 . prim-#%module-begin)
      (103 . prim-define-syntaxes)
      (104 . prim-define-values)
      (105 . prim-if)
      (106 . prim-wcm)
      (107 . prim-begin)
      (108 . prim-begin0)
      (109 . prim-#%app)
      (110 . prim-lambda)
      (111 . prim-case-lambda)
      (112 . prim-let-values)
      (113 . prim-letrec-values)
      (114 . prim-letrec-syntaxes+values)
      (115 . prim-#%datum)
      (116 . prim-#%top)
      (117 . prim-quote)
      (118 . prim-quote-syntax)
      (119 . prim-require)
      (120 . prim-require-for-syntax)
      (121 . prim-require-for-template)
      (122 . prim-provide)
      (123 . prim-set!)
      (124 . prim-let*-values)
      (125 . ,token-variable)
      (126 . ,token-enter-check)
      (127 . ,token-exit-check)
      (128 . ,token-lift-loop)
      (129 . ,token-lift)
      (130 . ,token-enter-local)
      (131 . ,token-exit-local)
      (132 . ,token-local-pre)
      (133 . ,token-local-post)
      (134 . ,token-lift-statement)
      (135 . ,token-module-lift-end-loop)
      (136 . ,token-lift/let-loop)
      (137 . ,token-module-lift-loop)
      ))
  
  (define (tokenize sig-n val pos)
    (let ([p (assv sig-n signal-mapping)])
      (if (pair? p)
          (make-position-token
           (cond [(procedure? (cdr p)) ((cdr p) val)]
                 [(symbol? (cdr p)) (cdr p)])
           pos
           pos)
          (error 'tokenize "bad signal: ~s" sig-n))))
  
  )
