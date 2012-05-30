#lang scheme/base

(require (for-syntax scheme/base
                     "private-yacc/parser-builder.rkt"
                     "private-yacc/grammar.rkt"
                     "private-yacc/yacc-helper.rkt"
                     "private-yacc/parser-actions.rkt"))
(require "private-lex/token.rkt"
         "private-yacc/parser-actions.rkt"
         mzlib/etc
         mzlib/pretty
         syntax/readerr)

(provide parser)


  ;; convert-parse-table : (vectorof (listof (cons/c gram-sym? action?))) ->
  ;;                       (vectorof (symbol runtime-action hashtable))
  (define-for-syntax (convert-parse-table table)
    (list->vector
     (map
      (lambda (state-entry)
        (let ((ht (make-hasheq)))
          (for-each
           (lambda (gs/action)
             (hash-set! ht
                        (gram-sym-symbol (car gs/action))
                        (action->runtime-action (cdr gs/action))))
           state-entry)
          ht))
      (vector->list table))))
  
  (define-syntax (parser stx)
    (syntax-case stx ()
      ((_ args ...)
       (let ((arg-list (syntax->list (syntax (args ...))))
             (src-pos #f)
             (debug #f)
             (error #f)
             (tokens #f)
             (start #f)
             (end #f)
             (precs #f)
             (suppress #f)
             (grammar #f)
             (yacc-output #f))
         (for-each
          (lambda (arg)
            (syntax-case* arg (debug error tokens start end precs grammar
                               suppress src-pos yacc-output)
              (lambda (a b)
                (eq? (syntax-e a) (syntax-e b)))
              ((debug filename)
               (cond
                 ((not (string? (syntax-e (syntax filename))))
                  (raise-syntax-error 
                   #f
                   "Debugging filename must be a string"
                   stx
                   (syntax filename)))
                 (debug
                  (raise-syntax-error #f "Multiple debug declarations" stx))
                 (else
                  (set! debug (syntax-e (syntax filename))))))
              ((suppress)
               (set! suppress #t))
              ((src-pos)
               (set! src-pos #t))
              ((error expression)
               (if error
                   (raise-syntax-error #f "Multiple error declarations" stx)
                   (set! error (syntax expression))))
              ((tokens def ...)
               (begin
                 (when tokens
                   (raise-syntax-error  #f "Multiple tokens declarations" stx))
                 (let ((defs (syntax->list (syntax (def ...)))))
                   (for-each 
                    (lambda (d)
                      (unless (identifier? d)
                        (raise-syntax-error 
                         #f 
                         "Token-group name must be an identifier"
                         stx
                         d)))
                    defs)
                   (set! tokens defs))))
              ((start symbol ...)
               (let ((symbols (syntax->list (syntax (symbol ...)))))
                 (for-each
                  (lambda (sym)
                    (unless (identifier? sym)
                      (raise-syntax-error #f
                                          "Start symbol must be a symbol"
                                          stx
                                          sym)))
                  symbols)
                 (when start
                   (raise-syntax-error #f "Multiple start declarations" stx))
                 (when (null? symbols)
                   (raise-syntax-error #f
                                       "Missing start symbol"
                                       stx
                                       arg))
                 (set! start symbols)))
              ((end symbols ...)
               (let ((symbols (syntax->list (syntax (symbols ...)))))
                 (for-each
                  (lambda (sym)
                    (unless (identifier? sym)
                      (raise-syntax-error #f
                                          "End token must be a symbol"
                                          stx
                                          sym)))
                  symbols)
                 (let ((d (duplicate-list? (map syntax-e symbols))))
                   (when d
                     (raise-syntax-error
                      #f
                      (format "Duplicate end token definition for ~a" d)
                      stx
                      arg))
                   (when (null? symbols)
                     (raise-syntax-error
                      #f
                      "end declaration must contain at least 1 token"
                      stx
                      arg))
                   (when end
                     (raise-syntax-error #f "Multiple end declarations" stx))
                   (set! end symbols))))
              ((precs decls ...)
               (if precs
                   (raise-syntax-error #f "Multiple precs declarations" stx)
                   (set! precs (syntax/loc arg (decls ...)))))
              ((grammar prods ...)
               (if grammar
                   (raise-syntax-error #f "Multiple grammar declarations" stx)
                   (set! grammar (syntax/loc arg (prods ...)))))
              ((yacc-output filename)
               (cond
                 ((not (string? (syntax-e (syntax filename))))
                  (raise-syntax-error #f
                                      "Yacc-output filename must be a string"
                                      stx
                                      (syntax filename)))
                 (yacc-output
                  (raise-syntax-error #f "Multiple yacc-output declarations" stx))
                 (else
                  (set! yacc-output (syntax-e (syntax filename))))))
              (_ (raise-syntax-error #f "argument must match (debug filename), (error expression), (tokens def ...), (start non-term), (end tokens ...), (precs decls ...), or  (grammar prods ...)" stx arg))))
          (syntax->list (syntax (args ...))))
         (unless tokens
           (raise-syntax-error #f "missing tokens declaration" stx))
         (unless error
           (raise-syntax-error #f "missing error declaration" stx))
         (unless grammar
           (raise-syntax-error #f "missing grammar declaration" stx))
         (unless end
           (raise-syntax-error #f "missing end declaration" stx))
         (unless start
           (raise-syntax-error #f "missing start declaration" stx))
         (let-values (((table all-term-syms actions check-syntax-fix)
                       (build-parser (if debug debug "")
                                     src-pos
                                     suppress
                                     tokens
                                     start
                                     end
                                     precs
                                     grammar)))
           (when (and yacc-output (not (string=? yacc-output "")))
             (with-handlers [(exn:fail:filesystem?
                              (lambda (e)
                                (eprintf
                                 "Cannot write yacc-output to file \"~a\"\n"
                                 yacc-output)))]
               (call-with-output-file yacc-output
                 (lambda (port)
                   (display-yacc (syntax->datum grammar) 
                                 tokens 
                                 (map syntax->datum start)
                                 (if precs
                                     (syntax->datum precs)
                                     #f)
                                 port))
                 #:exists 'truncate)))
           (with-syntax ((check-syntax-fix check-syntax-fix)
                         (err error)
                         (ends end)
                         (starts start)
                         (debug debug)
                         (table (convert-parse-table table))
                         (all-term-syms all-term-syms)
                         (actions actions)
                         (src-pos src-pos))
             (syntax
              (begin
                check-syntax-fix
                (parser-body debug err (quote starts) (quote ends) table all-term-syms actions src-pos)))))))
      (_
       (raise-syntax-error #f
                           "parser must have the form (parser args ...)"
                           stx))))
  
  (define (reduce-stack stack num ret-vals src-pos)
    (cond
      ((> num 0)
       (let* ((top-frame (car stack))
              (ret-vals
               (if src-pos
                   (cons (stack-frame-value top-frame)
                         (cons (stack-frame-start-pos top-frame)
                               (cons (stack-frame-end-pos top-frame)
                                     ret-vals)))
                   (cons (stack-frame-value top-frame) ret-vals))))
         (reduce-stack (cdr stack) (sub1 num) ret-vals src-pos)))
      (else (values stack ret-vals))))
  
  ;; extract-helper : (symbol or make-token) any any -> symbol any any any
  (define (extract-helper tok v1 v2)
    (cond
      ((symbol? tok)
       (values tok #f v1 v2))
      ((token? tok)
       (values (real-token-name tok) (real-token-value tok) v1 v2))
      (else (raise-argument-error 'parser 
                                  "(or/c symbol? token?)"
                                  0 
                                  tok))))
  
  ;; extract-src-pos : position-token -> symbol any any any
  (define (extract-src-pos ip)
    (cond
      ((position-token? ip)
       (extract-helper (position-token-token ip)
                       (position-token-start-pos ip)
                       (position-token-end-pos ip)))
      (else
       (raise-argument-error 'parser 
                             "position-token?"
                             0
                             ip))))
  
  ;; extract-no-src-pos : (symbol or make-token) -> symbol any any any
  (define (extract-no-src-pos ip)
    (extract-helper ip #f #f))
  
  (define-struct stack-frame (state value start-pos end-pos) #:inspector (make-inspector))
  
  (define (make-empty-stack i) (list (make-stack-frame i #f #f #f)))
  
  
  ;; The table is a vector that maps each state to a hash-table that maps a
  ;; terminal symbol to either an accept, shift, reduce, or goto structure.
  ;  We encode the structures according to the runtime-action data definition in
  ;; parser-actions.rkt
  (define (parser-body debug? err starts ends table all-term-syms actions src-pos)
    (local ((define extract
              (if src-pos
                  extract-src-pos
                  extract-no-src-pos))
            
            (define (fix-error stack tok val start-pos end-pos get-token)
              (when debug? (pretty-print stack))
              (local ((define (remove-input tok val start-pos end-pos)
                        (if (memq tok ends)
                            (raise-read-error "parser: Cannot continue after error"
                                              #f #f #f #f #f)
                            (let ((a (find-action stack tok val start-pos end-pos)))
                              (cond
                                ((runtime-shift? a)
                                 ;; (printf "shift:~a\n" (runtime-shift-state a))
                                 (cons (make-stack-frame (runtime-shift-state a)
                                                         val
                                                         start-pos
                                                         end-pos)
                                       stack))
                                (else
                                 ;; (printf "discard input:~a\n" tok)
                                 (let-values (((tok val start-pos end-pos)
                                               (extract (get-token))))
                                   (remove-input tok val start-pos end-pos))))))))
                (let remove-states ()
                  (let ((a (find-action stack 'error #f start-pos end-pos)))
                    (cond
                      ((runtime-shift? a)
                       ;; (printf "shift:~a\n" (runtime-shift-state a))
                       (set! stack 
                             (cons
                              (make-stack-frame (runtime-shift-state a) 
                                                #f 
                                                start-pos
                                                end-pos)
                              stack))
                       (remove-input tok val start-pos end-pos))
                      (else
                       ;; (printf "discard state:~a\n" (car stack))
                       (cond
                         ((< (length stack) 2)
                          (raise-read-error "parser: Cannot continue after error"
                                            #f #f #f #f #f))
                         (else
                          (set! stack (cdr stack))
                          (remove-states)))))))))
            
            (define (find-action stack tok val start-pos end-pos)
              (unless (hash-ref all-term-syms
                                tok
                                #f)
                (if src-pos
                    (err #f tok val start-pos end-pos)
                    (err #f tok val))
                (raise-read-error (format "parser: got token of unknown type ~a" tok)
                                  #f #f #f #f #f))
              (hash-ref (vector-ref table (stack-frame-state (car stack)))
                        tok
                        #f))

            (define (make-parser start-number)
              (lambda (get-token)
                (unless (and (procedure? get-token)
                             (procedure-arity-includes? get-token 0))
                  (error 'get-token "expected a nullary procedure, got ~e" get-token))
                (let parsing-loop ((stack (make-empty-stack start-number))
                                   (ip (get-token)))
                  (let-values (((tok val start-pos end-pos)
                                (extract ip)))
                    (let ((action (find-action stack tok val start-pos end-pos)))
                      (cond
                        ((runtime-shift? action)
                         ;; (printf "shift:~a\n" (runtime-shift-state action))
                         (parsing-loop (cons (make-stack-frame (runtime-shift-state action)
                                                               val
                                                               start-pos
                                                               end-pos)
                                             stack)
                                       (get-token)))
                        ((runtime-reduce? action)
                         ;; (printf "reduce:~a\n" (runtime-reduce-prod-num action))
                         (let-values (((new-stack args)
                                       (reduce-stack stack 
                                                     (runtime-reduce-rhs-length action)
                                                     null
                                                     src-pos)))
                           (let ((goto 
                                  (runtime-goto-state
                                   (hash-ref 
                                    (vector-ref table (stack-frame-state (car new-stack)))
                                    (runtime-reduce-lhs action)))))
                             (parsing-loop 
                              (cons
                               (if src-pos
                                   (make-stack-frame
                                    goto 
                                    (apply (vector-ref actions (runtime-reduce-prod-num action)) args)
                                    (if (null? args) start-pos (cadr args))
                                    (if (null? args) 
                                        end-pos
                                        (list-ref args (- (* (runtime-reduce-rhs-length action) 3) 1))))
                                   (make-stack-frame
                                    goto 
                                    (apply (vector-ref actions (runtime-reduce-prod-num action)) args)
                                    #f
                                    #f))
                               new-stack)
                              ip))))
                        ((runtime-accept? action)
                         ;; (printf "accept\n")
                         (stack-frame-value (car stack)))
                        (else 
                         (if src-pos
                             (err #t tok val start-pos end-pos)
                             (err #t tok val))
                         (parsing-loop (fix-error stack tok val start-pos end-pos get-token)
                                       (get-token))))))))))
      (cond
        ((null? (cdr starts)) (make-parser 0))
        (else
         (let loop ((l starts)
                    (i 0))
           (cond
             ((null? l) null)
             (else (cons (make-parser i) (loop (cdr l) (add1 i))))))))))
