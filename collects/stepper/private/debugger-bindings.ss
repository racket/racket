; this module is a cheap hack; it interacts with the debugger 
; REPL by getting & setting values in the top-level environment

(module debugger-bindings mzscheme
  (require (lib "contract.ss")
           "marks.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (prefix kernel: (lib "kerncase.ss" "syntax")))
  
  (provide/contract [set-event-num! (-> number? void?)] 
                    [bt (-> void?)] 
                    [set-frame-num! (-> number? void?)]
                    [src (-> void?)]
                    [binding (-> symbol? any)])

  (provide install-debugger-bindings)

  (define (install-debugger-bindings)
    ; yuck!  dependence on the list of names provided by the module
    (namespace-set-variable-value! 'e set-event-num!)
    (namespace-set-variable-value! 'bt bt)
    (namespace-set-variable-value! 'f set-frame-num!)
    (namespace-set-variable-value! 'src src)
    (namespace-set-variable-value! 'v binding)
    (namespace-set-variable-value! 'c continue)
    (namespace-set-variable-value! 'bound bound)
    (namespace-set-variable-value! 'help help))
  
  (define (help)
    (printf "Help Summary:\n")
    (call-with-input-file (build-path (collection-path "stepper" "private") "debugger-summary.txt")
      (lambda (port)
	(let loop ([line (read-line port)])
	  (unless (eof-object? line)
          (printf "~a\n" line)
          (loop (read-line port)))))))

  (define (continue)
    (semaphore-post (namespace-variable-value 'go-semaphore)))
  
  (define (events)
    ((namespace-variable-value 'events)))
  
  (define (current-event-num)
    (namespace-variable-value 'current-event-num))

  (define (current-event)
    (list-ref (events) (current-event-num)))
  
  ; this retrieves the mark list from the most recent event with normal breakpoint info
  ; unless an event with breakpoint info has been specified, in which case it returns that
  (define (current-mark-list)
    (if (normal-breakpoint-info? (current-event))
        (normal-breakpoint-info-mark-list (current-event))
        (let loop ((l  (reverse (events))))
          (cond
            ((null? l) (error 'current-mark-list "no events with mark lists: ~v" (events)))
            ((normal-breakpoint-info? (car l)) (normal-breakpoint-info-mark-list (car l)))
            (else (loop (cdr l)))))))

  (define (current-frame-num)
    (namespace-variable-value 'current-frame-num))
  
  (define (current-frame)
    (list-ref (current-mark-list) (current-frame-num)))
  
  (define (check-range num bottom top)
    (when (or (< num bottom) (> num top))
      (error 'check-range "argument ~v out of range [~v ... ~v]" num bottom top)))
  
  ; pretty-print code (represented as sexp)
  ; stolen from MrFlow
  (define (simplify t)
    (kernel:kernel-syntax-case t #f
      [(#%app . rest) (map simplify (syntax->list #`rest))]
      [(#%datum . d) #`d]
      [(#%top . v) #`v]
      [(a ...) (map simplify (syntax->list #`(a ...)))]
      [x #`x]))
  
  (define (unexpand t)
    (if (pair? t)
        (let ([kw (car t)])
          (if (list? t)
              (cond
                [(eq? kw '#%app) (map unexpand (cdr t))]
                [else (map unexpand t)])
              (cond
                [(eq? kw '#%datum) (cdr t)]
                [(eq? kw '#%top) (cdr t)]
                [else t])))
        t))
  
  (define (set-event-num! num)
    (check-range num 0 (- (length (events)) 1))
    (namespace-set-variable-value! 'current-event-num num)
    (namespace-set-variable-value! 'current-frame-num 0))
  
  (define (set-frame-num! num)
    (check-range num 0 (- (length (current-mark-list)) 1))
    (namespace-set-variable-value! 'current-frame-num num))
  
  (define (bt)
    (for-each 
     (lambda (mark num)
       (printf "~v: ~v\n" num (unexpand (syntax-object->datum (mark-source mark)))))
     (current-mark-list)
     (build-list (length (current-mark-list)) (lambda (x) x))))

  (define (src)
    (let ([source (mark-source (list-ref (current-mark-list) (current-frame-num)))])
      ((namespace-variable-value 'highlight-source-position) (syntax-position source))
      (printf "~v\n" source)))

  (define (binding sym)
    (map (lambda (binding) (list (mark-binding-binding binding) (mark-binding-value binding)))
    (lookup-all-bindings (lambda (id) (eq? (syntax-e id) sym)) (do-n-times cdr (current-frame-num) (current-mark-list)))))

  (define (bound)
    (map (lambda (binding) (list (syntax-e binding) binding))
         (all-bindings (car (do-n-times cdr (current-frame-num) (current-mark-list))))))
  
  (define (do-n-times fn n arg)
    (foldl (lambda (x arg) (fn arg)) arg (build-list n (lambda (x) x)))))