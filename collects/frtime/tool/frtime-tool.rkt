#lang racket/base
(require racket/unit
         racket/class
         racket/bool
         mred
         (only-in racket/function identity)
         racket/list
         drracket/tool
         string-constants)

(provide tool@)

(define tool@
  (unit 
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (define big-frtime-language%
      (class* object% (drscheme:language:simple-module-based-language<%>)
        (define/public (get-language-numbers)
          '(1000 -400))
        (define/public (get-language-position)
          (list (string-constant experimental-languages) "FrTime"))
        (define/public (get-module)
          'frtime/frtime-big)
        (define/public (get-one-line-summary)
          "Language for functional programming of event-driven systems")
        (define/public (get-language-url) #f)
        (define/public (get-reader)
          (lambda (name port)
            (let ([v (read-syntax name port)])
              (if (eof-object? v)
                  v
                  (namespace-syntax-introduce v)))))
        (super-instantiate ())))
    
    (define (weak-member obj lis)
      (let ([cmp (lambda (v) (eq? v obj))])
        (let loop ([lis lis])
          (and (cons? lis)
               (or
                (cond
                  [(weak-box-value (first lis)) => cmp]
                  [else false])
                (loop (rest lis)))))))
    
    (define (watch watch-list value super-render-fun)
      (foldl
       (lambda (wb acc)
         (cond
           [(weak-box-value wb)
            => (lambda (f) (f acc super-render-fun))]
           [else acc]))
       value
       watch-list))
    
    (define (make-frtime-language base)
      (class (drscheme:language:module-based-language->language-mixin
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               base))
        (field (watch-list empty))
        (inherit get-language-position)
        (define/override (get-language-name)
          "FrTime")
        (define/override (on-execute settings run-in-user-thread)
          (let ([drs-eventspace (current-eventspace)])
            (super on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (let ([new-watch (namespace-variable-value 'watch)]
                     [set-evspc (namespace-variable-value 'set-eventspace)])
                 (set-evspc drs-eventspace)
                 (set! watch-list
                       ((if (weak-member new-watch watch-list)
                            identity
                            (lambda (r) (cons (make-weak-box new-watch) r)))
                        (filter weak-box-value watch-list))))))))
        
        ;; pass (lambda (v) (super render-value(/format) v settings width port))
        ;; to watcher
        (override render-value/format render-value)
        (define (render-value/format value settings port width)
          (super render-value/format (watch watch-list value (lambda (v prt) (render-value/format v settings prt width)))
                 settings port width))
        (define (render-value value settings port)
          (super render-value (watch watch-list value (lambda (v prt) (render-value settings prt)))
                 settings port))
        (define/override (use-namespace-require/copy?) #t)
        (super-instantiate ())))
    
    (define (phase1) (void))
    (define (phase2)
      (drscheme:language-configuration:add-language 
       (make-object ((drscheme:language:get-default-mixin) (make-frtime-language big-frtime-language%)))))))
