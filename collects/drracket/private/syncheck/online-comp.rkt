#lang racket/base
(require racket/class
         racket/place
         "../../private/eval-helpers.rkt"
         "traversals.rkt"
         "local-member-names.rkt"
         "intf.rkt"
         "xref.rkt")

(provide go)

(define obj%
  (class (annotations-mixin object%)
    (init-field src)
    (define trace '())
    
    (define-values (remote local) (place-channel))
    (define table (make-hash))
    (thread
     (λ () 
       (with-handlers ((exn:fail? (λ (x) (eprintf "online-comp.rkt: thread failed ~a\n" (exn-message x)))))
         (let loop ()
           (define id/name (place-channel-get local))
           (define id (list-ref id/name 0))
           (define name (list-ref id/name 1))
           (define res ((hash-ref table id) name))
           (place-channel-put local res)))))
    
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    (define-syntax-rule
      (log name)
      (define/override (name . args)
        (set! trace (cons (cons 'name args) trace))))

    ; (log syncheck:color-range) ;; we don't want log these as they are too distracting to keep popping up
    (log syncheck:add-mouse-over-status)
    (log syncheck:add-arrow)
    (log syncheck:add-tail-arrow)
    (log syncheck:add-background-color)
    (log syncheck:add-require-open-menu)
    (log syncheck:add-docs-menu)
    (log syncheck:add-jump-to-definition)
    (define/override (syncheck:add-rename-menu id-as-sym to-be-renamed/poss dup-name?)
      (define id (hash-count table))
      (hash-set! table id dup-name?)
      (set! trace (cons (list 'syncheck:add-rename-menu id-as-sym to-be-renamed/poss remote id)
                        trace)))
    
    (define/public (get-trace) (reverse trace))
    (super-new)))

(define (go expanded path the-source)
  (with-handlers ((exn:fail? (λ (x) 
                               (printf "~a\n" (exn-message x))
                               (printf "---\n")
                               (for ([x (in-list 
                                         (continuation-mark-set->context 
                                          (exn-continuation-marks
                                           x)))])
                                 (printf "  ~s\n" x))
                               (printf "===\n")
                               (raise x))))
    (define obj (new obj% [src the-source]))
    (define-values (expanded-expression expansion-completed) 
      (make-traversal (current-namespace)
                      (get-init-dir path)))
    (parameterize ([current-annotations obj])
      (expanded-expression expanded)
      (expansion-completed))
    (send obj get-trace)))
