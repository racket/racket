#lang racket/base
(require racket/class
         racket/place
         (for-syntax racket/base)
         "../../private/eval-helpers.rkt"
         "traversals.rkt"
         "local-member-names.rkt"
         "intf.rkt"
         "xref.rkt")

(provide go)

(define obj%
  (class (annotations-mixin object%)
    (init-field src orig-cust)
    (define trace '())
    
    (define-values (remote-chan local-chan) (place-channel))
    (define table (make-hash))

    (create-rename-answerer-thread orig-cust local-chan table)
    
    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    
    ;; send over the non _ variables in the message to the main drracket place
    (define-syntax (log stx)
      (syntax-case stx ()
        [(_ name args ...)
         (with-syntax ([(wanted-args ...)
                        (filter (λ (x) (not (regexp-match #rx"^_" (symbol->string (syntax-e x)))))
                                (syntax->list #'(args ...)))])
           #'(define/override (name args ...)
               (add-to-trace (vector 'name wanted-args ...))))]))

    (define/override (syncheck:add-arrow/name-dup _start-text start-pos-left start-pos-right
                                                  _end-text end-pos-left end-pos-right
                                                  actual? level require-arrow? name-dup?)
      (define id (hash-count table))
      (hash-set! table id name-dup?)
      (add-to-trace (vector 'syncheck:add-arrow/name-dup
                            start-pos-left start-pos-right
                            end-pos-left end-pos-right
                            actual? level require-arrow? remote-chan id)))
    (log syncheck:add-tail-arrow _from-text from-pos _to-text to-pos)
    (log syncheck:add-mouse-over-status _text pos-left pos-right str)
    (log syncheck:add-background-color _text color start fin)
    (log syncheck:add-jump-to-definition _text start end id filename submods)
    (log syncheck:add-definition-target _text start-pos end-pos id mods)
    (log syncheck:add-require-open-menu _text start-pos end-pos file)
    (log syncheck:add-docs-menu _text start-pos end-pos key the-label path definition-tag tag)
    (define/override (syncheck:add-id-set to-be-renamed/poss dup-name?)
      (define id (hash-count table))
      (hash-set! table id dup-name?)
      (add-to-trace (vector 'syncheck:add-id-set (map cdr to-be-renamed/poss) remote-chan id)))
    
    (define/public (get-trace) (reverse trace))
    (define/private (add-to-trace thing) 
      (set! trace (cons thing trace)))
    (super-new)))

(define (create-rename-answerer-thread orig-cust local-chan table)
  ;; the hope is that changing the custodian like this
  ;; shouldn't leak these threads, but it does seem to
  ;; so for now we don't use it
  (parameterize (#;[current-custodian orig-cust])
    (thread
     (λ () 
       (with-handlers ((exn:fail? (λ (x) (eprintf "online-comp.rkt: thread failed ~a\n" (exn-message x)))))
         (let loop ()
           (define id/name (place-channel-get local-chan))
           (define id (list-ref id/name 0))
           (define name (list-ref id/name 1))
           (define res ((hash-ref table id) name))
           (place-channel-put local-chan res)
           (loop))))))
  (void))

(define (go expanded path the-source orig-cust)
  (parameterize ([current-max-to-send-at-once 50])
    (with-handlers ((exn:fail? (λ (x) 
                                 (printf "exception noticed in online-comp.rkt\n")
                                 (printf "~a\n" (exn-message x))
                                 (printf "---\n")
                                 (for ([x (in-list 
                                           (continuation-mark-set->context 
                                            (exn-continuation-marks
                                             x)))])
                                   (printf "  ~s\n" x))
                                 (printf "===\n")
                                 (raise x))))
      (define obj (new obj%
                       [src the-source]
                       [orig-cust orig-cust]))
      (define-values (expanded-expression expansion-completed) 
        (make-traversal (current-namespace)
                        (get-init-dir path)))
      (parameterize ([current-annotations obj])
        (expanded-expression expanded)
        (expansion-completed))
      (send obj get-trace))))
