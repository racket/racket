#lang racket/base
(require racket/class
         racket/place
         racket/match
         racket/contract
         (for-syntax racket/base)
         drracket/private/syncheck/traversals
         drracket/private/syncheck/syncheck-intf
         drracket/private/syncheck/xref
         "../../private/eval-helpers-and-pref-init.rkt"
         "intf.rkt"
         "local-member-names.rkt")

(provide go monitor)

(define (create-rename-answerer-thread orig-cust local-chan table)
  ;; the hope is that changing the custodian like this
  ;; shouldn't leak these threads, but it does seem to
  ;; so for now we don't use it
  (parameterize (#;[current-custodian orig-cust])
    (thread
     (λ () 
       (with-handlers ([exn:fail? (λ (x) (eprintf "online-comp.rkt: thread failed ~a\n"
                                                  (exn-message x)))])
         (let loop ()
           (define id/name (place-channel-get local-chan))
           (define id (list-ref id/name 0))
           (define name (list-ref id/name 1))
           (define res ((hash-ref table id) name))
           (place-channel-put local-chan res)
           (loop))))))
  (void))

(define-logger online-check-syntax)
(define (go expanded path the-source orig-cust)
  (define c (make-channel))
  (unless (exn? expanded)
    (log-message online-check-syntax-logger 'info  "" (list expanded)))
  (log-message online-check-syntax-logger 'info  "" c)
  ;; wait for everything to actually get sent back to the main place
  (channel-get c))

(define (build-trace stxes the-source orig-cust path)
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
      (define obj (new build-place-chan-trace%
                       [src the-source]
                       [orig-cust orig-cust]))
      (define-values (expanded-expression expansion-completed) 
        (make-traversal (current-namespace)
                        (get-init-dir path)))
      (parameterize ([current-annotations obj])
        (for ([stx (in-list stxes)])
          (expanded-expression stx))
        (expansion-completed))
      (send obj get-trace))))

(define build-place-chan-trace%
  (class build-trace%
    (inherit add-to-trace)
    (init-field  orig-cust)
    (define-values (remote-chan local-chan) (place-channel))
    (define table (make-hash))
    (create-rename-answerer-thread orig-cust local-chan table)
    (define/override (syncheck:add-arrow/name-dup _start-text start-pos-left start-pos-right
                                                  _end-text end-pos-left end-pos-right
                                                  actual? level require-arrow? name-dup?)
      (define id (hash-count table))
      (hash-set! table id name-dup?)
      (add-to-trace (vector 'syncheck:add-arrow/name-dup
                            start-pos-left start-pos-right
                            end-pos-left end-pos-right
                            actual? level require-arrow? remote-chan id)))
    
    (define/override (syncheck:add-id-set to-be-renamed/poss dup-name?)
      (define id (hash-count table))
      (hash-set! table id dup-name?)
      (add-to-trace (vector 'syncheck:add-id-set (map cdr to-be-renamed/poss) remote-chan id)))
    (super-new)))
    

(define (monitor send-back path the-source orig-cust)
  (define lr (make-log-receiver (current-logger)
                                'info
                                'online-check-syntax))
  (thread
   (λ ()
     (let loop ()
       (define val (sync lr))
       (match val
         [(vector level message obj name)
          (cond
            [(and (list? obj) (andmap syntax? obj))
             (define trace (build-trace obj the-source orig-cust path))
             (send-back trace)]
            [(channel? obj)
             ;; signal back to the main place that we've gotten everything
             ;; and sent it back over
             (channel-put obj (void))])]
         [_ (void)])
       (loop)))))
