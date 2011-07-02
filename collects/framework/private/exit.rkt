#lang scheme/unit

  (require string-constants
           "sig.rkt"
           "../preferences.rkt"
           "../gui-utils.rkt"
           mred/mred-sig)
  
  (import mred^)
  (export (rename framework:exit^
                  (-exit exit)))
  
  (define can?-callbacks '())
  (define on-callbacks '())
  
  (define insert-can?-callback
    (λ (cb)
      (set! can?-callbacks (cons cb can?-callbacks))
      (λ ()
        (set! can?-callbacks
              (let loop ([cb-list can?-callbacks])
                (cond
                  [(null? cb-list) '()]
                  [(eq? cb (car cb-list)) (cdr cb-list)]
                  [else (cons (car cb-list) (loop (cdr cb-list)))]))))))
  
  (define insert-on-callback
    (λ (cb)
      (set! on-callbacks (cons cb on-callbacks))
      (λ ()
        (set! on-callbacks
              (let loop ([cb-list on-callbacks])
                (cond
                  [(null? cb-list) '()]
                  [(eq? cb (car cb-list)) (cdr cb-list)]
                  [else (cons (car cb-list) (loop (cdr cb-list)))]))))))
  
  (define is-exiting? #f)
  (define (set-exiting b) (set! is-exiting? b))
  (define (exiting?) is-exiting?)
  
  (define (can-exit?) (andmap (λ (cb) (cb)) can?-callbacks))
  (define (on-exit) (for-each (λ (cb) (cb)) on-callbacks))
  
  (define (user-oks-exit)
    (if (preferences:get 'framework:verify-exit)
        (gui-utils:get-choice
         (if (eq? (system-type) 'windows)
             (string-constant are-you-sure-exit)
             (string-constant are-you-sure-quit))
         (if (eq? (system-type) 'windows)
             (string-constant exit)
             (string-constant quit))
         (if (eq? (system-type) 'windows)
             (string-constant dont-exit)
             (string-constant dont-quit))
         (string-constant warning)
         #f
         #f
         'app
         (case-lambda
           [() (not (preferences:get 'framework:verify-exit))]
           [(new) (preferences:set 'framework:verify-exit (not new))]))
        #t))
  
  (define (-exit)
    (set! is-exiting? #t)
    (cond
      [(can-exit?)
       (on-exit)
       (queue-callback 
        (λ () 
          (exit)
          (set! is-exiting? #f)))]
      [else
       (set! is-exiting? #f)]))
