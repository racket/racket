#lang racket/base
(require "blame.rkt"
         "kwd-info-struct.rkt"
         "list.rkt")

(provide do-arity-checking
         
         ;; for test suites
         arity-as-string
         raw-arity-as-string)

(define (do-arity-checking blame val 
                           ->stct-doms
                           ->stct-rest
                           ->stct-min-arity
                           ->stct-kwd-infos
                           method?)
  (define proc/meth (if method? "a method" "a procedure"))
  (let/ec k
    (unless (procedure? val)
      (k
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            `(expected: ,proc/meth
                                        given: "~e")
                            val))))
     (define-values (actual-mandatory-kwds actual-optional-kwds) (procedure-keywords val))
     (define arity (if (list? (procedure-arity val))
                       (procedure-arity val)
                       (list (procedure-arity val))))
    
     (define exra-required-args (if (ellipsis-rest-arg-ctc? ->stct-rest)
                                    (length (*list-ctc-suffix ->stct-rest))
                                    0))

     ;; the function must be ok for *all* the arities the contract says are ok
     (for/and ([base-number-of-non-keyword-args (in-range ->stct-min-arity (add1 (length ->stct-doms)))])
       (define expected-number-of-non-keyword-args (+ base-number-of-non-keyword-args exra-required-args))
       (define matching-arity?
         (and (for/or ([a (in-list arity)])
                (or (and (equal? expected-number-of-non-keyword-args a))
                    (and (arity-at-least? a)
                         (>= expected-number-of-non-keyword-args (arity-at-least-value a)))))
              (if ->stct-rest
                  (let ([lst (car (reverse arity))])
                    (and (arity-at-least? lst)
                         (<= (arity-at-least-value lst) (+ exra-required-args ->stct-min-arity))))
                  #t)))
       (unless matching-arity?
         (k
          (λ (neg-party)
            (define expected-number-of-non-keyword-args*
              ((if method? sub1 values) expected-number-of-non-keyword-args))
            (raise-blame-error blame #:missing-party neg-party val
                               `(expected:
                                 ,(string-append "a "
                                                 proc/meth
                                                 " that accepts ~a non-keyword argument~a~a")
                                 given: "~e"
                                 "\n  ~a")
                               expected-number-of-non-keyword-args*
                               (if (= expected-number-of-non-keyword-args* 1) "" "s")
                               (if ->stct-rest
                                   " and arbitrarily many more"
                                   "")
                               val
                               (arity-as-string val))))))

    (define (should-have-supplied kwd)
      (k
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            `(expected: 
                              ,(string-append proc/meth " that accepts the ~a keyword argument")
                              given: "~e"
                              "\n  ~a")
                            kwd
                            val
                            (arity-as-string val method?)))))
    
    (define (should-not-have-supplied kwd)
      (k
       (λ (neg-party)
         (raise-blame-error blame #:missing-party neg-party val
                            `(expected: 
                              ,(string-append proc/meth " that does not require the ~a keyword argument")
                              given: "~e"
                              "\n  ~a")
                            kwd
                            val
                            (arity-as-string val method?)))))
    
    (when actual-optional-kwds ;; when all kwds are okay, no checking required
      (let loop ([mandatory-kwds actual-mandatory-kwds]
                 [all-kwds actual-optional-kwds]
                 [kwd-infos ->stct-kwd-infos])
        (cond
          [(null? kwd-infos)
           (unless (null? mandatory-kwds)
             (should-not-have-supplied (car mandatory-kwds)))]
          [else
           (define kwd-info (car kwd-infos))
           (define-values (mandatory? kwd new-mandatory-kwds new-all-kwds)
             (cond
               [(null? all-kwds)
                (should-have-supplied (kwd-info-kwd kwd-info))]
               [else
                (define mandatory? 
                  (and (pair? mandatory-kwds)
                       (equal? (car mandatory-kwds) (car all-kwds))))
                (values mandatory? 
                        (car all-kwds)
                        (if mandatory?
                            (cdr mandatory-kwds)
                            mandatory-kwds)
                        (cdr all-kwds))]))
           (cond
             [(equal? kwd (kwd-info-kwd kwd-info))
              (when (and (not (kwd-info-mandatory? kwd-info))
                         mandatory?)
                (k
                 (λ (neg-party)
                   (raise-blame-error 
                    blame #:missing-party neg-party val
                    `(expected:
                      ,(string-append proc/meth " that optionally accepts the keyword ~a (this one is mandatory)")
                      given: "~e"
                      "\n  ~a")
                    val
                    kwd
                    (arity-as-string val method?)))))
              (loop new-mandatory-kwds new-all-kwds (cdr kwd-infos))]
             [(keyword<? kwd (kwd-info-kwd kwd-info))
              (when mandatory?
                (should-not-have-supplied kwd))
              (loop new-mandatory-kwds new-all-kwds kwd-infos)]
             [else
              (loop new-mandatory-kwds new-all-kwds kwd-infos)])])))
    
    #f))


(define (arity-as-string v [method? #f])
  (define prefix (if (object-name v)
                     (format "~a accepts: " (object-name v))
                     (format "accepts: ")))
  (string-append prefix (raw-arity-as-string v method?)))

(define (raw-arity-as-string v [method? #f])
  (define ar (procedure-arity v))
  (define adjust (if method? sub1 values))
  (define (plural n) (if (= n 1) "" "s"))
  (define-values (man-kwds all-kwds) (procedure-keywords v))
  (define opt-kwds (if all-kwds (remove* man-kwds all-kwds) #f))
  (define normal-str (if (null? all-kwds) "" "normal "))
  (define normal-args
    (cond
      [(null? ar) "no arguments"]
      [(number? ar)
       (define ar* (adjust ar))
       (format "~a ~aargument~a" ar* normal-str (plural ar*))]
      [(arity-at-least? ar) (format "~a or arbitrarily many more ~aarguments"
                                    (adjust (arity-at-least-value ar))
                                    normal-str)]
      [else
       (define comma
         (if (and (= (length ar) 2)
                  (not (arity-at-least? (list-ref ar 1))))
             ""
             ","))
       (apply
        string-append
        (let loop ([ar ar])
          (cond
            [(null? (cdr ar))
             (define v (car ar))
             (cond
               [(arity-at-least? v)
                (list
                 (format "~a, or arbitrarily many more ~aarguments" 
                         (arity-at-least-value (adjust v))
                         normal-str))]
               [else
                (list (format "or ~a ~aarguments" (adjust v) normal-str))])]
            [else 
             (cons (format "~a~a " (adjust (car ar)) comma)
                   (loop (cdr ar)))])))]))
  (cond
    [(and (null? man-kwds) (null? opt-kwds)) 
     normal-args]
    [(and (null? man-kwds) (not opt-kwds))
     (string-append normal-args " and optionally any keyword")]
    [(and (null? man-kwds) (pair? opt-kwds))
     (string-append normal-args 
                    " and the optional keyword" 
                    (plural (length opt-kwds))
                    " "
                    (kwd-list-as-string opt-kwds))]
    [(and (pair? man-kwds) (not opt-kwds))
     (string-append normal-args
                    ", the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds)
                    ", and optionally any keyword")]
    [(and (pair? man-kwds) (null? opt-kwds))
     (string-append normal-args
                    " and the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds))]
    [(and (pair? man-kwds) (pair? opt-kwds))
     (string-append normal-args
                    ", the mandatory keyword"
                    (plural (length man-kwds))
                    " "
                    (kwd-list-as-string man-kwds)
                    ", and the optional keyword"
                    (plural (length opt-kwds))
                    " "
                    (kwd-list-as-string opt-kwds))]))

(define (kwd-list-as-string kwds)
  (cond
    [(null? (cdr kwds))
     (format "~a" (list-ref kwds 0))]
    [(null? (cddr kwds))
     (format "~a and ~a" (list-ref kwds 0) (list-ref kwds 1))]
    [else 
     (apply
      string-append
      (let loop ([kwds kwds])
        (cond
          [(null? (cdr kwds))
           (list (format "and ~a" (car kwds)))]
          [else
           (cons (format "~a, " (car kwds))
                 (loop (cdr kwds)))])))]))

