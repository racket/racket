;; Drive a servlet. Pretend to be a Web browser, and send a request to the
;; server. Produce the response.

(module send-assertions mzscheme
  (require (lib "contract.ss")
           (lib "test.ss" "schemeunit")
           (lib "etc.ss")
           (lib "match.ss")
           "servlet-testing-framework.ss"
           (lib "servlet.ss" "web-server"))

  (provide
    assert-output-response/suspended
    (struct unknown ())
    hyperlink->k-url
    form->k-url)

  ;; The unknown value
  (define-struct unknown () (make-inspector))

  #|
  (define in?
    (union
      (list-immutable/c (xexpr/callback? . -> . string?)
                        (listof (cons/c symbol? string?)))
      (symbols 'back 'forward)
      (list-immutable/c (xexpr/callback? . -> . string?)
                        (listof (cons/c symbol? string?))
                        (listof (cons/c symbol? string?)))))
  |#

  ;; Ensure output-response produces the right value. Feed the send/suspends.
  ;; (-> response) (listof in?) xexpr? . -> . boolean?
  (define (assert-output-response/suspended outputter ins out)
    (special-equal?
      (assert-loop ins (start-servlet outputter) 0 '())
      out))

  ;; A history is a
  ;; (make-history string? (listof (cons/p symbol? string?))
  ;;                       (union #f (listof (cons/p symbol? string?)))
  (define-struct history (k-url bindings headers))

  ;; Go over the input list, resuming the servlet on each choice, until the
  ;; end. Support the back-button with an indexed list.
  ;; This is written in a pattern-matching style to increment/decrement the
  ;; counter correctly, and do any sanity checks (e.g. 'forward after a
  ;; non-'back).
  (define (assert-loop ins r i hs)
    (let ((forward-error (make-exn:fail:contract:variable
                           (string->immutable-string
                             "Cannot go forward more times than back")
                           (current-continuation-marks)
                           'blah)))
      (match ins
        (('back 'back . ls) (assert-loop (cons 'back ls)
                                         (resumer (list-ref hs (add1 i)))
                                         (add1 i)
                                         hs))
        (('back 'forward . ls) (assert-loop (cons 'forward ls)
                                            (resumer (list-ref hs (add1 i)))
                                            (add1 i)
                                            hs))
        (('back l . ls) (assert-loop (cons l ls)
                                     (resumer (list-ref hs (add1 i)))
                                     0
                                     (list-tail hs (add1 i))))
        (('forward 'back . ls) (if (> i 0)
                                 (assert-loop (cons 'back ls)
                                              (resumer (list-ref hs (sub1 i)))
                                              (sub1 i)
                                              hs)
                                 (raise forward-error)))
        (('forward 'forward . ls) (if (> i 0)
                                    (assert-loop (cons 'forward ls)
                                                 (resumer (list-ref hs (sub1 i)))
                                                 (sub1 i)
                                                 hs)
                                    (raise forward-error)))
        (('forward l . ls) (if (> i 0)
                             (assert-loop (cons l ls)
                                          (resumer (list-ref hs (sub1 i)))
                                          0
                                          (list-tail (sub1 i)))
                             (raise forward-error)))
        ((l 'back . ls) (let ((h (history/list l r)))
                          (assert-loop (cons 'back ls)
                                       (resumer h)
                                       0
                                       (cons h hs))))
        ((l n . ls) (if (and (symbol? n) (symbol=? n 'forward))
                      (raise forward-error)
                      (let ((h (history/list l r)))
                        (assert-loop (cons n ls)
                                     (resumer h)
                                     0
                                     (cons h hs)))))
        (('back) (resumer (list-ref hs (add1 i))))
        (('forward) (if (> i 0)
                      (resumer (list-ref hs (sub1 i)))
                      (raise forward-error)))
        ((l) (resumer (history/list l r)))
        ((? null?) r))))

  ;; history/list : (union (list/p (xexpr? . -> . string?)
  ;;                               (listof (cons/p symbol? string?)))
  ;;                       (list/p (xexpr? . -> . string?)
  ;;                               (listof (cons/p symbol? string?))
  ;;                               (listof (cons/p symbol? string?))))
  ;;                response? . -> . history?
  (define (history/list l r)
    (let ((k-url ((car l) r)))
      (if k-url
        (cond
          ((= (length l) 2) (make-history k-url (cadr l) #f))
          ((= (length l) 3) (make-history k-url (cadr l) (caadr l))))
        (fail (format "~a~n~v~n~a~v~n"
                       "Failed to match the response" r
                       "with the function" (car l))))))

  ;; resumer : history? . -> . response?
  ;; Resume the servlet correctly.
  (define (resumer a-history)
    (if (history-headers a-history)
      (resume-servlet/headers (history-k-url a-history)
                              (history-bindings a-history)
                              (history-headers a-history))
      (resume-servlet (history-k-url a-history)
                      (history-bindings a-history))))

  ;; True if:
  ;;  a or b is `unknown'
  ;;  a and b are not pairs and are equal?
  ;;  a and b are pairs and their car and cdr are special-equal?
  (define (special-equal? a b)
    (cond
      ((or (unknown? a) (unknown? b)) #t)
      ((and (not (pair? a)) (not (pair? b))) (equal? a b))
      ((and (pair? a) (pair? b)) (and (special-equal? (car a) (car b))
                                      (special-equal? (cdr a) (cdr b))))
      (else (equal? a b))))

  ;; Produce the k-url for the form on this page
  (define form->k-url
    (match-lambda
      ((`form ((attrs rhss) ...) . rest)
       (ormap (lambda (attr rhs) (and (eqv? attr 'action) rhs))
              attrs rhss))
      ((tag (attrs ...) body ...) (ormap form->k-url body))
      (else #f)))

  ;; Produce the k-url used for the next part of the form
  ;; (xexpr/callback? . -> . (union false? string?))
  (define-syntax hyperlink->k-url
    (syntax-rules ()
      ((_ str)
       (letrec ((f (match-lambda
                     (('a ((attrs rhss) (... ...)) str)
                      (ormap (lambda (attr rhs) (and (eqv? attr 'href) rhs))
                             attrs rhss))
                     ((tag (attrs (... ...)) body (... ...)) (ormap f body))
                     (else #f))))
         f))))

 )
