#lang racket
(require racket/stxparam
         net/url
         web-server/dispatchers/dispatch
         web-server/dispatch/http-expanders
         web-server/dispatch/bidi-match
         (for-syntax web-server/dispatch/pattern))

(define (default-else req)
  (next-dispatcher))

(define (string-list->url strlist)
  (url->string
   (make-url #f #f #f #f #t
             (if (empty? strlist)
                 (list (make-path/param "" empty))
                 (map (lambda (s) (make-path/param s empty))
                      strlist))
             empty #f)))

(define-syntax (dispatch-case stx)
  (syntax-case stx (else)
    [(_ [(path-pat ...) fun]
        ...
        [else else-fun])
     (for-each dispatch-pattern?
               (syntax->list #'((path-pat ...) ...)))
     (with-syntax
         ([((path-pat/id ...) ...)
           (map dispatch-pattern->dispatch-pattern/ids
                (syntax->list #'((path-pat ...) ...)))])
       (with-syntax
           ([((path-pat-id ...) ...)
             (map (lambda (pp/is)
                    (map (lambda (bs)
                           (with-syntax ([(bidi-id arg ... id) bs])
                             #'id))
                         (filter (lambda (pp/i)
                                   (syntax-case pp/i ()
                                     [(bidi-id arg ... id) #t]
                                     [_ #f]))
                                 (syntax->list pp/is))))
                  (syntax->list #'((path-pat/id ...) ...)))])
         (syntax/loc stx
           (lambda (the-req)
             (syntax-parameterize ([bidi-match-going-in? #t])
                                  (match the-req
                                    [(request/url (url/paths path-pat/id ...)) 
                                     (fun the-req path-pat-id ...)]
                                    ...
                                    [_ (else-fun the-req)]))))))]
    [(dc [(path-pat ...) fun]
         ...)
     (syntax/loc stx
       (dc [(path-pat ...) fun]
           ...
           [else default-else]))]))

(define-syntax (dispatch-url stx)
  (syntax-case stx ()
    [(_ [(path-pat ...) fun]
        ...)
     (for-each dispatch-pattern?
               (syntax->list #'((path-pat ...) ...)))
     (with-syntax
         ([((path-pat/id ...) ...)
           (map dispatch-pattern->dispatch-pattern/ids
                (syntax->list #'((path-pat ...) ...)))])
       (with-syntax
           ([((from-path-pat ...) ...)
             (map (lambda (pp/is-pre)
                    (define pp/is (datum->syntax pp/is-pre (filter (compose not string-syntax?) (syntax->list pp/is-pre)) pp/is-pre))
                    (for/list ([pp/i (dispatch-pattern-not-... pp/is)]
                               [next-...? (dispatch-pattern-next-...? pp/is)])
                      (with-syntax ([pp pp/i]
                                    [(bidi-id arg ... id) pp/i])
                        (if next-...?
                            (syntax/loc pp/i (list pp (... ...)))
                            pp/i))))
                  (syntax->list #'((path-pat/id ...) ...)))
             #;(map (lambda (pp/is)
                      (filter (compose not string-syntax?)
                              (syntax->list pp/is)))
                    (syntax->list #'((path-pat/id ...) ...)))]              
            [((from-body ...) ...)
             (map (lambda (pp/is)
                    (for/list ([pp/i (dispatch-pattern-not-... pp/is)]
                               [next-...? (dispatch-pattern-next-...? pp/is)])
                      (with-syntax ([pp pp/i])
                        (if (string-syntax? pp/i)
                            (syntax/loc pp/i (list pp))
                            (with-syntax ([(bidi-id arg ... id) pp/i])
                              (if next-...?
                                  (syntax/loc pp/i id)
                                  (syntax/loc pp/i (list id))))))))  
                  (syntax->list #'((path-pat/id ...) ...)))])
         (syntax/loc stx
           (syntax-parameterize ([bidi-match-going-in? #f])
                                (match-lambda*
                                  [(list (? (lambda (x) (eq? x fun))) from-path-pat ...)
                                   (string-list->url (append from-body ...))]
                                  ...)))))]))

(define-syntax (dispatch-rules stx)
  (syntax-case stx (else)
    [(_ [(path-pat ...) fun]
        ...
        [else else-fun])
     (for-each dispatch-pattern?
               (syntax->list #'((path-pat ...) ...)))
     (syntax/loc stx
       (values 
        (dispatch-case [(path-pat ...) fun]
                       ...
                       [else else-fun])
        (dispatch-url [(path-pat ...) fun]
                      ...)))]
    [(dr [(path-pat ...) fun]
         ...)
     (syntax/loc stx
       (dr [(path-pat ...) fun]
           ...
           [else default-else]))]))

(define (dispatch-succ . _) #t)
(define (dispatch-fail . _) #f)

(define-syntax-rule (dispatch-rules+applies
                     [pat fun]
                     ...)
  (let-values ([(dispatch url)
                (dispatch-rules
                 [pat fun]
                 ...)]
               [(applies? _)
                (dispatch-rules
                 [pat dispatch-succ]
                 ...
                 [else dispatch-fail])])
    (values dispatch url applies?)))

(provide dispatch-case
         dispatch-url
         dispatch-rules
         dispatch-rules+applies)
