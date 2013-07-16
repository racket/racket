#lang racket
(require rackunit
         web-server/http
         web-server/dispatchers/dispatch
         net/url
         racket/stxparam
         web-server/dispatch/coercion
         web-server/dispatch/bidi-match
         web-server/dispatch/http-expanders
         web-server/dispatch/pattern
         web-server/dispatch/url-patterns
         web-server/dispatch/syntax
         web-server/dispatch/serve
         web-server/dispatch/container)
(provide all-dispatch-tests)

(define (test-request url #:method [method #"GET"])
  (make-request method url null (delay null) #f "1.2.3.4" 123 "4.3.2.1"))

(define all-dispatch-tests
  (test-suite
   "Dispatch"

   #;(local
   [(define-syntax test-match=>
   (syntax-rules ()
   [(_ val pat res)
   (test-equal? (format "~S" 'pat)
   (match=> val [pat => (lambda x x)])
   res)]))]
   (test-suite
   "match"

   (test-match=> (list 1 2) (list a b) (list 1 2))
   (test-match=> (list 1 2) (list _ b) (list 2))))

   (test-suite
    "coercion"

    (test-suite "make-coerce-safe?"
                (local [(define string->number? (make-coerce-safe? string->number))]
                       (test-not-false "1" (string->number? "1"))
                       (test-not-false "1.2" (string->number? "1.2"))
                       (test-not-false "+inf.0" (string->number? "+inf.0"))
                       (test-false "a" (string->number? "a"))))


    (test-suite "define-coercion-match-expander"
                (local [(define string->number? (make-coerce-safe? string->number))
                        (define-coercion-match-expander string->number/m string->number? string->number)
                        (define (test i r)
                          (test-equal? i (match i [(string->number/m a) a] [_ #f]) r))]
                       (test "1" 1)
                       (test "1.2" 1.2)
                       (test "+inf.0" +inf.0)
                       (test "a" #f))))

   (local [(define string->number? (make-coerce-safe? string->number))
           (define-coercion-match-expander string->number/m string->number? string->number)
           (define-coercion-match-expander number->string/m number? number->string)
           (define-bidi-match-expander number-arg string->number/m number->string/m)
           (define (test i r)
             (cond
               [(and i r)
                (test-suite (format "~S" (list i r))
                            (test-equal? (format "~S" i)
                                         (syntax-parameterize ([bidi-match-going-in? #t])
                                           (match i [(number-arg a) a] [_ #f]))
                                         r)
                            (test-equal? (format "~S" r)
                                         (syntax-parameterize ([bidi-match-going-in? #f])
                                           (match r [(number-arg a) a] [_ #f]))
                                         i))]

               [i
                (test-equal? (format "~S" i)
                             (syntax-parameterize ([bidi-match-going-in? #t])
                               (match i [(number-arg a) a] [_ #f]))
                             r)]
               [r
                (test-equal? (format "~S" r)
                             (syntax-parameterize ([bidi-match-going-in? #f])
                               (match r [(number-arg a) a] [_ #f]))
                             i)]))]
          (test-suite
           "bidi-match"
           (test "1" 1)
           (test "1.2" 1.2)
           (test "+inf.0" +inf.0)
           (test "a" #f)
           (test #f "a")))

   (test-suite
    "http-expanders"

    (test-not-false "http://www.example.com/new"
                    (match (string->url "http://www.example.com/new")
                      [(url/paths "new")
                       #t]
                      [else
                       #f]))

    (test-not-false "http://www.example.com/"
                    (match (string->url "http://www.example.com/")
                      [(url/paths "")
                       #t]
                      [else
                       #f]))

    (test-not-false "http://www.example.com"
                    (match (string->url "http://www.example.com")
                      [(url/paths)
                       #t]
                      [else
                       #f]))

    (test-false "http://www.example.com/foo"
                (match (string->url "http://www.example.com/foo")
                  [(url/paths "new")
                   #t]
                  [else
                   #f]))

    (test-equal? "http://www.example.com/new/50"
                 (match (string->url "http://www.example.com/new/50")
                   [(url/paths "new" (integer-arg a))
                    a]
                   [else
                    #f])
                 50)

    (test-false "http://www.example.com/new"
                (match (string->url "http://www.example.com/new")
                  [(url/paths "new" (integer-arg a))
                   a]
                  [else
                   #f]))

    (test-not-false "http://www.example.com/new"
                    (match (test-request (string->url "http://www.example.com/new"))
                      [(request/url (url/paths "new"))
                       #t]
                      [else
                       #f]))

    (test-equal? "http://www.example.com/new/50"
                 (match (test-request (string->url "http://www.example.com/new/50"))
                   [(request/url (url/paths "new" (integer-arg a)))
                    a]
                   [else
                    #f])
                 50))

   (test-suite
    "pattern"

    (test-false "string-syntax?" (string-syntax? #'a))
    (test-false "string-syntax?" (string-syntax? #'(a b)))
    (test-not-false "string-syntax?" (string-syntax? #'"foo"))

    (test-equal? "dispatch-pattern-next-...?"
                 (dispatch-pattern-next-...? #'(a))
                 (list #f))
    (test-equal? "dispatch-pattern-next-...?"
                 (dispatch-pattern-next-...? #'(a b))
                 (list #f #f))
    (test-equal? "dispatch-pattern-next-...?"
                 (dispatch-pattern-next-...? #'(a (... ...)))
                 (list #t))

    (test-equal? "dispatch-pattern-not-..."
                 (map syntax->datum (dispatch-pattern-not-... #'(a)))
                 '(a))
    (test-equal? "dispatch-pattern-not-..."
                 (map syntax->datum (dispatch-pattern-not-... #'(a b)))
                 '(a b))
    (test-equal? "dispatch-pattern-not-..."
                 (map syntax->datum (dispatch-pattern-not-... #'(a (... ...))))
                 '(a))

    (local
     [(define (test in out)
        (test-equal? "dispatch-pattern->dispatch-pattern/ids"
                     (map syntax->datum (dispatch-pattern->dispatch-pattern/ids in))
                     out))]
     (test-suite
      "dispatch-pattern->dispatch-pattern/ids"
      (test #'() empty)
      (test #'("string") (list "string"))
      (test #'((... ...)) (list '...))

      (test-case "arg"
                 (check-equal? (first (first (map syntax->datum (dispatch-pattern->dispatch-pattern/ids #'((string-arg))))))
                               'string-arg)
                 (check-pred symbol? (second (first (map syntax->datum (dispatch-pattern->dispatch-pattern/ids #'((string-arg))))))))))

    (test-exn "dispatch-pattern? ..." exn? (lambda () (dispatch-pattern? #'((... ...)))))
    (test-exn "dispatch-pattern? foo ..." exn? (lambda () (dispatch-pattern? #'("foo" (... ...)))))
    (test-not-false "dispatch-pattern? integer-arg a ..." (dispatch-pattern? #'((integer-arg a) (... ...))))
    (test-not-false "dispatch-pattern? integer-arg a " (dispatch-pattern? #'((integer-arg a))))
    (test-not-false "dispatch-pattern? list a b" (dispatch-pattern? #'((list a b) (... ...))))
    (test-not-false "dispatch-pattern?" (dispatch-pattern? #'((integer-arg) (... ...))))
    (test-not-false "dispatch-pattern?" (dispatch-pattern? #'((integer-arg))))
    (test-not-false "dispatch-pattern?" (dispatch-pattern? #'("foo")))

    (test-exn "dispatch-pattern/ids?" exn? (lambda () (dispatch-pattern/ids? #'((... ...)))))
    (test-exn "dispatch-pattern/ids?" exn? (lambda () (dispatch-pattern/ids? #'("foo" (... ...)))))
    (test-not-false "dispatch-pattern/ids?" (dispatch-pattern/ids? #'((integer-arg a) (... ...))))
    (test-not-false "dispatch-pattern/ids?" (dispatch-pattern/ids? #'((integer-arg a))))
    (test-not-false "dispatch-pattern/ids?" (dispatch-pattern/ids? #'((list a b) (... ...))))
    (test-exn "dispatch-pattern/ids?" exn? (lambda () (dispatch-pattern/ids? #'((integer-arg) (... ...)))))
    (test-exn "dispatch-pattern/ids?" exn? (lambda () (dispatch-pattern/ids? #'((integer-arg)))))
    (test-not-false "dispatch-pattern/ids?" (dispatch-pattern/ids? #'("foo"))))

   (local [(define-syntax test-arg
             (syntax-rules ()
               [(_ (arg arg-a ...)
                   ([in-expr out-expr] ...)
                   [in-fail-expr ...]
                   [out-fail-expr ...])
                (test-suite (format "~S" 'arg)
                            (test-equal? (format "in ~S" in-expr)
                                         (syntax-parameterize ([bidi-match-going-in? #t])
                                           (match in-expr [(arg arg-a ... a) a]))
                                         out-expr)
                            ...
                            (test-equal? (format "out ~S" out-expr)
                                         (syntax-parameterize ([bidi-match-going-in? #f])
                                           (match out-expr [(arg arg-a ... a) a]))
                                         in-expr)
                            ...
                            (test-false (format "in-fail ~S" in-fail-expr)
                                        (syntax-parameterize ([bidi-match-going-in? #t])
                                          (match in-fail-expr [(arg arg-a ... a) a] [_ #f])))
                            ...
                            (test-false (format "out-fail ~S" out-fail-expr)
                                        (syntax-parameterize ([bidi-match-going-in? #f])
                                          (match out-fail-expr [(arg arg-a ... a) a] [_ #f])))
                            ...)]))]
          (test-suite
           "url-patterns"

           (test-arg (number-arg)
                     (["1" 1]
                      ["2.3" 2.3]
                      ["+inf.0" +inf.0])
                     ["a"]
                     ['a #t])

           (test-arg (integer-arg)
                     (["1" 1])
                     ["a" "2.3" "+inf.0"]
                     ['a #t 2.3 +inf.0])

           (test-arg (real-arg)
                     (["1" 1]
                      ["2.3" 2.3]
                      ["+inf.0" +inf.0])
                     ["a"]
                     ['a #t])

           (test-arg (string-arg)
                     (["1" "1"]
                      ["foo" "foo"]
                      ["/" "/"])
                     []
                     ['a #t 5])

           (test-arg (symbol-arg)
                     (["1" '|1|]
                      ["foo" 'foo]
                      ["/" '/])
                     []
                     ["a" #t 5])

           (local [(define-match-expander const-m
                     (syntax-rules ()
                       [(_ v id) (? (curry equal? v) id)]))
                   (define-bidi-match-expander const-arg const-m const-m)]
                  (test-arg (const-arg "1")
                            (["1" "1"])
                            ["2"]
                            ["2"]))))

   (test-suite
    "syntax"

    (test-suite
     "methods"
     (local
      [(define (get req i) (add1 i))
       (define (post req i) (sub1 i))
       (define-values (blog-dispatch blog-url blog-applies?)
         (dispatch-rules+applies
          [((integer-arg)) #:method "get" get]
          [((integer-arg)) #:method "post" post]
          [((integer-arg)) get]))
       (define (test-blog-dispatch url method val)
         (test-equal? url
                      (blog-dispatch
                       (test-request #:method method (string->url url)))
                      val))]

      (test-blog-dispatch "http://www.example.com/5" #"get" 6)
      (test-blog-dispatch "http://www.example.com/6" #"get" 7)
      (test-blog-dispatch "http://www.example.com/7" #"post" 6)
      (test-blog-dispatch "http://www.example.com/8" #"post" 7)))

    (test-suite
     "applies"
     (local
      [(define (list-posts req) `(list-posts))
       (define (review-post req p) `(review-post ,p))
       (define (review-archive req y m) `(review-archive ,y ,m))
       (define-values (blog-dispatch blog-url blog-applies?)
         (dispatch-rules+applies
          [("") list-posts]
          [() list-posts]
          [("posts" (string-arg)) review-post]
          [("archive" (integer-arg) (integer-arg)) review-archive]))
       (define (test-blog-dispatch url)
         (test-equal? url (blog-applies? (test-request (string->url url))) #t))
       (define (test-blog-dispatch/exn url)
         (test-equal? url (blog-applies? (test-request (string->url url))) #f))]

      (test-blog-dispatch "http://www.example.com")
      (test-blog-dispatch "http://www.example.com/")
      (test-blog-dispatch "http://www.example.com/posts/hello-world")
      (test-blog-dispatch "http://www.example.com/archive/2008/02")
      (test-blog-dispatch/exn "http://www.example.com/posts")
      (test-blog-dispatch/exn "http://www.example.com/archive/post/02")
      (test-blog-dispatch/exn "http://www.example.com/archive/2008/post")
      (test-blog-dispatch/exn "http://www.example.com/foo")))

    (let ()
      (define (list-posts req) `(list-posts))
      (define (review-post req p) `(review-post ,p))
      (define (review-archive req y m) `(review-archive ,y ,m))

      (define (make-dispatch-test-suite blog-dispatch blog-url)
        (define (test-blog-dispatch url res)
          (test-equal? url (blog-dispatch (test-request (string->url url))) res))
        (define (test-blog-url url . args)
          (test-equal? (format "~S" args)
                       (apply blog-url args)
                       url))
        (define (test-blog-url/exn . args)
          (test-exn (format "~S" args)
                    exn?
                    (lambda ()
                      (apply blog-url args))))
        (define (test-blog-dispatch/exn url)
          (test-exn url exn:dispatcher? (lambda () (blog-dispatch (test-request (string->url url))))))

        (test-suite
         "blog"

         (test-blog-dispatch "http://www.example.com" `(list-posts))
         (test-blog-dispatch "http://www.example.com/" `(list-posts))
         (test-blog-dispatch "http://www.example.com/posts/hello-world" `(review-post "hello-world"))
         (test-blog-dispatch "http://www.example.com/archive/2008/02" `(review-archive 2008 02))
         (test-blog-dispatch/exn "http://www.example.com/posts")
         (test-blog-dispatch/exn "http://www.example.com/archive/post/02")
         (test-blog-dispatch/exn "http://www.example.com/archive/2008/post")
         (test-blog-dispatch/exn "http://www.example.com/foo")

         (test-blog-url "/" list-posts)
         (test-blog-url "/posts/hello-world" review-post "hello-world")
         (test-blog-url "/archive/2008/2" review-archive 2008 02)
         (test-blog-url/exn list-posts 50)
         (test-blog-url/exn +)
         (test-blog-url/exn review-post 50)
         (test-blog-url/exn review-post "hello" "world")
         (test-blog-url/exn review-archive 2008 02 1)
         (test-blog-url/exn review-archive "2008" 02)
         (test-blog-url/exn review-archive 2008 "02")))

      (test-suite
       "dispatch"
       (let ()
         (define-values (blog-dispatch blog-url)
           (dispatch-rules
            [("") list-posts]
            [() list-posts]
            [("posts" (string-arg)) review-post]
            [("archive" (integer-arg) (integer-arg)) review-archive]))
         (make-dispatch-test-suite blog-dispatch blog-url))

       (let ()
         (define-container blog-container
           (blog-dispatch blog-url))
         (dispatch-rules! blog-container
                          [("") list-posts])
         (dispatch-rules! blog-container
                          [() list-posts])
         (dispatch-rules! blog-container
                          [("posts" (string-arg)) review-post])
         (dispatch-rules! blog-container
                          [("archive" (integer-arg) (integer-arg)) review-archive])
         (make-dispatch-test-suite blog-dispatch blog-url))))

    (local
     [(define (sum req as) (apply + as))
      (define-values (rest-dispatch rest-url)
        (dispatch-rules
         [((integer-arg) ...) sum]))
      (define (test-rest-dispatch url res)
        (test-equal? url (rest-dispatch (test-request (string->url url))) res))
      (define (test-rest-url url . args)
        (test-equal? (format "~S" args)
                     (apply rest-url args)
                     url))
      (define (test-rest-url/exn . args)
        (test-exn (format "~S" args)
                  exn?
                  (lambda ()
                    (apply rest-url args))))
      (define (test-rest-dispatch/exn url)
        (test-exn url exn:dispatcher? (lambda () (rest-dispatch (test-request (string->url url))))))]
     (test-suite
      "rest args"

      (test-rest-dispatch "http://www.sum.com" 0)
      (test-rest-dispatch "http://www.sum.com/1" 1)
      (test-rest-dispatch "http://www.sum.com/1/2" 3)
      (test-rest-dispatch "http://www.sum.com/1/2/3" 6)
      (test-rest-dispatch/exn "http://www.sum.com/1/2/3/bar")
      (test-rest-dispatch/exn "http://www.sum.com/1/bar")
      (test-rest-dispatch/exn "http://www.sum.com/bar")

      (test-rest-url "/" sum empty)
      (test-rest-url "/1" sum (list 1))
      (test-rest-url "/1/2" sum (list 1 2))
      (test-rest-url "/1/2/3" sum (list 1 2 3))
      (test-rest-url/exn sum "foo")
      (test-rest-url/exn sum 'bar)
      (test-rest-url/exn sum 1)
      (test-rest-url/exn sum #t)))

    (local
     [(define (sum req as ss) (list* (apply + as) ss))
      (define-values (rest-dispatch rest-url)
        (dispatch-rules
         [((integer-arg) ... (string-arg) ...) sum]))
      (define (test-rest-dispatch url res)
        (test-equal? url (rest-dispatch (test-request (string->url url))) res))
      (define (test-rest-url url . args)
        (test-equal? (format "~S" args)
                     (apply rest-url args)
                     url))
      (define (test-rest-url/exn . args)
        (test-exn (format "~S" args)
                  exn?
                  (lambda ()
                    (apply rest-url args))))
      (define (test-rest-dispatch/exn url)
        (test-exn url exn:dispatcher? (lambda () (rest-dispatch (test-request (string->url url))))))]
     (test-suite
      "rest args (2)"

      (test-rest-dispatch "http://www.sum.com" (list 0))
      (test-rest-dispatch "http://www.sum.com/1" (list 1))
      (test-rest-dispatch "http://www.sum.com/1/2" (list 3))
      (test-rest-dispatch "http://www.sum.com/1/2/3" (list 6))
      (test-rest-dispatch "http://www.sum.com/1/2/3/bar" (list 6 "bar"))
      (test-rest-dispatch "http://www.sum.com/1/bar" (list 1 "bar"))
      (test-rest-dispatch "http://www.sum.com/1/bar/zog" (list 1 "bar" "zog"))
      (test-rest-dispatch "http://www.sum.com/bar/zog" (list 0 "bar" "zog"))

      (test-rest-url "/" sum empty empty)
      (test-rest-url "/1" sum (list 1) empty)
      (test-rest-url "/1/2" sum (list 1 2) empty)
      (test-rest-url "/1/2/3" sum (list 1 2 3) empty)
      (test-rest-url "/bar" sum empty (list "bar"))
      (test-rest-url "/bar/zog" sum empty (list "bar" "zog"))
      (test-rest-url "/1/2/bar" sum (list 1 2) (list "bar"))
      (test-rest-url/exn sum "foo")
      (test-rest-url/exn sum 'bar)
      (test-rest-url/exn sum 1)
      (test-rest-url/exn sum #t))))

   (test-suite
    "serve")))

(define (test-serve/dispatch)
  (define-values (start url)
    (dispatch-rules
     [("") get-first-number]
     [("/2nd" (number-arg)) get-second-number]
     [("sum" (number-arg) (number-arg)) display-sum]))
  (define (get-first-number req)
    `(html (head (title "First number"))
           (a ([href ,(url get-second-number 50)]) (h1 "+ 50"))))
  (define (get-second-number req fst)
    `(html (head (title "Second number"))
           (a ([href ,(url display-sum fst 100)]) (h1 "+ 100"))))
  (define (display-sum req fst snd)
    `(html (head (title "Sum"))
           (h1 ,(number->string (+ fst snd)))))

  (serve/dispatch start))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-dispatch-tests))
