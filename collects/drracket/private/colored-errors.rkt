#lang racket

(require unstable/contract)

(struct msg-fragment:str (str) #:transparent)
(struct msg-fragment:v (v) #:transparent)
(struct colored-msg-fragment (locs frags important color) #:transparent)
(define (msg-fragment? v) (or (msg-fragment:str v) (msg-fragment:v v) (colored-msg-fragment v)))

(define srcloc-syntax/c (rename-contract (or/c srcloc? syntax? (listof (or/c srcloc? syntax?))) 'srcloc-syntax/c))

(struct colored-error-message (fragments additional-highlights) #:transparent)
(provide/contract [struct colored-error-message
                          ([fragments (listof msg-fragment?)]
                           [additional-highlights srcloc-syntax/c])]
                  [struct msg-fragment:str ([str string?])]
                  [struct msg-fragment:v ([v any/c])]
                  [struct colored-msg-fragment ([locs srcloc-syntax/c]
                                                [frags (listof (or/c msg-fragment:str? msg-fragment:v?))]
                                                [important boolean?]
                                                [color (or/c #f symbol?)])])

(define-values (prop:exn:colored-message
                exn:colored-message?
                exn:colored-message-accessor)
  (make-struct-type-property
   'colored-message
   (lambda (v str-info)
     (contract (exn? . -> . colored-error-message?) v
               'struct-definition 'color-error-accessor))))

(provide/contract [get-error-messages/color (exn? . -> . colored-error-message?)])
(define (get-error-messages/color exn)
  (cond [(exn:colored-message? exn) ((exn:colored-message-accessor exn) exn)]
        [(exn:srclocs? exn)
         (colored-error-message (list (msg-fragment:str (exn-message exn)))
                                ((exn:srclocs-accessor exn) exn))]
        [else
         (colored-error-message (list (msg-fragment:str (exn-message exn))) empty)]))

(require gmarceau/test gmarceau/cut gmarceau/list gmarceau/contract gmarceau/util parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (only-in srfi/1 break)
         unstable/function)




(define lex (lexer
             [(eof) empty]
             ["~|" (cons 'TildaPipe (lex input-port))]
             ["|~" (cons 'PipeTilda (lex input-port))]
             ["~v" (cons 'TildaV (lex input-port))]
             [(:or any-char "~~")
              (begin
                (define rst (lex input-port))
                (match rst
                  [(list (? string? str) tail ...)
                   (cons (string-append lexeme str) tail)]
                  [_ (cons lexeme rst)]))]))

(test 'lex (check-equal? (lex (open-input-string "~~foo ~| ~~| bar ~v|~ foo ~ "))
                         '("~~foo " TildaPipe " ~~| bar " TildaV PipeTilda " foo ~ ")))

(define (check-tildas-are-paired parsed)
  (let loop ([tildas (filter (// match? <> (or 'TildaPipe 'PipeTilda)) parsed)] [i 1])
    (match tildas
      [(list) (void)]
      [(list 'PipeTilda rst ...)
       (error 'colored-format "The ~ath tilda-pipe is an unexpected close" i)]
      [(list one) (error 'colored-format "There is an dangling ~a" one)]
      [(list 'TildaPipe 'TildaPipe rst ...)
       (error 'colored-format "The ~ath tilda-pipe is an unexpected open" (add1 i))]
      [(list 'TildaPipe 'PipeTilda rst ...)
       (loop rst (+ 2 i))])))

(test 'check-tildas-are-paired
      (define (go str) (check-tildas-are-paired (lex (open-input-string str))))
      (check-exn-msg exn:fail? "dangling" (lambda () (go "~| |~ ~|")))
      (check-exn-msg exn:fail? #rx"3.*close" (lambda () (go "~| |~ |~ |~")))
      (check-exn-msg exn:fail? #rx"6.*open" (lambda () (go "~| |~ ~| |~ ~| ~|")))
      (check-exn-msg exn:fail? #rx"2.*open" (lambda () (go "~| ~| |~ |~")))
      (check-exn-msg exn:fail? #rx"3.*close" (lambda () (go "~| ~~| |~ |~"))))

(define (count-tildas fmt)
  (- (length (regexp-match* #rx"~" fmt))
     (* 2 (length (regexp-match* #rx"~~" fmt)))))

(define (check-arg who args n)
  (when (< (length args) n)
    (error 'colored-error-message "Missing arguments for ~a" who)))

(define (colored-format:v args)
  (check-arg "~v" args 1)
  (values (msg-fragment:v (first args))
          (rest args)))

(define (colored-format:str fmt args)
  (define n (count-tildas fmt))
  (check-arg fmt args n)
  (values (msg-fragment:str (apply format fmt (take args n)))
          (drop args n)))

(define (colored-format:str-or-v fmt-or-v args)
  (if (eq? fmt-or-v 'TildaV)
      (colored-format:v args)
      (colored-format:str fmt-or-v args)))

(define (colored-format:TildaPipe fragments args)
  (check-arg "~|" args 1)
  (define-values (sub rest-args)
    (let loop ([fragments fragments] [args (rest args)])
      (if (empty? fragments)
          (values empty args)
          (let ()
            (define-values (f rest-args) (colored-format:str-or-v (first fragments) args))
            (define-values (rest-fs rest-rest-args) (loop (rest fragments) rest-args))
            (values (cons f rest-fs) rest-rest-args)))))
  (define the-arg (first args))
  (match the-arg
    [(list loc imp col other ..1)
     (error 'colored-format "Extraneous arguments to TildaPipe: ~a" other)]
    [_ (void)])
  
  (contract (or/c srcloc-syntax/c
                  (list/c srcloc-syntax/c boolean?)
                  (list/c srcloc-syntax/c symbol?)
                  (list/c srcloc-syntax/c boolean? symbol?)
                  (list/c srcloc-syntax/c symbol? boolean?))
            the-arg 'caller 'TildaPipe)
  
  (define is-important (and (list? the-arg) (findf (// eq? <> #t) the-arg)))
  (define color (and (list? the-arg) (findf symbol? the-arg)))
  (values (colored-msg-fragment (if (list? the-arg) (first the-arg) the-arg) sub is-important color) rest-args))

(provide/contract [colored-format (([fmt string?]) (#:additional-highlights [additional-highlights srcloc-syntax/c]) #:rest [_ any/c]
                                                   . ->i . [_ colored-error-message?])])
(define (colored-format fmt #:additional-highlights [additional-highlights empty] . args)
  (define parsed (lex (open-input-string fmt)))
  
  (check-tildas-are-paired parsed)
  
  (define fragments
    (let loop ([parsed parsed] [args args])
      (match parsed
        [(list) (unless (empty? args)
                  (error 'colored-format "There are ~a unused arguments" (length args)))
                empty]
        [(list 'TildaPipe tail ...)
         (define-values (left right) (break (// match? <> 'PipeTilda) tail))
         (define-values (result rest-args) (colored-format:TildaPipe left args))
         (cons result (loop (rest right) rest-args))]
        [(list f tail ...)
         (define-values (result rest-args) (colored-format:str-or-v f args))
         (cons result (loop tail rest-args))])))
  
  (colored-error-message fragments additional-highlights))

(test 'colored-format
      (check-equal? (colored-format "  ") (colored-error-message (list (msg-fragment:str "  ")) empty))
      (check-equal? (colored-format "--~a--" 5)
                    (colored-error-message (list (msg-fragment:str "--5--")) empty))
      (check-match (colored-format "~|--~a--|~ foo" #'here 5)
                   (colored-error-message (list (colored-msg-fragment (app syntax-e 'here) (list (msg-fragment:str "--5--")) #f #f) (msg-fragment:str " foo")) empty))
      
      (check-match (colored-format "~|--~a--~v--~a--|~ foo ~v bar ~||~ ~a" #'first 1 "inserted" 2 'inserted-too (list #'second 'red) 3 #:additional-highlights '(a b))
                   (colored-error-message (list (colored-msg-fragment
                                                 (app syntax-e 'first)
                                                 (list (msg-fragment:str "--1--")
                                                       (msg-fragment:v "inserted")
                                                       (msg-fragment:str "--2--")) #f #f)
                                                (msg-fragment:str " foo ")
                                                (msg-fragment:v 'inserted-too)
                                                (msg-fragment:str " bar ")
                                                (colored-msg-fragment (app syntax-e 'second) empty #f 'red)
                                                (msg-fragment:str " 3"))
                                          '(a b)))
      (check-match (colored-format " ~~ ~a ~a ~a ~a ~||~~~|" 1 2 3 4 #'here)
                   (colored-error-message (list (msg-fragment:str " ~ 1 2 3 4 ")
                                                (colored-msg-fragment (app syntax-e 'here) empty #f #f)
                                                (msg-fragment:str "~|")) empty))
      (check-exn-msg exn:fail? #rx"2.*unused" (lambda () (colored-format " ~a " 1 2 3))))

(define (uncolor-message msg)
  (let loop ([f (colored-error-message-fragments msg)])
    (match f
      [(msg-fragment:str str) str]
      [(msg-fragment:v v) (format "~a" v)]
      [(colored-msg-fragment locs frags imp col)
       (string-append* (map loop frags))])))

(define (important-srclocs msg)
  (flatten
   (filter-map (// match <> [(colored-msg-fragment locs _ #t _) locs] [_ #f])
               (colored-error-message-fragments msg))
   (colored-error-message-additional-highlights msg)))

(struct exn:fail:colored:syntax exn:fail:syntax (colored-message)
        #:transparent
        #:property prop:exn:colored-message (lambda (v)
                                              (define vec (struct->vector v))
                                              (vector-ref vec (sub1 (vector-length vec)))))

(define (raise-colored-syntax-error fmt #:additional-highlights [additional-highlights empty] . args)
  (define formatted (apply colored-format fmt #:additional-highlights [additional-highlights empty] args))
  (raise (exn:fail:colored:syntax (uncolor-message formatted)
                                  (current-continuation-marks)
                                  (important-srclocs formatted)
                                  formatted)))




(test 'get-error-messages/color
      (check-equal? (get-error-messages/color (exn:fail:colored:syntax "ah!" (current-continuation-marks) (list #'here) (colored-error-message 1 2)))
                    (colored-error-message 1 2)))
