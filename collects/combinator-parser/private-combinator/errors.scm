(module errors scheme/base
  
  (require "structs.scm" "parser-sigs.ss")
  
  (require scheme/unit)
    
  (provide (all-defined-out))
  
  (define-unit error-formatting@
    (import error-format-parameters^ language-format-parameters^ out^)
    (export (rename error^ (public-fail-type->message fail-type->message)))
    
    ;public-fail-type->message : fail-type -> err
    (define (public-fail-type->message fail)
      (fail-type->message fail null))
    
    ;fail-type->message: fail-type (listof err) -> err
    (define (fail-type->message fail-type message-to-date)
      (let* ([name (fail-type-name fail-type)]
             [a (a/an name)]
             [msg (lambda (m) 
                    (make-err m
                              (if (and (list? (fail-type-src fail-type))
                                       (list? (car (fail-type-src fail-type))))
                                  (car (fail-type-src fail-type))
                                  (fail-type-src fail-type))))])
        #;(printf "fail-type->message ~a\n" fail-type)
        (cond
          [(terminal-fail? fail-type)
           (collapse-message
            (add-to-message
             (msg
              (case (terminal-fail-kind fail-type)
                [(end) (format "Expected to find ~a ~a, but ~a ended prematurely."
                               a name input-type)]
                [(wrong) (format "Expected to find ~a ~a, but instead found ~a."
                                 a name (input->output-name (terminal-fail-found fail-type)))]
                [(misscase) (format "Expected to find ~a ~a, found ~a which may be miscapitalized."
                                    a name (input->output-name (terminal-fail-found fail-type)))]
                [(misspell) (format "Expected to find ~a ~a, found ~a which may be misspelled."
                                    a name (input->output-name (terminal-fail-found fail-type)))]
                [(missclass) (format "Found ~a instead of ~a ~a, a ~a cannot be used as ~a ~a."
                                     (input->output-name (terminal-fail-found fail-type)) a name class-type a name)]))
             name #f message-to-date))]
          [(sequence-fail? fail-type)
           #;(printf "sequence-fail case: kind is ~a\n" (sequence-fail-kind fail-type))
           (let* ([curr-id (sequence-fail-id fail-type)]
                  [id-name 
                   (if curr-id (string-append name " " (sequence-fail-id fail-type)) name)]
                  [expected (sequence-fail-expected fail-type)]
                  [a2 (a/an expected)]
                  [show-sequence (sequence-fail-correct fail-type)])
             (case (sequence-fail-kind fail-type)
               [(end)
                (collapse-message
                 (add-to-message
                  (msg (format "Expected ~a to contain ~a ~a to complete the ~a. \nFound ~a before ~a ended."
                               input-type a2 expected id-name (format-seen show-sequence) input-type))
                  name curr-id message-to-date))]
               [(wrong)
                (collapse-message
                 (add-to-message
                  (msg
                   (let* ([poss-repeat ((sequence-fail-repeat? fail-type))]
                          [repeat? (and (res? poss-repeat) (res-a poss-repeat) (res-msg poss-repeat))])
                     (cond 
                       [repeat?
                        (format "Found a repitition of ~a; the required number are present. Expected ~a ~a next."
                                (sequence-fail-last-seen fail-type) a2 expected)]
                       [(null? show-sequence)
                        (format "Expected ~a ~a to begin this ~a, instead found ~a."
                                a2 expected id-name (input->output-name (sequence-fail-found fail-type)))]
                       [else
                        (format "Expected ~a ~a to continue this ~a. Instead, found ~a after ~a."
                                a2 expected id-name (input->output-name (sequence-fail-found fail-type))
                                (format-seen show-sequence))])))
                  name curr-id message-to-date))]
               [(misscase) 
                (collapse-message
                 (add-to-message
                  (msg (format "Expected to find ~a ~a to continue this ~a, found ~a which may be miscapitalized."
                               a2 expected id-name (input->output-name (sequence-fail-found fail-type))))
                  name curr-id message-to-date))]
               [(misspell) 
                (collapse-message
                 (add-to-message
                  (msg (format "Expected to find ~a ~a to continue this ~a, found ~a which may be misspelled."
                               a2 expected id-name (input->output-name (sequence-fail-found fail-type))))
                  name curr-id message-to-date))]
               [(missclass) 
                (collapse-message
                 (add-to-message
                  (msg (format "Found ~a instead of ~a ~a, a ~a cannot be used as ~a ~a."
                               (input->output-name (sequence-fail-found fail-type)) a2 expected class-type a2 expected))
                  name curr-id message-to-date))]
               [(sub-seq choice)
                (fail-type->message (sequence-fail-found fail-type)
                                    (add-to-message (msg (format "An error occurred in ~a.\n" id-name))
                                                    name (sequence-fail-id fail-type) message-to-date))]
               [(options)
                (let ([sorted-opts (sort (options-fail-opts (sequence-fail-found fail-type))
                                         (lambda (a b) (>= (fail-type-chance a) (fail-type-chance b))))])
                  (if (null? show-sequence)
                      (fail-type->message (sequence-fail-found fail-type) #;(car sorted-opts)
                                          (add-to-message (msg (format "This ~a did not begin as expected." id-name))
                                                          name (sequence-fail-id fail-type) message-to-date))
                      (fail-type->message (sequence-fail-found fail-type) #;(car sorted-opts)
                                          (add-to-message
                                           (msg (format "There is an error in this ~a after ~a, the program resembles a(n) ~a here.\n"
                                                        id-name (car (reverse show-sequence)) 
                                                        (fail-type-name (car sorted-opts))))
                                           name (sequence-fail-id fail-type) message-to-date))))]))]
          [(options-fail? fail-type)
           #;(printf "selecting for options on ~a\n" name)
           (let* ([winners (select-errors (options-fail-opts fail-type))]
                     [top-names (map fail-type-name winners)]
                     [non-dup-tops (remove-dups top-names name)]
                     [top-name (car top-names)])
                (cond 
                  [(and (> (length winners) 1) 
                        (> (length non-dup-tops) 1)
                        (> (length winners) max-choice-depth))
                   (collapse-message 
                    (add-to-message
                     (msg (format "An error occurred in this ~a. Program resembles these: ~a.\n"
                                  name (nice-list non-dup-tops)))
                     name #f message-to-date))]
                  [(and (> (length winners) 1)
                        (<= (length winners) max-choice-depth))
                   (let ([messages (map (lambda (f) (fail-type->message f null)) winners)])
                     (cond
                       [(identical-messages? messages)
                        (collapse-message
                         (add-to-message (car messages) name #f message-to-date))]
                       [else
                        (let ([msg (cond
                                     [(apply equal? (map err-src messages)) (lambda (m) (make-err m (err-src (car messages))))]
                                     [else msg])])
                          (collapse-message
                           (add-to-message 
                            (msg (format "An error occurred in the ~a. Possible errors were: \n ~a"
                                         name 
                                         (alternate-error-list (map err-msg messages))))
                            name #f message-to-date)))]))]
                  [else
                   (fail-type->message 
                    (car winners)
                    (add-to-message
                     (msg
                      (format "There is an error in this ~a~a.\n"
                              name 
                              (if (equal? top-name name) "" 
                                  (format ", program resembles ~a ~a" (a/an top-name) top-name))))
                     name  #f message-to-date))]))]
           [(choice-fail? fail-type)
            #;(printf "selecting for ~a\n message-to-date ~a\n" name message-to-date)
            (let* ([winners (select-errors (choice-fail-messages fail-type))]               
                   [top-names (map fail-type-name winners)]
                   [top-name (car top-names)]
                   [no-dup-names (remove-dups (choice-fail-names fail-type) name)])
              (cond 
                [(and (choice-fail-ended? fail-type)
                      (> (length winners) 1))
                 (collapse-message 
                  (add-to-message
                   (msg (format "Expected a ~a, possible options include ~a." name
                                (nice-list (first-n max-choice-depth no-dup-names))))
                   name #f message-to-date))]
                [(and (<= (choice-fail-options fail-type) max-choice-depth)
                      (> (length no-dup-names) 1)
                      (> (length winners) 1)
                      (equal? top-names no-dup-names))
                 (collapse-message 
                  (add-to-message
                   (msg (format "An error occurred in this ~a; expected ~a instead."
                                name (nice-list no-dup-names)))
                   name #f message-to-date))]
                [(and (<= (choice-fail-options fail-type) max-choice-depth)
                      (> (length no-dup-names) 1)
                      (> (length winners) 1))
                 (let ([messages (map (lambda (f) (fail-type->message f null)) winners)])
                   (cond
                     [(identical-messages? messages)
                      (collapse-message
                       (add-to-message (car messages) #f #f
                                       (add-to-message
                                        (msg (format "An error occurred in this ~a, expected ~a instead."
                                                     name (nice-list no-dup-names)))
                                        name #f message-to-date)))]
                     [else
                      (collapse-message 
                       (add-to-message
                        (msg (format "An error occurred in this ~a; expected ~a instead. Possible errors were:\n~a"
                                     name (nice-list no-dup-names) 
                                     (alternate-error-list (map err-msg messages))))
                        name #f message-to-date))]))]
                [(and (> (length no-dup-names) max-choice-depth)
                      (> (length winners) 1))
                 (collapse-message 
                  (add-to-message
                   (msg (format "An error occurred in this ~a. Possible options include ~a.\n"
                                name (nice-list 
                                      (first-n max-choice-depth no-dup-names))))
                   name #f message-to-date))]
                [else                 
                 (fail-type->message
                  (car winners)
                  (add-to-message
                   (msg (format "An error occurred in this ~a~a.~a\n"
                                name 
                                (if (equal? name top-name) "" (format ", it is possible you intended ~a ~a here"
                                                                      (a/an top-name) top-name))
                                (if show-options " To see all options click here." "")))
                   name #f message-to-date))]))])))
    
    (define (chance-used a) (* (fail-type-chance a) (fail-type-used a)))
    (define (chance-may-use a) (* (fail-type-chance a) (fail-type-may-use a)))
    (define (chance a) (fail-type-chance a))
    (define (composite a) 
      (/ (+ (chance-used a) (chance-may-use a) (chance a)) 3))
    
    (define (narrow-opts rank options)
      (get-ties (sort options (lambda (a b) (> (rank a) (rank b)))) rank))
    
    (define (select-errors opts-list)
      (let* ([composite-winners 
              (narrow-opts composite opts-list)]
             
             [chance-used-winners
              (narrow-opts chance-used composite-winners)]
             
             [chance-may-winners
              (narrow-opts chance-may-use chance-used-winners)]
             
             [winners (narrow-opts chance chance-may-winners)])
        #;(printf "all options: ~a\n" opts-list)
        #;(printf "~a ~a ~a ~a ~a\n"
                  (map fail-type-name opts-list)
                  (map fail-type-chance opts-list)
                  (map fail-type-used opts-list)
                  (map fail-type-may-use opts-list)
                  (map composite opts-list))
        #;(printf "composite round: ~a ~a \n"
                  (map fail-type-name composite-winners)
                  (map composite composite-winners))
        #;(printf "final sorting: ~a\n" (map fail-type-name winners))
        winners))
    
    (define (first-n n lst)
      (if (<= (length lst) n)
          lst
          (let loop ([count 0] [l lst])
            (cond
              [(>= count n) null]
              [else (cons (car l) (loop (add1 count) (cdr l)))]))))
    
    (define (get-ties lst evaluate)
      (if (> (length lst) 1)
          (letrec ([getter
                    (lambda (sub)
                      (cond
                        [(null? sub) null]
                        [(>= (- (evaluate (car lst)) (evaluate (car sub))) .0001) null]
                        [else (cons (car sub) (getter (cdr sub)))]))])
            (cons (car lst) (getter (cdr lst))))
          lst))
    
    (define (a/an next-string)
      (if (string? next-string)
          (if (member (substring next-string 0 1) `("a" "e" "i" "o" "u"))
              "an" "a")
          "a"))
    
    (define (format-seen l)
      (if (null? l)
          ""
          (string-append "'"
                         (car l)
                         (apply string-append
                                (map (lambda (i) (string-append " " i)) (cdr l)))
                         "'")))
    
    (define (nice-list l)
      (letrec ([formatter 
                (lambda (l)
                  (cond
                    [(null? l) ""]
                    [(null? (cdr l)) (string-append "or " (car l))]
                    [else (string-append (car l) ", " (formatter (cdr l)))]))])
        (cond
          [(null? l) (error 'internal-error "nice-list in combinator-parser/errors.scm received null list")]
          [(null? (cdr l)) (car l)]
          [(null? (cddr l)) (string-append (car l) " or " (cadr l))]
          [else (formatter l)])))
    
    (define (alternate-error-list l)
      (cond
        [(null? l) ""]
        [else
         (let ([msg (if (equal? #\newline (string-ref (car l) (sub1 (string-length (car l)))))
                        (substring (car l) 0 (sub1 (string-length (car l))))
                        (car l))])
           (string-append (format "~a~a\n" #\tab msg)
                          (alternate-error-list (cdr l))))]))
    
    (define (downcase string)
      (string-append (string-downcase (substring string 0 1))
                     (substring string 1 (string-length string))))
    
    (define (identical-messages? msgs)
      (andmap (lambda (err) (equal? (err-msg (car msgs))
                                    (err-msg err)))
              (cdr msgs)))
        
    (define (remove-dups l n)
      (cond
        [(null? l) null]
        [(equal? (car l) n)
         (remove-dups (cdr l) n)]
        [(member (car l) (cdr l))
         (remove-dups (cdr l) n)]
        [else (cons (car l) (remove-dups (cdr l) n))]))
    
    (define-struct ms (who id? say))
    
    ;add-to-message: err string bool (list err) -> (list err)
    (define (add-to-message msg name id? rest)
      (let ([next (make-ms name id? msg)]
            [curr-len (length rest)])
        (cond
          [(null? rest) (list next)]
          [(equal? (ms-who (car rest)) name) (cons next (cdr rest))]
          [(and id? (ms-id? (car rest)) (< curr-len max-depth)) (cons next rest)]
          [(and id? (ms-id? (car rest))) (cons next (first-n (sub1 max-depth) rest))]
          [id? (add-to-message msg name id? (cdr rest))]
          [(< (length rest) max-depth) (cons next rest)]
          [else (cons next (first-n (sub1 max-depth) rest))])))
    
    ;combine-message: (list ms) -> err
    (define (collapse-message messages)
      (let loop ([end-msg (ms-say (car messages))]
                 [messages (cdr messages)])
        (cond
          [(null? messages) end-msg]
          [else 
           (loop 
            (make-err (string-append (err-msg (ms-say (car messages)))
                                     (err-msg end-msg))
                      (err-src end-msg))
            (cdr messages))])))
    
    )
  )
