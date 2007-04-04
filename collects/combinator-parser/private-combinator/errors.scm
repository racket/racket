(module errors mzscheme
  
  (require "structs.scm" "parser-sigs.ss")
  
  (require (lib "force.ss" "lazy")
           (lib "etc.ss")
           (lib "unit.ss")
           (lib "list.ss"))
  
  (provide (all-defined))
  
  (define-unit error-formatting@
    (import error-format-parameters^ language-format-parameters^ out^)
    (export (rename error^ (public-fail-type->message fail-type->message)))
    
    ;public-fail-type->message : fail-type -> err
    (define (public-fail-type->message fail)
      (fail-type->message fail null))
    
    ;fail-type->message: fail-type (listof err) -> err
    (define (fail-type->message fail-type message-to-date)
      (let* ([fail-type (!!!-fail fail-type)]
             [input->output-name (!!! input->output-name)]
             [name (fail-type-name fail-type)]
             [a (a/an name)]
             [msg (lambda (m) (make-err m (fail-type-src fail-type)))])
        #;(printf "fail-type->message ~a~n" fail-type)
        (cond
          [(terminal-fail? fail-type)
           (combine-message
            (msg
             (case (terminal-fail-kind fail-type)
               [(end) (format "Expected to find ~a ~a, but ~a ended prematurely."
                              a name input-type)]
               [(wrong) (format "Expected to find ~a ~a, but instead found ~a, which is illegal here."
                                a name (input->output-name (terminal-fail-found fail-type)))]
               [(misscase) (format "Expected to find ~a ~a, found ~a which may be miscapitalized."
                                   a name (input->output-name (terminal-fail-found fail-type)))]
               [(misspell) (format "Expected to find ~a ~a, found ~a which seems to be misspelled."
                                   a name (input->output-name (terminal-fail-found fail-type)))]
               [(missclass) (format "Found ~a instead of ~a ~a, a ~a cannot be used as ~a ~a."
                                    (input->output-name (terminal-fail-found fail-type)) a name class-type a name)]))
            message-to-date)]
          [(sequence-fail? fail-type)
           (let* ([id-name 
                   (if (sequence-fail-id fail-type)
                       (string-append name " " (sequence-fail-id fail-type)) name)]
                  [expected (sequence-fail-expected fail-type)]
                  [a2 (a/an expected)]
                  [show-sequence (sequence-fail-correct fail-type)])
             (case (sequence-fail-kind fail-type)
               [(end)
                (combine-message
                 (msg (format "Expected ~a to contain ~a ~a to complete the ~a. ~nFound ~a before ~a ended."
                              input-type a2 expected id-name (format-seen show-sequence) input-type)) message-to-date)]
               [(wrong)
                (combine-message
                 (msg
                  (cond 
                    [(sequence-fail-repeat? fail-type)
                     (format "Found a repitition of ~a; the required number are present. Expected ~a ~a next."
                             (sequence-fail-last-seen fail-type) a2 expected)]
                    [(null? show-sequence)
                     (format "Expected ~a ~a to begin this ~a, instead found ~a."
                             a2 expected id-name (input->output-name (sequence-fail-found fail-type)))]
                    [else
                     (format "Expected ~a ~a to continue this ~a. Instead, found ~a after ~a."
                             a2 expected id-name (input->output-name (sequence-fail-found fail-type))
                             (format-seen show-sequence))]))
                 message-to-date)]
               [(misscase) 
                (combine-message
                 (msg (format "Expected to find ~a ~a to continue this ~a, found ~a which may be miscapitalized."
                              a2 expected id-name (input->output-name (sequence-fail-found fail-type))))
                 message-to-date)]
               [(misspell) 
                (combine-message
                 (msg (format "Expected to find ~a ~a to continue this ~a, found ~a which seems to be misspelled."
                              a2 expected id-name (input->output-name (sequence-fail-found fail-type))))
                 message-to-date)]
               [(missclass) 
                (combine-message
                 (msg (format "Found ~a instead of ~a ~a, a ~a cannot be used as a(n) ~a."
                              (input->output-name (sequence-fail-found fail-type)) a2 expected class-type expected))
                 message-to-date)]
               [(sub-seq choice)
                (fail-type->message (sequence-fail-found fail-type)
                                    (add-to-message (msg (format "An error occured in ~a.~n" id-name)) 
                                                    name (sequence-fail-id fail-type) message-to-date))]
               [(options)
                (let ([sorted-opts (sort (options-fail-opts (sequence-fail-found fail-type))
                                         (lambda (a b) (>= (fail-type-chance a) (fail-type-chance b))))])
                  (fail-type->message (car sorted-opts)
                                      (add-to-message
                                       (msg (format "There is an error in this ~a after ~a, it is likely you intended a(n) ~a here.~n"
                                                    id-name (car (reverse show-sequence)) (fail-type-name (car sorted-opts))))
                                       name (sequence-fail-id fail-type) message-to-date)))]))]
          [(options-fail? fail-type)
           #;(printf "selecting for options on ~a~n" name)
              (let* ([winners (select-errors (options-fail-opts fail-type))]
                     [top-names (map fail-type-name winners)]
                     [non-dup-tops (remove-dups top-names name)]
                     [top-name (car top-names)])
                (fail-type->message 
                 (car winners)
                 (add-to-message
                  (msg
                   (cond
                     [(and (> (length winners) 1) (> (length non-dup-tops) 1))
                      (format "There is an error in this ~a. It is likely you intended one of ~a here.~n"
                              name (nice-list non-dup-tops))]
                     [else
                      (format "There is an error in this ~a~a.~n"
                              name 
                              (if (equal? top-name name) "" 
                                  (format ", it is likely you intended ~a ~a here" (a/an top-name) top-name)))]))
                  name  #f message-to-date)))]
           [(choice-fail? fail-type)
            #;(printf "selecting for ~a~n" name)
              (let* ([winners (select-errors (choice-fail-messages fail-type))]               
                     [top-names (map fail-type-name winners)]
                     [top-name (car top-names)]
                     [no-dup-names (remove-dups (choice-fail-names fail-type) name)])
                (fail-type->message
                 (car winners)
                 (add-to-message
                  (msg (cond
                         [(and (<= (choice-fail-options fail-type) max-choice-depth)
                               (> (length no-dup-names) 1)
                               (> (length winners) 1)
                               (equal? top-names no-dup-names))
                          (format "An error occured in this ~a, one of ~a is expected here."
                                  name (nice-list no-dup-names))]
                         [(and (<= (choice-fail-options fail-type) max-choice-depth)
                               (> (length no-dup-names) 1)
                               (> (length winners) 1))
                          (format "An error occured in this ~a, one of ~a is expected here. Input is close to one of ~a.~n"
                                  name (nice-list no-dup-names) (nice-list top-names))]
                         [(and (<= (choice-fail-options fail-type) max-choice-depth)
                               (> (length no-dup-names) 1))
                          (format "An error occured in this ~a, one of ~a is expected here. Current input is close to ~a.~a~n"
                                  name (nice-list no-dup-names) top-name
                                  (if show-options " To see all options click here." ""))] ;Add support for formatting and passing up all options
                         [else
                          (format "An error occured in this ~a~a.~a~n"
                                  name 
                                  (if (equal? name top-name) "" (format ", it is likely that you intended ~a ~a here"
                                                                        (a/an top-name) top-name))
                                  (if show-options " To see all options click here." ""))]))
                  name #f message-to-date)))])))
    
    (define (chance-used a) (* (fail-type-chance a) (fail-type-used a)))
    (define (chance-may-use a) (* (fail-type-chance a) (fail-type-may-use a)))
    (define (chance a) (fail-type-chance a))
    (define (composite a) 
      (/ (+ (chance-used a) (chance-may-use a) (chance a)) 3))
    
    (define (narrow-opts rank options)
      (get-ties (sort options (lambda (a b) (> (rank a) (rank b)))) rank))
    
    (define (select-errors opts-list)
      (let* ([composite-winners 
              (narrow-opts composite (!!list opts-list))]
             
             [chance-used-winners
              (narrow-opts chance-used composite-winners)]
             
             [chance-may-winners
              (narrow-opts chance-may-use chance-used-winners)]
             
             [winners (narrow-opts chance chance-may-winners)])
        #;(printf "all options: ~a~n" (!!list opts-list))
        #;(printf "~a ~a ~a ~a ~n"
                  (map fail-type-name (map !!! (!!list opts-list)))
                  (map !!! (map fail-type-chance (!!list opts-list)))
                  (map !!! (map fail-type-used (!!list opts-list)))
                  (map !!! (map fail-type-may-use (!!list opts-list))))
        #;(printf "composite round: ~a ~a ~n"
                  (map fail-type-name (map !!! composite-winners))
                  (map composite (map !!! composite-winners)))
        #;(printf "final sorting: ~a~n" (map fail-type-name (map !!! winners)))
        winners))
    
    (define (first-n n lst)
      (let loop ([count 0] [l lst])
        (cond
          [(= count n) null]
          [else (cons (car lst) (loop (add1 count) (cdr lst)))])))
    
    (define (get-ties lst evaluate)
      (letrec ([getter
                (lambda (sub)
                  (cond
                    [(null? sub) null]
                    [(>= (- (evaluate (car lst)) (evaluate (car sub))) .0001) null]
                    [else (cons (car sub) (getter (cdr sub)))]))])
        (cons (car lst) (getter (cdr lst)))))
    
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
                    [(null? (cdr l)) (string-append "or " (car l))]
                    [else (string-append (car l) ", " (formatter (cdr l)))]))])
        (cond
          [(null? l) (error 'internal-error "nice-list in combinator-parser/errors.scm recieved null list")]
          [(null? (cdr l)) (car l)]
          [(null? (cddr l)) (string-append (car l) " or " (cadr l))]
          [else (formatter l)])))
    
    (define (downcase string)
      (string-append (string-downcase (substring string 0 1))
                     (substring string 1 (string-length string))))
    
    (define (remove-dups l n)
      (cond
        [(null? l) null]
        [(equal? (car l) n)
         (remove-dups (cdr l) n)]
        [(member (car l) (cdr l))
         (remove-dups (cdr l) n)]
        [else (cons (car l) (remove-dups (cdr l) n))]))
    
    (define-struct ms (who id? say))
    
    ;add-to-message: err string (list err) -> (list err)
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
    
    ;combine-message: err (list ms) -> err
    (define (combine-message end-msg messages)
      (cond
        [(null? messages) end-msg]
        [else 
         (combine-message 
          (make-err (string-append (err-msg (ms-say (car messages)))
                                   (err-msg end-msg))
                    (err-src end-msg))
          (cdr messages))]))
    
    )
  )
