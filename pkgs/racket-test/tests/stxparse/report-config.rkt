#lang racket
(require rackunit
         syntax/parse
         syntax/parse/debug
         syntax/parse/define
         syntax/parse/report-config
         "setup.rkt")

(define config1
  (hasheq 'literal-to-what (lambda (v) '("id" "ids"))
          'literal-to-string (lambda (v)
                               (format "«~s»" (if (syntax? v)
                                                  (syntax-e v)
                                                  v)))
          'datum-to-what (lambda (v)
                             (cond
                               [(number? v)
                                '("number" "numbers")]
                               [(or (eq? v 'spike)
                                    (eq? v 'prickly))
                                '("cactus" "cacti")]
                               [else
                                "stuff"]))
          'datum-to-string (lambda (v)
                             (format "=~s" v))))

(check-equal? (report-configuration? config1) #t)
(check-equal? (report-configuration? (hasheq)) #f)
(check-equal? (report-configuration? 0) #f)

(tcerr "id"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [(~literal cons) #true]))
       #rx"expected the id «cons»")

(tcerr "ids"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [(~literal cons) #true]
           [(~literal list) #true]))
       #rx"expected one of these ids: «cons» or «list»")

(tcerr "number"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [11 #true]))
       #rx"expected the number =11")

(tcerr "cactus"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [(~datum spike) #true]))
       #rx"expected the cactus =spike")

(tcerr "stuff"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [#true #true]))
       #rx"expected the stuff =#t")

(tcerr "numbers"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [11 #true]
           [12 #true]
           [13 #true]))
       #rx" expected one of these numbers: =11, =12, or =13")

(tcerr "cacti"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [(~datum spike) #true]
           [(~datum prickly) #true]))
       #rx"expected one of these cacti: =spike or =prickly")

(tcerr "stuff (plural)"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [#true #true]
           [#false #true]
           ["apple" #true]))
       #rx"expected one of these stuff: =#t, =#f, or =\"apple\"")

(tcerr "number or cactus"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [11 #true]
           [(~datum spike) #true]))
       #rx"expected the number =11 or expected the cactus =spike")

(tcerr "numbers or cactus"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [11 #true]
           [12 #true]
           [(~datum spike) #true]))
       #rx"expected one of these numbers: =11 or =12 or expected the cactus =spike")

(tcerr "numbers or cactus or ids"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'no
           [11 #true]
           [12 #true]
           [(~datum spike) #true]
           [(~literal cons) #true]
           [(~literal list) #true]))
       #rx"expected one of these ids: «cons» or «list»; expected one of these numbers: =11 or =12; or expected the cactus =spike")

(tcerr "numbers or cactus or ids"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'(no)
           [(_ (~literal cons)) #true]))
       #rx"expected more terms starting with the id «cons»")

(tcerr "numbers or cactus or ids"
       (parameterize ([current-report-configuration config1])
         (syntax-parse #'(no)
           [(_ (~datum spike)) #true]))
       #rx"expected more terms starting with the cactus =spike")
