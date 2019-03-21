(module error-reporting "pre-base.rkt"

  ; module implements error reporting conforming to Racket error conventions

  (provide
   ;; structs
 
   error-report
   error-report?
           
   ; replaces error-field struct's constructor
   (rename-out [make-error-field error-field])

   ; predicate matching any error-field and ellipsis-field
   error-field?
   
   ; replaces ellipsis-field struct's constructor
   (rename-out [make-ellipsis-field ellipsis-field])
   ellipsis-field?
           
   ; use to indicate lack of provided value for an error-report's field
   ; since #f can be used in any of error-report's fields, need an alternative
   ; to indicate lack of valid values.
   absent
   absent?)

  (provide
   ;; procedures

   ; control how long detail can be printed on one line
   error-detail-print-width
   
   ; convert error-report to string for use as exn's message field
   error-report->string

   ; construct exn:fail:contract using an error-report
   exn:fail:contract/error-report

   ; commonly used error fields so provide for convenience
   expected-field
   given-field)

  ;;; -----------------------------------------------------------------------------------------
  ;;; implementation section
  
  (require "struct.rkt"
           "list.rkt")
  

  ; Control how long error details can be in error reporting output.
  (define error-detail-print-width
    (make-parameter 72
                    (lambda (v)
                      (if (exact-nonnegative-integer? v)
                          v
                          (raise-argument-error 'error-detail-print-width
                                                "exact-nonnegative-integer?"
                                                v)))))
  
  ; Assume that any/c and absent? are disjoint in rest of the module.
  (define absent (let ([private (let () (struct absent ()) (absent))]) (lambda () private)))
  (define (absent? v) (eq? (absent) v))
  
  ; Racket error reporting convention.
  ;
  ; <error-report> :-
  ; [<srcloc>:] [<name>:] <message>[;
  ;  <continued-message>] ...
  ;   [<error-field> | <ellipsis-field>]
  ;   ... 
  ;
  ; <error-field> :-
  ; <label>: <detail>
  ;  <detail>
  ;  ...
  ;
  ; <ellipsis-field> :-
  ; <label>...:
  ;  <detail>
  ;  ...

  ; struct error-field
  ; label : string/c
  ; details : (listof any/c)
  ; indent-all? : (or/c #f (not #f)) if #f and details is not empty then first detail
  ;                                 is printed on same line as label and all other details
  ;                                 are printed on separate lines and indented with respect
  ;                                 to the label line.
  ;                                 if not #f and details is not empty then all details
  ;                                 are printed on new lines and indented.
  ; print-mode (or/c '~a '~v) controls how detail is printed
  (struct error-field (label details indent-all? print-mode)
    #:transparent
    #:guard (lambda (label details indent-all print-mode struct-name)
              (unless (string? label)
                (raise-argument-error struct-name "string?" 0 label details indent-all print-mode))
              (unless (list? details)
                (raise-argument-error struct-name "(listof any/c)" 1 label details indent-all print-mode))
              (unless (or (eq? print-mode '~a) (eq? print-mode '~v))
                (raise-argument-error struct-name "(or/c '~a '~v)" 3 label details indent-all print-mode))
              (values label details indent-all print-mode)))

  ; ~v is the default print mode for the field detail but allow ~a to be specified
  ; instead if desired.
  ; indent-all? is #f by default.
  (define (make-error-field label #:indent-all? [indent-all? #f] #:print-mode [print-mode '~v] . details)
    (error-field label details indent-all? print-mode))
  
  ; struct ellipsis-field : error-field
  ; all details are always indented so indent-all? is #t by default
  (struct ellipsis-field error-field () #:transparent)

  (define (make-ellipsis-field label #:print-mode [print-mode '~v] . details)
    (ellipsis-field label details #t print-mode))

  ; struct error-report
  ; srcloc : (or/c srcloc? absent?)
  ; name : (or/c any/c absent?)
  ; message : (or/c any/c absent?)
  ; continued-messages : (or/c (listof any/c) absent?)
  ; fields : (or/c (listof error-field?) absent?)
  (struct error-report (srcloc name message continued-messages fields)
    #:transparent
    #:guard (lambda (srcloc name message continued-messages fields struct-name)
              (unless (or (absent? srcloc) (srcloc? srcloc))
                (raise-argument-error struct-name
                                      "(or/c srcloc? absent?)"
                                      0
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
                
              (unless (or (absent? continued-messages) (list? continued-messages))
                (raise-argument-error struct-name
                                      "(or/c (listof any/c) absent?)"
                                      3
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (unless (or (absent? fields) (and (list? fields) (andmap error-field? fields)))
                (raise-argument-error struct-name
                                      "(or/c (listof error-field?) absent?)"
                                      4
                                      srcloc
                                      name
                                      message
                                      continued-messages
                                      fields))
              (values srcloc name message continued-messages fields)))

  ; cms : (listof any/c)
  ; Format continued-messages for error reporting output.
  ; Strings get special handling. If a string has line break characters then each break
  ; character is replaced by another break character that is postfixed with appropriate
  ; amount of whitespace for <continued-message> grammar form.
  (define (continued-messages-format cms)
    (define cms-format-string " ~a")
    (foldl (lambda (m result)
             (define cm-format (cond
                                 [(string? m) (lambda (m) (format cms-format-string
                                                                  (regexp-replace* #rx"\n" m "\n ")))]
                                 [else (lambda (m) (format cms-format-string m))]))
             (if (string=? result "")
                 (cm-format m)
                 (string-append result "\n" (cm-format m))))
           ""
           cms))

  ; error-field-format : error-field? -> string?
  ; Format error-field for error reporting output.
  ; If first detail is too long, meaning its printed
  ; representation exceeds error-detail-print-width then it's
  ; moved to next line with rest of other details.
  (define (error-field-format ef)
    (define formatted-label (format "  ~a:" (error-field-label ef)))
    (define detail-format (if (eq? (error-field-print-mode ef) '~v)
                              (lambda (d) (format "~v" d))
                              (lambda (d) (format "~a" d))))
    (define details (error-field-details ef))
    (cond
      [(null? details) formatted-label]
      [(or (first-detail-too-long? ef) (error-field-indent-all? ef))
       (foldl (lambda (d result)
                (string-append result
                               "\n"
                               "   "
                               (detail-format d)))
              formatted-label
              details)]
      [else
       (foldl (lambda (d result)
                (string-append result
                               "\n"
                               "   "
                               (detail-format d)))
              (string-append formatted-label " " (detail-format (car details)))
              (cdr details))]))

  ; ellipsis-field-format : ellipsis-field? -> string?
  ; Format ellipsis-field for error reporting output.
  (define (ellipsis-field-format ef)
    (define formatted-label (format "  ~a...:" (error-field-label ef)))
    (define detail-format (if (eq? (error-field-print-mode ef) '~v)
                              (lambda (d) (format "~v" d))
                              (lambda (d) (format "~a" d))))
    (define details (error-field-details ef))
    (foldl (lambda (d result)
             (string-append result
                            "\n"
                            "   "
                            (detail-format d)))
           formatted-label
           details))

  ; first-detail-too-long? : any/c -> boolean?
  ; checks to see if error-field's first detail when printed for error output would exceed
  ; (error-detail-print-width)
  (define (first-detail-too-long? ef)
    (define details (error-field-details ef))
    (cond
      [(null? details) #f]
      [else
       (define first-detail (car details))
       (cond [(and (symbol? first-detail)
                   (> (string-length (symbol->string first-detail)) (error-detail-print-width)))
              #t]
             [(and (string? first-detail)
                   (> (string-length first-detail) (error-detail-print-width)))
              #t]
             [else #f])]))

  ; fields-format : (listof error-field?) -> string?
  (define (fields-format fs)    
    (foldl (lambda (f result)
             (define field-format (cond
                                    [(ellipsis-field? f) ellipsis-field-format]
                                    [(error-field? f) error-field-format]))
             (if (string=? result "")
                 (field-format f)
                 (string-append result "\n" (field-format f))))
           ""
           fs))

  ; exn:fail:contract/error-report : error-report? continuation-mark-set? -> exn:fail:contract?
  ; Make an exn:fail:contract using error-report as its message.
  (define (exn:fail:contract/error-report err-rpt cmarks)
    (exn:fail:contract (error-report->string err-rpt) cmarks))

  (define (error-report->string err-rpt)
    (unless (error-report? err-rpt) (raise-argument-error 'error-report->string "error-report?" err-rpt))
    
    (define srcloc (error-report-srcloc err-rpt))
    (define name (error-report-name err-rpt))
    (define message (error-report-message err-rpt))
    (define continued-messages (error-report-continued-messages err-rpt))
    (define fields (error-report-fields err-rpt))

    (define srcloc-string (if (absent? srcloc) "" (format "~a: " (srcloc->string srcloc))))
    (define name-string (if (absent? name) "" (format "~a: " name)))
    (define message-string (if (absent? message) "" (string-append (format "~a" message) (if (absent? continued-messages) "" ";"))))
    (define continued-messages-string (if (absent? continued-messages) "" (continued-messages-format continued-messages)))
    (define fields-string (if (absent? fields) "" (fields-format fields)))

    (define pieces (list (string-append srcloc-string name-string message-string)
                         continued-messages-string
                         fields-string))                  

    (foldl (lambda (p result)
             (cond
               [(string=? p "") result]
               [(string=? result "") p]     
               [else (string-append result "\n" p)]))
           ""
           pieces))

  ;; --------------------------------------------
  ;; commonly used fields
  
  ; Expected fields always format detail using '~a print-mode.
  (define (expected-field detail)
    (make-error-field "expected" detail #:print-mode '~a))

  (define (given-field detail)
    (make-error-field "given" detail))
  )