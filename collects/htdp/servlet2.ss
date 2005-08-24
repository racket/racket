(module servlet2 mzscheme
     #| TODO -----------------------------------------------------------------------
        buttons: multiple points of returns: continuation functions 
     |# 
     (require (lib "servlet-env.ss" "web-server")
              (lib "error.ss" "htdp")
              (lib "list.ss")
              (lib "etc.ss"))
     (provide (all-from (lib "servlet-env.ss" "web-server"))
              
              single-query        ; FormElement -> Answer
              
              queries             ; (listof FormElement) -> (listof Answer)
              echo-answers        ; (listof Answers) -> true
              
              form-query          ; Form -> Response
              echo-response       ; Response -> true
              extract/single      ; Symbol Response -> Answer
              extract             ; Symbol Response -> (listof Answer)
              
              inform              ; String String *-> true 
              inform/html         ; (listof Xexpr) -> true
              final-page          ; String String *-> true
                            
              ; Structures  --------------------------------------------------------------
              make-password
              (rename make-numeric make-number)
              make-yes-no
              (rename make-check make-boolean)
              make-radio
              ;   make-button
              
              form? 
              form-element?
              
              #| Data Definitions --------------------------------------------------------
           
           FormElement = (union String
                                (make-password String)
                                (make-number String)
                                (make-boolean String)
                                (make-yes-no String String String)
                                (make-radio String (cons String (listof String)))
                                (make-button String))
           
           FormItem    = (list Symbol FormElement)
           Form        = (cons FormItem (listof FormItem))
           
           Answer      = (union String Number Boolean)
           Response    = (listof (list Symbol Answer))
           |#
              )
     
     ;       ;;        
     ;;;;;           ;                    ;;;         ;;;;;                                   ;        
     ;   ;          ;                   ;  ;          ;  ;;                                  ;        
     ;   ;  ;;;;   ;;;;;  ;;;;          ;             ;   ;  ; ;;;   ;;;  ;;; ;;; ;;;     ;;;;   ;;;  
     ;   ;      ;   ;         ;          ;   ;        ;  ;;   ;     ;   ;  ;   ;    ;    ;   ;  ;   ; 
     ;   ;   ;;;;   ;      ;;;;         ;;; ;         ;;;;    ;     ;   ;  ;   ;    ;    ;   ;  ;;;;; 
     ;   ;  ;   ;   ;     ;   ;        ;   ;          ;       ;     ;   ;   ; ;     ;    ;   ;  ;     
     ;   ;  ;   ;   ;   ; ;   ;        ;   ;;         ;       ;     ;   ;   ;;;     ;    ;   ;  ;   ; 
     ;;;;;    ;;; ;   ;;;   ;;; ;        ;;;  ;       ;;;;    ;;;;    ;;;     ;    ;;;;;   ;;; ;  ;;;  
     
     
     #| Documentation ------------------------------------------------------------
  (define-checked (name pred_1? ... pred_n?))
  re-defines the constructor make-name so that it makes sure that its
  n arguments satisfy pred_1? ... pred_n?
  
  Note: 
  to maintain the invariant, also modify the mutators to check their arguments
  |#
     (define-syntax (define-checked stx)
       (syntax-case stx ()
         [(_ (name pred? ...))
          (let ([make-variable 
                 ; String SyntaxObject -> SyntaxObject
                 (lambda (x y)
                   (datum->syntax-object
                    stx
                    (string->symbol 
                     (format "~a-~a" x (syntax-object->datum y)))))])
            (with-syntax ([make-name   (make-variable "make" (syntax name))]
                          [(x ...) (generate-temporaries (syntax (pred? ...)))])
              (syntax
               (set! make-name 
                     (let ([make-name make-name])
                       (lambda (x ...)
                         (check-arg 'make-name (pred? x) pred? "an" x)
                         ...
                         (make-name x ...)))))))]))
     ; _ -> true
     (define (true? x) #t)
     
     ;; Structure Definitions ----------------------------------------------------
     (define-struct fe (question))
     
     (define-struct (password fe)())
     (define-struct (numeric fe) ())
     (define-struct (check fe)   ())
     (define-struct (yes-no fe)  (positive negative))
     (define-struct (radio fe)   (labels))
     
     (define-checked (password string?))
     (define-checked (numeric string?))
     (define-checked (check string?))
     (define (list-of-strings? l)
       (and (list? l) (andmap string? l)))
     (define-checked (radio string? list-of-strings?))
     (define-checked (yes-no string? true? true?))
     
     ; todo
     (define-struct (button fe)  ())
     
     
     ; _ -> (union true String) 
     (define (form? x)
       (cond
         [(not (list? x)) (format "list expected, given ~e" x)]
         [(find-non list? x)
          => 
          (lambda (non-list)
            (format "list of lists expected, give list with ~e" non-list))]
         [(find-non 
           (lambda (x)
             (and (list? x)
                  (= (length x) 2)
                  (symbol? (car x))
                  (form-element? (cadr x))))
           x)
          => 
          (lambda (non-tagged-fe)
            (format "list of (list Symbol FormElement) expected, given ~s" non-tagged-fe))]
         [else true]))
     
     ; _ -> Boolean 
     (define (form-element? x)
       (cond
         [(string? x) #t]
         [(fe? x) (and
                   (string? (fe-question x))
                   (cond
                     [(radio? x)  (and (non-empty-list? (radio-labels x))
                                       (andmap string? (radio-labels x)))]
                     [(yes-no? x) (and (string? (yes-no-positive x)) 
                                       (string? (yes-no-negative x)))]
                     [else #t]))]
         [else #f]))
     
     ; _ -> Boolean 
     (define (non-empty-list? x)
       (and (cons? x) (list? x)))
     
     ;                 
     ;;   ;;                            
     ;; ;;                             
     ;; ;;  ;;;;   ;;;   ; ;;;    ;;;  
     ; ; ;      ;    ;    ;;  ;  ;   ; 
     ; ; ;   ;;;;    ;    ;   ;   ;;;  
     ; ; ;  ;   ;    ;    ;   ;      ; 
     ;   ;  ;   ;    ;    ;   ;  ;   ; 
     ;;; ;;;  ;;; ; ;;;;; ;;;  ;;  ;;;  
     
     
     ; posing questions ----------------------------------------------------------
     
     ; FormElement -> Answer
     ; to pose one question, receive one answer 
     (define (single-query fe)
       (check-arg 'single-query (form-element? fe) "form element" "first" fe)
       (car (queries (list fe))))
     
     ; (listof FormElement) -> (listof Answer)
     ; to ask N questions and to get N answers
     ; assert: (lambda (result) (= (length fes) (length result)))
     (define (queries fes)
       (check-arg 'queries (and (list? fes) (andmap form-element? fes))
                  "list of form elements" "first" fes)
       (conduct-query "Web Query" (map list (make-keys fes) fes)))
     
     ; Form -> Bindings 
     ; to ask N questions with tags, receive N answers 
     ; assert: (lambda (result) (set-equal? (map car aloss) (map car result)))
     (define (form-query f)
       (check-list-list 'form-query (form? f) "form" f)
       (map list (map first f)
            (conduct-query "Web Query" (map list (make-keys f) (map second f)))))
     
     
     ; extracting values from forms ----------------------------------------------
     
     ; extract : Symbol Response -> (listof Answer)
     ; extract all answers associated with a tag 
     (define (extract tag r)
       (map second (filter (lambda (a) (eq? (first a) tag)) r)))
     
     ; extract/single : Symbol Response -> Answer
     (define (extract/single tag r)
       (let ([all (extract tag r)])
         (if (and (pair? all) (null? (rest all))) ; (= (length all) 1)
             (first all)
             (cond
               [(null? all) 
                (error 'extract/single "~e contains no tag ~e" r tag)]
               [else 
                (error 'extract/single "~e contains more than one tag ~e" r tag)]))))
     
     ; echoing responses ---------------------------------------------------------
     
     ; Response -> true
     ; to display a response on a web page 
     (define (echo-response form)
       (make-echo-page
        (map (lambda (tag answer)
               `(tr (td ,(symbol->string tag))
                    (td ,(answer->string answer))))
             (map first form)
             (map second form))))
     
     ; (listof Answer) -> true
     ; to display a list of answers on a web page
     (define (echo-answers form)
       (make-echo-page 
        (map (lambda (answer) `(tr (td ,(answer->string answer)))) form)))
     
     ; displaying information ----------------------------------------------------
     
     ; String String *-> true
     ; to deliver an intermediate message and a link to continue
     (define (inform title . paragraph)
       (check-arg 'inform (string? title) "string" "first" title)
       (check-arg 'inform (andmap string? paragraph) 
                  "list of strings" "second, third, ..." paragraph)
       (send/suspend 
        (lambda (url)
          `(html 
            (title ,title)
            (body ([bgcolor "white"])
                  (h3 ,title)
                  (br)
                  (p ,@paragraph)
                  (br)
                  (a ([href ,url]) "Continue")))))
       #t)
     
     ; (listof Xexpr) -> true 
     (define (inform/html stuff)
       (send/suspend 
        (lambda (url)
          `(html 
            (title "Information")
            (body ([bgcolor "white"])
                  (hr)
                  (div ,@stuff)
                  (hr)
                  (a ([href ,url]) "Continue")))))
       #t)
     
     ; String String *-> true
     ; to deliver a final web page and terminate the web dialog
     (define (final-page title . paragraph)
       (check-arg 'final-page (string? title) "string" "first" title)
       (check-arg 'final-page (andmap string? paragraph) 
                  "list of strings" "second, third, ..." paragraph)
       (send/finish
        `(html 
          (title ,title)
          (body ([bgcolor "white"])
                (h3 ,title)
                (br)
                (p ,@paragraph))))
       #t)
     
     
     ;    ;;;      ;                    ;                 
     ;;;                           ;                                             
     ;                           ;                                             
     ; ;  ;;  ;; ;;; ;;; ;;;      ;    ;;;    ;;;;   ; ;;;  ;;;     ;;;    ;;;  
     ; ;   ;   ;   ; ;     ;      ;      ;        ;   ;       ;    ;   ;  ;   ; 
     ;;;;;  ;   ;    ;      ;      ;      ;     ;;;;   ;       ;    ;;;;;   ;;;  
     ;   ;  ;   ;   ; ;     ;      ;      ;    ;   ;   ;       ;    ;          ; 
     ;   ;  ;   ;  ;   ;    ;      ;      ;    ;   ;   ;       ;    ;   ;  ;   ; 
     ;;; ;;;  ;;; ;;;   ;; ;;;;;  ;;;;;; ;;;;;   ;;; ; ;;;;   ;;;;;   ;;;    ;;;  
     
     ; String Form -> FormResults
     (define (conduct-query text aloss)
       (let ([keys (map car aloss)])
         (let cq ([text text])
           (let ([res (build-form text aloss)])
             (let/ec restart 
               (map (lambda (k fe)
                      (get-binding restart (string->symbol k) res (cadr fe) cq))
                    keys aloss))))))
     
     ; String (String -> (listof Answer)) -> (listof Answer)
     (define (handle message cq n)
       (cq `(font ([color "red"]) ,(string-append message " " (fe-question n)))))
     
     ; Continuation Symbol Bindings FormElement -> Answer
     ; effect:  raise 'yes-no if a yes-no button goes unchecked 
     (define (get-binding restart tag bindings fe cq)
       (let ([cq       (compose restart cq)]
             [result   (extract-bindings tag bindings)]
             [question "Please respond to the question:"])
         (cond
           [(check? fe) (if (null? result) #f #t)]
           [(numeric? fe) 
            (if (null? result)
                (handle question cq fe)
                (let ([r (string->number (car result))]
                      [question  "Please respond with a number to the question:"])
                  (if r r (handle question cq fe))))]
           [(yes-no? fe)
            (if (null? result) (handle question cq fe) (car result))]
           [(radio? fe)
            (if (null? result) (handle question cq fe) (car result))]
           [(button? fe) ; at most one button should be clicked
            (if (null? result) #f (car result))]
           [(null? result) (format "error ~e -> ~e :: ~e" tag fe bindings)]
           [else (car result)])))
     
     ; String Form -> FormResults
     ; assert: (lambda (result) (set-equal? (domain aloss) (domain result)))
     (define (build-form title f)
       (request-bindings
        (send/suspend
         (lambda (url) 
           `(html 
             (title ,title)
             (body ([bgcolor "white"])
                   (h3 ,title)
                   (br)
                   (form ([action ,url][method "post"])                        
                         (table ,@(map build-row f))
                         ,@(add-submit-button (map second f)))))))))
     
     ; build-row : (list Symbol FormElement) -> Xexpr[tr]
     (define (build-row x)
       (let* ([tag (first x)]
              [fe  (second x)]
              [rad (lambda (x)
                     `(td (input ([type "radio"][name ,tag][value ,x])) " " ,x))]
              [make-radio
               (lambda (loq) `(td (table (tr ,@(map rad loq)))))])
         (cond
           [(string? fe) 
            `(tr (td ,fe) (td (input ([type "text"][name ,tag][value ""]))))]
           [(password? fe)
            `(tr (td ,(fe-question fe)) 
                 (td (input ([type "password"][name ,tag]))))]
           [(numeric? fe)
            `(tr (td ,(fe-question fe)) 
                 (td (input ([type "text"][name ,tag]))))]
           [(check? fe)
            `(tr (td ,(fe-question fe))
                 (td (input ([type "checkbox"][name ,tag][value ,(fe-question fe)]))))]
           [(yes-no? fe) 
            `(tr (td ,(fe-question fe))
                 ,(make-radio (list (yes-no-positive fe) (yes-no-negative fe))))]
           [(radio? fe)
            `(tr (td ,(fe-question fe)) ,(make-radio (radio-labels fe)))]
           [(button? fe)
            `(tr (td) 
                 (td (input ([type "submit"][name ,tag][value ,(fe-question fe)]))))]
           [else (error 'build-row "can't happen: ~e" fe)])))
     
     ; (listof Forms) -> (union Empty (list SUBMIT-BUTTON))
     (define (add-submit-button fes)
       (if (pair? (cdr fes))
           (if (ormap button? fes) '() (list SUBMIT-BUTTON))
           (let ([fe (car fes)])
             (if (or (string? fe) (password? fe) (numeric? fe))
                 '()
                 (list SUBMIT-BUTTON)))))
     
     ;;                   ;;            
     ;;;   ;                    ;            
     ;   ;  ;                    ;            
     ;   ;  ; ;;    ;;;    ;;;   ;  ;;   ;;;  
     ;      ;;  ;  ;   ;  ;   ;  ; ;    ;   ; 
     ;      ;   ;  ;;;;;  ;      ;;      ;;;  
     ;   ;  ;   ;  ;      ;      ; ;        ; 
     ;   ;  ;   ;  ;   ;  ;   ;  ;  ;   ;   ; 
     ;;;  ;;; ;;;  ;;;    ;;;  ;;   ;;  ;;;  
     
     
     ; Answer -> String 
     (define (answer->string a)
       (cond
         [(string? a)  (format "~s" a)]
         [(number? a)  (number->string a)]
         [(boolean? a) (if a "true" "false")]
         [else (format "~a" a)]))
     
     ; ---------------------------------------------------------------------------
     ; (listof X) -> (listof String)
     ; make unique, readable string/tags for a list of queries 
     (define make-keys
       (let ([counter 0])
         (lambda (fes)
           (map (lambda (x) 
                  (set! counter (+ counter 1))
                  (format "tag~a" counter))
                fes))))
     
     ; ---------------------------------------------------------------------------
     ; (listof Xexpr[tr]) -> true 
     ; echo stuff, wait for click on continue link
     (define (make-echo-page stuff)
       (send/suspend
        (lambda (url)
          `(html
            (title "Echoed Answers")
            (body ([bgcolor "white"])
                  (br)
                  (table 
                   ,@stuff)
                  (br)
                  (a ([href ,url]) "Continue")))))
       #t)
     
     ; constants -----------------------------------------------------------------
     (define SUBMIT-BUTTON '(input ([type "submit"][value "Submit"])))
     )   