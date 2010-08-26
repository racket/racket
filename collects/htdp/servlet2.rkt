#lang scheme

#| TODO -----------------------------------------------------------------------
        buttons: multiple points of returns: continuation functions 
  |#
(require (prefix-in servlet: web-server/servlet-env)
         (prefix-in servlet: web-server/servlet)
         htdp/error
         scheme/tcp
         scheme/bool
         scheme/list
         scheme/match)

(require (for-syntax scheme/base))

(provide single-query ; FormElement -> Answer
         
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
         (rename-out (make-numeric make-number)
                     (make-check make-boolean))
         make-yes-no
         make-radio
         ;   make-button
         
         form? 
         form-element?
         
         ;; Advanced API from servlet-env:
         send/suspend
         send/finish
         (rename-out (servlet:extract-binding/single extract-binding/single)
                     (servlet:extract-bindings extract-bindings))
         
         
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
              (datum->syntax
               stx
               (string->symbol 
                (format "~a-~a" x (syntax->datum y)))))])
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
(define (internal:single-query fe)
  (check-arg 'single-query (form-element? fe) "form element" "first" fe)
  (car (internal:queries (list fe))))

; (listof FormElement) -> (listof Answer)
; to ask N questions and to get N answers
; assert: (lambda (result) (= (length fes) (length result)))
(define (internal:queries fes)
  (check-arg 'queries (and (list? fes) (andmap form-element? fes))
             "list of form elements" "first" fes)
  (conduct-query "Web Query" (map list (make-keys fes) fes)))

; Form -> Bindings 
; to ask N questions with tags, receive N answers 
; assert: (lambda (result) (set-equal? (map car aloss) (map car result)))
(define (internal:form-query f)
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
(define (internal:echo-response form)
  (make-echo-page
   (map (lambda (tag answer)
          `(tr (td ,(symbol->string tag))
               (td ,(answer->string answer))))
        (map first form)
        (map second form))))

; (listof Answer) -> true
; to display a list of answers on a web page
(define (internal:echo-answers form)
  (make-echo-page 
   (map (lambda (answer) `(tr (td ,(answer->string answer)))) form)))

; displaying information ----------------------------------------------------

; String String *-> true
; to deliver an intermediate message and a link to continue
(define (internal:inform title . paragraph)
  (check-arg 'inform (string? title) "string" "first" title)
  (check-arg 'inform (andmap string? paragraph) 
             "list of strings" "second, third, ..." paragraph)
  (servlet:send/suspend
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
(define (internal:inform/html stuff)
  (servlet:send/suspend
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
(define (internal:final-page title . paragraph)
  (check-arg 'final-page (string? title) "string" "first" title)
  (check-arg 'final-page (andmap string? paragraph) 
             "list of strings" "second, third, ..." paragraph)
  (servlet:send/finish
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
        [result (servlet:extract-bindings tag bindings)]
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
  (servlet:request-bindings
   (servlet:send/suspend
    (lambda (url) 
      `(html 
        (head (title ,title))
        (body ([bgcolor "white"])
              (h3 ,title)
              (br)
              (form ([action ,url])                       
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
       `(tr (td ,fe) (td (input ([type "text"][name ,tag][value ""]) " ")))]
      [(password? fe)
       `(tr (td ,(fe-question fe)) 
            (td (input ([type "password"][name ,tag]) " ")))]
      [(numeric? fe)
       `(tr (td ,(fe-question fe)) 
            (td (input ([type "text"][name ,tag]) " ")))]
      [(check? fe)
       `(tr (td ,(fe-question fe))
            (td (input ([type "checkbox"][name ,tag][value ,(fe-question fe)]) " ")))]
      [(yes-no? fe) 
       `(tr (td ,(fe-question fe))
            ,(make-radio (list (yes-no-positive fe) (yes-no-negative fe))))]
      [(radio? fe)
       `(tr (td ,(fe-question fe)) ,(make-radio (radio-labels fe)))]
      [(button? fe)
       `(tr (td) 
            (td (input ([type "submit"][name ,tag][value ,(fe-question fe)]) " ")))]
      [else (error 'build-row "can't happen: ~e" fe)])))

; (listof Forms) -> (union Empty (list SUBMIT-BUTTON))
(define (add-submit-button fes)
  ; XXX Commented
  #;(if (pair? (cdr fes))
        (if (ormap button? fes) '() (list SUBMIT-BUTTON))
        (let ([fe (car fes)])
          (if (or (string? fe) (password? fe) (numeric? fe))
              '()
              (list SUBMIT-BUTTON))))
  (list SUBMIT-BUTTON))

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
  (servlet:send/suspend
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




;                                            
;                                            
;                                            
;    ;;;;                                    
;   ;;   ;                                   
;   ;       ;;;    ; ;;  ;   ;   ;;;    ; ;; 
;   ;;     ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;    ;;;;  ;   ;   ;      ; ;   ;   ;   ;    
;       ;; ;;;;;   ;      ; ;   ;;;;;   ;    
;        ; ;       ;      ;;;   ;       ;    
;   ;   ;; ;;  ;   ;       ;    ;;  ;   ;    
;    ;;;;   ;;;    ;       ;     ;;;    ;    
;                                            
;                                            
;                                     ;    ;;


;; get-next-port: -> number
;; Returns the next port, starting from 8000, that's available.
;; If we can't get one after max-attempt tries, give up and raise
;; an error.
(define (get-next-port)
  (define starting-at 8000)
  (define max-attempts 32)
  (let loop ([port-no starting-at]
             [attempts 0])
    (let ([port-available?
           (with-handlers ([exn:fail:network?
                            (lambda (exn) #f)])
             (let ([listener (tcp-listen port-no 4 #t #f)])
               (tcp-close listener)
               #t))])
      (cond
        [port-available?
         port-no]
        [(< attempts max-attempts)
         (loop (add1 port-no) (add1 attempts))]
        [else
         (error 'get-next-port
                "Couldn't find an available port between ~a and ~a\n"
                starting-at (+ starting-at max-attempts))]))))

;; the current-server is a (make-parameter (or/c #f a-server))
;; where a-server is a server, defined below.
(define current-server (make-parameter #f))

;; A server consists of the maintenance thread, the request and response channels.
(define-struct server (th req-ch))

;; A request is either a
;;   (make-req:standard f args)
;; or a
;;   (make-req:final f args)
;; where f is a function that consumes args.
(define-struct req (res-ch func args) #:prefab)
(define-struct (req:standard req) () #:prefab)
(define-struct (req:final req) () #:prefab)

;; If an exception happens when evaluating a request, we
;; return an error response.  exn holds the exception that happened.
(define-struct error-res (exn))

;; ensure-standalone-server-running!: -> void
;; Initializes the current-server parameter.
;; Makes sure that the standalone server is up and running.
(define (ensure-standalone-server-running!)
  (unless (current-server)
    (local [(define req-ch (make-channel))
            (define (server-loop)
              (let ([msg (channel-get req-ch)])
                (match msg
                  [(struct req (res-ch func args))
                   (define res 
                     (with-handlers ([void make-error-res])
                       (apply func args)))
                   (channel-put res-ch res)
                   (unless (req:final? msg)
                     (server-loop))])))
            (define th
              (thread (lambda ()
                        (let ([port (get-next-port)]
                              [called? #f])
                          (parameterize ([read-accept-dot #t])
                            (servlet:serve/servlet 
                             (lambda (req)
                               (if (not called?)
                                   (begin (set! called? #t)
                                          (server-loop))
                                   "The servlet has executed already."))
                             #:banner? #f
                             #:port port))))))
            (define a-server (make-server th req-ch))]
      (current-server a-server))))

;; Each of the wrapped functions turns function calls into requests
;; to the server.
(define (make-wrapped-function internal:function make-req)
  (lambda args
    (ensure-standalone-server-running!)
    (match (current-server)
      [(struct server (th req-ch))
       (thread-resume th)
       (let* ([res-ch (make-channel)]
              [req (make-req res-ch internal:function args)])
         (channel-put req-ch req)
         (let ([result (channel-get res-ch)])
           (cond
             [(error-res? result)
              (raise (error-res-exn result))]
             [else
              result])))])))

;; Provides wrappers around the internal functions to make the wrapped functions
;; run in the context of a standalone server.
(define-syntax (define-wrapped-function stx)
  (syntax-case stx ()
    [(_ function internal:function #:final final)
     (with-syntax ([make-request (if (syntax->datum #'final)
                                     #'make-req:final
                                     #'make-req:standard)])
       (syntax/loc stx 
         (define function (make-wrapped-function internal:function make-request))))]
    
    [(_ function internal:function)
     (syntax/loc stx
       (define-wrapped-function function internal:function #:final #f))]))


(define-wrapped-function single-query internal:single-query)
(define-wrapped-function queries internal:queries)
(define-wrapped-function echo-answers internal:echo-answers)

(define-wrapped-function form-query internal:form-query)
(define-wrapped-function echo-response internal:echo-response)

(define-wrapped-function inform/html internal:inform/html)
(define-wrapped-function inform internal:inform)

;; fixme: on a final-page, send/finish doesn't seem to return control.
(define-wrapped-function final-page internal:final-page #:final #t)

(define-wrapped-function send/suspend servlet:send/suspend)
(define-wrapped-function send/finish servlet:send/finish #:final #t)
