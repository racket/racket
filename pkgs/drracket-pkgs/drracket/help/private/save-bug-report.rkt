#lang racket/base

(require racket/match
         racket/contract
         racket/serialize
         racket/list
         framework/preferences)

(define bug-classes '(("software bug" "sw-bug")
                      ("documentation bug" "doc-bug")
                      ("change request" "change-request")
                      ("support" "support")))

(define (translate-class class)
  (cadr (assoc class bug-classes)))

(define bug-severities '("critical" "serious" "non-critical"))

(define (with-pref func #:rewrite? rewrite?)
  (define old (preferences:get 'drracket:unfinished-bug-reports))
  (define ans (func old))
  (if rewrite?
      (preferences:set 'drracket:unfinished-bug-reports ans)
      ans))

(define (discard-all-except f)
  (with-pref
   #:rewrite? #t
   (λ (exp)
     (filter (λ (saved-report) (f (saved-report-id saved-report)))
             exp))))

(define valid-keys (sort '(severity 
                           class 
                           subject
                           description
                           how-to-repeat)
                         symbol<?))

;; id : number
;; open? : boolean?
;; table : (listof (list sym string?))
;; (the above are only an upper bound on the constraints here; 
;;  see validate for more info)
(serializable-struct saved-report (id table) #:transparent)

(define (blank-bug-form id) 
  (saved-report id 
                (for/list ([key (in-list valid-keys)])
                  (list key
                        (case key
                          [(class) (car (list-ref bug-classes default-class))]
                          [(severity) (list-ref bug-severities default-severity)]
                          [else ""])))))

(define default-class 0)
(define default-severity 1)


;; valid? : any -> boolean?
;; returns #t if the saved-reports are well formed
(define (valid? saved-reports)
  (cond
    [(list? saved-reports)
     (and (for/and ([saved-report (in-list saved-reports)])
            (validate-single saved-report))
          (no-dups (map saved-report-id saved-reports)))]
    [else #f]))

(define (no-dups l) (equal? l (remove-duplicates l)))

(define (validate-single a-saved-report)
  (match a-saved-report
    [(struct saved-report ((? number?)
                           (list (list (? symbol? keys) vals) ...)))
     (and (equal? (sort keys symbol<?) valid-keys)
          (for/and ([key (in-list keys)]
                    [val (in-list vals)])
            (case key
              [(class) (member val (map car bug-classes))]
              [(severity) (member val bug-severities)]
              [else (string? val)])))]
    [else #f]))

(define (register-new-bug-id)
  (define ans #f)
  (with-pref
   #:rewrite? #t
   (λ (bug-reports)
     (define ids (map saved-report-id bug-reports))
     (define new-id
       (let loop ([i 0])
         (cond
           [(member i ids)
            (loop (+ i 1))]
           [else 
            i])))
     (set! ans (blank-bug-form new-id))
     (cons ans bug-reports)))
  ans)

;; title : label-string?
;; id : number?
(struct brinfo (title id) #:transparent)

(define (saved-bug-report-titles/ids)
  (with-pref
   #:rewrite? #f
   (λ (x) 
     (for/list ([a-saved-report (in-list x)])
       (define assoc-l (saved-report-table a-saved-report))
       (define subj-p (assoc 'subject assoc-l))
       (define title (cadr subj-p))
       (brinfo (if (string=? title "")
                   "<<no title>>"
                   (trim-to-200 (regexp-replace* #rx"&" title "&&"))) 
               (saved-report-id a-saved-report))))))

(define (trim-to-200 str)
  (cond
    [(<= (string-length str) 200)
     str]
    [else
     (define len (string-length str))
     (define spacer " ... ")
     (string-append
      (substring str 0 100)
      spacer
      (substring str (+ (- len 100) (string-length spacer)) len))]))

(define (lookup-bug-report id)
  (or (with-pref
       #:rewrite? #f
       (λ (exp)
         (ormap (λ (x) (and (equal? id (saved-report-id x)) x))
                exp)))
      (register-new-bug-id)))

(define (saved-report-lookup a-saved-report key)
  (cadr (assoc key (saved-report-table a-saved-report))))

(define (save-bug-report id
                         #:severity severity
                         #:class class
                         #:subject subject
                         #:description description
                         #:how-to-repeat how-to-repeat)
  (with-pref
   #:rewrite? #t
   (λ (reports)
     (cons
      (saved-report id (list (list 'severity severity)
                             (list 'class class)
                             (list 'subject subject)
                             (list 'description description)
                             (list 'how-to-repeat  how-to-repeat)))
      (filter (λ (saved-report) 
                (not (equal? id (saved-report-id saved-report))))
              reports)))))

  
(define (unsave-bug-report id)
  (with-pref
   #:rewrite? #t
   (λ (reports)
     (filter (λ (saved-report) (not (equal? id (saved-report-id saved-report))))
             reports))))

(preferences:set-default 'drracket:unfinished-bug-reports 
                         '()
                         valid?)
(preferences:set-un/marshall 'drracket:unfinished-bug-reports
                             serialize
                             (λ (x) 
                               (with-handlers ((exn:fail? (λ (exn) '())))
                                 (deserialize x))))

(provide bug-severities
         bug-classes
         translate-class
         (struct-out brinfo)
         saved-report?
         default-severity
         default-class)
(provide/contract
 [register-new-bug-id (-> saved-report?)]
 [lookup-bug-report (-> number? saved-report?)]
 [saved-report-lookup (-> saved-report? (apply or/c valid-keys) string?)]
 [saved-report-id (-> saved-report? number?)]
 [save-bug-report (-> number?
                      #:severity (apply or/c bug-severities)
                      #:class (apply or/c (map car bug-classes))
                      #:subject string?
                      #:description string?
                      #:how-to-repeat string?
                      void?)]
 [unsave-bug-report (-> number? void?)]
 [saved-bug-report-titles/ids (-> (listof brinfo?))]
 [discard-all-except (-> (-> number? boolean?) void?)])
