; (match-performance-test
;  (file
;   (name "")
;   write-new
;   | add-results)
; (tag
;    identifier
;  | date
;  | time)
; (display
;    positive-change
;  | negative-change
;  | change
;  | percent-change
;  | rt-positive-change
;  | rt-negative-change
;  | rt-change
;  | whole-list
;  | last-two)
; (patterns
;  (pattern clauses ...)
;  ...)
; )

; Data

; ((pattern ((date count time) ...))
;  ...)
(define-syntax (match-performance-test stx)
  (syntax-case stx (file patterns display tag)
    ((_ (file (name file-name) params ...)
        (tag tag-type)
        (display dis-type)
        (patterns pats ...))
                                        ; decypher params
     (let* ((prms (syntax-object->datum (syntax (params ...))))
            (write-new (member
                        'write-new prms))
            (read-old-table
             (lambda ()
               (if write-new
                   #`'()
                   #`(with-input-from-file file-name
                       (lambda ()
                         (read))))))
            (add-results (member
                          'add-results prms))
            (write-new-table
             (lambda ()
               (if (or write-new add-results)
                   #`(call-with-output-file file-name
                       (lambda (port)
                         (pretty-print old-table port))
                       'replace)
                   #`())))
            (disp-gen-wrap
             (lambda (pattern stx)
               (if (not write-new)
                   #`(begin
                       (update-table (quote #,pattern) new-res)
                       (let* ((whole-list (get-entry (quote #,pattern)))
                              (last-two (get-last-two (quote #,pattern)))
                              (prev-count (get-count (car last-two)))
                              (cur-count (get-count (cadr last-two)))
                              (rt-prev-count (get-rt (car last-two)))
                              (rt-cur-count (get-rt (cadr last-two)))
                              )
                         #,stx))
                   #`(begin
                       (update-table (quote #,pattern) new-res)
                       (let* ((whole-list (get-entry (quote #,pattern))))
                         #,stx)))))
            (disp-gen
             (if write-new
                 (lambda (pattern)
                  (disp-gen-wrap
                   pattern
                   #'whole-list))
                 (syntax-case
                     (syntax dis-type)
                     (positive-change
                      negative-change
                      change
                      rt-positive-change
                      rt-negative-change
                      rt-change
                      whole-list
                      last-two
                      percent-change
                      rt-percent-change
                      )
                   (positive-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(if (< prev-count cur-count)
                             `(+ ,(- cur-count prev-count))
                             #t))))
                   (negative-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(if (> prev-count cur-count)
                             `(- ,(- prev-count cur-count))
                             #t))))
                   (change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(cond ((= prev-count cur-count)
                                #t)
                               ((> prev-count cur-count)
                                `(- ,(- prev-count cur-count)))
                               (else `(+ ,(- cur-count prev-count)))))))
                   (percent-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(cond ((= prev-count cur-count)
                                #t)
                               ((> prev-count cur-count)
                                `(-% ,(percent-of cur-count prev-count)))
                               (else `(+% ,(percent-of cur-count prev-count)))))))
                   (rt-percent-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(cond ((= rt-prev-count rt-cur-count)
                                #t)
                               ((> rt-prev-count rt-cur-count)
                            `(-% ,(percent-of rt-cur-count rt-prev-count)))
                               (else `(+% ,(percent-of rt-cur-count rt-prev-count)))))))
                   (rt-positive-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(if (< rt-prev-count rt-cur-count)
                             `(+ ,(- rt-cur-count rt-prev-count))
                             #t))))
                   (rt-negative-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(if (> rt-prev-count rt-cur-count)
                             `(- ,(- rt-prev-count rt-cur-count))
                             #t))))
                   (rt-change
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #`(cond ((= rt-prev-count rt-cur-count)
                                #t)
                               ((> rt-prev-count rt-cur-count)
                                `(- ,(- rt-prev-count rt-cur-count)))
                           (else `(+ ,(- rt-cur-count rt-prev-count)))))))
                   (whole-list
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #'whole-list)))
                   (last-two
                    (lambda (pattern)
                      (disp-gen-wrap
                       pattern
                       #'last-two))))))
            (test-gen
             (lambda (patt)
               (syntax-case patt (pattern)
                 ((pattern clauses ...)
                  #`(test #t
                        #,(quasisyntax/loc patt  (lambda ()
                            (let ((new-res
                                   (match (list 
                                           (match-test
                                            #,@(syntax ((clauses #t) ...)))
                                          ;  (match-test
;                                             #,@(syntax ((clauses #t) ...)))
;                                            (match-test
;                                             #,@(syntax ((clauses #t) ...)))
;                                            (match-test
;                                             #,@(syntax ((clauses #t) ...)))
;                                            (match-test
;                                             #,@(syntax ((clauses #t) ...)))
                                           ;(match-test
                                           ; #,@(syntax ((clauses #t) ...)))
                                           )
                                          ((list (list b c) ..1)
                                           (list (car b) (round (/ (apply + c) (length c))))))))
                              #,(disp-gen
                                 patt))))))
                 (_ (error 'problem-here)))))
            (handle-patterns
             (lambda (pattern-list)
               #`(begin
                   #,@(map test-gen (syntax->list pattern-list))))))
       #`(let* ((old-table #,(read-old-table)))
           (letrec ((update-table
                     (lambda (pattern new-res)
                       (set! old-table
                             (if (assoc pattern old-table)
                                 (map
                                  (lambda (x)
                                    (if (equal? (car x) pattern)
                                        (cons (car x)
                                              (list
                                               (append (cadr x)
                                                       (list (cons
                                                              (let ((dat-struct
                                                                     (seconds->date
                                                                      (current-seconds))))
                                                                #,(syntax-case (syntax tag-type) (date time) 
                                                                    (date
                                                                     #'(list (date-month dat-struct)
                                                                             (date-day dat-struct)
                                                                             (date-year dat-struct)))
                                                                    (time
                                                                     #'(list (date-hour dat-struct)
                                                                             (date-minute dat-struct)))
                                                                    (id
                                                                     (identifier? (syntax id))
                                                                     (syntax (quote id)))))
                                                              new-res)))))
                                        x))
                                  old-table)
                                 (cons
                                  (cons pattern
                                        (list
                                         (list
                                          (cons
                                           (let ((dat-struct
                                                  (seconds->date
                                                   (current-seconds))))
                                             #,(syntax-case (syntax tag-type) (date time) 
                                                 (date
                                                  #'(list (date-month dat-struct)
                                                          (date-day dat-struct)
                                                          (date-year dat-struct)))
                                                 (time
                                                  #'(list (date-hour dat-struct)
                                                          (date-minute dat-struct)))
                                                 (id
                                                  (identifier? (syntax id))
                                                  (syntax (quote id)))))
                                           new-res))))
                                  old-table)))))
                    (get-entry
                     (lambda (pattern)
                       (let ((entry (assoc pattern old-table)))
                         entry)))
                    (get-last-two
                     (lambda (pattern)
                       (letrec ((RC (lambda (l)
                                      (if (= 2 (length l))
                                          l
                                          (RC (cdr l))))))
                         (RC (cadr (get-entry pattern))))))
                    (get-rt
                     (lambda (x) (caddr x)))
                    (get-count
                     (lambda (x) (cadr x)))
                    (percent-of 
                      (lambda (x y) (* 100 (exact->inexact (/ (abs (- x y)) y)))))
                    )
             #,(handle-patterns (syntax (pats ...)))
             #,(write-new-table)))))))
