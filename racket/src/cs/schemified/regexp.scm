(export (rename (1/byte-pregexp byte-pregexp)
                (1/byte-pregexp? byte-pregexp?)
                (1/byte-regexp byte-regexp)
                (1/byte-regexp? byte-regexp?)
                (1/pregexp pregexp)
                (1/pregexp? pregexp?)
                (1/regexp regexp)
                (1/regexp-match regexp-match)
                (1/regexp-match-peek regexp-match-peek)
                (1/regexp-match-peek-immediate regexp-match-peek-immediate)
                (1/regexp-match-peek-positions regexp-match-peek-positions)
                (1/regexp-match-peek-positions-immediate
                 regexp-match-peek-positions-immediate)
                (1/regexp-match-peek-positions-immediate/end
                 regexp-match-peek-positions-immediate/end)
                (1/regexp-match-peek-positions/end
                 regexp-match-peek-positions/end)
                (1/regexp-match-positions regexp-match-positions)
                (1/regexp-match-positions/end regexp-match-positions/end)
                (1/regexp-match/end regexp-match/end)
                (1/regexp-match? regexp-match?)
                (1/regexp-max-lookbehind regexp-max-lookbehind)
                (regexp-place-init! regexp-place-init!)
                (1/regexp-replace regexp-replace)
                (1/regexp-replace* regexp-replace*)
                (1/regexp? regexp?)))
(define hash1688
  (hash
   '#vu8(97 108 110 117 109)
   '6
   '#vu8(97 108 112 104 97)
   '1
   '#vu8(97 115 99 105 105)
   '12
   '#vu8(98 108 97 110 107)
   '8
   '#vu8(99 110 116 114 108)
   '11
   '#vu8(100 105 103 105 116)
   '4
   '#vu8(103 114 97 112 104)
   '10
   '#vu8(108 111 119 101 114)
   '3
   '#vu8(112 114 105 110 116)
   '10
   '#vu8(115 112 97 99 101)
   '9
   '#vu8(117 112 112 101 114)
   '2
   '#vu8(119 111 114 100)
   '7
   '#vu8(120 100 105 103 105 116)
   '5))
(define hash2956
  (hash
   '#vu8(46)
   '39
   '#vu8(67)
   '38
   '#vu8(67 99)
   '33
   '#vu8(67 102)
   '34
   '#vu8(67 110)
   '36
   '#vu8(67 111)
   '37
   '#vu8(67 115)
   '35
   '#vu8(76)
   '7
   '#vu8(76 38)
   '5
   '#vu8(76 108)
   '1
   '#vu8(76 109)
   '4
   '#vu8(76 111)
   '6
   '#vu8(76 116)
   '3
   '#vu8(76 117)
   '2
   '#vu8(77)
   '23
   '#vu8(77 99)
   '21
   '#vu8(77 101)
   '22
   '#vu8(77 110)
   '20
   '#vu8(78)
   '11
   '#vu8(78 100)
   '8
   '#vu8(78 108)
   '9
   '#vu8(78 111)
   '10
   '#vu8(80)
   '19
   '#vu8(80 99)
   '16
   '#vu8(80 100)
   '17
   '#vu8(80 101)
   '13
   '#vu8(80 102)
   '15
   '#vu8(80 105)
   '14
   '#vu8(80 111)
   '18
   '#vu8(80 115)
   '12
   '#vu8(83)
   '28
   '#vu8(83 99)
   '24
   '#vu8(83 107)
   '25
   '#vu8(83 109)
   '26
   '#vu8(83 111)
   '27
   '#vu8(90)
   '32
   '#vu8(90 108)
   '29
   '#vu8(90 112)
   '30
   '#vu8(90 115)
   '31))
(define hash2589 (hasheqv))
(define-values
 (prop:keyword-impersonator keyword-impersonator? keyword-impersonator-ref)
 (make-struct-type-property 'keyword-impersonator))
(define keyword-procedure-impersonator-of
  (lambda (v_0)
    (if (keyword-impersonator? v_0)
      (|#%app| (keyword-impersonator-ref v_0) v_0)
      #f)))
(define-values
 (struct:keyword-procedure
  mk-kw-proc
  keyword-procedure?
  keyword-procedure-ref
  keyword-procedure-set!)
 (let ((app_0
        (list
         (cons prop:checked-procedure #t)
         (cons prop:impersonator-of keyword-procedure-impersonator-of))))
   (make-struct-type
    'keyword-procedure
    #f
    4
    0
    #f
    app_0
    (current-inspector)
    #f
    '(0 1 2 3))))
(define keyword-procedure-required
  (make-struct-field-accessor keyword-procedure-ref 2))
(define keyword-procedure-allowed
  (make-struct-field-accessor keyword-procedure-ref 3))
(define-values
 (prop:procedure-accessor procedure-accessor? procedure-accessor-ref)
 (make-struct-type-property
  'procedure
  (lambda (v_0 info-l_0)
    (if (exact-integer? v_0)
      (make-struct-field-accessor (list-ref info-l_0 3) v_0)
      #f))))
(define-values
 (new-prop:procedure new-procedure? new-procedure-ref)
 (make-struct-type-property
  'procedure
  #f
  (list (cons prop:procedure values) (cons prop:procedure-accessor values))
  #t))
(define procedure-keywords
  (lambda (p_0)
    (if (keyword-procedure? p_0)
      (let ((app_0 (keyword-procedure-required p_0)))
        (values app_0 (keyword-procedure-allowed p_0)))
      (if (procedure? p_0)
        (if (new-procedure? p_0)
          (let ((v_0 (new-procedure-ref p_0)))
            (if (procedure? v_0)
              (procedure-keywords v_0)
              (let ((a_0 (procedure-accessor-ref p_0)))
                (if a_0
                  (procedure-keywords (|#%app| a_0 p_0))
                  (values null null)))))
          (values null null))
        (raise-argument-error*
         'procedure-keywords
         'racket/primitive
         "procedure?"
         p_0)))))
(define reverse$1
  (|#%name|
   reverse
   (lambda (l_0)
     (begin
       (begin
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda (a_0 l_1)
               (begin
                 (if (null? l_1)
                   a_0
                   (let ((app_0 (cons (car l_1) a_0)))
                     (loop_0 app_0 (cdr l_1)))))))))
          (loop_0 null l_0)))))))
(define-values
 (prop:stream stream-via-prop? stream-ref)
 (make-struct-type-property
  'stream
  (lambda (v_0 si_0)
    (begin
      (if (if (vector? v_0)
            (if (= 3 (vector-length v_0))
              (if (procedure? (vector-ref v_0 0))
                (if (procedure-arity-includes? (vector-ref v_0 0) 1)
                  (if (procedure? (vector-ref v_0 1))
                    (if (procedure-arity-includes? (vector-ref v_0 1) 1)
                      (if (procedure? (vector-ref v_0 2))
                        (procedure-arity-includes? (vector-ref v_0 2) 1)
                        #f)
                      #f)
                    #f)
                  #f)
                #f)
              #f)
            #f)
        (void)
        (raise-argument-error
         'guard-for-prop:stream
         (string-append
          "(vector/c (procedure-arity-includes/c 1)\n"
          "          (procedure-arity-includes/c 1)\n"
          "          (procedure-arity-includes/c 1))")
         v_0))
      (vector->immutable-vector v_0)))
  '()
  #t))
(define-values
 (prop:gen-sequence sequence-via-prop? sequence-ref)
 (make-struct-type-property
  'sequence
  (lambda (v_0 si_0)
    (begin
      (if (if (procedure? v_0) (procedure-arity-includes? v_0 1) #f)
        (void)
        (raise-argument-error
         'guard-for-prop:sequence
         "(procedure-arity-includes/c 1)"
         v_0))
      v_0))))
(define-values
 (struct:range make-range range? range-ref range-set!)
 (make-struct-type
  'stream
  #f
  3
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0)
       (let ((cont?_0 (|#%app| range-ref v_0 2)))
         (if cont?_0 (not (|#%app| cont?_0 (|#%app| range-ref v_0 0))) #f)))
     (lambda (v_0) (|#%app| range-ref v_0 0))
     (lambda (v_0)
       (let ((app_0
              (let ((app_0 (|#%app| range-ref v_0 1)))
                (|#%app| app_0 (|#%app| range-ref v_0 0)))))
         (let ((app_1 (|#%app| range-ref v_0 1)))
           (make-range app_0 app_1 (|#%app| range-ref v_0 2)))))))
   (cons
    prop:gen-sequence
    (lambda (v_0)
      (let ((app_0 (|#%app| range-ref v_0 1)))
        (let ((app_1 (|#%app| range-ref v_0 0)))
          (values values #f app_0 app_1 (|#%app| range-ref v_0 2) #f #f))))))))
(define check-range$1
  (|#%name|
   check-range
   (lambda (a_0 b_0 step_0)
     (begin (check-range-generic 'in-range a_0 b_0 step_0)))))
(define check-range-generic
  (lambda (who_0 a_0 b_0 step_0)
    (begin
      (if (real? a_0) (void) (raise-argument-error who_0 "real?" a_0))
      (if (real? b_0) (void) (raise-argument-error who_0 "real?" b_0))
      (if (real? step_0) (void) (raise-argument-error who_0 "real?" step_0)))))
(define check-naturals
  (lambda (n_0)
    (if (if (integer? n_0) (if (exact? n_0) (>= n_0 0) #f) #f)
      (void)
      (raise-argument-error 'in-naturals "exact-nonnegative-integer?" n_0))))
(define-values
 (struct:list-stream
  make-list-stream
  list-stream?
  list-stream-ref
  list-stream-set!)
 (make-struct-type
  'stream
  #f
  1
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0) (not (pair? (|#%app| list-stream-ref v_0 0))))
     (lambda (v_0) (car (|#%app| list-stream-ref v_0 0)))
     (lambda (v_0) (make-list-stream (cdr (|#%app| list-stream-ref v_0 0))))))
   (cons
    prop:gen-sequence
    (lambda (v_0)
      (values car cdr values (|#%app| list-stream-ref v_0 0) pair? #f #f))))))
(define check-list
  (lambda (l_0)
    (if (list? l_0) (void) (raise-argument-error 'in-list "list?" l_0))))
(define check-in-hash-keys
  (lambda (ht_0)
    (if (hash? ht_0)
      (void)
      (raise-argument-error 'in-hash-keys "hash?" ht_0))))
(define check-ranges
  (lambda (who_0 type-name_0 vec_0 start_0 stop_0 step_0 len_0)
    (begin
      (if (exact-nonnegative-integer? start_0)
        (void)
        (raise-argument-error who_0 "exact-nonnegative-integer?" start_0))
      (if (let ((or-part_0 (< start_0 len_0)))
            (if or-part_0 or-part_0 (= len_0 start_0 stop_0)))
        (void)
        (raise-range-error
         who_0
         type-name_0
         "starting "
         start_0
         vec_0
         0
         (sub1 len_0)))
      (if (exact-integer? stop_0)
        (void)
        (raise-argument-error who_0 "exact-integer?" stop_0))
      (if (if (<= -1 stop_0) (<= stop_0 len_0) #f)
        (void)
        (raise-range-error
         who_0
         type-name_0
         "stopping "
         stop_0
         vec_0
         -1
         len_0))
      (if (if (exact-integer? step_0) (not (zero? step_0)) #f)
        (void)
        (raise-argument-error
         who_0
         "(and/c exact-integer? (not/c zero?))"
         step_0))
      (if (if (< start_0 stop_0) (< step_0 0) #f)
        (raise-arguments-error
         who_0
         "starting index less than stopping index, but given a negative step"
         "starting index"
         start_0
         "stopping index"
         stop_0
         "step"
         step_0)
        (void))
      (if (if (< stop_0 start_0) (> step_0 0) #f)
        (raise-arguments-error
         who_0
         "starting index more than stopping index, but given a positive step"
         "starting index"
         start_0
         "stopping index"
         stop_0
         "step"
         step_0)
        (void)))))
(define normalise-inputs
  (lambda (who_0
           type-name_0
           vector?_0
           unsafe-vector-length_0
           vec_0
           start_0
           stop_0
           step_0)
    (begin
      (if (|#%app| vector?_0 vec_0)
        (void)
        (raise-argument-error who_0 type-name_0 vec_0))
      (let ((len_0 (|#%app| unsafe-vector-length_0 vec_0)))
        (let ((stop*_0 (if stop_0 stop_0 len_0)))
          (begin
            (check-ranges who_0 type-name_0 vec_0 start_0 stop*_0 step_0 len_0)
            (values vec_0 start_0 stop*_0 step_0)))))))
(define unsafe-normalise-inputs
  (lambda (unsafe-vector-length_0 vec_0 start_0 stop_0 step_0)
    (values
     vec_0
     start_0
     (if stop_0 stop_0 (|#%app| unsafe-vector-length_0 vec_0))
     step_0)))
(define check-vector
  (lambda (v_0)
    (if (vector? v_0) (void) (raise-argument-error 'in-vector "vector" v_0))))
(define-values
 (struct:do-stream make-do-stream do-stream? do-stream-ref do-stream-set!)
 (make-struct-type
  'stream
  #f
  3
  0
  #f
  (list
   (cons
    prop:stream
    (vector
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 0)))
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 1)))
     (lambda (v_0) (|#%app| (|#%app| do-stream-ref v_0 2))))))))
(define empty-stream (make-do-stream (lambda () #t) void void))
(define map_1346
  (|#%name|
   map
   (case-lambda
    ((f_0 l_0)
     (begin
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (l_1)
             (begin
               (if (null? l_1)
                 null
                 (let ((r_0 (cdr l_1)))
                   (let ((app_0 (|#%app| f_0 (car l_1))))
                     (cons app_0 (loop_0 r_0))))))))))
        (loop_0 l_0))))
    ((f_0 l1_0 l2_0)
     (letrec*
      ((loop_0
        (|#%name|
         loop
         (lambda (l1_1 l2_1)
           (begin
             (if (null? l1_1)
               null
               (let ((r1_0 (cdr l1_1)))
                 (let ((r2_0 (cdr l2_1)))
                   (let ((r1_1 r1_0))
                     (let ((app_0
                            (let ((app_0 (car l1_1)))
                              (|#%app| f_0 app_0 (car l2_1)))))
                       (cons app_0 (loop_0 r1_1 r2_0))))))))))))
      (loop_0 l1_0 l2_0)))
    ((f_0 l_0 . args_0) (gen-map f_0 (cons l_0 args_0))))))
(define ormap_2765
  (|#%name|
   ormap
   (case-lambda
    ((f_0 l_0)
     (begin
       (if (null? l_0)
         #f
         (letrec*
          ((loop_0
            (|#%name|
             loop
             (lambda (l_1)
               (begin
                 (if (null? (cdr l_1))
                   (|#%app| f_0 (car l_1))
                   (let ((r_0 (cdr l_1)))
                     (let ((or-part_0 (|#%app| f_0 (car l_1))))
                       (if or-part_0 or-part_0 (loop_0 r_0))))))))))
          (loop_0 l_0)))))
    ((f_0 l1_0 l2_0)
     (if (null? l1_0)
       #f
       (letrec*
        ((loop_0
          (|#%name|
           loop
           (lambda (l1_1 l2_1)
             (begin
               (if (null? (cdr l1_1))
                 (let ((app_0 (car l1_1))) (|#%app| f_0 app_0 (car l2_1)))
                 (let ((r1_0 (cdr l1_1)))
                   (let ((r2_0 (cdr l2_1)))
                     (let ((r1_1 r1_0))
                       (let ((or-part_0
                              (let ((app_0 (car l1_1)))
                                (|#%app| f_0 app_0 (car l2_1)))))
                         (if or-part_0 or-part_0 (loop_0 r1_1 r2_0))))))))))))
        (loop_0 l1_0 l2_0))))
    ((f_0 l_0 . args_0) (gen-ormap f_0 (cons l_0 args_0))))))
(define check-args
  (lambda (who_0 f_0 ls_0)
    (begin
      (if (procedure? f_0)
        (void)
        (raise-argument-error who_0 "procedure?" f_0))
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (prev-len_0 ls_1 i_0)
            (begin
              (if (null? ls_1)
                (void)
                (let ((l_0 (car ls_1)))
                  (begin
                    (if (list? l_0)
                      (void)
                      (raise-argument-error who_0 "list?" l_0))
                    (let ((len_0 (length l_0)))
                      (begin
                        (if (if prev-len_0 (not (= len_0 prev-len_0)) #f)
                          (raise-arguments-error
                           who_0
                           "all lists must have same size"
                           "first list length"
                           prev-len_0
                           "other list length"
                           len_0
                           "procedure"
                           f_0)
                          (void))
                        (let ((app_0 (cdr ls_1)))
                          (loop_0 len_0 app_0 (add1 i_0)))))))))))))
       (loop_0 #f ls_0 1))
      (if (procedure-arity-includes? f_0 (length ls_0))
        (void)
        (call-with-values
         (lambda () (procedure-keywords f_0))
         (case-lambda
          ((required-keywords_0 optional-keywords_0)
           (let ((app_0
                  (if (pair? required-keywords_0)
                    (string-append
                     "argument mismatch;\n"
                     " the given procedure expects keyword arguments")
                    (string-append
                     "argument mismatch;\n"
                     " the given procedure's expected number of arguments does not match"
                     " the given number of lists"))))
             (let ((app_1
                    (unquoted-printing-string
                     (let ((or-part_0
                            (let ((n_0 (object-name f_0)))
                              (if (symbol? n_0) (symbol->string n_0) #f))))
                       (if or-part_0 or-part_0 "#<procedure>")))))
               (apply
                raise-arguments-error
                who_0
                app_0
                "given procedure"
                app_1
                (let ((app_2
                       (let ((a_0 (procedure-arity f_0)))
                         (if (pair? required-keywords_0)
                           null
                           (if (integer? a_0)
                             (list "expected" a_0)
                             (if (arity-at-least? a_0)
                               (list
                                "expected"
                                (unquoted-printing-string
                                 (string-append
                                  "at least "
                                  (number->string
                                   (arity-at-least-value a_0)))))
                               null))))))
                  (let ((app_3
                         (if (pair? required-keywords_0)
                           null
                           (list "given" (length ls_0)))))
                    (let ((app_4
                           (if (pair? required-keywords_0)
                             (list
                              "required keywords"
                              (unquoted-printing-string
                               (apply
                                string-append
                                (cdr
                                 (letrec*
                                  ((loop_0
                                    (|#%name|
                                     loop
                                     (lambda (kws_0)
                                       (begin
                                         (if (null? kws_0)
                                           null
                                           (let ((app_4
                                                  (string-append
                                                   "#:"
                                                   (keyword->string
                                                    (car kws_0)))))
                                             (list*
                                              " "
                                              app_4
                                              (loop_0 (cdr kws_0))))))))))
                                  (loop_0 required-keywords_0))))))
                             null)))
                      (append
                       app_2
                       app_3
                       app_4
                       (let ((w_0
                              (let ((app_5 (error-print-width)))
                                (quotient app_5 (length ls_0)))))
                         (if (> w_0 10)
                           (list
                            "argument lists..."
                            (unquoted-printing-string
                             (apply
                              string-append
                              (letrec*
                               ((loop_0
                                 (|#%name|
                                  loop
                                  (lambda (ls_1)
                                    (begin
                                      (if (null? ls_1)
                                        null
                                        (let ((app_5
                                               (string-append
                                                "\n   "
                                                (let ((app_5
                                                       (error-value->string-handler)))
                                                  (|#%app|
                                                   app_5
                                                   (car ls_1)
                                                   w_0)))))
                                          (cons
                                           app_5
                                           (loop_0 (cdr ls_1))))))))))
                               (loop_0 ls_0)))))
                           null))))))))))
          (args (raise-binding-result-arity-error 2 args))))))))
(define gen-map
  (lambda (f_0 ls_0)
    (begin
      #t
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (ls_1)
            (begin
              (if (null? (car ls_1))
                null
                (let ((next-ls_0 (map_1346 cdr ls_1)))
                  (let ((app_0 (apply f_0 (map_1346 car ls_1))))
                    (cons app_0 (loop_0 next-ls_0))))))))))
       (loop_0 ls_0)))))
(define gen-ormap
  (lambda (f_0 ls_0)
    (begin
      #t
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (ls_1)
            (begin
              (if (null? (car ls_1))
                #f
                (if (null? (cdar ls_1))
                  (apply f_0 (map_1346 car ls_1))
                  (let ((next-ls_0 (map_1346 cdr ls_1)))
                    (let ((or-part_0 (apply f_0 (map_1346 car ls_1))))
                      (if or-part_0 or-part_0 (loop_0 next-ls_0)))))))))))
       (loop_0 ls_0)))))
(define print-value-columns
  (make-parameter
   +inf.0
   (lambda (c_0)
     (if (let ((or-part_0 (eqv? c_0 +inf.0)))
           (if or-part_0 or-part_0 (if (exact-integer? c_0) (> c_0 5) #f)))
       c_0
       (raise-argument-error
        'print-value-columns
        "(or/c +inf.0 (and/c exact-integer? (>/c 5)))"
        c_0)))
   'print-value-columns))
(define regexp-error-tag (make-continuation-prompt-tag 'regexp-error))
(define regexp-error
  (lambda (fmt_0 . args_0)
    (abort-current-continuation regexp-error-tag (apply format fmt_0 args_0))))
(define chytes-length$1
  (|#%name|
   chytes-length
   (lambda (s_0)
     (begin (if (bytes? s_0) (unsafe-bytes-length s_0) (string-length s_0))))))
(define chytes-ref$1
  (|#%name|
   chytes-ref
   (lambda (s_0 i_0)
     (begin
       (if (bytes? s_0)
         (unsafe-bytes-ref s_0 i_0)
         (char->integer (string-ref s_0 i_0)))))))
(define chytes-ref/char
  (lambda (s_0 i_0)
    (if (bytes? s_0)
      (integer->char (unsafe-bytes-ref s_0 i_0))
      (string-ref s_0 i_0))))
(define chytes-limit (lambda (s_0) (if (bytes? s_0) 255 1114111)))
(define empty-range null)
(define range-invert
  (lambda (r_0 limit-c_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (r_1 start_0)
          (begin
            (if (null? r_1)
              (if (> start_0 limit-c_0) null (list (cons start_0 limit-c_0)))
              (if (= start_0 (caar r_1))
                (let ((app_0 (cdr r_1))) (loop_0 app_0 (add1 (cdar r_1))))
                (let ((app_0 (cons start_0 (sub1 (caar r_1)))))
                  (cons
                   app_0
                   (let ((app_1 (cdr r_1)))
                     (loop_0 app_1 (add1 (cdar r_1)))))))))))))
     (loop_0 r_0 0))))
(define range-in?
  (lambda (r_0 v_0)
    (begin
      (letrec*
       ((for-loop_0
         (|#%name|
          for-loop
          (lambda (result_0 lst_0)
            (begin
              (if (pair? lst_0)
                (let ((p_0 (unsafe-car lst_0)))
                  (let ((rest_0 (unsafe-cdr lst_0)))
                    (let ((result_1
                           (let ((result_1
                                  (if (>= v_0 (car p_0))
                                    (<= v_0 (cdr p_0))
                                    #f)))
                             (values result_1))))
                      (if (if (not (let ((x_0 (list p_0))) result_1)) #t #f)
                        (for-loop_0 result_1 rest_0)
                        result_1))))
                result_0))))))
       (for-loop_0 #f r_0)))))
(define range-add
  (lambda (r_0 v_0)
    (if (not v_0)
      r_0
      (if (range-in? r_0 v_0) r_0 (range-union r_0 (list (cons v_0 v_0)))))))
(define range-union
  (lambda (r1_0 r2_0)
    (if (null? r1_0)
      r2_0
      (if (null? r2_0)
        r1_0
        (if (let ((app_0 (caar r1_0))) (<= app_0 (caar r2_0)))
          (if (let ((app_0 (add1 (cdar r1_0)))) (>= app_0 (caar r2_0)))
            (if (let ((app_0 (cdar r1_0))) (<= app_0 (cdar r2_0)))
              (let ((app_0
                     (let ((app_0
                            (let ((app_0 (caar r1_0)))
                              (cons app_0 (cdar r2_0)))))
                       (cons app_0 (cdr r2_0)))))
                (range-union app_0 (cdr r1_0)))
              (range-union r1_0 (cdr r2_0)))
            (let ((app_0 (car r1_0)))
              (cons app_0 (range-union (cdr r1_0) r2_0))))
          (range-union r2_0 r1_0))))))
(define range-add-span
  (lambda (range_0 from-c_0 to-c_0)
    (range-union range_0 (list (cons from-c_0 to-c_0)))))
(define range-singleton
  (lambda (range_0)
    (if (pair? range_0)
      (if (null? (cdr range_0))
        (if (let ((app_0 (caar range_0))) (= app_0 (cdar range_0)))
          (caar range_0)
          #f)
        #f)
      #f)))
(define range-includes?
  (lambda (range_0 low_0 hi_0)
    (if (null? range_0)
      null
      (if (> low_0 (cdar range_0))
        (range-includes? (cdr range_0) low_0 hi_0)
        (if (>= low_0 (caar range_0)) (<= hi_0 (cdar range_0)) #f)))))
(define range-within?
  (lambda (range_0 low_0 hi_0)
    (if (null? range_0)
      #t
      (if (< (caar range_0) low_0)
        #f
        (if (> (cdar range_0) hi_0)
          #f
          (range-within? (cdr range_0) low_0 hi_0))))))
(define range->list (lambda (range_0) range_0))
(define cell.1 (unsafe-make-place-local (make-weak-hash)))
(define range-place-init!
  (lambda () (unsafe-place-local-set! cell.1 (make-weak-hash))))
(define compile-range
  (lambda (range_0)
    (let ((or-part_0 (hash-ref (unsafe-place-local-ref cell.1) range_0 #f)))
      (if or-part_0
        or-part_0
        (let ((rng_0 (make-bytes 256 0)))
          (begin
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (lst_0)
                    (begin
                      (if (pair? lst_0)
                        (let ((p_0 (unsafe-car lst_0)))
                          (let ((rest_0 (unsafe-cdr lst_0)))
                            (call-with-values
                             (lambda ()
                               (let ((start_0 (car p_0)))
                                 (let ((end_0 (add1 (cdr p_0))))
                                   (let ((start_1 start_0))
                                     (begin
                                       (letrec*
                                        ((for-loop_1
                                          (|#%name|
                                           for-loop
                                           (lambda (pos_0)
                                             (begin
                                               (if (< pos_0 end_0)
                                                 (begin
                                                   (unsafe-bytes-set!
                                                    rng_0
                                                    pos_0
                                                    1)
                                                   (for-loop_1 (+ pos_0 1)))
                                                 (values)))))))
                                        (for-loop_1 start_1)))))))
                             (case-lambda
                              (() (for-loop_0 rest_0))
                              (args
                               (raise-binding-result-arity-error 0 args))))))
                        (values)))))))
               (for-loop_0 range_0)))
            (void)
            (hash-set! (unsafe-place-local-ref cell.1) range_0 rng_0)
            rng_0))))))
(define rng-in? (lambda (rng_0 v_0) (eq? 1 (unsafe-bytes-ref rng_0 v_0))))
(define rx:never 'never)
(define rx:empty 'empty)
(define rx:any 'any)
(define rx:start 'start)
(define rx:end 'end)
(define rx:line-start 'line-start)
(define rx:line-end 'line-end)
(define rx:word-boundary 'word-boundary)
(define rx:not-word-boundary 'not-word-boundary)
(define finish_2542
  (make-struct-type-install-properties
   '(rx:alts)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:alts))
(define struct:rx:alts
  (make-record-type-descriptor
   'rx:alts
   #f
   (|#%nongenerative-uid| rx:alts)
   #f
   #f
   '(2 . 0)))
(define effect_2414 (finish_2542 struct:rx:alts))
(define rx:alts1.1
  (|#%name|
   rx:alts
   (record-constructor
    (make-record-constructor-descriptor struct:rx:alts #f #f))))
(define rx:alts?_2576 (|#%name| rx:alts? (record-predicate struct:rx:alts)))
(define rx:alts?
  (|#%name|
   rx:alts?
   (lambda (v)
     (if (rx:alts?_2576 v)
       #t
       ($value
        (if (impersonator? v) (rx:alts?_2576 (impersonator-val v)) #f))))))
(define rx:alts-rx_2530
  (|#%name| rx:alts-rx1 (record-accessor struct:rx:alts 0)))
(define rx:alts-rx_2265
  (|#%name|
   rx:alts-rx1
   (lambda (s)
     (if (rx:alts?_2576 s)
       (rx:alts-rx_2530 s)
       ($value (impersonate-ref rx:alts-rx_2530 struct:rx:alts 0 s 'rx1))))))
(define rx:alts-rx_2917
  (|#%name| rx:alts-rx2 (record-accessor struct:rx:alts 1)))
(define rx:alts-rx_1912
  (|#%name|
   rx:alts-rx2
   (lambda (s)
     (if (rx:alts?_2576 s)
       (rx:alts-rx_2917 s)
       ($value (impersonate-ref rx:alts-rx_2917 struct:rx:alts 1 s 'rx2))))))
(define finish_2732
  (make-struct-type-install-properties
   '(rx:sequence)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:sequence))
(define struct:rx:sequence
  (make-record-type-descriptor
   'rx:sequence
   #f
   (|#%nongenerative-uid| rx:sequence)
   #f
   #f
   '(2 . 0)))
(define effect_2459 (finish_2732 struct:rx:sequence))
(define rx:sequence2.1
  (|#%name|
   rx:sequence
   (record-constructor
    (make-record-constructor-descriptor struct:rx:sequence #f #f))))
(define rx:sequence?_3213
  (|#%name| rx:sequence? (record-predicate struct:rx:sequence)))
(define rx:sequence?
  (|#%name|
   rx:sequence?
   (lambda (v)
     (if (rx:sequence?_3213 v)
       #t
       ($value
        (if (impersonator? v) (rx:sequence?_3213 (impersonator-val v)) #f))))))
(define rx:sequence-rxs_2380
  (|#%name| rx:sequence-rxs (record-accessor struct:rx:sequence 0)))
(define rx:sequence-rxs
  (|#%name|
   rx:sequence-rxs
   (lambda (s)
     (if (rx:sequence?_3213 s)
       (rx:sequence-rxs_2380 s)
       ($value
        (impersonate-ref rx:sequence-rxs_2380 struct:rx:sequence 0 s 'rxs))))))
(define rx:sequence-needs-backtrack?_2458
  (|#%name|
   rx:sequence-needs-backtrack?
   (record-accessor struct:rx:sequence 1)))
(define rx:sequence-needs-backtrack?
  (|#%name|
   rx:sequence-needs-backtrack?
   (lambda (s)
     (if (rx:sequence?_3213 s)
       (rx:sequence-needs-backtrack?_2458 s)
       ($value
        (impersonate-ref
         rx:sequence-needs-backtrack?_2458
         struct:rx:sequence
         1
         s
         'needs-backtrack?))))))
(define finish_2954
  (make-struct-type-install-properties
   '(rx:group)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:group))
(define struct:rx:group
  (make-record-type-descriptor
   'rx:group
   #f
   (|#%nongenerative-uid| rx:group)
   #f
   #f
   '(2 . 0)))
(define effect_1819 (finish_2954 struct:rx:group))
(define rx:group3.1
  (|#%name|
   rx:group
   (record-constructor
    (make-record-constructor-descriptor struct:rx:group #f #f))))
(define rx:group?_3085 (|#%name| rx:group? (record-predicate struct:rx:group)))
(define rx:group?
  (|#%name|
   rx:group?
   (lambda (v)
     (if (rx:group?_3085 v)
       #t
       ($value
        (if (impersonator? v) (rx:group?_3085 (impersonator-val v)) #f))))))
(define rx:group-rx_2903
  (|#%name| rx:group-rx (record-accessor struct:rx:group 0)))
(define rx:group-rx
  (|#%name|
   rx:group-rx
   (lambda (s)
     (if (rx:group?_3085 s)
       (rx:group-rx_2903 s)
       ($value (impersonate-ref rx:group-rx_2903 struct:rx:group 0 s 'rx))))))
(define rx:group-number_2715
  (|#%name| rx:group-number (record-accessor struct:rx:group 1)))
(define rx:group-number
  (|#%name|
   rx:group-number
   (lambda (s)
     (if (rx:group?_3085 s)
       (rx:group-number_2715 s)
       ($value
        (impersonate-ref rx:group-number_2715 struct:rx:group 1 s 'number))))))
(define finish_1837
  (make-struct-type-install-properties
   '(rx:repeat)
   4
   0
   #f
   null
   #f
   #f
   '(0 1 2 3)
   #f
   'rx:repeat))
(define struct:rx:repeat
  (make-record-type-descriptor
   'rx:repeat
   #f
   (|#%nongenerative-uid| rx:repeat)
   #f
   #f
   '(4 . 0)))
(define effect_2312 (finish_1837 struct:rx:repeat))
(define rx:repeat4.1
  (|#%name|
   rx:repeat
   (record-constructor
    (make-record-constructor-descriptor struct:rx:repeat #f #f))))
(define rx:repeat?_2609
  (|#%name| rx:repeat? (record-predicate struct:rx:repeat)))
(define rx:repeat?
  (|#%name|
   rx:repeat?
   (lambda (v)
     (if (rx:repeat?_2609 v)
       #t
       ($value
        (if (impersonator? v) (rx:repeat?_2609 (impersonator-val v)) #f))))))
(define rx:repeat-rx_2269
  (|#%name| rx:repeat-rx (record-accessor struct:rx:repeat 0)))
(define rx:repeat-rx
  (|#%name|
   rx:repeat-rx
   (lambda (s)
     (if (rx:repeat?_2609 s)
       (rx:repeat-rx_2269 s)
       ($value
        (impersonate-ref rx:repeat-rx_2269 struct:rx:repeat 0 s 'rx))))))
(define rx:repeat-min_2947
  (|#%name| rx:repeat-min (record-accessor struct:rx:repeat 1)))
(define rx:repeat-min
  (|#%name|
   rx:repeat-min
   (lambda (s)
     (if (rx:repeat?_2609 s)
       (rx:repeat-min_2947 s)
       ($value
        (impersonate-ref rx:repeat-min_2947 struct:rx:repeat 1 s 'min))))))
(define rx:repeat-max_2564
  (|#%name| rx:repeat-max (record-accessor struct:rx:repeat 2)))
(define rx:repeat-max
  (|#%name|
   rx:repeat-max
   (lambda (s)
     (if (rx:repeat?_2609 s)
       (rx:repeat-max_2564 s)
       ($value
        (impersonate-ref rx:repeat-max_2564 struct:rx:repeat 2 s 'max))))))
(define rx:repeat-non-greedy?_2609
  (|#%name| rx:repeat-non-greedy? (record-accessor struct:rx:repeat 3)))
(define rx:repeat-non-greedy?
  (|#%name|
   rx:repeat-non-greedy?
   (lambda (s)
     (if (rx:repeat?_2609 s)
       (rx:repeat-non-greedy?_2609 s)
       ($value
        (impersonate-ref
         rx:repeat-non-greedy?_2609
         struct:rx:repeat
         3
         s
         'non-greedy?))))))
(define finish_3260
  (make-struct-type-install-properties
   '(rx:maybe)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:maybe))
(define struct:rx:maybe
  (make-record-type-descriptor
   'rx:maybe
   #f
   (|#%nongenerative-uid| rx:maybe)
   #f
   #f
   '(2 . 0)))
(define effect_2202 (finish_3260 struct:rx:maybe))
(define rx:maybe5.1
  (|#%name|
   rx:maybe
   (record-constructor
    (make-record-constructor-descriptor struct:rx:maybe #f #f))))
(define rx:maybe?_2766 (|#%name| rx:maybe? (record-predicate struct:rx:maybe)))
(define rx:maybe?
  (|#%name|
   rx:maybe?
   (lambda (v)
     (if (rx:maybe?_2766 v)
       #t
       ($value
        (if (impersonator? v) (rx:maybe?_2766 (impersonator-val v)) #f))))))
(define rx:maybe-rx_2762
  (|#%name| rx:maybe-rx (record-accessor struct:rx:maybe 0)))
(define rx:maybe-rx
  (|#%name|
   rx:maybe-rx
   (lambda (s)
     (if (rx:maybe?_2766 s)
       (rx:maybe-rx_2762 s)
       ($value (impersonate-ref rx:maybe-rx_2762 struct:rx:maybe 0 s 'rx))))))
(define rx:maybe-non-greedy?_2749
  (|#%name| rx:maybe-non-greedy? (record-accessor struct:rx:maybe 1)))
(define rx:maybe-non-greedy?
  (|#%name|
   rx:maybe-non-greedy?
   (lambda (s)
     (if (rx:maybe?_2766 s)
       (rx:maybe-non-greedy?_2749 s)
       ($value
        (impersonate-ref
         rx:maybe-non-greedy?_2749
         struct:rx:maybe
         1
         s
         'non-greedy?))))))
(define finish_2500
  (make-struct-type-install-properties
   '(rx:conditional)
   6
   0
   #f
   null
   #f
   #f
   '(0 1 2 3 4 5)
   #f
   'rx:conditional))
(define struct:rx:conditional
  (make-record-type-descriptor
   'rx:conditional
   #f
   (|#%nongenerative-uid| rx:conditional)
   #f
   #f
   '(6 . 0)))
(define effect_2905 (finish_2500 struct:rx:conditional))
(define rx:conditional6.1
  (|#%name|
   rx:conditional
   (record-constructor
    (make-record-constructor-descriptor struct:rx:conditional #f #f))))
(define rx:conditional?_2616
  (|#%name| rx:conditional? (record-predicate struct:rx:conditional)))
(define rx:conditional?
  (|#%name|
   rx:conditional?
   (lambda (v)
     (if (rx:conditional?_2616 v)
       #t
       ($value
        (if (impersonator? v)
          (rx:conditional?_2616 (impersonator-val v))
          #f))))))
(define rx:conditional-tst_3132
  (|#%name| rx:conditional-tst (record-accessor struct:rx:conditional 0)))
(define rx:conditional-tst
  (|#%name|
   rx:conditional-tst
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-tst_3132 s)
       ($value
        (impersonate-ref
         rx:conditional-tst_3132
         struct:rx:conditional
         0
         s
         'tst))))))
(define rx:conditional-rx_2590
  (|#%name| rx:conditional-rx1 (record-accessor struct:rx:conditional 1)))
(define rx:conditional-rx_2162
  (|#%name|
   rx:conditional-rx1
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-rx_2590 s)
       ($value
        (impersonate-ref
         rx:conditional-rx_2590
         struct:rx:conditional
         1
         s
         'rx1))))))
(define rx:conditional-rx_3084
  (|#%name| rx:conditional-rx2 (record-accessor struct:rx:conditional 2)))
(define rx:conditional-rx_2328
  (|#%name|
   rx:conditional-rx2
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-rx_3084 s)
       ($value
        (impersonate-ref
         rx:conditional-rx_3084
         struct:rx:conditional
         2
         s
         'rx2))))))
(define rx:conditional-n-start_1886
  (|#%name| rx:conditional-n-start (record-accessor struct:rx:conditional 3)))
(define rx:conditional-n-start
  (|#%name|
   rx:conditional-n-start
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-n-start_1886 s)
       ($value
        (impersonate-ref
         rx:conditional-n-start_1886
         struct:rx:conditional
         3
         s
         'n-start))))))
(define rx:conditional-num-n_2530
  (|#%name| rx:conditional-num-n (record-accessor struct:rx:conditional 4)))
(define rx:conditional-num-n
  (|#%name|
   rx:conditional-num-n
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-num-n_2530 s)
       ($value
        (impersonate-ref
         rx:conditional-num-n_2530
         struct:rx:conditional
         4
         s
         'num-n))))))
(define rx:conditional-needs-backtrack?_2562
  (|#%name|
   rx:conditional-needs-backtrack?
   (record-accessor struct:rx:conditional 5)))
(define rx:conditional-needs-backtrack?
  (|#%name|
   rx:conditional-needs-backtrack?
   (lambda (s)
     (if (rx:conditional?_2616 s)
       (rx:conditional-needs-backtrack?_2562 s)
       ($value
        (impersonate-ref
         rx:conditional-needs-backtrack?_2562
         struct:rx:conditional
         5
         s
         'needs-backtrack?))))))
(define finish_2488
  (make-struct-type-install-properties
   '(rx:lookahead)
   4
   0
   #f
   null
   #f
   #f
   '(0 1 2 3)
   #f
   'rx:lookahead))
(define struct:rx:lookahead
  (make-record-type-descriptor
   'rx:lookahead
   #f
   (|#%nongenerative-uid| rx:lookahead)
   #f
   #f
   '(4 . 0)))
(define effect_2486 (finish_2488 struct:rx:lookahead))
(define rx:lookahead7.1
  (|#%name|
   rx:lookahead
   (record-constructor
    (make-record-constructor-descriptor struct:rx:lookahead #f #f))))
(define rx:lookahead?_3136
  (|#%name| rx:lookahead? (record-predicate struct:rx:lookahead)))
(define rx:lookahead?
  (|#%name|
   rx:lookahead?
   (lambda (v)
     (if (rx:lookahead?_3136 v)
       #t
       ($value
        (if (impersonator? v)
          (rx:lookahead?_3136 (impersonator-val v))
          #f))))))
(define rx:lookahead-rx_2682
  (|#%name| rx:lookahead-rx (record-accessor struct:rx:lookahead 0)))
(define rx:lookahead-rx
  (|#%name|
   rx:lookahead-rx
   (lambda (s)
     (if (rx:lookahead?_3136 s)
       (rx:lookahead-rx_2682 s)
       ($value
        (impersonate-ref rx:lookahead-rx_2682 struct:rx:lookahead 0 s 'rx))))))
(define rx:lookahead-match?_1967
  (|#%name| rx:lookahead-match? (record-accessor struct:rx:lookahead 1)))
(define rx:lookahead-match?
  (|#%name|
   rx:lookahead-match?
   (lambda (s)
     (if (rx:lookahead?_3136 s)
       (rx:lookahead-match?_1967 s)
       ($value
        (impersonate-ref
         rx:lookahead-match?_1967
         struct:rx:lookahead
         1
         s
         'match?))))))
(define rx:lookahead-n-start_2202
  (|#%name| rx:lookahead-n-start (record-accessor struct:rx:lookahead 2)))
(define rx:lookahead-n-start
  (|#%name|
   rx:lookahead-n-start
   (lambda (s)
     (if (rx:lookahead?_3136 s)
       (rx:lookahead-n-start_2202 s)
       ($value
        (impersonate-ref
         rx:lookahead-n-start_2202
         struct:rx:lookahead
         2
         s
         'n-start))))))
(define rx:lookahead-num-n_2643
  (|#%name| rx:lookahead-num-n (record-accessor struct:rx:lookahead 3)))
(define rx:lookahead-num-n
  (|#%name|
   rx:lookahead-num-n
   (lambda (s)
     (if (rx:lookahead?_3136 s)
       (rx:lookahead-num-n_2643 s)
       ($value
        (impersonate-ref
         rx:lookahead-num-n_2643
         struct:rx:lookahead
         3
         s
         'num-n))))))
(define finish_3095
  (make-struct-type-install-properties
   '(rx:lookbehind)
   6
   0
   #f
   null
   #f
   #f
   '(0 1 4 5)
   #f
   'rx:lookbehind))
(define struct:rx:lookbehind
  (make-record-type-descriptor
   'rx:lookbehind
   #f
   (|#%nongenerative-uid| rx:lookbehind)
   #f
   #f
   '(6 . 12)))
(define effect_2468 (finish_3095 struct:rx:lookbehind))
(define rx:lookbehind8.1
  (|#%name|
   rx:lookbehind
   (record-constructor
    (make-record-constructor-descriptor struct:rx:lookbehind #f #f))))
(define rx:lookbehind?_3001
  (|#%name| rx:lookbehind? (record-predicate struct:rx:lookbehind)))
(define rx:lookbehind?
  (|#%name|
   rx:lookbehind?
   (lambda (v)
     (if (rx:lookbehind?_3001 v)
       #t
       ($value
        (if (impersonator? v)
          (rx:lookbehind?_3001 (impersonator-val v))
          #f))))))
(define rx:lookbehind-rx_2597
  (|#%name| rx:lookbehind-rx (record-accessor struct:rx:lookbehind 0)))
(define rx:lookbehind-rx
  (|#%name|
   rx:lookbehind-rx
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-rx_2597 s)
       ($value
        (impersonate-ref
         rx:lookbehind-rx_2597
         struct:rx:lookbehind
         0
         s
         'rx))))))
(define rx:lookbehind-match?_2428
  (|#%name| rx:lookbehind-match? (record-accessor struct:rx:lookbehind 1)))
(define rx:lookbehind-match?
  (|#%name|
   rx:lookbehind-match?
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-match?_2428 s)
       ($value
        (impersonate-ref
         rx:lookbehind-match?_2428
         struct:rx:lookbehind
         1
         s
         'match?))))))
(define rx:lookbehind-lb-min_2372
  (|#%name| rx:lookbehind-lb-min (record-accessor struct:rx:lookbehind 2)))
(define rx:lookbehind-lb-min
  (|#%name|
   rx:lookbehind-lb-min
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-lb-min_2372 s)
       ($value
        (impersonate-ref
         rx:lookbehind-lb-min_2372
         struct:rx:lookbehind
         2
         s
         'lb-min))))))
(define rx:lookbehind-lb-max_3076
  (|#%name| rx:lookbehind-lb-max (record-accessor struct:rx:lookbehind 3)))
(define rx:lookbehind-lb-max
  (|#%name|
   rx:lookbehind-lb-max
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-lb-max_3076 s)
       ($value
        (impersonate-ref
         rx:lookbehind-lb-max_3076
         struct:rx:lookbehind
         3
         s
         'lb-max))))))
(define rx:lookbehind-n-start_2946
  (|#%name| rx:lookbehind-n-start (record-accessor struct:rx:lookbehind 4)))
(define rx:lookbehind-n-start
  (|#%name|
   rx:lookbehind-n-start
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-n-start_2946 s)
       ($value
        (impersonate-ref
         rx:lookbehind-n-start_2946
         struct:rx:lookbehind
         4
         s
         'n-start))))))
(define rx:lookbehind-num-n_2613
  (|#%name| rx:lookbehind-num-n (record-accessor struct:rx:lookbehind 5)))
(define rx:lookbehind-num-n
  (|#%name|
   rx:lookbehind-num-n
   (lambda (s)
     (if (rx:lookbehind?_3001 s)
       (rx:lookbehind-num-n_2613 s)
       ($value
        (impersonate-ref
         rx:lookbehind-num-n_2613
         struct:rx:lookbehind
         5
         s
         'num-n))))))
(define set-rx:lookbehind-lb-min!_2809
  (|#%name| set-rx:lookbehind-lb-min! (record-mutator struct:rx:lookbehind 2)))
(define set-rx:lookbehind-lb-min!
  (|#%name|
   set-rx:lookbehind-lb-min!
   (lambda (s v)
     (if (rx:lookbehind?_3001 s)
       (set-rx:lookbehind-lb-min!_2809 s v)
       ($value
        (impersonate-set!
         set-rx:lookbehind-lb-min!_2809
         struct:rx:lookbehind
         2
         2
         s
         v
         'lb-min))))))
(define set-rx:lookbehind-lb-max!_2352
  (|#%name| set-rx:lookbehind-lb-max! (record-mutator struct:rx:lookbehind 3)))
(define set-rx:lookbehind-lb-max!
  (|#%name|
   set-rx:lookbehind-lb-max!
   (lambda (s v)
     (if (rx:lookbehind?_3001 s)
       (set-rx:lookbehind-lb-max!_2352 s v)
       ($value
        (impersonate-set!
         set-rx:lookbehind-lb-max!_2352
         struct:rx:lookbehind
         3
         3
         s
         v
         'lb-max))))))
(define finish_2346
  (make-struct-type-install-properties
   '(rx:cut)
   4
   0
   #f
   null
   #f
   #f
   '(0 1 2 3)
   #f
   'rx:cut))
(define struct:rx:cut
  (make-record-type-descriptor
   'rx:cut
   #f
   (|#%nongenerative-uid| rx:cut)
   #f
   #f
   '(4 . 0)))
(define effect_2158 (finish_2346 struct:rx:cut))
(define rx:cut9.1
  (|#%name|
   rx:cut
   (record-constructor
    (make-record-constructor-descriptor struct:rx:cut #f #f))))
(define rx:cut?_2453 (|#%name| rx:cut? (record-predicate struct:rx:cut)))
(define rx:cut?
  (|#%name|
   rx:cut?
   (lambda (v)
     (if (rx:cut?_2453 v)
       #t
       ($value
        (if (impersonator? v) (rx:cut?_2453 (impersonator-val v)) #f))))))
(define rx:cut-rx_2624 (|#%name| rx:cut-rx (record-accessor struct:rx:cut 0)))
(define rx:cut-rx
  (|#%name|
   rx:cut-rx
   (lambda (s)
     (if (rx:cut?_2453 s)
       (rx:cut-rx_2624 s)
       ($value (impersonate-ref rx:cut-rx_2624 struct:rx:cut 0 s 'rx))))))
(define rx:cut-n-start_2924
  (|#%name| rx:cut-n-start (record-accessor struct:rx:cut 1)))
(define rx:cut-n-start
  (|#%name|
   rx:cut-n-start
   (lambda (s)
     (if (rx:cut?_2453 s)
       (rx:cut-n-start_2924 s)
       ($value
        (impersonate-ref rx:cut-n-start_2924 struct:rx:cut 1 s 'n-start))))))
(define rx:cut-num-n_2085
  (|#%name| rx:cut-num-n (record-accessor struct:rx:cut 2)))
(define rx:cut-num-n
  (|#%name|
   rx:cut-num-n
   (lambda (s)
     (if (rx:cut?_2453 s)
       (rx:cut-num-n_2085 s)
       ($value
        (impersonate-ref rx:cut-num-n_2085 struct:rx:cut 2 s 'num-n))))))
(define rx:cut-needs-backtrack?_2736
  (|#%name| rx:cut-needs-backtrack? (record-accessor struct:rx:cut 3)))
(define rx:cut-needs-backtrack?
  (|#%name|
   rx:cut-needs-backtrack?
   (lambda (s)
     (if (rx:cut?_2453 s)
       (rx:cut-needs-backtrack?_2736 s)
       ($value
        (impersonate-ref
         rx:cut-needs-backtrack?_2736
         struct:rx:cut
         3
         s
         'needs-backtrack?))))))
(define finish_1921
  (make-struct-type-install-properties
   '(rx:reference)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:reference))
(define struct:rx:reference
  (make-record-type-descriptor
   'rx:reference
   #f
   (|#%nongenerative-uid| rx:reference)
   #f
   #f
   '(2 . 0)))
(define effect_2306 (finish_1921 struct:rx:reference))
(define rx:reference10.1
  (|#%name|
   rx:reference
   (record-constructor
    (make-record-constructor-descriptor struct:rx:reference #f #f))))
(define rx:reference?_2938
  (|#%name| rx:reference? (record-predicate struct:rx:reference)))
(define rx:reference?
  (|#%name|
   rx:reference?
   (lambda (v)
     (if (rx:reference?_2938 v)
       #t
       ($value
        (if (impersonator? v)
          (rx:reference?_2938 (impersonator-val v))
          #f))))))
(define rx:reference-n_2302
  (|#%name| rx:reference-n (record-accessor struct:rx:reference 0)))
(define rx:reference-n
  (|#%name|
   rx:reference-n
   (lambda (s)
     (if (rx:reference?_2938 s)
       (rx:reference-n_2302 s)
       ($value
        (impersonate-ref rx:reference-n_2302 struct:rx:reference 0 s 'n))))))
(define rx:reference-case-sensitive?_2306
  (|#%name|
   rx:reference-case-sensitive?
   (record-accessor struct:rx:reference 1)))
(define rx:reference-case-sensitive?
  (|#%name|
   rx:reference-case-sensitive?
   (lambda (s)
     (if (rx:reference?_2938 s)
       (rx:reference-case-sensitive?_2306 s)
       ($value
        (impersonate-ref
         rx:reference-case-sensitive?_2306
         struct:rx:reference
         1
         s
         'case-sensitive?))))))
(define finish_2471
  (make-struct-type-install-properties
   '(rx:range)
   1
   0
   #f
   null
   #f
   #f
   '(0)
   #f
   'rx:range))
(define struct:rx:range
  (make-record-type-descriptor
   'rx:range
   #f
   (|#%nongenerative-uid| rx:range)
   #f
   #f
   '(1 . 0)))
(define effect_2071 (finish_2471 struct:rx:range))
(define rx:range11.1
  (|#%name|
   rx:range
   (record-constructor
    (make-record-constructor-descriptor struct:rx:range #f #f))))
(define rx:range?_1948 (|#%name| rx:range? (record-predicate struct:rx:range)))
(define rx:range?
  (|#%name|
   rx:range?
   (lambda (v)
     (if (rx:range?_1948 v)
       #t
       ($value
        (if (impersonator? v) (rx:range?_1948 (impersonator-val v)) #f))))))
(define rx:range-range_2664
  (|#%name| rx:range-range (record-accessor struct:rx:range 0)))
(define rx:range-range
  (|#%name|
   rx:range-range
   (lambda (s)
     (if (rx:range?_1948 s)
       (rx:range-range_2664 s)
       ($value
        (impersonate-ref rx:range-range_2664 struct:rx:range 0 s 'range))))))
(define finish_2339
  (make-struct-type-install-properties
   '(rx:unicode-categories)
   2
   0
   #f
   null
   #f
   #f
   '(0 1)
   #f
   'rx:unicode-categories))
(define struct:rx:unicode-categories
  (make-record-type-descriptor
   'rx:unicode-categories
   #f
   (|#%nongenerative-uid| rx:unicode-categories)
   #f
   #f
   '(2 . 0)))
(define effect_2341 (finish_2339 struct:rx:unicode-categories))
(define rx:unicode-categories12.1
  (|#%name|
   rx:unicode-categories
   (record-constructor
    (make-record-constructor-descriptor struct:rx:unicode-categories #f #f))))
(define rx:unicode-categories?_2156
  (|#%name|
   rx:unicode-categories?
   (record-predicate struct:rx:unicode-categories)))
(define rx:unicode-categories?
  (|#%name|
   rx:unicode-categories?
   (lambda (v)
     (if (rx:unicode-categories?_2156 v)
       #t
       ($value
        (if (impersonator? v)
          (rx:unicode-categories?_2156 (impersonator-val v))
          #f))))))
(define rx:unicode-categories-symlist_1950
  (|#%name|
   rx:unicode-categories-symlist
   (record-accessor struct:rx:unicode-categories 0)))
(define rx:unicode-categories-symlist
  (|#%name|
   rx:unicode-categories-symlist
   (lambda (s)
     (if (rx:unicode-categories?_2156 s)
       (rx:unicode-categories-symlist_1950 s)
       ($value
        (impersonate-ref
         rx:unicode-categories-symlist_1950
         struct:rx:unicode-categories
         0
         s
         'symlist))))))
(define rx:unicode-categories-match?_2521
  (|#%name|
   rx:unicode-categories-match?
   (record-accessor struct:rx:unicode-categories 1)))
(define rx:unicode-categories-match?
  (|#%name|
   rx:unicode-categories-match?
   (lambda (s)
     (if (rx:unicode-categories?_2156 s)
       (rx:unicode-categories-match?_2521 s)
       ($value
        (impersonate-ref
         rx:unicode-categories-match?_2521
         struct:rx:unicode-categories
         1
         s
         'match?))))))
(define needs-backtrack?
  (lambda (rx_0)
    (if (rx:alts? rx_0)
      #t
      (if (rx:sequence? rx_0)
        (rx:sequence-needs-backtrack? rx_0)
        (if (rx:group? rx_0)
          #t
          (if (rx:repeat? rx_0)
            #t
            (if (rx:maybe? rx_0)
              #t
              (if (rx:conditional? rx_0)
                (rx:conditional-needs-backtrack? rx_0)
                (if (rx:cut? rx_0)
                  (rx:cut-needs-backtrack? rx_0)
                  (if (rx:unicode-categories? rx_0) #t #f))))))))))
(define rx-range
  (lambda (range_0 limit-c_0)
    (let ((c1_0 (range-singleton range_0)))
      (if c1_0
        c1_0
        (if (range-includes? range_0 0 limit-c_0)
          'any
          (rx:range11.1 range_0))))))
(define rx-sequence
  (lambda (l_0)
    (if (null? l_0)
      'empty
      (if (null? (cdr l_0))
        (car l_0)
        (let ((merged-l_0 (merge-adjacent l_0)))
          (if (null? (cdr merged-l_0))
            (car merged-l_0)
            (rx:sequence2.1
             merged-l_0
             (ormap_2765 needs-backtrack? merged-l_0))))))))
(define merge-adjacent
  (lambda (l_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (mode_0 accum_0 l_1)
          (begin
            (if (if (pair? l_1) (rx:sequence? (car l_1)) #f)
              (loop_0
               mode_0
               accum_0
               (let ((app_0 (rx:sequence-rxs (car l_1))))
                 (append app_0 (cdr l_1))))
              (if (if (pair? l_1)
                    (let ((or-part_0 (eq? 'empty (car l_1))))
                      (if or-part_0
                        or-part_0
                        (let ((or-part_1 (equal? "" (car l_1))))
                          (if or-part_1 or-part_1 (equal? #vu8() (car l_1))))))
                    #f)
                (loop_0 mode_0 accum_0 (cdr l_1))
                (if (let ((or-part_0 (null? l_1)))
                      (if or-part_0
                        or-part_0
                        (not
                         (if (eq? mode_0 'byte)
                           (let ((or-part_1 (byte? (car l_1))))
                             (if or-part_1 or-part_1 (bytes? (car l_1))))
                           (if (eq? mode_0 'char)
                             (let ((or-part_1 (integer? (car l_1))))
                               (if or-part_1 or-part_1 (string? (car l_1))))
                             #t)))))
                  (if (null? accum_0)
                    null
                    (if (null? (cdr accum_0))
                      (let ((app_0 (car accum_0)))
                        (cons app_0 (loop_0 #f null l_1)))
                      (let ((app_0
                             (if (eq? mode_0 'byte)
                               (apply
                                bytes-append
                                (reverse$1
                                 (let ((lst_0 (reverse$1 accum_0)))
                                   (begin
                                     (letrec*
                                      ((for-loop_0
                                        (|#%name|
                                         for-loop
                                         (lambda (fold-var_0 lst_1)
                                           (begin
                                             (if (pair? lst_1)
                                               (let ((a_0 (unsafe-car lst_1)))
                                                 (let ((rest_0
                                                        (unsafe-cdr lst_1)))
                                                   (let ((fold-var_1
                                                          (let ((fold-var_1
                                                                 (cons
                                                                  (if (byte?
                                                                       a_0)
                                                                    (bytes a_0)
                                                                    a_0)
                                                                  fold-var_0)))
                                                            (values
                                                             fold-var_1))))
                                                     (for-loop_0
                                                      fold-var_1
                                                      rest_0))))
                                               fold-var_0))))))
                                      (for-loop_0 null lst_0))))))
                               (if (eq? mode_0 'char)
                                 (apply
                                  string-append
                                  (reverse$1
                                   (let ((lst_0 (reverse$1 accum_0)))
                                     (begin
                                       (letrec*
                                        ((for-loop_0
                                          (|#%name|
                                           for-loop
                                           (lambda (fold-var_0 lst_1)
                                             (begin
                                               (if (pair? lst_1)
                                                 (let ((a_0
                                                        (unsafe-car lst_1)))
                                                   (let ((rest_0
                                                          (unsafe-cdr lst_1)))
                                                     (let ((fold-var_1
                                                            (let ((fold-var_1
                                                                   (cons
                                                                    (if (integer?
                                                                         a_0)
                                                                      (string
                                                                       (integer->char
                                                                        a_0))
                                                                      a_0)
                                                                    fold-var_0)))
                                                              (values
                                                               fold-var_1))))
                                                       (for-loop_0
                                                        fold-var_1
                                                        rest_0))))
                                                 fold-var_0))))))
                                        (for-loop_0 null lst_0))))))
                                 (error "internal error")))))
                        (cons app_0 (loop_0 #f null l_1)))))
                  (if mode_0
                    (let ((app_0 (cons (car l_1) accum_0)))
                      (loop_0 mode_0 app_0 (cdr l_1)))
                    (if (let ((or-part_0 (byte? (car l_1))))
                          (if or-part_0 or-part_0 (bytes? (car l_1))))
                      (let ((app_0 (list (car l_1))))
                        (loop_0 'byte app_0 (cdr l_1)))
                      (if (let ((or-part_0 (integer? (car l_1))))
                            (if or-part_0 or-part_0 (string? (car l_1))))
                        (let ((app_0 (list (car l_1))))
                          (loop_0 'char app_0 (cdr l_1)))
                        (let ((app_0 (car l_1)))
                          (cons app_0 (loop_0 #f null (cdr l_1)))))))))))))))
     (loop_0 #f null l_0))))
(define rx-alts
  (lambda (rx1_0 rx2_0 limit-c_0)
    (if (eq? 'never rx1_0)
      rx2_0
      (if (eq? 'never rx2_0)
        rx1_0
        (if (if (rx:range? rx1_0) (rx:range? rx2_0) #f)
          (rx-range
           (let ((app_0 (rx:range-range rx1_0)))
             (range-union app_0 (rx:range-range rx2_0)))
           limit-c_0)
          (if (if (rx:range? rx1_0)
                (if (rx:alts? rx2_0) (rx:range? (rx:alts-rx_2265 rx2_0)) #f)
                #f)
            (let ((app_0 (rx-alts rx1_0 (rx:alts-rx_2265 rx2_0) limit-c_0)))
              (rx-alts app_0 (rx:alts-rx_1912 rx2_0) limit-c_0))
            (if (if (rx:range? rx1_0) (integer? rx2_0) #f)
              (rx-range (range-add (rx:range-range rx1_0) rx2_0) limit-c_0)
              (if (if (rx:range? rx2_0) (integer? rx1_0) #f)
                (rx-alts rx2_0 rx1_0 limit-c_0)
                (if (if (integer? rx1_0) (integer? rx2_0) #f)
                  (rx-range (range-add (range-add null rx1_0) rx2_0) limit-c_0)
                  (rx:alts1.1 rx1_0 rx2_0))))))))))
(define rx-group (lambda (rx_0 n_0) (rx:group3.1 rx_0 n_0)))
(define rx-cut
  (lambda (rx_0 n-start_0 num-n_0)
    (rx:cut9.1 rx_0 n-start_0 num-n_0 (needs-backtrack? rx_0))))
(define rx-conditional
  (lambda (tst_0 pces1_0 pces2_0 n-start_0 num-n_0)
    (rx:conditional6.1
     tst_0
     pces1_0
     pces2_0
     n-start_0
     num-n_0
     (let ((or-part_0 (needs-backtrack? pces1_0)))
       (if or-part_0 or-part_0 (needs-backtrack? pces2_0))))))
(define finish_2581
  (make-struct-type-install-properties
   '(parse-config)
   7
   0
   #f
   null
   (current-inspector)
   #f
   '(0 1 2 3 4 5 6)
   #f
   'parse-config))
(define struct:parse-config
  (make-record-type-descriptor
   'parse-config
   #f
   (|#%nongenerative-uid| parse-config)
   #f
   #f
   '(7 . 0)))
(define effect_2622 (finish_2581 struct:parse-config))
(define parse-config1.1
  (|#%name|
   parse-config
   (record-constructor
    (make-record-constructor-descriptor struct:parse-config #f #f))))
(define parse-config?_2003
  (|#%name| parse-config? (record-predicate struct:parse-config)))
(define parse-config?
  (|#%name|
   parse-config?
   (lambda (v)
     (if (parse-config?_2003 v)
       #t
       ($value
        (if (impersonator? v)
          (parse-config?_2003 (impersonator-val v))
          #f))))))
(define parse-config-who_2100
  (|#%name| parse-config-who (record-accessor struct:parse-config 0)))
(define parse-config-who
  (|#%name|
   parse-config-who
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-who_2100 s)
       ($value
        (impersonate-ref
         parse-config-who_2100
         struct:parse-config
         0
         s
         'who))))))
(define parse-config-px?_2965
  (|#%name| parse-config-px? (record-accessor struct:parse-config 1)))
(define parse-config-px?
  (|#%name|
   parse-config-px?
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-px?_2965 s)
       ($value
        (impersonate-ref
         parse-config-px?_2965
         struct:parse-config
         1
         s
         'px?))))))
(define parse-config-case-sensitive?_2517
  (|#%name|
   parse-config-case-sensitive?
   (record-accessor struct:parse-config 2)))
(define parse-config-case-sensitive?
  (|#%name|
   parse-config-case-sensitive?
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-case-sensitive?_2517 s)
       ($value
        (impersonate-ref
         parse-config-case-sensitive?_2517
         struct:parse-config
         2
         s
         'case-sensitive?))))))
(define parse-config-multi-line?_2034
  (|#%name| parse-config-multi-line? (record-accessor struct:parse-config 3)))
(define parse-config-multi-line?
  (|#%name|
   parse-config-multi-line?
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-multi-line?_2034 s)
       ($value
        (impersonate-ref
         parse-config-multi-line?_2034
         struct:parse-config
         3
         s
         'multi-line?))))))
(define parse-config-group-number-box_2275
  (|#%name|
   parse-config-group-number-box
   (record-accessor struct:parse-config 4)))
(define parse-config-group-number-box
  (|#%name|
   parse-config-group-number-box
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-group-number-box_2275 s)
       ($value
        (impersonate-ref
         parse-config-group-number-box_2275
         struct:parse-config
         4
         s
         'group-number-box))))))
(define parse-config-references?-box_2624
  (|#%name|
   parse-config-references?-box
   (record-accessor struct:parse-config 5)))
(define parse-config-references?-box
  (|#%name|
   parse-config-references?-box
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-references?-box_2624 s)
       ($value
        (impersonate-ref
         parse-config-references?-box_2624
         struct:parse-config
         5
         s
         'references?-box))))))
(define parse-config-error-handler?_2788
  (|#%name|
   parse-config-error-handler?
   (record-accessor struct:parse-config 6)))
(define parse-config-error-handler?
  (|#%name|
   parse-config-error-handler?
   (lambda (s)
     (if (parse-config?_2003 s)
       (parse-config-error-handler?_2788 s)
       ($value
        (impersonate-ref
         parse-config-error-handler?_2788
         struct:parse-config
         6
         s
         'error-handler?))))))
(define make-parse-config.1
  (|#%name|
   make-parse-config
   (lambda (error-handler?4_0 px?3_0 who2_0)
     (begin
       (parse-config1.1
        who2_0
        px?3_0
        #t
        #f
        (box 0)
        (box #f)
        error-handler?4_0)))))
(define config-case-sensitive
  (lambda (config_0 cs?_0)
    (if (parse-config? config_0)
      (let ((app_0 (parse-config-who config_0)))
        (let ((app_1 (parse-config-px? config_0)))
          (let ((app_2 (parse-config-multi-line? config_0)))
            (let ((app_3 (parse-config-group-number-box config_0)))
              (let ((app_4 (parse-config-references?-box config_0)))
                (parse-config1.1
                 app_0
                 app_1
                 cs?_0
                 app_2
                 app_3
                 app_4
                 (parse-config-error-handler? config_0)))))))
      (raise-argument-error 'struct-copy "parse-config?" config_0))))
(define config-multi-line
  (lambda (config_0 mm?_0)
    (if (parse-config? config_0)
      (let ((app_0 (parse-config-who config_0)))
        (let ((app_1 (parse-config-px? config_0)))
          (let ((app_2 (parse-config-case-sensitive? config_0)))
            (let ((app_3 (parse-config-group-number-box config_0)))
              (let ((app_4 (parse-config-references?-box config_0)))
                (parse-config1.1
                 app_0
                 app_1
                 app_2
                 mm?_0
                 app_3
                 app_4
                 (parse-config-error-handler? config_0)))))))
      (raise-argument-error 'struct-copy "parse-config?" config_0))))
(define config-group-number
  (lambda (config_0) (unbox (parse-config-group-number-box config_0))))
(define config-group-number+1
  (lambda (config_0)
    (let ((b_0 (parse-config-group-number-box config_0)))
      (begin (set-box! b_0 (add1 (unbox b_0))) config_0))))
(define parse-error
  (lambda (s_0 pos_0 config_0 fmt_0 . args_0)
    (apply regexp-error fmt_0 args_0)))
(define parse-class
  (lambda (s_0 pos_0 config_0)
    (let ((success_0
           (|#%name|
            success
            (lambda (v_0) (begin (values #t v_0 (add1 pos_0)))))))
      (let ((tmp_0 (integer->char (chytes-ref$1 s_0 pos_0))))
        (if (eqv? tmp_0 '#\x64)
          (success_0 (range:d))
          (if (eqv? tmp_0 '#\x44)
            (success_0
             (let ((app_0 (range:d))) (range-invert app_0 (chytes-limit s_0))))
            (if (eqv? tmp_0 '#\x77)
              (success_0 (range:w))
              (if (eqv? tmp_0 '#\x57)
                (success_0
                 (let ((app_0 (range:w)))
                   (range-invert app_0 (chytes-limit s_0))))
                (if (eqv? tmp_0 '#\x73)
                  (success_0 (range:s))
                  (if (eqv? tmp_0 '#\x53)
                    (success_0
                     (let ((app_0 (range:s)))
                       (range-invert app_0 (chytes-limit s_0))))
                    (values #f #f #f)))))))))))
(define range:d
  (lambda () (begin-unsafe (range-union null (list (cons 48 57))))))
(define range:w
  (lambda ()
    (range-add
     (let ((range_0
            (let ((range_0 (range:d)))
              (begin-unsafe (range-union range_0 (list (cons 97 122)))))))
       (begin-unsafe (range-union range_0 (list (cons 65 90)))))
     95)))
(define range:s
  (lambda ()
    (let ((r_0 (range-add null 32)))
      (let ((r_1 (range-add r_0 9)))
        (let ((r_2 (range-add r_1 10)))
          (let ((r_3 (range-add r_2 12)))
            (let ((r_4 (range-add r_3 13))) r_4)))))))
(define parse-posix-char-class
  (lambda (s_0 pos_0)
    (let ((pos_1 pos_0))
      (let ((tmp_0
             (if (= pos_1 (chytes-length$1 s_0))
               'eos
               (chytes-ref/char s_0 pos_1))))
        (if (eqv? tmp_0 '#\x3a)
          (let ((class_0
                 (letrec*
                  ((loop_0
                    (|#%name|
                     loop
                     (lambda (accum_0 pos_2)
                       (begin
                         (if (= pos_2 (chytes-length$1 s_0))
                           #f
                           (let ((c_0 (chytes-ref$1 s_0 pos_2)))
                             (if (if (>= c_0 97) (<= c_0 122) #f)
                               (let ((app_0 (cons c_0 accum_0)))
                                 (loop_0 app_0 (add1 pos_2)))
                               (if (if (= c_0 58)
                                     (if (let ((app_0 (add1 pos_2)))
                                           (< app_0 (chytes-length$1 s_0)))
                                       (= (chytes-ref$1 s_0 (add1 pos_2)) 93)
                                       #f)
                                     #f)
                                 (list->bytes (reverse$1 accum_0))
                                 #f)))))))))
                  (loop_0 null (add1 pos_0)))))
            (let ((range_0
                   (let ((index_0 (hash-ref hash1688 class_0 (lambda () 0))))
                     (if (unsafe-fx< index_0 6)
                       (if (unsafe-fx< index_0 2)
                         (if (unsafe-fx< index_0 1)
                           #f
                           (let ((range_0
                                  (begin-unsafe
                                   (range-union null (list (cons 97 122))))))
                             (begin-unsafe
                              (range-union range_0 (list (cons 65 90))))))
                         (if (unsafe-fx< index_0 3)
                           (begin-unsafe
                            (range-union null (list (cons 65 90))))
                           (if (unsafe-fx< index_0 4)
                             (begin-unsafe
                              (range-union null (list (cons 97 122))))
                             (if (unsafe-fx< index_0 5)
                               (begin-unsafe
                                (range-union null (list (cons 48 57))))
                               (let ((range_0
                                      (let ((range_0
                                             (begin-unsafe
                                              (range-union
                                               null
                                               (list (cons 48 57))))))
                                        (begin-unsafe
                                         (range-union
                                          range_0
                                          (list (cons 97 102)))))))
                                 (begin-unsafe
                                  (range-union
                                   range_0
                                   (list (cons 65 70)))))))))
                       (if (unsafe-fx< index_0 9)
                         (if (unsafe-fx< index_0 7)
                           (let ((range_0
                                  (let ((range_0
                                         (begin-unsafe
                                          (range-union
                                           null
                                           (list (cons 48 57))))))
                                    (begin-unsafe
                                     (range-union
                                      range_0
                                      (list (cons 97 122)))))))
                             (begin-unsafe
                              (range-union range_0 (list (cons 65 90)))))
                           (if (unsafe-fx< index_0 8)
                             (range-add
                              (let ((range_0
                                     (let ((range_0
                                            (begin-unsafe
                                             (range-union
                                              null
                                              (list (cons 48 57))))))
                                       (begin-unsafe
                                        (range-union
                                         range_0
                                         (list (cons 97 122)))))))
                                (begin-unsafe
                                 (range-union range_0 (list (cons 65 90)))))
                              95)
                             (range-add (range-add null 32) 9)))
                         (if (unsafe-fx< index_0 10)
                           (range:s)
                           (if (unsafe-fx< index_0 11)
                             (let ((range_0
                                    (begin
                                      (letrec*
                                       ((for-loop_0
                                         (|#%name|
                                          for-loop
                                          (lambda (range_0 pos_2)
                                            (begin
                                              (if (unsafe-fx< pos_2 128)
                                                (let ((range_1
                                                       (let ((range_1
                                                              (if (char-graphic?
                                                                   (integer->char
                                                                    pos_2))
                                                                (range-add
                                                                 range_0
                                                                 pos_2)
                                                                range_0)))
                                                         (values range_1))))
                                                  (for-loop_0
                                                   range_1
                                                   (unsafe-fx+ pos_2 1)))
                                                range_0))))))
                                       (for-loop_0 null 0)))))
                               (if (equal? class_0 #vu8(112 114 105 110 116))
                                 (range-add (range-add range_0 32) 9)
                                 range_0))
                             (if (unsafe-fx< index_0 12)
                               (begin-unsafe
                                (range-union null (list (cons 0 31))))
                               (begin-unsafe
                                (range-union null (list (cons 0 127))))))))))))
              (if range_0
                (values
                 #t
                 range_0
                 (let ((app_0 pos_0))
                   (+ app_0 3 (unsafe-bytes-length class_0))))
                (values #f #f #f))))
          (values #f #f #f))))))
(define xor (lambda (a_0 b_0) (if a_0 (if b_0 #f a_0) b_0)))
(define parse-unicode-categories
  (lambda (p-c_0 s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eqv? tmp_0 '#\x7b)
        (call-with-values
         (lambda ()
           (let ((pos_1 (add1 pos_0)))
             (let ((tmp_1
                    (if (= pos_1 (chytes-length$1 s_0))
                      'eos
                      (chytes-ref/char s_0 pos_1))))
               (if (eqv? tmp_1 '#\x5e)
                 (values #t (+ pos_0 2))
                 (values #f (add1 pos_0))))))
         (case-lambda
          ((cat-negated?_0 next-pos_0)
           (call-with-values
            (lambda ()
              (letrec*
               ((loop_0
                 (|#%name|
                  loop
                  (lambda (accum_0 pos_1)
                    (begin
                      (let ((tmp_1
                             (if (= pos_1 (chytes-length$1 s_0))
                               'eos
                               (chytes-ref/char s_0 pos_1))))
                        (if (eq? tmp_1 'eos)
                          (let ((fmt_0 "missing `}` to close `\\~a{`"))
                            (let ((args_0 (list (integer->char p-c_0))))
                              (let ((fmt_1 fmt_0))
                                (begin-unsafe
                                 (apply regexp-error fmt_1 args_0)))))
                          (if (eqv? tmp_1 '#\x7d)
                            (let ((app_0 (reverse$1 accum_0)))
                              (values app_0 (add1 pos_1)))
                            (let ((app_0
                                   (cons (chytes-ref$1 s_0 pos_1) accum_0)))
                              (loop_0 app_0 (add1 pos_1)))))))))))
               (loop_0 null next-pos_0)))
            (case-lambda
             ((l_0 pos2_0)
              (let ((categories_0
                     (let ((tmp_1 (list->bytes l_0)))
                       (let ((index_0
                              (hash-ref hash2956 tmp_1 (lambda () 0))))
                         (if (unsafe-fx< index_0 19)
                           (if (unsafe-fx< index_0 9)
                             (if (unsafe-fx< index_0 4)
                               (if (unsafe-fx< index_0 1)
                                 (let ((fmt_0
                                        "unrecognized property name in `\\~a{}`: `~a`"))
                                   (let ((args_0
                                          (let ((app_0 (integer->char p-c_0)))
                                            (list
                                             app_0
                                             (list->string
                                              (map_1346 integer->char l_0))))))
                                     (let ((fmt_1 fmt_0))
                                       (begin-unsafe
                                        (apply regexp-error fmt_1 args_0)))))
                                 (if (unsafe-fx< index_0 2)
                                   'll
                                   (if (unsafe-fx< index_0 3) 'lu 'lt)))
                               (if (unsafe-fx< index_0 6)
                                 (if (unsafe-fx< index_0 5) 'lm '(ll lu lt lm))
                                 (if (unsafe-fx< index_0 7)
                                   'lo
                                   (if (unsafe-fx< index_0 8)
                                     '(ll lu lt lm lo)
                                     'nd))))
                             (if (unsafe-fx< index_0 13)
                               (if (unsafe-fx< index_0 10)
                                 'nl
                                 (if (unsafe-fx< index_0 11)
                                   'no
                                   (if (unsafe-fx< index_0 12)
                                     '(nd nl no)
                                     'ps)))
                               (if (unsafe-fx< index_0 15)
                                 (if (unsafe-fx< index_0 14) 'pe 'pi)
                                 (if (unsafe-fx< index_0 16)
                                   'pf
                                   (if (unsafe-fx< index_0 17)
                                     'pc
                                     (if (unsafe-fx< index_0 18) 'pd 'po))))))
                           (if (unsafe-fx< index_0 29)
                             (if (unsafe-fx< index_0 23)
                               (if (unsafe-fx< index_0 20)
                                 '(ps pe pi pf pc pd po)
                                 (if (unsafe-fx< index_0 21)
                                   'mn
                                   (if (unsafe-fx< index_0 22) 'mc 'me)))
                               (if (unsafe-fx< index_0 25)
                                 (if (unsafe-fx< index_0 24) '(mn mc me) 'sc)
                                 (if (unsafe-fx< index_0 26)
                                   'sk
                                   (if (unsafe-fx< index_0 27)
                                     'sm
                                     (if (unsafe-fx< index_0 28)
                                       'so
                                       '(sc sk sm so))))))
                             (if (unsafe-fx< index_0 34)
                               (if (unsafe-fx< index_0 31)
                                 (if (unsafe-fx< index_0 30) 'zl 'zp)
                                 (if (unsafe-fx< index_0 32)
                                   'zs
                                   (if (unsafe-fx< index_0 33)
                                     '(zl zp zs)
                                     'cc)))
                               (if (unsafe-fx< index_0 36)
                                 (if (unsafe-fx< index_0 35) 'cf 'cs)
                                 (if (unsafe-fx< index_0 37)
                                   'cn
                                   (if (unsafe-fx< index_0 38)
                                     'co
                                     (if (unsafe-fx< index_0 39)
                                       '(cc cf cs cn so)
                                       #t)))))))))))
                (let ((prop-negated?_0 (= p-c_0 80)))
                  (values
                   (rx:unicode-categories12.1
                    categories_0
                    (not (xor prop-negated?_0 cat-negated?_0)))
                   pos2_0))))
             (args (raise-binding-result-arity-error 2 args)))))
          (args (raise-binding-result-arity-error 2 args))))
        (let ((fmt_0 "expected `{` after `\\~a`"))
          (let ((args_0 (list (integer->char p-c_0))))
            (let ((fmt_1 fmt_0))
              (begin-unsafe (apply regexp-error fmt_1 args_0)))))))))
(define range-add*
  (lambda (range_0 c_0 config_0)
    (if (not c_0)
      range_0
      (let ((range2_0 (range-add range_0 c_0)))
        (if (parse-config-case-sensitive? config_0)
          range2_0
          (let ((range3_0
                 (range-add
                  range2_0
                  (char->integer (char-upcase (integer->char c_0))))))
            (let ((range4_0
                   (range-add
                    range3_0
                    (char->integer (char-foldcase (integer->char c_0))))))
              (range-add
               range4_0
               (char->integer (char-downcase (integer->char c_0)))))))))))
(define range-add-span*
  (lambda (range_0 from-c_0 to-c_0 config_0)
    (if (parse-config-case-sensitive? config_0)
      (begin-unsafe (range-union range_0 (list (cons from-c_0 to-c_0))))
      (let ((end_0 (add1 to-c_0)))
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (range_1 pos_0)
                (begin
                  (if (< pos_0 end_0)
                    (let ((range_2
                           (let ((range_2 (range-add* range_1 pos_0 config_0)))
                             (values range_2))))
                      (for-loop_0 range_2 (+ pos_0 1)))
                    range_1))))))
           (for-loop_0 range_0 from-c_0)))))))
(define parse-range/not
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eq? tmp_0 'eos)
        (begin-unsafe
         (parse-error
          s_0
          pos_0
          config_0
          "missing closing square bracket in pattern"))
        (if (eqv? tmp_0 '#\x5e)
          (call-with-values
           (lambda () (parse-range s_0 (add1 pos_0) config_0))
           (case-lambda
            ((range_0 pos2_0)
             (values (range-invert range_0 (chytes-limit s_0)) pos2_0))
            (args (raise-binding-result-arity-error 2 args))))
          (parse-range s_0 pos_0 config_0))))))
(define parse-range
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eq? tmp_0 'eos)
        (begin-unsafe
         (parse-error
          s_0
          pos_0
          config_0
          "missing closing square bracket in pattern"))
        (if (eqv? tmp_0 '#\x5d)
          (let ((temp20_0 (range-add null 93)))
            (let ((temp22_0 (add1 pos_0)))
              (let ((temp20_1 temp20_0))
                (parse-range-rest.1 #f #f temp20_1 s_0 temp22_0 config_0))))
          (if (eqv? tmp_0 '#\x2d)
            (let ((temp24_0 (range-add null 45)))
              (let ((temp26_0 (add1 pos_0)))
                (let ((temp24_1 temp24_0))
                  (parse-range-rest.1 #f #f temp24_1 s_0 temp26_0 config_0))))
            (parse-range-rest.1 #f #f null s_0 pos_0 config_0)))))))
(define parse-range-rest.1
  (|#%name|
   parse-range-rest
   (lambda (must-span-from2_0 span-from1_0 range5_0 s6_0 pos7_0 config8_0)
     (begin
       (let ((tmp_0
              (if (= pos7_0 (chytes-length$1 s6_0))
                'eos
                (chytes-ref/char s6_0 pos7_0))))
         (if (eq? tmp_0 'eos)
           (begin-unsafe
            (parse-error
             s6_0
             pos7_0
             config8_0
             "missing closing square bracket in pattern"))
           (if (eqv? tmp_0 '#\x5d)
             (let ((app_0 (range-add* range5_0 span-from1_0 config8_0)))
               (values app_0 (add1 pos7_0)))
             (if (eqv? tmp_0 '#\x2d)
               (let ((pos2_0 (add1 pos7_0)))
                 (let ((tmp_1
                        (if (= pos2_0 (chytes-length$1 s6_0))
                          'eos
                          (chytes-ref/char s6_0 pos2_0))))
                   (if (eq? tmp_1 'eos)
                     (let ((pos_0 (add1 pos2_0)))
                       (begin-unsafe
                        (parse-error
                         s6_0
                         pos_0
                         config8_0
                         "missing closing square bracket in pattern")))
                     (if (eqv? tmp_1 '#\x5d)
                       (if must-span-from2_0
                         (begin-unsafe
                          (parse-error
                           s6_0
                           pos7_0
                           config8_0
                           "misplaced hyphen within square brackets in pattern"))
                         (let ((app_0
                                (range-add
                                 (range-add* range5_0 span-from1_0 config8_0)
                                 45)))
                           (values app_0 (add1 pos2_0))))
                       (if span-from1_0
                         (parse-range-rest.1
                          span-from1_0
                          #f
                          range5_0
                          s6_0
                          pos2_0
                          config8_0)
                         (begin-unsafe
                          (parse-error
                           s6_0
                           pos7_0
                           config8_0
                           "misplaced hyphen within square brackets in pattern")))))))
               (if (eqv? tmp_0 '#\x5c)
                 (if (parse-config-px? config8_0)
                   (let ((pos2_0 (add1 pos7_0)))
                     (if (= pos2_0 (chytes-length$1 s6_0))
                       (parse-error
                        s6_0
                        pos7_0
                        config8_0
                        "escaping backslash at end pattern (within square brackets)")
                       (let ((c_0 (chytes-ref$1 s6_0 pos2_0)))
                         (if (let ((or-part_0
                                    (if (>= c_0 97) (<= c_0 122) #f)))
                               (if or-part_0
                                 or-part_0
                                 (if (>= c_0 65) (<= c_0 90) #f)))
                           (if must-span-from2_0
                             (parse-error
                              s6_0
                              pos7_0
                              config8_0
                              "misplaced hyphen within square brackets in pattern")
                             (call-with-values
                              (lambda () (parse-class s6_0 pos2_0 config8_0))
                              (case-lambda
                               ((success?_0 range1_0 pos3_0)
                                (begin
                                  (if success?_0
                                    (void)
                                    (parse-error
                                     s6_0
                                     pos3_0
                                     config8_0
                                     "illegal alphabetic escape"))
                                  (let ((range2_0
                                         (range-union
                                          range1_0
                                          (range-add*
                                           range5_0
                                           span-from1_0
                                           config8_0))))
                                    (let ((temp39_0 (add1 pos2_0)))
                                      (parse-range-rest.1
                                       #f
                                       #f
                                       range2_0
                                       s6_0
                                       temp39_0
                                       config8_0)))))
                               (args
                                (raise-binding-result-arity-error 3 args)))))
                           (let ((temp44_0 (add1 pos2_0)))
                             (parse-range-rest/span.1
                              must-span-from2_0
                              span-from1_0
                              c_0
                              range5_0
                              s6_0
                              temp44_0
                              config8_0))))))
                   (let ((temp51_0 (add1 pos7_0)))
                     (parse-range-rest/span.1
                      must-span-from2_0
                      span-from1_0
                      92
                      range5_0
                      s6_0
                      temp51_0
                      config8_0)))
                 (if (eqv? tmp_0 '#\x5b)
                   (call-with-values
                    (lambda ()
                      (if (if (parse-config-px? config8_0)
                            (not must-span-from2_0)
                            #f)
                        (parse-posix-char-class s6_0 (add1 pos7_0))
                        (values #f #f #f)))
                    (case-lambda
                     ((success?_0 range1_0 pos2_0)
                      (if success?_0
                        (let ((range2_0
                               (range-union
                                range1_0
                                (range-add* range5_0 span-from1_0 config8_0))))
                          (parse-range-rest.1
                           #f
                           #f
                           range2_0
                           s6_0
                           pos2_0
                           config8_0))
                        (let ((temp62_0 (add1 pos7_0)))
                          (parse-range-rest/span.1
                           must-span-from2_0
                           span-from1_0
                           91
                           range5_0
                           s6_0
                           temp62_0
                           config8_0))))
                     (args (raise-binding-result-arity-error 3 args))))
                   (let ((temp66_0 (chytes-ref$1 s6_0 pos7_0)))
                     (let ((temp69_0 (add1 pos7_0)))
                       (let ((temp66_1 temp66_0))
                         (parse-range-rest/span.1
                          must-span-from2_0
                          span-from1_0
                          temp66_1
                          range5_0
                          s6_0
                          temp69_0
                          config8_0))))))))))))))
(define parse-range-rest/span.1
  (|#%name|
   parse-range-rest/span
   (lambda (must-span-from11_0
            span-from10_0
            c14_0
            range15_0
            s16_0
            pos17_0
            config18_0)
     (begin
       (if must-span-from11_0
         (if (> must-span-from11_0 c14_0)
           (parse-error
            s16_0
            pos17_0
            config18_0
            "invalid range within square brackets in pattern")
           (let ((temp73_0
                  (range-add-span*
                   range15_0
                   must-span-from11_0
                   c14_0
                   config18_0)))
             (parse-range-rest.1 #f #f temp73_0 s16_0 pos17_0 config18_0)))
         (let ((temp77_0 (range-add* range15_0 span-from10_0 config18_0)))
           (parse-range-rest.1
            #f
            c14_0
            temp77_0
            s16_0
            pos17_0
            config18_0)))))))
(define missing-square-closing-error
  (lambda (s_0 pos_0 config_0)
    (parse-error
     s_0
     pos_0
     config_0
     "missing closing square bracket in pattern")))
(define misplaced-hyphen-error
  (lambda (s_0 pos_0 config_0)
    (parse-error
     s_0
     pos_0
     config_0
     "misplaced hyphen within square brackets in pattern")))
(define parse.1
  (|#%name|
   parse
   (lambda (px?1_0 p3_0)
     (begin
       (let ((config_0 (make-parse-config.1 #f px?1_0 'regexp)))
         (call-with-values
          (lambda () (parse-regexp.1 unsafe-undefined p3_0 0 config_0))
          (case-lambda
           ((rx_0 pos_0)
            (let ((pos_1 pos_0))
              (let ((tmp_0
                     (if (let ((app_0 pos_1)) (= app_0 (chytes-length$1 p3_0)))
                       'eos
                       (chytes-ref/char p3_0 pos_1))))
                (if (eqv? tmp_0 '#\x29)
                  (parse-error p3_0 pos_0 config_0 "unmatched `)` in pattern")
                  (let ((app_0
                         (begin-unsafe
                          (unbox (parse-config-group-number-box config_0)))))
                    (values
                     rx_0
                     app_0
                     (unbox (parse-config-references?-box config_0))))))))
           (args (raise-binding-result-arity-error 2 args)))))))))
(define parse-regexp.1
  (|#%name|
   parse-regexp
   (lambda (parse-regexp5_0 s7_0 pos8_0 config9_0)
     (begin
       (let ((parse-regexp_0
              (if (eq? parse-regexp5_0 unsafe-undefined)
                (|#%name|
                 parse-regexp
                 (lambda (s_0 pos_0 config_0)
                   (begin
                     (parse-regexp.1 unsafe-undefined s_0 pos_0 config_0))))
                parse-regexp5_0)))
         (call-with-values
          (lambda () (parse-pces s7_0 pos8_0 config9_0))
          (case-lambda
           ((rxs_0 pos2_0)
            (let ((tmp_0
                   (if (= pos2_0 (chytes-length$1 s7_0))
                     'eos
                     (chytes-ref/char s7_0 pos2_0))))
              (if (eqv? tmp_0 '#\x7c)
                (call-with-values
                 (lambda ()
                   (|#%app| parse-regexp_0 s7_0 (add1 pos2_0) config9_0))
                 (case-lambda
                  ((rx_0 pos3_0)
                   (values
                    (let ((app_0 (rx-sequence rxs_0)))
                      (rx-alts app_0 rx_0 (chytes-limit s7_0)))
                    pos3_0))
                  (args (raise-binding-result-arity-error 2 args))))
                (values (rx-sequence rxs_0) pos2_0))))
           (args (raise-binding-result-arity-error 2 args)))))))))
(define parse-regexp/maybe-empty
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eqv? tmp_0 '#\x29)
        (values 'empty pos_0)
        (parse-regexp.1 parse-regexp/maybe-empty s_0 pos_0 config_0)))))
(define parse-pces
  (lambda (s_0 pos_0 config_0)
    (if (= pos_0 (chytes-length$1 s_0))
      (values null pos_0)
      (call-with-values
       (lambda () (parse-pce s_0 pos_0 config_0))
       (case-lambda
        ((rx_0 pos2_0)
         (let ((tmp_0
                (if (= pos2_0 (chytes-length$1 s_0))
                  'eos
                  (chytes-ref/char s_0 pos2_0))))
           (if (eq? tmp_0 'eos)
             (values (list rx_0) pos2_0)
             (if (if (eqv? tmp_0 '#\x7c) #t (eqv? tmp_0 '#\x29))
               (values (list rx_0) pos2_0)
               (call-with-values
                (lambda () (parse-pces s_0 pos2_0 config_0))
                (case-lambda
                 ((rxs_0 pos3_0) (values (cons rx_0 rxs_0) pos3_0))
                 (args (raise-binding-result-arity-error 2 args))))))))
        (args (raise-binding-result-arity-error 2 args)))))))
(define parse-pce
  (lambda (s_0 pos_0 config_0)
    (call-with-values
     (lambda () (parse-atom s_0 pos_0 config_0))
     (case-lambda
      ((rx_0 pos2_0)
       (let ((tmp_0
              (if (= pos2_0 (chytes-length$1 s_0))
                'eos
                (chytes-ref/char s_0 pos2_0))))
         (if (eqv? tmp_0 '#\x2a)
           (call-with-values
            (lambda () (parse-non-greedy s_0 (add1 pos2_0) config_0))
            (case-lambda
             ((non-greedy?_0 pos3_0)
              (values (rx:repeat4.1 rx_0 0 +inf.0 non-greedy?_0) pos3_0))
             (args (raise-binding-result-arity-error 2 args))))
           (if (eqv? tmp_0 '#\x2b)
             (call-with-values
              (lambda () (parse-non-greedy s_0 (add1 pos2_0) config_0))
              (case-lambda
               ((non-greedy?_0 pos3_0)
                (values (rx:repeat4.1 rx_0 1 +inf.0 non-greedy?_0) pos3_0))
               (args (raise-binding-result-arity-error 2 args))))
             (if (eqv? tmp_0 '#\x3f)
               (call-with-values
                (lambda () (parse-non-greedy s_0 (add1 pos2_0) config_0))
                (case-lambda
                 ((non-greedy?_0 pos3_0)
                  (values (rx:maybe5.1 rx_0 non-greedy?_0) pos3_0))
                 (args (raise-binding-result-arity-error 2 args))))
               (if (eqv? tmp_0 '#\x7b)
                 (if (parse-config-px? config_0)
                   (call-with-values
                    (lambda () (parse-integer 0 s_0 (add1 pos2_0) config_0))
                    (case-lambda
                     ((n1_0 pos3_0)
                      (let ((tmp_1
                             (if (= pos3_0 (chytes-length$1 s_0))
                               'eos
                               (chytes-ref/char s_0 pos3_0))))
                        (if (eqv? tmp_1 '#\x2c)
                          (call-with-values
                           (lambda ()
                             (parse-integer 0 s_0 (add1 pos3_0) config_0))
                           (case-lambda
                            ((n2_0 pos4_0)
                             (let ((tmp_2
                                    (if (= pos4_0 (chytes-length$1 s_0))
                                      'eos
                                      (chytes-ref/char s_0 pos4_0))))
                               (if (eqv? tmp_2 '#\x7d)
                                 (let ((n2*_0
                                        (if (= pos4_0 (add1 pos3_0))
                                          +inf.0
                                          n2_0)))
                                   (call-with-values
                                    (lambda ()
                                      (parse-non-greedy
                                       s_0
                                       (add1 pos4_0)
                                       config_0))
                                    (case-lambda
                                     ((non-greedy?_0 pos5_0)
                                      (values
                                       (rx:repeat4.1
                                        rx_0
                                        n1_0
                                        n2*_0
                                        non-greedy?_0)
                                       pos5_0))
                                     (args
                                      (raise-binding-result-arity-error
                                       2
                                       args)))))
                                 (parse-error
                                  s_0
                                  pos3_0
                                  config_0
                                  "expected digit or `}` to end repetition specification started with `{`"))))
                            (args (raise-binding-result-arity-error 2 args))))
                          (if (eqv? tmp_1 '#\x7d)
                            (call-with-values
                             (lambda ()
                               (parse-non-greedy s_0 (add1 pos3_0) config_0))
                             (case-lambda
                              ((non-greedy?_0 pos4_0)
                               (values
                                (rx:repeat4.1 rx_0 n1_0 n1_0 non-greedy?_0)
                                pos4_0))
                              (args
                               (raise-binding-result-arity-error 2 args))))
                            (parse-error
                             s_0
                             pos3_0
                             config_0
                             "expected digit, `,`, or `}' for repetition specification started with `{`")))))
                     (args (raise-binding-result-arity-error 2 args))))
                   (values rx_0 pos2_0))
                 (values rx_0 pos2_0)))))))
      (args (raise-binding-result-arity-error 2 args))))))
(define parse-non-greedy
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eqv? tmp_0 '#\x3f)
        (values #t (check-not-nested s_0 (add1 pos_0) config_0))
        (values #f (check-not-nested s_0 pos_0 config_0))))))
(define check-not-nested
  (lambda (s_0 pos_0 config_0)
    (begin
      (let ((tmp_0
             (if (= pos_0 (chytes-length$1 s_0))
               'eos
               (chytes-ref/char s_0 pos_0))))
        (if (if (eqv? tmp_0 '#\x3f)
              #t
              (if (eqv? tmp_0 '#\x2a) #t (eqv? tmp_0 '#\x2b)))
          (let ((fmt_0 "nested `~a` in pattern"))
            (let ((args_0 (list (integer->char (chytes-ref$1 s_0 pos_0)))))
              (let ((fmt_1 fmt_0))
                (begin-unsafe (apply regexp-error fmt_1 args_0)))))
          (if (eqv? tmp_0 '#\x7b)
            (if (parse-config-px? config_0)
              (parse-error s_0 pos_0 config_0 "nested `{` in pattern")
              (void))
            (void))))
      pos_0)))
(define parse-atom
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0 (integer->char (chytes-ref$1 s_0 pos_0))))
      (if (eqv? tmp_0 '#\x7c)
        (values 'empty pos_0)
        (if (eqv? tmp_0 '#\x28)
          (parse-parenthesized-atom s_0 (add1 pos_0) config_0)
          (if (eqv? tmp_0 '#\x5b)
            (call-with-values
             (lambda () (parse-range/not s_0 (add1 pos_0) config_0))
             (case-lambda
              ((range_0 pos2_0)
               (values (rx-range range_0 (chytes-limit s_0)) pos2_0))
              (args (raise-binding-result-arity-error 2 args))))
            (if (eqv? tmp_0 '#\x2e)
              (let ((rx_0
                     (if (parse-config-multi-line? config_0)
                       (let ((app_0
                              (let ((app_0 (range-add null 10)))
                                (range-invert app_0 (chytes-limit s_0)))))
                         (rx-range app_0 (chytes-limit s_0)))
                       'any)))
                (values rx_0 (add1 pos_0)))
              (if (eqv? tmp_0 '#\x5e)
                (let ((app_0
                       (if (parse-config-multi-line? config_0)
                         'line-start
                         'start)))
                  (values app_0 (add1 pos_0)))
                (if (eqv? tmp_0 '#\x24)
                  (let ((app_0
                         (if (parse-config-multi-line? config_0)
                           'line-end
                           'end)))
                    (values app_0 (add1 pos_0)))
                  (parse-literal s_0 pos_0 config_0))))))))))
(define parse-parenthesized-atom
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eq? tmp_0 'eos)
        (begin-unsafe
         (parse-error
          s_0
          pos_0
          config_0
          "missing closing parenthesis in pattern"))
        (if (eqv? tmp_0 '#\x3f)
          (let ((pos2_0 (add1 pos_0)))
            (let ((tmp_1
                   (if (= pos2_0 (chytes-length$1 s_0))
                     'eos
                     (chytes-ref/char s_0 pos2_0))))
              (if (eq? tmp_1 'eos)
                (begin-unsafe
                 (parse-error
                  s_0
                  pos2_0
                  config_0
                  "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`"))
                (if (eqv? tmp_1 '#\x3e)
                  (let ((pre-num-groups_0
                         (begin-unsafe
                          (unbox (parse-config-group-number-box config_0)))))
                    (call-with-values
                     (lambda ()
                       (parse-regexp/maybe-empty s_0 (add1 pos2_0) config_0))
                     (case-lambda
                      ((rx_0 pos3_0)
                       (let ((post-num-groups_0
                              (begin-unsafe
                               (unbox
                                (parse-config-group-number-box config_0)))))
                         (let ((app_0
                                (let ((num-n_0
                                       (- post-num-groups_0 pre-num-groups_0)))
                                  (begin-unsafe
                                   (rx:cut9.1
                                    rx_0
                                    pre-num-groups_0
                                    num-n_0
                                    (needs-backtrack? rx_0))))))
                           (values
                            app_0
                            (check-close-paren s_0 pos3_0 config_0)))))
                      (args (raise-binding-result-arity-error 2 args)))))
                  (if (eqv? tmp_1 '#\x28)
                    (parse-conditional s_0 (add1 pos2_0) config_0)
                    (if (if (eqv? tmp_1 '#\x69)
                          #t
                          (if (eqv? tmp_1 '#\x73)
                            #t
                            (if (eqv? tmp_1 '#\x6d)
                              #t
                              (if (eqv? tmp_1 '#\x2d)
                                #t
                                (eqv? tmp_1 '#\x3a)))))
                      (call-with-values
                       (lambda () (parse-mode s_0 pos2_0 config_0))
                       (case-lambda
                        ((config2_0 pos3_0)
                         (let ((tmp_2
                                (if (= pos3_0 (chytes-length$1 s_0))
                                  'eos
                                  (chytes-ref/char s_0 pos3_0))))
                           (if (eqv? tmp_2 '#\x3a)
                             (call-with-values
                              (lambda ()
                                (parse-regexp/maybe-empty
                                 s_0
                                 (add1 pos3_0)
                                 config2_0))
                              (case-lambda
                               ((rx_0 pos4_0)
                                (values
                                 rx_0
                                 (check-close-paren s_0 pos4_0 config2_0)))
                               (args
                                (raise-binding-result-arity-error 2 args))))
                             (parse-error
                              s_0
                              pos3_0
                              config2_0
                              (string-append
                               "expected `:` or another mode after `(?` and a mode sequence;\n"
                               " a mode is `i`, `-i`, `m`, `-m`, `s`, or `-s`")))))
                        (args (raise-binding-result-arity-error 2 args))))
                      (parse-look s_0 pos2_0 config_0)))))))
          (let ((group-number_0
                 (begin-unsafe
                  (unbox (parse-config-group-number-box config_0)))))
            (call-with-values
             (lambda ()
               (parse-regexp/maybe-empty
                s_0
                pos_0
                (config-group-number+1 config_0)))
             (case-lambda
              ((rx_0 pos2_0)
               (let ((app_0 (begin-unsafe (rx:group3.1 rx_0 group-number_0))))
                 (values app_0 (check-close-paren s_0 pos2_0 config_0))))
              (args (raise-binding-result-arity-error 2 args))))))))))
(define parse-look
  (lambda (s_0 pos2_0 config_0)
    (let ((pre-num-groups_0
           (begin-unsafe (unbox (parse-config-group-number-box config_0)))))
      (let ((span-num-groups_0
             (|#%name|
              span-num-groups
              (lambda ()
                (begin
                  (-
                   (begin-unsafe
                    (unbox (parse-config-group-number-box config_0)))
                   pre-num-groups_0))))))
        (let ((tmp_0 (integer->char (chytes-ref$1 s_0 pos2_0))))
          (if (eqv? tmp_0 '#\x3d)
            (call-with-values
             (lambda () (parse-regexp/maybe-empty s_0 (add1 pos2_0) config_0))
             (case-lambda
              ((rx_0 pos3_0)
               (let ((app_0
                      (rx:lookahead7.1
                       rx_0
                       #t
                       pre-num-groups_0
                       (span-num-groups_0))))
                 (values app_0 (check-close-paren s_0 pos3_0 config_0))))
              (args (raise-binding-result-arity-error 2 args))))
            (if (eqv? tmp_0 '#\x21)
              (call-with-values
               (lambda ()
                 (parse-regexp/maybe-empty s_0 (add1 pos2_0) config_0))
               (case-lambda
                ((rx_0 pos3_0)
                 (let ((app_0
                        (rx:lookahead7.1
                         rx_0
                         #f
                         pre-num-groups_0
                         (span-num-groups_0))))
                   (values app_0 (check-close-paren s_0 pos3_0 config_0))))
                (args (raise-binding-result-arity-error 2 args))))
              (if (eqv? tmp_0 '#\x3c)
                (let ((pos2+_0 (add1 pos2_0)))
                  (let ((tmp_1
                         (if (= pos2+_0 (chytes-length$1 s_0))
                           'eos
                           (chytes-ref/char s_0 pos2+_0))))
                    (if (eq? tmp_1 'eos)
                      (begin-unsafe
                       (parse-error
                        s_0
                        pos2+_0
                        config_0
                        "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`"))
                      (if (eqv? tmp_1 '#\x3d)
                        (call-with-values
                         (lambda ()
                           (parse-regexp/maybe-empty
                            s_0
                            (add1 pos2+_0)
                            config_0))
                         (case-lambda
                          ((rx_0 pos3_0)
                           (let ((app_0
                                  (rx:lookbehind8.1
                                   rx_0
                                   #t
                                   0
                                   0
                                   pre-num-groups_0
                                   (span-num-groups_0))))
                             (values
                              app_0
                              (check-close-paren s_0 pos3_0 config_0))))
                          (args (raise-binding-result-arity-error 2 args))))
                        (if (eqv? tmp_1 '#\x21)
                          (call-with-values
                           (lambda ()
                             (parse-regexp/maybe-empty
                              s_0
                              (add1 pos2+_0)
                              config_0))
                           (case-lambda
                            ((rx_0 pos3_0)
                             (let ((app_0
                                    (rx:lookbehind8.1
                                     rx_0
                                     #f
                                     0
                                     0
                                     pre-num-groups_0
                                     (span-num-groups_0))))
                               (values
                                app_0
                                (check-close-paren s_0 pos3_0 config_0))))
                            (args (raise-binding-result-arity-error 2 args))))
                          (begin-unsafe
                           (parse-error
                            s_0
                            pos2+_0
                            config_0
                            "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`")))))))
                (begin-unsafe
                 (parse-error
                  s_0
                  pos2_0
                  config_0
                  "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`"))))))))))
(define parse-conditional
  (lambda (s_0 pos_0 config_0)
    (let ((tst-pre-num-groups_0
           (begin-unsafe (unbox (parse-config-group-number-box config_0)))))
      (call-with-values
       (lambda () (parse-test s_0 pos_0 config_0))
       (case-lambda
        ((tst_0 pos2_0)
         (let ((tst-span-num-groups_0
                (-
                 (begin-unsafe
                  (unbox (parse-config-group-number-box config_0)))
                 tst-pre-num-groups_0)))
           (call-with-values
            (lambda () (parse-pces s_0 pos2_0 config_0))
            (case-lambda
             ((pces_0 pos3_0)
              (let ((tmp_0
                     (if (= pos3_0 (chytes-length$1 s_0))
                       'eos
                       (chytes-ref/char s_0 pos3_0))))
                (if (eq? tmp_0 'eos)
                  (begin-unsafe
                   (parse-error
                    s_0
                    pos3_0
                    config_0
                    "missing closing parenthesis in pattern"))
                  (if (eqv? tmp_0 '#\x7c)
                    (call-with-values
                     (lambda () (parse-pces s_0 (add1 pos3_0) config_0))
                     (case-lambda
                      ((pces2_0 pos4_0)
                       (let ((tmp_1
                              (if (= pos4_0 (chytes-length$1 s_0))
                                'eos
                                (chytes-ref/char s_0 pos4_0))))
                         (if (eq? tmp_1 'eos)
                           (begin-unsafe
                            (parse-error
                             s_0
                             pos4_0
                             config_0
                             "missing closing parenthesis in pattern"))
                           (if (eqv? tmp_1 '#\x29)
                             (let ((app_0
                                    (let ((app_0 (rx-sequence pces_0)))
                                      (rx-conditional
                                       tst_0
                                       app_0
                                       (rx-sequence pces2_0)
                                       tst-pre-num-groups_0
                                       tst-span-num-groups_0))))
                               (values app_0 (add1 pos4_0)))
                             (parse-error
                              s_0
                              pos4_0
                              config_0
                              "expected `)` to close `(?(...)...` after second branch")))))
                      (args (raise-binding-result-arity-error 2 args))))
                    (if (eqv? tmp_0 '#\x29)
                      (let ((app_0
                             (rx-conditional
                              tst_0
                              (rx-sequence pces_0)
                              'empty
                              tst-pre-num-groups_0
                              tst-span-num-groups_0)))
                        (values app_0 (add1 pos3_0)))
                      (void))))))
             (args (raise-binding-result-arity-error 2 args))))))
        (args (raise-binding-result-arity-error 2 args)))))))
(define parse-test
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eq? tmp_0 'eos)
        (begin-unsafe
         (parse-error
          s_0
          pos_0
          config_0
          "missing closing parenthesis in pattern"))
        (if (eqv? tmp_0 '#\x3f)
          (parse-look s_0 (add1 pos_0) config_0)
          (let ((c_0 (chytes-ref$1 s_0 pos_0)))
            (if (if (>= c_0 48) (<= c_0 57) #f)
              (begin
                (set-box! (parse-config-references?-box config_0) #t)
                (call-with-values
                 (lambda () (parse-integer 0 s_0 pos_0 config_0))
                 (case-lambda
                  ((n_0 pos3_0)
                   (begin
                     (if (if (< pos3_0 (chytes-length$1 s_0))
                           (= (chytes-ref$1 s_0 pos3_0) 41)
                           #f)
                       (void)
                       (parse-error
                        s_0
                        pos3_0
                        config_0
                        "expected `)` after `(?(` followed by digits"))
                     (let ((app_0 (rx:reference10.1 n_0 #f)))
                       (values app_0 (add1 pos3_0)))))
                  (args (raise-binding-result-arity-error 2 args)))))
              (parse-error
               s_0
               pos_0
               config_0
               "expected `(?=`, `(?!`, `(?<`, or digit after `(?(`"))))))))
(define parse-integer
  (lambda (n_0 s_0 pos_0 config_0)
    (if (= pos_0 (chytes-length$1 s_0))
      (values n_0 pos_0)
      (let ((c_0 (chytes-ref$1 s_0 pos_0)))
        (if (if (>= c_0 48) (<= c_0 57) #f)
          (let ((n2_0 (let ((app_0 (* n_0 10))) (+ app_0 (- c_0 48)))))
            (parse-integer n2_0 s_0 (add1 pos_0) config_0))
          (values n_0 pos_0))))))
(define parse-literal
  (lambda (s_0 pos_0 config_0)
    (let ((c_0 (chytes-ref$1 s_0 pos_0)))
      (let ((tmp_0 (integer->char c_0)))
        (if (if (eqv? tmp_0 '#\x2a)
              #t
              (if (eqv? tmp_0 '#\x2b) #t (eqv? tmp_0 '#\x3f)))
          (let ((fmt_0 "`~a` follows nothing in pattern"))
            (let ((args_0 (list (integer->char c_0))))
              (let ((fmt_1 fmt_0))
                (begin-unsafe (apply regexp-error fmt_1 args_0)))))
          (if (eqv? tmp_0 '#\x7b)
            (if (parse-config-px? config_0)
              (parse-error s_0 pos_0 config_0 "`{` follows nothing in pattern")
              (values c_0 (add1 pos_0)))
            (if (eqv? tmp_0 '#\x5c)
              (parse-backslash-literal s_0 (add1 pos_0) config_0)
              (if (eqv? tmp_0 '#\x29)
                (parse-error s_0 pos_0 config_0 "unmatched `)` in pattern")
                (if (if (eqv? tmp_0 '#\x5d) #t (eqv? tmp_0 '#\x7d))
                  (if (parse-config-px? config_0)
                    (let ((fmt_0 "unmatched `~a` in pattern"))
                      (let ((args_0 (list (integer->char c_0))))
                        (let ((fmt_1 fmt_0))
                          (begin-unsafe (apply regexp-error fmt_1 args_0)))))
                    (values c_0 (add1 pos_0)))
                  (if (parse-config-case-sensitive? config_0)
                    (values c_0 (add1 pos_0))
                    (let ((app_0
                           (let ((app_0 (range-add* null c_0 config_0)))
                             (rx-range app_0 (chytes-limit s_0)))))
                      (values app_0 (add1 pos_0)))))))))))))
(define parse-backslash-literal
  (lambda (s_0 pos2_0 config_0)
    (if (= pos2_0 (chytes-length$1 s_0))
      (values 0 pos2_0)
      (let ((c2_0 (chytes-ref$1 s_0 pos2_0)))
        (if (if (parse-config-px? config_0)
              (if (>= c2_0 48) (<= c2_0 57) #f)
              #f)
          (begin
            (set-box! (parse-config-references?-box config_0) #t)
            (call-with-values
             (lambda () (parse-integer 0 s_0 pos2_0 config_0))
             (case-lambda
              ((n_0 pos3_0)
               (values
                (rx:reference10.1 n_0 (parse-config-case-sensitive? config_0))
                pos3_0))
              (args (raise-binding-result-arity-error 2 args)))))
          (if (if (parse-config-px? config_0)
                (let ((or-part_0 (if (>= c2_0 97) (<= c2_0 122) #f)))
                  (if or-part_0 or-part_0 (if (>= c2_0 65) (<= c2_0 90) #f)))
                #f)
            (let ((tmp_0 (integer->char c2_0)))
              (if (if (eqv? tmp_0 '#\x70) #t (eqv? tmp_0 '#\x50))
                (parse-unicode-categories c2_0 s_0 (add1 pos2_0) config_0)
                (if (eqv? tmp_0 '#\x62)
                  (values 'word-boundary (add1 pos2_0))
                  (if (eqv? tmp_0 '#\x42)
                    (values 'not-word-boundary (add1 pos2_0))
                    (call-with-values
                     (lambda () (parse-class s_0 pos2_0 config_0))
                     (case-lambda
                      ((success?_0 range_0 pos3_0)
                       (if success?_0
                         (values (rx-range range_0 (chytes-limit s_0)) pos3_0)
                         (parse-error
                          s_0
                          pos2_0
                          config_0
                          "illegal alphabetic escape")))
                      (args (raise-binding-result-arity-error 3 args))))))))
            (values c2_0 (add1 pos2_0))))))))
(define parse-mode
  (lambda (s_0 pos_0 config_0)
    (let ((tmp_0
           (if (= pos_0 (chytes-length$1 s_0))
             'eos
             (chytes-ref/char s_0 pos_0))))
      (if (eq? tmp_0 'eos)
        (values config_0 pos_0)
        (if (eqv? tmp_0 '#\x69)
          (let ((app_0 (add1 pos_0)))
            (parse-mode s_0 app_0 (config-case-sensitive config_0 #f)))
          (if (eqv? tmp_0 '#\x73)
            (let ((app_0 (add1 pos_0)))
              (parse-mode s_0 app_0 (config-multi-line config_0 #f)))
            (if (eqv? tmp_0 '#\x6d)
              (let ((app_0 (add1 pos_0)))
                (parse-mode s_0 app_0 (config-multi-line config_0 #t)))
              (if (eqv? tmp_0 '#\x2d)
                (let ((pos2_0 (add1 pos_0)))
                  (let ((tmp_1
                         (if (= pos2_0 (chytes-length$1 s_0))
                           'eos
                           (chytes-ref/char s_0 pos2_0))))
                    (if (eq? tmp_1 'eos)
                      (values config_0 pos_0)
                      (if (eqv? tmp_1 '#\x69)
                        (let ((app_0 (add1 pos2_0)))
                          (parse-mode
                           s_0
                           app_0
                           (config-case-sensitive config_0 #t)))
                        (if (eqv? tmp_1 '#\x73)
                          (let ((app_0 (add1 pos2_0)))
                            (parse-mode
                             s_0
                             app_0
                             (config-multi-line config_0 #t)))
                          (if (eqv? tmp_1 '#\x6d)
                            (let ((app_0 (add1 pos2_0)))
                              (parse-mode
                               s_0
                               app_0
                               (config-multi-line config_0 #f)))
                            (values config_0 pos_0)))))))
                (values config_0 pos_0)))))))))
(define check-close-paren
  (lambda (s_0 pos_0 config_0)
    (begin
      (if (if (< pos_0 (chytes-length$1 s_0))
            (= 41 (chytes-ref$1 s_0 pos_0))
            #f)
        (void)
        (parse-error s_0 pos_0 config_0 "expected a closing `)`"))
      (add1 pos_0))))
(define missing-closing-error
  (lambda (s_0 pos_0 config_0)
    (parse-error s_0 pos_0 config_0 "missing closing parenthesis in pattern")))
(define bad-?-sequence-error
  (lambda (s_0 pos_0 config_0)
    (parse-error
     s_0
     pos_0
     config_0
     "expected `:`, `=`, `!`, `<=`, `<!`, `i`, `-i`, `m`, `-m`, `s`, or `-s` after `(?`")))
(define validate
  (lambda (rx_0 num-groups_0)
    (let ((group-sizes_0 hash2589))
      (let ((depends-sizes_0 hash2589))
        (let ((must-sizes_0 hash2589))
          (let ((might-be-empty-error_0
                 (|#%name|
                  might-be-empty-error
                  (lambda ()
                    (begin
                      (regexp-error
                       "`*`, `+`, or `{...}` operand could be empty"))))))
            (call-with-values
             (lambda ()
               (letrec*
                ((validate_0
                  (|#%name|
                   validate
                   (lambda (rx_1)
                     (begin
                       (if (eq? rx_1 'never)
                         (values 1 1 0)
                         (if (let ((or-part_0 (eq? rx_1 'any)))
                               (if or-part_0
                                 or-part_0
                                 (let ((or-part_1 (exact-integer? rx_1)))
                                   (if or-part_1 or-part_1 (rx:range? rx_1)))))
                           (values 1 1 0)
                           (if (bytes? rx_1)
                             (let ((len_0 (unsafe-bytes-length rx_1)))
                               (values len_0 len_0 0))
                             (if (let ((or-part_0 (eq? rx_1 'empty)))
                                   (if or-part_0
                                     or-part_0
                                     (let ((or-part_1 (eq? rx_1 'end)))
                                       (if or-part_1
                                         or-part_1
                                         (eq? rx_1 'line-end)))))
                               (values 0 0 0)
                               (if (let ((or-part_0 (eq? rx_1 'start)))
                                     (if or-part_0
                                       or-part_0
                                       (eq? rx_1 'line-start)))
                                 (values 0 0 1)
                                 (if (let ((or-part_0
                                            (eq? rx_1 'word-boundary)))
                                       (if or-part_0
                                         or-part_0
                                         (eq? rx_1 'not-word-boundary)))
                                   (values 0 0 1)
                                   (if (rx:alts? rx_1)
                                     (call-with-values
                                      (lambda ()
                                        (validate_0 (rx:alts-rx_2265 rx_1)))
                                      (case-lambda
                                       ((min1_0 max1_0 lb1_0)
                                        (call-with-values
                                         (lambda ()
                                           (validate_0 (rx:alts-rx_1912 rx_1)))
                                         (case-lambda
                                          ((min2_0 max2_0 lb2_0)
                                           (let ((app_0 (min min1_0 min2_0)))
                                             (let ((app_1 (max max1_0 max2_0)))
                                               (values
                                                app_0
                                                app_1
                                                (max lb1_0 lb2_0)))))
                                          (args
                                           (raise-binding-result-arity-error
                                            3
                                            args)))))
                                       (args
                                        (raise-binding-result-arity-error
                                         3
                                         args))))
                                     (if (rx:sequence? rx_1)
                                       (let ((lst_0 (rx:sequence-rxs rx_1)))
                                         (begin
                                           (letrec*
                                            ((for-loop_0
                                              (|#%name|
                                               for-loop
                                               (lambda (min-len_0
                                                        max-len_0
                                                        max-lb_0
                                                        lst_1)
                                                 (begin
                                                   (if (pair? lst_1)
                                                     (let ((rx_2
                                                            (unsafe-car
                                                             lst_1)))
                                                       (let ((rest_0
                                                              (unsafe-cdr
                                                               lst_1)))
                                                         (call-with-values
                                                          (lambda ()
                                                            (call-with-values
                                                             (lambda ()
                                                               (call-with-values
                                                                (lambda ()
                                                                  (validate_0
                                                                   rx_2))
                                                                (case-lambda
                                                                 ((min1_0
                                                                   max1_0
                                                                   lb1_0)
                                                                  (let ((app_0
                                                                         (+
                                                                          min-len_0
                                                                          min1_0)))
                                                                    (let ((app_1
                                                                           (+
                                                                            max-len_0
                                                                            max1_0)))
                                                                      (values
                                                                       app_0
                                                                       app_1
                                                                       (max
                                                                        max-lb_0
                                                                        lb1_0)))))
                                                                 (args
                                                                  (raise-binding-result-arity-error
                                                                   3
                                                                   args)))))
                                                             (case-lambda
                                                              ((min-len_1
                                                                max-len_1
                                                                max-lb_1)
                                                               (values
                                                                min-len_1
                                                                max-len_1
                                                                max-lb_1))
                                                              (args
                                                               (raise-binding-result-arity-error
                                                                3
                                                                args)))))
                                                          (case-lambda
                                                           ((min-len_1
                                                             max-len_1
                                                             max-lb_1)
                                                            (for-loop_0
                                                             min-len_1
                                                             max-len_1
                                                             max-lb_1
                                                             rest_0))
                                                           (args
                                                            (raise-binding-result-arity-error
                                                             3
                                                             args))))))
                                                     (values
                                                      min-len_0
                                                      max-len_0
                                                      max-lb_0)))))))
                                            (for-loop_0 0 0 0 lst_0))))
                                       (if (rx:group? rx_1)
                                         (call-with-values
                                          (lambda ()
                                            (validate_0 (rx:group-rx rx_1)))
                                          (case-lambda
                                           ((min1_0 max1_0 lb1_0)
                                            (begin
                                              (set! group-sizes_0
                                                (let ((app_0 group-sizes_0))
                                                  (hash-set
                                                   app_0
                                                   (rx:group-number rx_1)
                                                   min1_0)))
                                              (values min1_0 max1_0 lb1_0)))
                                           (args
                                            (raise-binding-result-arity-error
                                             3
                                             args))))
                                         (if (rx:repeat? rx_1)
                                           (let ((old-depends-sizes_0
                                                  depends-sizes_0))
                                             (begin
                                               (set! depends-sizes_0
                                                 hash2589)
                                               (call-with-values
                                                (lambda ()
                                                  (validate_0
                                                   (rx:repeat-rx rx_1)))
                                                (case-lambda
                                                 ((min1_0 max1_0 lb1_0)
                                                  (begin
                                                    (if (zero? min1_0)
                                                      (might-be-empty-error_0)
                                                      (void))
                                                    (set! must-sizes_0
                                                      (let ((app_0
                                                             must-sizes_0))
                                                        (merge-depends-sizes
                                                         app_0
                                                         depends-sizes_0)))
                                                    (set! depends-sizes_0
                                                      (merge-depends-sizes
                                                       old-depends-sizes_0
                                                       depends-sizes_0))
                                                    (let ((app_0
                                                           (*
                                                            min1_0
                                                            (rx:repeat-min
                                                             rx_1))))
                                                      (values
                                                       app_0
                                                       (*
                                                        max1_0
                                                        (rx:repeat-max rx_1))
                                                       lb1_0))))
                                                 (args
                                                  (raise-binding-result-arity-error
                                                   3
                                                   args))))))
                                           (if (rx:maybe? rx_1)
                                             (call-with-values
                                              (lambda ()
                                                (validate_0
                                                 (rx:maybe-rx rx_1)))
                                              (case-lambda
                                               ((min1_0 max1_0 lb1_0)
                                                (values 0 max1_0 lb1_0))
                                               (args
                                                (raise-binding-result-arity-error
                                                 3
                                                 args))))
                                             (if (rx:conditional? rx_1)
                                               (call-with-values
                                                (lambda ()
                                                  (validate_0
                                                   (rx:conditional-tst rx_1)))
                                                (case-lambda
                                                 ((min0_0 max0_0 lb0_0)
                                                  (call-with-values
                                                   (lambda ()
                                                     (validate_0
                                                      (rx:conditional-rx_2162
                                                       rx_1)))
                                                   (case-lambda
                                                    ((min1_0 max1_0 lb1_0)
                                                     (call-with-values
                                                      (lambda ()
                                                        (validate_0
                                                         (rx:conditional-rx_2328
                                                          rx_1)))
                                                      (case-lambda
                                                       ((min2_0 max2_0 lb2_0)
                                                        (let ((app_0
                                                               (min
                                                                min1_0
                                                                min2_0)))
                                                          (let ((app_1
                                                                 (max
                                                                  max1_0
                                                                  max2_0)))
                                                            (values
                                                             app_0
                                                             app_1
                                                             (max
                                                              lb0_0
                                                              lb1_0
                                                              lb2_0)))))
                                                       (args
                                                        (raise-binding-result-arity-error
                                                         3
                                                         args)))))
                                                    (args
                                                     (raise-binding-result-arity-error
                                                      3
                                                      args)))))
                                                 (args
                                                  (raise-binding-result-arity-error
                                                   3
                                                   args))))
                                               (if (rx:lookahead? rx_1)
                                                 (call-with-values
                                                  (lambda ()
                                                    (validate_0
                                                     (rx:lookahead-rx rx_1)))
                                                  (case-lambda
                                                   ((min1_0 max1_0 lb1_0)
                                                    (values 0 0 lb1_0))
                                                   (args
                                                    (raise-binding-result-arity-error
                                                     3
                                                     args))))
                                                 (if (rx:lookbehind? rx_1)
                                                   (call-with-values
                                                    (lambda ()
                                                      (validate_0
                                                       (rx:lookbehind-rx
                                                        rx_1)))
                                                    (case-lambda
                                                     ((min1_0 max1_0 lb1_0)
                                                      (begin
                                                        (if (= +inf.0 max1_0)
                                                          (regexp-error
                                                           "lookbehind pattern does not match a bounded length")
                                                          (void))
                                                        (set-rx:lookbehind-lb-min!
                                                         rx_1
                                                         min1_0)
                                                        (set-rx:lookbehind-lb-max!
                                                         rx_1
                                                         max1_0)
                                                        (values
                                                         0
                                                         0
                                                         (max max1_0 lb1_0))))
                                                     (args
                                                      (raise-binding-result-arity-error
                                                       3
                                                       args))))
                                                   (if (rx:cut? rx_1)
                                                     (validate_0
                                                      (rx:cut-rx rx_1))
                                                     (if (rx:reference? rx_1)
                                                       (let ((n_0
                                                              (rx:reference-n
                                                               rx_1)))
                                                         (begin
                                                           (if (<=
                                                                n_0
                                                                num-groups_0)
                                                             (void)
                                                             (regexp-error
                                                              "backreference number is larger than the highest-numbered cluster"))
                                                           (let ((min-size_0
                                                                  (hash-ref
                                                                   group-sizes_0
                                                                   n_0
                                                                   #f)))
                                                             (if min-size_0
                                                               (values
                                                                min-size_0
                                                                +inf.0
                                                                0)
                                                               (begin
                                                                 (set! depends-sizes_0
                                                                   (let ((app_0
                                                                          depends-sizes_0))
                                                                     (hash-set
                                                                      app_0
                                                                      (sub1
                                                                       n_0)
                                                                      #t)))
                                                                 (values
                                                                  1
                                                                  +inf.0
                                                                  0))))))
                                                       (if (rx:unicode-categories?
                                                            rx_1)
                                                         (values 1 4 0)
                                                         (error
                                                          'validate
                                                          "internal error: ~s"
                                                          rx_1)))))))))))))))))))))))
                (validate_0 rx_0)))
             (case-lambda
              ((min-len_0 max-len_0 max-lookbehind_0)
               (begin
                 (let ((ht_0 must-sizes_0))
                   (begin
                     (letrec*
                      ((for-loop_0
                        (|#%name|
                         for-loop
                         (lambda (i_0)
                           (begin
                             (if i_0
                               (let ((n_0 (hash-iterate-key ht_0 i_0)))
                                 (begin
                                   (if (positive?
                                        (hash-ref group-sizes_0 n_0 0))
                                     (void)
                                     (might-be-empty-error_0))
                                   (for-loop_0 (hash-iterate-next ht_0 i_0))))
                               (values)))))))
                      (for-loop_0 (hash-iterate-first ht_0)))))
                 (void)
                 max-lookbehind_0))
              (args (raise-binding-result-arity-error 3 args))))))))))
(define merge-depends-sizes
  (lambda (ht1_0 ht2_0)
    (if (zero? (hash-count ht1_0))
      ht2_0
      (if (let ((app_0 (hash-count ht2_0))) (< app_0 (hash-count ht1_0)))
        (merge-depends-sizes ht2_0 ht1_0)
        (begin
          (letrec*
           ((for-loop_0
             (|#%name|
              for-loop
              (lambda (ht2_1 i_0)
                (begin
                  (if i_0
                    (let ((k_0 (hash-iterate-key ht1_0 i_0)))
                      (let ((ht2_2
                             (let ((ht2_2 (hash-set ht2_1 k_0 #t)))
                               (values ht2_2))))
                        (for-loop_0 ht2_2 (hash-iterate-next ht1_0 i_0))))
                    ht2_1))))))
           (for-loop_0 ht2_0 (hash-iterate-first ht1_0))))))))
(define convert
  (lambda (rx_0)
    (if (eq? rx_0 'any)
      (rx:unicode-categories12.1 null #f)
      (if (exact-integer? rx_0)
        (if (< rx_0 128)
          rx_0
          (string->bytes/utf-8 (string (integer->char rx_0))))
        (if (rx:range? rx_0)
          (let ((range_0 (rx:range-range rx_0)))
            (if (range-within? range_0 0 127) rx_0 (range->alts range_0)))
          (if (bytes? rx_0)
            (convert (bytes->string/latin-1 rx_0))
            (if (string? rx_0)
              (string->bytes/utf-8 rx_0)
              (if (rx:alts? rx_0)
                (let ((app_0 (convert (rx:alts-rx_2265 rx_0))))
                  (rx-alts app_0 (convert (rx:alts-rx_1912 rx_0)) 255))
                (if (rx:sequence? rx_0)
                  (let ((new-rxs_0
                         (reverse$1
                          (let ((lst_0 (rx:sequence-rxs rx_0)))
                            (begin
                              (letrec*
                               ((for-loop_0
                                 (|#%name|
                                  for-loop
                                  (lambda (fold-var_0 lst_1)
                                    (begin
                                      (if (pair? lst_1)
                                        (let ((rx_1 (unsafe-car lst_1)))
                                          (let ((rest_0 (unsafe-cdr lst_1)))
                                            (let ((fold-var_1
                                                   (let ((fold-var_1
                                                          (cons
                                                           (convert rx_1)
                                                           fold-var_0)))
                                                     (values fold-var_1))))
                                              (for-loop_0 fold-var_1 rest_0))))
                                        fold-var_0))))))
                               (for-loop_0 null lst_0)))))))
                    (if (rx:sequence? rx_0)
                      (let ((needs-backtrack?2_0
                             (ormap_2765 needs-backtrack? new-rxs_0)))
                        (rx:sequence2.1 new-rxs_0 needs-backtrack?2_0))
                      (raise-argument-error 'struct-copy "rx:sequence?" rx_0)))
                  (if (rx:group? rx_0)
                    (if (rx:group? rx_0)
                      (let ((rx3_0 (convert (rx:group-rx rx_0))))
                        (rx:group3.1 rx3_0 (rx:group-number rx_0)))
                      (raise-argument-error 'struct-copy "rx:group?" rx_0))
                    (if (rx:repeat? rx_0)
                      (if (rx:repeat? rx_0)
                        (let ((rx4_0 (convert (rx:repeat-rx rx_0))))
                          (let ((app_0 (rx:repeat-min rx_0)))
                            (let ((app_1 (rx:repeat-max rx_0)))
                              (rx:repeat4.1
                               rx4_0
                               app_0
                               app_1
                               (rx:repeat-non-greedy? rx_0)))))
                        (raise-argument-error 'struct-copy "rx:repeat?" rx_0))
                      (if (rx:maybe? rx_0)
                        (if (rx:maybe? rx_0)
                          (let ((rx5_0 (convert (rx:maybe-rx rx_0))))
                            (rx:maybe5.1 rx5_0 (rx:maybe-non-greedy? rx_0)))
                          (raise-argument-error 'struct-copy "rx:maybe?" rx_0))
                        (if (rx:conditional? rx_0)
                          (let ((new-rx1_0
                                 (convert (rx:conditional-rx_2162 rx_0))))
                            (let ((new-rx2_0
                                   (convert (rx:conditional-rx_2328 rx_0))))
                              (if (rx:conditional? rx_0)
                                (let ((tst6_0
                                       (convert (rx:conditional-tst rx_0))))
                                  (let ((needs-backtrack?9_0
                                         (let ((or-part_0
                                                (needs-backtrack? new-rx1_0)))
                                           (if or-part_0
                                             or-part_0
                                             (needs-backtrack? new-rx2_0)))))
                                    (let ((tst6_1 tst6_0))
                                      (let ((app_0
                                             (rx:conditional-n-start rx_0)))
                                        (rx:conditional6.1
                                         tst6_1
                                         new-rx1_0
                                         new-rx2_0
                                         app_0
                                         (rx:conditional-num-n rx_0)
                                         needs-backtrack?9_0)))))
                                (raise-argument-error
                                 'struct-copy
                                 "rx:conditional?"
                                 rx_0))))
                          (if (rx:lookahead? rx_0)
                            (if (rx:lookahead? rx_0)
                              (let ((rx10_0 (convert (rx:lookahead-rx rx_0))))
                                (let ((app_0 (rx:lookahead-match? rx_0)))
                                  (let ((app_1 (rx:lookahead-n-start rx_0)))
                                    (rx:lookahead7.1
                                     rx10_0
                                     app_0
                                     app_1
                                     (rx:lookahead-num-n rx_0)))))
                              (raise-argument-error
                               'struct-copy
                               "rx:lookahead?"
                               rx_0))
                            (if (rx:lookbehind? rx_0)
                              (if (rx:lookbehind? rx_0)
                                (let ((rx11_0
                                       (convert (rx:lookbehind-rx rx_0))))
                                  (let ((app_0 (rx:lookbehind-match? rx_0)))
                                    (let ((app_1 (rx:lookbehind-lb-min rx_0)))
                                      (let ((app_2
                                             (rx:lookbehind-lb-max rx_0)))
                                        (let ((app_3
                                               (rx:lookbehind-n-start rx_0)))
                                          (rx:lookbehind8.1
                                           rx11_0
                                           app_0
                                           app_1
                                           app_2
                                           app_3
                                           (rx:lookbehind-num-n rx_0)))))))
                                (raise-argument-error
                                 'struct-copy
                                 "rx:lookbehind?"
                                 rx_0))
                              (if (rx:cut? rx_0)
                                (let ((new-rx_0 (convert (rx:cut-rx rx_0))))
                                  (if (rx:cut? rx_0)
                                    (let ((needs-backtrack?13_0
                                           (needs-backtrack? rx_0)))
                                      (let ((app_0 (rx:cut-n-start rx_0)))
                                        (rx:cut9.1
                                         new-rx_0
                                         app_0
                                         (rx:cut-num-n rx_0)
                                         needs-backtrack?13_0)))
                                    (raise-argument-error
                                     'struct-copy
                                     "rx:cut?"
                                     rx_0)))
                                rx_0))))))))))))))))
(define range->alts
  (lambda (args_0)
    (let ((l_0 (begin-unsafe args_0)))
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (l_1)
            (begin
              (if (null? l_1)
                'never
                (let ((start_0 (caar l_1)))
                  (let ((end_0 (cdar l_1)))
                    (let ((start_1 start_0))
                      (let ((seg-end_0
                             (if (<= start_1 127)
                               127
                               (if (<= start_1 2047)
                                 2047
                                 (if (<= start_1 65535)
                                   65535
                                   (if (<= start_1 2097151)
                                     2097151
                                     (void)))))))
                        (if (> end_0 seg-end_0)
                          (loop_0
                           (let ((app_0 (cons start_1 seg-end_0)))
                             (cons
                              app_0
                              (let ((app_1 (cons (add1 seg-end_0) end_0)))
                                (cons app_1 (cdr l_1))))))
                          (if (<= end_0 127)
                            (let ((app_0
                                   (rx-range
                                    (begin-unsafe
                                     (range-union
                                      null
                                      (list (cons start_1 end_0))))
                                    255)))
                              (rx-alts app_0 (loop_0 (cdr l_1)) 255))
                            (let ((app_0
                                   (let ((app_0
                                          (string->bytes/utf-8
                                           (string (integer->char start_1)))))
                                     (bytes-range
                                      app_0
                                      (string->bytes/utf-8
                                       (string (integer->char end_0)))))))
                              (rx-alts
                               app_0
                               (loop_0 (cdr l_1))
                               255))))))))))))))
       (loop_0 l_0)))))
(define bytes-range
  (lambda (start-str_0 end-str_0)
    (if (equal? start-str_0 end-str_0)
      start-str_0
      (if (= 1 (unsafe-bytes-length start-str_0))
        (rx-range
         (let ((from-c_0 (unsafe-bytes-ref start-str_0 0)))
           (let ((to-c_0 (unsafe-bytes-ref end-str_0 0)))
             (let ((from-c_1 from-c_0))
               (begin-unsafe
                (range-union null (list (cons from-c_1 to-c_0)))))))
         255)
        (let ((common_0
               (letrec*
                ((loop_0
                  (|#%name|
                   loop
                   (lambda (i_0)
                     (begin
                       (if (let ((app_0 (unsafe-bytes-ref start-str_0 i_0)))
                             (= app_0 (unsafe-bytes-ref end-str_0 i_0)))
                         (loop_0 (add1 i_0))
                         i_0))))))
                (loop_0 0))))
          (let ((common-str_0
                 (if (zero? common_0) #vu8() (subbytes start-str_0 0 common_0))))
            (let ((n_0 (unsafe-bytes-ref start-str_0 common_0)))
              (let ((m_0 (unsafe-bytes-ref end-str_0 common_0)))
                (let ((p_0
                       (if (zero-tail? start-str_0 (add1 common_0))
                         n_0
                         (add1 n_0))))
                  (let ((q_0
                         (if (zero-tail? end-str_0 (add1 common_0))
                           m_0
                           (sub1 m_0))))
                    (let ((tail-len_0
                           (sub1
                            (- (unsafe-bytes-length start-str_0) common_0))))
                      (let ((n-to-p_0
                             (rx-sequence
                              (list
                               n_0
                               (let ((app_0
                                      (subbytes start-str_0 (add1 common_0))))
                                 (bytes-range
                                  app_0
                                  (vector-ref FFFF-tails tail-len_0)))))))
                        (let ((m-and-up_0
                               (rx-sequence
                                (list
                                 m_0
                                 (let ((app_0
                                        (vector-ref 0000-tails tail-len_0)))
                                   (bytes-range
                                    app_0
                                    (subbytes end-str_0 (add1 common_0))))))))
                          (let ((p-through-q_0
                                 (if (= (add1 p_0) q_0)
                                   'never
                                   (rx-sequence
                                    (let ((app_0
                                           (rx-range
                                            (begin-unsafe
                                             (range-union
                                              null
                                              (list (cons p_0 q_0))))
                                            255)))
                                      (cons
                                       app_0
                                       (reverse$1
                                        (begin
                                          (letrec*
                                           ((for-loop_0
                                             (|#%name|
                                              for-loop
                                              (lambda (fold-var_0 pos_0)
                                                (begin
                                                  (if (< pos_0 tail-len_0)
                                                    (let ((fold-var_1
                                                           (cons
                                                            'any
                                                            fold-var_0)))
                                                      (let ((fold-var_2
                                                             (values
                                                              fold-var_1)))
                                                        (for-loop_0
                                                         fold-var_2
                                                         (+ pos_0 1))))
                                                    fold-var_0))))))
                                           (for-loop_0 null 0))))))))))
                            (rx-sequence
                             (let ((app_0
                                    (if (=
                                         1
                                         (unsafe-bytes-length common-str_0))
                                      (unsafe-bytes-ref common-str_0 0)
                                      common-str_0)))
                               (list
                                app_0
                                (rx-alts
                                 n-to-p_0
                                 (rx-alts p-through-q_0 m-and-up_0 255)
                                 255))))))))))))))))))
(define FFFF-tails '#(#vu8() #vu8(255) #vu8(255 255) #vu8(255 255 255) #vu8(255 255 255 255)))
(define 0000-tails '#(#vu8() #vu8(0) #vu8(0 0) #vu8(0 0 0) #vu8(0 0 0 0)))
(define zero-tail?
  (lambda (bstr_0 i_0)
    (call-with-values
     (lambda () (unsafe-normalise-inputs unsafe-bytes-length bstr_0 i_0 #f 1))
     (case-lambda
      ((v*_0 start*_0 stop*_0 step*_0)
       (begin
         #t
         (letrec*
          ((for-loop_0
            (|#%name|
             for-loop
             (lambda (result_0 idx_0)
               (begin
                 (if (unsafe-fx< idx_0 stop*_0)
                   (let ((c_0 (unsafe-bytes-ref v*_0 idx_0)))
                     (let ((result_1
                            (let ((result_1 (= c_0 0))) (values result_1))))
                       (if (if (not (let ((x_0 (list c_0))) (not result_1)))
                             #t
                             #f)
                         (for-loop_0 result_1 (unsafe-fx+ idx_0 1))
                         result_1)))
                   result_0))))))
          (for-loop_0 #t start*_0))))
      (args (raise-binding-result-arity-error 4 args))))))
(define anchored?
  (lambda (rx_0)
    (if (eq? rx_0 'start)
      #t
      (if (rx:sequence? rx_0)
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (rxs_0)
              (begin
                (if (null? rxs_0)
                  #f
                  (if (rx:lookahead? (car rxs_0))
                    (loop_0 (cdr rxs_0))
                    (if (rx:lookbehind? (car rxs_0))
                      (loop_0 (cdr rxs_0))
                      (anchored? (car rxs_0))))))))))
         (loop_0 (rx:sequence-rxs rx_0)))
        (if (rx:alts? rx_0)
          (if (anchored? (rx:alts-rx_2265 rx_0))
            (anchored? (rx:alts-rx_1912 rx_0))
            #f)
          (if (rx:conditional? rx_0)
            (if (anchored? (rx:conditional-rx_2162 rx_0))
              (anchored? (rx:conditional-rx_2328 rx_0))
              #f)
            (if (rx:group? rx_0)
              (anchored? (rx:group-rx rx_0))
              (if (rx:cut? rx_0) (anchored? (rx:cut-rx rx_0)) #f))))))))
(define get-must-string
  (lambda (rx_0)
    (if (something-expensive? rx_0)
      (let ((app_0 (must-string rx_0))) (choose app_0 (must-range rx_0)))
      #f)))
(define choose
  (lambda (bstr_0 seq_0)
    (if (not seq_0)
      bstr_0
      (if (not bstr_0)
        (compile-range-sequence seq_0)
        (if (>= (unsafe-bytes-length bstr_0) (quotient (length seq_0) 2))
          bstr_0
          (compile-range-sequence seq_0))))))
(define something-expensive?
  (lambda (rx_0)
    (if (let ((or-part_0 (rx:alts? rx_0)))
          (if or-part_0 or-part_0 (rx:repeat? rx_0)))
      #t
      (if (rx:maybe? rx_0)
        (something-expensive? (rx:maybe-rx rx_0))
        (if (rx:sequence? rx_0)
          (let ((lst_0 (rx:sequence-rxs rx_0)))
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (result_0 lst_1)
                    (begin
                      (if (pair? lst_1)
                        (let ((rx_1 (unsafe-car lst_1)))
                          (let ((rest_0 (unsafe-cdr lst_1)))
                            (let ((result_1
                                   (let ((result_1
                                          (something-expensive? rx_1)))
                                     (values result_1))))
                              (if (if (not (let ((x_0 (list rx_1))) result_1))
                                    #t
                                    #f)
                                (for-loop_0 result_1 rest_0)
                                result_1))))
                        result_0))))))
               (for-loop_0 #f lst_0))))
          (if (rx:conditional? rx_0)
            (let ((or-part_0
                   (something-expensive? (rx:conditional-rx_2162 rx_0))))
              (if or-part_0
                or-part_0
                (something-expensive? (rx:conditional-rx_2328 rx_0))))
            (if (rx:group? rx_0)
              (something-expensive? (rx:group-rx rx_0))
              (if (rx:cut? rx_0)
                (something-expensive? (rx:cut-rx rx_0))
                (if (rx:lookahead? rx_0)
                  (something-expensive? (rx:lookahead-rx rx_0))
                  (if (rx:lookbehind? rx_0)
                    (something-expensive? (rx:lookbehind-rx rx_0))
                    #f))))))))))
(define must-string
  (lambda (rx_0)
    (if (bytes? rx_0)
      rx_0
      (if (integer? rx_0)
        (bytes rx_0)
        (if (rx:sequence? rx_0)
          (let ((lst_0 (rx:sequence-rxs rx_0)))
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (bstr_0 lst_1)
                    (begin
                      (if (pair? lst_1)
                        (let ((rx_1 (unsafe-car lst_1)))
                          (let ((rest_0 (unsafe-cdr lst_1)))
                            (let ((bstr_1
                                   (let ((bstr_1
                                          (let ((bstr1_0 (must-string rx_1)))
                                            (if (not bstr_0)
                                              bstr1_0
                                              (if (not bstr1_0)
                                                bstr_0
                                                (if (>
                                                     (unsafe-bytes-length
                                                      bstr_0)
                                                     (unsafe-bytes-length
                                                      bstr1_0))
                                                  bstr_0
                                                  bstr1_0))))))
                                     (values bstr_1))))
                              (for-loop_0 bstr_1 rest_0))))
                        bstr_0))))))
               (for-loop_0 #f lst_0))))
          (if (rx:repeat? rx_0)
            (if (positive? (rx:repeat-min rx_0))
              (must-string (rx:repeat-rx rx_0))
              #f)
            (if (rx:group? rx_0)
              (must-string (rx:group-rx rx_0))
              (if (rx:cut? rx_0)
                (must-string (rx:cut-rx rx_0))
                (if (rx:lookahead? rx_0)
                  (if (rx:lookahead-match? rx_0)
                    (must-string (rx:lookahead-rx rx_0))
                    #f)
                  (if (rx:lookbehind? rx_0)
                    (if (rx:lookbehind-match? rx_0)
                      (must-string (rx:lookbehind-rx rx_0))
                      #f)
                    #f))))))))))
(define must-range
  (lambda (rx_0)
    (if (bytes? rx_0)
      (bytes->list rx_0)
      (if (integer? rx_0)
        (list rx_0)
        (if (rx:range? rx_0)
          (list (rx:range-range rx_0))
          (if (rx:sequence? rx_0)
            (letrec*
             ((loop_0
               (|#%name|
                loop
                (lambda (seq_0 l_0)
                  (begin
                    (if (null? l_0)
                      (if (pair? seq_0) (reverse$1 seq_0) #f)
                      (if (bytes? (car l_0))
                        (let ((app_0
                               (append
                                (reverse$1 (bytes->list (car l_0)))
                                seq_0)))
                          (loop_0 app_0 (cdr l_0)))
                        (if (rx:range? (car l_0))
                          (let ((app_0
                                 (cons (rx:range-range (car l_0)) seq_0)))
                            (loop_0 app_0 (cdr l_0)))
                          (if (null? seq_0)
                            (loop_0 null (cdr l_0))
                            (let ((rest-seq_0 (loop_0 null (cdr l_0))))
                              (if (if rest-seq_0
                                    (let ((app_0 (length rest-seq_0)))
                                      (> app_0 (length seq_0)))
                                    #f)
                                rest-seq_0
                                (reverse$1 seq_0))))))))))))
             (loop_0 null (rx:sequence-rxs rx_0)))
            (if (rx:repeat? rx_0)
              (if (positive? (rx:repeat-min rx_0))
                (must-range (rx:repeat-rx rx_0))
                #f)
              (if (rx:group? rx_0)
                (must-range (rx:group-rx rx_0))
                (if (rx:cut? rx_0)
                  (must-range (rx:cut-rx rx_0))
                  (if (rx:lookahead? rx_0)
                    (if (rx:lookahead-match? rx_0)
                      (must-range (rx:lookahead-rx rx_0))
                      #f)
                    (if (rx:lookbehind? rx_0)
                      (if (rx:lookbehind-match? rx_0)
                        (must-range (rx:lookbehind-rx rx_0))
                        #f)
                      #f)))))))))))
(define compile-range-sequence
  (lambda (seq_0)
    (reverse$1
     (begin
       (letrec*
        ((for-loop_0
          (|#%name|
           for-loop
           (lambda (fold-var_0 lst_0)
             (begin
               (if (pair? lst_0)
                 (let ((r_0 (unsafe-car lst_0)))
                   (let ((rest_0 (unsafe-cdr lst_0)))
                     (let ((fold-var_1
                            (let ((fold-var_1
                                   (cons
                                    (if (exact-integer? r_0)
                                      (compile-range (range-add null r_0))
                                      (compile-range r_0))
                                    fold-var_0)))
                              (values fold-var_1))))
                       (for-loop_0 fold-var_1 rest_0))))
                 fold-var_0))))))
        (for-loop_0 null seq_0))))))
(define get-start-range
  (lambda (rx_0)
    (let ((r_0 (start-range rx_0))) (if r_0 (compile-range r_0) #f))))
(define start-range
  (lambda (rx_0)
    (if (integer? rx_0)
      (range-add null rx_0)
      (if (bytes? rx_0)
        (range-add null (unsafe-bytes-ref rx_0 0))
        (if (rx:sequence? rx_0)
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (l_0)
                (begin
                  (if (null? l_0)
                    #f
                    (let ((rx_1 (car l_0)))
                      (if (zero-sized? rx_1)
                        (loop_0 (cdr l_0))
                        (start-range rx_1)))))))))
           (loop_0 (rx:sequence-rxs rx_0)))
          (if (rx:alts? rx_0)
            (let ((app_0 (start-range (rx:alts-rx_2265 rx_0))))
              (union app_0 (start-range (rx:alts-rx_1912 rx_0))))
            (if (rx:conditional? rx_0)
              (let ((app_0 (start-range (rx:conditional-rx_2162 rx_0))))
                (union app_0 (start-range (rx:conditional-rx_2328 rx_0))))
              (if (rx:group? rx_0)
                (start-range (rx:group-rx rx_0))
                (if (rx:cut? rx_0)
                  (start-range (rx:cut-rx rx_0))
                  (if (rx:repeat? rx_0)
                    (if (positive? (rx:repeat-min rx_0))
                      (start-range (rx:repeat-rx rx_0))
                      #f)
                    (if (rx:range? rx_0) (rx:range-range rx_0) #f)))))))))))
(define zero-sized?
  (lambda (rx_0)
    (let ((or-part_0 (eq? rx_0 'empty)))
      (if or-part_0
        or-part_0
        (let ((or-part_1 (eq? rx_0 'start)))
          (if or-part_1
            or-part_1
            (let ((or-part_2 (eq? rx_0 'line-start)))
              (if or-part_2
                or-part_2
                (let ((or-part_3 (eq? rx_0 'word-boundary)))
                  (if or-part_3
                    or-part_3
                    (let ((or-part_4 (eq? rx_0 'not-word-boundary)))
                      (if or-part_4
                        or-part_4
                        (let ((or-part_5 (rx:lookahead? rx_0)))
                          (if or-part_5
                            or-part_5
                            (let ((or-part_6 (rx:lookbehind? rx_0)))
                              (if or-part_6
                                or-part_6
                                (let ((or-part_7
                                       (if (rx:group? rx_0)
                                         (zero-sized? (rx:group-rx rx_0))
                                         #f)))
                                  (if or-part_7
                                    or-part_7
                                    (if (rx:cut? rx_0)
                                      (zero-sized? (rx:cut-rx rx_0))
                                      #f)))))))))))))))))))
(define union (lambda (a_0 b_0) (if a_0 (if b_0 (range-union a_0 b_0) #f) #f)))
(define finish_2590
  (make-struct-type-install-properties
   '(lazy-bytes)
   13
   0
   #f
   null
   (current-inspector)
   #f
   '(2 3 4 5 6 7 8 9 12)
   #f
   'lazy-bytes))
(define struct:lazy-bytes
  (make-record-type-descriptor
   'lazy-bytes
   #f
   (|#%nongenerative-uid| lazy-bytes)
   #f
   #f
   '(13 . 3075)))
(define effect_2741 (finish_2590 struct:lazy-bytes))
(define lazy-bytes1.1
  (|#%name|
   lazy-bytes
   (record-constructor
    (make-record-constructor-descriptor struct:lazy-bytes #f #f))))
(define lazy-bytes?_2654
  (|#%name| lazy-bytes? (record-predicate struct:lazy-bytes)))
(define lazy-bytes?
  (|#%name|
   lazy-bytes?
   (lambda (v)
     (if (lazy-bytes?_2654 v)
       #t
       ($value
        (if (impersonator? v) (lazy-bytes?_2654 (impersonator-val v)) #f))))))
(define lazy-bytes-bstr_2345
  (|#%name| lazy-bytes-bstr (record-accessor struct:lazy-bytes 0)))
(define lazy-bytes-bstr
  (|#%name|
   lazy-bytes-bstr
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-bstr_2345 s)
       ($value
        (impersonate-ref lazy-bytes-bstr_2345 struct:lazy-bytes 0 s 'bstr))))))
(define lazy-bytes-end_2914
  (|#%name| lazy-bytes-end (record-accessor struct:lazy-bytes 1)))
(define lazy-bytes-end
  (|#%name|
   lazy-bytes-end
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-end_2914 s)
       ($value
        (impersonate-ref lazy-bytes-end_2914 struct:lazy-bytes 1 s 'end))))))
(define lazy-bytes-in_2568
  (|#%name| lazy-bytes-in (record-accessor struct:lazy-bytes 2)))
(define lazy-bytes-in
  (|#%name|
   lazy-bytes-in
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-in_2568 s)
       ($value
        (impersonate-ref lazy-bytes-in_2568 struct:lazy-bytes 2 s 'in))))))
(define lazy-bytes-skip-amt_2649
  (|#%name| lazy-bytes-skip-amt (record-accessor struct:lazy-bytes 3)))
(define lazy-bytes-skip-amt
  (|#%name|
   lazy-bytes-skip-amt
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-skip-amt_2649 s)
       ($value
        (impersonate-ref
         lazy-bytes-skip-amt_2649
         struct:lazy-bytes
         3
         s
         'skip-amt))))))
(define lazy-bytes-prefix-len_2565
  (|#%name| lazy-bytes-prefix-len (record-accessor struct:lazy-bytes 4)))
(define lazy-bytes-prefix-len
  (|#%name|
   lazy-bytes-prefix-len
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-prefix-len_2565 s)
       ($value
        (impersonate-ref
         lazy-bytes-prefix-len_2565
         struct:lazy-bytes
         4
         s
         'prefix-len))))))
(define lazy-bytes-peek?_2174
  (|#%name| lazy-bytes-peek? (record-accessor struct:lazy-bytes 5)))
(define lazy-bytes-peek?
  (|#%name|
   lazy-bytes-peek?
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-peek?_2174 s)
       ($value
        (impersonate-ref
         lazy-bytes-peek?_2174
         struct:lazy-bytes
         5
         s
         'peek?))))))
(define lazy-bytes-immediate-only?_2496
  (|#%name| lazy-bytes-immediate-only? (record-accessor struct:lazy-bytes 6)))
(define lazy-bytes-immediate-only?
  (|#%name|
   lazy-bytes-immediate-only?
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-immediate-only?_2496 s)
       ($value
        (impersonate-ref
         lazy-bytes-immediate-only?_2496
         struct:lazy-bytes
         6
         s
         'immediate-only?))))))
(define lazy-bytes-progress-evt_2923
  (|#%name| lazy-bytes-progress-evt (record-accessor struct:lazy-bytes 7)))
(define lazy-bytes-progress-evt
  (|#%name|
   lazy-bytes-progress-evt
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-progress-evt_2923 s)
       ($value
        (impersonate-ref
         lazy-bytes-progress-evt_2923
         struct:lazy-bytes
         7
         s
         'progress-evt))))))
(define lazy-bytes-out_2340
  (|#%name| lazy-bytes-out (record-accessor struct:lazy-bytes 8)))
(define lazy-bytes-out
  (|#%name|
   lazy-bytes-out
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-out_2340 s)
       ($value
        (impersonate-ref lazy-bytes-out_2340 struct:lazy-bytes 8 s 'out))))))
(define lazy-bytes-max-lookbehind_2217
  (|#%name| lazy-bytes-max-lookbehind (record-accessor struct:lazy-bytes 9)))
(define lazy-bytes-max-lookbehind
  (|#%name|
   lazy-bytes-max-lookbehind
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-max-lookbehind_2217 s)
       ($value
        (impersonate-ref
         lazy-bytes-max-lookbehind_2217
         struct:lazy-bytes
         9
         s
         'max-lookbehind))))))
(define lazy-bytes-failed?_2073
  (|#%name| lazy-bytes-failed? (record-accessor struct:lazy-bytes 10)))
(define lazy-bytes-failed?
  (|#%name|
   lazy-bytes-failed?
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-failed?_2073 s)
       ($value
        (impersonate-ref
         lazy-bytes-failed?_2073
         struct:lazy-bytes
         10
         s
         'failed?))))))
(define lazy-bytes-discarded-count_3290
  (|#%name| lazy-bytes-discarded-count (record-accessor struct:lazy-bytes 11)))
(define lazy-bytes-discarded-count
  (|#%name|
   lazy-bytes-discarded-count
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-discarded-count_3290 s)
       ($value
        (impersonate-ref
         lazy-bytes-discarded-count_3290
         struct:lazy-bytes
         11
         s
         'discarded-count))))))
(define lazy-bytes-max-peek_2492
  (|#%name| lazy-bytes-max-peek (record-accessor struct:lazy-bytes 12)))
(define lazy-bytes-max-peek
  (|#%name|
   lazy-bytes-max-peek
   (lambda (s)
     (if (lazy-bytes?_2654 s)
       (lazy-bytes-max-peek_2492 s)
       ($value
        (impersonate-ref
         lazy-bytes-max-peek_2492
         struct:lazy-bytes
         12
         s
         'max-peek))))))
(define set-lazy-bytes-bstr!_2695
  (|#%name| set-lazy-bytes-bstr! (record-mutator struct:lazy-bytes 0)))
(define set-lazy-bytes-bstr!
  (|#%name|
   set-lazy-bytes-bstr!
   (lambda (s v)
     (if (lazy-bytes?_2654 s)
       (set-lazy-bytes-bstr!_2695 s v)
       ($value
        (impersonate-set!
         set-lazy-bytes-bstr!_2695
         struct:lazy-bytes
         0
         0
         s
         v
         'bstr))))))
(define set-lazy-bytes-end!_2897
  (|#%name| set-lazy-bytes-end! (record-mutator struct:lazy-bytes 1)))
(define set-lazy-bytes-end!
  (|#%name|
   set-lazy-bytes-end!
   (lambda (s v)
     (if (lazy-bytes?_2654 s)
       (set-lazy-bytes-end!_2897 s v)
       ($value
        (impersonate-set!
         set-lazy-bytes-end!_2897
         struct:lazy-bytes
         1
         1
         s
         v
         'end))))))
(define set-lazy-bytes-failed?!_2421
  (|#%name| set-lazy-bytes-failed?! (record-mutator struct:lazy-bytes 10)))
(define set-lazy-bytes-failed?!
  (|#%name|
   set-lazy-bytes-failed?!
   (lambda (s v)
     (if (lazy-bytes?_2654 s)
       (set-lazy-bytes-failed?!_2421 s v)
       ($value
        (impersonate-set!
         set-lazy-bytes-failed?!_2421
         struct:lazy-bytes
         10
         10
         s
         v
         'failed?))))))
(define set-lazy-bytes-discarded-count!_2738
  (|#%name|
   set-lazy-bytes-discarded-count!
   (record-mutator struct:lazy-bytes 11)))
(define set-lazy-bytes-discarded-count!
  (|#%name|
   set-lazy-bytes-discarded-count!
   (lambda (s v)
     (if (lazy-bytes?_2654 s)
       (set-lazy-bytes-discarded-count!_2738 s v)
       ($value
        (impersonate-set!
         set-lazy-bytes-discarded-count!_2738
         struct:lazy-bytes
         11
         11
         s
         v
         'discarded-count))))))
(define make-lazy-bytes
  (lambda (in_0
           skip-amt_0
           prefix_0
           peek?_0
           immediate-only?_0
           progress-evt_0
           out_0
           max-lookbehind_0
           max-peek_0)
    (let ((len_0 (unsafe-bytes-length prefix_0)))
      (lazy-bytes1.1
       prefix_0
       len_0
       in_0
       skip-amt_0
       len_0
       peek?_0
       immediate-only?_0
       progress-evt_0
       out_0
       max-lookbehind_0
       #f
       0
       max-peek_0))))
(define lazy-bytes-before-end?
  (lambda (s_0 pos_0 end_0)
    (if (let ((or-part_0 (not (exact-integer? end_0))))
          (if or-part_0 or-part_0 (< pos_0 end_0)))
      (if (< pos_0 (lazy-bytes-end s_0))
        #t
        (if (get-more-bytes! s_0) (lazy-bytes-before-end? s_0 pos_0 end_0) #f))
      #f)))
(define lazy-bytes-ref
  (lambda (s_0 pos_0)
    (let ((app_0 (lazy-bytes-bstr s_0)))
      (unsafe-bytes-ref app_0 (- pos_0 (lazy-bytes-discarded-count s_0))))))
(define lazy-bytes-advance!
  (lambda (s_0 given-pos_0 force?_0)
    (begin
      (if force?_0 (lazy-bytes-before-end? s_0 given-pos_0 'eof) (void))
      (let ((pos_0 (min given-pos_0 (lazy-bytes-end s_0))))
        (if (if (lazy-bytes? s_0) (not (lazy-bytes-peek? s_0)) #f)
          (let ((discarded-count_0 (lazy-bytes-discarded-count s_0)))
            (let ((unneeded_0
                   (let ((app_0 pos_0))
                     (-
                      app_0
                      discarded-count_0
                      (lazy-bytes-max-lookbehind s_0)))))
              (if (if force?_0 force?_0 (> unneeded_0 4096))
                (let ((amt_0
                       (if force?_0
                         (let ((app_0 pos_0))
                           (- app_0 (lazy-bytes-discarded-count s_0)))
                         4096)))
                  (let ((bstr_0 (lazy-bytes-bstr s_0)))
                    (let ((out_0 (lazy-bytes-out s_0)))
                      (begin
                        (if out_0
                          (let ((prefix-len_0 (lazy-bytes-prefix-len s_0)))
                            (write-bytes
                             bstr_0
                             out_0
                             (if (> discarded-count_0 prefix-len_0)
                               0
                               (min amt_0 (- prefix-len_0 discarded-count_0)))
                             amt_0))
                          (void))
                        (let ((copy-end_0
                               (- (lazy-bytes-end s_0) discarded-count_0)))
                          (begin
                            (if (= amt_0 copy-end_0)
                              (void)
                              (unsafe-bytes-copy!
                               bstr_0
                               0
                               bstr_0
                               amt_0
                               copy-end_0))
                            (set-lazy-bytes-discarded-count!
                             s_0
                             (+ amt_0 discarded-count_0))))))))
                (void))))
          (void))))))
(define get-more-bytes!
  (lambda (s_0)
    (if (lazy-bytes? s_0)
      (let ((discarded-count_0 (lazy-bytes-discarded-count s_0)))
        (let ((len_0 (- (lazy-bytes-end s_0) discarded-count_0)))
          (let ((bstr_0 (lazy-bytes-bstr s_0)))
            (if (lazy-bytes-failed? s_0)
              #f
              (if (< len_0 (unsafe-bytes-length bstr_0))
                (let ((n_0
                       (let ((app_0
                              (if (lazy-bytes-immediate-only? s_0)
                                peek-bytes-avail!*
                                peek-bytes-avail!)))
                         (let ((app_1
                                (let ((app_1
                                       (- len_0 (lazy-bytes-prefix-len s_0))))
                                  (+
                                   app_1
                                   (lazy-bytes-skip-amt s_0)
                                   discarded-count_0))))
                           (let ((app_2 (lazy-bytes-progress-evt s_0)))
                             (|#%app|
                              app_0
                              bstr_0
                              app_1
                              app_2
                              (lazy-bytes-in s_0)
                              len_0))))))
                  (if (eof-object? n_0)
                    #f
                    (if (not (fixnum? n_0))
                      (raise-arguments-error
                       'regexp-match
                       "non-character in an unsupported context"
                       "port"
                       (lazy-bytes-in s_0))
                      (if (zero? n_0)
                        (begin (set-lazy-bytes-failed?! s_0 #t) #f)
                        (begin
                          (set-lazy-bytes-end!
                           s_0
                           (+ n_0 len_0 discarded-count_0))
                          #t)))))
                (let ((max-peek_0 (lazy-bytes-max-peek s_0)))
                  (let ((prefix-len_0
                         (if max-peek_0 (lazy-bytes-prefix-len s_0) #f)))
                    (if (if max-peek_0
                          (>=
                           len_0
                           (- (+ max-peek_0 prefix-len_0) discarded-count_0))
                          #f)
                      #f
                      (let ((bstr2_0
                             (make-bytes
                              (let ((sz_0
                                     (max
                                      32
                                      (* 2 (unsafe-bytes-length bstr_0)))))
                                (if max-peek_0
                                  (min
                                   sz_0
                                   (-
                                    (+ prefix-len_0 max-peek_0)
                                    discarded-count_0))
                                  sz_0)))))
                        (begin
                          (unsafe-bytes-copy! bstr2_0 0 bstr_0 0 len_0)
                          (set-lazy-bytes-bstr! s_0 bstr2_0)
                          (get-more-bytes! s_0)))))))))))
      #f)))
(define bytes->char/utf-8
  (lambda (last-b_0 accum_0)
    (if (< last-b_0 128)
      (if (null? accum_0) (integer->char last-b_0) 'fail)
      (if (continue-byte? last-b_0)
        (if (null? accum_0)
          'fail
          (if (two-byte-prefix? (car accum_0))
            (integer->char*
             128
             (let ((app_0 (arithmetic-shift (bitwise-and 31 (car accum_0)) 6)))
               (+ app_0 (begin-unsafe (bitwise-and last-b_0 63)))))
            (if (three-byte-prefix? (car accum_0))
              'continue
              (if (four-byte-prefix? (car accum_0))
                'continue
                (if (if (pair? (cdr accum_0))
                      (three-byte-prefix? (cadr accum_0))
                      #f)
                  (integer->char*
                   2048
                   (let ((app_0
                          (arithmetic-shift
                           (bitwise-and 15 (cadr accum_0))
                           12)))
                     (let ((app_1
                            (arithmetic-shift
                             (let ((b_0 (car accum_0)))
                               (begin-unsafe (bitwise-and b_0 63)))
                             6)))
                       (+
                        app_0
                        app_1
                        (begin-unsafe (bitwise-and last-b_0 63))))))
                  (if (if (pair? (cdr accum_0))
                        (four-byte-prefix? (cadr accum_0))
                        #f)
                    'continue
                    (if (if (pair? (cdr accum_0))
                          (if (pair? (cddr accum_0))
                            (four-byte-prefix? (caddr accum_0))
                            #f)
                          #f)
                      (integer->char*
                       65536
                       (let ((app_0
                              (arithmetic-shift
                               (bitwise-and 15 (caddr accum_0))
                               18)))
                         (let ((app_1
                                (arithmetic-shift
                                 (let ((b_0 (cadr accum_0)))
                                   (begin-unsafe (bitwise-and b_0 63)))
                                 12)))
                           (let ((app_2
                                  (arithmetic-shift
                                   (let ((b_0 (car accum_0)))
                                     (begin-unsafe (bitwise-and b_0 63)))
                                   6)))
                             (+
                              app_0
                              app_1
                              app_2
                              (begin-unsafe (bitwise-and last-b_0 63)))))))
                      'fail)))))))
        (if (if (let ((or-part_0 (two-byte-prefix? last-b_0)))
                  (if or-part_0
                    or-part_0
                    (let ((or-part_1 (three-byte-prefix? last-b_0)))
                      (if or-part_1 or-part_1 (four-byte-prefix? last-b_0)))))
              (null? accum_0)
              #f)
          'continue
          'fail)))))
(define integer->char*
  (lambda (lower-bound_0 n_0)
    (if (let ((or-part_0 (< n_0 lower-bound_0)))
          (if or-part_0
            or-part_0
            (let ((or-part_1 (> n_0 1114111)))
              (if or-part_1 or-part_1 (if (>= n_0 55296) (<= n_0 57343) #f)))))
      'fail
      (integer->char n_0))))
(define continue-byte? (lambda (b_0) (= (bitwise-and b_0 192) 128)))
(define continue-value (lambda (b_0) (bitwise-and b_0 63)))
(define two-byte-prefix? (lambda (b_0) (= (bitwise-and b_0 224) 192)))
(define three-byte-prefix? (lambda (b_0) (= (bitwise-and b_0 240) 224)))
(define four-byte-prefix? (lambda (b_0) (= (bitwise-and b_0 248) 240)))
(define done-m
  (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0) pos_0))
(define continue-m
  (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
    (|#%app| (car stack_0) pos_0)))
(define limit-m
  (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0) (= pos_0 limit_0)))
(define byte-matcher
  (lambda (b_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (< pos_0 limit_0) (= b_0 (unsafe-bytes-ref s_0 pos_0)) #f)
            (if (lazy-bytes-before-end? s_0 pos_0 limit_0)
              (= b_0 (lazy-bytes-ref s_0 pos_0))
              #f))
        (|#%app|
         next-m_0
         s_0
         (add1 pos_0)
         start_0
         limit_0
         end_0
         state_0
         stack_0)
        #f))))
(define byte-tail-matcher
  (lambda (b_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (< pos_0 limit_0) (= b_0 (unsafe-bytes-ref s_0 pos_0)) #f)
            (if (lazy-bytes-before-end? s_0 pos_0 limit_0)
              (= b_0 (lazy-bytes-ref s_0 pos_0))
              #f))
        (add1 pos_0)
        #f))))
(define byte-matcher*
  (lambda (b_0 max_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0)
      (if (bytes? s_0)
        (let ((limit_1 (if max_0 (min limit_0 (+ pos_0 (* 1 max_0))) limit_0)))
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (pos_1 n_0)
                (begin
                  (let ((pos3_0 (+ pos_1 1)))
                    (if (let ((or-part_0 (> pos3_0 limit_1)))
                          (if or-part_0
                            or-part_0
                            (not (= b_0 (unsafe-bytes-ref s_0 pos_1)))))
                      (values pos_1 n_0 1)
                      (loop_0 pos3_0 (add1 n_0)))))))))
           (loop_0 pos_0 0)))
        (let ((limit_1 (if max_0 (+ pos_0 (* 1 max_0)) #f)))
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (pos_1 n_0)
                (begin
                  (if (let ((or-part_0
                             (if limit_1 (> (+ pos_1 1) limit_1) #f)))
                        (if or-part_0
                          or-part_0
                          (let ((or-part_1
                                 (not
                                  (lazy-bytes-before-end? s_0 pos_1 limit_1))))
                            (if or-part_1
                              or-part_1
                              (not (= b_0 (lazy-bytes-ref s_0 pos_1)))))))
                    (values pos_1 n_0 1)
                    (let ((app_0 (+ pos_1 1))) (loop_0 app_0 (add1 n_0)))))))))
           (loop_0 pos_0 0)))))))
(define bytes-matcher
  (lambda (bstr_0 len_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (<= (+ pos_0 len_0) limit_0)
              (call-with-values
               (lambda ()
                 (unsafe-normalise-inputs
                  unsafe-bytes-length
                  bstr_0
                  0
                  len_0
                  1))
               (case-lambda
                ((v*_0 start*_0 stop*_0 step*_0)
                 (call-with-values
                  (lambda ()
                    (unsafe-normalise-inputs
                     unsafe-bytes-length
                     s_0
                     pos_0
                     (+ pos_0 len_0)
                     1))
                  (case-lambda
                   ((v*_1 start*_1 stop*_1 step*_1)
                    (let ((v*_2 v*_0)
                          (start*_2 start*_0)
                          (stop*_2 stop*_0)
                          (step*_2 step*_0))
                      (begin
                        #t
                        #t
                        (letrec*
                         ((for-loop_0
                           (|#%name|
                            for-loop
                            (lambda (result_0 idx_0 idx_1)
                              (begin
                                (if (if (unsafe-fx< idx_0 stop*_2)
                                      (unsafe-fx< idx_1 stop*_1)
                                      #f)
                                  (let ((c1_0 (unsafe-bytes-ref v*_2 idx_0)))
                                    (let ((c2_0 (unsafe-bytes-ref v*_1 idx_1)))
                                      (let ((c1_1 c1_0))
                                        (let ((result_1
                                               (let ((result_1 (= c1_1 c2_0)))
                                                 (values result_1))))
                                          (if (if (not
                                                   (let ((x_0 (list c1_1)))
                                                     (not result_1)))
                                                (if (not
                                                     (let ((x_0 (list c2_0)))
                                                       (not result_1)))
                                                  #t
                                                  #f)
                                                #f)
                                            (for-loop_0
                                             result_1
                                             (unsafe-fx+ idx_0 1)
                                             (unsafe-fx+ idx_1 1))
                                            result_1)))))
                                  result_0))))))
                         (for-loop_0 #t start*_2 start*_1)))))
                   (args (raise-binding-result-arity-error 4 args)))))
                (args (raise-binding-result-arity-error 4 args))))
              #f)
            (call-with-values
             (lambda ()
               (unsafe-normalise-inputs unsafe-bytes-length bstr_0 0 len_0 1))
             (case-lambda
              ((v*_0 start*_0 stop*_0 step*_0)
               (let ((start_1 pos_0))
                 (let ((v*_1 v*_0)
                       (start*_1 start*_0)
                       (stop*_1 stop*_0)
                       (step*_1 step*_0))
                   (begin
                     #t
                     (void)
                     (letrec*
                      ((for-loop_0
                        (|#%name|
                         for-loop
                         (lambda (result_0 idx_0 pos_1)
                           (begin
                             (if (if (unsafe-fx< idx_0 stop*_1) #t #f)
                               (let ((c1_0 (unsafe-bytes-ref v*_1 idx_0)))
                                 (let ((result_1
                                        (let ((result_1
                                               (if (lazy-bytes-before-end?
                                                    s_0
                                                    pos_1
                                                    limit_0)
                                                 (let ((c2_0
                                                        (lazy-bytes-ref
                                                         s_0
                                                         pos_1)))
                                                   (= c1_0 c2_0))
                                                 #f)))
                                          (values result_1))))
                                   (if (if (not
                                            (let ((x_0 (list c1_0)))
                                              (not result_1)))
                                         (if (not
                                              (let ((x_0 (list pos_1)))
                                                (not result_1)))
                                           #t
                                           #f)
                                         #f)
                                     (for-loop_0
                                      result_1
                                      (unsafe-fx+ idx_0 1)
                                      (+ pos_1 1))
                                     result_1)))
                               result_0))))))
                      (for-loop_0 #t start*_1 start_1))))))
              (args (raise-binding-result-arity-error 4 args)))))
        (|#%app|
         next-m_0
         s_0
         (+ pos_0 len_0)
         start_0
         limit_0
         end_0
         state_0
         stack_0)
        #f))))
(define bytes-tail-matcher
  (lambda (bstr_0 len_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (<= (+ pos_0 len_0) limit_0)
              (call-with-values
               (lambda ()
                 (unsafe-normalise-inputs
                  unsafe-bytes-length
                  bstr_0
                  0
                  len_0
                  1))
               (case-lambda
                ((v*_0 start*_0 stop*_0 step*_0)
                 (call-with-values
                  (lambda ()
                    (unsafe-normalise-inputs
                     unsafe-bytes-length
                     s_0
                     pos_0
                     (+ pos_0 len_0)
                     1))
                  (case-lambda
                   ((v*_1 start*_1 stop*_1 step*_1)
                    (let ((v*_2 v*_0)
                          (start*_2 start*_0)
                          (stop*_2 stop*_0)
                          (step*_2 step*_0))
                      (begin
                        #t
                        #t
                        (letrec*
                         ((for-loop_0
                           (|#%name|
                            for-loop
                            (lambda (result_0 idx_0 idx_1)
                              (begin
                                (if (if (unsafe-fx< idx_0 stop*_2)
                                      (unsafe-fx< idx_1 stop*_1)
                                      #f)
                                  (let ((c1_0 (unsafe-bytes-ref v*_2 idx_0)))
                                    (let ((c2_0 (unsafe-bytes-ref v*_1 idx_1)))
                                      (let ((c1_1 c1_0))
                                        (let ((result_1
                                               (let ((result_1 (= c1_1 c2_0)))
                                                 (values result_1))))
                                          (if (if (not
                                                   (let ((x_0 (list c1_1)))
                                                     (not result_1)))
                                                (if (not
                                                     (let ((x_0 (list c2_0)))
                                                       (not result_1)))
                                                  #t
                                                  #f)
                                                #f)
                                            (for-loop_0
                                             result_1
                                             (unsafe-fx+ idx_0 1)
                                             (unsafe-fx+ idx_1 1))
                                            result_1)))))
                                  result_0))))))
                         (for-loop_0 #t start*_2 start*_1)))))
                   (args (raise-binding-result-arity-error 4 args)))))
                (args (raise-binding-result-arity-error 4 args))))
              #f)
            (call-with-values
             (lambda ()
               (unsafe-normalise-inputs unsafe-bytes-length bstr_0 0 len_0 1))
             (case-lambda
              ((v*_0 start*_0 stop*_0 step*_0)
               (let ((start_1 pos_0))
                 (let ((v*_1 v*_0)
                       (start*_1 start*_0)
                       (stop*_1 stop*_0)
                       (step*_1 step*_0))
                   (begin
                     #t
                     (void)
                     (letrec*
                      ((for-loop_0
                        (|#%name|
                         for-loop
                         (lambda (result_0 idx_0 pos_1)
                           (begin
                             (if (if (unsafe-fx< idx_0 stop*_1) #t #f)
                               (let ((c1_0 (unsafe-bytes-ref v*_1 idx_0)))
                                 (let ((result_1
                                        (let ((result_1
                                               (if (lazy-bytes-before-end?
                                                    s_0
                                                    pos_1
                                                    limit_0)
                                                 (let ((c2_0
                                                        (lazy-bytes-ref
                                                         s_0
                                                         pos_1)))
                                                   (= c1_0 c2_0))
                                                 #f)))
                                          (values result_1))))
                                   (if (if (not
                                            (let ((x_0 (list c1_0)))
                                              (not result_1)))
                                         (if (not
                                              (let ((x_0 (list pos_1)))
                                                (not result_1)))
                                           #t
                                           #f)
                                         #f)
                                     (for-loop_0
                                      result_1
                                      (unsafe-fx+ idx_0 1)
                                      (+ pos_1 1))
                                     result_1)))
                               result_0))))))
                      (for-loop_0 #t start*_1 start_1))))))
              (args (raise-binding-result-arity-error 4 args)))))
        (+ pos_0 len_0)
        #f))))
(define bytes-matcher*
  (lambda (bstr_0 max_0)
    (let ((len_0 (unsafe-bytes-length bstr_0)))
      (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0)
        (if (bytes? s_0)
          (let ((limit_1
                 (if max_0 (min limit_0 (+ pos_0 (* len_0 max_0))) limit_0)))
            (letrec*
             ((loop_0
               (|#%name|
                loop
                (lambda (pos_1 n_0)
                  (begin
                    (let ((pos3_0 (+ pos_1 len_0)))
                      (if (let ((or-part_0 (> pos3_0 limit_1)))
                            (if or-part_0
                              or-part_0
                              (not
                               (call-with-values
                                (lambda ()
                                  (unsafe-normalise-inputs
                                   unsafe-bytes-length
                                   bstr_0
                                   0
                                   len_0
                                   1))
                                (case-lambda
                                 ((v*_0 start*_0 stop*_0 step*_0)
                                  (call-with-values
                                   (lambda ()
                                     (unsafe-normalise-inputs
                                      unsafe-bytes-length
                                      s_0
                                      pos_1
                                      (+ pos_1 len_0)
                                      1))
                                   (case-lambda
                                    ((v*_1 start*_1 stop*_1 step*_1)
                                     (let ((v*_2 v*_0)
                                           (start*_2 start*_0)
                                           (stop*_2 stop*_0)
                                           (step*_2 step*_0))
                                       (begin
                                         #t
                                         #t
                                         (letrec*
                                          ((for-loop_0
                                            (|#%name|
                                             for-loop
                                             (lambda (result_0 idx_0 idx_1)
                                               (begin
                                                 (if (if (unsafe-fx<
                                                          idx_0
                                                          stop*_2)
                                                       (unsafe-fx<
                                                        idx_1
                                                        stop*_1)
                                                       #f)
                                                   (let ((c1_0
                                                          (unsafe-bytes-ref
                                                           v*_2
                                                           idx_0)))
                                                     (let ((c2_0
                                                            (unsafe-bytes-ref
                                                             v*_1
                                                             idx_1)))
                                                       (let ((c1_1 c1_0))
                                                         (let ((result_1
                                                                (let ((result_1
                                                                       (=
                                                                        c1_1
                                                                        c2_0)))
                                                                  (values
                                                                   result_1))))
                                                           (if (if (not
                                                                    (let ((x_0
                                                                           (list
                                                                            c1_1)))
                                                                      (not
                                                                       result_1)))
                                                                 (if (not
                                                                      (let ((x_0
                                                                             (list
                                                                              c2_0)))
                                                                        (not
                                                                         result_1)))
                                                                   #t
                                                                   #f)
                                                                 #f)
                                                             (for-loop_0
                                                              result_1
                                                              (unsafe-fx+
                                                               idx_0
                                                               1)
                                                              (unsafe-fx+
                                                               idx_1
                                                               1))
                                                             result_1)))))
                                                   result_0))))))
                                          (for-loop_0 #t start*_2 start*_1)))))
                                    (args
                                     (raise-binding-result-arity-error
                                      4
                                      args)))))
                                 (args
                                  (raise-binding-result-arity-error
                                   4
                                   args)))))))
                        (values pos_1 n_0 len_0)
                        (loop_0 pos3_0 (add1 n_0)))))))))
             (loop_0 pos_0 0)))
          (let ((limit_1 (if max_0 (+ pos_0 (* len_0 max_0)) #f)))
            (letrec*
             ((loop_0
               (|#%name|
                loop
                (lambda (pos_1 n_0)
                  (begin
                    (if (let ((or-part_0
                               (if limit_1 (> (+ pos_1 len_0) limit_1) #f)))
                          (if or-part_0
                            or-part_0
                            (let ((or-part_1
                                   (not
                                    (lazy-bytes-before-end?
                                     s_0
                                     pos_1
                                     limit_1))))
                              (if or-part_1
                                or-part_1
                                (not
                                 (call-with-values
                                  (lambda ()
                                    (unsafe-normalise-inputs
                                     unsafe-bytes-length
                                     bstr_0
                                     0
                                     len_0
                                     1))
                                  (case-lambda
                                   ((v*_0 start*_0 stop*_0 step*_0)
                                    (let ((start_1 pos_1))
                                      (let ((v*_1 v*_0)
                                            (start*_1 start*_0)
                                            (stop*_1 stop*_0)
                                            (step*_1 step*_0))
                                        (begin
                                          #t
                                          (void)
                                          (letrec*
                                           ((for-loop_0
                                             (|#%name|
                                              for-loop
                                              (lambda (result_0 idx_0 pos_2)
                                                (begin
                                                  (if (if (unsafe-fx<
                                                           idx_0
                                                           stop*_1)
                                                        #t
                                                        #f)
                                                    (let ((c1_0
                                                           (unsafe-bytes-ref
                                                            v*_1
                                                            idx_0)))
                                                      (let ((result_1
                                                             (let ((result_1
                                                                    (if (lazy-bytes-before-end?
                                                                         s_0
                                                                         pos_2
                                                                         limit_1)
                                                                      (let ((c2_0
                                                                             (lazy-bytes-ref
                                                                              s_0
                                                                              pos_2)))
                                                                        (=
                                                                         c1_0
                                                                         c2_0))
                                                                      #f)))
                                                               (values
                                                                result_1))))
                                                        (if (if (not
                                                                 (let ((x_0
                                                                        (list
                                                                         c1_0)))
                                                                   (not
                                                                    result_1)))
                                                              (if (not
                                                                   (let ((x_0
                                                                          (list
                                                                           pos_2)))
                                                                     (not
                                                                      result_1)))
                                                                #t
                                                                #f)
                                                              #f)
                                                          (for-loop_0
                                                           result_1
                                                           (unsafe-fx+ idx_0 1)
                                                           (+ pos_2 1))
                                                          result_1)))
                                                    result_0))))))
                                           (for-loop_0
                                            #t
                                            start*_1
                                            start_1))))))
                                   (args
                                    (raise-binding-result-arity-error
                                     4
                                     args)))))))))
                      (values pos_1 n_0 len_0)
                      (let ((app_0 (+ pos_1 len_0)))
                        (loop_0 app_0 (add1 n_0)))))))))
             (loop_0 pos_0 0))))))))
(define never-matcher
  (lambda () (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0) #f)))
(define any-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (< pos_0 limit_0)
            (lazy-bytes-before-end? s_0 pos_0 limit_0))
        (|#%app|
         next-m_0
         s_0
         (add1 pos_0)
         start_0
         limit_0
         end_0
         state_0
         stack_0)
        #f))))
(define any-tail-matcher
  (lambda ()
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (< pos_0 limit_0)
            (lazy-bytes-before-end? s_0 pos_0 limit_0))
        (add1 pos_0)
        #f))))
(define any-matcher*
  (lambda (max-repeat_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0)
      (if (bytes? s_0)
        (let ((n_0
               (if max-repeat_0
                 (min max-repeat_0 (- limit_0 pos_0))
                 (- limit_0 pos_0))))
          (values (+ pos_0 n_0) n_0 1))
        (letrec*
         ((grow-loop_0
           (|#%name|
            grow-loop
            (lambda (size_0)
              (begin
                (let ((n_0 (if max-repeat_0 (min size_0 max-repeat_0) size_0)))
                  (let ((pos2_0 (+ pos_0 n_0)))
                    (if (if (lazy-bytes-before-end? s_0 (sub1 pos2_0) limit_0)
                          (let ((or-part_0 (not max-repeat_0)))
                            (if or-part_0 or-part_0 (< n_0 max-repeat_0)))
                          #f)
                      (grow-loop_0 (* size_0 2))
                      (letrec*
                       ((search-loop_0
                         (|#%name|
                          search-loop
                          (lambda (min_0 too-high_0)
                            (begin
                              (let ((mid_0 (quotient (+ min_0 too-high_0) 2)))
                                (if (= mid_0 min_0)
                                  (values mid_0 (- mid_0 pos_0) 1)
                                  (if (lazy-bytes-before-end?
                                       s_0
                                       (sub1 mid_0)
                                       limit_0)
                                    (search-loop_0 mid_0 too-high_0)
                                    (search-loop_0 min_0 mid_0)))))))))
                       (search-loop_0 pos_0 (add1 pos2_0)))))))))))
         (grow-loop_0 1))))))
(define range-matcher
  (lambda (rng_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (< pos_0 limit_0)
              (let ((v_0 (unsafe-bytes-ref s_0 pos_0)))
                (begin-unsafe (eq? 1 (unsafe-bytes-ref rng_0 v_0))))
              #f)
            (if (lazy-bytes-before-end? s_0 pos_0 limit_0)
              (let ((v_0 (lazy-bytes-ref s_0 pos_0)))
                (begin-unsafe (eq? 1 (unsafe-bytes-ref rng_0 v_0))))
              #f))
        (|#%app|
         next-m_0
         s_0
         (add1 pos_0)
         start_0
         limit_0
         end_0
         state_0
         stack_0)
        #f))))
(define range-tail-matcher
  (lambda (rng_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (if (< pos_0 limit_0)
              (let ((v_0 (unsafe-bytes-ref s_0 pos_0)))
                (begin-unsafe (eq? 1 (unsafe-bytes-ref rng_0 v_0))))
              #f)
            (if (lazy-bytes-before-end? s_0 pos_0 limit_0)
              (let ((v_0 (lazy-bytes-ref s_0 pos_0)))
                (begin-unsafe (eq? 1 (unsafe-bytes-ref rng_0 v_0))))
              #f))
        (add1 pos_0)
        #f))))
(define range-matcher*
  (lambda (rng_0 max_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0)
      (if (bytes? s_0)
        (let ((limit_1 (if max_0 (min limit_0 (+ pos_0 (* 1 max_0))) limit_0)))
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (pos_1 n_0)
                (begin
                  (let ((pos3_0 (+ pos_1 1)))
                    (if (let ((or-part_0 (> pos3_0 limit_1)))
                          (if or-part_0
                            or-part_0
                            (not
                             (let ((v_0 (unsafe-bytes-ref s_0 pos_1)))
                               (begin-unsafe
                                (eq? 1 (unsafe-bytes-ref rng_0 v_0)))))))
                      (values pos_1 n_0 1)
                      (loop_0 pos3_0 (add1 n_0)))))))))
           (loop_0 pos_0 0)))
        (let ((limit_1 (if max_0 (+ pos_0 (* 1 max_0)) #f)))
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (pos_1 n_0)
                (begin
                  (if (let ((or-part_0
                             (if limit_1 (> (+ pos_1 1) limit_1) #f)))
                        (if or-part_0
                          or-part_0
                          (let ((or-part_1
                                 (not
                                  (lazy-bytes-before-end? s_0 pos_1 limit_1))))
                            (if or-part_1
                              or-part_1
                              (not
                               (let ((v_0 (lazy-bytes-ref s_0 pos_1)))
                                 (begin-unsafe
                                  (eq? 1 (unsafe-bytes-ref rng_0 v_0)))))))))
                    (values pos_1 n_0 1)
                    (let ((app_0 (+ pos_1 1))) (loop_0 app_0 (add1 n_0)))))))))
           (loop_0 pos_0 0)))))))
(define start-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (= pos_0 start_0)
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define end-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (= pos_0 end_0)
            (not (lazy-bytes-before-end? s_0 pos_0 end_0)))
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define line-start-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (let ((or-part_0 (= pos_0 start_0)))
            (if or-part_0
              or-part_0
              (if (bytes? s_0)
                (= 10 (unsafe-bytes-ref s_0 (sub1 pos_0)))
                (if (lazy-bytes-before-end? s_0 (sub1 pos_0) limit_0)
                  (= 10 (lazy-bytes-ref s_0 (sub1 pos_0)))
                  #f))))
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define line-end-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (if (bytes? s_0)
            (let ((or-part_0 (= pos_0 end_0)))
              (if or-part_0 or-part_0 (= 10 (unsafe-bytes-ref s_0 pos_0))))
            (let ((or-part_0 (not (lazy-bytes-before-end? s_0 pos_0 end_0))))
              (if or-part_0 or-part_0 (= 10 (lazy-bytes-ref s_0 pos_0)))))
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define word-boundary-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (word-boundary? s_0 pos_0 start_0 limit_0 end_0)
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define not-word-boundary-matcher
  (lambda (next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (not (word-boundary? s_0 pos_0 start_0 limit_0 end_0))
        (|#%app| next-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        #f))))
(define word-boundary?
  (lambda (s_0 pos_0 start_0 limit_0 end_0)
    (not
     (let ((app_0
            (let ((or-part_0 (= pos_0 start_0)))
              (if or-part_0
                or-part_0
                (not
                 (if (bytes? s_0)
                   (word-byte? (unsafe-bytes-ref s_0 (sub1 pos_0)))
                   (if (lazy-bytes-before-end? s_0 (sub1 pos_0) end_0)
                     (word-byte? (lazy-bytes-ref s_0 (sub1 pos_0)))
                     #f)))))))
       (eq?
        app_0
        (let ((or-part_0
               (if (bytes? s_0)
                 (= pos_0 end_0)
                 (not (lazy-bytes-before-end? s_0 pos_0 end_0)))))
          (if or-part_0
            or-part_0
            (not
             (word-byte?
              (if (bytes? s_0)
                (unsafe-bytes-ref s_0 pos_0)
                (lazy-bytes-ref s_0 pos_0)))))))))))
(define word-byte?
  (lambda (c_0)
    (let ((or-part_0 (if (>= c_0 48) (<= c_0 57) #f)))
      (if or-part_0
        or-part_0
        (let ((or-part_1 (if (>= c_0 97) (<= c_0 122) #f)))
          (if or-part_1
            or-part_1
            (let ((or-part_2 (if (>= c_0 65) (<= c_0 90) #f)))
              (if or-part_2 or-part_2 (= c_0 95)))))))))
(define alts-matcher
  (lambda (m1_0 m2_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((or-part_0
             (|#%app| m1_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)))
        (if or-part_0
          or-part_0
          (|#%app| m2_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0))))))
(define repeat-matcher
  (lambda (r-m_0 min_0 max_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (letrec*
       ((rloop_0
         (|#%name|
          rloop
          (lambda (pos_1 n_0)
            (begin
              (if (< n_0 min_0)
                (let ((new-stack_0
                       (cons
                        (lambda (pos_2) (rloop_0 pos_2 (add1 n_0)))
                        stack_0)))
                  (|#%app|
                   r-m_0
                   s_0
                   pos_1
                   start_0
                   limit_0
                   end_0
                   state_0
                   new-stack_0))
                (if (if max_0 (= n_0 max_0) #f)
                  (|#%app|
                   next-m_0
                   s_0
                   pos_1
                   start_0
                   limit_0
                   end_0
                   state_0
                   stack_0)
                  (let ((new-stack_0
                         (cons
                          (lambda (pos_2) (rloop_0 pos_2 (add1 n_0)))
                          stack_0)))
                    (let ((or-part_0
                           (|#%app|
                            r-m_0
                            s_0
                            pos_1
                            start_0
                            limit_0
                            end_0
                            state_0
                            new-stack_0)))
                      (if or-part_0
                        or-part_0
                        (|#%app|
                         next-m_0
                         s_0
                         pos_1
                         start_0
                         limit_0
                         end_0
                         state_0
                         stack_0)))))))))))
       (rloop_0 pos_0 0)))))
(define r-stack (list (lambda (pos_0) pos_0)))
(define repeat-simple-matcher
  (lambda (r-m_0 min_0 max_0 group-n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (letrec*
       ((rloop_0
         (|#%name|
          rloop
          (lambda (pos_1 n_0 back-amt_0)
            (begin
              (let ((pos2_0
                     (if (let ((or-part_0 (not max_0)))
                           (if or-part_0 or-part_0 (< n_0 max_0)))
                       (|#%app|
                        r-m_0
                        s_0
                        pos_1
                        start_0
                        limit_0
                        end_0
                        state_0
                        r-stack)
                       #f)))
                (if pos2_0
                  (let ((app_0 (add1 n_0)))
                    (rloop_0 pos2_0 app_0 (- pos2_0 pos_1)))
                  (letrec*
                   ((bloop_0
                     (|#%name|
                      bloop
                      (lambda (pos_2 n_1)
                        (begin
                          (if (< n_1 min_0)
                            #f
                            (if (if group-n_0 state_0 #f)
                              (let ((old-span_0
                                     (vector-ref state_0 group-n_0)))
                                (begin
                                  (vector-set!
                                   state_0
                                   group-n_0
                                   (if (zero? n_1)
                                     #f
                                     (cons (- pos_2 back-amt_0) pos_2)))
                                  (let ((group-revert_0
                                         (|#%name|
                                          group-revert
                                          (lambda ()
                                            (begin
                                              (vector-set!
                                               state_0
                                               group-n_0
                                               old-span_0))))))
                                    (let ((or-part_0
                                           (|#%app|
                                            next-m_0
                                            s_0
                                            pos_2
                                            start_0
                                            limit_0
                                            end_0
                                            state_0
                                            stack_0)))
                                      (if or-part_0
                                        or-part_0
                                        (begin
                                          (group-revert_0)
                                          (let ((app_0 (- pos_2 back-amt_0)))
                                            (bloop_0 app_0 (sub1 n_1)))))))))
                              (let ((group-revert_0
                                     (|#%name|
                                      group-revert
                                      (lambda () (begin (void))))))
                                (let ((or-part_0
                                       (|#%app|
                                        next-m_0
                                        s_0
                                        pos_2
                                        start_0
                                        limit_0
                                        end_0
                                        state_0
                                        stack_0)))
                                  (if or-part_0
                                    or-part_0
                                    (begin
                                      (group-revert_0)
                                      (let ((app_0 (- pos_2 back-amt_0)))
                                        (bloop_0 app_0 (sub1 n_1))))))))))))))
                   (bloop_0 pos_1 n_0)))))))))
       (rloop_0 pos_0 0 0)))))
(define repeat-simple-many-matcher
  (lambda (r-m*_0 min_0 max_0 group-n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (call-with-values
       (lambda () (|#%app| r-m*_0 s_0 pos_0 start_0 limit_0 end_0 state_0))
       (case-lambda
        ((pos2_0 n_0 back-amt_0)
         (letrec*
          ((bloop_0
            (|#%name|
             bloop
             (lambda (pos_1 n_1)
               (begin
                 (if (< n_1 min_0)
                   #f
                   (if (if group-n_0 state_0 #f)
                     (let ((old-span_0 (vector-ref state_0 group-n_0)))
                       (begin
                         (vector-set!
                          state_0
                          group-n_0
                          (if (zero? n_1)
                            #f
                            (cons (- pos_1 back-amt_0) pos_1)))
                         (let ((group-revert_0
                                (|#%name|
                                 group-revert
                                 (lambda ()
                                   (begin
                                     (vector-set!
                                      state_0
                                      group-n_0
                                      old-span_0))))))
                           (let ((or-part_0
                                  (|#%app|
                                   next-m_0
                                   s_0
                                   pos_1
                                   start_0
                                   limit_0
                                   end_0
                                   state_0
                                   stack_0)))
                             (if or-part_0
                               or-part_0
                               (begin
                                 (group-revert_0)
                                 (let ((app_0 (- pos_1 back-amt_0)))
                                   (bloop_0 app_0 (sub1 n_1)))))))))
                     (let ((group-revert_0
                            (|#%name|
                             group-revert
                             (lambda () (begin (void))))))
                       (let ((or-part_0
                              (|#%app|
                               next-m_0
                               s_0
                               pos_1
                               start_0
                               limit_0
                               end_0
                               state_0
                               stack_0)))
                         (if or-part_0
                           or-part_0
                           (begin
                             (group-revert_0)
                             (let ((app_0 (- pos_1 back-amt_0)))
                               (bloop_0 app_0 (sub1 n_1))))))))))))))
          (bloop_0 pos2_0 n_0)))
        (args (raise-binding-result-arity-error 3 args)))))))
(define lazy-repeat-matcher
  (lambda (r-m_0 min_0 max_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (letrec*
       ((rloop_0
         (|#%name|
          rloop
          (lambda (pos_1 n_0 min_1)
            (begin
              (if (< n_0 min_1)
                (let ((new-stack_0
                       (cons
                        (lambda (pos_2) (rloop_0 pos_2 (add1 n_0) min_1))
                        stack_0)))
                  (|#%app|
                   r-m_0
                   s_0
                   pos_1
                   start_0
                   limit_0
                   end_0
                   state_0
                   new-stack_0))
                (if (if max_0 (= n_0 max_0) #f)
                  (|#%app|
                   next-m_0
                   s_0
                   pos_1
                   start_0
                   limit_0
                   end_0
                   state_0
                   stack_0)
                  (let ((or-part_0
                         (|#%app|
                          next-m_0
                          s_0
                          pos_1
                          start_0
                          limit_0
                          end_0
                          state_0
                          stack_0)))
                    (if or-part_0
                      or-part_0
                      (rloop_0 pos_1 n_0 (add1 min_1)))))))))))
       (rloop_0 pos_0 0 min_0)))))
(define lazy-repeat-simple-matcher
  (lambda (r-m_0 min_0 max_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (letrec*
       ((rloop_0
         (|#%name|
          rloop
          (lambda (pos_1 n_0 min_1)
            (begin
              (if (< n_0 min_1)
                (let ((pos2_0
                       (|#%app|
                        r-m_0
                        s_0
                        pos_1
                        start_0
                        limit_0
                        end_0
                        state_0
                        stack_0)))
                  (if pos2_0 (rloop_0 pos2_0 (add1 n_0) min_1) #f))
                (if (if max_0 (= n_0 max_0) #f)
                  (|#%app|
                   next-m_0
                   s_0
                   pos_1
                   start_0
                   limit_0
                   end_0
                   state_0
                   stack_0)
                  (let ((or-part_0
                         (|#%app|
                          next-m_0
                          s_0
                          pos_1
                          start_0
                          limit_0
                          end_0
                          state_0
                          stack_0)))
                    (if or-part_0
                      or-part_0
                      (rloop_0 pos_1 n_0 (add1 min_1)))))))))))
       (rloop_0 pos_0 0 min_0)))))
(define group-push-matcher
  (lambda (n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((new-stack_0
             (cons
              (cons pos_0 (if state_0 (vector-ref state_0 n_0) #f))
              stack_0)))
        (|#%app|
         next-m_0
         s_0
         pos_0
         start_0
         limit_0
         end_0
         state_0
         new-stack_0)))))
(define group-set-matcher
  (lambda (n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((old-pos+span_0 (car stack_0)))
        (let ((old-span_0 (cdr old-pos+span_0)))
          (begin
            (if state_0
              (vector-set! state_0 n_0 (cons (car old-pos+span_0) pos_0))
              (void))
            (let ((or-part_0
                   (|#%app|
                    next-m_0
                    s_0
                    pos_0
                    start_0
                    limit_0
                    end_0
                    state_0
                    (cdr stack_0))))
              (if or-part_0
                or-part_0
                (begin
                  (if state_0 (vector-set! state_0 n_0 old-span_0) (void))
                  #f)))))))))
(define reference-matcher
  (lambda (n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((p_0 (vector-ref state_0 n_0)))
        (if (not p_0)
          #f
          (let ((len_0 (let ((app_0 (cdr p_0))) (- app_0 (car p_0)))))
            (let ((matches?_0
                   (if (bytes? s_0)
                     (if (<= (+ pos_0 len_0) limit_0)
                       (call-with-values
                        (lambda ()
                          (let ((app_0 (car p_0)))
                            (unsafe-normalise-inputs
                             unsafe-bytes-length
                             s_0
                             app_0
                             (cdr p_0)
                             1)))
                        (case-lambda
                         ((v*_0 start*_0 stop*_0 step*_0)
                          (call-with-values
                           (lambda ()
                             (unsafe-normalise-inputs
                              unsafe-bytes-length
                              s_0
                              pos_0
                              (+ pos_0 len_0)
                              1))
                           (case-lambda
                            ((v*_1 start*_1 stop*_1 step*_1)
                             (let ((v*_2 v*_0)
                                   (start*_2 start*_0)
                                   (stop*_2 stop*_0)
                                   (step*_2 step*_0))
                               (begin
                                 #t
                                 #t
                                 (letrec*
                                  ((for-loop_0
                                    (|#%name|
                                     for-loop
                                     (lambda (result_0 idx_0 idx_1)
                                       (begin
                                         (if (if (unsafe-fx< idx_0 stop*_2)
                                               (unsafe-fx< idx_1 stop*_1)
                                               #f)
                                           (let ((c1_0
                                                  (unsafe-bytes-ref
                                                   v*_2
                                                   idx_0)))
                                             (let ((c2_0
                                                    (unsafe-bytes-ref
                                                     v*_1
                                                     idx_1)))
                                               (let ((c1_1 c1_0))
                                                 (let ((result_1
                                                        (let ((result_1
                                                               (= c1_1 c2_0)))
                                                          (values result_1))))
                                                   (if (if (not
                                                            (let ((x_0
                                                                   (list
                                                                    c1_1)))
                                                              (not result_1)))
                                                         (if (not
                                                              (let ((x_0
                                                                     (list
                                                                      c2_0)))
                                                                (not
                                                                 result_1)))
                                                           #t
                                                           #f)
                                                         #f)
                                                     (for-loop_0
                                                      result_1
                                                      (unsafe-fx+ idx_0 1)
                                                      (unsafe-fx+ idx_1 1))
                                                     result_1)))))
                                           result_0))))))
                                  (for-loop_0 #t start*_2 start*_1)))))
                            (args (raise-binding-result-arity-error 4 args)))))
                         (args (raise-binding-result-arity-error 4 args))))
                       #f)
                     (let ((start_1 (car p_0)))
                       (let ((end_1 (cdr p_0)))
                         (let ((start_2 start_1))
                           (begin
                             (letrec*
                              ((for-loop_0
                                (|#%name|
                                 for-loop
                                 (lambda (result_0 pos_1 pos_2)
                                   (begin
                                     (if (if (< pos_1 end_1) #t #f)
                                       (let ((result_1
                                              (let ((result_1
                                                     (if (lazy-bytes-before-end?
                                                          s_0
                                                          pos_2
                                                          limit_0)
                                                       (let ((c1_0
                                                              (lazy-bytes-ref
                                                               s_0
                                                               pos_1)))
                                                         (let ((c2_0
                                                                (lazy-bytes-ref
                                                                 s_0
                                                                 pos_2)))
                                                           (= c1_0 c2_0)))
                                                       #f)))
                                                (values result_1))))
                                         (if (if (not
                                                  (let ((x_0 (list pos_1)))
                                                    (not result_1)))
                                               (if (not
                                                    (let ((x_0 (list pos_2)))
                                                      (not result_1)))
                                                 #t
                                                 #f)
                                               #f)
                                           (let ((app_0 (+ pos_1 1)))
                                             (for-loop_0
                                              result_1
                                              app_0
                                              (+ pos_2 1)))
                                           result_1))
                                       result_0))))))
                              (for-loop_0 #t start_2 pos_0)))))))))
              (if matches?_0
                (|#%app|
                 next-m_0
                 s_0
                 (+ pos_0 len_0)
                 start_0
                 limit_0
                 end_0
                 state_0
                 stack_0)
                #f))))))))
(define reference-matcher/case-insensitive
  (lambda (n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((p_0 (vector-ref state_0 n_0)))
        (if (not p_0)
          #f
          (let ((len_0 (let ((app_0 (cdr p_0))) (- app_0 (car p_0)))))
            (let ((matches?_0
                   (if (bytes? s_0)
                     (if (<= (+ pos_0 len_0) limit_0)
                       (call-with-values
                        (lambda ()
                          (let ((app_0 (car p_0)))
                            (unsafe-normalise-inputs
                             unsafe-bytes-length
                             s_0
                             app_0
                             (cdr p_0)
                             1)))
                        (case-lambda
                         ((v*_0 start*_0 stop*_0 step*_0)
                          (call-with-values
                           (lambda ()
                             (unsafe-normalise-inputs
                              unsafe-bytes-length
                              s_0
                              pos_0
                              (+ pos_0 len_0)
                              1))
                           (case-lambda
                            ((v*_1 start*_1 stop*_1 step*_1)
                             (let ((v*_2 v*_0)
                                   (start*_2 start*_0)
                                   (stop*_2 stop*_0)
                                   (step*_2 step*_0))
                               (begin
                                 #t
                                 #t
                                 (letrec*
                                  ((for-loop_0
                                    (|#%name|
                                     for-loop
                                     (lambda (result_0 idx_0 idx_1)
                                       (begin
                                         (if (if (unsafe-fx< idx_0 stop*_2)
                                               (unsafe-fx< idx_1 stop*_1)
                                               #f)
                                           (let ((c1_0
                                                  (unsafe-bytes-ref
                                                   v*_2
                                                   idx_0)))
                                             (let ((c2_0
                                                    (unsafe-bytes-ref
                                                     v*_1
                                                     idx_1)))
                                               (let ((c1_1 c1_0))
                                                 (let ((result_1
                                                        (let ((result_1
                                                               (let ((app_0
                                                                      (chyte-to-lower
                                                                       c1_1)))
                                                                 (=
                                                                  app_0
                                                                  (chyte-to-lower
                                                                   c2_0)))))
                                                          (values result_1))))
                                                   (if (if (not
                                                            (let ((x_0
                                                                   (list
                                                                    c1_1)))
                                                              (not result_1)))
                                                         (if (not
                                                              (let ((x_0
                                                                     (list
                                                                      c2_0)))
                                                                (not
                                                                 result_1)))
                                                           #t
                                                           #f)
                                                         #f)
                                                     (for-loop_0
                                                      result_1
                                                      (unsafe-fx+ idx_0 1)
                                                      (unsafe-fx+ idx_1 1))
                                                     result_1)))))
                                           result_0))))))
                                  (for-loop_0 #t start*_2 start*_1)))))
                            (args (raise-binding-result-arity-error 4 args)))))
                         (args (raise-binding-result-arity-error 4 args))))
                       #f)
                     (let ((start_1 (car p_0)))
                       (let ((end_1 (cdr p_0)))
                         (let ((start_2 start_1))
                           (begin
                             (letrec*
                              ((for-loop_0
                                (|#%name|
                                 for-loop
                                 (lambda (result_0 pos_1 pos_2)
                                   (begin
                                     (if (if (< pos_1 end_1) #t #f)
                                       (let ((result_1
                                              (let ((result_1
                                                     (if (lazy-bytes-before-end?
                                                          s_0
                                                          pos_2
                                                          limit_0)
                                                       (let ((c1_0
                                                              (lazy-bytes-ref
                                                               s_0
                                                               pos_1)))
                                                         (let ((c2_0
                                                                (lazy-bytes-ref
                                                                 s_0
                                                                 pos_2)))
                                                           (let ((app_0
                                                                  (chyte-to-lower
                                                                   c1_0)))
                                                             (=
                                                              app_0
                                                              (chyte-to-lower
                                                               c2_0)))))
                                                       #f)))
                                                (values result_1))))
                                         (if (if (not
                                                  (let ((x_0 (list pos_1)))
                                                    (not result_1)))
                                               (if (not
                                                    (let ((x_0 (list pos_2)))
                                                      (not result_1)))
                                                 #t
                                                 #f)
                                               #f)
                                           (let ((app_0 (+ pos_1 1)))
                                             (for-loop_0
                                              result_1
                                              app_0
                                              (+ pos_2 1)))
                                           result_1))
                                       result_0))))))
                              (for-loop_0 #t start_2 pos_0)))))))))
              (if matches?_0
                (|#%app|
                 next-m_0
                 s_0
                 (+ pos_0 len_0)
                 start_0
                 limit_0
                 end_0
                 state_0
                 stack_0)
                #f))))))))
(define chyte-to-lower
  (lambda (c_0) (if (if (>= c_0 65) (<= c_0 90) #f) (+ c_0 32) c_0)))
(define lookahead-matcher
  (lambda (match?_0 sub-m_0 n-start_0 num-n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((old-state_0 (save-groups state_0 n-start_0 num-n_0)))
        (let ((pos2_0
               (|#%app| sub-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 null)))
          (if match?_0
            (if pos2_0
              (let ((or-part_0
                     (|#%app|
                      next-m_0
                      s_0
                      pos_0
                      start_0
                      limit_0
                      end_0
                      state_0
                      stack_0)))
                (if or-part_0
                  or-part_0
                  (restore-groups state_0 old-state_0 n-start_0 num-n_0)))
              #f)
            (if pos2_0
              (restore-groups state_0 old-state_0 n-start_0 num-n_0)
              (|#%app|
               next-m_0
               s_0
               pos_0
               start_0
               limit_0
               end_0
               state_0
               stack_0))))))))
(define lookbehind-matcher
  (lambda (match?_0 lb-min_0 lb-max_0 sub-m_0 n-start_0 num-n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((lb-min-pos_0 (max start_0 (- pos_0 lb-max_0))))
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (lb-pos_0)
              (begin
                (if (< lb-pos_0 lb-min-pos_0)
                  (if match?_0
                    #f
                    (|#%app|
                     next-m_0
                     s_0
                     pos_0
                     start_0
                     limit_0
                     end_0
                     state_0
                     stack_0))
                  (let ((old-state_0 (save-groups state_0 n-start_0 num-n_0)))
                    (let ((pos2_0
                           (|#%app|
                            sub-m_0
                            s_0
                            lb-pos_0
                            start_0
                            pos_0
                            end_0
                            state_0
                            null)))
                      (if match?_0
                        (if pos2_0
                          (let ((or-part_0
                                 (|#%app|
                                  next-m_0
                                  s_0
                                  pos_0
                                  start_0
                                  limit_0
                                  end_0
                                  state_0
                                  stack_0)))
                            (if or-part_0
                              or-part_0
                              (restore-groups
                               state_0
                               old-state_0
                               n-start_0
                               num-n_0)))
                          (loop_0 (sub1 lb-pos_0)))
                        (if pos2_0
                          (restore-groups
                           state_0
                           old-state_0
                           n-start_0
                           num-n_0)
                          (|#%app|
                           next-m_0
                           s_0
                           pos_0
                           start_0
                           limit_0
                           end_0
                           state_0
                           stack_0)))))))))))
         (loop_0 (- pos_0 lb-min_0)))))))
(define conditional/reference-matcher
  (lambda (n_0 m1_0 m2_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (if (vector-ref state_0 n_0)
        (|#%app| m1_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
        (|#%app| m2_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)))))
(define conditional/look-matcher
  (lambda (tst-m_0 m1_0 m2_0 n-start_0 num-n_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((old-state_0 (save-groups state_0 n-start_0 num-n_0)))
        (let ((or-part_0
               (if (|#%app|
                    tst-m_0
                    s_0
                    pos_0
                    start_0
                    limit_0
                    end_0
                    state_0
                    null)
                 (|#%app| m1_0 s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
                 (|#%app|
                  m2_0
                  s_0
                  pos_0
                  start_0
                  limit_0
                  end_0
                  state_0
                  stack_0))))
          (if or-part_0
            or-part_0
            (restore-groups state_0 old-state_0 n-start_0 num-n_0)))))))
(define cut-matcher
  (lambda (sub-m_0 n-start_0 num-n_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (let ((old-state_0 (save-groups state_0 n-start_0 num-n_0)))
        (let ((pos2_0
               (|#%app| sub-m_0 s_0 pos_0 start_0 limit_0 end_0 state_0 null)))
          (if pos2_0
            (let ((or-part_0
                   (|#%app|
                    next-m_0
                    s_0
                    pos2_0
                    start_0
                    limit_0
                    end_0
                    state_0
                    stack_0)))
              (if or-part_0
                or-part_0
                (restore-groups state_0 old-state_0 n-start_0 num-n_0)))
            #f))))))
(define save-groups
  (lambda (state_0 n-start_0 num-n_0)
    (if (zero? num-n_0)
      #f
      (if (not state_0)
        #f
        (let ((vec_0 (make-vector num-n_0)))
          (begin
            (vector-copy! vec_0 0 state_0 n-start_0 (+ n-start_0 num-n_0))
            vec_0))))))
(define restore-groups
  (lambda (state_0 old-state_0 n-start_0 num-n_0)
    (begin
      (if old-state_0 (vector-copy! state_0 n-start_0 old-state_0) (void))
      #f)))
(define unicode-categories-matcher
  (lambda (cats_0 match?_0 next-m_0)
    (lambda (s_0 pos_0 start_0 limit_0 end_0 state_0 stack_0)
      (letrec*
       ((loop_0
         (|#%name|
          loop
          (lambda (pos_1 accum_0)
            (begin
              (let ((b_0
                     (if (bytes? s_0)
                       (if (< pos_1 limit_0) (unsafe-bytes-ref s_0 pos_1) #f)
                       (if (lazy-bytes-before-end? s_0 pos_1 limit_0)
                         (lazy-bytes-ref s_0 pos_1)
                         #f))))
                (if (not b_0)
                  #f
                  (let ((c_0 (bytes->char/utf-8 b_0 accum_0)))
                    (if (char? c_0)
                      (if (eq?
                           match?_0
                           (let ((c-cat_0 (char-general-category c_0)))
                             (if (list? cats_0)
                               (begin
                                 (letrec*
                                  ((for-loop_0
                                    (|#%name|
                                     for-loop
                                     (lambda (result_0 lst_0)
                                       (begin
                                         (if (pair? lst_0)
                                           (let ((cat_0 (unsafe-car lst_0)))
                                             (let ((rest_0 (unsafe-cdr lst_0)))
                                               (let ((result_1
                                                      (eq? cat_0 c-cat_0)))
                                                 (let ((result_2
                                                        (values result_1)))
                                                   (if (if (not
                                                            (let ((x_0
                                                                   (list
                                                                    cat_0)))
                                                              result_2))
                                                         #t
                                                         #f)
                                                     (for-loop_0
                                                      result_2
                                                      rest_0)
                                                     result_2)))))
                                           result_0))))))
                                  (for-loop_0 #f cats_0)))
                               (eq? cats_0 c-cat_0))))
                        (|#%app|
                         next-m_0
                         s_0
                         (add1 pos_1)
                         start_0
                         limit_0
                         end_0
                         state_0
                         stack_0)
                        #f)
                      (if (eq? c_0 'fail)
                        #f
                        (let ((app_0 (add1 pos_1)))
                          (loop_0 app_0 (cons b_0 accum_0)))))))))))))
       (loop_0 pos_0 null)))))
(define 1/compile
  (|#%name|
   compile
   (lambda (rx_0)
     (begin
       (letrec*
        ((compile_0
          (|#%name|
           compile
           (lambda (rx_1 next-m_0)
             (begin
               (if (exact-integer? rx_1)
                 (if (eq? next-m_0 done-m)
                   (byte-tail-matcher rx_1)
                   (byte-matcher rx_1 next-m_0))
                 (if (bytes? rx_1)
                   (let ((len_0 (unsafe-bytes-length rx_1)))
                     (if (eq? next-m_0 done-m)
                       (bytes-tail-matcher rx_1 len_0)
                       (bytes-matcher rx_1 len_0 next-m_0)))
                   (if (eq? rx_1 'empty)
                     next-m_0
                     (if (eq? rx_1 'never)
                       (never-matcher)
                       (if (eq? rx_1 'any)
                         (if (eq? next-m_0 done-m)
                           (any-tail-matcher)
                           (any-matcher next-m_0))
                         (if (rx:range? rx_1)
                           (let ((rng_0 (compile-range (rx:range-range rx_1))))
                             (if (eq? next-m_0 done-m)
                               (range-tail-matcher rng_0)
                               (range-matcher rng_0 next-m_0)))
                           (if (eq? rx_1 'start)
                             (start-matcher next-m_0)
                             (if (eq? rx_1 'end)
                               (end-matcher next-m_0)
                               (if (eq? rx_1 'line-start)
                                 (line-start-matcher next-m_0)
                                 (if (eq? rx_1 'line-end)
                                   (line-end-matcher next-m_0)
                                   (if (eq? rx_1 'word-boundary)
                                     (word-boundary-matcher next-m_0)
                                     (if (eq? rx_1 'not-word-boundary)
                                       (not-word-boundary-matcher next-m_0)
                                       (if (rx:sequence? rx_1)
                                         (let ((rxs_0 (rx:sequence-rxs rx_1)))
                                           (letrec*
                                            ((loop_0
                                              (|#%name|
                                               loop
                                               (lambda (rxs_1)
                                                 (begin
                                                   (if (null? rxs_1)
                                                     next-m_0
                                                     (let ((rest-node_0
                                                            (loop_0
                                                             (cdr rxs_1))))
                                                       (compile_0
                                                        (car rxs_1)
                                                        rest-node_0))))))))
                                            (loop_0 rxs_0)))
                                         (if (rx:alts? rx_1)
                                           (let ((app_0
                                                  (compile_0
                                                   (rx:alts-rx_2265 rx_1)
                                                   next-m_0)))
                                             (alts-matcher
                                              app_0
                                              (compile_0
                                               (rx:alts-rx_1912 rx_1)
                                               next-m_0)))
                                           (if (rx:maybe? rx_1)
                                             (if (rx:maybe-non-greedy? rx_1)
                                               (alts-matcher
                                                next-m_0
                                                (compile_0
                                                 (rx:maybe-rx rx_1)
                                                 next-m_0))
                                               (alts-matcher
                                                (compile_0
                                                 (rx:maybe-rx rx_1)
                                                 next-m_0)
                                                next-m_0))
                                             (if (rx:repeat? rx_1)
                                               (let ((actual-r-rx_0
                                                      (rx:repeat-rx rx_1)))
                                                 (let ((r-rx_0
                                                        (if (if (rx:group?
                                                                 actual-r-rx_0)
                                                              (if (not
                                                                   (rx:repeat-non-greedy?
                                                                    rx_1))
                                                                (not
                                                                 (needs-backtrack?
                                                                  (rx:group-rx
                                                                   actual-r-rx_0)))
                                                                #f)
                                                              #f)
                                                          (rx:group-rx
                                                           actual-r-rx_0)
                                                          actual-r-rx_0)))
                                                   (let ((simple?_0
                                                          (not
                                                           (needs-backtrack?
                                                            r-rx_0))))
                                                     (let ((group-n_0
                                                            (if simple?_0
                                                              (if (rx:group?
                                                                   actual-r-rx_0)
                                                                (rx:group-number
                                                                 actual-r-rx_0)
                                                                #f)
                                                              #f)))
                                                       (let ((min_0
                                                              (rx:repeat-min
                                                               rx_1)))
                                                         (let ((max_0
                                                                (let ((n_0
                                                                       (rx:repeat-max
                                                                        rx_1)))
                                                                  (if (=
                                                                       n_0
                                                                       +inf.0)
                                                                    #f
                                                                    n_0))))
                                                           (let ((r-m*_0
                                                                  (compile*/maybe
                                                                   r-rx_0
                                                                   min_0
                                                                   max_0)))
                                                             (if (if r-m*_0
                                                                   (not
                                                                    (rx:repeat-non-greedy?
                                                                     rx_1))
                                                                   #f)
                                                               (repeat-simple-many-matcher
                                                                r-m*_0
                                                                min_0
                                                                max_0
                                                                group-n_0
                                                                next-m_0)
                                                               (let ((r-m_0
                                                                      (compile_0
                                                                       r-rx_0
                                                                       (if simple?_0
                                                                         done-m
                                                                         continue-m))))
                                                                 (if (rx:repeat-non-greedy?
                                                                      rx_1)
                                                                   (if simple?_0
                                                                     (lazy-repeat-simple-matcher
                                                                      r-m_0
                                                                      min_0
                                                                      max_0
                                                                      next-m_0)
                                                                     (lazy-repeat-matcher
                                                                      r-m_0
                                                                      min_0
                                                                      max_0
                                                                      next-m_0))
                                                                   (if simple?_0
                                                                     (repeat-simple-matcher
                                                                      r-m_0
                                                                      min_0
                                                                      max_0
                                                                      group-n_0
                                                                      next-m_0)
                                                                     (repeat-matcher
                                                                      r-m_0
                                                                      min_0
                                                                      max_0
                                                                      next-m_0))))))))))))
                                               (if (rx:group? rx_1)
                                                 (let ((n_0
                                                        (rx:group-number
                                                         rx_1)))
                                                   (let ((m_0
                                                          (let ((app_0
                                                                 (rx:group-rx
                                                                  rx_1)))
                                                            (compile_0
                                                             app_0
                                                             (group-set-matcher
                                                              n_0
                                                              next-m_0)))))
                                                     (group-push-matcher
                                                      n_0
                                                      m_0)))
                                                 (if (rx:reference? rx_1)
                                                   (let ((n_0
                                                          (rx:reference-n
                                                           rx_1)))
                                                     (if (zero? n_0)
                                                       (never-matcher)
                                                       (if (rx:reference-case-sensitive?
                                                            rx_1)
                                                         (reference-matcher
                                                          (sub1 n_0)
                                                          next-m_0)
                                                         (reference-matcher/case-insensitive
                                                          (sub1 n_0)
                                                          next-m_0))))
                                                   (if (rx:cut? rx_1)
                                                     (let ((app_0
                                                            (compile_0
                                                             (rx:cut-rx rx_1)
                                                             done-m)))
                                                       (let ((app_1
                                                              (rx:cut-n-start
                                                               rx_1)))
                                                         (cut-matcher
                                                          app_0
                                                          app_1
                                                          (rx:cut-num-n rx_1)
                                                          next-m_0)))
                                                     (if (rx:conditional? rx_1)
                                                       (let ((tst_0
                                                              (rx:conditional-tst
                                                               rx_1)))
                                                         (let ((m1_0
                                                                (compile_0
                                                                 (rx:conditional-rx_2162
                                                                  rx_1)
                                                                 next-m_0)))
                                                           (let ((m2_0
                                                                  (compile_0
                                                                   (rx:conditional-rx_2328
                                                                    rx_1)
                                                                   next-m_0)))
                                                             (if (rx:reference?
                                                                  tst_0)
                                                               (let ((n_0
                                                                      (sub1
                                                                       (rx:reference-n
                                                                        tst_0))))
                                                                 (conditional/reference-matcher
                                                                  n_0
                                                                  m1_0
                                                                  m2_0))
                                                               (let ((app_0
                                                                      (compile_0
                                                                       tst_0
                                                                       done-m)))
                                                                 (let ((app_1
                                                                        (rx:conditional-n-start
                                                                         rx_1)))
                                                                   (conditional/look-matcher
                                                                    app_0
                                                                    m1_0
                                                                    m2_0
                                                                    app_1
                                                                    (rx:conditional-num-n
                                                                     rx_1))))))))
                                                       (if (rx:lookahead? rx_1)
                                                         (let ((app_0
                                                                (rx:lookahead-match?
                                                                 rx_1)))
                                                           (let ((app_1
                                                                  (compile_0
                                                                   (rx:lookahead-rx
                                                                    rx_1)
                                                                   done-m)))
                                                             (let ((app_2
                                                                    (rx:lookahead-n-start
                                                                     rx_1)))
                                                               (lookahead-matcher
                                                                app_0
                                                                app_1
                                                                app_2
                                                                (rx:lookahead-num-n
                                                                 rx_1)
                                                                next-m_0))))
                                                         (if (rx:lookbehind?
                                                              rx_1)
                                                           (let ((app_0
                                                                  (rx:lookbehind-match?
                                                                   rx_1)))
                                                             (let ((app_1
                                                                    (rx:lookbehind-lb-min
                                                                     rx_1)))
                                                               (let ((app_2
                                                                      (rx:lookbehind-lb-max
                                                                       rx_1)))
                                                                 (let ((app_3
                                                                        (compile_0
                                                                         (rx:lookbehind-rx
                                                                          rx_1)
                                                                         limit-m)))
                                                                   (let ((app_4
                                                                          (rx:lookbehind-n-start
                                                                           rx_1)))
                                                                     (lookbehind-matcher
                                                                      app_0
                                                                      app_1
                                                                      app_2
                                                                      app_3
                                                                      app_4
                                                                      (rx:lookbehind-num-n
                                                                       rx_1)
                                                                      next-m_0))))))
                                                           (if (rx:unicode-categories?
                                                                rx_1)
                                                             (let ((app_0
                                                                    (rx:unicode-categories-symlist
                                                                     rx_1)))
                                                               (unicode-categories-matcher
                                                                app_0
                                                                (rx:unicode-categories-match?
                                                                 rx_1)
                                                                next-m_0))
                                                             (error
                                                              'compile/bt
                                                              "internal error: unrecognized ~s"
                                                              rx_1)))))))))))))))))))))))))))))
        (compile_0 rx_0 done-m))))))
(define compile*/maybe
  (lambda (rx_0 min_0 max_0)
    (if (exact-integer? rx_0)
      (byte-matcher* rx_0 max_0)
      (if (bytes? rx_0)
        (bytes-matcher* rx_0 max_0)
        (if (eq? rx_0 'any)
          (any-matcher* max_0)
          (if (rx:range? rx_0)
            (range-matcher* (compile-range (rx:range-range rx_0)) max_0)
            #f))))))
(define finish_2797
  (make-struct-type-install-properties
   '(regexp)
   10
   0
   #f
   (list
    (cons
     prop:equal+hash
     (list
      (lambda (a_0 b_0 eql?_0)
        (if (let ((app_0 (rx:regexp-px? a_0))) (eq? app_0 (rx:regexp-px? b_0)))
          (let ((app_0 (rx:regexp-source a_0)))
            (equal? app_0 (rx:regexp-source b_0)))
          #f))
      (lambda (a_0 hc_0) (|#%app| hc_0 (rx:regexp-source a_0)))
      (lambda (a_0 hc_0) (|#%app| hc_0 (rx:regexp-source a_0)))))
    (cons prop:object-name 2)
    (cons
     prop:custom-write
     (lambda (rx_0 port_0 mode_0)
       (begin
         (write-bytes (if (rx:regexp-px? rx_0) #vu8(35 112 120) #vu8(35 114 120)) port_0)
         (write (rx:regexp-source rx_0) port_0)))))
   (current-inspector)
   #f
   '(0 1 2 3 4 5 6 7 8 9)
   #f
   'rx:regexp))
(define struct:rx:regexp
  (make-record-type-descriptor
   'regexp
   #f
   (|#%nongenerative-uid| regexp)
   #f
   #f
   '(10 . 0)))
(define effect_2726 (finish_2797 struct:rx:regexp))
(define rx:regexp1.1
  (|#%name|
   rx:regexp
   (record-constructor
    (make-record-constructor-descriptor struct:rx:regexp #f #f))))
(define rx:regexp?_2382 (|#%name| regexp? (record-predicate struct:rx:regexp)))
(define rx:regexp?
  (|#%name|
   regexp?
   (lambda (v)
     (if (rx:regexp?_2382 v)
       #t
       ($value
        (if (impersonator? v) (rx:regexp?_2382 (impersonator-val v)) #f))))))
(define rx:regexp-bytes?_3116
  (|#%name| regexp-bytes? (record-accessor struct:rx:regexp 0)))
(define rx:regexp-bytes?
  (|#%name|
   regexp-bytes?
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-bytes?_3116 s)
       ($value
        (impersonate-ref
         rx:regexp-bytes?_3116
         struct:rx:regexp
         0
         s
         'bytes?))))))
(define rx:regexp-px?_2839
  (|#%name| regexp-px? (record-accessor struct:rx:regexp 1)))
(define rx:regexp-px?
  (|#%name|
   regexp-px?
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-px?_2839 s)
       ($value
        (impersonate-ref rx:regexp-px?_2839 struct:rx:regexp 1 s 'px?))))))
(define rx:regexp-source_2786
  (|#%name| regexp-source (record-accessor struct:rx:regexp 2)))
(define rx:regexp-source
  (|#%name|
   regexp-source
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-source_2786 s)
       ($value
        (impersonate-ref
         rx:regexp-source_2786
         struct:rx:regexp
         2
         s
         'source))))))
(define rx:regexp-matcher_2640
  (|#%name| regexp-matcher (record-accessor struct:rx:regexp 3)))
(define rx:regexp-matcher
  (|#%name|
   regexp-matcher
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-matcher_2640 s)
       ($value
        (impersonate-ref
         rx:regexp-matcher_2640
         struct:rx:regexp
         3
         s
         'matcher))))))
(define rx:regexp-num-groups_2013
  (|#%name| regexp-num-groups (record-accessor struct:rx:regexp 4)))
(define rx:regexp-num-groups
  (|#%name|
   regexp-num-groups
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-num-groups_2013 s)
       ($value
        (impersonate-ref
         rx:regexp-num-groups_2013
         struct:rx:regexp
         4
         s
         'num-groups))))))
(define rx:regexp-references?_2683
  (|#%name| regexp-references? (record-accessor struct:rx:regexp 5)))
(define rx:regexp-references?
  (|#%name|
   regexp-references?
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-references?_2683 s)
       ($value
        (impersonate-ref
         rx:regexp-references?_2683
         struct:rx:regexp
         5
         s
         'references?))))))
(define rx:regexp-max-lookbehind_2091
  (|#%name| regexp-max-lookbehind (record-accessor struct:rx:regexp 6)))
(define rx:regexp-max-lookbehind
  (|#%name|
   regexp-max-lookbehind
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-max-lookbehind_2091 s)
       ($value
        (impersonate-ref
         rx:regexp-max-lookbehind_2091
         struct:rx:regexp
         6
         s
         'max-lookbehind))))))
(define rx:regexp-anchored?_2381
  (|#%name| regexp-anchored? (record-accessor struct:rx:regexp 7)))
(define rx:regexp-anchored?
  (|#%name|
   regexp-anchored?
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-anchored?_2381 s)
       ($value
        (impersonate-ref
         rx:regexp-anchored?_2381
         struct:rx:regexp
         7
         s
         'anchored?))))))
(define rx:regexp-must-string_2180
  (|#%name| regexp-must-string (record-accessor struct:rx:regexp 8)))
(define rx:regexp-must-string
  (|#%name|
   regexp-must-string
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-must-string_2180 s)
       ($value
        (impersonate-ref
         rx:regexp-must-string_2180
         struct:rx:regexp
         8
         s
         'must-string))))))
(define rx:regexp-start-range_2644
  (|#%name| regexp-start-range (record-accessor struct:rx:regexp 9)))
(define rx:regexp-start-range
  (|#%name|
   regexp-start-range
   (lambda (s)
     (if (rx:regexp?_2382 s)
       (rx:regexp-start-range_2644 s)
       ($value
        (impersonate-ref
         rx:regexp-start-range_2644
         struct:rx:regexp
         9
         s
         'start-range))))))
(define make-regexp
  (lambda (who_0 orig-p_0 px?_0 as-bytes?_0 handler_0)
    (call-with-continuation-prompt
     (lambda ()
       (let ((p_0
              (if (bytes? orig-p_0)
                (bytes->immutable-bytes orig-p_0)
                (string->immutable-string orig-p_0))))
         (call-with-values
          (lambda () (parse.1 px?_0 p_0))
          (case-lambda
           ((raw-rx_0 num-groups_0 references?_0)
            (let ((rx_0 (if as-bytes?_0 raw-rx_0 (convert raw-rx_0))))
              (let ((max-lookbehind_0 (validate rx_0 num-groups_0)))
                (let ((matcher_0 (1/compile rx_0)))
                  (let ((app_0 (anchored? rx_0)))
                    (let ((app_1 (get-must-string rx_0)))
                      (rx:regexp1.1
                       as-bytes?_0
                       px?_0
                       p_0
                       matcher_0
                       num-groups_0
                       references?_0
                       max-lookbehind_0
                       app_0
                       app_1
                       (get-start-range rx_0))))))))
           (args (raise-binding-result-arity-error 3 args))))))
     regexp-error-tag
     (lambda (str_0)
       (if handler_0
         (|#%app| handler_0 str_0)
         (raise-arguments-error who_0 str_0 "pattern" orig-p_0))))))
(define 1/regexp?
  (|#%name|
   regexp?
   (lambda (v_0)
     (begin (if (rx:regexp? v_0) (not (rx:regexp-bytes? v_0)) #f)))))
(define 1/byte-regexp?
  (|#%name|
   byte-regexp?
   (lambda (v_0) (begin (if (rx:regexp? v_0) (rx:regexp-bytes? v_0) #f)))))
(define 1/pregexp?
  (|#%name|
   pregexp?
   (lambda (v_0)
     (begin
       (if (rx:regexp? v_0)
         (if (not (rx:regexp-bytes? v_0)) (rx:regexp-px? v_0) #f)
         #f)))))
(define 1/byte-pregexp?
  (|#%name|
   byte-pregexp?
   (lambda (v_0)
     (begin
       (if (rx:regexp? v_0)
         (if (rx:regexp-bytes? v_0) (rx:regexp-px? v_0) #f)
         #f)))))
(define copy-port-bytes
  (lambda (in_0 out_0 n_0)
    (let ((bstr_0 (make-bytes (min 4096 (if n_0 n_0 4096)))))
      (let ((copy_0
             (|#%name|
              copy
              (lambda (got_0 expect_0)
                (begin
                  (if (eof-object? got_0)
                    #f
                    (begin
                      (if out_0 (write-bytes bstr_0 out_0 0 got_0) (void))
                      (let ((or-part_0 (if (not n_0) (positive? got_0) #f)))
                        (if or-part_0
                          or-part_0
                          (if n_0 (= got_0 expect_0) #f))))))))))
        (letrec*
         ((loop_0
           (|#%name|
            loop
            (lambda (n_1)
              (begin
                (if (if n_1 (< n_1 4096) #f)
                  (copy_0 (read-bytes! bstr_0 in_0 0 n_1) n_1)
                  (if (copy_0 (read-bytes! bstr_0 in_0) 4096)
                    (loop_0 (if n_1 (- n_1 4096) #f))
                    #f)))))))
         (loop_0 n_0))))))
(define open-input-bytes/no-copy
  (lambda (bstr_0 pos_0 end_0)
    (let ((fill!_0
           (|#%name|
            fill!
            (lambda (dest-bstr_0 skip_0)
              (begin
                (let ((pos+skip_0 (+ pos_0 skip_0)))
                  (if (>= pos+skip_0 end_0)
                    eof
                    (let ((len_0
                           (min
                            (unsafe-bytes-length dest-bstr_0)
                            (- end_0 pos+skip_0))))
                      (begin
                        (unsafe-bytes-copy!
                         dest-bstr_0
                         0
                         bstr_0
                         pos+skip_0
                         (+ pos+skip_0 len_0))
                        len_0)))))))))
      (make-input-port
       'bytes
       (lambda (dest-bstr_0)
         (let ((len_0 (fill!_0 dest-bstr_0 0)))
           (begin
             (if (eof-object? len_0) (void) (set! pos_0 (+ len_0 pos_0)))
             len_0)))
       (lambda (dest-bstr_0 skip_0 evt_0) (fill!_0 dest-bstr_0 skip_0))
       void))))
(define open-input-string/lazy
  (lambda (str_0 pos_0 end_0)
    (let ((bstr_0 (make-bytes 64)))
      (let ((bstr-pos_0 0))
        (let ((bstr-end_0 0))
          (letrec*
           ((fill!_0
             (|#%name|
              fill!
              (lambda (dest-bstr_0 skip_0)
                (begin
                  (let ((bstr-pos+skip_0 (+ bstr-pos_0 skip_0)))
                    (begin
                      (if (>= bstr-pos+skip_0 bstr-end_0)
                        (decode-more!_0 (add1 bstr-pos+skip_0))
                        (void))
                      (if (>= bstr-pos+skip_0 bstr-end_0)
                        eof
                        (let ((len_0
                               (min
                                (unsafe-bytes-length dest-bstr_0)
                                (- bstr-end_0 bstr-pos+skip_0))))
                          (begin
                            (let ((app_0 bstr_0))
                              (unsafe-bytes-copy!
                               dest-bstr_0
                               0
                               app_0
                               bstr-pos+skip_0
                               (+ bstr-pos+skip_0 len_0)))
                            len_0)))))))))
            (decode-more!_0
             (|#%name|
              decode-more!
              (lambda (target-pos_0)
                (begin
                  (if (= pos_0 end_0)
                    (void)
                    (let ((len_0 (min 64 (- end_0 pos_0))))
                      (let ((new-bstr_0
                             (let ((app_0 pos_0))
                               (string->bytes/utf-8
                                str_0
                                0
                                app_0
                                (+ pos_0 len_0)))))
                        (begin
                          (set! pos_0 (+ len_0 pos_0))
                          (let ((new-len_0 (unsafe-bytes-length new-bstr_0)))
                            (begin
                              (if (<
                                   (let ((app_0 (unsafe-bytes-length bstr_0)))
                                     (- app_0 bstr-end_0))
                                   new-len_0)
                                (let ((bstr2_0
                                       (make-bytes
                                        (let ((app_0
                                               (*
                                                (unsafe-bytes-length bstr_0)
                                                2)))
                                          (max
                                           app_0
                                           (+ bstr-end_0 new-len_0))))))
                                  (begin
                                    (let ((app_0 bstr_0))
                                      (unsafe-bytes-copy!
                                       bstr2_0
                                       0
                                       app_0
                                       0
                                       bstr-end_0))
                                    (set! bstr_0 bstr2_0)))
                                (void))
                              (let ((app_0 bstr_0))
                                (unsafe-bytes-copy!
                                 app_0
                                 bstr-end_0
                                 new-bstr_0))
                              (set! bstr-end_0 (+ bstr-end_0 new-len_0))
                              (if (< bstr-end_0 target-pos_0)
                                (decode-more!_0 target-pos_0)
                                (void)))))))))))))
           (make-input-port
            'string
            (lambda (dest-bstr_0)
              (let ((len_0 (fill!_0 dest-bstr_0 0)))
                (begin
                  (if (eof-object? len_0)
                    (void)
                    (set! bstr-pos_0 (+ bstr-pos_0 len_0)))
                  len_0)))
            (lambda (dest-bstr_0 skip_0 evt_0) (fill!_0 dest-bstr_0 skip_0))
            void)))))))
(define byte-positions->byte-positions.1
  (|#%name|
   byte-positions->byte-positions
   (lambda (delta1_0 ms-pos3_0 me-pos4_0 state5_0)
     (begin
       (if (not state5_0)
         (list
          (let ((app_0 (+ ms-pos3_0 delta1_0)))
            (cons app_0 (+ me-pos4_0 delta1_0))))
         (if (zero? delta1_0)
           (let ((app_0 (cons ms-pos3_0 me-pos4_0)))
             (cons app_0 (vector->list state5_0)))
           (let ((app_0
                  (let ((app_0 (+ ms-pos3_0 delta1_0)))
                    (cons app_0 (+ me-pos4_0 delta1_0)))))
             (cons
              app_0
              (reverse$1
               (call-with-values
                (lambda ()
                  (begin
                    (check-vector state5_0)
                    (values state5_0 (unsafe-vector-length state5_0))))
                (case-lambda
                 ((vec_0 len_0)
                  (begin
                    #f
                    (letrec*
                     ((for-loop_0
                       (|#%name|
                        for-loop
                        (lambda (fold-var_0 pos_0)
                          (begin
                            (if (unsafe-fx< pos_0 len_0)
                              (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                                (let ((fold-var_1
                                       (let ((fold-var_1
                                              (cons
                                               (if p_0
                                                 (let ((app_1
                                                        (+
                                                         (car p_0)
                                                         delta1_0)))
                                                   (cons
                                                    app_1
                                                    (+ (cdr p_0) delta1_0)))
                                                 #f)
                                               fold-var_0)))
                                         (values fold-var_1))))
                                  (for-loop_0
                                   fold-var_1
                                   (unsafe-fx+ 1 pos_0))))
                              fold-var_0))))))
                     (for-loop_0 null 0))))
                 (args (raise-binding-result-arity-error 2 args)))))))))))))
(define byte-positions->bytess.1
  (|#%name|
   byte-positions->bytess
   (lambda (delta7_0 in9_0 ms-pos10_0 me-pos11_0 state12_0)
     (begin
       (let ((app_0
              (let ((app_0 (- ms-pos10_0 delta7_0)))
                (subbytes in9_0 app_0 (- me-pos11_0 delta7_0)))))
         (cons
          app_0
          (if state12_0
            (reverse$1
             (call-with-values
              (lambda ()
                (begin
                  (check-vector state12_0)
                  (values state12_0 (unsafe-vector-length state12_0))))
              (case-lambda
               ((vec_0 len_0)
                (begin
                  #f
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (fold-var_0 pos_0)
                        (begin
                          (if (unsafe-fx< pos_0 len_0)
                            (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                              (let ((fold-var_1
                                     (let ((fold-var_1
                                            (cons
                                             (if p_0
                                               (let ((app_1
                                                      (- (car p_0) delta7_0)))
                                                 (subbytes
                                                  in9_0
                                                  app_1
                                                  (- (cdr p_0) delta7_0)))
                                               #f)
                                             fold-var_0)))
                                       (values fold-var_1))))
                                (for-loop_0 fold-var_1 (unsafe-fx+ 1 pos_0))))
                            fold-var_0))))))
                   (for-loop_0 null 0))))
               (args (raise-binding-result-arity-error 2 args)))))
            null)))))))
(define byte-positions->string-positions.1
  (|#%name|
   byte-positions->string-positions
   (lambda (delta15_0
            result-offset16_0
            start-index14_0
            bstr-in20_0
            ms-pos21_0
            me-pos22_0
            state23_0)
     (begin
       (let ((string-offset_0
              (|#%name|
               string-offset
               (lambda (pos_0)
                 (begin
                   (let ((end-index_0 (- pos_0 delta15_0)))
                     (if (< end-index_0 start-index14_0)
                       (-
                        result-offset16_0
                        (bytes-utf-8-length
                         bstr-in20_0
                         '#\x3f
                         end-index_0
                         start-index14_0))
                       (+
                        result-offset16_0
                        (bytes-utf-8-length
                         bstr-in20_0
                         '#\x3f
                         start-index14_0
                         end-index_0)))))))))
         (let ((app_0
                (let ((app_0 (string-offset_0 ms-pos21_0)))
                  (cons app_0 (string-offset_0 me-pos22_0)))))
           (cons
            app_0
            (if state23_0
              (reverse$1
               (call-with-values
                (lambda ()
                  (begin
                    (check-vector state23_0)
                    (values state23_0 (unsafe-vector-length state23_0))))
                (case-lambda
                 ((vec_0 len_0)
                  (begin
                    #f
                    (letrec*
                     ((for-loop_0
                       (|#%name|
                        for-loop
                        (lambda (fold-var_0 pos_0)
                          (begin
                            (if (unsafe-fx< pos_0 len_0)
                              (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                                (let ((fold-var_1
                                       (let ((fold-var_1
                                              (cons
                                               (if p_0
                                                 (let ((app_1
                                                        (string-offset_0
                                                         (car p_0))))
                                                   (cons
                                                    app_1
                                                    (string-offset_0
                                                     (cdr p_0))))
                                                 #f)
                                               fold-var_0)))
                                         (values fold-var_1))))
                                  (for-loop_0
                                   fold-var_1
                                   (unsafe-fx+ 1 pos_0))))
                              fold-var_0))))))
                     (for-loop_0 null 0))))
                 (args (raise-binding-result-arity-error 2 args)))))
              null))))))))
(define byte-positions->strings.1
  (|#%name|
   byte-positions->strings
   (lambda (delta25_0 bstr-in27_0 ms-pos28_0 me-pos29_0 state30_0)
     (begin
       (let ((app_0
              (let ((app_0 (- ms-pos28_0 delta25_0)))
                (bytes->string/utf-8
                 bstr-in27_0
                 '#\x3f
                 app_0
                 (- me-pos29_0 delta25_0)))))
         (cons
          app_0
          (if state30_0
            (reverse$1
             (call-with-values
              (lambda ()
                (begin
                  (check-vector state30_0)
                  (values state30_0 (unsafe-vector-length state30_0))))
              (case-lambda
               ((vec_0 len_0)
                (begin
                  #f
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (fold-var_0 pos_0)
                        (begin
                          (if (unsafe-fx< pos_0 len_0)
                            (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                              (let ((fold-var_1
                                     (let ((fold-var_1
                                            (cons
                                             (if p_0
                                               (let ((app_1
                                                      (- (car p_0) delta25_0)))
                                                 (bytes->string/utf-8
                                                  bstr-in27_0
                                                  '#\x3f
                                                  app_1
                                                  (- (cdr p_0) delta25_0)))
                                               #f)
                                             fold-var_0)))
                                       (values fold-var_1))))
                                (for-loop_0 fold-var_1 (unsafe-fx+ 1 pos_0))))
                            fold-var_0))))))
                   (for-loop_0 null 0))))
               (args (raise-binding-result-arity-error 2 args)))))
            null)))))))
(define byte-index->string-index
  (lambda (str_0 start-pos_0 pos_0)
    (letrec*
     ((loop_0
       (|#%name|
        loop
        (lambda (lo-pos_0 lo_0 hi_0)
          (begin
            (if (= lo_0 hi_0)
              lo_0
              (if (= (add1 lo_0) hi_0)
                (if (= lo-pos_0 pos_0) lo_0 hi_0)
                (let ((mid_0 (quotient (+ lo_0 hi_0) 2)))
                  (let ((len_0
                         (let ((app_0 (+ start-pos_0 lo_0)))
                           (string-utf-8-length
                            str_0
                            app_0
                            (+ start-pos_0 mid_0)))))
                    (let ((mid-pos_0 (+ lo-pos_0 len_0)))
                      (if (= mid-pos_0 pos_0)
                        mid_0
                        (if (> mid-pos_0 pos_0)
                          (loop_0 lo-pos_0 lo_0 mid_0)
                          (loop_0 (+ lo-pos_0 len_0) mid_0 hi_0)))))))))))))
     (loop_0
      0
      0
      (let ((app_0 (- (string-length str_0) start-pos_0)))
        (min app_0 (* pos_0 6)))))))
(define add-end-bytes
  (lambda (results_0 end-bytes-count_0 bstr_0 me-pos_0)
    (if end-bytes-count_0
      (values
       results_0
       (if results_0
         (subbytes bstr_0 (max 0 (- me-pos_0 end-bytes-count_0)) me-pos_0)
         #f))
      results_0)))
(define interp
  (lambda (m_0 s_0 pos_0 start_0 limit/end_0 state_0)
    (|#%app| m_0 s_0 pos_0 start_0 limit/end_0 limit/end_0 state_0 null)))
(define search-match
  (lambda (rx_0 in_0 pos_0 start-pos_0 end-pos_0 state_0)
    (let ((must-string_0 (rx:regexp-must-string rx_0)))
      (if (not (check-must-string must-string_0 in_0 pos_0 end-pos_0))
        (values #f #f)
        (let ((matcher_0 (rx:regexp-matcher rx_0)))
          (let ((anchored?_0 (rx:regexp-anchored? rx_0)))
            (let ((start-range_0 (rx:regexp-start-range rx_0)))
              (letrec*
               ((loop_0
                 (|#%name|
                  loop
                  (lambda (pos_1)
                    (begin
                      (if (if anchored?_0 (not (= pos_1 start-pos_0)) #f)
                        (values #f #f)
                        (if (if start-range_0
                              (if (bytes? in_0)
                                (= pos_1 end-pos_0)
                                (not
                                 (lazy-bytes-before-end?
                                  in_0
                                  pos_1
                                  end-pos_0)))
                              #f)
                          (values #f #f)
                          (if (if start-range_0
                                (not
                                 (check-start-range
                                  start-range_0
                                  in_0
                                  pos_1
                                  end-pos_0))
                                #f)
                            (loop_0 (add1 pos_1))
                            (let ((pos2_0
                                   (begin-unsafe
                                    (|#%app|
                                     matcher_0
                                     in_0
                                     pos_1
                                     start-pos_0
                                     end-pos_0
                                     end-pos_0
                                     state_0
                                     null))))
                              (if pos2_0
                                (values pos_1 pos2_0)
                                (if start-range_0
                                  (loop_0 (add1 pos_1))
                                  (if (if (bytes? in_0)
                                        (< pos_1 end-pos_0)
                                        (lazy-bytes-before-end?
                                         in_0
                                         pos_1
                                         end-pos_0))
                                    (let ((pos2_1 (add1 pos_1)))
                                      (begin
                                        (if (bytes? in_0)
                                          (void)
                                          (lazy-bytes-advance! in_0 pos2_1 #f))
                                        (loop_0 pos2_1)))
                                    (values #f #f)))))))))))))
               (loop_0 pos_0)))))))))
(define check-must-string
  (lambda (must-string_0 in_0 pos_0 end-pos_0)
    (if (not must-string_0)
      #t
      (if (not (bytes? in_0))
        #t
        (if (bytes? must-string_0)
          (if (= 1 (unsafe-bytes-length must-string_0))
            (let ((mc_0 (unsafe-bytes-ref must-string_0 0)))
              (call-with-values
               (lambda ()
                 (unsafe-normalise-inputs
                  unsafe-bytes-length
                  in_0
                  pos_0
                  end-pos_0
                  1))
               (case-lambda
                ((v*_0 start*_0 stop*_0 step*_0)
                 (begin
                   #t
                   (letrec*
                    ((for-loop_0
                      (|#%name|
                       for-loop
                       (lambda (result_0 idx_0)
                         (begin
                           (if (unsafe-fx< idx_0 stop*_0)
                             (let ((c_0 (unsafe-bytes-ref v*_0 idx_0)))
                               (let ((result_1
                                      (let ((result_1 (= c_0 mc_0)))
                                        (values result_1))))
                                 (if (if (not
                                          (let ((x_0 (list c_0))) result_1))
                                       #t
                                       #f)
                                   (for-loop_0 result_1 (unsafe-fx+ idx_0 1))
                                   result_1)))
                             result_0))))))
                    (for-loop_0 #f start*_0))))
                (args (raise-binding-result-arity-error 4 args)))))
            (let ((mc1_0 (unsafe-bytes-ref must-string_0 0)))
              (let ((end_0
                     (- end-pos_0 (sub1 (unsafe-bytes-length must-string_0)))))
                (begin
                  (letrec*
                   ((for-loop_0
                     (|#%name|
                      for-loop
                      (lambda (result_0 pos_1)
                        (begin
                          (if (< pos_1 end_0)
                            (let ((result_1
                                   (let ((result_1
                                          (if (=
                                               mc1_0
                                               (unsafe-bytes-ref in_0 pos_1))
                                            (call-with-values
                                             (lambda ()
                                               (unsafe-normalise-inputs
                                                unsafe-bytes-length
                                                in_0
                                                (add1 pos_1)
                                                #f
                                                1))
                                             (case-lambda
                                              ((v*_0 start*_0 stop*_0 step*_0)
                                               (call-with-values
                                                (lambda ()
                                                  (unsafe-normalise-inputs
                                                   unsafe-bytes-length
                                                   must-string_0
                                                   1
                                                   #f
                                                   1))
                                                (case-lambda
                                                 ((v*_1
                                                   start*_1
                                                   stop*_1
                                                   step*_1)
                                                  (let ((v*_2 v*_0)
                                                        (start*_2 start*_0)
                                                        (stop*_2 stop*_0)
                                                        (step*_2 step*_0))
                                                    (begin
                                                      #t
                                                      #t
                                                      (letrec*
                                                       ((for-loop_1
                                                         (|#%name|
                                                          for-loop
                                                          (lambda (result_1
                                                                   idx_0
                                                                   idx_1)
                                                            (begin
                                                              (if (if (unsafe-fx<
                                                                       idx_0
                                                                       stop*_2)
                                                                    (unsafe-fx<
                                                                     idx_1
                                                                     stop*_1)
                                                                    #f)
                                                                (let ((c_0
                                                                       (unsafe-bytes-ref
                                                                        v*_2
                                                                        idx_0)))
                                                                  (let ((mc_0
                                                                         (unsafe-bytes-ref
                                                                          v*_1
                                                                          idx_1)))
                                                                    (let ((c_1
                                                                           c_0))
                                                                      (let ((result_2
                                                                             (let ((result_2
                                                                                    (=
                                                                                     c_1
                                                                                     mc_0)))
                                                                               (values
                                                                                result_2))))
                                                                        (if (if (not
                                                                                 (let ((x_0
                                                                                        (list
                                                                                         c_1)))
                                                                                   (not
                                                                                    result_2)))
                                                                              (if (not
                                                                                   (let ((x_0
                                                                                          (list
                                                                                           mc_0)))
                                                                                     (not
                                                                                      result_2)))
                                                                                #t
                                                                                #f)
                                                                              #f)
                                                                          (for-loop_1
                                                                           result_2
                                                                           (unsafe-fx+
                                                                            idx_0
                                                                            1)
                                                                           (unsafe-fx+
                                                                            idx_1
                                                                            1))
                                                                          result_2)))))
                                                                result_1))))))
                                                       (for-loop_1
                                                        #t
                                                        start*_2
                                                        start*_1)))))
                                                 (args
                                                  (raise-binding-result-arity-error
                                                   4
                                                   args)))))
                                              (args
                                               (raise-binding-result-arity-error
                                                4
                                                args))))
                                            #f)))
                                     (values result_1))))
                              (if (if (not (let ((x_0 (list pos_1))) result_1))
                                    #t
                                    #f)
                                (for-loop_0 result_1 (+ pos_1 1))
                                result_1))
                            result_0))))))
                   (for-loop_0 #f pos_0))))))
          (let ((end_0 (- end-pos_0 (sub1 (length must-string_0)))))
            (begin
              (letrec*
               ((for-loop_0
                 (|#%name|
                  for-loop
                  (lambda (result_0 pos_1)
                    (begin
                      (if (< pos_1 end_0)
                        (let ((result_1
                               (let ((result_1
                                      (letrec*
                                       ((loop_0
                                         (|#%name|
                                          loop
                                          (lambda (i_0 l_0)
                                            (begin
                                              (if (null? l_0)
                                                #t
                                                (let ((e_0 (car l_0)))
                                                  (if (let ((v_0
                                                             (unsafe-bytes-ref
                                                              in_0
                                                              i_0)))
                                                        (begin-unsafe
                                                         (eq?
                                                          1
                                                          (unsafe-bytes-ref
                                                           e_0
                                                           v_0))))
                                                    (let ((app_0 (add1 i_0)))
                                                      (loop_0 app_0 (cdr l_0)))
                                                    #f))))))))
                                       (loop_0 pos_1 must-string_0))))
                                 (values result_1))))
                          (if (if (not (let ((x_0 (list pos_1))) result_1))
                                #t
                                #f)
                            (for-loop_0 result_1 (+ pos_1 1))
                            result_1))
                        result_0))))))
               (for-loop_0 #f pos_0)))))))))
(define check-start-range
  (lambda (start-range_0 in_0 pos_0 end-pos_0)
    (let ((v_0
           (if (bytes? in_0)
             (unsafe-bytes-ref in_0 pos_0)
             (lazy-bytes-ref in_0 pos_0))))
      (begin-unsafe (eq? 1 (unsafe-bytes-ref start-range_0 v_0))))))
(define FAST-STRING-LEN 64)
(define fast-drive-regexp-match?/bytes
  (lambda (rx_0 in_0 start-pos_0 end-pos_0)
    (let ((state_0
           (if (rx:regexp-references? rx_0)
             (make-vector (rx:regexp-num-groups rx_0) #f)
             #f)))
      (call-with-values
       (lambda ()
         (search-match
          rx_0
          in_0
          start-pos_0
          start-pos_0
          (if end-pos_0 end-pos_0 (unsafe-bytes-length in_0))
          state_0))
       (case-lambda
        ((ms-pos_0 me-pos_0) (if ms-pos_0 #t #f))
        (args (raise-binding-result-arity-error 2 args)))))))
(define fast-drive-regexp-match?/string
  (lambda (rx_0 in-str_0 start-offset_0 end-offset_0)
    (let ((state_0
           (if (rx:regexp-references? rx_0)
             (make-vector (rx:regexp-num-groups rx_0) #f)
             #f)))
      (let ((in_0
             (string->bytes/utf-8
              in-str_0
              0
              start-offset_0
              (if end-offset_0 end-offset_0 (string-length in-str_0)))))
        (call-with-values
         (lambda ()
           (search-match rx_0 in_0 0 0 (unsafe-bytes-length in_0) state_0))
         (case-lambda
          ((ms-pos_0 me-pos_0) (if ms-pos_0 #t #f))
          (args (raise-binding-result-arity-error 2 args))))))))
(define fast-drive-regexp-match-positions/bytes
  (lambda (rx_0 in_0 start-pos_0 end-pos_0)
    (let ((state_0
           (let ((n_0 (rx:regexp-num-groups rx_0)))
             (if (positive? n_0) (make-vector n_0 #f) #f))))
      (call-with-values
       (lambda ()
         (search-match
          rx_0
          in_0
          start-pos_0
          start-pos_0
          (if end-pos_0 end-pos_0 (unsafe-bytes-length in_0))
          state_0))
       (case-lambda
        ((ms-pos_0 me-pos_0)
         (if ms-pos_0
           (if state_0
             (let ((app_0 (cons ms-pos_0 me-pos_0)))
               (cons app_0 (vector->list state_0)))
             (list (cons ms-pos_0 me-pos_0)))
           #f))
        (args (raise-binding-result-arity-error 2 args)))))))
(define fast-drive-regexp-match-positions/string
  (lambda (rx_0 in-str_0 start-offset_0 end-offset_0)
    (let ((in_0
           (string->bytes/utf-8
            in-str_0
            0
            start-offset_0
            (if end-offset_0 end-offset_0 (string-length in-str_0)))))
      (let ((state_0
             (let ((n_0 (rx:regexp-num-groups rx_0)))
               (if (positive? n_0) (make-vector n_0 #f) #f))))
        (call-with-values
         (lambda ()
           (search-match rx_0 in_0 0 0 (unsafe-bytes-length in_0) state_0))
         (case-lambda
          ((ms-pos_0 me-pos_0)
           (let ((string-offset_0
                  (|#%name|
                   string-offset
                   (lambda (pos_0)
                     (begin
                       (+
                        start-offset_0
                        (bytes-utf-8-length in_0 '#\x3f 0 pos_0)))))))
             (if ms-pos_0
               (let ((app_0
                      (let ((app_0 (string-offset_0 ms-pos_0)))
                        (cons app_0 (string-offset_0 me-pos_0)))))
                 (cons
                  app_0
                  (if state_0
                    (reverse$1
                     (call-with-values
                      (lambda ()
                        (begin
                          (check-vector state_0)
                          (values state_0 (unsafe-vector-length state_0))))
                      (case-lambda
                       ((vec_0 len_0)
                        (begin
                          #f
                          (letrec*
                           ((for-loop_0
                             (|#%name|
                              for-loop
                              (lambda (fold-var_0 pos_0)
                                (begin
                                  (if (unsafe-fx< pos_0 len_0)
                                    (let ((p_0
                                           (unsafe-vector-ref vec_0 pos_0)))
                                      (let ((fold-var_1
                                             (let ((fold-var_1
                                                    (cons
                                                     (if p_0
                                                       (let ((app_1
                                                              (string-offset_0
                                                               (car p_0))))
                                                         (cons
                                                          app_1
                                                          (string-offset_0
                                                           (cdr p_0))))
                                                       #f)
                                                     fold-var_0)))
                                               (values fold-var_1))))
                                        (for-loop_0
                                         fold-var_1
                                         (unsafe-fx+ 1 pos_0))))
                                    fold-var_0))))))
                           (for-loop_0 null 0))))
                       (args (raise-binding-result-arity-error 2 args)))))
                    null)))
               #f)))
          (args (raise-binding-result-arity-error 2 args))))))))
(define fast-drive-regexp-match/bytes
  (lambda (rx_0 in_0 start-pos_0 end-pos_0)
    (let ((state_0
           (let ((n_0 (rx:regexp-num-groups rx_0)))
             (if (positive? n_0) (make-vector n_0 #f) #f))))
      (call-with-values
       (lambda ()
         (search-match
          rx_0
          in_0
          start-pos_0
          start-pos_0
          (if end-pos_0 end-pos_0 (unsafe-bytes-length in_0))
          state_0))
       (case-lambda
        ((ms-pos_0 me-pos_0)
         (if ms-pos_0
           (let ((app_0 (subbytes in_0 ms-pos_0 me-pos_0)))
             (cons
              app_0
              (if state_0
                (reverse$1
                 (call-with-values
                  (lambda ()
                    (begin
                      (check-vector state_0)
                      (values state_0 (unsafe-vector-length state_0))))
                  (case-lambda
                   ((vec_0 len_0)
                    (begin
                      #f
                      (letrec*
                       ((for-loop_0
                         (|#%name|
                          for-loop
                          (lambda (fold-var_0 pos_0)
                            (begin
                              (if (unsafe-fx< pos_0 len_0)
                                (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                                  (let ((fold-var_1
                                         (let ((fold-var_1
                                                (cons
                                                 (if p_0
                                                   (let ((app_1 (car p_0)))
                                                     (subbytes
                                                      in_0
                                                      app_1
                                                      (cdr p_0)))
                                                   #f)
                                                 fold-var_0)))
                                           (values fold-var_1))))
                                    (for-loop_0
                                     fold-var_1
                                     (unsafe-fx+ 1 pos_0))))
                                fold-var_0))))))
                       (for-loop_0 null 0))))
                   (args (raise-binding-result-arity-error 2 args)))))
                null)))
           #f))
        (args (raise-binding-result-arity-error 2 args)))))))
(define fast-drive-regexp-match/string
  (lambda (rx_0 in-str_0 start-offset_0 end-offset_0)
    (let ((in_0
           (string->bytes/utf-8
            in-str_0
            0
            start-offset_0
            (if end-offset_0 end-offset_0 (string-length in-str_0)))))
      (let ((state_0
             (let ((n_0 (rx:regexp-num-groups rx_0)))
               (if (positive? n_0) (make-vector n_0 #f) #f))))
        (call-with-values
         (lambda ()
           (search-match rx_0 in_0 0 0 (unsafe-bytes-length in_0) state_0))
         (case-lambda
          ((ms-pos_0 me-pos_0)
           (if ms-pos_0
             (let ((app_0 (bytes->string/utf-8 in_0 '#\x3f ms-pos_0 me-pos_0)))
               (cons
                app_0
                (if state_0
                  (reverse$1
                   (call-with-values
                    (lambda ()
                      (begin
                        (check-vector state_0)
                        (values state_0 (unsafe-vector-length state_0))))
                    (case-lambda
                     ((vec_0 len_0)
                      (begin
                        #f
                        (letrec*
                         ((for-loop_0
                           (|#%name|
                            for-loop
                            (lambda (fold-var_0 pos_0)
                              (begin
                                (if (unsafe-fx< pos_0 len_0)
                                  (let ((p_0 (unsafe-vector-ref vec_0 pos_0)))
                                    (let ((fold-var_1
                                           (let ((fold-var_1
                                                  (cons
                                                   (if p_0
                                                     (let ((app_1 (car p_0)))
                                                       (bytes->string/utf-8
                                                        in_0
                                                        '#\x3f
                                                        app_1
                                                        (cdr p_0)))
                                                     #f)
                                                   fold-var_0)))
                                             (values fold-var_1))))
                                      (for-loop_0
                                       fold-var_1
                                       (unsafe-fx+ 1 pos_0))))
                                  fold-var_0))))))
                         (for-loop_0 null 0))))
                     (args (raise-binding-result-arity-error 2 args)))))
                  null)))
             #f))
          (args (raise-binding-result-arity-error 2 args))))))))
(define drive-regexp-match.1
  (|#%name|
   drive-regexp-match
   (lambda (end-bytes-count9_0
            end-bytes?8_0
            immediate-only?6_0
            in-path-ok?4_0
            in-port-ok?3_0
            mode2_0
            peek?5_0
            progress-evt7_0
            search-offset1_0
            who19_0
            orig-rx20_0
            orig-in21_0
            orig-start-offset22_0
            orig-end-offset23_0
            out24_0
            prefix25_0)
     (begin
       (let ((search-offset_0
              (if (eq? search-offset1_0 unsafe-undefined)
                orig-start-offset22_0
                search-offset1_0)))
         (let ((rx_0
                (if (rx:regexp? orig-rx20_0)
                  orig-rx20_0
                  (if (string? orig-rx20_0)
                    (make-regexp who19_0 orig-rx20_0 #f #f #f)
                    (if (bytes? orig-rx20_0)
                      (make-regexp who19_0 orig-rx20_0 #f #t #f)
                      (raise-argument-error
                       who19_0
                       "(or/c regexp? byte-regexp? string? bytes?)"
                       orig-rx20_0))))))
           (let ((in_0
                  (if (if in-path-ok?4_0 (path? orig-in21_0) #f)
                    (if (rx:regexp-bytes? rx_0)
                      (path->bytes orig-in21_0)
                      (path->string orig-in21_0))
                    orig-in21_0)))
             (begin
               (if (let ((or-part_0 (if (bytes? in_0) (not peek?5_0) #f)))
                     (if or-part_0
                       or-part_0
                       (let ((or-part_1 (if (string? in_0) (not peek?5_0) #f)))
                         (if or-part_1
                           or-part_1
                           (if in-port-ok?3_0 (input-port? in_0) #f)))))
                 (void)
                 (raise-argument-error
                  who19_0
                  (if peek?5_0
                    "input-port?"
                    (if in-port-ok?3_0
                      "(or/c bytes? string? input-port? path?)"
                      (if in-path-ok?4_0
                        "(or/c bytes? string? path?)"
                        "(or/c bytes? string?)")))
                  orig-in21_0))
               (let ((start-offset_0
                      (if orig-start-offset22_0
                        (begin
                          (if (exact-nonnegative-integer?
                               orig-start-offset22_0)
                            (void)
                            (raise-argument-error
                             who19_0
                             "exact-nonnegative-integer?"
                             orig-start-offset22_0))
                          (check-range
                           who19_0
                           "starting index"
                           in_0
                           orig-start-offset22_0
                           0)
                          orig-start-offset22_0)
                        0)))
                 (let ((end-offset_0
                        (if orig-end-offset23_0
                          (begin
                            (if (exact-nonnegative-integer?
                                 orig-end-offset23_0)
                              (void)
                              (raise-argument-error
                               who19_0
                               "(or/c #f exact-nonnegative-integer?)"
                               orig-end-offset23_0))
                            (check-range
                             who19_0
                             "ending index"
                             in_0
                             orig-end-offset23_0
                             start-offset_0)
                            orig-end-offset23_0)
                          (if (bytes? in_0)
                            (unsafe-bytes-length in_0)
                            (if (string? in_0) (string-length in_0) 'eof)))))
                   (begin
                     (if (let ((or-part_0 (not out24_0)))
                           (if or-part_0 or-part_0 (output-port? out24_0)))
                       (void)
                       (raise-argument-error
                        who19_0
                        "(or/c #f output-port?)"
                        out24_0))
                     (begin
                       (if (bytes? prefix25_0)
                         (void)
                         (raise-argument-error who19_0 "bytes?" prefix25_0))
                       (begin
                         (if end-bytes?8_0
                           (if (exact-nonnegative-integer? end-bytes-count9_0)
                             (void)
                             (raise-argument-error
                              who19_0
                              "exact-nonnegative-integer?"
                              end-bytes-count9_0))
                           (void))
                         (let ((state_0
                                (if (let ((or-part_0 (not (eq? mode2_0 '?))))
                                      (if or-part_0
                                        or-part_0
                                        (rx:regexp-references? rx_0)))
                                  (let ((n_0 (rx:regexp-num-groups rx_0)))
                                    (if (positive? n_0)
                                      (make-vector n_0 #f)
                                      #f))
                                  #f)))
                           (if (if (bytes? in_0)
                                 (if (not out24_0) (equal? #vu8() prefix25_0) #f)
                                 #f)
                             (call-with-values
                              (lambda ()
                                (search-match
                                 rx_0
                                 in_0
                                 search-offset_0
                                 start-offset_0
                                 end-offset_0
                                 state_0))
                              (case-lambda
                               ((ms-pos_0 me-pos_0)
                                (begin
                                  (if out24_0
                                    (write-bytes
                                     in_0
                                     out24_0
                                     0
                                     (if ms-pos_0 ms-pos_0 end-offset_0))
                                    (void))
                                  (let ((tmp_0 (if ms-pos_0 mode2_0 #f)))
                                    (if (eq? tmp_0 #f)
                                      (add-end-bytes
                                       #f
                                       end-bytes-count9_0
                                       #f
                                       #f)
                                      (if (eq? tmp_0 '?)
                                        #t
                                        (if (eq? tmp_0 'positions)
                                          (let ((positions_0
                                                 (byte-positions->byte-positions.1
                                                  0
                                                  ms-pos_0
                                                  me-pos_0
                                                  state_0)))
                                            (add-end-bytes
                                             positions_0
                                             end-bytes-count9_0
                                             in_0
                                             me-pos_0))
                                          (if (eq? tmp_0 'strings)
                                            (let ((bytess_0
                                                   (byte-positions->bytess.1
                                                    0
                                                    in_0
                                                    ms-pos_0
                                                    me-pos_0
                                                    state_0)))
                                              (add-end-bytes
                                               bytess_0
                                               end-bytes-count9_0
                                               in_0
                                               me-pos_0))
                                            (void))))))))
                               (args
                                (raise-binding-result-arity-error 2 args))))
                             (if (if (string? in_0)
                                   (if (not out24_0)
                                     (if (equal? #vu8() prefix25_0)
                                       (< (- end-offset_0 start-offset_0) 64)
                                       #f)
                                     #f)
                                   #f)
                               (let ((bstr-in_0
                                      (string->bytes/utf-8
                                       in_0
                                       0
                                       start-offset_0
                                       end-offset_0)))
                                 (let ((search-pos_0
                                        (if (= start-offset_0 search-offset_0)
                                          0
                                          (string-utf-8-length
                                           in_0
                                           start-offset_0
                                           search-offset_0))))
                                   (let ((end-pos_0
                                          (unsafe-bytes-length bstr-in_0)))
                                     (call-with-values
                                      (lambda ()
                                        (search-match
                                         rx_0
                                         bstr-in_0
                                         search-pos_0
                                         0
                                         end-pos_0
                                         state_0))
                                      (case-lambda
                                       ((ms-pos_0 me-pos_0)
                                        (begin
                                          (if out24_0
                                            (begin
                                              (write-string
                                               in_0
                                               out24_0
                                               0
                                               start-offset_0)
                                              (write-bytes
                                               bstr-in_0
                                               out24_0
                                               0
                                               (if ms-pos_0
                                                 ms-pos_0
                                                 end-pos_0)))
                                            (void))
                                          (let ((tmp_0
                                                 (if ms-pos_0 mode2_0 #f)))
                                            (if (eq? tmp_0 #f)
                                              (add-end-bytes
                                               #f
                                               end-bytes-count9_0
                                               #f
                                               #f)
                                              (if (eq? tmp_0 '?)
                                                #t
                                                (if (eq? tmp_0 'positions)
                                                  (let ((positions_0
                                                         (if (rx:regexp-bytes?
                                                              rx_0)
                                                           (let ((delta_0
                                                                  (string-utf-8-length
                                                                   in_0
                                                                   0
                                                                   start-offset_0)))
                                                             (byte-positions->byte-positions.1
                                                              delta_0
                                                              ms-pos_0
                                                              me-pos_0
                                                              state_0))
                                                           (byte-positions->string-positions.1
                                                            0
                                                            start-offset_0
                                                            0
                                                            bstr-in_0
                                                            ms-pos_0
                                                            me-pos_0
                                                            state_0))))
                                                    (add-end-bytes
                                                     positions_0
                                                     end-bytes-count9_0
                                                     bstr-in_0
                                                     me-pos_0))
                                                  (if (eq? tmp_0 'strings)
                                                    (let ((bytes/strings_0
                                                           (if (rx:regexp-bytes?
                                                                rx_0)
                                                             (byte-positions->bytess.1
                                                              0
                                                              bstr-in_0
                                                              ms-pos_0
                                                              me-pos_0
                                                              state_0)
                                                             (byte-positions->strings.1
                                                              0
                                                              bstr-in_0
                                                              ms-pos_0
                                                              me-pos_0
                                                              state_0))))
                                                      (add-end-bytes
                                                       bytes/strings_0
                                                       end-bytes-count9_0
                                                       bstr-in_0
                                                       me-pos_0))
                                                    (void))))))))
                                       (args
                                        (raise-binding-result-arity-error
                                         2
                                         args)))))))
                               (let ((prefix-len_0
                                      (unsafe-bytes-length prefix25_0)))
                                 (let ((search-pos_0
                                        (if (= start-offset_0 search-offset_0)
                                          prefix-len_0
                                          (+
                                           prefix-len_0
                                           (if (string? in_0)
                                             (string-utf-8-length
                                              in_0
                                              start-offset_0
                                              search-offset_0)
                                             (-
                                              search-offset_0
                                              start-offset_0))))))
                                   (let ((port-in_0
                                          (if (bytes? in_0)
                                            (open-input-bytes/no-copy
                                             in_0
                                             start-offset_0
                                             end-offset_0)
                                            (if (string? in_0)
                                              (open-input-string/lazy
                                               in_0
                                               start-offset_0
                                               end-offset_0)
                                              in_0))))
                                     (let ((any-bytes-left?_0
                                            (if (if (input-port? in_0)
                                                  (positive? start-offset_0)
                                                  #f)
                                              (if peek?5_0
                                                (not
                                                 (eof-object?
                                                  (peek-byte
                                                   port-in_0
                                                   (sub1 start-offset_0))))
                                                (copy-port-bytes
                                                 port-in_0
                                                 #f
                                                 start-offset_0))
                                              #t)))
                                       (let ((skip-amt_0
                                              (if peek?5_0 start-offset_0 0)))
                                         (let ((lb-in_0
                                                (let ((max-lookbehind_0
                                                       (max
                                                        (rx:regexp-max-lookbehind
                                                         rx_0)
                                                        (if end-bytes-count9_0
                                                          end-bytes-count9_0
                                                          0))))
                                                  (let ((max-peek_0
                                                         (if (input-port? in_0)
                                                           (if (not
                                                                (eq?
                                                                 'eof
                                                                 end-offset_0))
                                                             (-
                                                              end-offset_0
                                                              start-offset_0)
                                                             #f)
                                                           #f)))
                                                    (let ((max-lookbehind_1
                                                           max-lookbehind_0)
                                                          (skip-amt_1
                                                           skip-amt_0))
                                                      (begin-unsafe
                                                       (let ((len_0
                                                              (unsafe-bytes-length
                                                               prefix25_0)))
                                                         (lazy-bytes1.1
                                                          prefix25_0
                                                          len_0
                                                          port-in_0
                                                          skip-amt_1
                                                          len_0
                                                          peek?5_0
                                                          immediate-only?6_0
                                                          progress-evt7_0
                                                          out24_0
                                                          max-lookbehind_1
                                                          #f
                                                          0
                                                          max-peek_0))))))))
                                           (let ((end-pos_0
                                                  (if (let ((or-part_0
                                                             (eq?
                                                              'eof
                                                              end-offset_0)))
                                                        (if or-part_0
                                                          or-part_0
                                                          (string? in_0)))
                                                    'eof
                                                    (+
                                                     prefix-len_0
                                                     (-
                                                      end-offset_0
                                                      start-offset_0)))))
                                             (call-with-values
                                              (lambda ()
                                                (if any-bytes-left?_0
                                                  (search-match
                                                   rx_0
                                                   lb-in_0
                                                   search-pos_0
                                                   0
                                                   end-pos_0
                                                   state_0)
                                                  (values #f #f)))
                                              (case-lambda
                                               ((ms-pos_0 me-pos_0)
                                                (let ((write/consume-skipped_0
                                                       (|#%name|
                                                        write/consume-skipped
                                                        (lambda ()
                                                          (begin
                                                            (if (not peek?5_0)
                                                              (if ms-pos_0
                                                                (begin
                                                                  (if out24_0
                                                                    (lazy-bytes-advance!
                                                                     lb-in_0
                                                                     ms-pos_0
                                                                     #t)
                                                                    (void))
                                                                  (if (input-port?
                                                                       in_0)
                                                                    (copy-port-bytes
                                                                     port-in_0
                                                                     #f
                                                                     (-
                                                                      me-pos_0
                                                                      prefix-len_0))
                                                                    (void)))
                                                                (if (eq?
                                                                     end-pos_0
                                                                     'eof)
                                                                  (if (if out24_0
                                                                        out24_0
                                                                        (input-port?
                                                                         in_0))
                                                                    (copy-port-bytes
                                                                     port-in_0
                                                                     out24_0
                                                                     #f)
                                                                    (void))
                                                                  (begin
                                                                    (if out24_0
                                                                      (lazy-bytes-advance!
                                                                       lb-in_0
                                                                       end-pos_0
                                                                       #t)
                                                                      (void))
                                                                    (if (input-port?
                                                                         in_0)
                                                                      (copy-port-bytes
                                                                       port-in_0
                                                                       #f
                                                                       (-
                                                                        end-pos_0
                                                                        prefix-len_0))
                                                                      (void)))))
                                                              (void)))))))
                                                  (begin0
                                                    (let ((tmp_0
                                                           (if ms-pos_0
                                                             (if (not
                                                                  (lazy-bytes-failed?
                                                                   lb-in_0))
                                                               mode2_0
                                                               #f)
                                                             #f)))
                                                      (if (eq? tmp_0 #f)
                                                        (add-end-bytes
                                                         #f
                                                         end-bytes-count9_0
                                                         #f
                                                         #f)
                                                        (if (eq? tmp_0 '?)
                                                          #t
                                                          (if (eq?
                                                               tmp_0
                                                               'positions)
                                                            (let ((bstr_0
                                                                   (lazy-bytes-bstr
                                                                    lb-in_0)))
                                                              (let ((positions_0
                                                                     (if (let ((or-part_0
                                                                                (not
                                                                                 (string?
                                                                                  in_0))))
                                                                           (if or-part_0
                                                                             or-part_0
                                                                             (rx:regexp-bytes?
                                                                              rx_0)))
                                                                       (let ((delta_0
                                                                              (-
                                                                               start-offset_0
                                                                               prefix-len_0)))
                                                                         (byte-positions->byte-positions.1
                                                                          delta_0
                                                                          ms-pos_0
                                                                          me-pos_0
                                                                          state_0))
                                                                       (let ((ms-str-pos_0
                                                                              (byte-index->string-index
                                                                               in_0
                                                                               start-offset_0
                                                                               (-
                                                                                ms-pos_0
                                                                                prefix-len_0))))
                                                                         (let ((delta_0
                                                                                (lazy-bytes-discarded-count
                                                                                 lb-in_0)))
                                                                           (let ((temp59_0
                                                                                  (-
                                                                                   ms-pos_0
                                                                                   delta_0)))
                                                                             (let ((temp61_0
                                                                                    (+
                                                                                     ms-str-pos_0
                                                                                     start-offset_0)))
                                                                               (let ((temp59_1
                                                                                      temp59_0))
                                                                                 (byte-positions->string-positions.1
                                                                                  delta_0
                                                                                  temp61_0
                                                                                  temp59_1
                                                                                  bstr_0
                                                                                  ms-pos_0
                                                                                  me-pos_0
                                                                                  state_0)))))))))
                                                                (add-end-bytes
                                                                 positions_0
                                                                 end-bytes-count9_0
                                                                 bstr_0
                                                                 (-
                                                                  me-pos_0
                                                                  (lazy-bytes-discarded-count
                                                                   lb-in_0)))))
                                                            (if (eq?
                                                                 tmp_0
                                                                 'strings)
                                                              (let ((bstr_0
                                                                     (lazy-bytes-bstr
                                                                      lb-in_0)))
                                                                (let ((delta_0
                                                                       (lazy-bytes-discarded-count
                                                                        lb-in_0)))
                                                                  (let ((bytes/strings_0
                                                                         (if (let ((or-part_0
                                                                                    (not
                                                                                     (string?
                                                                                      in_0))))
                                                                               (if or-part_0
                                                                                 or-part_0
                                                                                 (rx:regexp-bytes?
                                                                                  rx_0)))
                                                                           (byte-positions->bytess.1
                                                                            delta_0
                                                                            bstr_0
                                                                            ms-pos_0
                                                                            me-pos_0
                                                                            state_0)
                                                                           (byte-positions->strings.1
                                                                            delta_0
                                                                            bstr_0
                                                                            ms-pos_0
                                                                            me-pos_0
                                                                            state_0))))
                                                                    (add-end-bytes
                                                                     bytes/strings_0
                                                                     end-bytes-count9_0
                                                                     bstr_0
                                                                     (-
                                                                      me-pos_0
                                                                      delta_0)))))
                                                              (void))))))
                                                    (write/consume-skipped_0))))
                                               (args
                                                (raise-binding-result-arity-error
                                                 2
                                                 args)))))))))))))))))))))))))))
(define check-range
  (lambda (who_0 what_0 in_0 pos_0 start-pos_0)
    (let ((len_0
           (if (bytes? in_0)
             (unsafe-bytes-length in_0)
             (if (string? in_0) (string-length in_0) +inf.0))))
      (begin
        (if (>= pos_0 start-pos_0)
          (void)
          (raise-arguments-error
           who_0
           (format "~a is smaller than starting index" what_0)
           what_0
           pos_0
           "starting index"
           start-pos_0))
        (if (<= pos_0 len_0)
          (void)
          (raise-arguments-error
           who_0
           (format "~a is out of range" what_0)
           what_0
           pos_0))))))
(define chytes-ref
  (lambda (s_0 pos_0)
    (if (bytes? s_0)
      (unsafe-bytes-ref s_0 pos_0)
      (char->integer (string-ref s_0 pos_0)))))
(define subchytes
  (let ((subchytes_0
         (|#%name|
          subchytes
          (lambda (s2_0 a3_0 b1_0)
            (begin
              (if (bytes? s2_0)
                (subbytes s2_0 a3_0 (if b1_0 b1_0 (unsafe-bytes-length s2_0)))
                (substring s2_0 a3_0 (if b1_0 b1_0 (string-length s2_0)))))))))
    (case-lambda
     ((s_0 a_0) (subchytes_0 s_0 a_0 #f))
     ((s_0 a_0 b1_0) (subchytes_0 s_0 a_0 b1_0)))))
(define chytes-append
  (case-lambda
   ((a_0) a_0)
   ((a_0 b_0) (if (bytes? a_0) (bytes-append a_0 b_0) (string-append a_0 b_0)))
   ((a_0 b_0 c_0)
    (if (bytes? a_0) (bytes-append a_0 b_0 c_0) (string-append a_0 b_0 c_0)))
   ((a_0 . l_0)
    (if (bytes? a_0)
      (apply bytes-append a_0 l_0)
      (apply string-append a_0 l_0)))))
(define chytes?
  (lambda (ex_0 v_0) (if (bytes? ex_0) (bytes? v_0) (string? v_0))))
(define chytes-length
  (lambda (s_0)
    (if (bytes? s_0) (unsafe-bytes-length s_0) (string-length s_0))))
(define 1/regexp-replace
  (let ((regexp-replace_0
         (|#%name|
          regexp-replace
          (lambda (rx2_0 orig-in3_0 insert4_0 prefix1_0)
            (begin
              (do-regexp-replace
               'regexp-replace
               rx2_0
               orig-in3_0
               insert4_0
               prefix1_0
               #f))))))
    (|#%name|
     regexp-replace
     (case-lambda
      ((rx_0 orig-in_0 insert_0)
       (begin (regexp-replace_0 rx_0 orig-in_0 insert_0 #vu8())))
      ((rx_0 orig-in_0 insert_0 prefix1_0)
       (regexp-replace_0 rx_0 orig-in_0 insert_0 prefix1_0))))))
(define 1/regexp-replace*
  (let ((regexp-replace*_0
         (|#%name|
          regexp-replace*
          (lambda (rx6_0 orig-in7_0 insert8_0 prefix5_0)
            (begin
              (do-regexp-replace
               'regexp-replace*
               rx6_0
               orig-in7_0
               insert8_0
               prefix5_0
               #t))))))
    (|#%name|
     regexp-replace*
     (case-lambda
      ((rx_0 orig-in_0 insert_0)
       (begin (regexp-replace*_0 rx_0 orig-in_0 insert_0 #vu8())))
      ((rx_0 orig-in_0 insert_0 prefix5_0)
       (regexp-replace*_0 rx_0 orig-in_0 insert_0 prefix5_0))))))
(define do-regexp-replace
  (lambda (who_0 rx-in_0 orig-in_0 insert_0 prefix_0 all?_0)
    (let ((string-mode?_0
           (if (let ((or-part_0 (string? rx-in_0)))
                 (if or-part_0 or-part_0 (1/regexp? rx-in_0)))
             (string? orig-in_0)
             #f)))
      (let ((in_0
             (if (if (not string-mode?_0) (string? orig-in_0) #f)
               (string->bytes/utf-8 orig-in_0)
               orig-in_0)))
        (begin
          (if (if string-mode?_0
                string-mode?_0
                (if (let ((or-part_0 (bytes? rx-in_0)))
                      (if or-part_0 or-part_0 (1/byte-regexp? rx-in_0)))
                  (let ((or-part_0 (string? orig-in_0)))
                    (if or-part_0 or-part_0 (bytes? orig-in_0)))
                  #f))
            (if (let ((or-part_0 (string? insert_0)))
                  (if or-part_0
                    or-part_0
                    (let ((or-part_1 (bytes? insert_0)))
                      (if or-part_1 or-part_1 (procedure? insert_0)))))
              (void)
              (raise-argument-error
               who_0
               "(or/c string? bytes? procedure?)"
               insert_0))
            (void))
          (begin
            (if string-mode?_0
              (if (bytes? insert_0)
                (raise-arguments-error
                 who_0
                 "cannot replace a string with a byte string"
                 "byte string"
                 insert_0)
                (void))
              (void))
            (let ((rx_0
                   (if (string? rx-in_0)
                     (make-regexp who_0 rx-in_0 #f #f #f)
                     (if (bytes? rx-in_0)
                       (make-regexp who_0 rx-in_0 #f #t #f)
                       rx-in_0))))
              (let ((ins_0
                     (if (if (not string-mode?_0) (string? insert_0) #f)
                       (string->bytes/utf-8 insert_0)
                       insert_0)))
                (let ((need-lookbehind_0 (rx:regexp-max-lookbehind rx_0)))
                  (letrec*
                   ((loop_0
                     (|#%name|
                      loop
                      (lambda (search-pos_0 get-list?_0)
                        (begin
                          (let ((use-prefix_0
                                 (if (< search-pos_0 need-lookbehind_0)
                                   prefix_0
                                   #f)))
                            (let ((in-start_0
                                   (if use-prefix_0
                                     0
                                     (max
                                      0
                                      (- search-pos_0 need-lookbehind_0)))))
                              (let ((poss_0
                                     (drive-regexp-match.1
                                      #f
                                      #f
                                      #f
                                      #f
                                      #f
                                      'positions
                                      #f
                                      #f
                                      search-pos_0
                                      who_0
                                      rx_0
                                      in_0
                                      in-start_0
                                      #f
                                      #f
                                      prefix_0)))
                                (let ((recur_0
                                       (|#%name|
                                        recur
                                        (lambda ()
                                          (begin
                                            (let ((end_0 (cdar poss_0)))
                                              (if (= (caar poss_0) end_0)
                                                (if (=
                                                     end_0
                                                     (chytes-length in_0))
                                                  null
                                                  (let ((app_0
                                                         (subchytes
                                                          in_0
                                                          end_0
                                                          (add1 end_0))))
                                                    (cons
                                                     app_0
                                                     (loop_0
                                                      (add1 end_0)
                                                      #t))))
                                                (loop_0 end_0 #t))))))))
                                  (if (not poss_0)
                                    (let ((result_0
                                           (if (zero? search-pos_0)
                                             in_0
                                             (subchytes in_0 search-pos_0))))
                                      (if get-list?_0
                                        (list result_0)
                                        result_0))
                                    (let ((pre_0
                                           (subchytes
                                            in_0
                                            search-pos_0
                                            (caar poss_0))))
                                      (let ((new_0
                                             (replacements
                                              who_0
                                              in_0
                                              poss_0
                                              ins_0
                                              prefix_0)))
                                        (if all?_0
                                          (let ((result_0
                                                 (list*
                                                  pre_0
                                                  new_0
                                                  (recur_0))))
                                            (if get-list?_0
                                              result_0
                                              (apply chytes-append result_0)))
                                          (chytes-append
                                           pre_0
                                           new_0
                                           (subchytes
                                            in_0
                                            (cdar poss_0))))))))))))))))
                   (loop_0 0 #f)))))))))))
(define replacements
  (lambda (who_0 in_0 poss_0 insert_0 prefix_0)
    (if (procedure? insert_0)
      (let ((a_0
             (apply
              insert_0
              (reverse$1
               (begin
                 (letrec*
                  ((for-loop_0
                    (|#%name|
                     for-loop
                     (lambda (fold-var_0 lst_0)
                       (begin
                         (if (pair? lst_0)
                           (let ((pos_0 (unsafe-car lst_0)))
                             (let ((rest_0 (unsafe-cdr lst_0)))
                               (let ((fold-var_1
                                      (let ((fold-var_1
                                             (cons
                                              (if pos_0
                                                (let ((app_0 (car pos_0)))
                                                  (subchytes*
                                                   in_0
                                                   app_0
                                                   (cdr pos_0)
                                                   prefix_0))
                                                #f)
                                              fold-var_0)))
                                        (values fold-var_1))))
                                 (for-loop_0 fold-var_1 rest_0))))
                           fold-var_0))))))
                  (for-loop_0 null poss_0)))))))
        (begin
          (if (chytes? in_0 a_0)
            (void)
            (raise-result-error
             who_0
             (if (bytes? in_0) "bytes?" "string?")
             a_0))
          a_0))
      (let ((count_0 (length poss_0)))
        (let ((get-chytes_0
               (|#%name|
                get-chytes
                (lambda (n_0)
                  (begin
                    (if (< n_0 count_0)
                      (let ((pos_0 (list-ref poss_0 n_0)))
                        (if pos_0
                          (let ((app_0 (car pos_0)))
                            (subchytes* in_0 app_0 (cdr pos_0) prefix_0))
                          (subchytes in_0 0 0)))
                      (subchytes in_0 0 0)))))))
          (let ((cons-chytes_0
                 (|#%name|
                  cons-chytes
                  (lambda (since_0 pos_0 l_0)
                    (begin
                      (if (= since_0 pos_0)
                        l_0
                        (cons (subchytes insert_0 since_0 pos_0) l_0)))))))
            (let ((len_0 (chytes-length insert_0)))
              (apply
               (if (bytes? insert_0) bytes-append string-append)
               (letrec*
                ((loop_0
                  (|#%name|
                   loop
                   (lambda (pos_0 since_0)
                     (begin
                       (if (= pos_0 len_0)
                         (cons-chytes_0 since_0 pos_0 null)
                         (if (= 38 (chytes-ref insert_0 pos_0))
                           (cons-chytes_0
                            since_0
                            pos_0
                            (let ((app_0 (get-chytes_0 0)))
                              (cons
                               app_0
                               (let ((app_1 (add1 pos_0)))
                                 (loop_0 app_1 (add1 pos_0))))))
                           (if (= 92 (chytes-ref insert_0 pos_0))
                             (cons-chytes_0
                              since_0
                              pos_0
                              (let ((c_0
                                     (if (< (add1 pos_0) len_0)
                                       (chytes-ref insert_0 (add1 pos_0))
                                       #f)))
                                (if (let ((or-part_0 (eq? c_0 38)))
                                      (if or-part_0 or-part_0 (eq? c_0 92)))
                                  (let ((app_0 (+ pos_0 2)))
                                    (loop_0 app_0 (add1 pos_0)))
                                  (if (eq? c_0 36)
                                    (let ((app_0 (+ pos_0 2)))
                                      (loop_0 app_0 (+ pos_0 2)))
                                    (letrec*
                                     ((d-loop_0
                                       (|#%name|
                                        d-loop
                                        (lambda (pos_1 accum_0)
                                          (begin
                                            (if (= pos_1 len_0)
                                              (list (get-chytes_0 accum_0))
                                              (let ((c_1
                                                     (chytes-ref
                                                      insert_0
                                                      pos_1)))
                                                (if (if (>= c_1 48)
                                                      (<= c_1 57)
                                                      #f)
                                                  (let ((app_0 (add1 pos_1)))
                                                    (d-loop_0
                                                     app_0
                                                     (let ((app_1
                                                            (* accum_0 10)))
                                                       (+ app_1 (- c_1 48)))))
                                                  (let ((app_0
                                                         (get-chytes_0
                                                          accum_0)))
                                                    (cons
                                                     app_0
                                                     (loop_0
                                                      pos_1
                                                      pos_1)))))))))))
                                     (d-loop_0 (add1 pos_0) 0))))))
                             (loop_0 (add1 pos_0) since_0)))))))))
                (loop_0 0 0))))))))))
(define subchytes*
  (lambda (in_0 start_0 end_0 prefix_0)
    (if (< start_0 0)
      (let ((len_0 (unsafe-bytes-length prefix_0)))
        (if (string? in_0)
          (letrec*
           ((loop_0
             (|#%name|
              loop
              (lambda (index_0 start_1 end-index_0 end_1)
                (begin
                  (if (zero? start_1)
                    (let ((pre-part_0
                           (bytes->string/utf-8
                            (subbytes prefix_0 index_0 end-index_0))))
                      (if (> end_1 0)
                        (string-append pre-part_0 (substring in_0 0 end_1))
                        pre-part_0))
                    (letrec*
                     ((bloop_0
                       (|#%name|
                        bloop
                        (lambda (index_1)
                          (begin
                            (let ((b_0 (unsafe-bytes-ref prefix_0 index_1)))
                              (if (let ((or-part_0
                                         (not (bitwise-bit-set? b_0 7))))
                                    (if or-part_0
                                      or-part_0
                                      (bitwise-bit-set? b_0 6)))
                                (if (>= end_1 0)
                                  (loop_0
                                   index_1
                                   (add1 start_1)
                                   end-index_0
                                   end_1)
                                  (let ((app_0 (add1 start_1)))
                                    (loop_0
                                     index_1
                                     app_0
                                     index_1
                                     (add1 end_1))))
                                (bloop_0 (sub1 index_1)))))))))
                     (bloop_0 (sub1 index_0)))))))))
           (loop_0 len_0 start_0 len_0 end_0))
          (let ((pre-part_0
                 (let ((app_0 (+ (unsafe-bytes-length prefix_0) start_0)))
                   (subbytes
                    prefix_0
                    app_0
                    (+ (unsafe-bytes-length prefix_0) (min 0 end_0))))))
            (if (> end_0 0)
              (bytes-append pre-part_0 (subbytes in_0 0 end_0))
              pre-part_0))))
      (subchytes in_0 start_0 end_0))))
(define 1/regexp
  (let ((regexp_0
         (|#%name|
          regexp
          (lambda (p2_0 handler1_0)
            (begin
              (begin
                (if (string? p2_0)
                  (void)
                  (raise-argument-error 'regexp "string?" p2_0))
                (make-regexp 'regexp p2_0 #f #f handler1_0)))))))
    (|#%name|
     regexp
     (case-lambda
      ((p_0) (begin (regexp_0 p_0 #f)))
      ((p_0 handler1_0) (regexp_0 p_0 handler1_0))))))
(define 1/byte-regexp
  (let ((byte-regexp_0
         (|#%name|
          byte-regexp
          (lambda (p4_0 handler3_0)
            (begin
              (begin
                (if (bytes? p4_0)
                  (void)
                  (raise-argument-error 'byte-regexp "bytes?" p4_0))
                (make-regexp 'byte-regexp p4_0 #f #t handler3_0)))))))
    (|#%name|
     byte-regexp
     (case-lambda
      ((p_0) (begin (byte-regexp_0 p_0 #f)))
      ((p_0 handler3_0) (byte-regexp_0 p_0 handler3_0))))))
(define 1/pregexp
  (let ((pregexp_0
         (|#%name|
          pregexp
          (lambda (p6_0 handler5_0)
            (begin
              (begin
                (if (string? p6_0)
                  (void)
                  (raise-argument-error 'pregexp "string?" p6_0))
                (make-regexp 'pregexp p6_0 #t #f handler5_0)))))))
    (|#%name|
     pregexp
     (case-lambda
      ((p_0) (begin (pregexp_0 p_0 #f)))
      ((p_0 handler5_0) (pregexp_0 p_0 handler5_0))))))
(define 1/byte-pregexp
  (let ((byte-pregexp_0
         (|#%name|
          byte-pregexp
          (lambda (p8_0 handler7_0)
            (begin
              (begin
                (if (bytes? p8_0)
                  (void)
                  (raise-argument-error 'byte-pregexp "bytes?" p8_0))
                (make-regexp 'byte-pregexp p8_0 #t #t handler7_0)))))))
    (|#%name|
     byte-pregexp
     (case-lambda
      ((p_0) (begin (byte-pregexp_0 p_0 #f)))
      ((p_0 handler7_0) (byte-pregexp_0 p_0 handler7_0))))))
(define 1/regexp-max-lookbehind
  (|#%name|
   regexp-max-lookbehind
   (lambda (rx_0)
     (begin
       (begin
         (if (let ((or-part_0 (1/regexp? rx_0)))
               (if or-part_0 or-part_0 (1/byte-regexp? rx_0)))
           (void)
           (raise-argument-error
            'regexp-max-lookbehind
            "(or regexp? byte-regexp?)"
            rx_0))
         (rx:regexp-max-lookbehind rx_0))))))
(define no-prefix #vu8())
(define fast-bytes?
  (lambda (rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix_0)
    (if (1/byte-regexp? rx_0)
      (if (bytes? in_0)
        (if (exact-nonnegative-integer? start-pos_0)
          (if (let ((len_0 (unsafe-bytes-length in_0)))
                (if (<= start-pos_0 len_0)
                  (let ((or-part_0 (not end-pos_0)))
                    (if or-part_0
                      or-part_0
                      (if (exact-nonnegative-integer? end-pos_0)
                        (if (<= end-pos_0 len_0) (>= end-pos_0 start-pos_0) #f)
                        #f)))
                  #f))
            (if (not out_0) (eq? prefix_0 no-prefix) #f)
            #f)
          #f)
        #f)
      #f)))
(define fast-string?
  (lambda (rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix_0)
    (if (1/regexp? rx_0)
      (if (string? in_0)
        (if (exact-nonnegative-integer? start-pos_0)
          (if (let ((len_0 (string-length in_0)))
                (if (< len_0 64)
                  (if (<= start-pos_0 len_0)
                    (let ((or-part_0 (not end-pos_0)))
                      (if or-part_0
                        or-part_0
                        (if (exact-nonnegative-integer? end-pos_0)
                          (if (<= end-pos_0 len_0)
                            (>= end-pos_0 start-pos_0)
                            #f)
                          #f)))
                    #f)
                  #f))
            (if (not out_0) (eq? prefix_0 no-prefix) #f)
            #f)
          #f)
        #f)
      #f)))
(define 1/regexp-match?
  (let ((regexp-match?_0
         (|#%name|
          regexp-match?
          (lambda (rx13_0 in14_0 start-pos9_0 end-pos10_0 out11_0 prefix12_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix12_0 unsafe-undefined)
                       no-prefix
                       prefix12_0)))
                (if (fast-bytes?
                     rx13_0
                     in14_0
                     start-pos9_0
                     end-pos10_0
                     out11_0
                     prefix_0)
                  (fast-drive-regexp-match?/bytes
                   rx13_0
                   in14_0
                   start-pos9_0
                   end-pos10_0)
                  (if (fast-string?
                       rx13_0
                       in14_0
                       start-pos9_0
                       end-pos10_0
                       out11_0
                       prefix_0)
                    (fast-drive-regexp-match?/string
                     rx13_0
                     in14_0
                     start-pos9_0
                     end-pos10_0)
                    (drive-regexp-match.1
                     #f
                     #f
                     #f
                     #t
                     #t
                     '?
                     #f
                     #f
                     unsafe-undefined
                     'regexp-match?
                     rx13_0
                     in14_0
                     start-pos9_0
                     end-pos10_0
                     out11_0
                     prefix_0)))))))))
    (|#%name|
     regexp-match?
     (case-lambda
      ((rx_0 in_0)
       (begin (regexp-match?_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix12_0)
       (regexp-match?_0 rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix12_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 out11_0)
       (regexp-match?_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out11_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos10_0)
       (regexp-match?_0 rx_0 in_0 start-pos_0 end-pos10_0 #f unsafe-undefined))
      ((rx_0 in_0 start-pos9_0)
       (regexp-match?_0 rx_0 in_0 start-pos9_0 #f #f unsafe-undefined))))))
(define 1/regexp-match-positions
  (let ((regexp-match-positions_0
         (|#%name|
          regexp-match-positions
          (lambda (rx19_0 in20_0 start-pos15_0 end-pos16_0 out17_0 prefix18_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix18_0 unsafe-undefined)
                       no-prefix
                       prefix18_0)))
                (if (fast-bytes?
                     rx19_0
                     in20_0
                     start-pos15_0
                     end-pos16_0
                     out17_0
                     prefix_0)
                  (fast-drive-regexp-match-positions/bytes
                   rx19_0
                   in20_0
                   start-pos15_0
                   end-pos16_0)
                  (if (fast-string?
                       rx19_0
                       in20_0
                       start-pos15_0
                       end-pos16_0
                       out17_0
                       prefix_0)
                    (fast-drive-regexp-match-positions/string
                     rx19_0
                     in20_0
                     start-pos15_0
                     end-pos16_0)
                    (drive-regexp-match.1
                     #f
                     #f
                     #f
                     #t
                     #t
                     'positions
                     #f
                     #f
                     unsafe-undefined
                     'regexp-match-positions
                     rx19_0
                     in20_0
                     start-pos15_0
                     end-pos16_0
                     out17_0
                     prefix_0)))))))))
    (|#%name|
     regexp-match-positions
     (case-lambda
      ((rx_0 in_0)
       (begin (regexp-match-positions_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix18_0)
       (regexp-match-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out_0
        prefix18_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 out17_0)
       (regexp-match-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out17_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos16_0)
       (regexp-match-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos16_0
        #f
        unsafe-undefined))
      ((rx_0 in_0 start-pos15_0)
       (regexp-match-positions_0
        rx_0
        in_0
        start-pos15_0
        #f
        #f
        unsafe-undefined))))))
(define 1/regexp-match
  (let ((regexp-match_0
         (|#%name|
          regexp-match
          (lambda (rx25_0 in26_0 start-pos21_0 end-pos22_0 out23_0 prefix24_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix24_0 unsafe-undefined)
                       no-prefix
                       prefix24_0)))
                (if (fast-bytes?
                     rx25_0
                     in26_0
                     start-pos21_0
                     end-pos22_0
                     out23_0
                     prefix_0)
                  (fast-drive-regexp-match/bytes
                   rx25_0
                   in26_0
                   start-pos21_0
                   end-pos22_0)
                  (if (fast-string?
                       rx25_0
                       in26_0
                       start-pos21_0
                       end-pos22_0
                       out23_0
                       prefix_0)
                    (fast-drive-regexp-match/string
                     rx25_0
                     in26_0
                     start-pos21_0
                     end-pos22_0)
                    (drive-regexp-match.1
                     #f
                     #f
                     #f
                     #t
                     #t
                     'strings
                     #f
                     #f
                     unsafe-undefined
                     'regexp-match
                     rx25_0
                     in26_0
                     start-pos21_0
                     end-pos22_0
                     out23_0
                     prefix_0)))))))))
    (|#%name|
     regexp-match
     (case-lambda
      ((rx_0 in_0) (begin (regexp-match_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix24_0)
       (regexp-match_0 rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix24_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 out23_0)
       (regexp-match_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out23_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos22_0)
       (regexp-match_0 rx_0 in_0 start-pos_0 end-pos22_0 #f unsafe-undefined))
      ((rx_0 in_0 start-pos21_0)
       (regexp-match_0 rx_0 in_0 start-pos21_0 #f #f unsafe-undefined))))))
(define 1/regexp-match-positions/end
  (let ((regexp-match-positions/end_0
         (|#%name|
          regexp-match-positions/end
          (lambda (rx32_0
                   in33_0
                   start-pos27_0
                   end-pos28_0
                   out29_0
                   prefix30_0
                   end-bytes-count31_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix30_0 unsafe-undefined)
                       no-prefix
                       prefix30_0)))
                (drive-regexp-match.1
                 end-bytes-count31_0
                 #t
                 #f
                 #t
                 #t
                 'positions
                 #f
                 #f
                 unsafe-undefined
                 'regexp-match-positions/end
                 rx32_0
                 in33_0
                 start-pos27_0
                 end-pos28_0
                 out29_0
                 prefix_0)))))))
    (|#%name|
     regexp-match-positions/end
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-positions/end_0 rx_0 in_0 0 #f #f unsafe-undefined 1)))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix_0 end-bytes-count31_0)
       (regexp-match-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out_0
        prefix_0
        end-bytes-count31_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix30_0)
       (regexp-match-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out_0
        prefix30_0
        1))
      ((rx_0 in_0 start-pos_0 end-pos_0 out29_0)
       (regexp-match-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out29_0
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos_0 end-pos28_0)
       (regexp-match-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos28_0
        #f
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos27_0)
       (regexp-match-positions/end_0
        rx_0
        in_0
        start-pos27_0
        #f
        #f
        unsafe-undefined
        1))))))
(define 1/regexp-match/end
  (let ((regexp-match/end_0
         (|#%name|
          regexp-match/end
          (lambda (rx39_0
                   in40_0
                   start-pos34_0
                   end-pos35_0
                   out36_0
                   prefix37_0
                   end-bytes-count38_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix37_0 unsafe-undefined)
                       no-prefix
                       prefix37_0)))
                (drive-regexp-match.1
                 end-bytes-count38_0
                 #t
                 #f
                 #t
                 #t
                 'strings
                 #f
                 #f
                 unsafe-undefined
                 'regexp-match/end
                 rx39_0
                 in40_0
                 start-pos34_0
                 end-pos35_0
                 out36_0
                 prefix_0)))))))
    (|#%name|
     regexp-match/end
     (case-lambda
      ((rx_0 in_0)
       (begin (regexp-match/end_0 rx_0 in_0 0 #f #f unsafe-undefined 1)))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix_0 end-bytes-count38_0)
       (regexp-match/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out_0
        prefix_0
        end-bytes-count38_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix37_0)
       (regexp-match/end_0 rx_0 in_0 start-pos_0 end-pos_0 out_0 prefix37_0 1))
      ((rx_0 in_0 start-pos_0 end-pos_0 out36_0)
       (regexp-match/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        out36_0
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos_0 end-pos35_0)
       (regexp-match/end_0
        rx_0
        in_0
        start-pos_0
        end-pos35_0
        #f
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos34_0)
       (regexp-match/end_0
        rx_0
        in_0
        start-pos34_0
        #f
        #f
        unsafe-undefined
        1))))))
(define 1/regexp-match-peek
  (let ((regexp-match-peek_0
         (|#%name|
          regexp-match-peek
          (lambda (rx45_0
                   in46_0
                   start-pos41_0
                   end-pos42_0
                   progress-evt43_0
                   prefix44_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix44_0 unsafe-undefined)
                       no-prefix
                       prefix44_0)))
                (drive-regexp-match.1
                 #f
                 #f
                 #f
                 #t
                 #t
                 'strings
                 #t
                 progress-evt43_0
                 unsafe-undefined
                 'regexp-match-peek
                 rx45_0
                 in46_0
                 start-pos41_0
                 end-pos42_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek
     (case-lambda
      ((rx_0 in_0)
       (begin (regexp-match-peek_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix44_0)
       (regexp-match-peek_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix44_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt43_0)
       (regexp-match-peek_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt43_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos42_0)
       (regexp-match-peek_0
        rx_0
        in_0
        start-pos_0
        end-pos42_0
        #f
        unsafe-undefined))
      ((rx_0 in_0 start-pos41_0)
       (regexp-match-peek_0
        rx_0
        in_0
        start-pos41_0
        #f
        #f
        unsafe-undefined))))))
(define 1/regexp-match-peek-immediate
  (let ((regexp-match-peek-immediate_0
         (|#%name|
          regexp-match-peek-immediate
          (lambda (rx51_0
                   in52_0
                   start-pos47_0
                   end-pos48_0
                   progress-evt49_0
                   prefix50_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix50_0 unsafe-undefined)
                       no-prefix
                       prefix50_0)))
                (drive-regexp-match.1
                 #f
                 #f
                 #t
                 #t
                 #t
                 'strings
                 #t
                 progress-evt49_0
                 unsafe-undefined
                 'regexp-match-peek-immediate
                 rx51_0
                 in52_0
                 start-pos47_0
                 end-pos48_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek-immediate
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-peek-immediate_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix50_0)
       (regexp-match-peek-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix50_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt49_0)
       (regexp-match-peek-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt49_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos48_0)
       (regexp-match-peek-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos48_0
        #f
        unsafe-undefined))
      ((rx_0 in_0 start-pos47_0)
       (regexp-match-peek-immediate_0
        rx_0
        in_0
        start-pos47_0
        #f
        #f
        unsafe-undefined))))))
(define 1/regexp-match-peek-positions
  (let ((regexp-match-peek-positions_0
         (|#%name|
          regexp-match-peek-positions
          (lambda (rx57_0
                   in58_0
                   start-pos53_0
                   end-pos54_0
                   progress-evt55_0
                   prefix56_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix56_0 unsafe-undefined)
                       no-prefix
                       prefix56_0)))
                (drive-regexp-match.1
                 #f
                 #f
                 #f
                 #t
                 #t
                 'positions
                 #t
                 progress-evt55_0
                 unsafe-undefined
                 'regexp-match-peek-positions
                 rx57_0
                 in58_0
                 start-pos53_0
                 end-pos54_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek-positions
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-peek-positions_0 rx_0 in_0 0 #f #f unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix56_0)
       (regexp-match-peek-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix56_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt55_0)
       (regexp-match-peek-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt55_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos54_0)
       (regexp-match-peek-positions_0
        rx_0
        in_0
        start-pos_0
        end-pos54_0
        #f
        unsafe-undefined))
      ((rx_0 in_0 start-pos53_0)
       (regexp-match-peek-positions_0
        rx_0
        in_0
        start-pos53_0
        #f
        #f
        unsafe-undefined))))))
(define 1/regexp-match-peek-positions/end
  (let ((regexp-match-peek-positions/end_0
         (|#%name|
          regexp-match-peek-positions/end
          (lambda (rx64_0
                   in65_0
                   start-pos59_0
                   end-pos60_0
                   progress-evt61_0
                   prefix62_0
                   end-bytes-count63_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix62_0 unsafe-undefined)
                       no-prefix
                       prefix62_0)))
                (drive-regexp-match.1
                 end-bytes-count63_0
                 #t
                 #f
                 #t
                 #t
                 'positions
                 #t
                 progress-evt61_0
                 unsafe-undefined
                 'regexp-match-peek-positions/end
                 rx64_0
                 in65_0
                 start-pos59_0
                 end-pos60_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek-positions/end
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-peek-positions/end_0
          rx_0
          in_0
          0
          #f
          #f
          unsafe-undefined
          1)))
      ((rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix_0
        end-bytes-count63_0)
       (regexp-match-peek-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix_0
        end-bytes-count63_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix62_0)
       (regexp-match-peek-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix62_0
        1))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt61_0)
       (regexp-match-peek-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt61_0
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos_0 end-pos60_0)
       (regexp-match-peek-positions/end_0
        rx_0
        in_0
        start-pos_0
        end-pos60_0
        #f
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos59_0)
       (regexp-match-peek-positions/end_0
        rx_0
        in_0
        start-pos59_0
        #f
        #f
        unsafe-undefined
        1))))))
(define 1/regexp-match-peek-positions-immediate
  (let ((regexp-match-peek-positions-immediate_0
         (|#%name|
          regexp-match-peek-positions-immediate
          (lambda (rx70_0
                   in71_0
                   start-pos66_0
                   end-pos67_0
                   progress-evt68_0
                   prefix69_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix69_0 unsafe-undefined)
                       no-prefix
                       prefix69_0)))
                (drive-regexp-match.1
                 #f
                 #f
                 #t
                 #t
                 #t
                 'positions
                 #t
                 progress-evt68_0
                 unsafe-undefined
                 'regexp-match-peek-positions-immediate
                 rx70_0
                 in71_0
                 start-pos66_0
                 end-pos67_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek-positions-immediate
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-peek-positions-immediate_0
          rx_0
          in_0
          0
          #f
          #f
          unsafe-undefined)))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix69_0)
       (regexp-match-peek-positions-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix69_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt68_0)
       (regexp-match-peek-positions-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt68_0
        unsafe-undefined))
      ((rx_0 in_0 start-pos_0 end-pos67_0)
       (regexp-match-peek-positions-immediate_0
        rx_0
        in_0
        start-pos_0
        end-pos67_0
        #f
        unsafe-undefined))
      ((rx_0 in_0 start-pos66_0)
       (regexp-match-peek-positions-immediate_0
        rx_0
        in_0
        start-pos66_0
        #f
        #f
        unsafe-undefined))))))
(define 1/regexp-match-peek-positions-immediate/end
  (let ((regexp-match-peek-positions-immediate/end_0
         (|#%name|
          regexp-match-peek-positions-immediate/end
          (lambda (rx77_0
                   in78_0
                   start-pos72_0
                   end-pos73_0
                   progress-evt74_0
                   prefix75_0
                   end-bytes-count76_0)
            (begin
              (let ((prefix_0
                     (if (eq? prefix75_0 unsafe-undefined)
                       no-prefix
                       prefix75_0)))
                (drive-regexp-match.1
                 end-bytes-count76_0
                 #t
                 #t
                 #t
                 #t
                 'positions
                 #t
                 progress-evt74_0
                 unsafe-undefined
                 'regexp-match-peek-positions-immediate/end
                 rx77_0
                 in78_0
                 start-pos72_0
                 end-pos73_0
                 #f
                 prefix_0)))))))
    (|#%name|
     regexp-match-peek-positions-immediate/end
     (case-lambda
      ((rx_0 in_0)
       (begin
         (regexp-match-peek-positions-immediate/end_0
          rx_0
          in_0
          0
          #f
          #f
          unsafe-undefined
          1)))
      ((rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix_0
        end-bytes-count76_0)
       (regexp-match-peek-positions-immediate/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix_0
        end-bytes-count76_0))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt_0 prefix75_0)
       (regexp-match-peek-positions-immediate/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt_0
        prefix75_0
        1))
      ((rx_0 in_0 start-pos_0 end-pos_0 progress-evt74_0)
       (regexp-match-peek-positions-immediate/end_0
        rx_0
        in_0
        start-pos_0
        end-pos_0
        progress-evt74_0
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos_0 end-pos73_0)
       (regexp-match-peek-positions-immediate/end_0
        rx_0
        in_0
        start-pos_0
        end-pos73_0
        #f
        unsafe-undefined
        1))
      ((rx_0 in_0 start-pos72_0)
       (regexp-match-peek-positions-immediate/end_0
        rx_0
        in_0
        start-pos72_0
        #f
        #f
        unsafe-undefined
        1))))))
(define regexp-place-init! (lambda () (range-place-init!)))
