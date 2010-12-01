;; Comprehensions for vector like data types
(module for-vector '#%kernel

  (#%require "more-scheme.rkt"
             "misc.rkt"
             "define.rkt"
             "letstx-scheme.rkt"
             "for-base.rkt"
             '#%unsafe
             (for-syntax '#%kernel
                         "stx.rkt"
                         "qqstx.rkt"
                         "define.rkt"
                         "small-scheme.rkt"
                         "stxcase-scheme.rkt"))

  (#%provide in-vector
             in-string
             in-bytes
             
             :vector-gen
             :string-gen
             :bytes-gen

             *in-vector
             *in-string
             *in-bytes

             define-in-vector-like
             define-:vector-like-gen

             (for-syntax make-in-vector-like))

  ;; (: check-ranges (Symbol Natural Natural Integer -> Void))
  (define (check-ranges who start stop step)
    (unless (exact-nonnegative-integer? start) (raise-type-error who "exact non-negative integer" start))
    (unless (exact-nonnegative-integer? stop) (raise-type-error who "exact non-negative integer or #f" stop))
    (unless (and (exact-integer? step) (not (zero? step)))
      (raise-type-error who "exact non-zero integer" step))
    (when (and (< start stop) (< step 0))
      (raise-mismatch-error who (format "start: ~a less than stop: ~a but given negative step: "
                                        start stop)
                            step))
    (when (and (< stop start) (> step 0))
      (raise-mismatch-error who (format "start: ~a more than stop: ~a but given positive step: "
                                        start stop)
                            step)))

  (define-syntax define-in-vector-like
    (syntax-rules ()
      [(define-in-vector-like in-vector-name
         type-name-str vector?-id vector-length-id :vector-gen-id)
       (define in-vector-name
        (case-lambda
          [(v) (in-vector-name v 0 #f 1)]
          [(v start) (in-vector-name v start #f 1)]
          [(v start stop) (in-vector-name v start stop 1)]
          [(v start stop step)
           (unless (vector?-id v) (raise-type-error (quote in-vector-name) type-name-str v))
           (let ([stop (or stop (vector-length-id v))])
             (check-ranges (quote in-vector-name) start stop step)
             (make-do-sequence (lambda () (:vector-gen-id v start stop step))))]))]))

  (define-syntax define-:vector-like-gen
    (syntax-rules ()
      [(define-:vector-like-gen :vector-like-name unsafe-vector-ref-id)
       (define (:vector-like-name v start stop step)
         (values
          ;; pos->element
          (lambda (i) (unsafe-vector-ref-id v i))
          ;; next-pos
          ;; Minor optimisation.  I assume add1 is faster than \x.x+1
          (if (= step 1) add1 (lambda (i) (+ i step)))
          ;; initial pos
          start
          ;; continue?
          (if (> step 0)
              (lambda (i) (< i stop))
              (lambda (i) (> i stop)))
          void
          void))]))

  (define-for-syntax (make-in-vector-like vector?-id
                                          unsafe-vector-length-id
                                          in-vector-id
                                          unsafe-vector-ref-id)
     (define (in-vector-like stx)
       (with-syntax ([vector? vector?-id]
                     [in-vector in-vector-id]
                     [unsafe-vector-length unsafe-vector-length-id]
                     [unsafe-vector-ref unsafe-vector-ref-id])
         (syntax-case stx ()
           ;; Fast case
           [[(id) (_ vec-expr)]
            #'[(id)
               (:do-in
                ;;outer bindings
                ([(vec len) (let ([vec vec-expr])
                              (unless (vector? vec)
                                (in-vector vec))
                              (values vec (unsafe-vector-length vec)))])
                ;; outer check
                #f
                ;; loop bindings
                ([pos 0])
                ;; pos check
                (pos . unsafe-fx< . len)
                ;; inner bindings
                ([(id) (unsafe-vector-ref vec pos)])
                ;; pre guard
                #t
                ;; post guard
                #t
                ;; loop args
                ((unsafe-fx+ 1 pos)))]]
           ;; General case
           [((id) (_ vec-expr start))
            (in-vector-like (syntax ((id) (_ vec-expr start #f 1))))]
           [((id) (_ vec-expr start stop))
            (in-vector-like (syntax ((id) (_ vec-expr start stop 1))))]
           [((id) (_ vec-expr start stop step))
            (let ([all-fx? (memq (syntax-e #'step) '(1 -1))])
              #`[(id)
                 (:do-in
                  ;; Outer bindings
                  ;; Prevent multiple evaluation
                  ([(v* stop*) (let ([vec vec-expr]
                                     [stop* stop])
                                 (if (and (not stop*) (vector? vec))
                                     (values vec (unsafe-vector-length vec))
                                     (values vec stop*)))]
                   [(start*) start]
                   [(step*) step])
                  ;; Outer check
                  (when (or (not (vector? v*))
                            (not (exact-integer? start*))
                            (not (exact-integer? stop*))
                            (not (exact-integer? step*))
                            (zero? step*)
                            (and (< start* stop*) (< step* 0))
                            (and (> start* stop*) (> step* 0)))
                    ;; Let in-vector report the error
                    (in-vector v* start* stop* step*))
                  ;; Loop bindings
                  ([idx start*])
                  ;; Pos guard
                  #,(cond
                     [(not (number? (syntax-e #'step)))
                      #`(if (step* . >= . 0) (< idx stop*) (> idx stop*))]
                     [((syntax-e #'step) . >= . 0)
                      (if all-fx?
                          #'(unsafe-fx< idx stop*)
                          #'(< idx stop*))]
                     [else
                      (if all-fx?
                          #'(unsafe-fx> idx stop*)
                          #'(> idx stop*))])
                  ;; Inner bindings
                  ([(id) (unsafe-vector-ref v* idx)])
                  ;; Pre guard
                  #t
                  ;; Post guard
                  #t
                  ;; Loop args
                  ((#,(if all-fx? #'unsafe-fx+ #'+) idx step)))])]
           [_ #f])))
          in-vector-like)

  (define-:vector-like-gen :vector-gen unsafe-vector-ref)
  
  (define-in-vector-like in-vector
    "vector" vector? vector-length :vector-gen)


  (define-:vector-like-gen :string-gen unsafe-string-ref)
  
  (define-in-vector-like in-string
    "string" string? string-length :string-gen)


  (define-:vector-like-gen :bytes-gen unsafe-bytes-ref)
  
  (define-in-vector-like in-bytes
    "bytes" bytes? bytes-length :bytes-gen)

  (define-sequence-syntax *in-vector
    (lambda () #'in-vector)
    (make-in-vector-like #'vector?
                         #'unsafe-vector-length
                         #'in-vector
                         #'unsafe-vector-ref))

  (define-sequence-syntax *in-string
    (lambda () #'in-string)
    (make-in-vector-like #'string?
                         #'string-length
                         #'in-string
                         #'string-ref))

  (define-sequence-syntax *in-bytes
    (lambda () #'in-bytes)
    (make-in-vector-like #'bytes?
                         #'bytes-length
                         #'in-bytes
                         #'bytes-ref))
  
  )