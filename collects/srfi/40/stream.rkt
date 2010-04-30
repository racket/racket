; SRFI 40
; STREAM -- LIBRARY OF SYNTAX AND FUNCTIONS TO MANIPULATE STREAMS
; Zhu Chongkai    mrmathematica@yahoo.com
; 3-Apr-2005

(module stream mzscheme
  
  (provide stream-null
           stream-cons
           stream?
           stream-null?
           stream-pair?
           stream-car
           stream-cdr
           stream-delay
           (rename my-stream stream)
           stream-unfoldn
           stream-map
           stream-for-each
           stream-filter)
  
  ;;; PROMISES A LA SRFI-45:
  
  ;;; A separate implementation is necessary to
  ;;; have promises that answer #t to stream?
  ;;; This requires lots of complicated type conversions.
  
  (define-struct s:promise (kind content))
  
  (define-syntax srfi-40:lazy
    (syntax-rules ()
      ((_ exp)
       (box (make-s:promise 'lazy (lambda () exp))))))
  
  (define (srfi-40:eager x)
    (make-stream (box (make-s:promise 'eager x))))
  
  (define-syntax srfi-40:delay
    (syntax-rules ()
      ((_ exp)
       (srfi-40:lazy (srfi-40:eager exp)))))
  
  (define (srfi-40:force promise)
    (let ((content (unbox promise)))
      (case (s:promise-kind content)
        ((eager) (s:promise-content content))
        ((lazy)
         (let* ((promise* (stream-promise ((s:promise-content content))))
                (content (unbox promise)))
           (unless (eq? 'eager (s:promise-kind content))
             (set-s:promise-kind! content (s:promise-kind (unbox promise*)))
             (set-s:promise-content! content (s:promise-content (unbox promise*)))
             (set-box! promise* content))
           (srfi-40:force promise))))))
  
  ;;; A stream is a new data type, disjoint from all other data types, that
  ;;; contains a promise that, when forced, is either nil (a single object
  ;;; distinguishable from all other objects) or consists of an object
  ;;; (the stream element) followed by a stream.  Each stream element is
  ;;; evaluated exactly once, when it is first retrieved (not when it is
  ;;; created); once evaluated its value is saved to be returned by
  ;;; subsequent retrievals without being evaluated again.
  
  ;; STREAM? object -- #t if object is a stream, #f otherwise
  (define-struct stream (promise))
  
  ;; STREAM-NULL -- the distinguished nil stream
  (define stream-null (make-stream (srfi-40:delay '())))
  
  ;; STREAM-CONS object stream -- primitive constructor of streams
  (define-syntax stream-cons
    (syntax-rules ()
      ((_ obj strm)
       (make-stream (srfi-40:delay (cons obj strm))))))
  
  ;; STREAM-NULL? object -- #t if object is the null stream, #f otherwise
  (define (stream-null? obj)
    (and (stream? obj)
         (null? (srfi-40:force (stream-promise obj)))))
  
  ;; STREAM-PAIR? object -- #t if object is a non-null stream, #f otherwise
  (define (stream-pair? obj)
    (and (stream? obj)
         (not (null? (srfi-40:force (stream-promise obj))))))
  
  ;; STREAM-CAR stream -- first element of stream
  (define (stream-car strm)
    (unless (stream? strm)
      (raise-type-error 'stream-car "stream" strm))
    (let ((pair (srfi-40:force (stream-promise strm))))
      (if (null? pair)
          (raise-type-error 'stream-car "stream-pair" strm)
          (car pair))))
  
  ;; STREAM-CDR stream -- remaining elements of stream after first
  (define (stream-cdr strm)
    (unless (stream? strm)
      (raise-type-error 'stream-cdr "stream" strm))
    (let ((pair (srfi-40:force (stream-promise strm))))
      (if (null? pair)
          (raise-type-error 'stream-cdr "stream-pair" strm)
          (cdr pair))))
  
  ;; STREAM-DELAY object -- the essential stream mechanism
  (define-syntax stream-delay
    (syntax-rules ()
      ((_ expr)
       (make-stream (srfi-40:lazy expr)))))
  
  ;; STREAM object ... -- new stream whose elements are object ...
  (define (my-stream . objs)
    (let loop ((objs objs))
      (stream-delay
       (if (null? objs)
           stream-null
           (stream-cons (car objs) (loop (cdr objs)))))))
  
  ;; STREAM-UNFOLDN generator seed n -- n streams from (generator seed)
  (define stream-unfoldn
    (letrec ((unfold-result-stream
              (lambda (gen seed)
                (let loop ((seed seed))
                  (stream-delay
                   (call-with-values
                    (lambda () (gen seed))
                    (lambda (next . results)
                      (stream-cons results (loop next))))))))
             (result-stream->output-stream
              (lambda (result-stream i)
                (stream-delay
                 (let ((result
                        (list-ref (stream-car result-stream) i)))
                   (cond ((pair? result)
                          (stream-cons (car result)
                                       (result-stream->output-stream
                                        (stream-cdr result-stream) i)))
                         ((not result)
                          (result-stream->output-stream
                           (stream-cdr result-stream) i))
                         ((null? result)
                          stream-null)
                         (else
                          (raise-mismatch-error
                           'stream-unfoldn
                           "result of the generator should be <pair>/#f/null; given "
                           result)))))))
             (result-stream->output-streams
              (lambda (result-stream n)
                (let loop ((i n) (outputs '()))
                  (if (zero? i)
                      (apply values outputs)
                      (let ((i (sub1 i)))
                        (loop i
                              (cons (result-stream->output-stream result-stream i)
                                    outputs))))))))
      (lambda (gen seed n)
        (unless (procedure-arity-includes? gen 1)
          (raise-type-error 'stream-unfoldn "procedure of arity 1" gen))
        (unless (and (integer? n)
                     (exact? n)
                     (positive? n))
          (raise-type-error 'stream-unfoldn "exact, non-negative integer" n))
        (result-stream->output-streams (unfold-result-stream gen seed) n))))
  
  ;; STREAM-MAP func stream ... -- stream produced by applying func element-wise
  (define (stream-map func strm1 . strms)
    (unless (procedure? func)
      (raise-type-error 'stream-map "procedure" func))
    (unless (stream? strm1)
      (raise-type-error 'stream-map "stream" strm1))
    (if (null? strms)
        (let loop ((strm strm1))
          (stream-delay
           (if (stream-null? strm)
               stream-null
               (stream-cons (func (stream-car strm))
                            (loop (stream-cdr strm))))))
        (if (andmap stream? strms)
            (let loop ((strms (cons strm1 strms)))
              (stream-delay
               (if (ormap stream-null? strms)
                   stream-null
                   (stream-cons (apply func (map stream-car strms))
                                (loop (map stream-cdr strms))))))
            (raise-type-error 'stream-map "streams" strms))))
  
  ;; STREAM-FOR-EACH proc stream ... -- apply proc element-wise for side-effects
  (define (stream-for-each proc strm1 . strms)
    (unless (procedure? proc)
      (raise-type-error 'stream-for-each "procedure" proc))
    (unless (stream? strm1)
      (raise-type-error 'stream-for-each "stream" strm1))
    (if (null? strms)
        (let loop ((strm strm1))
          (unless (stream-null? strm)
            (proc (stream-car strm))
            (loop (stream-cdr strm))))
        (if (andmap stream? strms)
            (let loop ((strms (cons strm1 strms)))
              (unless (ormap stream-null? strms)
                (apply proc (map stream-car strms))
                (loop (map stream-cdr strms))))
            (raise-type-error 'stream-for-each "streams" strms))))
  
  ;; STREAM-FILTER pred? stream -- new stream including only items passing pred?
  (define (stream-filter pred? strm)
    (unless (procedure? pred?)
      (raise-type-error 'stream-filter "procedure" pred?))
    (unless (stream? strm)
      (raise-type-error 'stream-filter "stream" strm))
    (let loop ((s strm))
      (stream-delay
       (cond ((stream-null? s)
              stream-null)
             ((pred? (stream-car s))
              (stream-cons (stream-car s)
                           (loop (stream-cdr s))))
             (else
              (loop (stream-cdr s))))))))
