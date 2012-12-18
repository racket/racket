#lang racket/base

(require racket/place/distributed
         racket/match
         racket/list
         racket/place
         racket/class
         racket/flonum
         racket/fixnum
         racket/vector)

(provide rmpi-init
         rmpi-send
         rmpi-recv
         rmpi-broadcast
         rmpi-reduce
         rmpi-gather
         rmpi-allreduce
         rmpi-allgather
         rmpi-alltoall
         rmpi-alltoallv
         rmpi-barrier
         rmpi-id
         rmpi-cnt
         rmpi-comm-split
         rmpi-partition
         rmpi-build-default-config
         rmpi-make-localhost-config
         rmpi-launch
         rmpi-finish
         (struct-out rmpi-comm))

(struct rmpi-comm (id cnt channels) #:transparent)

(define (rmpi-id comm) (rmpi-comm-id comm))
(define (rmpi-cnt comm) (rmpi-comm-cnt comm))
(define (rmpi-send comm dest val) (place-channel-put (vector-ref (rmpi-comm-channels comm) dest) val))
(define (rmpi-recv comm src) (place-channel-get (vector-ref (rmpi-comm-channels comm) src)))

(define (rmpi-init ch)
  (define tc (new named-place-typed-channel% [ch ch]))
  (match-define (list (list 'rmpi-id id config) return-ch) (tc-get 'rmpi-id tc))
  (match-define (list (list 'args args) src-ch) (tc-get 'args tc))

  (define mpi-comm-vector
    (for/vector #:length (length config) ([c config])
      (match-define (list dest dest-port dest-name dest-id) c)
      (cond
        [(< id dest-id)
         (send-new-place-channel-to-named-dest ch id (list dest dest-port dest-name))]
        [else null])))

  (for ([i (length config)])
    (when (> id i)
      (match-define (list (list 'new-place-channel src-id) src-ch) (tc-get 'new-place-channel tc))
      (vector-set! mpi-comm-vector src-id src-ch)))

  (values
    (rmpi-comm id (length config) mpi-comm-vector)
    args
    tc))


(define rmpi-broadcast 
  (case-lambda
    [(comm src)
     (rmpi-broadcast comm src (void))]
    [(comm src val)
     (match-define (rmpi-comm real-id cnt chs) comm)
     (define offset (- cnt src))
     (define id (modulo (+ real-id (- cnt src)) cnt))
     (let loop ([i 0]
                [val val])
       (define round (arithmetic-shift 1 i))
       (cond
         [(< round cnt)
           (loop 
             (add1 i)
             (cond
               [(< id (arithmetic-shift round 1))
                 (cond 
                   [(not (= 0 (bitwise-and id round)))
                    (define peer-id (- id round))
                    (define real-peer-id (modulo (+ peer-id src) cnt))
                    ;(printf "BRECV ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset)
                    (place-channel-get (vector-ref chs real-peer-id))
                    ]
                   [else
                    (define peer-id (+ id round))
                    (cond
                      [(< peer-id cnt)
                        (define real-peer-id (modulo (+ peer-id src) cnt))
                        ;(printf "BSEND ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset)
                        (place-channel-put (vector-ref chs real-peer-id) val)])
                    val])]
               [else val]))]
         [else val]))]))

(define (fancy-reducer op recv-val val)
  (cond 
    [(or (number? recv-val)
         (boolean? recv-val))
     (op recv-val val)]
    [(vector? recv-val)
     (for/vector #:length (vector-length recv-val)
                 ([a (in-vector recv-val)]
                  [b (in-vector val)])
       (fancy-reducer op a b))]
    [(fxvector? recv-val)
     (for/fxvector #:length (fxvector-length recv-val)
                 ([a (in-fxvector recv-val)]
                  [b (in-fxvector val)])
       (fancy-reducer op a b))]
    [(flvector? recv-val)
     (for/flvector #:length (flvector-length recv-val)
                 ([a (in-flvector recv-val)]
                  [b (in-flvector val)])
       (fancy-reducer op a b))]
    [else (raise (format "fancy-reducer error on ~a ~a ~a" op recv-val val))]))

(define (rmpi-reduce comm dest op val)
  (match-define (rmpi-comm real-id cnt chs) comm)
  (define i
    (let loop ([i 0])
      (if (>= (arithmetic-shift 1 i) cnt)
        i
        (loop (add1 i)))))


  (define offset (- cnt dest))
  (define (convert v) (modulo (+ v offset) cnt))
  (define (pconvert v) (modulo (+ v dest) cnt))
  (define id (convert real-id))
  (let loop ([i i]
             [val val])
    (cond
      [(> i 0)
       (define round (arithmetic-shift 1 (sub1 i)))
       (loop 
         (sub1 i)
         (cond
           [(< id (arithmetic-shift round 1))
             (cond 
               [(not (= 0 (bitwise-and id round)))
                (define peer-id (- id round))
                (define real-peer-id (pconvert peer-id))
                ;(printf/f "SEND ROUND ~a RID ~a RPID ~a ID ~a PID ~a OFF ~a\n" round real-id real-peer-id id peer-id offset)
                (place-channel-put (vector-ref chs real-peer-id) val)
                val
                ]
               [else
                (define peer-id (+ id round))
                (cond
                  [(< peer-id cnt)
                    (define real-peer-id (pconvert peer-id))
                    ;(printf/f "RECV ROUND ~a RID ~a RPID ~a ID ~a PID ~a OFF ~a\n" round real-id real-peer-id id peer-id offset)
                    (define recv-val (place-channel-get (vector-ref chs real-peer-id)))
                    ;(printf/f "RECVVAL ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset recv-val)
                    (fancy-reducer op recv-val val)]
                  [else val])])]))]
      [else val])))

(define (rmpi-gather comm dest val)
  (match-define (rmpi-comm real-id cnt chs) comm)
  (define i
    (let loop ([i 0])
      (if (>= (arithmetic-shift 1 i) cnt)
        i
        (loop (add1 i)))))


  (define offset (- cnt dest))
  (define (convert v) (modulo (+ v offset) cnt))
  (define (pconvert v) (modulo (+ v dest) cnt))
  (define id (convert real-id))
  (define retval
    (let loop ([i i]
               [val (vector val)])
      (cond
        [(> i 0)
         (define round (arithmetic-shift 1 (sub1 i)))
         (loop 
           (sub1 i)
           (cond
             [(< id (arithmetic-shift round 1))
               (cond 
                 [(not (= 0 (bitwise-and id round)))
                  (define peer-id (- id round))
                  (define real-peer-id (pconvert peer-id))
                  ;(printf/f "SEND ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset)
                  (place-channel-put (vector-ref chs real-peer-id) val)
                  val
                  ]
                 [else
                  (define peer-id (+ id round))
                  (cond
                    [(< peer-id cnt)
                      (define real-peer-id (pconvert peer-id))
                      ;(printf/f "RECV ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset)
                      (define recv-val (place-channel-get (vector-ref chs real-peer-id)))
                      ;(printf/f "RECVVAL ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset recv-val)
                      (define (order real-id val real-peer-id pval)
                        (define vl (vector-length val))
                        (define pvl (vector-length pval))
                        (define new-val (make-vector (+ vl pvl)))
                        (define v1 (if (< real-id real-peer-id) val pval))
                        (define v2 (if (< real-id real-peer-id) pval val))
                        (define v1l (if (< real-id real-peer-id) vl pvl))
                        (define v2l (if (< real-id real-peer-id) pvl vl))
                        (let loop ([i 0]
                                   [j 0]
                                   [ii 0])
                          (cond 
                            [(< i v1l)
                              (vector-set! new-val ii (vector-ref v1 i))
                              (cond 
                                [(< j v2l)
                                 (vector-set! new-val (+ 1 ii) (vector-ref v2 j))
                                 (loop (+ 1 i) (+ 1 j) (+ 2 ii))]
                                [else
                                 (loop (+ 1 i) j (+ 1 ii))])]
                             [(< j v2l)
                              (vector-set! new-val (+ 1 ii) (vector-ref v2 j))
                              (loop i (+ 1 j) (+ 1 ii))]
                             [else
                               new-val])))
                      (order id val peer-id recv-val)]
                    [else val])])]))]
        [else val])))
  (cond
    [(and (not (zero? dest)) (= real-id dest))
     (for/vector #:length cnt ([i cnt])
       (vector-ref retval (modulo (+ i offset) cnt)))]
    [else retval]))


(define (rmpi-barrier comm)
  (rmpi-reduce comm 0 + 1)
  (rmpi-broadcast comm 0 1))

(define (rmpi-allreduce comm op val)
  (define rv (rmpi-reduce comm 0 op val))
  (rmpi-broadcast comm 0 rv))

(define (rmpi-allgather comm val)
  (define rv (rmpi-gather comm 0 val))
  (rmpi-broadcast comm 0 rv))

(define (partit num cnt id)
  (define-values (quo rem) (quotient/remainder num cnt))
  (values (+ (* id quo) (if (< id rem) id rem))
          (+ quo (if (< id rem) 1 0))))

(define (rmpi-partition comm num)
  (define id (rmpi-id comm))
  (define cnt (rmpi-cnt comm))
  (partit num cnt id))

(define rmpi-build-default-config
  (make-keyword-procedure (lambda (kws kw-args . rest)
    (for/hash ([kw kws]
               [kwa kw-args])
;      (displayln (keyword? kw))
      (values kw kwa)))))

(define (rmpi-launch default config #:no-wait [no-wait #f])
  (define (lookup-config-value rest key-str)
    (define key
      (string->keyword key-str))
    (cond
      [(null? rest)
        (hash-ref default key #f)]
      [else
        (hash-ref (car rest) key (lambda ()
                                   (hash-ref default key #f)))]))

;  (printf/f "~v\n" default)
;  (exit 1)
  (define nodes
    (spawn-nodes/join
      (for/list ([c config])
        (match-define (list-rest host port name id _rest) c)
        (define rest 
          (cond 
            [(null? _rest)
             (list (make-immutable-hash (list (cons (string->keyword "listen-port") port))))]
            [else
              (list
                (hash-set (car _rest) (string->keyword "listen-port") port))]))
;        (printf/f "~a\n" rest)
        (define-values (k v) 
          (let loop ([keys (list "racket-path" "listen-port" "distributed-launch-path")]
                     [k null]
                     [v null])
            (match keys
             [(cons head tail)
              (cond
                [(lookup-config-value rest head) => (lambda (x) 
                  (loop tail
                        (cons (string->keyword head) k)
                        (cons x v)))]
                [else
                  (loop (cdr keys) k v)])]
             [(list)
               (values k v)])))
;        (printf/f "~a\n" (list k v (list host)))
        (list k v (list host)))))

  (for ([n nodes]
        [c config])
    (match-define (list-rest host port name id rest) c)
    (supervise-place-at n 
                        (lookup-config-value rest "mpi-module")
                        (lookup-config-value rest "mpi-func")
                        #:named name))

  (define-values (mrth ch)
    (start-message-router/thread
      #:nodes nodes))

  (define simple-config 
    (for/list ([c config])
      (match-define (list-rest host port name id rest) c)
      (list host port name id)))

  (for ([c config])
    (match-define (list-rest host port name id rest) c)
    (define npch (mr-connect-to ch (list host port) name))
    (*channel-put npch (list 'rmpi-id id simple-config))
    (*channel-put npch (list 'args (or (lookup-config-value rest "mpi-args") null))))

  (cond
    [no-wait
      (for/first ([c config])
        (match-define (list-rest host port name id rest) c)
        (define npch (mr-connect-to ch (list host port) name))
        (list npch))]
    [else
      (for/first ([c config])
        (match-define (list-rest host port name id rest) c)
        (define npch (mr-connect-to ch (list host port) name))
        (*channel-put npch (list 'done?))
        ;Wait for 'done message from mpi node id 0
        (*channel-get npch))]))


(define (rmpi-finish comm tc)
  (when (= (rmpi-id comm) 0)
        (place-channel-put (second (tc-get 'done? tc)) 'done)))

(define (rmpi-make-localhost-config cnt start-port name)
  (for/list ([i cnt])
    (list "localhost" (+ start-port i) (string->symbol (format "~a_~a" (symbol->string name) (number->string i)))
          i)))

(define (rmpi-alltoall comm outvec)
  (match-define (rmpi-comm id cnt chs) comm)
  (define-values (v! vr mkv vcopy)
    (cond 
      [(vector? outvec) (values vector-set! vector-ref make-vector vector-copy)]
      [(fxvector? outvec) (values fxvector-set! fxvector-ref make-fxvector fxvector-copy)]
      [(flvector? outvec) (values flvector-set! flvector-ref make-flvector flvector-copy)]
      [else (error (format "Unrecognized type of vector ~a" outvec))]))

  (define invec (mkv cnt))
  (v! invec id (vr outvec id)) 

  (define n
    (let loop ([i 0])
      (if (>= (arithmetic-shift 1 i) cnt)
        i
        (loop (add1 i)))))

  (for ([i (in-range 1 (arithmetic-shift 1 n))])
    (define peer-id (bitwise-xor id i))
    (when (< peer-id cnt)
      (if (> id peer-id)
        (begin
          (place-channel-put (vector-ref chs peer-id) (vr outvec peer-id))
          (v! invec peer-id (place-channel-get (vector-ref chs peer-id))))
        (begin
          (v! invec peer-id (place-channel-get (vector-ref chs peer-id)))
          (place-channel-put (vector-ref chs peer-id) (vr outvec peer-id))))))

   invec)


(define (rmpi-alltoallv comm outvec send-count send-displ invec recv-count recv-displ)
  (match-define (rmpi-comm real-id cnt chs) comm)
  (define-values (v! vr mk-v in-v)
    (cond 
      [(vector? outvec) (values vector-set! vector-ref make-vector in-vector)]
      [(fxvector? outvec) (values fxvector-set! fxvector-ref make-fxvector in-fxvector)]
      [(flvector? outvec) (values flvector-set! flvector-ref make-flvector in-flvector)]
      [else (error (format "Unrecognized type of vector ~a" outvec))]))

  ;; convert from outvec to vector of outvectors 
  (define outvv
    (for/vector #:length (fxvector-length send-count) ([i (in-fxvector send-count)]
                                                       [d (in-fxvector send-displ)])
      (define vv (mk-v i))
      (for ([ii (in-range d (fx+ d i))]
            [iii (in-naturals)])
        (v! vv iii (vr outvec ii)))
      vv))

  (define invv (rmpi-alltoall comm outvv))

  ;; convert form vector of invectors to invector
  (for ([v (in-vector invv)]
        [i (in-fxvector recv-count)]
        [d (in-fxvector recv-displ)])
    (for ([x (in-v v)]
          [ii (in-range d (fx+ d i))])
      (v! invec ii x)))

  invec)

(define (rmpi-comm-split comm color key)
  (match-define (rmpi-comm id cnt chs) comm)
  (define r (rmpi-allgather comm (vector id color key)))
  (match-define (vector nid nc nk) (vector-ref r id))
  (define ncl 
    (for/fold ([nl null]) ([x (in-vector r)])
      (cond 
        [(= nc (vector-ref x 1))
         (cons x nl)]
        [else nl])))
  (define nchsl (length ncl))
  (define nchs (make-vector nchsl))
  (for ([x ncl])
    (match-define (vector cid cc ck) x)
    (when (>= ck nchsl)
      (error "new key value ~a is >= new comm size ~a" ck ncl))
    (when (not (= 0 (vector-ref nchs ck)))
      (error "duplicate key value ~a" ck))
    (cond
        [(= nid cid)
         (vector-set! nchs ck null)]
        [else
         (vector-set! nchs ck (vector-ref chs cid))]))

  (define ncomm (rmpi-comm nk nchsl nchs))
  #;(list id ncl ncomm)
  ncomm)

(module+ bcast-print-test
  (rmpi-broadcast (rmpi-comm 0 8 (vector 0 1 2 3 4 5 6 7)) 0 "Hi")
  (rmpi-broadcast (rmpi-comm 3 8 (vector 0 1 2 3 4 5 6 7)) 0)
  (rmpi-broadcast (rmpi-comm 0 8 (vector 0 1 2 3 4 5 6 7)) 3)
  )

(module+ reduce-print-test
  (rmpi-reduce (rmpi-comm 0 8 (vector 0 1 2 3 4 5 6 7)) 0 + 7)
  (rmpi-reduce (rmpi-comm 3 8 (vector 0 1 2 3 4 5 6 7)) 0 + 7)
  (rmpi-reduce (rmpi-comm 0 8 (vector 0 1 2 3 4 5 6 7)) 3 + 7)
  )

;Don't use eli-tester in the core, use rackunit
#;(module+ test
  (require tests/eli-tester)
  (test 
    (partit 10 3 0) => (values 0 4)
    (partit 10 3 1) => (values 4 3)
    (partit 10 3 2) => (values 7 3)))
