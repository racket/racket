#lang racket/base

(require racket/place/distributed
         racket/match
         racket/list
         racket/place
         racket/class)

(provide rmpi-init
         rmpi-send
         rmpi-recv
         rmpi-broadcast
         rmpi-reduce
         rmpi-allreduce
         rmpi-barrier
         rmpi-id
         rmpi-cnt
         rmpi-partition
         rmpi-build-default-config
         rmpi-launch
         rmpi-finish)

(struct rmpi-comm (id cnt channels) #:transparent)

(define (rmpi-id comm) (rmpi-comm-id comm))
(define (rmpi-cnt comm) (rmpi-comm-cnt comm))
(define (rmpi-send comm dest val) (place-channel-put (vector-ref (rmpi-comm-channels comm) dest) val))
(define (rmpi-recv comm src) (place-channel-get (vector-ref (rmpi-comm-channels comm) src)))

(define (rmpi-init ch)
  (define tc (new named-place-typed-channel% [ch ch]))
  (match-define (list (list 'mpi-id id config) return-ch) (tc-get 'mpi-id tc))
  (match-define (list (list 'args args) src-ch) (tc-get 'args tc))
  (define mpi-comm-vector
    (for/vector #:length (length config) ([c config])
      (match-define (list dest dest-port dest-name dest-id) c)
      (cond
        [(< id dest-id)
         ;(printf/f "sending connect to dest-id ~a from id ~a over ~a" dest-id id ch)
         (send-new-place-channel-to-named-dest ch id (list dest dest-port dest-name))]
        [else null])))
  (for ([i (length config)])
    (cond
      [(> id i)
        (match-define (list (list 'new-place-channel src-id) src-ch) (tc-get 'new-place-channel tc))
        ;(printf/f "received connect from id ~a ~a" src-id src-ch)
        (vector-set! mpi-comm-vector src-id src-ch)]
      [else null]))
  (values
    (rmpi-comm id (length config) mpi-comm-vector)
    args
    tc
    ))


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
                    (define real-peer-id (modulo (+ peer-id offset) cnt))
                    ;(printf "RECV ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset val)
                    (place-channel-get (vector-ref chs real-peer-id))
                    ]
                   [else
                    (define peer-id (+ id round))
                    (define real-peer-id (modulo (+ peer-id offset) cnt))
                    ;(printf "SEND ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset val)
                    (place-channel-put (vector-ref chs real-peer-id) val)
                    val])]
               [else val]))]
         [else val]))]))

(define (fancy-reducer op recv-val val)
  (cond 
    [(number? recv-val)
     (op recv-val val)]
    [(vector? recv-val)
     (for/vector #:length (vector-length recv-val)
                 ([a (in-vector recv-val)]
                  [b (in-vector val)])
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
                (define real-peer-id (convert peer-id))
                ;(printf "SEND ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset val)
                (place-channel-put (vector-ref chs real-peer-id) val)
                val
                ]
               [else
                (define peer-id (+ id round))
                (define real-peer-id (convert peer-id))
                ;(printf "RECV ~a ~a ~a ~a ~a ~a ~a\n" round real-id id peer-id real-peer-id offset val)
                (define recv-val (place-channel-get (vector-ref chs real-peer-id)))
                ;(define recv-val val)
                (fancy-reducer op recv-val val)])]))]
      [else val])))

(define (rmpi-barrier comm)
  (rmpi-reduce comm 0 + 1)
  (rmpi-broadcast comm 0 1))

(define (rmpi-allreduce comm op val)
  (define rv (rmpi-reduce comm 0 op val))
  (rmpi-broadcast comm 0 rv))

(define (partit num cnt id)
  (define-values (quo rem) (quotient/remainder num cnt))
  (values (+ (* id quo) (if (< id rem) id 0))
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

(define (rmpi-launch default config)
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
            (cond 
             [(pair? keys)
              (cond
                [(lookup-config-value rest (car keys)) => (lambda (x) 
                  (loop (cdr keys)
                        (cons (string->keyword (car keys)) k)
                        (cons x v)))]
                [else
                  (loop (cdr keys) k v)])]
             [else
               (values k v)])))
;        (printf/f "~a\n" (list k v (list host)))
        (list k v (list host)))))

  (for ([n nodes]
        [c config])
    (match-define (list-rest host port name id rest) c)
    (supervise-named-dynamic-place-at n 
                                      name 
                                      (lookup-config-value rest "mpi-module")
                                      (lookup-config-value rest "mpi-func")))

  (define-values (mrth ch)
    (start-message-router/thread
      #:nodes nodes))

  (for ([c config])
    (match-define (list-rest host port name id rest) c)
    (define npch (mr-connect-to ch (list host port) name))
    (*channel-put npch (list 'mpi-id id config))
    (*channel-put npch (list 'args (or (lookup-config-value rest "mpi-args") null))))

  (for/first ([c config])
    (match-define (list-rest host port name id rest) c)
    (define npch (mr-connect-to ch (list host port) name))
    (*channel-put npch (list 'done?))
    ;Wait for 'done message from mpi node id 0
    (*channel-get npch)))


(define (rmpi-finish comm tc)
  (when (= (rmpi-id comm) 0)
        (place-channel-put (second (tc-get 'done? tc)) 'done)))

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

#;
(module+ test
  (require tests/eli-tester)
  (test 
    (partit 10 3 0) => (values 0 4)
    (partit 10 3 1) => (values 3 3)
    (partit 10 3 2) => (values 6 3)))
