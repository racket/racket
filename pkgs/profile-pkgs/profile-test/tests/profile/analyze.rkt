#lang racket/base

(require tests/eli-tester profile/structs profile/analyzer
         racket/match racket/list)

(define A '(A . #f))
(define B '(B . #f))
(define C '(C . #f))

(define (analyze cpu+lists)
  (profile->sexpr
   (analyze-samples
    (cons (car cpu+lists)
          (map (lambda (x) (append (take x 2) (reverse (drop x 2))))
               (reverse (cdr cpu+lists)))))))

(define (profile->sexpr prof)
  (define (node-id* node)
    (or (node-id node) (if (node-src node) '??? '*)))
  (define (edges->sexprs node get get-time)
    (for/list ([edge (get node)])
      `(,(node-id* (edge-caller edge)) -> ,(node-id* (edge-callee edge))
        time=  ,(get-time edge)
        total= ,(edge-total edge))))
  (define (node->sexpr node)
    `(,(node-id* node)
          total=   ,(node-total node)
          self=    ,(node-self node)
          callers: ,@(edges->sexprs node node-callers edge-caller-time)
          callees: ,@(edges->sexprs node node-callees edge-callee-time)
          threads= ,(node-thread-ids node)))
  `(total=   ,(profile-total-time    prof)
    samples= ,(profile-sample-number prof)
    cpu=     ,(profile-cpu-time      prof)
    thread-times= ,(profile-thread-times prof)
    ,@(map node->sexpr (cons (profile-*-node prof) (profile-nodes prof)))))

(provide analyze-tests)
(module+ main (analyze-tests))
(define (analyze-tests)
  (test

   (match (analyze `(10
                     [0 0 ,A]
                     [0 1 ,A]))
     [`(total= 2 samples= 2 cpu= 10 thread-times= ([0 . 2])
        [* total= 2 self= 0
           callers: [A -> * time= 2 total= 2]
           callees: [* -> A time= 2 total= 2]
           threads= ()]
        [A total= 2 self= 2
           callers: [* -> A time= 2 total= 2]
           callees: [A -> * time= 2 total= 2]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   ;; demonstrates different edge-caller/lee-times
   (match (analyze `(10
                     [0 0 ,A ,B ,A]
                     [0 1 ,A ,B ,A]))
     [`(total= 2 samples= 2 cpu= 10 thread-times= ([0 . 2])
        [* total= 2 self= 0
           callers: [A -> * time= 2 total= 2]
           callees: [* -> A time= 2 total= 2]
           threads= ()]
        [A total= 2 self= 2
           callers: [B -> A time= 2/2 total= 2]
                    [* -> A time= 2/2 total= 2]
           callees: [A -> B time= 2/2 total= 2]
                    [A -> * time= 2/2 total= 2]
           threads= (0)]
        [B total= 2 self= 0
           callers: [A -> B time= 2 total= 2]
           callees: [B -> A time= 2 total= 2]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   (match (analyze `(10
                     [0 0 ,A ,B ,A]
                     [0 1 ,A ,C ,A]
                     [0 2 ,A ,C ,A]
                     [0 3 ,A ,C ,A]))
     [`(total= 4 samples= 4 cpu= 10 thread-times= ([0 . 4])
        [* total= 4 self= 0
           callers: [A -> * time= 4 total= 4]
           callees: [* -> A time= 4 total= 4]
           threads= ()]
        [A total= 4 self= 4
           callers: [* -> A time= 4/2 total= 4]
                    [C -> A time= 3/2 total= 3]
                    [B -> A time= 1/2 total= 1]
           callees: [A -> * time= 4/2 total= 4]
                    [A -> C time= 3/2 total= 3]
                    [A -> B time= 1/2 total= 1]
           threads= (0)]
        [C total= 3 self= 0
           callers: [A -> C time= 3 total= 3]
           callees: [C -> A time= 3 total= 3]
           threads= (0)]
        [B total= 1 self= 0
           callers: [A -> B time= 1 total= 1]
           callees: [B -> A time= 1 total= 1]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   ))
