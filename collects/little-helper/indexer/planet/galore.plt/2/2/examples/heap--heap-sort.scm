; heap-sort.scm  -- Jens Axel Søgaard

(require (planet "heap.scm" ("soegaard" "galore.plt" 2 1)))

(define (heap-sort l)
  (heap->list (list->heap l)))

(define (list->heap l)
  (foldl insert (empty) l))

(define (heap->list H)
  (define (loop H l)
    (if (empty? H)
        (reverse l)
        (loop (delete-min H) (cons (find-min H) l))))
  (loop H '()))

(heap-sort '(3 1 4 1 5 9))


;;; Alternative using srfi-42 (and srfi-67)

(require (lib "42.ss" "srfi")
         (lib "67.ss" "srfi"))

(define (heap-sort l)
  (heap->list (list->heap l)))

(define (list->heap l)
  (heap-ec default-compare (: x l) x))

(define (heap->list H)
  (list-ec (: x H) x))

(heap-sort '(3 1 4 1 5 9))
