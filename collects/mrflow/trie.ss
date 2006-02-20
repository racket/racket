
(module trie (lib "mrflow.ss" "mrflow")
  (require (prefix list: (lib "list.ss"))
           (prefix cst: "constants.ss")
           
           "dfa.ss"
           "types.ss"
           "util.ss")
  
  ;; DFA Tries - Allows for testing of a DFA being previously hashconsed
  ;;             in Theta(|DFA|) time. 
  (provide (struct trie ())
           add-dfa-states
           dfa-present?)
  
  ; DFA states are analogous to letters and at a node we have map of handles
  ; indexed by the DFA representative (the handle of the last DFA state in a DFA
  ; canonically ordered by minimization).
  ;
  ; Two equivalent (minimal, strongly connected) DFAs will yield the same
  ; canonically ordered DFAs regardless of the start state picked.
  ;
  ; An association list and hash-table are used to store the maps, but perhaps
  ; there is a better choice of data structures.
  (define-struct trie (dfa-representative->handle dfa-state->trie))
  (set! make-trie
	(let ([old-make-trie make-trie])
	  (lambda ()
	    (old-make-trie '() (make-hash-table 'equal)))))
  
  ; Get the trie on the edge labeled by the DFA state
  (define/contract get-trie-child (trie? state? . -> . (or/c trie? false/c))
    (lambda (trie letter)
      (hash-table-get (trie-dfa-state->trie trie) letter cst:thunk-false)))
  
  ; Each DFA state added to the trie must map to a unique handle. 
  (define/contract add-trie-state-handle!
    (trie? handle? handle? . ->d .
           (lambda (trie representative-handle state-handle)
             (let ([dfa->handle (trie-dfa-representative->handle trie)])
               (when (assq representative-handle dfa->handle)
                 (error 'add-trie-state-handle!
                        "Mapping ~a to ~a, but trie already has mapping from DFA representative ~a to handle ~a"
                        representative-handle state-handle
                        representative-handle (cdr (assq representative-handle dfa->handle))))
               trie?)))
    (lambda (trie representative-handle state-handle)
      (let ([dfa->handle (trie-dfa-representative->handle trie)])
        (set-trie-dfa-representative->handle! trie (cons (cons representative-handle state-handle) dfa->handle))
        trie)))
  
  (define/contract get-state-handle (trie? handle? . -> . handle?)
    (lambda (trie representative-handle)
      (let ([dfa-representative->handle (trie-dfa-representative->handle trie)])
        (cdr (assq representative-handle dfa-representative->handle)))))
  
  (define/contract get-handle-from-representative
    (trie? . ->d .
           (lambda (trie)
             (let ([dfa->handle (trie-dfa-representative->handle trie)])
               (unless (length-one? dfa->handle)
                 (error 'get-handle-from-representative
                        "~a (!= 1) representatives present: ~a" (length dfa->handle) dfa->handle))
               (unless (= (caar dfa->handle) (cdar dfa->handle))
                 (error 'get-handle-from-representative "Representative handle ~a not equal to representative handle ~a"
                        (caar dfa->handle) (cdar dfa->handle)))
               handle?)))
    (lambda (trie)
      (caar (trie-dfa-representative->handle trie))))
  
  ; Return a handle of the DFAs start state if the DFA has already
  ; been hasconsed.  For each of the ordered DFA states we descend one
  ; level in the trie until we reach the last state (the
  ; representative).  As we are descending we note which of the tries
  ; contains the start state.  Getting the representative handle, we
  ; can lookup the handle of the start state in this noted trie.
  (define/contract dfa-present?
    (trie? (nonempty-list-of? state?) . -> . (or/c false/c (listof handle?)))
    (lambda (trie nstates) 
      (let/ec return-with
        (let* ([rev-tries (list:foldl (lambda (state tries)
                                        (let ([trie (get-trie-child (car tries) state)])
                                          (if trie
                                              (cons trie tries)
                                              (return-with #f))))
                                      (list trie)
                                      nstates)]
               [rep-handle (get-handle-from-representative (car rev-tries))])
          ;; get the handles for each state, in reverse order from the (reversed) list of tries
          (list:foldr (lambda (trie states) (cons (get-state-handle trie rep-handle) states))
                      '() (cdr (reverse rev-tries)))))))
  
  ; Add a list of DFA states and their corresponding handles to the trie
  (define/contract add-dfa-states
    (trie? (nonempty-list-of? state?) (listof handle?) . ->d .
           (lambda (trie states handles)
             (unless (= (length states) (length handles))
               (error 'add "length of list of types ~a != length of DFA handle list ~a"
                      (length states) (length handles)))
             (lambda (_)
               (let loop ([trie trie] [states states])
                 (if (null? states)
                     (begin
                       (unless (hash-table-empty? (trie-dfa-state->trie trie))
                         (error 'add-dfa-states "Representative node has a child node"))
                       (unless (length-one? (trie-dfa-representative->handle trie))
                         (error 'add-dfa-states "Representative node has more than one representative handle")))
                     (loop (get-trie-child trie (car states)) (cdr states)))))))
    (lambda (trie states handles)
      (let ([add-child (lambda (trie letter representative-handle dfa-handle)
                         (add-trie-state-handle!
                          (if (get-trie-child trie letter) (get-trie-child trie letter)
                              (let ([child-trie (make-trie)])
                                (hash-table-put! (trie-dfa-state->trie trie) letter child-trie)
                                child-trie))
                          representative-handle dfa-handle))]
            [representative-handle (list-ref handles (sub1 (length handles)))])
        (let loop ([trie trie] [states states] [handles handles])
          (unless (null? states)
            (loop (add-child trie (car states) representative-handle (car handles))
                  (cdr states)
                  (cdr handles)))))))
  
  ) ;; end module trie