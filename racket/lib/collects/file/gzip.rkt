#|
/* deflate.c -- compress data using the deflation algorithm
 * Copyright (C) 1992-1993 Jean-loup Gailly
 */
|#
;; Taken from the gzip source distribution
;; Translated directly from C (obviously) by Matthew, July 2000

;; *** The original version that this code was taken from was
;; distributed with a GPL license, but later the code (later version of
;; it) was also included in zlib, which is distributed with an
;; LGPL-compatible license.  I (Eli Barzilay) have tried to contact the
;; author, but no reply yet.

(module deflate mzscheme

  (provide deflate gzip-through-ports gzip)

  (require mzlib/unit200)

  (define (vector-ref* v i)
    (let ([r (vector-ref v i)])
      (if (<= 0 r 255) r (error 'vector-ref "BOOM: ~s" r))))

  (define (vector-set!* v i n)
    (if (<= 0 n 255) (vector-set! v i n) (error 'vector-ref "BOOM!: ~s" n)))

  (define-syntax INSERT_STRING
    (syntax-rules ()
      [(_ s match_head UPDATE_HASH window-vec head-vec prev-vec ins_h)
       (begin (UPDATE_HASH (bytes-ref window-vec (+ s MIN_MATCH-1)))
              (let ([mh (vector-ref head-vec (+ ins_h head-vec-delta))])
                (set! match_head mh)
                (vector-set! prev-vec (bitwise-and s  WMASK) mh))
              (vector-set! head-vec (+ head-vec-delta ins_h) s))]))

  (define-syntax pqremove
    (syntax-rules ()
      [(_ tree top heap heap_len SMALLEST)
       (begin (set! top (vector-ref heap SMALLEST))
              (vector-set! heap SMALLEST (vector-ref heap heap_len))
              (set! heap_len (sub1 heap_len))
              (pqdownheap tree SMALLEST))]))

  (define-syntax DEBUG (lambda (stx) #'(void)))

  (define-syntax Assert (lambda (stx) #'(void)))

  (define-syntax for
    (syntax-rules (:= then do)
      [(for n := start < end do body ...)
       (for n := start then add1 < end do body ...)]
      [(for n := start then next < end do body ...)
       (let ([endval end])
         (let loop ([n start])
           (when (< n endval) body ... (loop (next n)))))]))

  (define-struct gzbytes (bytes offset))
  (define (gzbytes-ref v o)
    (bytes-ref (gzbytes-bytes v) (+ (gzbytes-offset v) o)))
  (define (gzbytes-set! v o x)
    (bytes-set! (gzbytes-bytes v) (+ (gzbytes-offset v) o) x))
  (define (gzbytes+ v o)
    (make-gzbytes (gzbytes-bytes v) (+ (gzbytes-offset v) o)))

  (define (Trace stderr str . args)
    (apply eprintf str args))
  (define Tracevv Trace)
  (define Tracev Trace)
  (define (Tracec test . args)
    (when test (apply Trace args)))
  (define Tracecv Tracec)
  (define stderr 'sdterr)

#|
/*
 *  PURPOSE
 *
 *      Identify new text as repetitions of old text within a fixed-
 *      length sliding window trailing behind the new text.
 *
 *  DISCUSSION
 *
 *      The "deflation" process depends on being able to identify portions
 *      of the input text which are identical to earlier input (within a
 *      sliding window trailing behind the input currently being processed).
 *
 *      The most straightforward technique turns out to be the fastest for
 *      most input files: try all possible matches and select the longest.
 *      The key feature of this algorithm is that insertions into the string
 *      dictionary are very simple and thus fast, and deletions are avoided
 *      completely. Insertions are performed at each input character, whereas
 *      string matches are performed only when the previous match ends. So it
 *      is preferable to spend more time in matches to allow very fast string
 *      insertions and avoid deletions. The matching algorithm for small
 *      strings is inspired from that of Rabin & Karp. A brute force approach
 *      is used to find longer strings when a small match has been found.
 *      A similar algorithm is used in comic (by Jan-Mark Wams) and freeze
 *      (by Leonid Broukhis).
 *         A previous version of this file used a more sophisticated algorithm
 *      (by Fiala and Greene) which is guaranteed to run in linear amortized
 *      time, but has a larger average cost, uses more memory and is patented.
 *      However the F&G algorithm may be faster for some highly redundant
 *      files if the parameter max_chain_length (described below) is too large.
 *
 *  ACKNOWLEDGEMENTS
 *
 *      The idea of lazy evaluation of matches is due to Jan-Mark Wams, and
 *      I found it in 'freeze' written by Leonid Broukhis.
 *      Thanks to many info-zippers for bug reports and testing.
 *
 *  REFERENCES
 *
 *      APPNOTE.TXT documentation file in PKZIP 1.93a distribution.
 *
 *      A description of the Rabin and Karp algorithm is given in the book
 *         "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
 *
 *      Fiala,E.R., and Greene,D.H.
 *         Data Compression with Finite Windows, Comm.ACM, 32,4 (1989) 490-595
 *
 *  INTERFACE
 *
 *      void lm_init (int pack_level, ush *flags)
 *          Initialize the "longest match" routines for a new file
 *
 *      ulg deflate (void)
 *          Processes a new input file and return its compressed length. Sets
 *          the compressed length, crc, deflate flags and internal file
 *          attributes.
 */

|#

 (define LEVEL 6)

 (define OUTBUFSIZ  16384);;   /* output buffer size */
 (define INBUFSIZ  #x8000);;   /* input buffer size */
 (define INBUF_EXTRA  64)

 (define WSIZE #x8000) ;; /* window size--must be a power of two, and */
 ;;                       /*  at least 32K for zip's deflate method */

 (define MIN_MATCH  3)
 (define MIN_MATCH-1 (- MIN_MATCH 1))
 (define MAX_MATCH  258)
 ;; /* The minimum and maximum match lengths */

 (define MIN_LOOKAHEAD (+ MAX_MATCH MIN_MATCH 1))
 ;; /* Minimum amount of lookahead, except at the end of the input file.
 ;;  * See for comments about the MIN_MATCH+1.
 ;; */

 (define MAX_DIST  (- WSIZE MIN_LOOKAHEAD))
 ;; /* In order to simplify the code, particularly on 16 bit machines, match
 ;;  * distances are limited to MAX_DIST instead of WSIZE.
 ;;  */

 (define HASH_BITS  15)
 (define BITS 16)

 (define << arithmetic-shift)
 (define (>> x y) (arithmetic-shift x (- y)))
 (define EOF-const -1)

 ;; /* To save space (see unlzw.c), we overlay prev+head with tab_prefix and
 ;;  * window with tab_suffix. Check that we can do this:
 ;;  */
 (Assert
  (when (> (<< WSIZE 1) (<< 1 BITS))
    (error "cannot overlay window with tab_suffix and prev with tab_prefix0")))
 (Assert
  (when (> HASH_BITS (- BITS 1))
    (error "cannot overlay head with tab_prefix1")))

 (define HASH_SIZE (<< 1 HASH_BITS))
 (define HASH_MASK (- HASH_SIZE 1))
 (define WMASK     (- WSIZE 1))
 ;; /* HASH_SIZE and WSIZE must be powers of two */

 (define NIL 0)
 ;; /* Tail of hash chains */

 (define FAST 4)
 (define SLOW 2)
 ;; /* speed options for the general purpose bit flag */

 (define TOO_FAR 4096)
 ;; /* Matches of length 3 are discarded if their distance exceeds TOO_FAR */

 (define bits_sent 0)
 (define (isgraph c) #t)

(define head-vec-delta WSIZE)

 ;; The gzip code wasn't defined for threads (or even to be
 ;;  multiply invoked), so we pack it up into a unit to
 ;;  invoke each time we need it.

 (define code
   (unit
     (import)
     (export)


;; /* ===========================================================================
;;  * Local data used by the "longest match" routines.
;;  */

(define real-table (make-vector (<< 1 BITS) 0))

(define prev-vec real-table)
(define head-vec real-table)

;;  /* DECLARE(uch, window, 2L*WSIZE); */
;;  /* Sliding window. Input bytes are read into the second half of the window,
;;   * and move to the first half later to keep a dictionary of at least WSIZE
;;   * bytes. With this organization, matches are limited to a distance of
;;   * WSIZE-MAX_MATCH bytes, but this ensures that IO is always
;;   * performed with a length multiple of the block size. Also, it limits
;;   * the window size to 64K, which is quite useful on MSDOS.
;;   * To do: limit the window size to WSIZE+BSZ if SMALL_MEM (the code would
;;   * be less efficient).
;;   */

;;  /* DECLARE(Pos, prev, WSIZE); */
;;  /* Link to older string with same hash index. To limit the size of this
;;   * array to 64K, this link is maintained only for the last 32K strings.
;;   * An index in this array is thus a window index modulo 32K.
;;   */

;;  /* DECLARE(Pos, head, 1<<HASH_BITS); */
;;  /* Heads of the hash chains or NIL. */

(define window_size (* 2 WSIZE))
;;  /* window size, 2*WSIZE
;;   */
(define window-vec (make-bytes window_size 0))
(define window (make-gzbytes window-vec 0))

(define block_start 0)
;;  /* window position at the beginning of the current output block. Gets
;;   * negative when the window is moved backwards.
;;   */

(define ins_h 0) ;; /* hash index of string to be inserted */

(define H_SHIFT  (quotient (+ HASH_BITS MIN_MATCH-1) MIN_MATCH))
;; /* Number of bits by which ins_h and del_h must be shifted at each
;;  * input step. It must be such that after MIN_MATCH steps, the oldest
;;  * byte no longer takes part in the hash key, that is:
;;  *   H_SHIFT * MIN_MATCH >= HASH_BITS
;;  */

(define prev_length 0)
;; /* Length of the best match at previous step. Matches not greater than this
;;  * are discarded. This is used in the lazy match evaluation.
;; */

(define strstart 0)    ;; /* start of string to insert */
(define match_start 0) ;; /* start of matching string */
(define eofile #f)     ;; /* flag set at end of input file */
(define lookahead 0)   ;; /* number of valid bytes ahead in window */

(define max_chain_length 0)
;; /* To speed up deflation, hash chains are never searched beyond this length.
;;  * A higher limit improves compression ratio but degrades the speed.
;;  */

(define max_lazy_match 0)
;; /* Attempt to find a better match only when the current match is strictly
;;  * smaller than this value. This mechanism is used only for compression
;;  * levels >= 4.
;;  */

(define (max_insert_length)  max_lazy_match)
;; /* Insert new strings in the hash table only if the match length
;;  * is not greater than this length. This saves time but degrades compression.
;;  * max_insert_length is used only for compression levels <= 3.
;;  */

(define good_match 0)
;; /* Use a faster search when the previous match is longer than this */


;; /* Values for max_lazy_match, good_match and max_chain_length, depending on
;;  * the desired pack level (0..9). The values given below have been tuned to
;;  * exclude worst case performance for pathological files. Better values may be
;;  * found for specific files.
;;  */

(define-struct config
  (good_length ;; /* reduce lazy search above this match length */
   max_lazy    ;; /* do not perform lazy search above this match length */
   nice_length ;; /* quit search above this match length */
   max_chain))

(define nice_match MAX_MATCH)
;; /* Stop searching when current match exceeds this */

(define configuration_table
  (vector
   ;; /* good lazy nice chain */
   (make-config 0 0 0 0)           ;; /* 0 - store only */
   (make-config 4 4 8 4)           ;; /* 1 - maximum speed, no lazy matches */
   (make-config 4 5 16 8)          ;; /* 2 */
   (make-config 4 6 32 32)         ;; /* 3 */

   (make-config 4 4 16 16)         ;; /* 4 - lazy matches */
   (make-config 8 16 32 32)        ;; /* 5 */
   (make-config 8 16 128 128)      ;; /* 6 */
   (make-config 8 32 128 256)      ;; /* 7 */
   (make-config 32 128 258 1024)   ;; /* 8 */
   (make-config 32 258 258 4096))) ;; /* 9 - maximum compression */

;; /* Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >= 4
;;  * For deflate_fast() (levels <= 3) good is ignored and lazy has a different
;; * meaning.
;; */

;; /* ===========================================================================
;;  * Update a hash value with the given input byte
;;  * IN  assertion: all calls to to UPDATE_HASH are made with consecutive
;;  *    input characters, so that a running hash key can be computed from the
;;  *    previous key instead of complete recalculation each time.
;;  */
(define (UPDATE_HASH c)
  (set! ins_h (bitwise-and (bitwise-xor (<< ins_h H_SHIFT) c) HASH_MASK)))

;; /* ===========================================================================
;;  * Insert string s in the dictionary and set match_head to the previous head
;;  * of the hash chain (the most recent string with same hash key). Return
;;  * the previous length of the hash chain.
;;  * IN  assertion: all calls to to INSERT_STRING are made with consecutive
;;  *    input characters and the first MIN_MATCH bytes of s are valid
;;  *    (except for the last MIN_MATCH-1 bytes of the input file).
;;  */
;; (define-macro INSERT_STRING <see above>)

;; /* ===========================================================================
;;  * Initialize the "longest match" routines for a new file
;; */
(define (lm_init pack_level)
  ;; int pack_level; /* 0: store, 1: best speed, 9: best compression */

  (when (or (< pack_level 1)
            (> pack_level 9))
    (error "bad pack level"))

  ;; /* Initialize the hash table. */
  (for i := head-vec-delta < (+ head-vec-delta HASH_SIZE) do
    (vector-set! head-vec i 0))

  ;; /* prev will be initialized on the fly */

  ;; /* Set the default configuration parameters:
  ;; */
  (set! max_lazy_match (config-max_lazy (vector-ref configuration_table pack_level)))
  (set! good_match (config-good_length (vector-ref configuration_table pack_level)))
  (set! nice_match (config-nice_length (vector-ref configuration_table pack_level)))
  (set! max_chain_length (config-max_chain (vector-ref configuration_table pack_level)))

  (let ([flag (cond
               [(= pack_level 1) FAST]
               [(= pack_level 9) SLOW]
               [else 0])])
    ;; /* ??? reduce max_chain_length for binary files */

    (set! strstart 0)
    (set! block_start 0)

    (set! lookahead (read_buf 0 (* 2 WSIZE)))

    (if (or (= lookahead 0) (= lookahead EOF-const))
        (begin
          (set! eofile #t)
          (set! lookahead 0))
        (begin
          (set! eofile #f)
          ;; /* Make sure that we always have enough lookahead. This is important
          ;;  * if input comes from a device such as a tty.
          ;;  */
          (let loop ()
            (when (and (< lookahead MIN_LOOKAHEAD)
                       (not eofile))
              (fill_window)))

          (set! ins_h 0)
          (for j := 0 < MIN_MATCH-1 do (UPDATE_HASH (bytes-ref window-vec j)))
          (DEBUG (Trace stderr "hash init: ~a\n" ins_h))
          ;; /* If lookahead < MIN_MATCH, ins_h is garbage, but this is
          ;;  * not important since only literal bytes will be emitted.
          ;;  */
          ))

    flag))

;; /* ===========================================================================
;;  * Set match_start to the longest match starting at the given string and
;;  * return its length. Matches shorter or equal to prev_length are discarded,
;;  * in which case the result is equal to prev_length and match_start is
;;  * garbage.
;;  * IN assertions: cur_match is the head of the hash chain for the current
;;  *   string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1
;;  */

;; Since longest_match is not called recursively or in multiple threads, we can
;;  make this C-derived code have more C-like allocation by lifting out its local
;;  variables.

(define longest_match
  (let ((cur_match 0)
        (chain_length 0)
        (scanpos 0)
        (matchpos 0)
        (len 0)
        (best_len 0)
        (limit NIL)
        (strendpos 0)
        (scan_end1 0)
        (scan_end 0))

    (define (longest_match _cur_match)
      ;; IPos cur_match; /* current match */

      (set! cur_match _cur_match)

      (set! chain_length max_chain_length)   ;;   /* max hash chain length */
      (set! scanpos strstart) ;;   /* current string */
      (set! matchpos 0) ;; /* matched string */
      (set! len 0) ;; /* length of current match */
      (set! best_len prev_length) ;; /* best match length so far */
      (set! limit (if (> strstart MAX_DIST)
                      (- strstart MAX_DIST)
                      NIL))
      ;; /* Stop when cur_match becomes <= limit. To simplify the code,
      ;;  * we prevent matches with the string of window index 0.
      ;;  */

      ;; /* The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
      ;;  * It is easy to get rid of this optimization if necessary.
      ;;  */
      ;; #if HASH_BITS < 8 || MAX_MATCH != 258
      ;;   error: Code too clever
      ;; #endif

      (set! strendpos (+ strstart MAX_MATCH))
      (set! scan_end1 (bytes-ref window-vec (+ scanpos best_len -1)))
      (set! scan_end (bytes-ref window-vec (+ scanpos best_len)))

      ;; /* Do not waste too much time if we already have a good match: */
      (when (>= prev_length good_match)
        (set! chain_length (>> chain_length 2)))

      (Assert
       (unless (<= strstart (- window_size MIN_LOOKAHEAD))
         (error "insufficient lookahead")))

      (longest_match-loop)

      best_len)

    (define (continue)
      (set! cur_match (vector-ref prev-vec (bitwise-and cur_match WMASK)))
      (when (and (> cur_match limit)
                 (begin
                   (set! chain_length (sub1 chain_length))
                   (positive? chain_length)))
        (longest_match-loop)))
    (define (*++scan)
      (set! scanpos (add1 scanpos))
      (and (scanpos . < . window_size) ; the original C code can read past the end of the buffer
           (bytes-ref window-vec scanpos)))
    (define (*++match)
      (set! matchpos (add1 matchpos))
      (bytes-ref window-vec matchpos))

    (define (match-eight)
      (when (and (eq? (*++scan) (*++match)) (eq? (*++scan) (*++match))
                 (eq? (*++scan) (*++match)) (eq? (*++scan) (*++match))
                 (eq? (*++scan) (*++match)) (eq? (*++scan) (*++match))
                 (eq? (*++scan) (*++match)) (eq? (*++scan) (*++match))
                 (< scanpos strendpos))
        (match-eight)))

    (define (longest_match-loop)

      (Assert
       (unless (< cur_match strstart)
         (error "no future")))

      (set! matchpos cur_match)

      ;; /* Skip to next match if the match length cannot increase
      ;;  * or if the match length is less than 2:
      ;;  */

      (if (or (not (eq? (bytes-ref window-vec (+ matchpos best_len)) scan_end))
              (not (eq? (bytes-ref window-vec (+ matchpos best_len -1)) scan_end1))
              (not (eq? (bytes-ref window-vec matchpos) (bytes-ref window-vec scanpos)))
              (not (eq? (begin (set! matchpos (add1 matchpos))
                               (bytes-ref window-vec matchpos))
                        (bytes-ref window-vec (add1 scanpos)))))
          (continue)

          (begin
            ;; /* The check at best_len-1 can be removed because it will be made
            ;;  * again later. (This heuristic is not always a win.)
            ;;  * It is not necessary to compare scan[2] and match[2] since they
            ;;  * are always equal when the other bytes match, given that
            ;;  * the hash keys are equal and that HASH_BITS >= 8.
            ;;  */
            (set! scanpos (+ scanpos 2))
            (set! matchpos (+ matchpos 1))

            ;; /* We check for insufficient lookahead only every 8th comparison;
            ;; * the 256th check will be made at strstart+258.
            ;; */
            (match-eight)

            (set! len (- MAX_MATCH (- strendpos scanpos)))
            (set! scanpos (+ strendpos (- MAX_MATCH)))
            (DEBUG (Trace stderr "Match: ~a\n" len))

            (when (begin
                    (if (> len best_len)
                        (begin
                          (set! match_start cur_match)
                          (set! best_len len)
                          (if (>= len nice_match)
                              #f
                              (begin
                                (set! scan_end1 (bytes-ref window-vec (+ scanpos best_len -1)))
                                (set! scan_end (bytes-ref window-vec (+ scanpos best_len)))
                                #t)))
                        #t))
              (continue)))))
    longest_match))

;; /* ===========================================================================
;;  * Check that the match at match_start is indeed a match.
;;  */
;;
(define (check_match start match length)
  #t)

;; /* ===========================================================================
;;  * Fill the window when the lookahead becomes insufficient.
;;  * Updates strstart and lookahead, and sets eofile if end of input file.
;;  * IN assertion: lookahead < MIN_LOOKAHEAD && strstart + lookahead > 0
;;  * OUT assertions: at least one byte has been read, or eofile is set;
;;  *    file reads are performed for at least two bytes (required for the
;;  *    translate_eol option).
;;  */
(define (fill_window)
  (define more (- window_size lookahead strstart))
  ;; /* Amount of free space at the end of the window. */

  ;; /* If the window is almost full and there is insufficient lookahead,
  ;;  * move the upper half to the lower one to make room in the upper half.
  ;;  */
  (when (>= strstart (+ WSIZE MAX_DIST))
    (let ([bs (gzbytes-bytes window)] [ofs (gzbytes-offset window)])
      (bytes-copy! bs ofs bs (+ ofs WSIZE) (+ ofs WSIZE WSIZE)))
    (set! match_start (- match_start WSIZE))
    (set! strstart (- strstart WSIZE)) ;; /* we now have strstart >= MAX_DIST: */

    (set! block_start (- block_start WSIZE))

    (for n := 0 < HASH_SIZE do
      (let ([m (vector-ref head-vec (+ n head-vec-delta))])
        (vector-set! head-vec (+ n head-vec-delta)
                     (if (>= m WSIZE) (- m WSIZE) NIL))))

    (for n := 0 < WSIZE do
      (let ([m (vector-ref prev-vec n)])
        (vector-set! prev-vec n
                     (if (>= m WSIZE) (- m WSIZE) NIL)))
      ;; /* If n is not on any hash chain, prev[n] is garbage but
      ;;  * its value will never be used.
      ;;  */
      )

    (set! more (+ more WSIZE)))

  (when (not eofile)
    (let ([n (read_buf (+ strstart lookahead) more)])
      (if (or (= n 0) (= n EOF-const))
          (set! eofile #t)
          (set! lookahead (+ lookahead n))))))

;; /* ===========================================================================
;;  * Flush the current block, with given end-of-file flag.
;;  * IN assertion: strstart is set to the end of the current match.
;;  */
(define (FLUSH-BLOCK eof)
  (flush_block (and (>= block_start 0) (gzbytes+ window block_start))
               (- strstart block_start)
               eof))

;; /* ===========================================================================
;;  * Same as above, but achieves better compression. We use a lazy
;;  * evaluation for matches: a match is finally adopted only if there is
;;  * no better match at the next window position.
;;  */
(define (do-deflate)
  (define  hash_head 0) ;; /* head of hash chain */
  (define prev_match 0) ;; /* previous match */
  (define flush #f) ;; /* set if current block must be flushed */
  (define match_available #f) ;; /* set if previous match exists */
  (define match_length MIN_MATCH-1) ;; /* length of best match */

  ;; /* Process the input block. */
  (let dloop ()
    (when (not (zero? lookahead))
      (DEBUG (Trace stderr
                    "prep ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a\n" hash_head prev_length match_length max_lazy_match strstart
                    ins_h (+ strstart MIN_MATCH-1) (bytes-ref window-vec (+ strstart MIN_MATCH-1))
                    H_SHIFT HASH_MASK))

      ;; /* Insert the string window[strstart .. strstart+2] in the
      ;;  * dictionary, and set hash_head to the head of the hash chain:
      ;;  */
      (INSERT_STRING strstart hash_head UPDATE_HASH window-vec head-vec prev-vec ins_h)

      (DEBUG (Trace stderr
                    "inh ~a ~a ~a ~a ~a ~a ~a\n" hash_head prev_length match_length max_lazy_match strstart
                    ins_h (bytes-ref window-vec (+ strstart MIN_MATCH-1))))

      ;; /* Find the longest match, discarding those <= prev_length.
      ;;  */
      (set! prev_length match_length)
      (set! prev_match match_start)
      (set! match_length MIN_MATCH-1)

      (when (and (not (= hash_head NIL))
                 (< prev_length max_lazy_match)
                 (<= (- strstart hash_head) MAX_DIST))
        ;; /* To simplify the code, we prevent matches with the string
        ;;  * of window index 0 (in particular we have to avoid a match
        ;;  * of the string with itself at the start of the input file).
        ;;  */
        (set! match_length (longest_match hash_head))
        (DEBUG (Trace stderr "blip ~a\n" match_length))
        ;; /* longest_match() sets match_start */
        (when (> match_length lookahead)
          (set! match_length lookahead))

        ;; /* Ignore a length 3 match if it is too distant: */
        (when (and (= match_length MIN_MATCH)
                   (> (- strstart match_start) TOO_FAR))
          ;; /* If prev_match is also MIN_MATCH, match_start is garbage
          ;;  * but we will ignore the current match anyway.
          ;;  */
          (set! match_length (sub1 match_length))))

      ;; /* If there was a match at the previous step and the current
      ;;  * match is not better, output the previous match:
      ;;  */
      (cond
       [(and (>= prev_length MIN_MATCH)
             (<= match_length prev_length))
        (DEBUG (Trace stderr "x1\n"))

        (check_match (- strstart 1) prev_match prev_length)

        (set! flush (ct_tally (- strstart 1 prev_match)
                              (- prev_length MIN_MATCH)))

        ;; /* Insert in hash table all strings up to the end of the match.
        ;;  * strstart-1 and strstart are already inserted.
        ;;  */
        (set! lookahead (- lookahead (- prev_length 1)))
        (set! prev_length (- prev_length 2))
        (let loop ()
          (set! strstart (add1 strstart))
          (INSERT_STRING strstart hash_head UPDATE_HASH window-vec head-vec prev-vec ins_h)
          (DEBUG (Trace stderr
                        "inhx ~a ~a ~a ~a ~a ~a\n" hash_head prev_length max_lazy_match strstart
                        ins_h (bytes-ref window-vec (+ strstart MIN_MATCH -1))))
          ;; /* strstart never exceeds WSIZE-MAX_MATCH, so there are
          ;;  * always MIN_MATCH bytes ahead. If lookahead < MIN_MATCH
          ;;  * these bytes are garbage, but it does not matter since the
          ;;  * next lookahead bytes will always be emitted as literals.
          ;;  */
          (set! prev_length (sub1 prev_length))
          (when (not (= prev_length 0))
            (loop)))
        (set! match_available #f)
        (set! match_length MIN_MATCH-1)
        (set! strstart (add1 strstart))
        (when flush
          (DEBUG (Trace stderr "flush\n"))
          (FLUSH-BLOCK 0)
          (DEBUG (Trace stderr "flush done\n"))
          (set! block_start strstart))]

       [match_available
        (DEBUG (Trace stderr "x2\n"))
        ;; /* If there was no match at the previous position, output a
        ;;  * single literal. If there was a match but the current match
        ;;  * is longer, truncate the previous match to a single literal.
        ;;  */
        ;; (Tracevv stderr "~c" (integer->char (vector-ref window-vec (- strstart 1))))
        (when (ct_tally 0 (bytes-ref window-vec (- strstart 1)))
          (FLUSH-BLOCK 0)
          (set! block_start strstart))
        (set! strstart (add1 strstart))
        (set! lookahead (sub1 lookahead))]

       [else
        (DEBUG (Trace stderr "x3\n"))
        ;; /* There is no previous match to compare with, wait for
        ;;  * the next step to decide.
        ;;  */
        (set! match_available #t)
        (set! strstart (add1 strstart))
        (set! lookahead (sub1 lookahead))])

      (Assert
       (unless (and (<= strstart bytes_in)
                    (<= lookahead bytes_in))
         (error "a bit too far")))

      ;; /* Make sure that we always have enough lookahead, except
      ;;  * at the end of the input file. We need MAX_MATCH bytes
      ;;  * for the next match, plus MIN_MATCH bytes to insert the
      ;;   * string following the next match.
      ;;  */
      (let loop ()
        (when (and (< lookahead MIN_LOOKAHEAD)
                   (not eofile))
          (DEBUG (Trace stderr "fill\n"))
          (fill_window)
          (loop)))

      (dloop)))

  (when match_available
    (ct_tally 0 (bytes-ref window-vec (- strstart 1))))

  (FLUSH-BLOCK 1)); /* eof */

#|
/* trees.c -- output deflated data using Huffman coding
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

/*
 *  PURPOSE
 *
 *      Encode various sets of source values using variable-length
 *      binary code trees.
 *
 *  DISCUSSION
 *
 *      The PKZIP "deflation" process uses several Huffman trees. The more
 *      common source values are represented by shorter bit sequences.
 *
 *      Each code tree is stored in the ZIP file in a compressed form
 *      which is itself a Huffman encoding of the lengths of
 *      all the code strings (in ascending order by source values).
 *      The actual code strings are reconstructed from the lengths in
 *      the UNZIP process, as described in the "application note"
 *      (APPNOTE.TXT) distributed as part of PKWARE's PKZIP program.
 *
 *  REFERENCES
 *
 *      Lynch, Thomas J.
 *          Data Compression:  Techniques and Applications, pp. 53-55.
 *          Lifetime Learning Publications, 1985.  ISBN 0-534-03418-7.
 *
 *      Storer, James A.
 *          Data Compression:  Methods and Theory, pp. 49-50.
 *          Computer Science Press, 1988.  ISBN 0-7167-8156-5.
 *
 *      Sedgewick, R.
 *          Algorithms, p290.
 *          Addison-Wesley, 1983. ISBN 0-201-06672-6.
 *
 *  INTERFACE
 *
 *      void ct_init (ush *attr, int *methodp)
 *          Allocate the match buffer, initialize the various tables and save
 *          the location of the internal file attribute (ascii/binary) and
 *          method (DEFLATE/STORE)
 *
 *      void ct_tally (int dist, int lc);
 *          Save the match info and tally the frequency counts.
 *
 *      long flush_block (char *buf, ulg stored_len, int eof)
 *          Determine the best encoding for the current block: dynamic trees,
 *          static trees or store, and output the encoded block to the zip
 *          file. Returns the total compressed length for the file so far.
 *
 */

|#

;; /* ===========================================================================
;;  * Constants
;;  */

(define MAX_BITS 15)
;; /* All codes must not exceed MAX_BITS bits */

(define MAX_BL_BITS 7)
;; /* Bit length codes must not exceed MAX_BL_BITS bits */

(define LENGTH_CODES 29)
;; /* number of length codes, not counting the special END_BLOCK code */

(define LITERALS  256)
;; /* number of literal bytes 0..255 */

(define END_BLOCK 256)
;; /* end of block literal code */

(define L_CODES (+ LITERALS 1 LENGTH_CODES))
;; /* number of Literal or Length codes, including the END_BLOCK code */

(define D_CODES   30)
;; /* number of distance codes */

(define BL_CODES  19)
;; /* number of codes used to transfer the bit lengths */

(define extra_lbits ;; /* extra bits for each length code */
  (vector 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))

(define extra_dbits ;; /* extra bits for each distance code */
  (vector 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(define extra_blbits ;; /* extra bits for each bit length code */
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 7))

(define STORED_BLOCK 0)
(define STATIC_TREES 1)
(define DYN_TREES    2)
;; /* The three kinds of block type */

(define LIT_BUFSIZE  #x8000)
(define DIST_BUFSIZE #x8000)
;; /* Sizes of match buffers for literals/lengths and distances.  There are
;;  * 4 reasons for limiting LIT_BUFSIZE to 64K:
;;  *   - frequencies can be kept in 16 bit counters
;;  *   - if compression is not successful for the first block, all input data is
;;  *     still in the window so we can still emit a stored block even when input
;;  *     comes from standard input.  (This can also be done for all blocks if
;;  *     LIT_BUFSIZE is not greater than 32K.)
;;  *   - if compression is not successful for a file smaller than 64K, we can
;;  *     even emit a stored file instead of a stored block (saving 5 bytes).
;;  *   - creating new Huffman trees less frequently may not provide fast
;;  *     adaptation to changes in the input data statistics. (Take for
;;  *     example a binary file with poorly compressible code followed by
;;  *     a highly compressible string table.) Smaller buffer sizes give
;;  *     fast adaptation but have of course the overhead of transmitting trees
;;  *     more frequently.
;;  *   - I can't count above 4
;;  * The current code is general and allows DIST_BUFSIZE < LIT_BUFSIZE (to save
;;  * memory at the expense of compression). Some optimizations would be possible
;;  * if we rely on DIST_BUFSIZE == LIT_BUFSIZE.
;;  */
(when (> LIT_BUFSIZE INBUFSIZ)
  (error "cannot overlay l_buf and inbuf"))

(define REP_3_6      16)
;; /* repeat previous bit length 3-6 times (2 bits of repeat count) */

(define REPZ_3_10    17)
;; /* repeat a zero length 3-10 times  (3 bits of repeat count) */

(define REPZ_11_138  18)
;; /* repeat a zero length 11-138 times  (7 bits of repeat count) */

;; /* ===========================================================================
;;  * Local data
;;  */

;; /* Data structure describing a single value and its code string. */
(define-struct ct_data (freq code dad len))
;;    union {
;;        ush  freq;       ;; /* frequency count */
;;        ush  code;       ;; /* bit string */
;;    } fc;
;;    union {
;;        ush  dad;        ;; /* father node in Huffman tree */
;;        ush  len;        ;; /* length of bit string */
;;    } dl;
#|
(define ct_data-freq ct_data-freq/code)
(define ct_data-code ct_data-freq/code)
(define ct_data-dad ct_data-dad/len)
(define ct_data-len ct_data-dad/len)
(define set-ct_data-freq! set-ct_data-freq/code!)
(define set-ct_data-code! set-ct_data-freq/code!)
(define set-ct_data-dad! set-ct_data-dad/len!)
(define set-ct_data-len! set-ct_data-dad/len!)
(define (_make-ct_data f c d l) (make-ct_data (or f c) (or d l)))
|#
(define _make-ct_data make-ct_data)

(define HEAP_SIZE (+ (* 2 L_CODES) 1))
;; /* maximum heap size */

(define dyn_ltree (make-vector HEAP_SIZE 'uninit-dl))   ;; /* literal and length tree */
(define dyn_dtree (make-vector (+ (* 2 D_CODES) 1) 'uninit-dd)) ;; /* distance tree */

(define static_ltree (make-vector (+ L_CODES 2) 'uninit-sl))
;; /* The static literal tree. Since the bit lengths are imposed, there is no
;;  * need for the L_CODES extra codes used during heap construction. However
;;  * The codes 286 and 287 are needed to build a canonical tree (see ct_init
;;  * below).
;;  */

(define static_dtree (make-vector D_CODES 'uninit-sd))
;; /* The static distance tree. (Actually a trivial tree since all codes use
;;  * 5 bits.)
;;  */

(define bl_tree (make-vector (+ (* 2 BL_CODES) 1) 'uninit-dl))
;; /* Huffman tree for the bit lengths */

(define-struct tree_desc
  (dyn_tree;      ;; /* the dynamic tree */
   static_tree;   ;; /* corresponding static tree or NULL */
   extra_bits;    ;; /* extra bits for each code or NULL */
   extra_base;    ;; /* base index for extra_bits */
   elems;         ;; /* max number of elements in the tree */
   max_length;    ;; /* max bit length for the codes */
   max_code));    ;; /* largest code with non zero frequency */

(define l_desc (make-tree_desc
                dyn_ltree static_ltree extra_lbits
                (+ LITERALS 1) L_CODES MAX_BITS 0))

(define d_desc (make-tree_desc
                dyn_dtree static_dtree extra_dbits
                0 D_CODES MAX_BITS 0))

(define bl_desc (make-tree_desc
                 bl_tree #f extra_blbits
                 0 BL_CODES MAX_BL_BITS 0))


(define bl_count (make-vector (+ MAX_BITS 1) 0))
;; /* number of codes at each bit length for an optimal tree */

(define bl_order
  (vector 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
;; /* The lengths of the bit length codes are sent in order of decreasing
;;  * probability, to avoid transmitting the lengths for unused bit length codes.
;;  */

(define heap (make-vector (+ (* 2 L_CODES) 1) 0)) ;; /* heap used to build the Huffman trees */
(define heap_len 0)               ;; /* number of elements in the heap */
(define heap_max 0)               ;; /* element of largest frequency */
;; /* The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
;;  * The same heap array is used to build all trees.
;;  */

(define depth (make-vector (+ (* 2 L_CODES) 1) 0))
;; /* Depth of each subtree used as tie breaker for trees of equal frequency */

(define length_code (make-vector (- MAX_MATCH MIN_MATCH -1) 0))
;; /* length code for each normalized match length (0 == MIN_MATCH) */

(define dist_code (make-vector 512 0))
;; /* distance codes. The first 256 values correspond to the distances
;;  * 3 .. 258, the last 256 values correspond to the top 8 bits of
;;  * the 15 bit distances.
;;  */

(define base_length (make-vector LENGTH_CODES 0))
;; /* First normalized length for each code (0 = MIN_MATCH) */

(define base_dist (make-vector D_CODES 0))
;; /* First normalized distance for each code (0 = distance of 1) */

(define inbuf (make-bytes (+ INBUFSIZ INBUF_EXTRA) 0))
(define l_buf inbuf)
;; /* DECLARE(uch, l_buf, LIT_BUFSIZE);  buffer for literals or lengths */

(define d_buf (make-vector DIST_BUFSIZE 0))
;; /* DECLARE(ush, d_buf, DIST_BUFSIZE); buffer for distances */

(define flag_buf (make-vector (/ LIT_BUFSIZE 8) 0))
;; /* flag_buf is a bit array distinguishing literals from lengths in
;;  * l_buf, thus indicating the presence or absence of a distance.
;;  */

(define last_lit 0)    ;; /* running index in l_buf */
(define last_dist 0)   ;; /* running index in d_buf */
(define last_flags 0)  ;; /* running index in flag_buf */
(define flags 0)            ;; /* current flags not yet saved in flag_buf */
(define flag_bit 0)         ;; /* current bit used in flags */
;; /* bits are filled in flags starting at bit 0 (least significant).
;;  * Note: these flags are overkill in the current code since we don't
;;  * take advantage of DIST_BUFSIZE == LIT_BUFSIZE.
;; */

(define opt_len 0);        ;; /* bit length of current block with optimal trees */
(define static_len 0);     ;; /* bit length of current block with static trees */

(define compressed_len 0); ;; /* total bit length of compressed file */

(define input_len 0);      ;; /* total byte length of input file */
;; /* input_len is for debugging only since we can get it by other means. */

;; (define block_start 0);       ;; /* window offset of current block */
;; (define strstart 0); ;; /* window offset of current string */

(define (send_code c tree)
  (send_bits (ct_data-code (vector-ref tree c))
             (ct_data-len (vector-ref tree c))))
;; /* Send a code of the given tree. c and tree must not have side effects */

(define (d_code dist)
  (if (< dist 256)
    (vector-ref dist_code dist)
    (vector-ref dist_code (+ 256 (>> dist 7)))))
;; /* Mapping from a distance to a distance code. dist is the distance - 1 and
;;  * must not have side effects. dist_code[256] and dist_code[257] are never
;;  * used.
;; */

;; /* ===========================================================================
;;  * Allocate the match buffer, initialize the various tables and save the
;;  * location of the internal file attribute (ascii/binary) and method
;;  * (DEFLATE/STORE).
;; */
(define (ct_init)

  (define length 0)   ;; /* length value */
  (define dist 0)     ;; /* distance index */

  (set! compressed_len 0)
  (set! input_len 0)

  (unless (ct_data? (vector-ref static_dtree 0)) ;; /* ct_init already called? */
    ;; /* Initialize the mapping length (0..255) -> length code (0..28) */
    (set! length 0)
    (for code := 0 < (- LENGTH_CODES 1) do
      (vector-set! base_length code length)
      (for n := 0 < (<< 1 (vector-ref extra_lbits code)) do
        (vector-set! length_code length code)
        (set! length (add1 length))))

    (Assert
     (unless (= length 256)
       (error "ct_init: length != 256")))

    ;; /* Note that the length 255 (match length 258) can be represented
    ;;  * in two different ways: code 284 + 5 bits or code 285, so we
    ;;  * overwrite length_code[255] to use the best encoding:
    ;; */
    (vector-set! length_code (- length 1) (- LENGTH_CODES 1))

    ;; /* Initialize the mapping dist (0..32K) -> dist code (0..29) */
    (set! dist 0)
    (for code := 0 < 16 do
      (vector-set! base_dist code dist)
      (for n := 0 < (<< 1 (vector-ref extra_dbits code)) do
        (vector-set! dist_code dist code)
        (set! dist (add1 dist))))

    (Assert
     (unless (= dist 256)
       (error "ct_init: dist != 256")))
    (set! dist (>> dist 7)) ;; /* from now on, all distances are divided by 128 */
    (for code := 16 < D_CODES do
      (vector-set! base_dist code (<< dist 7))
      (for n := 0 < (<< 1 (- (vector-ref extra_dbits code) 7)) do
        (vector-set! dist_code (+ 256 dist) code)
        (set! dist (add1 dist))))

    (Assert
     (unless (= dist 256)
       (error "ct_init: 256+dist != 512")))

    ;; /* Construct the codes of the static literal tree */
    (for bits := 0 <= MAX_BITS do
      (vector-set! bl_count bits 0))

    (let ([init-ltree
           (lambda (s e v)
             (for n := s <= e do
               (vector-set! static_ltree n (_make-ct_data #f 0 #f v))
               (vector-set! bl_count v (add1 (vector-ref bl_count v)))))])
      (init-ltree 0 143 8)
      (init-ltree 144 255 9)
      (init-ltree 256 279 7)
      (init-ltree 280 287 8))
    ;; /* Codes 286 and 287 do not exist, but we must include them in the
    ;;  * tree construction to get a canonical Huffman tree (longest code
    ;;  * all ones)
    ;; */
    (gen_codes static_ltree (+ L_CODES 1))

    ;; /* The static distance tree is trivial: */
    (for n := 0 < D_CODES do
      (vector-set! static_dtree n
                   (_make-ct_data #f (bi_reverse n 5) #f 5)))

    ;; /* Initialize the first block of the first file: */
    (init_block)))

;; /* ===========================================================================
;;  * Initialize a new block.
;; */
(define inited-once? #f)
(define (init_block)
  (for n := 0 < (if inited-once? L_CODES HEAP_SIZE) do
    (vector-set! dyn_ltree n (_make-ct_data 0 #f 0 #f)))
  (for n := 0 < (if inited-once? D_CODES (+ (* 2 D_CODES) 1)) do
    (vector-set! dyn_dtree n (_make-ct_data 0 #f 0 #f)))
  (for n := 0 < (if inited-once? BL_CODES (+ (* 2 BL_CODES) 1)) do
    (vector-set! bl_tree n (_make-ct_data 0 #f 0 #f)))

  (set! inited-once? #t)

  (set-ct_data-freq! (vector-ref dyn_ltree END_BLOCK) 1)
  (set! opt_len 0)
  (set! static_len 0)
  (set! last_lit 0)
  (set! last_dist 0)
  (set! last_flags 0)
  (set! flags 0)
  (set! flag_bit 1))

(define SMALLEST 1)
;; /* Index within the heap array of least frequent node in the Huffman tree */


;; /* ===========================================================================
;;  * Remove the smallest element from the heap and recreate the heap with
;;  * one less element. Updates heap and heap_len.
;; */
;; (define-macro pqremove <see above>)

;; /* ===========================================================================
;;  * Compares to subtrees, using the tree depth as tie breaker when
;;  * the subtrees have equal frequency. This minimizes the worst case length.
;;  */
(define (smaller tree n m)
  (or (< (ct_data-freq (vector-ref tree n)) (ct_data-freq (vector-ref tree m)))
      (and (= (ct_data-freq (vector-ref tree n)) (ct_data-freq (vector-ref tree m)))
           (<= (vector-ref depth n) (vector-ref depth m)))))

;; /* ===========================================================================
;;  * Restore the heap property by moving down the tree starting at node k,
;;  * exchanging a node with the smallest of its two sons if necessary, stopping
;;  * when the heap property is re-established (each father smaller than its
;;  * two sons).
;;  */
(define (pqdownheap tree k)
  ;; ct_data near *tree;  /* the tree to restore */
  ;; int k;               /* node to move down */

  (define v (vector-ref heap k))
  (define j (<< k 1))  ;; /* left son of k */
  (let loop ([k k][j j])
    (if (<= j heap_len)
        ;; /* Set j to the smallest of the two sons: */
        (let ([j (if (and (< j heap_len)
                          (smaller tree
                                   (vector-ref heap (+ j 1))
                                   (vector-ref heap j)))
                     (add1 j)
                     j)])
          ;; /* Exit if v is smaller than both sons */
          (if (smaller tree v (vector-ref heap j))
              (vector-set! heap k v)
              (begin
                ;; /* Exchange v with the smallest son */
                (vector-set! heap k (vector-ref heap j))
                ;; /* And continue down the tree, setting j to the left son of k */
                (loop j (<< j 1)))))
        (vector-set! heap k v))))

;; /* ===========================================================================
;;  * Compute the optimal bit lengths for a tree and update the total bit length
;;  * for the current block.
;;  * IN assertion: the fields freq and dad are set, heap[heap_max] and
;;  *    above are the tree nodes sorted by increasing frequency.
;;  * OUT assertions: the field len is set to the optimal bit length, the
;;  *     array bl_count contains the frequencies for each bit length.
;;  *     The length opt_len is updated; static_len is also updated if stree is
;;  *     not null.
;; */
(define (gen_bitlen desc)
  ;; tree_desc near *desc; ;; /* the tree descriptor */

  (define tree (tree_desc-dyn_tree desc))
  (define extra (tree_desc-extra_bits desc))
  (define base (tree_desc-extra_base desc))
  (define max_code (tree_desc-max_code desc))
  (define max_length (tree_desc-max_length desc))
  (define stree (tree_desc-static_tree desc))
  (define n 0) (define m 0) ;; /* iterate over the tree elements */
  (define bits 0)           ;; /* bit length */
  (define xbits 0)          ;; /* extra bits */
  (define f 0);              ;; /* frequency */
  (define overflow 0);   ;; /* number of elements with bit length too large */
  (define h 0)

  (for bits := 0 <= MAX_BITS do
    (vector-set! bl_count bits 0))

  ;; /* In a first pass, compute the optimal bit lengths (which may
  ;;  * overflow in the case of the bit length tree).
  ;;  */
  (set-ct_data-len! (vector-ref tree (vector-ref heap heap_max)) 0) ;; /* root of the heap */

  (for h := (+ 1 heap_max) < HEAP_SIZE do
    (set! n (vector-ref heap h))
    (set! bits (+ (ct_data-len (vector-ref tree (ct_data-dad (vector-ref tree n)))) 1))
    (when (> bits max_length)
      (set! bits max_length)
      (set! overflow (add1 overflow)))
    (set-ct_data-len! (vector-ref tree n) bits)
    ;; /* We overwrite tree[n].Dad which is no longer needed */
    (unless (> n max_code)
      ;; /* leaf node */
      (vector-set! bl_count bits (add1 (vector-ref bl_count bits)))
      (set! xbits 0)
      (when (>= n base)
        (set! xbits (vector-ref extra (- n base))))
      (set! f (ct_data-freq (vector-ref tree n)))
      (set! opt_len (+ opt_len (* f (+ bits xbits))))
      (when stree
        (set! static_len
              (+ static_len
                 (* f (+ (ct_data-len (vector-ref stree n)) xbits)))))))

  (unless (= overflow 0)

    (DEBUG (Trace stderr "\nbit length overflow\n"))
    ;; /* This happens for example on obj2 and pic of the Calgary corpus */

    ;; /* Find the first bit length which could increase: */
    (let loop ()
      (set! bits (- max_length 1))
      (let loop ()
        (when (= (vector-ref bl_count bits) 0)
          (set! bits (sub1 bits))
          (loop)))
      (vector-set! bl_count bits (sub1 (vector-ref bl_count bits)))
      (vector-set! bl_count (+ bits 1) (+ (vector-ref bl_count (+ bits 1)) 2))
      (vector-set! bl_count max_length (sub1 (vector-ref bl_count max_length)))
      ;; /* The brother of the overflow item also moves one step up,
      ;;  * but this does not affect bl_count[max_length]
      ;;  */
      (set! overflow (- overflow 2))
      (when (> overflow 0)
        (loop)))

    (set! h HEAP_SIZE)
    ;; /* Now recompute all bit lengths, scanning in increasing frequency.
    ;;  * h is still equal to HEAP_SIZE. (It is simpler to reconstruct all
    ;;  * lengths instead of fixing only the wrong ones. This idea is taken
    ;;  * from 'ar' written by Haruhiko Okumura.)
    ;;  */
    (for bits := max_length then sub1 > 0 do
      (set! n (vector-ref bl_count bits))
      (let loop ()
        (when (not (= n 0))
          (set! h (sub1 h))
          (set! m (vector-ref heap h))
          (if (> m max_code)
            (loop)
            (begin
              (when (not (= (ct_data-len (vector-ref tree m)) bits))
                (set! opt_len
                      (+ opt_len (* (- bits (ct_data-len (vector-ref tree m)))
                                    (ct_data-freq (vector-ref tree m))))))
              (set-ct_data-len! (vector-ref tree m) bits)
              (set! n (sub1 n))
              (loop))))))))

;; /* ===========================================================================
;;  * Generate the codes for a given tree and bit counts (which need not be
;;  * optimal).
;;  * IN assertion: the array bl_count contains the bit length statistics for
;;  * the given tree and the field len is set for all tree elements.
;;  * OUT assertion: the field code is set for all tree elements of non
;;  *     zero code length.
;; */
(define (gen_codes tree max_code)
  ;; ct_data near *tree;        /* the tree to decorate */
  ;; int max_code;              /* largest code with non zero frequency */

  (define next_code (make-vector (+ MAX_BITS 1) 0)) ;; /* next code value for each bit length */
  (define code 0)              ;; /* running code value */
  (define bits 0)                  ;; /* bit index */

  ;; /* The distribution counts are first used to generate the code values
  ;;  * without bit reversal.
  ;;  */
  (for bits := 1 <= MAX_BITS do
    (set! code (<< (+ code (vector-ref bl_count (- bits 1))) 1))
    (vector-set! next_code bits code))
  ;; /* Check that the bit counts in bl_count are consistent. The last code
  ;;  * must be all ones.
  ;;  */
  (Assert
   (unless (= (+ code (vector-ref bl_count MAX_BITS)-1)
              (- (<< 1 MAX_BITS) 1))
     "inconsistent bit counts"))
  (DEBUG (Tracev stderr "\ngen_codes: max_code ~a " max_code))

  (for n := 0 <= max_code do
    (let ([len (ct_data-len (vector-ref tree n))])
      (unless (= len 0)
        ;; /* Now reverse the bits */
        (let ([nc (vector-ref next_code len)])
          (set-ct_data-code! (vector-ref tree n) (bi_reverse nc len))
          (vector-set! next_code len (add1 nc)))

        (DEBUG (Tracec (not (eq? tree static_ltree))
                       stderr
                       "\nn ~a ~c l ~a c ~x (~x) "
                       n #\space len
                       (or (ct_data-code (vector-ref tree n)) 0)
                       (or (- (vector-ref next_code len) 1) 0)))))))

;; /* ===========================================================================
;;  * Construct one Huffman tree and assigns the code bit strings and lengths.
;;  * Update the total bit length for the current block.
;;  * IN assertion: the field freq is set for all tree elements.
;;  * OUT assertions: the fields len and code are set to the optimal bit length
;;  *     and corresponding code. The length opt_len is updated; static_len is
;;  *     also updated if stree is not null. The field max_code is set.
;;  */
(define (build_tree desc)
  ;; tree_desc near *desc; ;; /* the tree descriptor */

  (define tree (tree_desc-dyn_tree desc))
  (define stree (tree_desc-static_tree desc))
  (define elems (tree_desc-elems desc))
  (define n 0) (define m 0)   ;; /* iterate over heap elements */
  (define max_code -1) ;; /* largest code with non zero frequency */
  (define node elems)  ;; /* next internal node of the tree */

  ;; /* Construct the initial heap, with least frequent element in
  ;;  * heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
  ;;  * heap[0] is not used.
  ;; */
  (set! heap_len 0)
  (set! heap_max HEAP_SIZE)

  (for n := 0 < elems do
    (DEBUG (Trace stderr "freq: ~a ~a\n" n (ct_data-freq (vector-ref tree n))))
    (if (not (= (ct_data-freq (vector-ref tree n)) 0))
      (begin (set! heap_len (add1 heap_len))
             (set! max_code n)
             (vector-set! heap heap_len n)
             (vector-set! depth n 0))
      (set-ct_data-len! (vector-ref tree n) 0)))

  (DEBUG (Trace stderr "Building: ~a ~a ~a\n" elems heap_len max_code))

  ;; /* The pkzip format requires that at least one distance code exists,
  ;;  * and that at least one bit should be sent even if there is only one
  ;;  * possible code. So to avoid special checks later on we force at least
  ;;  * two codes of non zero frequency.
  ;;  */
  (let loop ()
    (when (< heap_len 2)
      (let ([new (if (< max_code 2)
                     (begin
                       (set! max_code (add1 max_code))
                       max_code)
                     0)])
        (set! heap_len (add1 heap_len))
        (vector-set! heap heap_len new)
        (set-ct_data-freq! (vector-ref tree new) 1)
        (vector-set! depth new 0)
        (set! opt_len (sub1 opt_len))
        (when stree
          (set! static_len (- static_len (ct_data-len (vector-ref stree new)))))
        ;; /* new is 0 or 1 so it does not have extra bits */
        (loop))))

  (set-tree_desc-max_code! desc max_code)

  ;; /* The elements heap[heap_len/2+1 .. heap_len] are leaves of the tree,
  ;;  * establish sub-heaps of increasing lengths:
  ;;  */
  (for n := (quotient heap_len 2) then sub1 >= 1 do (pqdownheap tree n))

  ;; /* Construct the Huffman tree by repeatedly combining the least two
  ;;  * frequent nodes.
  ;;  */
  (let loop ()
    ;; /* n = node of least frequency */
    (set! n (vector-ref heap SMALLEST))
    (vector-set! heap SMALLEST (vector-ref heap heap_len))
    (set! heap_len (sub1 heap_len))
    (pqdownheap tree SMALLEST)

    (set! m (vector-ref heap SMALLEST))  ;; /* m = node of next least frequency */

    (set! heap_max (sub1 heap_max))
    (vector-set! heap heap_max n) ;; /* keep the nodes sorted by frequency */
    (set! heap_max (sub1 heap_max))
    (vector-set! heap heap_max m)

    ;; /* Create a new node father of n and m */
    (set-ct_data-freq! (vector-ref tree node)
                       (+ (ct_data-freq (vector-ref tree n))
                          (ct_data-freq (vector-ref tree m))))
    (vector-set! depth node (+ (max (vector-ref depth n)
                                    (vector-ref depth m))
                               1))
    (set-ct_data-dad! (vector-ref tree n) node)
    (set-ct_data-dad! (vector-ref tree m) node)

    ;; /* and insert the new node in the heap */
    (vector-set! heap SMALLEST node)
    (set! node (add1 node))
    (pqdownheap tree SMALLEST)

    (when (>= heap_len 2)
      (loop)))

  (set! heap_max (sub1 heap_max))
  (vector-set! heap heap_max (vector-ref heap SMALLEST))

  ;; /* At this point, the fields freq and dad are set. We can now
  ;;  * generate the bit lengths.
  ;;  */
  (gen_bitlen desc)

  (DEBUG (Trace stderr "Build: ~a\n" max_code))
  ;; /* The field len is now set, we can generate the bit codes */
  (gen_codes tree max_code))

;; /* ===========================================================================
;;  * Scan a literal or distance tree to determine the frequencies of the codes
;;  * in the bit length tree. Updates opt_len to take into account the repeat
;;  * counts. (The contribution of the bit length codes will be added later
;;  * during the construction of bl_tree.)
;;  */
(define (scan_tree tree max_code)
  ;; ct_data near *tree; ;; /* the tree to be scanned */
  ;; int max_code;       ;; /* and its largest code of non zero frequency */

  (define prevlen -1)          ;; /* last emitted length */
  (define curlen 0)                ;; /* length of current code */
  (define nextlen (ct_data-len (vector-ref tree 0))) ;; /* length of next code */
  (define count 0)             ;; /* repeat count of the current code */
  (define max_count 7)         ;; /* max repeat count */
  (define min_count 4)         ;; /* min repeat count */

  (when (= nextlen 0)
    (set! max_count 138)
    (set! min_count 3))

  (set-ct_data-len! (vector-ref tree (+ max_code 1)) #xffff) ;; /* guard */

  (for n := 0 <= max_code do
    (let/ec continue
      (define (inc-bl_tree-freq which amt)
        (set-ct_data-freq! (vector-ref bl_tree which)
                           (+ amt (ct_data-freq (vector-ref bl_tree which)))))

      (set! curlen nextlen)
      (set! nextlen (ct_data-len (vector-ref tree (+ n 1))))
      (set! count (add1 count))

      (cond [(and (< count max_count) (= curlen nextlen))
             (continue)]
            [(< count min_count)
             (inc-bl_tree-freq curlen count)]
            [(not (= curlen 0))
             (when (not (= curlen prevlen))
               (inc-bl_tree-freq curlen 1))
             (inc-bl_tree-freq REP_3_6 1)]
            [(<= count 10)
             (inc-bl_tree-freq REPZ_3_10 1)]
            [else
             (inc-bl_tree-freq REPZ_11_138 1)])

      (set! count 0)
      (set! prevlen curlen)

      (cond [(= nextlen 0)      (set! max_count 138) (set! min_count 3)]
            [(= curlen nextlen) (set! max_count   6) (set! min_count 3)]
            [else               (set! max_count   7) (set! min_count 4)]))))

;; /* ===========================================================================
;;  * Send a literal or distance tree in compressed form, using the codes in
;;  * bl_tree.
;;  */
(define (send_tree tree max_code)
  ;; ct_data near *tree; ;; /* the tree to be scanned */
  ;; int max_code;       ;; /* and its largest code of non zero frequency */

  (define prevlen -1)          ;; /* last emitted length */
  (define curlen 0)              ;; /* length of current code */
  (define nextlen (ct_data-len (vector-ref tree 0))) ;; /* length of next code */
  (define count 0)             ;; /* repeat count of the current code */
  (define max_count 7)         ;; /* max repeat count */
  (define min_count 4)         ;; /* min repeat count */

  ;; /* tree[max_code+1].Len = -1; */  ;; /* guard already set */
  (when (= nextlen 0)
    (set! max_count 138)
    (set! min_count 3))

  (for n := 0 <= max_code do
    (let/ec continue
      (set! curlen nextlen)
      (set! nextlen (ct_data-len (vector-ref tree (+ n 1))))

      (set! count (add1 count))
      (cond [(and (< count max_count) (= curlen nextlen))
             (continue)]
            [(< count min_count)
             (let loop ()
               (send_code curlen bl_tree)
               (set! count (sub1 count))
               (when (not (= count 0)) (loop)))]
            [(not (= curlen 0))
             (when (not (= curlen prevlen))
               (send_code curlen bl_tree)
               (set! count (sub1 count)))
             (Assert
              (unless (>= 6 count 3)
                (error " 3_6?")))
             (send_code REP_3_6 bl_tree)
             (send_bits (- count 3) 2)]
            [(<= count 10)
             (send_code REPZ_3_10 bl_tree)
             (send_bits (- count 3) 3)]
            [else
             (send_code REPZ_11_138 bl_tree)
             (send_bits (- count 11) 7)])

      (set! count 0)
      (set! prevlen curlen)

      (cond [(= nextlen 0)      (set! max_count 138) (set! min_count 3)]
            [(= curlen nextlen) (set! max_count   6) (set! min_count 3)]
            [else               (set! max_count   7) (set! min_count 4)]))))

;; /* ===========================================================================
;;  * Construct the Huffman tree for the bit lengths and return the index in
;;  * bl_order of the last bit length code to send.
;;  */
(define (build_bl_tree)
  (define max_blindex 0)  ;; /* index of last bit length code of non zero freq */

  ;; /* Determine the bit length frequencies for literal and distance trees */
  (scan_tree dyn_ltree (tree_desc-max_code l_desc))
  (scan_tree dyn_dtree (tree_desc-max_code d_desc))

  ;; /* Build the bit length tree: */
  (build_tree bl_desc)
  ;; /* opt_len now includes the length of the tree representations, except
  ;;  * the lengths of the bit lengths codes and the 5+5+4 bits for the counts.
  ;;  */

  ;; /* Determine the number of bit length codes to send. The pkzip format
  ;;  * requires that at least 4 bit length codes be sent. (appnote.txt says
  ;;  * 3 but the actual value used is 4.)
  ;;  */
  (set! max_blindex (- BL_CODES 1))
  (let loop ()
    (when (and (>= max_blindex 3)
               (= (ct_data-len (vector-ref bl_tree
                                           (vector-ref bl_order max_blindex)))
                  0))
      (set! max_blindex (sub1 max_blindex))
      (loop)))

  ;; /* Update opt_len to include the bit length tree and counts */
  (set! opt_len (+ opt_len (* 3 (+ max_blindex 1)) 5 5 4))
  (DEBUG (Tracev stderr "\ndyn trees: dyn ~a, stat ~a" opt_len static_len))

  max_blindex)

;; /* ===========================================================================
;;  * Send the header for a block using dynamic Huffman trees: the counts, the
;;  * lengths of the bit length codes, the literal tree and the distance tree.
;;  * IN assertion: lcodes >= 257, dcodes >= 1, blcodes >= 4.
;;  */
(define (send_all_trees lcodes dcodes blcodes)
  ;; int lcodes, dcodes, blcodes; ;; /* number of codes for each tree */

  (Assert
   (unless (and (>= lcodes 257)
                (>= dcodes 1)
                (>= blcodes 4))
     (error "not enough codes")))
  (Assert
   (unless (and (<= lcodes L_CODES)
                (<= dcodes D_CODES)
                (<= blcodes BL_CODES))
     (error "too many codes ~a(~a) ~a(~a) ~a(~a)"
            lcodes L_CODES
            dcodes D_CODES
            blcodes BL_CODES)))

  (DEBUG (Tracev stderr "\nbl counts: "))

  (send_bits (- lcodes 257) 5) ;; /* not +255 as stated in appnote.txt */
  (send_bits (- dcodes 1) 5)
  (send_bits (- blcodes 4) 4) ;; /* not -3 as stated in appnote.txt */
  (for rank := 0 < blcodes do
    (DEBUG (Tracev stderr "\nbl code ~a " (vector-ref bl_order rank)))
    (send_bits (ct_data-len (vector-ref bl_tree (vector-ref bl_order rank)))
               3))
  (DEBUG (Tracev stderr "\nbl tree: sent ~a" bits_sent))

  (send_tree dyn_ltree (- lcodes 1)) ;; /* send the literal tree */
  (DEBUG (Tracev stderr "\nlit tree: sent ~a" bits_sent))

  (send_tree dyn_dtree (- dcodes 1)) ;; /* send the distance tree */
  (DEBUG (Tracev stderr "\ndist tree: sent ~a" bits_sent)))

;; /* ===========================================================================
;;  * Determine the best encoding for the current block: dynamic trees, static
;;  * trees or store, and output the encoded block to the zip file. This function
;;  * returns the total compressed length for the file so far.
;;  */
(define (flush_block buf stored_len eof)
  ;; char *buf;        ;; /* input block, or NULL if too old */
  ;; ulg stored_len;   ;; /* length of input block */
  ;; int eof;          ;; /* true if this is the last block for a file */

  (define opt_lenb 0) (define static_lenb 0) ;; /* opt_len and static_len in bytes */
  (define max_blindex 0)  ;; /* index of last bit length code of non zero freq */

  (vector-set! flag_buf last_flags flags) ;; /* Save the flags for the last 8 items */

  ;; /* Construct the literal and distance trees */
  (build_tree l_desc)
  (DEBUG (Tracev stderr "\nlit data: dyn ~a, stat ~a" opt_len static_len))

  (build_tree d_desc)
  (DEBUG (Tracev stderr "\ndist data: dyn ~a, stat ~a" opt_len static_len))
  ;; /* At this point, opt_len and static_len are the total bit lengths of
  ;;  * the compressed block data, excluding the tree representations.
  ;;  */

  ;; /* Build the bit length tree for the above two trees, and get the index
  ;;  * in bl_order of the last bit length code to send.
  ;;  */
  (set! max_blindex (build_bl_tree))

  ;; /* Determine the best encoding. Compute first the block length in bytes */
  (set! opt_lenb (>> (+ opt_len 3 7) 3))
  (set! static_lenb (>> (+ static_len 3 7) 3))
  (set! input_len (+ input_len stored_len)) ;; /* for debugging only */

  (DEBUG (Trace stderr "\nopt ~a(~a) stat ~a(~a) stored ~a lit ~a dist ~a "
                opt_lenb opt_len static_lenb static_len stored_len
                last_lit last_dist))

  (when (<= static_lenb opt_lenb)
    (set! opt_lenb static_lenb))

  ;; /* If compression failed and this is the first and last block,
  ;;  * and if the zip file can be seeked (to rewrite the local header),
  ;;  * the whole file is transformed into a stored file:
  ;;  */
  (cond
   [(and buf (<= (+ stored_len 4) opt_lenb))
    ;; /* 4: two words for the lengths */

    ;; /* The test buf != NULL is only necessary if LIT_BUFSIZE > WSIZE.
    ;;  * Otherwise we can't have processed more than WSIZE input bytes since
    ;;  * the last block flush, because compression would have been
    ;;  * successful. If LIT_BUFSIZE <= WSIZE, it is never too late to
    ;;  * transform a block into a stored block.
    ;;  */
    (send_bits (+ (<< STORED_BLOCK 1) eof) 3)  ;; /* send block type */
    (set! compressed_len (bitwise-and (+ compressed_len 3 7) (bitwise-not 7)))
    (set! compressed_len (+ compressed_len (<< (+ stored_len 4) 3)))

    (copy_block buf stored_len #t)] ;; /* with header */
   [(= static_lenb opt_lenb)
    (send_bits (+ (<< STATIC_TREES 1) eof) 3)
    (compress_block static_ltree static_dtree)
    (set! compressed_len (+ compressed_len 3 static_len))]
   [else
    (send_bits (+ (<< DYN_TREES 1) eof) 3)
    (send_all_trees (+ (tree_desc-max_code l_desc) 1)
                    (+ (tree_desc-max_code d_desc) 1)
                    (+ max_blindex 1))
    (compress_block dyn_ltree dyn_dtree)
    (set! compressed_len (+ compressed_len 3 opt_len))])

  ;; Assert
  ;; (unless (= compressed_len bits_sent)
  ;;  (error "bad compressed size"))
  (init_block)

  (when (not (= eof 0))
    (Assert
     (unless (= input_len bytes_in)
       (newline (current-error-port))
       (error 'eof "bad input size: ~a != ~a" input_len bytes_in)))
    (bi_windup)
    (set! compressed_len   ;; /* align on byte boundary */
          (+ compressed_len 7)))

  (DEBUG (Tracev stderr "\ncomprlen ~a(~a) " (>> compressed_len 3)
                 (- compressed_len (* 7 eof))))

  (>> compressed_len 3))

;; /* ===========================================================================
;;  * Save the match info and tally the frequency counts. Return true if
;;  * the current block must be flushed.
;;  */
(define ct_tally
  (let ([dist 0])
    (lambda (_dist lc)
      ;; int dist;  ;; /* distance of matched string */
      ;; int lc;    ;; /* match length-MIN_MATCH or unmatched char (if dist==0) */

      (set! dist _dist)

      (bytes-set! l_buf last_lit lc)
      (set! last_lit (add1 last_lit))
      (if (= dist 0)
          ;; /* lc is the unmatched char */
          (set-ct_data-freq! (vector-ref dyn_ltree lc)
                             (add1 (ct_data-freq (vector-ref dyn_ltree lc))))
          (begin
            ;; /* Here, lc is the match length - MIN_MATCH */
            (set! dist (sub1 dist)) ;; /* dist = match distance - 1 */
            (Assert
             (unless (and (< dist MAX_DIST)
                          (<= lc (- MAX_MATCH MIN_MATCH))
                          (< (d_code dist) D_CODES))
               (error "ct_tally: bad match")))

            (let* ([i (+ (vector-ref length_code lc) LITERALS 1)]
                   [ct (vector-ref dyn_ltree i)])
              (DEBUG (Trace stderr "Set: ~a -> ~a\n" lc i))
              (set-ct_data-freq! ct (add1 (ct_data-freq ct))))
            (let ([ct (vector-ref dyn_dtree (d_code dist))])
              (set-ct_data-freq! ct (add1 (ct_data-freq ct))))

            (vector-set! d_buf last_dist dist)
            (set! last_dist (add1 last_dist))
            (set! flags (bitwise-ior flags flag_bit))))

      (set! flag_bit (<< flag_bit 1))

      ;; /* Output the flags if they fill a byte: */
      (when (= (bitwise-and last_lit 7) 0)
        (vector-set! flag_buf last_flags flags)
        (set! last_flags (add1 last_flags))
        (set! flags 0) (set! flag_bit 1))

      (or
       ;; /* Try to guess if it is profitable to stop the current block here */
       (and (and (> LEVEL 2) (= (bitwise-and last_lit #xfff) 0))
            (let ()
              ;; /* Compute an upper bound for the compressed length */
              (define out_length (* last_lit 8))
              (define in_length (- strstart block_start))

              (for dcode := 0 < D_CODES do
                (set! out_length
                      (+ out_length
                         (* (ct_data-freq (vector-ref dyn_dtree dcode))
                            (+ 5 (vector-ref extra_dbits dcode))))))
              (set! out_length (>> out_length 3))
              (DEBUG (Trace stderr "\nlast_lit ~a, last_dist ~a, in ~a, out ~~~a(~a%) "
                            last_lit last_dist in_length out_length
                            (- 100 (/ (* out_length 100) in_length))))
              (and (< last_dist (quotient last_lit 2))
                   (< out_length (quotient in_length 2)))))

       (or (= last_lit (- LIT_BUFSIZE 1))
           (= last_dist DIST_BUFSIZE))
       ;; /* We avoid equality with LIT_BUFSIZE because of wraparound at 64K
       ;;  * on 16 bit machines and because stored blocks are restricted to
       ;;  * 64K-1 bytes.
       ;;  */
       ))))

;; /* ===========================================================================
;;  * Send the block data compressed using the given Huffman trees
;;  */
(define (compress_block ltree dtree)
  ;; ct_data near *ltree; ;; /* literal tree */
  ;; ct_data near *dtree; ;; /* distance tree */

  (define dist 0) ;; /* distance of matched string */
  (define lc 0) ;; /* match length or unmatched char (if dist == 0) */
  (define lx 0) ;; /* running index in l_buf */
  (define  dx 0)    ;; /* running index in d_buf */
  (define fx 0)    ;; /* running index in flag_buf */
  (define flag 0)       ;; /* current flags */
  (define code 0)      ;; /* the code to send */
  (define extra 0) ;; /* number of extra bits to send */

  (when (not (= last_lit 0))
    (let loop ()
      (when (= (bitwise-and lx 7) 0)
        (set! flag (vector-ref flag_buf fx))
        (set! fx (add1 fx)))

      (set! lc (bytes-ref l_buf lx))
      (set! lx (add1 lx))

      (cond
       [(= (bitwise-and flag 1) 0)
        (send_code lc ltree) ;; /* send a literal byte */
        (DEBUG '(Tracecv (isgraph lc) stderr " '~c' " (integer->char lc)))]
       [else
        ;; /* Here, lc is the match length - MIN_MATCH */
        (set! code (vector-ref length_code lc))
        (send_code (+ code LITERALS 1) ltree) ;; /* send the length code */
        (set! extra (vector-ref extra_lbits code))
        (when (not (= extra 0))
          (set! lc (- lc (vector-ref base_length code)))
          (send_bits lc extra))        ;; /* send the extra length bits */
        (set! dist (vector-ref d_buf dx))
        (set! dx (add1 dx))

        ;; /* Here, dist is the match distance - 1 */
        (set! code (d_code dist))
        (Assert
         (unless (< code D_CODES)
           (error "bad d_code")))

        (send_code code dtree)       ;; /* send the distance code */
        (set! extra (vector-ref extra_dbits code))
        (when (not (= extra 0))
          (set! dist (- dist (vector-ref base_dist code)))
          (send_bits dist extra))])   ;; /* send the extra distance bits */
      ;; /* literal or match pair ? */
      (set! flag (>> flag 1))
      (when (< lx last_lit)
        (loop))))

  (send_code END_BLOCK ltree))

#|
/* bits.c -- output variable-length bit strings
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */


/*
 *  PURPOSE
 *
 *      Output variable-length bit strings. Compression can be done
 *      to a file or to memory. (The latter is not supported in this version.)
 *
 *  DISCUSSION
 *
 *      The PKZIP "deflate" file format interprets compressed file data
 *      as a sequence of bits.  Multi-bit strings in the file may cross
 *      byte boundaries without restriction.
 *
 *      The first bit of each byte is the low-order bit.
 *
 *      The routines in this file allow a variable-length bit value to
 *      be output right-to-left (useful for literal values). For
 *      left-to-right output (useful for code strings from the tree routines),
 *      the bits must have been reversed first with bi_reverse().
 *
 *      For in-memory compression, the compressed bit stream goes directly
 *      into the requested output buffer. The input data is read in blocks
 *      by the mem_read() function. The buffer is limited to 64K on 16 bit
 *      machines.
 *
 *  INTERFACE
 *
 *      void bi_init (FILE *zipfile)
 *          Initialize the bit string routines.
 *
 *      void send_bits (int value, int length)
 *          Write out a bit string, taking the source bits right to
 *          left.
 *
 *      int bi_reverse (int value, int length)
 *          Reverse the bits of a bit string, taking the source bits left to
 *          right and emitting them right to left.
 *
 *      void bi_windup (void)
 *          Write out any remaining bits in an incomplete byte.
 *
 *      void copy_block(char *buf, unsigned len, int header)
 *          Copy a stored block to the zip file, storing first the length and
 *          its one's complement if requested.
 *
 */
|#

(define bytes_in 0)

(define bi_buf 0)
;; /* Output buffer. bits are inserted starting at the bottom (least significant
;;  * bits).
;;  */

(define Buf_size (* 8 2))
;; /* Number of bits used within bi_buf. (bi_buf might be implemented on
;;  * more than 16 bits on some systems.)
;;  */

(define bi_valid 0)
;; /* Number of valid bits in bi_buf.  All bits above the last valid bit
;;  * are always zero.
;;  */

;; /* ===========================================================================
;;  * Initialize the bit string routines.
;;  */
(define (bi_init)
  (set! bi_buf 0)
  (set! bi_valid 0)
  (set! bits_sent 0))


;; /* ===========================================================================
;;  * Send a value on a given number of bits.
;;  * IN assertion: length <= 16 and value fits in length bits.
;;  */
(define (send_bits value length)
  ;; int value;  /* value to send */
  ;; int length; /* number of bits */

  (DEBUG (Tracev stderr " l ~a v ~x " length value))
  (Assert
   (unless (and (> length 0) (<= length 15))
     (error "invalid length")))
  (set! bits_sent (+ bits_sent length))

  ;; /* If not enough room in bi_buf, use (valid) bits from bi_buf and
  ;;  * (16 - bi_valid) bits from value, leaving (width - (16-bi_valid))
  ;;  * unused bits in value.
  ;;  */
  (if (> bi_valid (- Buf_size length))
    (begin (put_short (bitwise-and (bitwise-ior bi_buf (<< value bi_valid))
                                   #xFFFF))
           (set! bi_buf (>> value (- Buf_size bi_valid)))
           (set! bi_valid (+ bi_valid (- length Buf_size))))
    (begin (set! bi_buf (bitwise-ior bi_buf (<< value bi_valid)))
           (set! bi_valid (+ bi_valid length)))))

;; /* ===========================================================================
;;  * Reverse the first len bits of a code, using straightforward code (a faster
;;  * method would use a table)
;;  * IN assertion: 1 <= len <= 15
;;  */
(define (bi_reverse code len)
  ;; unsigned code; /* the value to invert */
  ;; int len;       /* its bit length */

  (let loop ([res 0][code code][len len])
    (let ([res (<< (bitwise-ior res (bitwise-and code 1)) 1)])
      (if (> len 1)
          (loop res (>> code 1) (sub1 len))
          (>> res 1)))))

;; /* ===========================================================================
;;  * Write out any remaining bits in an incomplete byte.
;;  */
(define (bi_windup)
  (cond [(> bi_valid 8) (put_short bi_buf)]
        [(> bi_valid 0) (put_byte bi_buf)])
  (set! bi_buf 0)
  (set! bi_valid 0)
  (set! bits_sent (bitwise-and (+ bits_sent 7) (bitwise-not 7))))

;; /* ===========================================================================
;;  * Run a set of bytes through the crc shift register.  If s is a NULL
;;  * pointer, then initialize the crc shift register contents instead.
;;  */
(define crc #xffffffff)
(define (updcrc s n)
  ;; uch *s;                 /* pointer to bytes to pump through */
  ;; unsigned n;             /* number of bytes in s[] */
  (if s
      (let loop ([c crc][p 0])
        (if (= p n)
            (set! crc c)
            (loop (bitwise-xor
                   (vector-ref crc_32_tab
                               (bitwise-and
                                (bitwise-xor c (bytes-ref window-vec (+ s p)))
                                #xff))
                   (arithmetic-shift c -8))
                  (add1 p))))
      (set! crc #xffffffff)))

(define crc_32_tab
   #(#x00000000
     #x77073096 #xee0e612c #x990951ba #x076dc419
     #x706af48f #xe963a535 #x9e6495a3 #x0edb8832 #x79dcb8a4
     #xe0d5e91e #x97d2d988 #x09b64c2b #x7eb17cbd #xe7b82d07
     #x90bf1d91 #x1db71064 #x6ab020f2 #xf3b97148 #x84be41de
     #x1adad47d #x6ddde4eb #xf4d4b551 #x83d385c7 #x136c9856
     #x646ba8c0 #xfd62f97a #x8a65c9ec #x14015c4f #x63066cd9
     #xfa0f3d63 #x8d080df5 #x3b6e20c8 #x4c69105e #xd56041e4
     #xa2677172 #x3c03e4d1 #x4b04d447 #xd20d85fd #xa50ab56b
     #x35b5a8fa #x42b2986c #xdbbbc9d6 #xacbcf940 #x32d86ce3
     #x45df5c75 #xdcd60dcf #xabd13d59 #x26d930ac #x51de003a
     #xc8d75180 #xbfd06116 #x21b4f4b5 #x56b3c423 #xcfba9599
     #xb8bda50f #x2802b89e #x5f058808 #xc60cd9b2 #xb10be924
     #x2f6f7c87 #x58684c11 #xc1611dab #xb6662d3d #x76dc4190
     #x01db7106 #x98d220bc #xefd5102a #x71b18589 #x06b6b51f
     #x9fbfe4a5 #xe8b8d433 #x7807c9a2 #x0f00f934 #x9609a88e
     #xe10e9818 #x7f6a0dbb #x086d3d2d #x91646c97 #xe6635c01
     #x6b6b51f4 #x1c6c6162 #x856530d8 #xf262004e #x6c0695ed
     #x1b01a57b #x8208f4c1 #xf50fc457 #x65b0d9c6 #x12b7e950
     #x8bbeb8ea #xfcb9887c #x62dd1ddf #x15da2d49 #x8cd37cf3
     #xfbd44c65 #x4db26158 #x3ab551ce #xa3bc0074 #xd4bb30e2
     #x4adfa541 #x3dd895d7 #xa4d1c46d #xd3d6f4fb #x4369e96a
     #x346ed9fc #xad678846 #xda60b8d0 #x44042d73 #x33031de5
     #xaa0a4c5f #xdd0d7cc9 #x5005713c #x270241aa #xbe0b1010
     #xc90c2086 #x5768b525 #x206f85b3 #xb966d409 #xce61e49f
     #x5edef90e #x29d9c998 #xb0d09822 #xc7d7a8b4 #x59b33d17
     #x2eb40d81 #xb7bd5c3b #xc0ba6cad #xedb88320 #x9abfb3b6
     #x03b6e20c #x74b1d29a #xead54739 #x9dd277af #x04db2615
     #x73dc1683 #xe3630b12 #x94643b84 #x0d6d6a3e #x7a6a5aa8
     #xe40ecf0b #x9309ff9d #x0a00ae27 #x7d079eb1 #xf00f9344
     #x8708a3d2 #x1e01f268 #x6906c2fe #xf762575d #x806567cb
     #x196c3671 #x6e6b06e7 #xfed41b76 #x89d32be0 #x10da7a5a
     #x67dd4acc #xf9b9df6f #x8ebeeff9 #x17b7be43 #x60b08ed5
     #xd6d6a3e8 #xa1d1937e #x38d8c2c4 #x4fdff252 #xd1bb67f1
     #xa6bc5767 #x3fb506dd #x48b2364b #xd80d2bda #xaf0a1b4c
     #x36034af6 #x41047a60 #xdf60efc3 #xa867df55 #x316e8eef
     #x4669be79 #xcb61b38c #xbc66831a #x256fd2a0 #x5268e236
     #xcc0c7795 #xbb0b4703 #x220216b9 #x5505262f #xc5ba3bbe
     #xb2bd0b28 #x2bb45a92 #x5cb36a04 #xc2d7ffa7 #xb5d0cf31
     #x2cd99e8b #x5bdeae1d #x9b64c2b0 #xec63f226 #x756aa39c
     #x026d930a #x9c0906a9 #xeb0e363f #x72076785 #x05005713
     #x95bf4a82 #xe2b87a14 #x7bb12bae #x0cb61b38 #x92d28e9b
     #xe5d5be0d #x7cdcefb7 #x0bdbdf21 #x86d3d2d4 #xf1d4e242
     #x68ddb3f8 #x1fda836e #x81be16cd #xf6b9265b #x6fb077e1
     #x18b74777 #x88085ae6 #xff0f6a70 #x66063bca #x11010b5c
     #x8f659eff #xf862ae69 #x616bffd3 #x166ccf45 #xa00ae278
     #xd70dd2ee #x4e048354 #x3903b3c2 #xa7672661 #xd06016f7
     #x4969474d #x3e6e77db #xaed16a4a #xd9d65adc #x40df0b66
     #x37d83bf0 #xa9bcae53 #xdebb9ec5 #x47b2cf7f #x30b5ffe9
     #xbdbdf21c #xcabac28a #x53b39330 #x24b4a3a6 #xbad03605
     #xcdd70693 #x54de5729 #x23d967bf #xb3667a2e #xc4614ab8
     #x5d681b02 #x2a6f2b94 #xb40bbe37 #xc30c8ea1 #x5a05df1b
     #x2d02ef8d))

;; /* ===========================================================================
;;  * Copy a stored block to the zip file, storing first the length and its
;;  * one's complement if requested.
;; */
(define (copy_block buf len header)
  ;; char     *buf;    /* the input data */
  ;; unsigned len;     /* its length */
  ;; int      header;  /* true if block header must be written */

  (bi_windup);; /* align on byte boundary */

  (when header
    (put_short len)
    (put_short (bitwise-and (bitwise-not len) #xFFFF))
    (set! bits_sent (+ bits_sent (* 2 16))))

  (set! bits_sent (+ bits_sent (<< len 3)))

  (for pos := 0 < len do (put_byte (gzbytes-ref buf pos))))

;; /* ===========================================================================
;;  * Read a new buffer from the current input file, perform end-of-line
;;  * translation, and update the crc and input file size.
;;  * IN assertion: size >= 2 (for end-of-line translation)
;;  */
(define (read_buf startpos size)
  ;; char *buf;
  ;; unsigned size;

  ;; Assert
  ;; (unless (= insize 0)
  ;;   (error "inbuf not empty"))

  (let* ([s (read-bytes! window-vec ifd startpos (+ size startpos))]
         [len (if (eof-object? s) EOF-const s)])
    (when (positive? len)
      (updcrc startpos len)
      (set! bytes_in (+ bytes_in len)))
    len))

;; Assumes being called with c in 0..FF
(define-syntax put_byte
  (syntax-rules ()
    [(_ c)
     (begin (bytes-set! outbuf outcnt c)
            (set! outcnt (add1 outcnt))
            (when (= outcnt OUTBUFSIZ) (flush_outbuf)))]))

;; /* Output a 16 bit value, lsb first */
;; Assumes being called with c in 0..FFFF
(define (put_short w)
  (if (< outcnt (- OUTBUFSIZ 2))
    (begin (bytes-set! outbuf outcnt (bitwise-and #xFF w))
           (bytes-set! outbuf (add1 outcnt) (>> w 8))
           ;; this is not faster...
           ;; (integer->integer-bytes w 2 #f #f outbuf outcnt)
           (set! outcnt (+ outcnt 2)))
    (begin (put_byte (bitwise-and #xFF w))
           (put_byte (>> w 8)))))

;; /* Output a 32 bit value to the bit stream, lsb first */
(define (put_long n)
  (put_short (bitwise-and #xFFFF n))
  (put_short (bitwise-and #xFFFF (>> n 16))))

(define outcnt 0)
(define bytes_out 0)
(define outbuf (make-bytes OUTBUFSIZ))

;; /* ===========================================================================
;;  * Write the output buffer outbuf[0..outcnt-1] and update bytes_out.
;;  * (used for the compressed data only)
;;  */
(define (flush_outbuf)
  (unless (= outcnt 0)

    (write-bytes outbuf ofd 0 outcnt)

    (set! bytes_out (+ bytes_out outcnt))
    (set! outcnt 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ifd #f)
(define ofd #f)

(define (deflate-inner in out)
  (do-deflate))

(define (deflate in out)

  (set! bytes_in 0)

  (set! ifd in)
  (set! ofd out)
  (set! outcnt 0)

  (bi_init)
  (ct_init)
  (lm_init LEVEL)

  (deflate-inner in out)

  (flush_outbuf)

  (values bytes_in bytes_out (bitwise-xor crc #xffffffff)))

(define (gzip-through-ports in out origname time_stamp)

  (define flags (if origname #x8 0)) ;; /* general purpose bit flags */

  ;; make origname be a byte string
  (set! origname (cond [(not origname)     #f]
                       [(string? origname) (string->bytes/utf-8 origname)]
                       [(path? origname)   (path->bytes origname)]
                       [else               origname]))

  (set! bytes_in 0)

  (set! ifd in)
  (set! ofd out)
  (set! outcnt 0)

  ;; /* Write the header to the gzip file. See algorithm.doc for the format */
  (put_byte #o037) ;; /* magic header */
  (put_byte #o213)
  (put_byte 8) ;; /* compression method */

  (put_byte flags);; /* general flags */
  (put_long time_stamp);

  ;; /* Write deflated file to zip file */
  (updcrc #f 0)

  (bi_init)
  (ct_init)

  (put_byte (lm_init LEVEL));; /* extra flags */
  (put_byte 3) ;; /* OS identifier */

  (when origname
    (for-each (lambda (b) (put_byte b)) (bytes->list origname))
    (put_byte 0))

  (do-deflate)

  ;; /* Write the crc and uncompressed size */
  (put_long (bitwise-xor crc #xffffffff))
  (put_long bytes_in)

  (flush_outbuf))

(define (gzip infile outfile)
  (let ([i (open-input-file infile)])
    (dynamic-wind
     void
     (lambda ()
       (let ([o (open-output-file outfile 'truncate/replace)])
         (dynamic-wind
          void
          (lambda ()
            (let ([name (with-handlers ([exn:fail? (lambda (x) #f)])
                          (let-values ([(base name dir?) (split-path infile)])
                            name))]
                  [timestamp (with-handlers ([exn:fail:filesystem? (lambda (x) 0)])
                               (file-or-directory-modify-seconds infile))])
              (gzip-through-ports i o name timestamp)))
          (lambda () (close-output-port o)))))
     (lambda () (close-input-port i)))))

(list gzip gzip-through-ports deflate)))

(define gzip
  (case-lambda
   [(infile) (gzip infile (string-append infile ".gz"))]
   [(infile outfile) ((car (invoke-unit code)) infile outfile)]))

(define (gzip-through-ports in out origname time_stamp)
  ((cadr (invoke-unit code)) in out origname time_stamp))

(define (deflate in out)
  ((caddr (invoke-unit code)) in out))

)
