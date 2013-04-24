#lang racket/base
(require (for-syntax racket/base))

  (provide inflate 
	  gunzip-through-ports
	  gunzip)

#|

/* inflate.c -- Not copyrighted 1992 by Mark Adler
   version c10p1, 10 January 1993 */
; Taken from the gzip source distribution
; Translated directly from C (obviously) by Matthew, April 1997

/* You can do whatever you like with this source file, though I would
   prefer that if you modify it and redistribute it that you include
   comments to that effect with your name and the date.  Thank you.
   [The history has been moved to the file ChangeLog.]
   ; ChangeLog is distributed with the gzip source. 
 */

/*
   Inflate deflated (PKZIP's method 8 compressed) data.  The compression
   method searches for as much of the current string of bytes (up to a
   length of 258) in the previous 32K bytes.  If it doesn't find any
   matches (of at least length 3), it codes the next byte.  Otherwise, it
   codes the length of the matched string and its distance backwards from
   the current position.  There is a single Huffman code that codes both
   single bytes (called "literals") and match lengths.  A second Huffman
   code codes the distance information, which follows a length code.  Each
   length or distance code actually represents a base value and a number
   of "extra" (sometimes zero) bits to get to add to the base value.  At
   the end of each deflated block is a special end-of-block (EOB) literal/
   length code.  The decoding process is basically: get a literal/length
   code; if EOB then done; if a literal, emit the decoded byte; if a
   length then get the distance and emit the referred-to bytes from the
   sliding window of previously emitted data.

   There are (currently) three kinds of inflate blocks: stored, fixed, and
   dynamic.  The compressor deals with some chunk of data at a time, and
   decides which method to use on a chunk-by-chunk basis.  A chunk might
   typically be 32K or 64K.  If the chunk is uncompressible, then the
   "stored" method is used.  In this case, the bytes are simply stored as
   is, eight bits per byte, with none of the above coding.  The bytes are
   preceded by a count, since there is no longer an EOB code.

   If the data is compressible, then either the fixed or dynamic methods
   are used.  In the dynamic method, the compressed data is preceded by
   an encoding of the literal/length and distance Huffman codes that are
   to be used to decode this block.  The representation is itself Huffman
   coded, and so is preceded by a description of that code.  These code
   descriptions take up a little space, and so for small blocks, there is
   a predefined set of codes, called the fixed codes.  The fixed method is
   used if the block codes up smaller that way (usually for quite small
   chunks), otherwise the dynamic method is used.  In the latter case, the
   codes are customized to the probabilities in the current block, and so
   can code it much better than the pre-determined fixed codes.
 
   The Huffman codes themselves are decoded using a mutli-level table
   lookup, in order to maximize the speed of decoding plus the speed of
   building the decoding tables.  See the comments below that precede the
   lbits and dbits tuning parameters.
 */


/*
   Notes beyond the 1.93a appnote.txt:

   1. Distance pointers never point before the beginning of the output
      stream.
   2. Distance pointers can point back across blocks, up to 32k away.
   3. There is an implied maximum of 7 bits for the bit length table and
      15 bits for the actual data.
   4. If only one code exists, then it is encoded using one bit.  (Zero
      would be more efficient, but perhaps a little confusing.)  If two
      codes exist, they are coded using one bit each (0 and 1).
   5. There is no way of sending zero distance codes--a dummy must be
      sent if there are none.  (History: a pre 2.0 version of PKZIP would
      store blocks with no distance codes, but this was discovered to be
      too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
      zero distance codes, which is sent as one code of zero bits in
      length.
   6. There are up to 286 literal/length codes.  Code 256 represents the
      end-of-block.  Note however that the static length tree defines
      288 codes just to fill out the Huffman codes.  Codes 286 and 287
      cannot be used though, since there is no length base or extra bits
      defined for them.  Similarly, there are up to 30 distance codes.
      However, static trees define 32 codes (all 5 bits) to fill out the
      Huffman codes, but the last two had better not show up in the data.
   7. Unzip can check dynamic Huffman blocks for complete code sets.
      The exception is that a single code would not be complete (see #4).
   8. The five bits following the block type is really the number of
      literal codes sent minus 257.
   9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
      (1+6+6).  Therefore, to output three times the length, you output
      three codes (1+1+1), whereas to output four times the same length,
      you only need two codes (1+3).  Hmm.
  10. In the tree reconstruction algorithm, Code = Code + Increment
      only if BitLength(i) is not zero.  (Pretty obvious.)
  11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
  12. Note: length code 284 can represent 227-258, but length code 285
      really is 258.  The last length deserves its own, short code
      since it gets used a lot in very redundant files.  The length
      258 is special since 258 - 3 (the min match length) is 255.
  13. The literal/length and distance code bit lengths are read as a
      single stream of lengths.  It is possible (and advantageous) for
      a repeat code (16, 17, or 18) to go across the boundary between
      the two sets of lengths.
 */

|#

#|
/* Huffman code lookup table entry--this entry is four bytes for machines
   that have 16-bit pointers (e.g. PC's in the small or medium model).
   Valid extra bits are 0..13.  e == 15 is EOB (end of block), e == 16
   means that v is a literal, 16 < e < 32 means that v is a pointer to
   the next table, which codes e - 16 bits, and lastly e == 99 indicates
   an unused code.  If a code with e == 99 is looked up, this implies an
   error in the data. */
|#

  (define-struct huft (e b v) #:mutable)
  
  (define (huft-copy dest src)
    (set-huft-e! dest (huft-e src))
    (set-huft-b! dest (huft-b src))
    (set-huft-v! dest (huft-v src)))
  
  (define (step start < end add1 f)
    (let loop ([i start])
      (when (< i end)
	(f i)
	(loop (add1 i)))))
  
  (define (subvector v offset)
    (let* ([len (- (vector-length v) offset)]
	   [new (make-vector len)])
      (step 0 < len add1 
	    (lambda (i) 
	      (vector-set! new i (vector-ref v (+ i offset)))))
      new))

  (define (build-vector n p)
    (let ([v (make-vector n)])
      (step 0 < n add1 (lambda (i) (vector-set! v i (p i))))
      v))

  ;; We know that inflating will be a bottleneck, so we might as
  ;;  well help out the compiler...
  (define-syntax define-const 
    (syntax-rules ()
      [(_ id v) (define-syntax id (make-const #'v))]))
  (define-for-syntax (make-const val)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
	 [(set! id . _) (raise-syntax-error (syntax-e #'id)
					    "cannot assign constant"
					    stx)]
	 [(id . rest) (quasisyntax/loc stx (#,val . rest))]
	 [id val]))))

#|
/* The inflate algorithm uses a sliding 32K byte window on the uncompressed
   stream to find repeated byte strings.  This is implemented here as a
   circular buffer.  The index is updated simply by incrementing and then
   and'ing with 0x7fff (32K-1). */
|#

  (define-const WSIZE 32768)

  (define border 
    (vector 
     16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
  
  (define cplens
    (vector
     3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
     35 43 51 59 67 83 99 115 131 163 195 227 258 0 0))
  ; /* note: see note #13 above about the 258 in this list. */
  (define cplext
    (vector
     0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2
     3 3 3 3 4 4 4 4 5 5 5 5 0 99 99)) ; /* 99==invalid */
  (define cpdist
    (vector
     1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
     257 385 513 769 1025 1537 2049 3073 4097 6145
     8193 12289 16385 24577))
  (define cpdext
    (vector
     0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6
     7 7 8 8 9 9 10 10 11 11
     12 12 13 13))
  
  (define mask_bits
    (vector
     #x0000
     #x0001 #x0003 #x0007 #x000f #x001f #x003f #x007f #x00ff
     #x01ff #x03ff #x07ff #x0fff #x1fff #x3fff #x7fff #xffff))

  (define-const lbits 9) ; /* bits in base literal/length lookup table */
  (define-const dbits 6) ; /* bits in base distance lookup table */


  ; /* If BMAX needs to be larger than 16, then h and x[] should be ulg. */
  (define-const BMAX 16) ; /* maximum bit length of any code (16 for explode) */
  (define-const N_MAX 288) ; /* maximum number of codes in any set */

(define (inflate input-port output-port)

  (define slide (make-bytes WSIZE))
  (define wp 0)
  
  (define (flush-output len)
    ; write out the data
    (write-bytes slide output-port 0 len))
  
  (define (check-flush)
    (when (= wp WSIZE)
      (flush-output WSIZE)
      (set! wp 0)))

#|
/* Macros for inflate() bit peeking and grabbing.
   The usage is:
   
        NEEDBITS(j)
        x = b & mask_bits[j];
        DUMPBITS(j)

   where NEEDBITS makes sure that b has at least j bits in it, and
   DUMPBITS removes the bits from b.  The macros use the variable k
   for the number of bits in b.  Normally, b and k are register
   variables for speed, and are initialized at the beginning of a
   routine that uses these macros from a global bit buffer and count.

   If we assume that EOB will be the longest code, then we will never
   ask for bits with NEEDBITS that are beyond the end of the stream.
   So, NEEDBITS should not read any more bytes than are needed to
   meet the request.  Then no bytes need to be "returned" to the buffer
   at the end of the last block.

   However, this assumption is not true for fixed blocks--the EOB code
   is 7 bits, but the other literal/length codes can be 8 or 9 bits.
   (The EOB code is shorter than other codes because fixed blocks are
   generally short.  So, while a block always has an EOB, many other
   literal/length codes have a significantly lower probability of
   showing up at all.)  However, by making the first table have a
   lookup of seven bits, the EOB code will be found in that first
   lookup, and so will not require that too many bits be pulled from
   the stream.
 */
|#

  ;; We can't read the bytes outright, because we may
  ;; look ahead. Assume that we need no more than 8 bytes
  ;; look ahead, and peek in 4096-byte blocks.
  (define MAX-LOOKAHEAD 8)
  (define BUFFER-SIZE 4096)
  (define buffer (make-bytes BUFFER-SIZE))
  (define buf-max 0) ; number of bytes in buffer
  (define buf-pos 0) ; index into buffer = number of used peeked bytes

  (define bb 0) ; /* bit buffer */
  (define bk 0) ; /* bits in bit buffer */
  
  (define (NEEDBITS n)
    (when (< bk n)
      (READBITS n)))
  (define (READBITS n)
    (if (= buf-pos buf-max)
        (begin
          (when (positive? buf-max)
            ;; Read consumed bytes, except for the last MAX-LOOKAHEAD bytes,
            ;; which we might unwind:
            (read-bytes! buffer input-port 0 (max 0 (- buf-max MAX-LOOKAHEAD)))
            ;; Even though we won't actually use bytes that we "unwind",
            ;; setting `buf-pos' to the number of unwound bytes lets us
            ;; keep track of how much to not actually read at the end.
            (set! buf-pos (min MAX-LOOKAHEAD buf-max)))
          ;; Peek (not read) some available bytes:
          (let ([got (peek-bytes-avail! buffer buf-pos #f input-port buf-pos BUFFER-SIZE)])
            (if (eof-object? got)
                ;; Treat an EOF as a -1 "byte":
                (begin
                  (bytes-set! buffer buf-pos 255)
                  (set! buf-max (add1 buf-pos)))
                ;; Got normal bytes:
                (set! buf-max (+ buf-pos got))))
          (READBITS n))
        (let ([v (bytes-ref buffer buf-pos)])
          (set! buf-pos (add1 buf-pos))
          (set! bb (+ bb (arithmetic-shift v bk)))
          (set! bk (+ bk 8))
          (NEEDBITS n))))
  (define (DUMPBITS n)
    (set! bb (arithmetic-shift bb (- n)))
    (set! bk (- bk n)))
  
  (define (GETBITS n)
    (NEEDBITS n)
    (begin0
      bb
      (DUMPBITS n)))
    
#|
/*
   Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   is not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary.
 */
|#

  (define (huft_build
	   b ; int vector           /* code lengths in bits (all assumed <= BMAX) */ 
	   n ;             /* number of codes (assumed <= N_MAX) */
	   s ;             /* number of simple-valued codes (0..s-1) */
	   d ; int vector                /* list of base values for non-simple codes */
	   e ; int vector                /* list of extra bits for non-simple codes */
	   m ; int                /* maximum lookup bits, returns actual */
	   incomp-ok?)
    ; return: new-t new-m ok?

#|
/* Given a list of code lengths and a maximum table size, make a set of
   tables to decode that set of codes.  Return zero on success, one if
   the given code set is incomplete (the tables are still built in this
   case), two if the input is invalid (all zero length codes or an
   oversubscribed set of lengths), and three if not enough memory. */
|#
    (define c (make-vector (add1 BMAX) 0))
    (define x (make-vector (add1 BMAX)))
    (define v (make-vector N_MAX))
    
    (define final-y 0)
    (define t-result #f)
    
    ; (printf "n: ~s\n" n)

    (let/ec return

#|
(if (= n 270)
    (step 0 < n add1
	     (lambda (i) (printf "b[~a] = ~a\n" i (vector-ref b i)))))
|#

      (step 0 < n add1
	    (lambda (i) 
	      (let ([pos (vector-ref b i)])
		(vector-set! c pos (add1 (vector-ref c pos))))))

      (when (= n (vector-ref c 0))
	; (printf "zero\n")
	(return #f 0 #t))

#|
(when (= n 270)
  (step 0 <= BMAX add1
	(lambda (i)
	  (printf "c[~s]: ~s\n" i (vector-ref c i)))))
|#

      ; /* Find minimum and maximum length, bound m-result by those */
      (let* ([j ; min-code-length
	      (let loop ([j 1])
		(cond
		  [(> j BMAX) j]
		  [(positive? (vector-ref c j)) j]
		  [else (loop (add1 j))]))]
	     [k j]
	     [i ; max-code-length
	      (let loop ([i BMAX])
		(cond
		  [(zero? i) 0]
		  [(positive? (vector-ref c i)) i]
		  [else (loop (sub1 i))]))]
	     [g i]
	     [l (min (max m j) i)]
	     [m-result l])
	; (printf "min: ~s max: ~s\n" k g)
	; /* Adjust last length count to fill out codes, if needed */
	(let-values ([(y j)
		      (let loop ([y (arithmetic-shift 1 j)][j j])
			(if (>= j i)
			    (values y j)
			    (let ([new-y (- y (vector-ref c j))])
			      (if (negative? new-y) 
				  (begin
				    (error 'inflate 
					   "bad input: more codes than bits")
				    (return null m-result #f))
				  (loop (* new-y 2) (add1 j))))))])
	  ; (printf "loop y: ~s\n" y)
	  (let ([y (- y (vector-ref c i))])
	    (when (negative? y)
	      (error 'inflate "bad input: more codes than bits")
	      (return #f m-result #f))
	    ; (printf "set c[~s] ~s + ~s\n" i (vector-ref c i) y)
	    (vector-set! c i (+ (vector-ref c i) y))
	    (set! final-y y)))
	; /* Generate starting offsets into the value table for each length */
	(vector-set! x 1 0)
	(let* ([j (let loop ([i (sub1 i)][x-pos 2][c-pos 1][j 0])
		    (if (zero? i)
			j
			(let ([v (vector-ref c c-pos)])
			  (vector-set! x x-pos (+ j v))
			  (loop (sub1 i) (add1 x-pos) (add1 c-pos) (+ j v)))))])
	  ; /* Make a table of values in order of bit lengths */
	  (let loop ([i 0][b-pos 0])
	    (let ([j (vector-ref b b-pos)])
	      (unless (zero? j)
		(let ([xj (vector-ref x j)])
		  (vector-set! x j (add1 xj))
		  (vector-set! v xj i)))
	      (let ([new-i (add1 i)])
		(when (< new-i n)
		  (loop new-i (add1 b-pos))))))
	  
	  ; /* Generate the Huffman codes and for each, make the table entries */
	  (vector-set! x 0 0) ; /* first Huffman code is zero */
	  (let ([v-pos 0] ; /* grab values in bit order */
		[i 0]     ; /* the Huffman code of length k bits for value *p */
		[h -1]    ; /* no tables yet--level -1 */
		[w (- l)] ; /* bits decoded == (l * h) */
		[u (make-vector BMAX)] ; /* table stack */
		[q null]  ; /* points to current table */
		[z 0]     ; /* number of entries in current table */
		[r (make-huft 0 0 0)]) ; /* table entry for structure assignment */
	    ; /* go through the bit lengths (k already is bits in shortest code) */
	    (let k-loop ([k k])
	      ; (printf "k: ~s\n" k)
	      (when (<= k g)
		(let ([a (vector-ref c k)])
		  (let a-loop ([a (sub1 a)])
		    (unless (negative? a)
		      ; (printf "a: ~s\n" a)
		      ; /* here i is the Huffman code of length k bits for value *p */
		      ; /* make tables up to required level */
		      (let kwl-loop ()
			(when (> k (+ w l))
			  (set! h (add1 h))
			  (set! w (+ w l)) ; /* previous table always l bits */

			  ; /* compute minimum size table less than or equal to l bits */
			  (set! z (min (- g w) l)) ; /* upper limit on table size */

			  ; (printf "z: ~s k: ~s w: ~s\n" z k w)
			  
			  (let* ([j (- k w)]
				 [f (arithmetic-shift 1 j)])
			    (when (> f (add1 a)) ; /* try a k-w bit table */
			      ; /* too few codes for k-w bit table */
			      (set! f (- f a 1)) ; /* deduct codes from patterns left */
			      ; /* try smaller tables up to z bits */
			      (let loop ([c-pos k])
				(set! j (add1 j))
				(when (< j z)
				  (set! f (* f 2))
				  (let* ([c-pos (add1 c-pos)]
					 [cv (vector-ref c c-pos)])
				    (if (<= f cv)
					(void) ; /* enough codes to use up j bits */
					(begin
					  (set! f (- f cv)) ; /* else deduct codes from patterns */
					  (loop c-pos)))))))
			    (set! z (arithmetic-shift 1 j)) ; /* table entries for j-bit table */

			    ; /* allocate and link in new table */
			    ; (printf "alloc: ~a\n" z)
			    (set! q (build-vector z (lambda (i) (make-huft 0 0 0))))
			    
			    (when (not t-result)
			      (set! t-result q))
			    
			    (vector-set! u h q)
			    
			    ; /* connect to last table, if there is one */
			    (unless (zero? h)
			      (vector-set! x h i) ; /* save pattern for backing up */
			      (set-huft-b! r l) ; /* bits to dump before this table */
			      (set-huft-e! r (+ j 16)); /* bits in this table */
			      (set-huft-v! r q) ; /* pointer to this table */
			      (set! j (arithmetic-shift i (- l w)))
			      ; /* connect to last table: */
			      (huft-copy (vector-ref (vector-ref u (sub1 h)) j) r))) 
			  
			  (kwl-loop)))
		      
		      (set-huft-b! r (- k w)) ; cast uch  (- k w) if needed
		      (if (>= v-pos n)
			  (set-huft-e! r 99) ; /* out of values--invalid code */
			  (let ([vv (vector-ref v v-pos)])
			    ; (printf "*p: ~s s: ~s\n" vv s)
			    (if (< vv s)
				(begin
				  (set-huft-e! r (if (< vv 256) 16 15)) ; /* 256 is end-of-block code */
				  (set-huft-v! r vv)) ; /* simple code is just the value */
				(begin
				  (set-huft-e! r (vector-ref e (- vv s))) ; /* non-simple--look up in lists */
				  (set-huft-v! r (vector-ref d (- vv s)))))
			    (set! v-pos (add1 v-pos))))
		      ; /* fill code-like entries with r */
		      ; (printf "i: ~s w: ~s k: ~s\n" i w k)
		      (let ([f (arithmetic-shift 1 (- k w))]) ; /* i repeats in table every f entries */
			(let loop ([j (arithmetic-shift i (- w))])
			  (when (< j z)
			    (huft-copy (vector-ref q j) r)
			    (loop (+ j f)))))
		      ; /* backwards increment the k-bit code i */
		      (let loop ([j (arithmetic-shift 1 (sub1 k))])
			(if (positive? (bitwise-and i j))
			    (begin
			      (set! i (bitwise-xor i j))
			      (loop (arithmetic-shift j -1)))
			    (set! i (bitwise-xor i j))))
		      ; /* backup over finished tables */
		      (let loop ()
			(unless (= (vector-ref x h) (bitwise-and i (sub1 (arithmetic-shift 1 w))))
			  (set! h (sub1 h)) ; /* don't need to update q */
			  (set! w (- w l))
			  (loop)))
		      
		      (a-loop (sub1 a))))
		  (k-loop (add1 k)))))
	    
	    ; /* Return #f as third if we were given an incomplete table */
	    ; (printf "done: ~s ~s\n" final-y g)
	    (let ([ok? (or incomp-ok?
			   (not (and (not (zero? final-y))
				     (not (= g 1)))))])
	      (unless ok? 
		      (error 'inflate "incomplete table"))
	      (values t-result m-result ok?)))))))
  
  (define (inflate_codes
	   tl ; vector of hufts ; /* literal/length tables */
	   td ; vector of hufts ; /* distance decoder tables */
	   bl ; /* number of bits decoded by tl */
	   bd) ; /* number of bits decoded by td[] */
    ; /* inflate (decompress) the codes in a deflated (compressed) block.
    ;    Return an error code or zero if it all goes ok. */

    ; /* inflate the coded data */

    ; /* precompute masks for speed */
    (define ml (vector-ref mask_bits bl))
    (define md (vector-ref mask_bits bd))
    (define t (void))
    (define e 0)
    (define n 0)
    (define d 0)
    
    (let/ec return

      (define (jump-to-next)
	(let loop ()
	  (when (= e 99)
	    (error 'inflate "bad inflate code")
	    (return #f))
	  (DUMPBITS (huft-b t))
	  (set! e (- e 16))
	  (NEEDBITS e)
	  (set! t (vector-ref (huft-v t) (bitwise-and bb (vector-ref mask_bits e))))
	  (set! e (huft-e t))
	  (when (> e 16)
	    (loop))))
      
      (let loop () ; /* do until end of block */
	(NEEDBITS bl)
	(set! t (vector-ref tl (bitwise-and bb ml)))
	; (printf "t->e: ~s t->b: ~s\n" (huft-e t) (huft-b t))
	(set! e (huft-e t))
	(when (> e 16)
          (jump-to-next))
	(DUMPBITS (huft-b t))
	; (printf "e: ~s\n" e)
	(if (= e 16) ; /* then it's a literal */
	    (begin
	      (bytes-set! slide wp (huft-v t))
	      (set! wp (add1 wp))
	      (check-flush))
	    (begin ; /* it's an EOB or a length */
	      ; /* exit if end of block */
	      (when (= e 15)
		(return #t))
	      
	      ; /* get length of block to copy */
	      (NEEDBITS e)
	      (set! n (+ (huft-v t) (bitwise-and bb (vector-ref mask_bits e))))
	      (DUMPBITS e)
	      ; (printf "n: ~s bb: ~s md: ~s\n" n bb md)
	      
	      ; /* decode distance of block to copy */
	      (NEEDBITS bd)
	      (set! t (vector-ref td (bitwise-and bb md)))
	      ; (printf "t->e: ~s t->b: ~s\n" (huft-e t) (huft-b t))
	      (set! e (huft-e t))
	      ; (printf "e: ~s\n" e)
	      (when (> e 16)
		(jump-to-next))
	      (DUMPBITS (huft-b t))
	      ; (printf "e: ~s\n" e)
	      
	      (NEEDBITS e)
	      (set! d (modulo (- wp (huft-v t) (bitwise-and bb (vector-ref mask_bits e))) WSIZE))
	      (DUMPBITS e)
	      
	      ; (printf "wp: ~s t->v: ~s d: ~s\n" wp (huft-v t) d)
	      
	      ; /* do the copy */
	      (let loop ()
		(set! d (bitwise-and d (sub1 WSIZE)))
		(set! e (min n (- WSIZE (max d wp))))
		(set! n (- n e))
		(let loop ()
		  (bytes-set! slide wp (bytes-ref slide d))
		  (set! wp (add1 wp))
		  (set! d (add1 d))
		  (set! e (sub1 e))
		  (unless (zero? e)
		    (loop)))
		(check-flush)
		(unless (zero? n)
		  (loop)))))
	(loop))))
  
  (define (inflate_stored)
    ; /* "decompress" an inflated type 0 (stored) block. */
    
    (let/ec return

      ; /* go to byte boundary */
      (DUMPBITS (bitwise-and bk 7))
      
      ; /* get the length and its complement */
      (NEEDBITS 16)
      (let ([n (bitwise-and bb #xffff)])
	(DUMPBITS 16)
	(NEEDBITS 16)
	(unless (= n (bitwise-and (bitwise-not bb) #xffff))
	  (error 'inflate "error in compressed data")
	  (return #f)) ; /* error in compressed data */
	(DUMPBITS 16)
	
	; /* read and output the compressed data */
	(let loop ([n n])
	  (when (positive? n)
	    (NEEDBITS 8)
	    (bytes-set! slide wp (bitwise-and bb #xff))
	    (set! wp (add1 wp))
	    (check-flush)
	    (DUMPBITS 8)
	    (loop (sub1 n))))
	
	#t)))

  (define (inflate_fixed)
    ; /* decompress an inflated type 1 (fixed Huffman codes) block.  We should
    ;    either replace this with a custom decoder, or at least precompute the
    ;    Huffman tables. */

    (define l (make-vector 288))
    
    (step 0 < 144 add1 (lambda (i) (vector-set! l i 8)))
    (step 144 < 256 add1 (lambda (i) (vector-set! l i 9)))
    (step 256 < 280 add1 (lambda (i) (vector-set! l i 7)))
    (step 280 < 288 add1 (lambda (i) (vector-set! l i 8)))
    
    (let-values ([(tl bl ok?)
		  (huft_build l 288 257 cplens cplext 7 #f)])
      
      (if (not ok?)
	  #f
	  (begin
	    (step 0 < 30 add1 (lambda (i) (vector-set! l i 5)))
	    (let-values ([(td bd ok?)
			  (huft_build l 30 0 cpdist cpdext 5 #t)])
	      (if (not ok?)
		  #f
		  ; /* decompress until an end-of-block code */
		  (inflate_codes tl td bl bd)))))))

  (define (inflate_dynamic)
    ; /* decompress an inflated type 2 (dynamic Huffman codes) block. */

    (begin ; let/ec return

      ; /* read in table lengths */
      ; (define junk1 (begin (NEEDBITS 5) (printf "~s ~s\n" bb bk)))
      (define nl (+ 257 (bitwise-and (GETBITS 5) #x1f)))
      ; (define junk2 (begin (NEEDBITS 5) (printf "~s ~s\n" bb bk)))
      (define nd (+ 1 (bitwise-and (GETBITS 5) #x1f)))
      ; (define junk3 (begin (NEEDBITS 4) (printf "~s ~s\n" bb bk)))
      (define nb (+ 4 (bitwise-and (GETBITS 4) #xf)))

      ; (define junk8 (printf "~s ~s ~s\n" nl nd nb))

      (define ll (make-vector (+ 286 30)))
      (define i 0)
      (define l 0)

      (if (or (> nl 286) (> nd 30))
	  (begin
	    (error 'inflate "bad lengths")
	    #f) ; /* bad lengths */
	  (begin
	    ; /* read in bit-length-code lengths */
	    (step 0 < nb add1 
		  (lambda (j) 
		    (vector-set! ll (vector-ref border j) (bitwise-and (GETBITS 3) 7))))
	    (step nb < 19 add1 
		  (lambda (j) 
		    (vector-set! ll (vector-ref border j) 0)))
	    
	    ; /* build decoding table for trees--single level, 7 bit lookup */
	    (let-values ([(tl bl ok?)
			  (huft_build ll 19 19 null null 7 #f)])
	      (if (not ok?)
		  #f
		  (begin
		    ; /* read in literal and distance code lengths */
		    (let ([n (+ nl nd)]
			  [m (vector-ref mask_bits bl)])
		      ; (printf "bl: ~s\n" bl)
		      (set! i 0)
		      (set! l 0)
		      (let loop ()
			(when (< i n)
			  (NEEDBITS bl)
			  (let* ([pos (bitwise-and bb m)]
				 [td (vector-ref tl pos)]
				 [dmp (huft-b td)]
				 [j (huft-v td)]
				 [set-lit
				  (lambda (j l)
				    (when (> (+ i j) n)
				      (error 'inflate "bad hop")
                                      #;(return #f))
				    (let loop ([j j])
				      (unless (zero? j)
					(vector-set! ll i l)
					(set! i (add1 i))
					(loop (sub1 j)))))])
			    (DUMPBITS dmp)
			    ; (printf "pos: ~s j: ~s l: ~s i: ~s\n" pos j l i)
			    (cond
			      [(< j 16) ; /* length of code in bits (0..15) */
			       (vector-set! ll i j)
			       (set! l j) ; /* save last length in l */
			       (set! i (add1 i))]
			      [(= j 16) ; /* repeat last length 3 to 6 times */
			       (let ([j (+ 3 (bitwise-and (GETBITS 2) 3))])
				 (set-lit j l))]
			      [(= j 17) ; /* 3 to 10 zero length codes */
			       (let ([j (+ 3 (bitwise-and (GETBITS 3) 7))])
				 (set-lit j 0)
				 (set! l 0))]
			      [else ; /* j == 18: 11 to 138 zero length codes */
			       (let ([j (+ 11 (bitwise-and (GETBITS 7) #x7f))])
				 (set-lit j 0)
				 (set! l 0))]))
			  (loop)))
		      
		      ; /* build the decoding tables for literal/length and distance codes */
		      (let-values ([(tl bl ok?)
				    (huft_build ll nl 257 cplens cplext lbits #f)])
			(if (not ok?)
			    (begin
			      (error 'inflate "incomplete code set")
			      #f) ; /* incomplete code set */
			    (let-values ([(td bd ok?)
					  (huft_build (subvector ll nl) nd 0 cpdist cpdext dbits #f)])
			      (if (not ok?)
				  (begin
				    (error 'inflate "incomplete code set")
				    #f) ; /* incomplete code set */
				  ; /* decompress until an end-of-block code */
				  (inflate_codes tl td bl bd)))))))))))))

  (define (inflate_block)
    ; return values: /* last block flag */ ok?
    ; /* decompress an inflated block */

    (define e-result (bitwise-and (GETBITS 1) 1))
    
    ; /* read in block type */
    (define t (bitwise-and (GETBITS 2) 3))

    (values e-result
	    (case t
	      [(2) (inflate_dynamic)]
	      [(0) (inflate_stored)]
	      [(1) (inflate_fixed)]
	      [else (error 'inflate "unknown inflate type")
		    #f])))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ; inflate starts here
  ;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; /* decompress an inflated entry */
  ; /* initialize window, bit buffer */
  (set! wp 0)
  (set! bk 0)
  (set! bb 0)

  
  ; /* decompress until the last block */
  (let loop ()
    (let-values ([(e ok?) (inflate_block)])
      (if ok?
	  (if (zero? e)
	      (loop)
	      (begin
		; /* Undo too much lookahead. The next read will be byte aligned so we
		; * can discard unused bits in the last meaningful byte.
		; */
		(let loop ()
		  (when (>= bk 8)
		    (set! bk (- bk 8))
		    (set! buf-pos (sub1 buf-pos))
		    (loop)))
                (read-bytes! buffer input-port 0 buf-pos) ; read consumed bytes
		(flush-output wp)
		#t = (void)))
	  #f))))

  (define make-small-endian
    (case-lambda
     [(a b) (+ a (arithmetic-shift b 8))]
     [(a b c d) (+ a
		   (arithmetic-shift b 8)
		   (arithmetic-shift c 16)
		   (arithmetic-shift d 24))]))
  
  (define (do-gunzip in out name-filter)
    (let ([header1 (read-byte in)]
	  [header2 (read-byte in)])
      (unless (and (= header1 #o037) (= header2 #o213))
	(error 'gnu-unzip "bad header")))
    (let ([compression-type (read-byte in)])
      (unless (= compression-type #o010)
	(error 'gnu-unzip "unknown compression type")))
    (let* ([flags (read-byte in)]
	   [ascii? (positive? (bitwise-and flags #b1))]
	   [continuation? (positive? (bitwise-and flags #b10))]
	   [has-extra-field? (positive? (bitwise-and flags #b100))]
	   [has-original-filename? (positive? (bitwise-and flags #b1000))]
	   [has-comment? (positive? (bitwise-and flags #b10000))]
	   [encrypted? (positive? (bitwise-and flags #b100000))])
      (when encrypted?
	(error 'gnu-unzip "cannot unzip encrypted file"))
      (when continuation?
	(error 'gnu-unzip "cannot handle multi-part files"))
      (let ([unix-mod-time (make-small-endian (read-byte in) (read-byte in)
					      (read-byte in) (read-byte in))]
	    [extra-flags (read-byte in)]
	    [source-os (read-byte in)])
	(when continuation?
	  (let ([part-number (make-small-endian (read-byte in) (read-byte in))])
	    'ok))
	(when has-extra-field?
	  (let ([len (make-small-endian (read-byte in) (read-byte in))])
	    (let loop ([len len])
	      (unless (zero? len)
		(read-byte in)
		(loop (sub1 len))))))
	(let* ([read-null-term-string
		(lambda ()
		  (let loop ([s null])
		    (let ([r (read-byte in)])
		      (if (zero? r)
			  (list->bytes (reverse s))
			  (loop (cons r s))))))]
	       [original-filename (and has-original-filename?
				       (read-null-term-string))]
	       [comment (and has-comment? (read-null-term-string))])
	  (when encrypted?
	    (let loop ([n 12])
	      (unless (zero? n)
		(read-byte in)
		(loop (sub1 n)))))
	  
	  (let-values ([(out close?) (if out
					 (values out #f)
					 (let-values ([(fn orig?)
						       (if original-filename
							   (values (bytes->path original-filename) #t)
							   (values "unzipped" #f))])
					   (values (open-output-file (name-filter fn orig?) #:exists 'truncate)
						   #t)))])
	    (dynamic-wind
	     void
	     (lambda () (begin0 (inflate in out)
                                (read-bytes 8 in))) ; read CRC32 and ISIZE
	     (lambda () (when close? (close-output-port out)))))))))
  
  (define (gunzip-through-ports in out)
    (do-gunzip in out void))
  
  (define gunzip
    (case-lambda
     [(src) (gunzip src (lambda (name from-file?) name))]
     [(src name-filter)
      (let ([in (open-input-file src #:mode 'binary)])
	(dynamic-wind
	 void
	 (lambda () (do-gunzip in #f name-filter))
	 (lambda () (close-input-port in))))]))

