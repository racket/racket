
/* The INDIRECT_TO_PROGRAM mode can be useful for debugging
   in 3m to ensure that a regexp "program" is not misinterpreted
   as a pointer by the conservative collector. (This was a problem
   once, anyway.) */
/* #define INDIRECT_TO_PROGRAM */

struct Regwork;

typedef int (*Scheme_Regexp_Matcher)(struct Regwork *rw);

typedef struct regexp {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *source;
  intptr_t nsubexp, ncounter, maxlookback;
  intptr_t regsize;
  short flags;
  unsigned char *regstart;      /* Infor about required starting bytes */
  intptr_t regmust;                 /* pointer relative to self to required starting string */
  intptr_t regmlen;			/* length of the string at regmust */
#ifdef INDIRECT_TO_PROGRAM
  char *program;
#else
  char program[1];		/* Unwarranted chumminess with compiler. */
#endif
} regexp;

#define REGEXP_IS_UTF8    0x01
#define REGEXP_IS_PCRE    0x02
#define REGEXP_ANCH       0x04
#define REGEXP_MUST_CI    0x08

#ifdef INDIRECT_TO_PROGRAM
# define N_ITO_DELTA(prog, extra, re) extra
# define N_ITO_SPACE(v) 0
# define ITO(x, y) x
#else
# define N_ITO_DELTA(prog, extra, re) ((prog+extra) - re)
# define N_ITO_SPACE(v) v
# define ITO(x, y) y
#endif

/* 156 is octal 234: */
#define MAGIC   156

/*
 * The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart	bitmap for chars that must begin a match; NULL if none obvious
 * reganch	is the match anchored (at beginning-of-line only)?
 * regmust	string (pointer into program) that match must include, or NULL
 * regmlen	length of regmust string
 *
 * Regstart and reganch permit very fast decisions on suitable starting points
 * for a match, cutting down the work a lot.  Regmust permits fast rejection
 * of lines that cannot possibly match.  The regmust tests are costly enough
 * that regcomp() supplies a regmust only if the r.e. contains something
 * potentially expensive (at present, the only such thing detected is * or +
 * at the start of the r.e., which can involve a lot of backup).  Regmlen is
 * supplied because the test in regexec() needs it and regcomp() is computing
 * it anyway.
 */

/*
 * Structure for regexp "program".  This is essentially a linear encoding
 * of a nondeterministic finite-state machine (aka syntax charts or
 * "railroad normal form" in parsing technology).  Each node is an opcode
 * plus a "next" pointer, possibly plus an operand.  "Next" pointers of
 * all nodes except BRANCH implement concatenation; a "next" pointer with
 * a BRANCH on both ends of it is connecting two alternatives.  (Here we
 * have one of the subtle syntax dependencies:  an individual BRANCH (as
 * opposed to a collection of them) is never concatenated with anything
 * because of operator precedence.)  The operand of some types of node is
 * a literal string; for others, it is a node leading into a sub-FSM.  In
 * particular, the operand of a BRANCH node is the first node of the branch.
 * (NB this is *not* a tree structure:  the tail of the branch connects
 * to the thing following the set of BRANCHes.)  The opcodes are:
 */

/* definition     number  opnd?   meaning */
#define END       0       /* no   End of program. */
#define BOI       1       /* no   Match "" at beginning of input. */
#define EOI       2       /* no   Match "" at end of input. */
#define ANY       3       /* no   Match any one character. */
#define ANYL      4       /* no   Anything but a linefeed */
#define ANYOF     5       /* bitmap  Match any character in the bitmap. */
#define EXACTLY1  6       /* byte Match the character. */
#define RANGE     7       /* byte,byte  Match any character in this range. */
#define NOTRANGE  8       /* byte,byte  Match any character not in this range. */
#define BRANCH    9       /* node Match this alternative, or the next... */
#define BACK      10      /* no   Match "", "next" ptr points backward. */
#define EXACTLY   11      /* str  Match this string. */
#define EXACTLY_CI 12     /* str  Match this string. */
#define NOTHING   13      /* no   Match empty string. */
#define STAR      14      /* node Match this (simple) thing 0 or more times. */
#define PLUS      15      /* node Match this (simple) thing 1 or more times. */
#define STAR2     16      /* non-greedy star. */
#define PLUS2     17      /* non-greedy plus. */
#define STAR3     18      /* 2 nos  Like STAR, but with numeric quantifiers */
#define STAR4     19      /* 2 nos  non-greedy STAR3 */
#define OPENN     20      /* like OPEN, but with an n >= 50, or n == 0 means (?:...) */
#define CLOSEN    21      /* like CLOSE, but with an n >= 50 */
#define LOOKT     22      /* (?=...) or (?<=...)*/
#define LOOKF     23      /* (?!...) or (?<!...) */
#define LOOKTX    24      /* (?>...) */
#define LOOKBT    25      /* (?<=...)*/
#define LOOKBF    26      /* (?<!...) */
#define LOOKE     27      /* ender for LOOK* */
#define BACKREF   28      /* \<n>, to match exactly the result for cluster <n> */
#define BACKREF_CI 29      /* case-insensitive version */
#define COUNTINIT 30 
#define COUNTOVER 31
#define COUNTUNDER 32
#define COUNTBACK 33
#define COUNTBACKFAIL 34
#define SAVECONST 35      /* no no   Save position and count */
#define MAYBECONST 36      /* no no   Save position and count */
#define WORDBOUND 37
#define NOTWORDBOUND 38
#define BOL       39      /* no   Match "" at beginning of line. */
#define EOL       40      /* no   Match "" at end of line. */
#define UNIPROP   41
#define CONDITIONAL 42
#define EXACTLY2  43      /* byte,byte  Match either byte (useful for some CI cases) */
#define OPEN      44      /* no   Mark this point in input as start of #n. */
/*      OPEN+1 is number 1, etc. */
#define CLOSE     78      /* no   Analogous to OPEN. */

# define OPSTR(o) (o + 2)
# define OPSTRx(o) (o + 1)
# define OPLEN(o, regstr) ((int)(((unsigned char *)regstr)[o] << 8) | (((unsigned char *)regstr)[o+1]))
# define OPRNGS(o, regstr) ((int)(((unsigned char *)regstr)[o]))

/*
 * Opcode notes:
 *
 * BRANCH	The set of branches constituting a single choice are hooked
 *		together with their "next" pointers, since precedence prevents
 *		anything being concatenated to any individual branch.  The
 *		"next" pointer of the last BRANCH in a choice points to the
 *		thing following the whole choice.  This is also where the
 *		final "next" pointer of each individual branch points; each
 *		branch starts with the operand node of a BRANCH node.
 *
 * BACK		Normal "next" pointers all implicitly point forward; BACK
 *		exists to make loop structures possible.
 *
 * STAR,PLUS	'?', and complex '*' and '+', are implemented as circular
 *		BRANCH structures using BACK.  Simple cases (one character
 *		per match) are implemented with STAR and PLUS for speed
 *		and to minimize recursive plunges.
 *
 * OPEN,CLOSE	...are numbered at compile time.
 */

/*
 * A node is one char of opcode followed by two chars of "next" pointer.
 * "Next" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it.
 * An operand, if any, simply follows the node.  (Note that much of the
 * code generation knows about this implicit relationship.)
 *
 * Using two bytes for the "next" pointer is vast overkill for most things,
 * but allows patterns to get big without disasters.
 */
#define	OP(p, regstr)	(regstr[p])
#define	NEXT(p, regstr)	(((((unsigned char *)regstr)[(p)+1]&255)<<8) + (((unsigned char *)regstr)[(p)+2]&255))
#define	OPERAND(p)	((p) + 3)
#define	OPERAND2(p)	((p) + 5)
#define	OPERAND3(p)	((p) + 7)
#define	OPERAND4(p)	((p) + 9)

/*
 * See regmagic.h for one further detail of program structure.
 */


/*
 * Utility definitions.
 */
#define	UCHAR(v) ((unsigned char)(v))

#define	ISMULT(c, parse_flags)  ((c) == '*' || (c) == '+' || (c) == '?' || ((parse_flags & PARSE_PCRE) && ((c) == '{')))
#define	META	   "^$.[()|?+*\\"
#define	PCRE_META  "^$.[()|?+*\\{}]"

/*
 * Flags to be passed up and down.
 */
#define	HASWIDTH       0x01	/* Known never to match null string. */
#define	SIMPLE	       0x02	/* Simple enough to be STAR/PLUS operand. */
#define	SPSTART	       0x04	/* Starts with * or +. */
#define	SPFIXED	       0x08	/* Always matches a particular length */
#define NEEDSAVECONST  0x10     /* Fixed-size thing inside (), lift out save in case of repeat. */
#define SPNOTHING      0x20     /* Unconditionally matches nothing. */
#define	WORST		0	/* Worst case. */

/* Parser flags: */
#define PARSE_CASE_SENS    0x1
#define PARSE_PCRE         0x2
#define PARSE_SINGLE_LINE  0x4

#define rx_tolower(c) (((c >= 'A') && (c <= 'Z')) ? (c + ('a' - 'A')) : c)
#define rx_toupper(c) (((c >= 'a') && (c <= 'z')) ? (c - ('a' - 'A')) : c)
#define rx_isword(c) (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || ((c >= '0') && (c <= '9')) || (c == '_'))

/*
 * Work variables for regtry().
 */
typedef struct Regwork {
  MZTAG_IF_REQUIRED
  char *str;              /* copy of regstr; used only to protect before thread swaps */
  char *instr;
  Scheme_Object *port;
  Scheme_Object *unless_evt;
  char nonblock, aborted;
  rxpos instr_size;       /* For port reads */
  rxpos input_maxend;     /* For port reads */
  rxpos input, input_end, input_start; /* String-input pointer. */
  rxpos input_min;        /* input_start minus prefix_size */
  rxpos boi;	          /* Beginning of input, for ^ check. */
  rxpos *startp;	  /* Pointer to startp array. */
  rxpos *maybep;	  /* Pointer to tentative startp array. */
  rxpos *endp;		  /* Ditto for endp. */
  int *counters;          /* For {} counters */
  Scheme_Object *peekskip;
  char *prefix;
  rxpos prefix_len, prefix_delta;
  struct rx_lazy_str_t *lazy_string;

  int non_tail, rewind_stack_size, rewind_stack_count, rewind_stack_prompt;
  rxpos *rewind_stack;
} Regwork;

typedef struct rx_lazy_str_t {
  MZTAG_IF_REQUIRED
  intptr_t start, done, end, blen;
  mzchar *chars;
  char *s;
} rx_lazy_str_t;
