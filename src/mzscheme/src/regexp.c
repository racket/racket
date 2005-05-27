/*
 * @(#)regexp.c	1.3 of 18 April 87
 * Revised for PLT MzScheme, 1995-2001
 * Copyright (c) 2004-2005 PLT Scheme, Inc.
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 *
 * Notable changes for MzScheme:
 *   Removed hardwired limits on parenthesis nesting
 *   Changed to index-based instead of pointer-based (better for GC)
 *   Added non-greedy operators *?, +?, and ??
 *   Added (?:...) grouping without reporting the group match
 *   Added MzScheme glue
 *
 * from Vladimir Tsyshevsky:
 *  additional optional parameter `offset' in `regexp-match'
 *  and `regexp-match-positions'
 */

#include "schpriv.h"
#include "schmach.h"

#ifndef NO_REGEXP_UTILS

#include <stdio.h>
#include <string.h>

typedef long rxpos;
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define BIGGEST_RXPOS 0x7FFFFFFFFFFFFFFF
#else
# define BIGGEST_RXPOS 0x7FFFFFFF
#endif

/* The INDIRECT_TO_PROGRAM mode can be useful for debugging
   in 3m to ensure that a regexp "program" is not misinterpreted
   as a pointer by the conservative collector. (This was a problem
   once, anyway.) */
/* #define INDIRECT_TO_PROGRAM */

typedef struct regexp {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *source;
  long nsubexp;
  long regsize;
  char is_utf8;
  char regstart;		/* Internal use only. */
  char reganch;			/* Internal use only. */
  long regmust;                 /* Internal use only: => pointer relative to self */
  long regmlen;			/* Internal use only. */
#ifdef INDIRECT_TO_PROGRAM
  char *program;
#else
  char program[1];		/* Unwarranted chumminess with compiler. */
#endif
} regexp;

#ifdef INDIRECT_TO_PROGRAM
# define N_ITO_DELTA(prog, extra, re) extra
# define N_ITO_SPACE(v) 0
# define ITO(x, y) x
#else
# define N_ITO_DELTA(prog, extra, re) ((prog+extra) - re)
# define N_ITO_SPACE(v) v
# define ITO(x, y) y
#endif

static regexp *regcomp(char *, rxpos, int);
/* static int regexec(regexp *, char *, int, int, rxpos *, rxpos * ...); */

/* 156 is octal 234: */
#define MAGIC   156

#ifdef MZ_PRECISE_GC
# define MZREGISTER /**/
#else
# define MZREGISTER register
#endif

/*
 * The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart	char that must begin a match; '\0' if none obvious
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

/* definition	number	opnd?	meaning */
#define	END	0	/* no	End of program. */
#define	BOL	1	/* no	Match "" at beginning of line. */
#define	EOL	2	/* no	Match "" at end of line. */
#define	ANY	3	/* no	Match any one character. */
#define	ANYOF	4	/* str	Match any character in this string. */
#define	ANYBUT	5	/* str	Match any character not in this string. */
#define INRANGE 6       /* str  Match character in range */
#define	BRANCH	7	/* node	Match this alternative, or the next... */
#define	BACK	8	/* no	Match "", "next" ptr points backward. */
#define	EXACTLY	9	/* str	Match this string. */
#define	NOTHING	10	/* no	Match empty string. */
#define	STAR	11	/* node	Match this (simple) thing 0 or more times. */
#define	PLUS	12	/* node	Match this (simple) thing 1 or more times. */
#define	STAR2	13	/* non-greedy star. */
#define	PLUS2	14	/* non-greedy plus. */
#define OPENN   15      /* like OPEN, but with an n >= 50, or n == 0 means (?:...) */
#define CLOSEN  16      /* like CLOSE, but with an n >= 50 */
#define	OPEN	20	/* no	Mark this point in input as start of #n. */
/*	OPEN+1 is number 1, etc. */
#define	CLOSE	70	/* no	Analogous to OPEN. */

# define OPSTR(o) (o + 2)
# define OPLEN(o) ((int)(((unsigned char *)regstr)[o] << 8) | (((unsigned char *)regstr)[o+1]))

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
#define	OP(p)	(regstr[p])
#define	NEXT(p)	(((((unsigned char *)regstr)[(p)+1]&255)<<8) + (((unsigned char *)regstr)[(p)+2]&255))
#define	OPERAND(p)	((p) + 3)

/*
 * See regmagic.h for one further detail of program structure.
 */


/*
 * Utility definitions.
 */
#ifndef CHARBITS
#define	UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define	UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#define	FAIL(m)	{ regcomperror(m); return 0; }
#define	ISMULT(c)	((c) == '*' || (c) == '+' || (c) == '?')
#define	META	"^$.[()|?+*\\"

/*
 * Flags to be passed up and down.
 */
#define	HASWIDTH	01	/* Known never to match null string. */
#define	SIMPLE		02	/* Simple enough to be STAR/PLUS operand. */
#define	SPSTART		04	/* Starts with * or +. */
#define	WORST		0	/* Worst case. */

/*
 * Global work variables for regcomp().
 */
static char *regstr, *regparsestr;

static rxpos regparse, regparse_end; /* Input-scan pointer. */
static int regnpar;		/* () count. */
static char regdummy;
static rxpos regcode;		/* Code-emit pointer; &regdummy = don't. */
static long regsize;		/* Code size. */

#define REGDUMMY &regdummy

/*
 * Forward declarations for regcomp()'s friends.
 */
#ifndef STATIC
#define	STATIC	static
#endif
STATIC rxpos reg(int, int *, int);
STATIC rxpos regbranch(int *);
STATIC rxpos regpiece(int *);
STATIC rxpos regatom(int *);
STATIC rxpos regnode(char);
STATIC rxpos regnext(rxpos);
STATIC void regc(char);
STATIC void reginsert(char, rxpos);
STATIC void regtail(rxpos, rxpos);
STATIC void regoptail(rxpos, rxpos);
STATIC int regstrcspn(char *, char *, char *);

static void
regerror(char *s)
{
  scheme_raise_exn(MZEXN_FAIL,
		   "regexp: %s", s);
}

const char *failure_msg_for_read;

static void
regcomperror(char *s)
{
  if (failure_msg_for_read) {
    failure_msg_for_read = s;
    scheme_longjmp(scheme_error_buf, 1);
  } else
    regerror(s);
}

/*
 - regcomp - compile a regular expression into internal code
 *
 * We can't allocate space until we know how big the compiled form will be,
 * but we can't compile it (and thus know how big it is) until we've got a
 * place to put the code.  So we cheat:  we compile it twice, once with code
 * generation turned off and size counting turned on, and once "for real".
 * This also means that we don't allocate space until we are sure that the
 * thing really will compile successfully, and we never have to move the
 * code and thus invalidate pointers into it.  (Note that it has to be in
 * one piece because free() must be able to free it all.)
 *
 * Beware that the optimization-preparation code in here knows about some
 * of the structure of the compiled regexp.
 */
static regexp *
regcomp(char *expstr, rxpos exp, int explen)
{
  regexp *r;
  rxpos scan, next;
  rxpos longest;
  int len;
  int flags;
  
  /* First pass: determine size, legality. */
  regstr = REGDUMMY;
  regparsestr = expstr;
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regsize = 0L;
  regcode = 1;
  regc(MAGIC);
  if (reg(0, &flags, 0) == 0)
    return NULL;
  
  /* Small enough for pointer-storage convention? */
  if (regsize >= 32767L)		/* Probably could be 65535L. */
    FAIL("regexp too big");
  
  /* Allocate space. */
  r = (regexp *)scheme_malloc_tagged(sizeof(regexp) + N_ITO_SPACE((unsigned)regsize));
  r->type = scheme_regexp_type;
  
#ifdef INDIRECT_TO_PROGRAM
  r->program = (char *)scheme_malloc_atomic((unsigned)regsize + 1);
#endif
  
  r->regsize = regsize;

  r->nsubexp = regnpar;
  
  /* Second pass: emit code. */
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
#ifdef INDIRECT_TO_PROGRAM
  regstr = r->program;
  regcode = 0;
#else
  regstr = (char *)r;
  regcode = (char *)r->program - (char *)r;
#endif
  regc(MAGIC);
  if (reg(0, &flags, 0) == 0)
    return NULL;
  
  /* Dig out information for optimizations. */
  r->regstart = '\0';	/* Worst-case defaults. */
  r->reganch = 0;
  r->regmust = -1;
  r->regmlen = 0;
  scan = N_ITO_DELTA(r->program, 1, (char *)r);    /* First BRANCH. */
  next = regnext(scan);
  if (OP(next) == END) {	/* Only one top-level choice. */
    scan = OPERAND(scan);
    
    /* Starting-point info. */
    if (OP(scan) == EXACTLY)
      r->regstart = regstr[OPSTR(OPERAND(scan))];
    else if (OP(scan) == BOL)
      r->reganch++;
    
    /*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     */
    if (flags&SPSTART) {
      longest = 0;
      len = 0;
      for (; scan != 0; scan = regnext(scan)) {
	if (OP(scan) == EXACTLY && OPLEN(OPERAND(scan)) >= len) {
	  /* Skip regmust if it contains a null character: */
	  rxpos ls = OPSTR(OPERAND(scan));
	  int ll = OPLEN(OPERAND(scan)), i;
	  for (i = 0; i < ll; i++) {
	    if (!regstr[ls + i])
	      break;
	  }
	  if (i >= ll) {
	    longest = ls;
	    len = ll;
	  }
	}
      }
      if (longest)
	r->regmust = longest;
      r->regmlen = len;
    }
  }

#if 0
  if (regcode > r->regsize + sizeof(regexp))
    scheme_signal_error("regexp too large!");
#endif
  
  return(r);
}

#ifdef DO_STACK_CHECK

static Scheme_Object *reg_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  int *flagp = (int *)p->ku.k.p1;
  int res;

  p->ku.k.p1 = NULL;

  res = reg(p->ku.k.i1, flagp, p->ku.k.i2);

  return scheme_make_integer(res);
}

#endif

/*
   - reg - regular expression, i.e. main body or parenthesized thing
   *
   * Caller must absorb opening parenthesis.
   *
   * Combining parenthesis handling with the base level of regular expression
   * is a trifle forced, but the need to tie the tails of the branches to what
   * follows makes it hard to avoid.
   */
static rxpos
reg(int paren, int *flagp, int paren_set)
{
  rxpos ret;
  rxpos br;
  rxpos ender;
  int parno = 0;
  int flags;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *ov;
      p->ku.k.i1 = paren;
      p->ku.k.p1 = (void *)flagp;
      p->ku.k.i2 = paren_set;
      ov = scheme_handle_stack_overflow(reg_k);
      return SCHEME_INT_VAL(ov);
    }
  }
#endif

  *flagp = HASWIDTH;		/* Tentatively. */

  /* Make an OPEN node, if parenthesized. */
  if (paren) {
    if (paren_set) {
      parno = regnpar;
      regnpar++;
    } else
      parno = 0;
    if (!paren_set || (OPEN + parno >= CLOSE)) {
      ret = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(OPENN, ret);
    } else {
      ret = regnode(OPEN+parno);
    }
  } else
    ret = 0;

  /* Pick up the branches, linking them together. */
  br = regbranch(&flags);
  if (br == 0)
    return 0;
  if (ret != 0)
    regtail(ret, br);		/* OPEN -> first. */
  else
    ret = br;
  if (!(flags&HASWIDTH))
    *flagp &= ~HASWIDTH;
  *flagp |= flags&SPSTART;
  while (regparsestr[regparse] == '|') {
    regparse++;
    br = regbranch(&flags);
    if (br == 0)
      return 0;
    regtail(ret, br);		/* BRANCH -> BRANCH. */
    if (!(flags&HASWIDTH))
      *flagp &= ~HASWIDTH;
    *flagp |= flags&SPSTART;
  }

  /* Make a closing node, and hook it on the end. */
  if (paren) {
    if (!paren_set || (OPEN + parno >= CLOSE)) {
      ender = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(CLOSEN, ender);
    } else
      ender = regnode(CLOSE+parno);
  } else {
    ender = regnode(END);	
  }
  regtail(ret, ender);

  /* Hook the tails of the branches to the closing node. */
  for (br = ret; br != 0; br = regnext(br)) {
    regoptail(br, ender);
  }

  /* Check for proper termination. */
  if (paren && regparsestr[regparse++] != ')') {
    FAIL("missing closing parenthesis in pattern");
  } else if (!paren && regparse != regparse_end) {
    if (regparsestr[regparse] == ')') {
      FAIL("extra closing parenthesis in pattern");
    } else
      FAIL("junk on end");	/* "Can't happen". */
    /* NOTREACHED */
  }

  return ret;
}

/*
   - regbranch - one alternative of an | operator
   *
   * Implements the concatenation operator.
   */
static rxpos
regbranch(int *flagp)
{
  rxpos ret;
  rxpos chain;
  rxpos latest;
  int flags;

  *flagp = WORST;		/* Tentatively. */

  ret = regnode(BRANCH);
  chain = 0;
  while (regparse != regparse_end 
	 && regparsestr[regparse] != '|' 
	 && regparsestr[regparse] != ')') {
    latest = regpiece(&flags);
    if (latest == 0)
      return 0;
    *flagp |= flags&HASWIDTH;
    if (chain == 0)		/* First piece. */
      *flagp |= flags&SPSTART;
    else
      regtail(chain, latest);
    chain = latest;
  }
  if (chain == 0)		/* Loop ran zero times. */
    (void) regnode(NOTHING);

  return(ret);
}

/*
   - regpiece - something followed by possible [*+?]
   *
   * Note that the branching code sequences used for ? and the general cases
   * of * and + are somewhat optimized:  they use the same NOTHING node as
   * both the endmarker for their branch list and the body of the last branch.
   * It might seem that this node could be dispensed with entirely, but the
   * endmarker role is not redundant.
   */
static rxpos 
regpiece(int *flagp)
{
  rxpos ret;
  char op;
  rxpos next;
  int flags, greedy;

  ret = regatom(&flags);
  if (ret == 0)
    return 0;

  op = regparsestr[regparse];
  if (!ISMULT(op)) {
    *flagp = flags;
    return(ret);
  }

  if (!(flags&HASWIDTH) && op != '?')
    FAIL("* or + operand could be empty");
  *flagp = (op != '+') ? (WORST|SPSTART) : (WORST|HASWIDTH);

  if (regparsestr[regparse+1] == '?') {
    greedy = 0;
    regparse++;
  } else
    greedy = 1;

  if (op == '*' && (flags&SIMPLE))
    reginsert(greedy ? STAR : STAR2, ret);
  else if (op == '*' && greedy) {
    /* Emit x* as (x&|), where & means "self". */
    reginsert(BRANCH, ret);	/* Either x */
    regoptail(ret, regnode(BACK)); /* and loop */
    regoptail(ret, ret);	/* back */
    regtail(ret, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '*') {
    /* Emit x*? as (|x&), where & means "self". */
    reginsert(BRANCH, ret);  /* will be next... */
    reginsert(NOTHING, ret);
    reginsert(BRANCH, ret);
    next = ret + 6;
    regtail(ret, next);
    regoptail(next, regnode(BACK)); /* loop */
    regoptail(next, ret);	/* back. */
    regtail(next, regnode(BACK));
    regtail(next, ret + 3);
  } else if (op == '+' && (flags&SIMPLE))
    reginsert(greedy ? PLUS : PLUS2, ret);
  else if (op == '+' && greedy) {
    /* Emit x+ as x(&|), where & means "self". */
    next = regnode(BRANCH);	/* Either */
    regtail(ret, next);
    regtail(regnode(BACK), ret); /* loop back */
    regtail(next, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '+') {
    /* Emit x+? as x(|&), where & means "self". */
    next = regnode(BRANCH);	/* Either */
    regtail(ret, next);
    regnode(NOTHING); /* op */
    regtail(next, regnode(BRANCH)); /* or */
    regtail(regnode(BACK), ret); /* loop back. */
    regtail(next, regnode(BACK));
    regtail(next, next + 3);
  } else if (op == '?' && greedy) {
    /* Emit x? as (x|) */
    reginsert(BRANCH, ret);	/* Either x */
    regtail(ret, regnode(BRANCH)); /* or */
    next = regnode(NOTHING);	/* null. */
    regtail(ret, next);
    regoptail(ret, next);
  } else if (op == '?') {
    /* Emit x?? as (|x) */
    reginsert(BRANCH, ret);  /* will be next... */
    reginsert(NOTHING, ret);
    reginsert(BRANCH, ret);
    regtail(ret, ret + 6);
    next = regnode(BACK);
    regtail(ret + 6, next);
    regoptail(ret + 6, next);
    regoptail(ret + 6, ret + 3);
  }
  regparse++;
  if (ISMULT(regparsestr[regparse]))
    FAIL("nested *, ?, or + in pattern");

  return(ret);
}

/*
   - regatom - the lowest level
   *
   * Optimization:  gobbles an entire sequence of ordinary characters so that
   * it can turn them into a single node, which is smaller to store and
   * faster to run.  Backslashed characters are exceptions, each becoming a
   * separate node; the code is simpler that way and it's not worth fixing.
 */
static rxpos 
regatom(int *flagp)
{
  rxpos ret;
  int flags;

  *flagp = WORST;		/* Tentatively. */

  switch (regparsestr[regparse++]) {
  case '^':
    ret = regnode(BOL);
    break;
  case '$':
    ret = regnode(EOL);
    break;
  case '.':
    ret = regnode(ANY);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  case '[': 
    /* Check for simple-range special case: */
    if (((regparse + 4) <= regparse_end)
	&& (regparsestr[regparse] != '^')
	&& (regparsestr[regparse] != '-')
	&& (regparsestr[regparse] != ']')
	&& (regparsestr[regparse + 1] == '-')
	&& (regparsestr[regparse + 2] != '-')
	&& (regparsestr[regparse + 2] != ']')
	&& (regparsestr[regparse + 3] == ']')) {
      ret = regnode(INRANGE);
      regc(regparsestr[regparse]);
      regc(regparsestr[regparse + 2]);
      regparse += 4;
      *flagp |= HASWIDTH|SIMPLE;
    } else {
      int xclass;
      int classend, len, can_range = 0;
      rxpos l0, l1;

      if (regparsestr[regparse] == '^') { /* Complement of range. */
	ret = regnode(ANYBUT);
	regparse++;
      } else
	ret = regnode(ANYOF);
      len = 0;
      l0 = regcode;
      regc(0);
      l1 = regcode;
      regc(0);
      if (regparsestr[regparse] == ']' || regparsestr[regparse] == '-') {
	regc(regparsestr[regparse++]);
	len++;
      }
      while (regparse != regparse_end && regparsestr[regparse] != ']') {
	if (regparsestr[regparse] == '-') {
	  regparse++;
	  if (regparsestr[regparse] == ']' || regparse == regparse_end) {
	    regc('-');
	    len++;
	  } else {
	    if (!can_range) {
	      FAIL("misplaced hypen within square brackets in pattern");
	    }
	    xclass = UCHARAT(regparsestr + (regparse-2))+1;
	    classend = UCHARAT(regparsestr + regparse);
	    if (classend == '-') {
	      FAIL("misplaced hypen within square brackets in pattern");
	    }
	    if (xclass > classend+1)
	      FAIL("invalid range within square brackets in pattern");
	    for (; xclass <= classend; xclass++) {
	      regc(xclass);
	      len++;
	    }
	    regparse++;
	  }
	  can_range = 0;
	} else {
	  regc(regparsestr[regparse++]);
	  len++;
	  can_range = 1;
	}
      }
      if (regparsestr[regparse] != ']')
	FAIL("missing closing square bracket in pattern");
      regparse++;
      regstr[l0] = (len >> 8);
      regstr[l1] = (len & 255);
      *flagp |= HASWIDTH|SIMPLE;
    }
  break;
  case '(':
    if ((regparsestr[regparse] == '?')
	&& (regparsestr[regparse+1] == ':')) {
      regparse += 2;
      ret = reg(1, &flags, 0);
    } else
      ret = reg(1, &flags, 1);
    if (ret == 0)
      return 0;
    *flagp |= flags&(HASWIDTH|SPSTART);
    break;
  case '|':
  case ')':
    FAIL("internal urp");	/* Supposed to be caught earlier. */
    break;
  case '?':
  case '+':
  case '*':
    FAIL("?, +, or * follows nothing in pattern");
    break;
  case '\\':
    if (regparse == regparse_end)
      FAIL("trailing backslash in pattern");
    ret = regnode(EXACTLY);
    regc(0);
    regc(1);
    regc(regparsestr[regparse++]);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  default: {
      int len;
      char ender;

      regparse--;
      len = regstrcspn(regparsestr + regparse, regparsestr + regparse_end, META);
      if (len <= 0)
	FAIL("internal disaster");
      ender = regparsestr[regparse+len];
      if (len > 1 && ISMULT(ender))
	len--;		/* Back off clear of ?+* operand. */
      *flagp |= HASWIDTH;
      if (len == 1)
	*flagp |= SIMPLE;
      ret = regnode(EXACTLY);
      regc(len >> 8); regc(len & 255);
      while (len > 0) {
	regc(regparsestr[regparse++]);
	len--;
      }
    }
    break;
  }
	
  return ret;
}

/*
   - regnode - emit a node
   */
static rxpos 			/* Location. */
regnode(char op)
{
  rxpos ret;
  rxpos ptr;

  ret = regcode;
  if (regstr == REGDUMMY) {
    regsize += 3;
    return ret;
  }

  ptr = ret;
  regstr[ptr++] = op;
  regstr[ptr++] = '\0';		/* Null "next" pointer. */
  regstr[ptr++] = '\0';
  regcode = ptr;

  return ret;
}

/*
   - regc - emit (if appropriate) a byte of code
   */
static void
regc(char b)
{
  if (regstr != REGDUMMY)
    regstr[regcode++] = b;
  else
    regsize++;
}

/*
   - reginsert - insert an operator in front of already-emitted operand
   *
   * Means relocating the operand.
   */
static void
reginsert(char op, rxpos opnd)
{
  rxpos src;
  rxpos dst;
  rxpos place;

  if (regstr == REGDUMMY) {
    regsize += 3;
    return;
  }

  src = regcode;
  regcode += 3;
  dst = regcode;
  while (src > opnd) {
    regstr[--dst] = regstr[--src];
  }

  place = opnd;			/* Op node, where operand used to be. */
  regstr[place++] = op;
  regstr[place++] = '\0';
  regstr[place++] = '\0';
}

/*
   - regtail - set the next-pointer at the end of a node chain
   */
static void
regtail(rxpos p, rxpos val)
{
  rxpos scan;
  rxpos temp;
  int offset;

  if (regstr == REGDUMMY)
    return;

  /* Find last node. */
  scan = p;
  for (;;) {
    temp = regnext(scan);
    if (temp == 0)
      break;
    scan = temp;
  }

  if (OP(scan) == BACK)
    offset = scan - val;
  else
    offset = val - scan;
  regstr[scan+1] = (offset>>8)&255;
  regstr[scan+2] = offset&255;
}

/*
   - regoptail - regtail on operand of first argument; nop if operandless
   */
static void
regoptail(rxpos p, rxpos val)
{
  /* "Operandless" and "op != BRANCH" are synonymous in practice. */
  if (p == 0 || regstr == REGDUMMY || OP(p) != BRANCH)
    return;
  regtail(OPERAND(p), val);
}

static rxpos l_strchr(char *str, rxpos a, int l, int c)
{
  int i;

  for (i = 0; i < l; i++) {
    if (str[a + i] == c)
      return a + i;
  }

  return -1;
}

/*
 * regexec and friends
 */

/*
 * Work variables for regtry().
 */
typedef struct Regwork {
  MZTAG_IF_REQUIRED
  char *str;              /* copy of regstr; used only to protect before thread swaps */
  char *instr;
  Scheme_Object *port;
  Scheme_Object *unless_evt;
  short nonblock, aborted;
  rxpos instr_size;       /* For port reads */
  rxpos input_maxend;     /* For port reads */
  rxpos input, input_end; /* String-input pointer. */
  rxpos bol;		  /* Beginning of input, for ^ check. */
  rxpos *startp;	  /* Pointer to startp array. */
  rxpos *endp;		  /* Ditto for endp. */
  Scheme_Object *peekskip;
} Regwork;

/*
 * Forwards.
 */
STATIC int regtry(regexp *, char *, int, int, rxpos *, rxpos *, Regwork *rw, int);
STATIC int regtry_port(regexp *, Scheme_Object *, Scheme_Object *, int nonblock,
		       rxpos *, rxpos *, 
		       char **, rxpos *, rxpos *, rxpos, Scheme_Object*, Scheme_Object*, int);
STATIC int regmatch(Regwork *rw, rxpos);
STATIC int regrepeat(Regwork *rw, rxpos);

#ifdef DEBUG
int regnarrate = 0;
void regdump();
STATIC char *regprop();
#endif

#define REGPORT_FLUSH_THRESHOLD 256

/*
   - regexec - match a regexp against a string
   */
static int
regexec(const char *who,
	regexp *prog, char *string, 
	/* used only for strings: */
	int stringpos, int stringlen, 
	/* Always used: */
	rxpos *startp, rxpos *endp,
	Scheme_Object *port, Scheme_Object *unless_evt, int nonblock,
	/* Used only when port is non-NULL: */
	char **stringp, int peek, int get_offsets,
	Scheme_Object *discard_oport, 
	Scheme_Object *portstart, Scheme_Object *portend, Scheme_Object **_dropped)
{
  int spos;
  int slen;
  Scheme_Object *dropped = NULL, *peekskip = NULL; /* used for ports, only */
 
  /* Check validity of program. */
  if (UCHARAT(prog->program) != MAGIC) {
    regerror("corrupted program");
    return(0);
  }

  /* If there is a "must appear" string, look for it. */
  if (!port && (prog->regmust >= 0)) {
    spos = stringpos;
    slen = stringlen;
    while ((spos = l_strchr(string, spos, slen, 
			    (ITO(prog->program, (char *)prog) XFORM_OK_PLUS prog->regmust)[0])) != -1) {
      int i, l = prog->regmlen;
      /* ASSUMING NO GC HERE! */
      GC_CAN_IGNORE char *p = (ITO(prog->program, (char *)prog) XFORM_OK_PLUS prog->regmust);
      slen = stringlen - (spos - stringpos);
      for (i = 0; (i < l) && (i < slen); i++) {
	if (string[spos + i] != p[i])
	  break;
      }
      if (i >= l)
	break; /* Found it. */
      spos++;
      slen--;
    }
    if (spos == -1) /* Not present. */
      return 0;
  }

  if (port) {
    if (peek) {
      peekskip = portstart;
      dropped = portstart;
    } else {
      /* In non-peek port mode, skip over portstart chars: */
      long amt, got;

      if (SCHEME_INTP(portstart)) {
	amt = SCHEME_INT_VAL(portstart);
	if (amt > 4096)
	  amt = 4096;
      } else
	amt = 4096;

      dropped = scheme_make_integer(0);
	
      if (amt) {
	char *drain;

	drain = (char *)scheme_malloc_atomic(amt);

	do {
	  got = scheme_get_byte_string(who, port, drain, 0, amt, 0, 0, 0);
	  if (got != EOF) {
	    Scheme_Object *delta;
	    
	    if (discard_oport)
	      scheme_put_byte_string(who, discard_oport, drain, 0, got, 0);
	    
	    dropped = scheme_bin_plus(dropped, scheme_make_integer(amt));
	    delta = scheme_bin_minus(portstart, dropped);
	    if (scheme_bin_gt(scheme_make_integer(amt), delta))
	      amt = SCHEME_INT_VAL(delta);
	  }
	} while ((got != EOF) && amt);
	if (amt)
	  return 0; /* can't skip far enough, so it fails */
      }
    }

    if (portend)
      portend = scheme_bin_minus(portend, dropped);
  }

  /* Simplest case:  anchored match need be tried only once. */
  if (prog->reganch) {
    if (port) {
      rxpos len = 0, space = 0;

      *stringp = NULL;
      if (regtry_port(prog, port, unless_evt, nonblock, 
		      startp, endp, stringp, &len, &space, 0, 
		      portend, peekskip, 1)) {
	if (!peek) {
	  /* Need to consume matched chars: */
	  char *drain;
	  long got;

	  if (discard_oport && *startp)
	    scheme_put_byte_string(who, discard_oport, *stringp, 0, *startp, 0);

	  if (get_offsets)
	    drain = *stringp;
	  else
	    /* Allocate fresh in case we get different results from previous peek: */
	    drain = (char *)scheme_malloc_atomic(*endp);
	  got = scheme_get_byte_string(who, port, drain, 0, *endp, 0, 0, 0);
	}

	*_dropped = dropped;

	return 1;
      } else {
	if (!peek) {
	  /* Need to consume all chars, up to portend */
	  char *drain;
	  long got;
	  
	  if (portend && SCHEME_INTP(portend) && SCHEME_INT_VAL(portend) < 4096) {
	    got = SCHEME_INT_VAL(portend);
	  } else
	    got = 4096;

	  drain = (char *)scheme_malloc_atomic(got);

	  while ((got = scheme_get_byte_string(who, port, drain, 0, got, 0, 0, 0)) != EOF) {
	    if (discard_oport)
	      scheme_put_byte_string(who, discard_oport, drain, 0, got, 0);

	    if (portend) {
	      portend = scheme_bin_minus(portend, scheme_make_integer(got));
	      if (SCHEME_INTP(portend)) {
		got = SCHEME_INT_VAL(portend);
		if (!got)
		  break;
		else if (got > 4096)
		  got = 4096;
	      }
	    } else
	      got = 4096;
	  }
	}
	return 0;
      }
    } else
      return regtry(prog, string, stringpos, stringlen, startp, endp, 0, 1);
  }

  /* Messy cases:  unanchored match. */
  spos = stringpos;
  if (!port && (prog->regstart != '\0')) {
    /* We know what char the string must start with. */
    while ((spos = l_strchr(string, spos, stringlen - (spos - stringpos), prog->regstart)) != -1) {
      if (regtry(prog, string, spos, stringlen - (spos - stringpos), startp, endp, 0, spos == stringpos))
	return 1;
      spos++;
    }
  } else {
    /* We don't know the starting char, or we have a port -- general
       case. */
    if (port) {
      rxpos len = 0, skip = 0, space = 0;
      *stringp = NULL;

      do {
	if (skip >= REGPORT_FLUSH_THRESHOLD) {
	  if (!peek) {
	    if (discard_oport)
	      scheme_put_byte_string(who, discard_oport, *stringp, 0, skip, 0);
	    
	    scheme_get_byte_string(who, port, *stringp, 0, skip, 0, 0, 0);

	    if (portend)
	      portend = scheme_bin_minus(portend, scheme_make_integer(skip));
	  } else {
	    peekskip = scheme_bin_plus(peekskip, scheme_make_integer(skip));
	  }

	  dropped = scheme_bin_plus(dropped, scheme_make_integer(skip));

	  len -= skip;
	  memmove(*stringp, *stringp + skip, len);
	  skip = 0;
	}

	if (regtry_port(prog, port, unless_evt, nonblock,
			startp, endp, stringp, &len, &space, skip, 
			portend, peekskip, !space)) {
	  if (!peek) {
	    char *drain;

	    if (discard_oport && *startp)
	      scheme_put_byte_string(who, discard_oport, *stringp, 0, *startp, 0);

	    if (get_offsets)
	      drain = *stringp;
	    else
	      /* Allocate fresh in case we get different results from previous peek: */
	      drain = (char *)scheme_malloc_atomic(*endp);

	    scheme_get_byte_string(who, port, drain, 0, *endp, 0, 0, 0);
	  }

	  *_dropped = dropped;

	  return 1;
	}
	skip++;
      } while (len >= skip);

      if (!peek) {
	/* If we get here, there must be `len' leftover characters in the port,
	   and `*stringp' must hold the characters: */
	if (len > 0) {
	  if (discard_oport)
	    scheme_put_byte_string(who, discard_oport, *stringp, 0, len, 0);
	  scheme_get_byte_string(who, port, *stringp, 0, len, 0, 0, 0);
	}
      }
    } else {
      rxpos e = stringpos + stringlen;
      do {
	if (regtry(prog, string, spos, stringlen - (spos - stringpos), startp, endp, 0, spos == stringpos))
	  return 1;
      } while (spos++ != e);
    }
  }

  /* Failure. */
  return 0;
}

/*
   - regtry - try match at specific point
   */
static int			/* 0 failure, 1 success */
regtry(regexp *prog, char *string, int stringpos, int stringlen, rxpos *startp, rxpos *endp, Regwork *rw, int atstart)
{
  int i;
  GC_CAN_IGNORE rxpos *sp, *ep;
  Regwork _rw;

  if (!rw) {
    rw = &_rw;
    rw->port = NULL;
  }
  rw->instr = string;
  rw->input = stringpos;
  rw->input_end = stringpos + stringlen;
  rw->startp = startp;
  rw->endp = endp;
  if (atstart)
    rw->bol = stringpos;
  else
    rw->bol = -1;

  /* ASSUMING NO GC in this loop: */ 
  sp = startp;
  ep = endp;
  for (i = prog->nsubexp; i > 0; i--) {
    *sp++ = -1;
    *ep++ = -1;
  }
  sp = ep = NULL;

#ifdef INDIRECT_TO_PROGRAM
  regstr = prog->program;
#else
  regstr = (char *)prog;
#endif
  if (regmatch(rw, N_ITO_DELTA(prog->program, 1, (char *)prog))) {
    startp[0] = stringpos;
    endp[0] = rw->input;
    return 1;
  } else
    return 0;
}

/*
   - regtry - try match in a port
   */
static int			/* 0 failure, 1 success */
regtry_port(regexp *prog, Scheme_Object *port, Scheme_Object *unless_evt, int nonblock,
	    rxpos *startp, rxpos *endp, 
	    char **work_string, rxpos *len, rxpos *size, rxpos skip, 
	    Scheme_Object *maxlen, Scheme_Object *peekskip, 
	    int atstart)
{
  int m;
  Regwork rw;

  rw.port = port;
  rw.unless_evt = unless_evt;
  rw.nonblock = (short)nonblock;
  rw.aborted = 0;
  rw.instr_size = *size;
  if (maxlen && SCHEME_INTP(maxlen))
    rw.input_maxend = SCHEME_INT_VAL(maxlen);
  else
    rw.input_maxend = BIGGEST_RXPOS;
  rw.peekskip = peekskip;

  m = regtry(prog, *work_string, skip, *len - skip, startp, endp, &rw, atstart);

  *work_string = rw.instr;
  *len = rw.input_end;
  *size = rw.instr_size;

  if (rw.aborted)
    return 0;
  else
    return m;
}

#define NEED_INPUT(rw, v, n) if (rw->port && (((v) + (n)) > rw->input_end)) read_more_from_regport(rw, (v) + (n))

static void read_more_from_regport(Regwork *rw, rxpos need_total)
     /* Called when we're about to look past our read-ahead */
{
  long got;
  Scheme_Object *peekskip;

  /* limit reading by rw->input_maxend: */
  if (need_total > rw->input_maxend) {
    need_total = rw->input_maxend;
    if (need_total <= rw->input_end) {
      rw->port = NULL; /* turn off further port reading */
      return;
    }
  }

  if (rw->instr_size < need_total) {
    char *naya;
    long size = rw->instr_size;
    
    size = size * 2;
    if (size < need_total)
      size += need_total;
    else if (size < 256)
      size = 256;

    naya = (char *)scheme_malloc_atomic(size);
    memcpy(naya, rw->instr, rw->input_end);

    rw->instr = naya;
    rw->instr_size = size;
  }

  rw->str = regstr; /* get_string can swap threads */

  if (rw->input_maxend < rw->instr_size)
    got = rw->input_maxend - rw->input_end;
  else
    got = rw->instr_size - rw->input_end;
  
  if (rw->peekskip)
    peekskip = scheme_bin_plus(scheme_make_integer(rw->input_end), rw->peekskip);
  else
    peekskip = scheme_make_integer(rw->input_end);

  /* Fill as much of our buffer as possible: */
  got = scheme_get_byte_string_unless("regexp-match", rw->port, 
				      rw->instr, rw->input_end, got,
				      (rw->nonblock
				       ? 2   /* non-blocking read, as much as possible */
				       : 1), /* read at least one char, and as much as possible */
				      1, peekskip,
				      rw->unless_evt);

  regstr = rw->str;

  if (got < 1) {
    /* EOF, special, or 0-due-to-unless/nonblock */
    if (!got)
      rw->aborted = 1;
    rw->port = NULL; /* turn off further port reading */
    rw->unless_evt = NULL;
  } else {
    rw->input_end += got;

    /* Non-blocking read got enough? If not, try again in blocking mode: */
    if (need_total > rw->input_end) {
      if (rw->nonblock) {
	rw->port = NULL; /* turn off further port reading */
	rw->unless_evt = NULL;
	rw->aborted = 1;
      } else {
	if (rw->peekskip)
	  peekskip = scheme_bin_plus(scheme_make_integer(rw->input_end), rw->peekskip);
	else
	  peekskip = scheme_make_integer(rw->input_end);

	rw->str = regstr; /* get_string can swap threads */
	got = scheme_get_byte_string_unless("regexp-match", rw->port, 
					    rw->instr, rw->input_end, need_total - rw->input_end,
					    0, /* blocking mode */
					    1, peekskip,
					    rw->unless_evt);
	regstr = rw->str;
      
	if (got == EOF) {
	  rw->port = NULL; /* turn off further port reading */
	  rw->unless_evt = NULL;
	} else
	  rw->input_end += got;
      }
    }
  }
}

#ifdef DO_STACK_CHECK

static Scheme_Object *regmatch_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Regwork *rw = (Regwork *)p->ku.k.p1;
  int res;

  p->ku.k.p1 = NULL;

  regstr = rw->str; /* in case of thread swap */
 
  res = regmatch(rw, p->ku.k.i1);

  return (res ? scheme_true : scheme_false);
}

#endif

/*
   - regmatch - main matching routine
   *
   * Conceptually the strategy is simple:  check to see whether the current
   * node matches, call self recursively to see whether the rest matches,
   * and then act accordingly.  In practice we make some effort to avoid
   * recursion, in particular by going through "ordinary" nodes (that don't
   * need to know whether the rest of the match failed) by a loop instead of
   * by recursion.
   */
static int			/* 0 failure, 1 success */
regmatch(Regwork *rw, rxpos prog)
{
  rxpos scan;		/* Current node. */
  rxpos next;			/* Next node. */

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Regwork *rw2;
      Scheme_Object *res;

      /* rw is likely be stack allocated, so copy out to
	 the heap and then copy result back in on return. */
      rw2 = MALLOC_ONE_RT(Regwork);
      memcpy(rw2, rw, sizeof(Regwork));
#ifdef MZTAG_REQUIRED
      rw2->type = scheme_rt_regwork;
#endif

      rw2->str = regstr; /* in case of thread swap */
      p->ku.k.p1 = rw2;
      p->ku.k.i1 = prog;
      res = scheme_handle_stack_overflow(regmatch_k);

      memcpy(rw, rw2, sizeof(Regwork));

      return SCHEME_TRUEP(res);
    }
  }
#endif

  if (DECREMENT_FUEL(scheme_fuel_counter, 1) <= 0) { 
    char *rs;
    rs = regstr;
    scheme_out_of_fuel();
    regstr = rs;
  }

  scan = prog;
  while (scan != 0) {
    next = regnext(scan);

    switch (OP(scan)) {
    case BOL:
      if (rw->input != rw->bol)
	return(0);
      break;
    case EOL:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input != rw->input_end)
	return(0);
      break;
    case ANY:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end)
	return(0);
      rw->input++;
      break;
    case EXACTLY: {
      int len, i;
      rxpos opnd;

      opnd = OPSTR(OPERAND(scan));
      len = OPLEN(OPERAND(scan));
      if (rw->port) {
	/* Like the other branch, but demand chars one at a time, as
           we need them */
	for (i = 0; i < len; i++) {
	  NEED_INPUT(rw, rw->input + i, 1);
	  if (rw->input + i >= rw->input_end)
	    return 0;
	  if (regstr[opnd+i] != rw->instr[rw->input+i])
	    return 0;
	}
      } else {
	if (len > rw->input_end - rw->input)
	  return 0;
	for (i = 0; i < len; i++) {
	  if (regstr[opnd+i] != rw->instr[rw->input+i])
	    return 0;
	}
      }
      rw->input += len;
    }
      break;
    case ANYOF:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end || (l_strchr(regstr, OPSTR(OPERAND(scan)), 
						  OPLEN(OPERAND(scan)), 
						  rw->instr[rw->input]) == -1))
	return(0);
      rw->input++;
      break;
    case ANYBUT:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end || (l_strchr(regstr, OPSTR(OPERAND(scan)), 
						  OPLEN(OPERAND(scan)), 
						  rw->instr[rw->input]) != -1))
	return(0);
      rw->input++;
      break;
    case INRANGE:
      {
	int lo, hi;
	lo = ((unsigned char *)(regstr + OPERAND(scan)))[0];
	hi = ((unsigned char *)(regstr + OPERAND(scan)))[1];
	NEED_INPUT(rw, rw->input, 1);
	if (rw->input == rw->input_end 
	    || (((unsigned char *)rw->instr)[rw->input] < lo)
	    || (((unsigned char *)rw->instr)[rw->input] > hi))
	  return(0);
	rw->input++;
      }
      break;
    case NOTHING:
      break;
    case BACK:
      break;
    case BRANCH: {
      rxpos save;

      if (OP(next) != BRANCH)	/* No choice. */
	next = OPERAND(scan);	/* Avoid recursion. */
      else {
	do {
	  save = rw->input;
	  if (regmatch(rw, OPERAND(scan)))
	    return(1);
	  rw->input = save;
	  scan = regnext(scan);
	} while (scan != 0 && OP(scan) == BRANCH);
	return(0);
	/* NOTREACHED */
      }
    }
      break;
    case STAR:
    case PLUS:
    case STAR2:
    case PLUS2: {
      char nextch;
      int no;
      rxpos save;
      int min;
      int nongreedy = (OP(scan) == STAR2 || OP(scan) == PLUS2);

      /*
       * Lookahead to avoid useless match attempts
       * when we know what character comes next.
       */
      nextch = '\0';
      if (OP(next) == EXACTLY)
	nextch = regstr[OPSTR(OPERAND(next))];
      min = ((OP(scan) == STAR) || (OP(scan) == STAR2)) ? 0 : 1;
      save = rw->input;

      if (nongreedy && rw->port) {
	/* Get at least one, but then don't
	   let regrepeat pull in arbitrary code: */
	Scheme_Object *saveport;
	NEED_INPUT(rw, save, 1);
	saveport = rw->port;
	rw->port = NULL;
	no = regrepeat(rw, OPERAND(scan));
	rw->port = saveport;
	nongreedy = 2;
      } else
	no = regrepeat(rw, OPERAND(scan));
      
      if (!nongreedy) {
	while (no >= min) {
	  /* If it could work, try it. */
	  if (nextch == '\0' || rw->instr[rw->input] == nextch)
	    if (regmatch(rw, next))
	      return(1);
	  /* Couldn't or didn't -- back up. */
	  no--;
	  rw->input = save + no;
	}
      } else {
	int i;
	for (i = min; i <= no; i++) {
	  rw->input = save + i;
	  /* If it could work, try it. */
	  NEED_INPUT(rw, save + i, 1);
	  if (nextch == '\0' || rw->instr[rw->input] == nextch)
	    if (regmatch(rw, next)) {
	      return(1);
	    }

	  if ((i == no) && (nongreedy == 2)) {
	    /* Maybe regrepeat can match more if we let it read from
	       the port. */
	    if ((rw->input_end - save) > no) {
	      /* We have pulled-in chars to try. */
	      int moreno;
	      Scheme_Object *saveport;

	      saveport = rw->port;
	      rw->port = NULL;
	      rw->input = save + no;
	      moreno = regrepeat(rw, OPERAND(scan));
	      rw->port = saveport;

	      if (!moreno)
		nongreedy = 1;
	      else
		no += moreno;
	    }
	  }
	}
      }
      return(0);
    }
      break;
    case END:
      return(1);		/* Success! */
      break;
    default:
      {
	int isopen;
	int no;
	rxpos save;

	switch (OP(scan)) {
	case OPENN:
	  isopen = 1;
	  no = OPLEN(OPERAND(scan));
	  if (!no)
	    no = -1; /* => don't set in result array */
	  break;
	case CLOSEN:
	  isopen = 0;
	  no = OPLEN(OPERAND(scan));
	  break;
	default:
	  if (OP(scan) < CLOSE) {
	    isopen = 1;
	    no = OP(scan) - OPEN;
	  } else {
	    isopen = 0;
	    no = OP(scan) - CLOSE;
	  }
	}

	save = rw->input;

	if (isopen) {
	  if (regmatch(rw, next)) {
	    if (no >= 0) {
	      /*
	       * Don't set startp if some later
	       * invocation of the same parentheses
	       * already has.
	       */
	      if (rw->startp[no] == -1)
		rw->startp[no] = save;
	    }
	    return(1);
	  } else
	    return(0);
	} else {
	  if (regmatch(rw, next)) {
	    if (no >= 0) {
	      /*
	       * Don't set endp if some later
	       * invocation of the same parentheses
	       * already has.
	       */
	      if (rw->endp[no] == -1)
		rw->endp[no] = save;
	    }
	    return(1);
	  } else
	    return(0);
	}
      }
      break;
    }

    scan = next;
  }

  /*
   * We get here only if there's trouble -- normally "case END" is
   * the terminating point.
   */
  regerror("corrupted pointers");
  return(0);
}

/*
   - regrepeat - repeatedly match something simple, report how many
   */
static int
regrepeat(Regwork *rw, rxpos p)
{
  int count = 0;
  rxpos scan;
  rxpos opnd;

  scan = rw->input;
  opnd = OPERAND(p);
  switch (OP(p)) {
  case ANY:
    /* need all port input: */
    while (rw->port) {
      read_more_from_regport(rw, rw->input_end + 4096);
    }
    count = rw->input_end - scan;
    scan += count;
    break;
  case EXACTLY:
    {
      rxpos opnd2 = OPSTR(opnd);
      NEED_INPUT(rw, scan, 1);
      while (scan != rw->input_end
	     && (regstr[opnd2] == rw->instr[scan])) {
	count++;
	scan++;
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  case ANYOF:
    NEED_INPUT(rw, scan, 1);
    while (scan != rw->input_end 
	   && (l_strchr(regstr, OPSTR(opnd), OPLEN(opnd), rw->instr[scan]) != -1)) {
      count++;
      scan++;
      NEED_INPUT(rw, scan, 1);
    }
    break;
  case ANYBUT:
    NEED_INPUT(rw, scan, 1);
    while (scan != rw->input_end 
	   && (l_strchr(regstr, OPSTR(opnd), OPLEN(opnd), rw->instr[scan]) == -1)) {
      count++;
      scan++;
      NEED_INPUT(rw, scan, 1);
    }
    break;
  case INRANGE:
    NEED_INPUT(rw, scan, 1);
    {
      int lo, hi;
      lo = ((unsigned char *)(regstr + opnd))[0];
      hi = ((unsigned char *)(regstr + opnd))[1];
      NEED_INPUT(rw, scan, 1);
      while (scan != rw->input_end 
	     && (((unsigned char *)rw->instr)[scan] >= lo)
	     && (((unsigned char *)rw->instr)[scan] <= hi)) {
	scan++;
	count++;
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  default:			/* Oh dear.  Called inappropriately. */
    regerror("internal foulup");
    count = 0;			/* Best compromise. */
    break;
  }
  rw->input = scan;

  return(count);
}

/*
   - regnext - dig the "next" pointer out of a node
   */
static rxpos
regnext(rxpos p)
{
  int offset;

  if (regstr == REGDUMMY)
    return 0;

  offset = NEXT(p);
  if (offset == 0)
    return 0;

  if (OP(p) == BACK)
    return (p-offset);
  else
    return (p+offset);
}

/*
 * strcspn - find length of initial segment of s1 consisting entirely
 * of characters not from s2
 */

static int
regstrcspn(char *s1, char *e1, char *s2)
{
  char *scan1;
  char *scan2;
  int count;

  count = 0;
  for (scan1 = s1; scan1 != e1; scan1++) {
    for (scan2 = s2; *scan2 != '\0';) { /* ++ moved down. */
      if (*scan1 == *scan2++)
	return(count);
    }
    count++;
  }
  return(count);
}

#ifndef strncpy
  extern char *strncpy();
#endif

/*
   - regsub - perform substitutions after a regexp match
   */
static 
char *regsub(regexp *prog, char *src, int sourcelen, long *lenout, char *insrc, rxpos *startp, rxpos *endp)
{
  char *dest;
  char c;
  long no;
  long len;
  long destalloc, destlen, srcpos;
	
  destalloc = 2 * sourcelen;
  destlen = 0;
  dest = (char *)scheme_malloc_atomic(destalloc + 1);
  
  srcpos = 0;
  while (srcpos < sourcelen) {
    c = src[srcpos++];
    if (c == '&')
      no = 0;
    else if (c == '\\') {
      if (src[srcpos] == '\\' || src[srcpos] == '&')
	no = -1;
      else if (src[srcpos] == '$') {
	no = prog->nsubexp + 1; /* Gives the empty string */
	srcpos++;
      } else {
	no = 0;
	while ('0' <= src[srcpos] && src[srcpos] <= '9') {
	  no = (no * 10) + (src[srcpos++] - '0');
	}
      }
    } else
      no = -1;


    if (no < 0) {		/* Ordinary character. */
      if (c == '\\' && (src[srcpos] == '\\' || src[srcpos] == '&'))
	c = src[srcpos++];
      if (destlen + 1 >= destalloc) {
	char *old = dest;
	destalloc *= 2;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      dest[destlen++] = c;
    } else if (no >= prog->nsubexp) {
      /* Number too big; prentend it's the empty string */
    } else if (startp[no] != -1 && endp[no] != -1) {
      len = endp[no] - startp[no];
      if (len + destlen >= destalloc) {
	char *old = dest;
	destalloc = 2 * destalloc + len + destlen;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      memcpy(dest + destlen, insrc + startp[no], len);
      destlen += len;
    }
  }
  dest[destlen] = '\0';

  if (lenout)
    *lenout = destlen;

  return dest;
}

/************************************************************/
/*              UTF-8 -> per-byte translation               */
/* Translate a UTF-8-encode regexp over the language of     */
/* unicode code points into a per-byte regexp that matches  */
/* equivalent portionals of a UTF-8-encoded sequences of    */
/* code points.                                             */
/************************************************************/

/* To avoid the broken qsort in Solaris: */
#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static int compare_ranges(const void *a, const void *b)
{
  if (*(unsigned int *)a < *(unsigned int *)b)
    return -1;
  else
    return 1;
}

/* For allocating the traslated string, as we go. When writing an
   original char (or something that takes its place), there's always
   space, but call make_room() before adding new content. */
typedef struct {
  int i;         /* number of original chars written */
  int orig_len;  /* original length */
  int size;      /* allocated size */
} RoomState;

static unsigned char *make_room(unsigned char *r, int j, int need_extra, RoomState *rs)
{
  int nrs;
  unsigned char *nr;

  if ((rs->size - j - (rs->orig_len - rs->i)) < need_extra) {
    nrs = ((rs->size) * 2) + need_extra;
    nr = (char *)scheme_malloc_atomic(nrs+1);
    memcpy(nr, r, j);
    r = nr;
    rs->size = nrs;
  }

  return r;
}

static unsigned char *add_byte_range(const unsigned char *lo, const unsigned char *hi, int count,
				     unsigned char *r, int *_j, RoomState *rs,
				     /* did_alt => no need to start with "|" */
				     int did_alt, 
				     /* wrap_alts => wrap "(?:...)" around multiple alts */
				     int wrap_alts)
     /* Adds alternatives for matching valid UTF-8 encodings lo
	through hi lexicographically. See add_range to get started. */
{
  int same_chars, j, i;
  const unsigned char *lowest = "\200\200\200\200\200";
  const unsigned char *highest = "\277\277\277\277\277";
  unsigned char p, q;

  /* Look for a common prefix: */
  for (same_chars = 0; same_chars < count; same_chars++) {
    if (lo[same_chars] != hi[same_chars])
      break;
  }

  j = *_j;

  /* Match exactly the part that's the same for hi and lo */
  if (same_chars) {
    r = make_room(r, j, 4 + same_chars, rs);
    if (!did_alt) {
      r[j++] = '|';
      did_alt = 1;
    }
    for (i = 0; i < same_chars; i++) {
      r[j++] = lo[i];
    }
  }

  if (same_chars < count) {
    /* We have something like nxxxx to mxxxx where n < m.
       Find p such that p >= n and p0000 >= nxxxx, and
       find q such that q0000 <= mxxxx */
    int choices = 0;

    /* If the xxxxs in nxxxx are 0, then p is n,
       otherwise it's n + 1 */
    for (i = same_chars + 1; i < count; i++) {
      if (lo[i] != 128)
	break;
    }
    if (i == count)
      p = lo[same_chars];
    else {
      p = lo[same_chars] + 1;
      choices++;
    }

    /* If the xxxxs in mxxxx are 0, then q is m,
       otherwise it's m - 1 */
    for (i = same_chars + 1; i < count; i++) {
      if (hi[i] != 191)
	break;
    }
    if (i == count)
      q = hi[same_chars];
    else {
      q = hi[same_chars] - 1;
      choices++;
    }

    if (p <= q)
      choices++;

    if ((wrap_alts || same_chars) && (choices > 1)) {
      r = make_room(r, j, 4, rs);
      if (!did_alt) {
	r[j++] = '|';
	did_alt = 1;
      }
      r[j++] = '(';
      r[j++] = '?';
      r[j++] = ':';
    }

    /* Fill out [nxxxx, p0000) */
    if (p > lo[same_chars]) {
      r = make_room(r, j, 2, rs);
      if (!did_alt) {
	r[j++] = '|';
	did_alt = 1;
      }
      r[j++] = lo[same_chars];
      *_j = j;
      r = add_byte_range(lo XFORM_OK_PLUS same_chars + 1, highest, count - same_chars - 1,
			 r, _j, rs, 1, 1);
      j = *_j;
      p = lo[same_chars] + 1;
      did_alt = 0;
    }
    
    /* Fill out [m0000, mxxxx] */
    if (q < hi[same_chars]) {
      r = make_room(r, j, 2, rs);
      if (!did_alt) {
	r[j++] = '|';
	did_alt = 1;
      }
      r[j++] = hi[same_chars];
      *_j = j;
      r = add_byte_range(lowest, hi  XFORM_OK_PLUS same_chars + 1, count - same_chars - 1,
			 r, _j, rs, 1, 1);
      j = *_j;
      did_alt = 0;

      q = hi[same_chars] - 1;
    }
    
    /* Fill out [p0000,m0000) */
    if (p <= q) {
      /* Make the alternative that lets the initial digit vary,
	 since there's room between the lo and hi leading digit */
      const char *any_str = "[\200-\277]";
      const int any_len = 5;

      r = make_room(r, j, 6 + ((count - same_chars - 1) * any_len), rs);
      if (!did_alt) {
	r[j++] = '|';
	did_alt = 1;
      }
      if (p == q) {
	r[j++] = p;
      } else {
	r[j++] = '[';
	r[j++] = p;
	r[j++] = '-';
	r[j++] = q;
	r[j++] = ']';
      }
      for (i = same_chars + 1; i < count; i++) {
	memcpy(r + j, any_str, any_len);
	j += any_len;
      }
    }

    if ((wrap_alts || same_chars) && (choices > 1)) {
      /* Close out the grouping */
      r = make_room(r, j, 1, rs);
      r[j++] = ')';
    } 
  }

  *_j = j;
  return r;
}

static unsigned char *add_range(unsigned char *r, int *_j, RoomState *rs,
				unsigned int start, unsigned int end, int did_alt)
{
  unsigned int top;
  int count;
  unsigned char lo[6], hi[6];

  /* If this range spans different-sized encodings, split it up
     with a recursive call. */
  if (start <= 0x7FF) {
    top = 0x7FF;
    count = 2;
  } else if (start <= 0xFFFF) {
    top = 0xFFFF;
    count = 3;
  } else if (start <= 0x1FFFFF) {
    top = 0x1FFFFF;
    count = 4;
  } else if (start <= 0x3FFFFFF) {
    top = 0x3FFFFFF;
    count = 5;
  } else {
    top = 0x7FFFFFFF;
    count = 6;
  }

  if (end > top) {
    r = add_range(r, _j, rs, top + 1, end, did_alt);
    end = top;
    did_alt = 0;
  }

  /* At this point, the situation is much like creating a
     regexp to match decimal digits. If we wanted to match the
     range 28 to 75 (inclusive), we'd need three parts:

          2[8-9]|[3-6][0-9]|7[0-5]

     It gets more complex with three digits, say 
     128 to 715:

       12[8-9]|1[3-6][0-9]|[2-6][0-9][0-9]|7[0-0][0-9]|71[0-5]

     but you get the idea. Note that any_str takes the place of
     [0-9].

     This same idea works with UTF-8 "digits", so first encode
     our code-point numbers in UTF-8: */

  scheme_utf8_encode_all(&start, 1, lo);
  scheme_utf8_encode_all(&end, 1, hi);

  return add_byte_range(lo, hi, count, r, _j, rs, did_alt, 0);
}

static int translate(unsigned char *s, int len, char **result)
{
  int j;
  RoomState rs;
  unsigned char *r;

  rs.orig_len = len;
  rs.size = len;
  
  r = (char *)scheme_malloc_atomic(rs.size + 1);

  /* We need to translate if the pattern contains any use of ".", if
     there's a big character in a range, if there's a not-range, or if
     there's a big character before '+', '*', or '?'. */

  for (rs.i = j = 0; rs.i < len;) {
    if (s[rs.i] == '[') {
      int k = rs.i + 1, saw_big = 0;
      int not_mode = 0;

      /* First, check whether we need to translate this particular
	 range. */

      /* Caret at start is special: */
      if ((k < len) && (s[k] == '^')) {
	not_mode = 1;
	k++;
      }
      /* Close bracket start is special: */
      if ((k < len) && (s[k] == ']'))
	k++;
      while ((k < len) && (s[k] != ']')) {
	if (s[k] > 127)
	  saw_big = 1;
	k++;
      }
      if ((k >= len) || (!saw_big && !not_mode)) {
	/* No translation necessary. */
	while (rs.i <= k) {
	  r[j++] = s[rs.i++];
	}
      } else {
	/* Need to translate. */
	char *simple_on;
	Scheme_Object *ranges;
	unsigned int *us, *range_array;
	int ulen, on_count, range_len, rp, p;

	ulen = scheme_utf8_decode(s, rs.i + 1, k, NULL, 0, -1, NULL, 0, 0);
	us = (unsigned int *)scheme_malloc_atomic(ulen * sizeof(unsigned int));
	scheme_utf8_decode(s, rs.i + 1, k, us, 0, -1, NULL, 0, 0);

	/* The simple_on array lists ASCII chars to (not) find
	   for the match */
	simple_on = (char *)scheme_malloc_atomic(128);
	memset(simple_on, 0, 128);
	/* The ranges list is pairs of larger ranges */
	ranges = scheme_null;
	
	p = 0;
	if (not_mode)
	  p++;
	if (us[p] == '-') {
	  simple_on['-'] = 1;
	  p++;
	}

	while (p < ulen) {
	  if (((p + 2) < ulen)
	      && us[p+1] == '-') {
	    int beg = us[p], end = us[p+2];
	    if (end == '-') {
	      FAIL("misplaced hypen within square brackets in pattern");
	      return 0;
	    }
	    if (end < beg) {
	      /* Bad regexp */
	      FAIL("invalid range within square brackets in pattern");	      
	      return 0;
	    }
	      
	    if ((beg > 127) || (end > 127)) {
	      /* A big-char range */
	      ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(us[p]),
							 scheme_make_integer_value_from_unsigned(us[p+2])),
					ranges);
	    } else {
	      /* Small range */
	      int w;
	      for (w = beg; w <= end; w++) {
		simple_on[w] = 1;
	      }
	    }
	    p += 3;
	  } else if (us[p] > 127) {
	    ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(us[p]),
						       scheme_make_integer_value_from_unsigned(us[p])),
				      ranges);
	    p++;
	  } else {
	    if (((p + 1) < ulen) && (us[p] == '-')) {
	      FAIL("misplaced hypen within square brackets in pattern");
	      return 0;
	    }
	    simple_on[us[p]] = 1;
	    p++;
	  }
	}

	/* Turn the ranges list into an array */
	range_len = scheme_list_length(ranges);
	range_array = (unsigned int *)scheme_malloc_atomic(2 * range_len * sizeof(unsigned int));
	for (rp = 0; SCHEME_PAIRP(ranges); ranges = SCHEME_CDR(ranges), rp++) {
	  long hi, lo;
	  scheme_get_unsigned_int_val(SCHEME_CAAR(ranges), &lo);
	  scheme_get_unsigned_int_val(SCHEME_CDR(SCHEME_CAR(ranges)), &hi);
	  range_array[rp] = lo;
	  range_array[rp+1] = hi;
	}
	/* Sort the ranges by the starting index. */
	my_qsort(range_array, range_len >> 1, 2 * sizeof(unsigned long), compare_ranges);
	
	/* If a range starts below 128, fill in the simple array */
	for (rp = 0; rp < range_len; rp += 2) {
	  if (range_array[rp] < 128) {
	    for (p = range_array[rp]; p < 128; p++) {
	      simple_on[p] = 1;
	    }
	    range_array[rp] = 128;
	  }
	}
	
	/* Count simples that are on */
	on_count = 0;
	for (p = 0; p < 128; p++) {
	  if (simple_on[p])
	    on_count++;
	}

	if (not_mode) {
	  /* "Not" mode. We produce something in regular mode */
	  /* Start with "(?:[...]|" for simples. */
	  unsigned int last_end;
	  int did_alt;
	  r = make_room(r, j, 6 + (128 - on_count), &rs);
	  r[j++] = '(';
	  r[j++] = '?';
	  r[j++] = ':';
	  if (on_count < 128) {
	    if (!on_count) {
	      r[j++] = '[';
	      r[j++] = 0;
	      r[j++] = '-';
	      r[j++] = 127;
	      r[j++] = ']';
	    } else {
	      r[j++] = '[';
	      if (!simple_on[']'])
		r[j++] = ']';
	      for (p = 0; p < 128; p++) {
		if ((p != '-') && (p != ']'))
		  if (!simple_on[p])
		    r[j++] = p;
	      }
	      if (!simple_on['-'])
		r[j++] = '-';
	      r[j++] = ']';
	    }
	    did_alt = 0;
	  } else
	    did_alt = 1;
	  last_end = 128;
	  for (rp = 0; rp < range_len; rp += 2) {
	    if (range_array[rp] > last_end) {
	      r = add_range(r, &j, &rs, last_end, range_array[rp] - 1, did_alt);
	      did_alt = 0;
	    }
	    if ((range_array[rp + 1] + 1) > last_end)
	      last_end = range_array[rp + 1] + 1;
	  }
	  if (last_end <= 0x10FFFF) {
	    if (last_end < 0xD800) {
	      r = add_range(r, &j, &rs, last_end, 0xD7FF, did_alt);
	      did_alt = 0;
	      r = add_range(r, &j, &rs, 0xE000, 0x10FFFF, did_alt);
	    } else {
	      r = add_range(r, &j, &rs, last_end, 0x10FFFF, did_alt);
	      did_alt = 0;
	    }
	  }
	  r = make_room(r, j, 1, &rs);
	  r[j++] = ')';
	} else {
	  /* Normal mode */
	  /* Start with "(?:[...]|" for simples. */
	  int p, did_alt;
	  r = make_room(r, j, 5 + on_count, &rs);
	  r[j++] = '(';
	  r[j++] = '?';
	  r[j++] = ':';
	  if (on_count) {
	    if (on_count == 128) {
	      r[j++] = '[';
	      r[j++] = 0;
	      r[j++] = '-';
	      r[j++] = 127;
	      r[j++] = ']';
	    } else {
	      r[j++] = '[';
	      if (simple_on[']'])
		r[j++] = ']';
	      for (p = 0; p < 128; p++) {
		if ((p != '-') && (p != ']'))
		  if (simple_on[p])
		    r[j++] = p;
	      }
	      if (simple_on['-'])
		r[j++] = '-';
	      r[j++] = ']';
	    }
	    did_alt = 0;
	  } else
	    did_alt = 1;
	  for (rp = 0; rp < range_len; rp += 2) {
	    r = add_range(r, &j, &rs, range_array[rp], range_array[rp+1], did_alt);
	    did_alt = 0;
	  }
	  r = make_room(r, j, 1, &rs);
	  r[j++] = ')';
	}
      }
      rs.i = k + 1;
    } else if (s[rs.i] == '\\') {
      /* Skip over next char, possibly big: */
      r[j++] = s[rs.i++];
      if ((rs.i < len)
	  && (s[rs.i] > 127)) {
	r[j++] = s[rs.i++];
	while ((rs.i < len) && ((s[rs.i] & 0xC0) == 0x80)) {
	  r[j++] = s[rs.i++];
	}
      } else
	r[j++] = s[rs.i++];
    } else if (s[rs.i] == '.') {
      /* "." has to be expanded. */
      r = make_room(r, j, 8, &rs);
      r[j++] = '(';
      r[j++] = '?';
      r[j++] = ':';
      r[j++] = '[';
      r[j++] = '\00';
      r[j++] = '-';
      r[j++] = '\177';
      r[j++] = ']';
      r = add_range(r, &j, &rs, 128, 0xD7FF, 0);
      r = add_range(r, &j, &rs, 0xE000, 0x10FFFF, 0);
      r = make_room(r, j, 1, &rs);
      r[j++] = ')';
      rs.i++;
    } else if (s[rs.i] > 127) {
      int k = rs.i + 1;
      /* Look for *, +, or ? after this big char */
      while ((k < len) && ((s[k] & 0xC0) == 0x80)) {
	k++;
      }
      if ((k < len) && ((s[k] == '+')
			|| (s[k] == '*')
			|| (s[k] == '?'))) {
	/* Need to translate; wrap char in (?: ...) */
	r = make_room(r, j, 4, &rs);
	r[j++] = '(';
	r[j++] = '?';
	r[j++] = ':';
	while (rs.i < k) {
	  r[j++] = s[rs.i++];
	}
	r[j++] = ')';
      } else {
	/* No translation. */
	while (rs.i < k) {
	  r[j++] = s[rs.i++];
	}
      }
    } else {
      r[j++] = s[rs.i++];
    }
  }

  r[j] = 0;
  *result = r;
  return j;
}

/************************************************************/
/*                   Scheme front end                       */
/************************************************************/


static Scheme_Object *do_make_regexp(const char *who, int is_byte, int argc, Scheme_Object *argv[])
{
  Scheme_Object *re, *bs;
  char *s;
  int slen;

  if (is_byte) {
    if (!SCHEME_BYTE_STRINGP(argv[0]))
      scheme_wrong_type(who, "byte string", 0, argc, argv);
    bs = argv[0];
  } else {
    if (!SCHEME_CHAR_STRINGP(argv[0]))
      scheme_wrong_type(who, "string", 0, argc, argv);
    bs = scheme_char_string_to_byte_string(argv[0]);
  }

  s = SCHEME_BYTE_STR_VAL(bs);
  slen = SCHEME_BYTE_STRTAG_VAL(bs);

  if (!is_byte) {
    slen = translate(s,slen, &s);
#if 0
    /* Debugging, to see the translated regexp: */
    {
      char *cp;
      int i;
      cp = (char *)scheme_malloc_atomic(slen + 1);
      memcpy(cp, s, slen + 1);
      for (i = 0; i < slen; i++) {
	if (!cp[i]) cp[i] = '0';
      } 
      printf("%d %s\n", slen, cp);
    }
#endif
  }

  re = (Scheme_Object *)regcomp(s, 0, slen);

  if (!is_byte)
    ((regexp *)re)->is_utf8 = 1;

  if (SCHEME_IMMUTABLEP(argv[0]))
    ((regexp *)re)->source = argv[0];
  else if (is_byte) {
    Scheme_Object *src;
    src = scheme_make_immutable_sized_byte_string(SCHEME_BYTE_STR_VAL(argv[0]), 
						  SCHEME_BYTE_STRTAG_VAL(argv[0]), 
						  1);
    ((regexp *)re)->source = src;
  } else {
    Scheme_Object *src;
    src = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(argv[0]), 
						  SCHEME_CHAR_STRTAG_VAL(argv[0]), 
						  1);
    ((regexp *)re)->source = src;
  }
  
  return re;
}

static Scheme_Object *make_regexp(int argc, Scheme_Object *argv[])
{
  return do_make_regexp("byte-regexp", 1, argc, argv);
}

static Scheme_Object *make_utf8_regexp(int argc, Scheme_Object *argv[])
{
  return do_make_regexp("regexp", 0, argc, argv);
}

Scheme_Object *scheme_make_regexp(Scheme_Object *str, int is_byte, int * volatile result_is_err_string)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Object * volatile result;

  *result_is_err_string = 0;

  /* we rely on single-threaded, non-blocking regexp compilation: */
  save = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  failure_msg_for_read = "yes";
  if (!scheme_setjmp(newbuf)) {
    if (is_byte)
      result = make_regexp(1, &str);
    else
      result = make_utf8_regexp(1, &str);
  } else {
    result = (Scheme_Object *)failure_msg_for_read;
    *result_is_err_string = 1;
  }

  failure_msg_for_read = NULL;
  scheme_current_thread->error_buf = save;
  return result;
}

static regexp *regcomp_object(Scheme_Object *str)
{
  if (SCHEME_BYTE_STRINGP(str))
    return (regexp *)make_regexp(1, &str);
  else
    return (regexp *)make_utf8_regexp(1, &str);
}

static Scheme_Object *gen_compare(char *name, int pos, 
				  int argc, Scheme_Object *argv[],
				  int peek, int nonblock)
{
  regexp *r;
  char *full_s;
  rxpos *startp, *endp;
  int offset = 0, orig_offset, endset, m, was_non_byte;
  Scheme_Object *iport, *oport = NULL, *startv = NULL, *endv = NULL, *dropped, *unless_evt = NULL;
  
  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_BYTE_STRINGP(argv[0])
      && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type(name, "regexp, byte-regexp, string, or byte string", 0, argc, argv);
  if ((peek || (!SCHEME_BYTE_STRINGP(argv[1]) && !SCHEME_CHAR_STRINGP(argv[1])))
      && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type(name, peek ? "input-port" : "string, byte string, or input port", 1, argc, argv);
  
  if (SCHEME_CHAR_STRINGP(argv[1])) {
    iport = NULL;
    endset = SCHEME_CHAR_STRLEN_VAL(argv[1]);
  } else if (SCHEME_INPORTP(argv[1])) {
    iport = argv[1];
    endset = -2;
  } else {
    iport = NULL;
    endset = SCHEME_BYTE_STRLEN_VAL(argv[1]);
  }

  if (argc > 2) {
    int len = endset;

    offset = scheme_extract_index(name, 2, argc, argv, len + 1, 0);

    if (!iport && (offset > len)) {
      scheme_out_of_string_range(name, "offset ", argv[2], argv[1], 0, len);
      return NULL;
    } else if (offset < 0) {
      /* argument was a bignum */
      offset = 0x7FFFFFFF;
    }
    startv = argv[2];
      
    if (argc > 3) {
      if (!SCHEME_FALSEP(argv[3])) {
	endset = scheme_extract_index(name, 3, argc, argv, len + 1, 1);
	
	if (iport) {
	  if (endset < 0) {
	    /* argument was a bignum */
	    endset = 0x7FFFFFFF;
	  }
	  /* compare numbers */
	  if (scheme_bin_lt(argv[3], argv[2])) {
	    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			     "%s: ending index %V is smaller than starting index %V for port",
			     name, argv[3], argv[2]);
	    return NULL;
	  }
	} else if (endset < offset || endset > len) {
	  scheme_out_of_string_range(name, "ending ", argv[3], argv[1], offset, len);
	  return NULL;
	}
	endv = argv[3];
      }
      
      if (argc > 4) {
	if (peek) {
	  if (!SCHEME_FALSEP(argv[4])) {
	    unless_evt = argv[4];
	    if (!SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type)) {
	      scheme_wrong_type(name, "progress evt or #f", 4, argc, argv);
	      return NULL;
	    }
	    if (!iport) {
	      scheme_arg_mismatch(name, 
				  "progress evt cannot be used with string input: ",
				  unless_evt);
	    } else if (!SAME_OBJ(iport, SCHEME_PTR1_VAL(unless_evt))) {
	      scheme_arg_mismatch(name,
				  "evt is not a progress evt for the given port:",
				  unless_evt);
	      return NULL;
	    }
	  }
	} else {
	  if (SCHEME_TRUEP(argv[4])) {
	    if (!SCHEME_OUTPORTP(argv[4]))
	      scheme_wrong_type(name, "output-port or #f", 4, argc, argv);
	    oport = argv[4];
	  }
	}
      }
    }
  }

  if (iport && !startv)
    startv = scheme_make_integer(0);

  if (SCHEME_BYTE_STRINGP(argv[0])
      || SCHEME_CHAR_STRINGP(argv[0]))
    r = regcomp_object(argv[0]);
  else
    r = (regexp *)argv[0];

  was_non_byte = 0;
  orig_offset = 0; /* extra offset */
  if (!iport) {
    if (SCHEME_BYTE_STRINGP(argv[1]))
      full_s = SCHEME_BYTE_STR_VAL(argv[1]);
    else {
      /* Extract substring and UTF-8 encode: */
      int blen;
      blen = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(argv[1]), offset, endset,
				NULL, 0,
				0 /* not UTF-16 */);
      full_s = (char *)scheme_malloc_atomic(blen);
      scheme_utf8_encode(SCHEME_CHAR_STR_VAL(argv[1]), offset, endset,
			 full_s, 0,
			 0 /* not UTF-16 */);
      orig_offset = offset;
      offset = 0;
      endset = blen;
      if (r->is_utf8)
	was_non_byte = 1;
      else {
	/* Convert orig_offset into encoded bytes */
	orig_offset = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(argv[1]), 0, orig_offset,
					 NULL, 0,
					 0);
      }
    }
  } else
    full_s = NULL;

  startp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

  dropped = scheme_make_integer(0);

  m = regexec(name, r, full_s, offset, endset - offset, startp, endp,
	      iport, unless_evt, nonblock,
	      &full_s, peek, pos, oport, 
	      startv, endv, &dropped);

  if (m) {
    int i;
    Scheme_Object *l = scheme_null, *rs;

    if (oport && !iport)
      scheme_put_byte_string(name, oport, full_s, 0, *startp, 0);

    for (i = r->nsubexp; i--; ) {
      if (startp[i] != -1) {
	if (pos) {
	  Scheme_Object *startpd, *endpd;

	  if (was_non_byte) {
	    /* Need to figure out how startpd and endpd correspond to
	       code points. Note that the input regexp matches only
	       unicode chars, so the start and end points can't be in
	       the middle of encoded characters. */
	    int uspd, uepd;
	    uspd = scheme_utf8_decode(full_s, offset, startp[i],
				      NULL, 0, -1,
				      NULL, 0, 0);
	    uspd += orig_offset;
	    startpd = scheme_make_integer(uspd);
	    uepd = scheme_utf8_decode(full_s, startp[i], endp[i],
				      NULL, 0, -1,
				      NULL, 0, 0);
	    uepd += uspd;
	    endpd = scheme_make_integer(uepd);
	  } else {
	    int v;
	    v = startp[i] + orig_offset;
	    startpd = scheme_make_integer(v);
	    v = endp[i] + orig_offset;
	    endpd = scheme_make_integer(v);
	    
	    if (iport) {
	      /* Increment by drop count: */
	      startpd = scheme_bin_plus(startpd, dropped);
	      endpd = scheme_bin_plus(endpd, dropped);
	    }
	  }
	  
	  l = scheme_make_pair(scheme_make_pair(startpd, endpd),
			       l);
	} else {
	  long len;
	  len = endp[i] - startp[i];
	  if (was_non_byte) {
	    rs = scheme_make_sized_offset_utf8_string(full_s, startp[i], len);
	  } else {
	    rs = scheme_make_sized_offset_byte_string(full_s, startp[i], len, 1);
	  }
	  l = scheme_make_pair(rs, l);
	}
      } else
	l = scheme_make_pair(scheme_false, l);
    }

    return l;
  } else {
    if (oport && !iport)
      scheme_put_byte_string(name, oport, full_s, 0, endset, 0);

    return scheme_false;
  }
}

static Scheme_Object *compare(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match", 0, argc, argv, 0, 0);
}

static Scheme_Object *positions(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-positions", 1, argc, argv, 0, 0);
}

static Scheme_Object *compare_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek", 0, argc, argv, 1, 0);
}

static Scheme_Object *positions_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions", 1, argc, argv, 1, 0);
}

static Scheme_Object *compare_peek_nonblock(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-immediate", 0, argc, argv, 1, 1);
}

static Scheme_Object *positions_peek_nonblock(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions-immediate", 1, argc, argv, 1, 1);
}

static Scheme_Object *gen_replace(const char *name, int argc, Scheme_Object *argv[], int all)
{
  Scheme_Object *orig;
  regexp *r;
  char *source, *prefix = NULL, *deststr;
  rxpos *startp, *endp;
  int prefix_len = 0, sourcelen, srcoffset = 0, was_non_byte, destlen;

  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_BYTE_STRINGP(argv[0])
      && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type(name, "regexp, byte-regexp, string, or byte string", 0, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[1])
      && !SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type(name, "string or byte string", 1, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[2])
      && !SCHEME_CHAR_STRINGP(argv[2]))
    scheme_wrong_type(name, "string or byte string", 2, argc, argv);

  if (SCHEME_BYTE_STRINGP(argv[1])) {
    if (!SCHEME_BYTE_STRINGP(argv[2])) {
      scheme_arg_mismatch(name, "cannot replace a byte string with a string: ",
			  argv[2]);
    }
  } else {
    if (!SCHEME_CHAR_STRINGP(argv[2])) {
      scheme_arg_mismatch(name, "cannot replace a string with a byte string: ",
			  argv[2]);
    }
  }

  if (SCHEME_BYTE_STRINGP(argv[0])
      || SCHEME_CHAR_STRINGP(argv[0]))
    r = regcomp_object(argv[0]);
  else
    r = (regexp *)argv[0];

  if (SCHEME_CHAR_STRINGP(argv[1])) {
    orig = scheme_char_string_to_byte_string(argv[1]);
    if (r->is_utf8)
      was_non_byte = 1;
    else
      was_non_byte = 0;
  } else {
    orig = argv[1];
    was_non_byte = 0;
  }
  source = SCHEME_BYTE_STR_VAL(orig);
  sourcelen = SCHEME_BYTE_STRTAG_VAL(orig);
  deststr = NULL;
  destlen = 0;

  startp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

  while (1) {
    int m;

    m = regexec("regexp-replace", r, source, srcoffset, sourcelen - srcoffset, startp, endp,
		NULL, NULL, 0,
		NULL, 0, 0, NULL, NULL, NULL, NULL);

    if (m) {
      char *insert;
      long len, end, startpd, endpd;

      if ((startp[0] == endp[0]) && all) {
	scheme_arg_mismatch(name, 
			    "found a zero-width match for pattern: ",
			    argv[0]);
	return NULL;
      }
      
      if (!deststr) {
	if (was_non_byte) {
	  Scheme_Object *bs;
	  bs = scheme_char_string_to_byte_string(argv[2]);
	  deststr = SCHEME_BYTE_STR_VAL(bs);
	  destlen = SCHEME_BYTE_STRTAG_VAL(bs);
	} else {
	  deststr = SCHEME_BYTE_STR_VAL(argv[2]);
	  destlen = SCHEME_BYTE_STRTAG_VAL(argv[2]);
	}
      }

      insert = regsub(r, deststr, destlen, &len, source, startp, endp);
      
      end = sourcelen;
      
      startpd = startp[0];
      endpd = endp[0];

      if (!startpd && (endpd == end) && !prefix) {
	if (was_non_byte)
	  return scheme_make_sized_utf8_string(insert, len);
	else
	  return scheme_make_sized_byte_string(insert, len, 0);
      } else if (!all) {
	char *result;
	long total;
	
	total = len + (startpd - srcoffset) + (end - endpd);
	
	result = (char *)scheme_malloc_atomic(total + 1);
	memcpy(result, source + srcoffset, startpd - srcoffset);
	memcpy(result + (startpd - srcoffset), insert, len);
	memcpy(result + (startpd - srcoffset) + len, source + endpd, (end - endpd) + 1);
	
	if (was_non_byte)
	  return scheme_make_sized_utf8_string(result, total);
	else
	  return scheme_make_sized_byte_string(result, total, 0);
      } else {
	char *naya;
	long total;
	
	total = len + prefix_len + (startpd - srcoffset);
	
	naya = (char *)scheme_malloc_atomic(total + 1);
	memcpy(naya, prefix, prefix_len);
	memcpy(naya + prefix_len, source + srcoffset, startpd - srcoffset);
	memcpy(naya + prefix_len + (startpd - srcoffset), insert, len);

	prefix = naya;
	prefix_len = total;

	srcoffset = endpd;
      }
    } else if (!prefix) {
      if (was_non_byte)
	return argv[1];
      else
	return orig;
    } else {
      char *result;
      long total, slen;
      
      slen = sourcelen - srcoffset;
      total = prefix_len + slen;
      
      result = (char *)scheme_malloc_atomic(total + 1);
      memcpy(result, prefix, prefix_len);
      memcpy(result + prefix_len, source + srcoffset, slen);
      result[prefix_len + slen] = 0;
      
      if (was_non_byte)
	return scheme_make_sized_utf8_string(result, total);
      else
	return scheme_make_sized_byte_string(result, total, 0);
    }

    SCHEME_USE_FUEL(1);
  }
}

static Scheme_Object *replace(int argc, Scheme_Object *argv[])
{
  return gen_replace("regexp-replace", argc, argv, 0);
}

static Scheme_Object *replace_star(int argc, Scheme_Object *argv[])
{
  return gen_replace("regexp-replace*", argc, argv, 1);
}

static Scheme_Object *regexp_p(int argc, Scheme_Object *argv[])
{
  return (((SCHEME_TYPE(argv[0]) == scheme_regexp_type) 
	   && ((regexp *)argv[0])->is_utf8)
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *byte_regexp_p(int argc, Scheme_Object *argv[])
{
  return (((SCHEME_TYPE(argv[0]) == scheme_regexp_type) 
	   && !((regexp *)argv[0])->is_utf8)
	  ? scheme_true 
	  : scheme_false);
}

Scheme_Object *scheme_regexp_source(Scheme_Object *re)
{
  return ((regexp *)re)->source;
}

int scheme_regexp_is_byte(Scheme_Object *re)
{
  return !((regexp *)re)->is_utf8;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#define MARKS_FOR_REGEXP_C
#include "mzmark.c"
END_XFORM_SKIP;
#endif

void scheme_regexp_initialize(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  GC_REG_TRAV(scheme_regexp_type, mark_regexp);
  GC_REG_TRAV(scheme_rt_regwork, mark_regwork);
#endif

  REGISTER_SO(regparsestr);
  REGISTER_SO(regstr);

  scheme_add_global_constant("byte-regexp", 
			     scheme_make_prim_w_arity(make_regexp, 
						      "byte-regexp", 
						      1, 1), 
			     env);
  scheme_add_global_constant("regexp", 
			     scheme_make_prim_w_arity(make_utf8_regexp, 
						      "regexp", 
						      1, 1), 
			     env);
  scheme_add_global_constant("regexp-match",
			     scheme_make_prim_w_arity(compare,
						      "regexp-match",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-positions", 
			     scheme_make_prim_w_arity(positions, 
						      "regexp-match-positions", 
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-peek",
			     scheme_make_prim_w_arity(compare_peek,
						      "regexp-match-peek",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-peek-positions", 
			     scheme_make_prim_w_arity(positions_peek, 
						      "regexp-match-peek-positions",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-peek-immediate",
			     scheme_make_prim_w_arity(compare_peek_nonblock,
						      "regexp-match-peek-immediate",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-peek-positions-immediate", 
			     scheme_make_prim_w_arity(positions_peek_nonblock, 
						      "regexp-match-peek-positions-immediate",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-replace", 
			     scheme_make_prim_w_arity(replace, 
						      "regexp-replace", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp-replace*", 
			     scheme_make_prim_w_arity(replace_star, 
						      "regexp-replace*", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp?", 
			     scheme_make_prim_w_arity(regexp_p, 
						      "regexp?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("byte-regexp?", 
			     scheme_make_prim_w_arity(byte_regexp_p, 
						      "byte-regexp?", 
						      1, 1), 
			     env);
}

#endif
/* NO_REGEXP_UTILS */
