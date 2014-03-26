/*
 * @(#)regexp.c 1.3 of 18 April 87
 * Revised for PLT Racket, 1995-2001
 * Copyright (c) 2004-2014 PLT Design Inc.
 *
 * Copyright (c) 1986 by University of Toronto.
 * Written by Henry Spencer.  Not derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose on any computer system, and to redistribute it freely,
 * subject to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of
 *    this software, no matter how awful, even if they arise
 *    from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either
 *    by explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 *
 * Notable changes for Racket:
 *   Removed hardwired limits on parenthesis nesting
 *   Changed to index-based instead of pointer-based (better for GC)
 *   Added non-greedy operators *?, +?, and ??
 *   Added (?:...) grouping without reporting the group match
 *   Added (?=...), (?!...), (?<=...), and (?<!...) lookahead and lookback
 *   Added \n backreferences
 *   Added numeric quantifiers
 *   Added case-insensitive and multi-line modes
 *   Added Racket glue
 *
 * from Vladimir Tsyshevsky:
 *  additional optional parameter `offset' in `regexp-match'
 *  and `regexp-match-positions'
 */

#include "schpriv.h"
#include "schmach.h"
#include "schgencat.h"
#include "schrx.h"

#include <stdio.h>
#include <string.h>

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define BIGGEST_RXPOS 0x7FFFFFFFFFFFFFFF
#else
# define BIGGEST_RXPOS 0x7FFFFFFF
#endif

# define rOP(o) OP(o, regstr)
# define rNEXT(o) NEXT(o, regstr)
# define rOPLEN(o) OPLEN(o, regstr)
# define rOPRNGS(o) OPRNGS(o, regstr)
# define NEXT_OP(scan) (scan + rNEXT(scan))

static regexp *regcomp(char *, rxpos, int, int);
/* static int regexec(regexp *, char *, int, int, rxpos *, rxpos * ...); */

/*
 * Global work variables for regcomp().
 */
THREAD_LOCAL_DECL(static char *regstr);
THREAD_LOCAL_DECL(static char *regparsestr);
THREAD_LOCAL_DECL(static int regmatchmin);
THREAD_LOCAL_DECL(static int regmatchmax);
THREAD_LOCAL_DECL(static int regmaxbackposn);
THREAD_LOCAL_DECL(static int regsavepos);

THREAD_LOCAL_DECL(static Scheme_Hash_Table *regbackknown); /* known/assumed backreference [non-]empty */
THREAD_LOCAL_DECL(static Scheme_Hash_Table *regbackdepends); /* backreferences required to be non-empty for the current to be non-empty */

THREAD_LOCAL_DECL(static rxpos regparse);
THREAD_LOCAL_DECL(static rxpos regparse_end); /* Input-scan pointer. */
THREAD_LOCAL_DECL(static int regnpar);       /* () count. */
THREAD_LOCAL_DECL(static int regncounter);   /* {} count */
THREAD_LOCAL_DECL(static rxpos regcode) ;    /* Code-emit pointer, if less than regcodesize */
THREAD_LOCAL_DECL(static rxpos regcodesize);
THREAD_LOCAL_DECL(static rxpos regcodemax);
THREAD_LOCAL_DECL(static intptr_t regmaxlookback);

/* caches to avoid gc */
THREAD_LOCAL_DECL(static intptr_t rx_buffer_size);
THREAD_LOCAL_DECL(static rxpos *startp_buffer_cache);
THREAD_LOCAL_DECL(static rxpos *endp_buffer_cache);
THREAD_LOCAL_DECL(static rxpos *maybep_buffer_cache);
THREAD_LOCAL_DECL(static rxpos *match_stack_buffer_cache);

#define MATCH_STACK_SIZE 24

/*
 * Forward declarations for regcomp()'s friends.
 */
static rxpos reg(int, int *, int, int, int);
static rxpos regbranch(int *, int, int);
static rxpos regpiece(int *, int, int);
static rxpos regatom(int *, int, int);
static rxpos regranges(int parse_flags, int at_start);
static rxpos regunicode(int invert);
static int regdigit();
static rxpos regnode(char);
static void regarg(int);
static rxpos regnext(rxpos);
static void regc(char);
static void reginsert(char, rxpos);
static rxpos reginsertwithop(char, rxpos, int);
static rxpos reginsertwithopop(char, rxpos, int, int);
static void regtail(rxpos, rxpos);
static void regoptail(rxpos, rxpos);
static int regstrcspn(char *, char *, char *);
static unsigned char *extract_regstart(rxpos scan, int *_anch);

static int check_and_propagate_depends(void);
static int merge_tables(Scheme_Hash_Table *dest, Scheme_Hash_Table *src);

READ_ONLY static Scheme_Object *empty_byte_string;

#define	FAIL(m)	{ regcomperror(m); return 0; }

static void
regerror(char *s)
{
  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		   "regexp: %s", s);
}

THREAD_LOCAL_DECL(const char *failure_msg_for_read);

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
regcomp(char *expstr, rxpos exp, int explen, int pcre)
{
  regexp *r;
  rxpos scan, next;
  rxpos longest;
  int len, longest_is_ci;
  int flags;

  /* First pass: determine size, legality. */
  regstr = NULL;
  regparsestr = expstr;
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regncounter = 0;
  regmaxlookback = 0;
  regcode = 1;
  regcodesize = 0;
  regcodemax = 0;
  regmaxbackposn = 0;
  regbackknown = NULL;
  regbackdepends = NULL;
  regc(MAGIC);
  if (reg(0, &flags, 0, 0, PARSE_CASE_SENS | PARSE_SINGLE_LINE | (pcre ? PARSE_PCRE : 0)) == 0) {
    FAIL("unknown regexp failure");
  }
  
  /* Small enough for pointer-storage convention? */
  if (regcodemax >= 32767L)		/* Probably could be 65535L. */
    FAIL("regexp too big");

  if (regmaxbackposn >= regnpar)
    FAIL("backreference number is larger than the highest-numbered cluster");
  
  /* Allocate space. */
  r = (regexp *)scheme_malloc_tagged(sizeof(regexp) + N_ITO_SPACE((unsigned)regcodemax));
  r->type = scheme_regexp_type;
  
#ifdef INDIRECT_TO_PROGRAM
  r->program = (char *)scheme_malloc_atomic((unsigned)regcodemax + 1);
#endif
  
  r->regsize = regcodemax;

  r->nsubexp = regnpar;
  r->ncounter = regncounter;
  r->maxlookback = regmaxlookback;
  
  /* Second pass: emit code. */
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regncounter = 0;
  regcodesize = regcodemax;
#ifdef INDIRECT_TO_PROGRAM
  regstr = r->program;
  regcode = 0;
#else
  regstr = (char *)r;
  regcode = (char *)r->program - (char *)r;
#endif
  regcodesize += regcode;
  regcodemax = 0;
  regbackknown = NULL;
  regbackdepends = NULL;
  regc(MAGIC);
  if (reg(0, &flags, 0, 0, PARSE_CASE_SENS | PARSE_SINGLE_LINE | (pcre ? PARSE_PCRE : 0)) == 0) {
    FAIL("unknown regexp failure (late)");
  }

  if (regcode >= regcodesize) {
    FAIL("wrote too far");
  }

  /* Dig out information for optimizations. */
  r->regstart = NULL;	/* Worst-case defaults. */
  r->regmust = -1;
  r->regmlen = 0;
  scan = N_ITO_DELTA(r->program, 1, (char *)r);    /* First BRANCH. */
  {
    unsigned char *rs;
    int anch = 0;
    rs = extract_regstart(scan, &anch);
    r->regstart = rs;
    if (anch)
      r->flags |= REGEXP_ANCH;
  }
  next = regnext(scan);
  if (rOP(next) == END) {	/* Only one top-level choice. */
    scan = OPERAND(scan);
    /*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     */
    if (flags&SPSTART) {
      int prev_op = 0;
      longest = 0;
      longest_is_ci = 0;
      len = 0;
      for (; scan != 0; scan = regnext(scan)) {
        int mscan = scan;
        while (1) {
          int mop;
          mop = rOP(mscan);
          if (((mop == EXACTLY) || (mop == EXACTLY_CI))
              && rOPLEN(OPERAND(mscan)) >= len) {
            /* Skip regmust if it contains a null character: */
            rxpos ls = OPSTR(OPERAND(mscan));
            int ll = rOPLEN(OPERAND(mscan)), i;
            for (i = 0; i < ll; i++) {
              if (!regstr[ls + i])
                break;
            }
            if (i >= ll) {
              longest = ls;
              len = ll;
              longest_is_ci = (rOP(mscan) == EXACTLY_CI);
            }
            break;
          } else if ((mop == EXACTLY1) && (1 >= len)) {
            /* Skip if it's a null character */
            if (regstr[OPERAND(mscan)]) {
              longest = OPERAND(mscan);
              len = 1;
              longest_is_ci = 0;
            }
            break;
          } else if ((mop == BRANCH) && (prev_op != BRANCH)) {
            int mnext;
            mnext = NEXT_OP(mscan);
            if (rOP(mnext) != BRANCH) {
              /* A branch with only one choice */
              mscan = OPERAND(mscan);
            } else
              break;
          } else
            break;
        }
        prev_op = rOP(scan);
      }
      if (longest) {
	r->regmust = longest;
	if (longest_is_ci)
	  r->flags |= REGEXP_MUST_CI;
	r->regmlen = len;
      }
    }
  }

#if 0
  if (regcode > r->regsize + sizeof(regexp))
    scheme_signal_error("regexp too large!");
#endif
  
  return(r);
}

static unsigned char *map_create(unsigned char *map)
{
  if (!map) {
    map = (unsigned char *)scheme_malloc_atomic(32);
    memset(map, 0, 32);
  }
  return map;
}

static unsigned char *map_start(unsigned char *map, int c)
{
  map = map_create(map);
  map[c >> 3] |= ((unsigned char)1 << (c & 0x7));
  return map;
}

static unsigned char *map_copy(unsigned char *map, char *s, int pos)
{
  map = map_create(map);
  memcpy(map, s XFORM_OK_PLUS pos, 32);
  return map;
}

static unsigned char *map_range(unsigned char *map, char *s, int pos, int invert)
{
  int rs, re;

  rs = UCHAR(s[pos++]);
  re = UCHAR(s[pos++]);

  if (!invert) {
    while (rs <= re) {
      map = map_start(map, rs);
      rs++;
    }
  } else {
    while (rs > 0) {
      map = map_start(map, rs - 1);
      --rs;
    }
    while (re < 255) {
      map = map_start(map, re + 1);
      re++;
    }
  }

  return map;
}

static unsigned char *extract_regstart(rxpos scan, int *_anch)
{
  rxpos next;
  int retry, the_op;
  unsigned char *map = NULL;

  do {
    retry = 0;
    
    the_op = rOP(scan);
    switch (the_op) {
    case BOL:
    case EOL:
    case NOTHING:
    case SAVECONST:
    case MAYBECONST:
    case COUNTINIT:
    case COUNTOVER:
    case COUNTUNDER:
      /* We can ignore zero-length things when finding starting info */
      scan = regnext(scan);
      retry = 1;
      break;
    case LOOKT:
    case LOOKF:
    case LOOKBT:
    case LOOKBF:
      /* Zero-length, but continuation in an unusual place */
      scan += rOPLEN(OPERAND(scan));
      scan = regnext(scan);
      retry = 1;
      break;
    case LOOKTX:
      scan = regnext(scan);
      retry = 1;
      break;
    case PLUS:
    case PLUS2:
      scan = OPERAND(scan);
      retry = 1;
      break;
    case STAR3:
    case STAR4:
      if (rOPLEN(OPERAND(scan)) > 0) {
	scan = OPERAND3(scan);
	retry = 1;
      }
      break;
    case EXACTLY:
      map = map_start(map, UCHAR(regstr[OPSTR(OPERAND(scan))]));
      break;
    case EXACTLY_CI:
      {
	int c = UCHAR(regstr[OPSTR(OPERAND(scan))]);
	map = map_start(map, c);
	map = map_start(map, rx_toupper(c));
      }
      break;
    case ANYOF:
      map = map_copy(map, regstr, OPERAND(scan));
      break;
    case EXACTLY1:
      map = map_start(map, UCHAR(regstr[OPERAND(scan)]));
      break;
    case EXACTLY2:
      map = map_start(map, UCHAR(regstr[OPERAND(scan)]));
      map = map_start(map, UCHAR(regstr[OPERAND(scan)+1]));
      break;
    case RANGE:
      map = map_range(map, regstr, OPERAND(scan), 0);
      break;
    case NOTRANGE:
      map = map_range(map, regstr, OPERAND(scan), 1);
      break;
    case BOI:
      if (_anch)
	*_anch = 1;
      break;
    case BRANCH:
      next = regnext(scan);
      if (!next || (rOP(next) == END) || (rOP(next) == LOOKE)) {
	/* Only one branch */
	scan = OPERAND(scan);
	retry = 1;
      }
      break;
    default:
      if ((the_op == OPENN) || (the_op >= OPEN && the_op < CLOSE)) {
	scan = NEXT_OP(scan);
	retry = 1;
      }
      break;
    }
  } while (retry);

  return map;
}

#ifdef DO_STACK_CHECK

static Scheme_Object *reg_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  int *flagp = (int *)p->ku.k.p1;
  int res;

  p->ku.k.p1 = NULL;

  res = reg(p->ku.k.i1, flagp, p->ku.k.i2, p->ku.k.i3, p->ku.k.i4);

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
reg(int paren, int *flagp, int paren_set, int lookahead, int parse_flags)
{
  rxpos ret;
  rxpos br;
  rxpos ender;
  int parno = 0;
  int flags, matchmin, matchmax, maxlookback, brcount;
  Scheme_Hash_Table *backdepends;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *ov;
      p->ku.k.i1 = paren;
      p->ku.k.p1 = (void *)flagp;
      p->ku.k.i2 = paren_set;
      p->ku.k.i3 = lookahead;
      p->ku.k.i4 = parse_flags;
      ov = scheme_handle_stack_overflow(reg_k);
      return SCHEME_INT_VAL(ov);
    }
  }
#endif

  *flagp = HASWIDTH;		/* Tentatively. */

  /* Make an OPEN node, if parenthesized. */
  if (paren) {
    if (lookahead) {
      parno = 0;
      ret = regnode(lookahead);
      regarg(0); /* space for LOOKE pointer */
      if ((lookahead == LOOKBT) || (lookahead == LOOKBF)) {
	regarg(0); /* space for min count */
	regarg(0); /* space for max count */
      }
    } else if (paren_set) {
      parno = regnpar;
      regnpar++;
      if (OPEN + parno >= CLOSE) {
	ret = regnode(OPENN);
	regarg(parno);
      } else {
	ret = regnode(OPEN+parno);
      }
    } else
      ret = 0;
  } else
    ret = 0;

  /* Pick up the branches, linking them together. */
  br = regbranch(&flags, parse_flags, 0);
  if (br == 0)
    FAIL("branch failed!?");
  if (ret != 0)
    regtail(ret, br);		/* OPEN -> first. */
  else
    ret = br;
  if (!(flags&HASWIDTH)) {
    *flagp &= ~HASWIDTH;
    backdepends = NULL;
  } else if (regbackdepends) {
    backdepends = regbackdepends;
    regbackdepends = NULL;
  } else
    backdepends = NULL;
  *flagp |= flags&(SPSTART|SPFIXED);
  matchmin = regmatchmin;
  matchmax = regmatchmax;
  maxlookback = regmaxlookback;
  brcount = 1;
  while (regparsestr[regparse] == '|') {
    brcount++;
    regparse++;
    br = regbranch(&flags, parse_flags, 0);
    if (br == 0)
      FAIL("next branch failed!?");
    regtail(ret, br);		/* BRANCH -> BRANCH. */
    if (!(flags&HASWIDTH))
      *flagp &= ~HASWIDTH;
    else if ((*flagp) & HASWIDTH) {
      if (regbackdepends) {
	if (backdepends)
	  merge_tables(backdepends, regbackdepends);
	else
	  backdepends = regbackdepends;
	regbackdepends = NULL;
      } else
	backdepends = NULL;
    }
    *flagp |= flags&SPSTART;
    if (!(flags&SPFIXED))
      *flagp &= ~SPFIXED;
    else {
      if (regmatchmin < matchmin)
	matchmin = regmatchmin;
      if (regmatchmax > matchmax)
	matchmax = regmatchmax;
      if (regmaxlookback > maxlookback)
        maxlookback = regmaxlookback;
    }
  }
  regbackdepends = backdepends;
  regmatchmin = matchmin;
  regmatchmax = matchmax;
  regmaxlookback = maxlookback;

  if (paren && paren_set) {
    Scheme_Object *assumed;

    if (!regbackknown)
      regbackknown = scheme_make_hash_table(SCHEME_hash_ptr);
    assumed = scheme_hash_get(regbackknown, scheme_make_integer(parno));

    if (!((*flagp) & HASWIDTH)) {
      if (assumed && !SCHEME_FALSEP(assumed)) {
	FAIL("`*', `+', or `{...,}' operand can be empty due to backreference");
      }
      scheme_hash_set(regbackknown, scheme_make_integer(parno), scheme_false);
    } else {
      if (!backdepends)
	scheme_hash_set(regbackknown, scheme_make_integer(parno), scheme_true);
      else {
	if (assumed) {
	  check_and_propagate_depends();
	} else
	  scheme_hash_set(regbackknown, scheme_make_integer(parno), (Scheme_Object *)backdepends);
      }
    }
  }

  if ((brcount == 1)
      && paren 
      && (!paren_set || ((flags & SPFIXED) 
			 && (regmatchmin == regmatchmax)
			 && (regmatchmax < 0x7FFFF)))
      && !lookahead) {
    /* Simplify to just the single branch: */
    if (br + 3 < regcodesize) {
      int top;
      if (regcode <= regcodesize)
	top = regcode;
      else
	top = regcodesize;
      memmove(regstr + ret, regstr + br + 3, top - (br + 3));
    }
    *flagp = flags;
    regcode -= (br + 3 - ret);
    if (paren_set) {
      /* Collude with regpiece: */
      *flagp |= NEEDSAVECONST;
      *flagp &= ~SPNOTHING;
      regsavepos = parno;
    }
  } else {
    if (lookahead) {
      if ((lookahead == LOOKBT) || (lookahead == LOOKBF)) {
	if (!((*flagp) & SPFIXED))
	  FAIL("lookbehind pattern does not match a bounded byte width");
	if (matchmax > 0x7FFF)
	  FAIL("lookbehind match is potentially too long (more than 32767 bytes)");
        regmaxlookback = matchmax + maxlookback;
	if (ret + 8 < regcodesize) {
	  regstr[ret + 5] = (matchmin >> 8);
	  regstr[ret + 6] = (matchmin & 255);
	  regstr[ret + 7] = (matchmax >> 8);
	  regstr[ret + 8] = (matchmax & 255);
	}
      }
    }

    /* Make a closing node, and hook it on the end. */
    if (paren) {
      if (lookahead) {
	ender = regnode(LOOKE);
	if (ret + 4 < regcodesize) {
	  int delta = (ender - ret);
	  regstr[ret + 3] = (delta >> 8);
	  regstr[ret + 4] = (delta & 255);
	}
      } else if (paren_set) {
	if (OPEN + parno >= CLOSE) {
	  ender = regcode;
	  regarg(parno);
	  reginsert(CLOSEN, ender);
	} else
	  ender = regnode(CLOSE+parno);
      } else {
	ender = regnode(NOTHING);
      }
    } else {
      ender = regnode(END);	
    }
    regtail(ret, ender);

    /* Hook the tails of the branches to the closing node. */
    if (regcodesize) {
      for (br = ret; br != 0; br = regnext(br)) {
	regoptail(br, ender);
      }
    }
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
regbranch(int *flagp, int parse_flags, int without_branch_node)
{
  rxpos ret;
  rxpos chain, latest;
  int flags = 0, matchmin = 0, matchmax = 0, maxlookback = 0, pcount = 0, save_flags;

  *flagp = (WORST|SPFIXED);		/* Tentatively. */

  if (!without_branch_node)
    ret = regnode(BRANCH);
  else
    ret = 0;
  chain = 0;
  while (regparse != regparse_end 
	 && regparsestr[regparse] != '|' 
	 && regparsestr[regparse] != ')') {
    save_flags = flags;
    latest = regpiece(&flags, parse_flags, !chain && !without_branch_node);
    if (latest == 0)
      FAIL("piece failed!?");
    if (flags & SPNOTHING) {
      /* no need to match nothing */
      regcode = latest; /* throw away dead code */
      flags = save_flags; /* in case all but the first is discarded */
    } else {
      pcount++;
      *flagp |= flags&HASWIDTH;
      if (chain == 0) {		/* First piece. */
	*flagp |= flags&SPSTART;
	if (without_branch_node)
	  ret = latest;
      } else
	regtail(chain, latest);
      if (!(flags&SPFIXED))
	*flagp &= ~SPFIXED;
      if ((regmaxlookback - matchmin) > maxlookback)
        maxlookback = regmaxlookback - matchmin;
      matchmin += regmatchmin;
      matchmax += regmatchmax;
      if (matchmax > 0x7FFF)
	matchmax = 0x10000;
      chain = latest;
    }
  }
  regmatchmin = matchmin;
  regmatchmax = matchmax;
  regmaxlookback = maxlookback;
  if (chain == 0) {  /* Loop ran zero times. */
    latest = regnode(NOTHING);
    if (without_branch_node)
      ret = latest;
    *flagp = SIMPLE|SPNOTHING|SPFIXED;
    regmatchmin = regmatchmax = 0;
  }

  if (pcount == 1) {
    *flagp = flags; /* BRANCH will be deleted if simplicity is relevant */
  }

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
regpiece(int *flagp, int parse_flags, int at_start)
{
  rxpos ret;
  char op;
  rxpos next;
  int flags, greedy;
  int minreps = 0, maxreps = 0, counter;
  int origsavepos, origmatchmin, origmatchmax;

  ret = regatom(&flags, parse_flags, at_start);
  if (ret == 0)
    FAIL("atom failed!?");

  origsavepos = regsavepos;
  origmatchmin = regmatchmin;
  origmatchmax = regmatchmax;

  op = regparsestr[regparse];
  if (!ISMULT(op, parse_flags)) {
    *flagp = (flags & ~NEEDSAVECONST);
  } else {
    if (op == '{') {
      int ch, maxspec = 0;
      minreps = maxreps = 0;
      regparse++;
      do {
	ch = regparsestr[regparse];
	if ((ch >= '0') && (ch <= '9')) {
	  minreps = (minreps * 10) + (ch - '0');
	  if (minreps > 0x7FFF)
	    FAIL("minimum repetition count too large");
	  regparse++;
	} else if (ch == ',' || ch == '}')
	  break;
	else {
	  FAIL("expected digit, comma, or `}' to end repetition specification started with `{'");
	}
      } while (1);
      if (ch == ',') {
	regparse++;
	do {
	  ch = regparsestr[regparse];
	  if ((ch >= '0') && (ch <= '9')) {
	    maxspec = 1;
	    maxreps = (maxreps * 10) + (ch - '0');
	    if (maxreps > 0x7FFF)
	      FAIL("maximum repetition count too large");
	    regparse++;
	  } else if (ch == '}')
	    break;
	  else {
	    FAIL("expected digit or `}' to end repetition specification started with `{'");
	  }
	} while (1);
      } else {
	maxspec = 1;
	maxreps = minreps;
      }
      if (maxspec && (maxreps < minreps)) {
	FAIL("maximum repetition count is less than maximum repetition count");
      }
      if (maxspec && !maxreps) {
	/* Match 0 instances */
	regparse++;
	if (regparsestr[regparse] == '?')
	  regparse++; /* non-greedy */
	if (ISMULT(regparsestr[regparse], parse_flags))
	  FAIL("nested `*', `?', `+', or `{...}' in pattern");
	regcode = ret; /* throw away dead code */
	*flagp = SPFIXED|SPNOTHING;
	regmatchmin = regmatchmax = 0;
	return regnode(NOTHING);
      }
      op = '*';
      if (maxreps || minreps)
	counter = regncounter++;
      else
	counter = 0;
    } else
      counter = 0;

    if (!(flags&HASWIDTH) && (op != '?')) {
      FAIL("`*', `+', or `{...}' operand could be empty");
    }

    if (regbackdepends) {
      /* Operand has width only if the indicated backreferences have width. */
      check_and_propagate_depends();
      /* Assumptions are registered, so we no longer need these backdepends: */
      regbackdepends = NULL;
    }

    if (maxreps || minreps) {
      if (minreps > 0)
	*flagp = HASWIDTH;
      if ((flags & SPFIXED) && maxreps) {
	regmatchmin = (origmatchmin * minreps);
	regmatchmax = (origmatchmax * maxreps);
	if (regmatchmax > 0x7FFF)
	  regmatchmax = 0x10000;
	*flagp |= SPFIXED;
      }
    } else {
      *flagp = (op != '+') ? WORST : HASWIDTH;
      if ((op == '*') || (op == '?'))
        regmatchmin = 0;
    }
    *flagp |= SPSTART;
    if ((op == '?') && (flags & SPFIXED)) {
      *flagp |= SPFIXED;
      regmatchmin = 0;
    }

    if (regparsestr[regparse+1] == '?') {
      greedy = 0;
      regparse++;
    } else
      greedy = 1;

    if (op == '*' && (flags&SIMPLE)) {
      if (!minreps && !maxreps)
	reginsert(greedy ? STAR : STAR2, ret);
      else
	reginsertwithopop(greedy ? STAR3 : STAR4, ret, minreps, maxreps);
    } else if (op == '*' && greedy) {
      /* Emit x* as (x&|), where & means "self".
	 If minreps or maxreps, also insert counter-managing
	 nodes. This counter detects empty matches, too.
	 The code is a little difficult to read because it often
	 uses reginsert, which puts nodes before existing nodes.
	 So, you almost have to read it backward. */
      rxpos br, nothing;
      if (minreps || maxreps) {
	/* Increment iteration counter, and fail if it's
	   already at the max: */
	rxpos x;
	x = reginsertwithopop(COUNTUNDER, ret, counter, maxreps);
	regtail(ret, x);
      }
      reginsert(BRANCH, ret);	/* Either x */
      if (minreps || maxreps) {
	/* Initialize the iteration counter on entry: */
	br = reginsertwithop(COUNTINIT, ret, counter);
	regtail(ret, br);
      } else
	br = ret;
      regoptail(br, regnode(BACK)); /* and loop */
      regoptail(br, br);	/* back */
      regtail(br, regnode(BRANCH)); /* or */
      nothing = regnode(NOTHING);
      if (minreps) {
	/* Fail to match if the counter isn't big enough, yet: */
	rxpos n;
	n = reginsertwithopop(COUNTOVER, nothing, counter, minreps);
	regtail(nothing, n);
      }
      if (minreps || maxreps) {
	/* We incremented the counter for an x match, but now
	   we're backtracking, so decrement it: */
	rxpos n;
	n = reginsertwithop(COUNTBACK, nothing, counter);
	regtail(nothing, n);
      }
      regtail(br, nothing); /* null. */
    } else if (op == '*') {
      /* Emit x*? as (|x&), where & means "self".
	 With a counter, we need (|(x|-)&), where - reverts
	 the iteration count and fails. */
      rxpos br, nothing, x, next_to_x;
      if (minreps || maxreps) {
	/* Increment iteration counter, and fail if it's
	   already at the max: */
	rxpos fail;
	x = reginsertwithopop(COUNTUNDER, ret, counter, maxreps);
	regtail(ret, x);

	fail = regnode(BRANCH);
	regnode(COUNTBACKFAIL);
	regarg(counter);
	reginsert(BRANCH, ret);
	fail += 3;
	regtail(ret, fail);
	x += 3;
      } else
	x = ret;
      reginsert(BRANCH, ret);  /* = next */
      next = ret;
      next_to_x = (x - next) + 3;
      reginsert(NOTHING, ret); /* = nothing */
      next += 3;
      nothing = ret;
      if (minreps) {
	/* Fail to match if the counter isn't big enough, yet: */
	nothing = reginsertwithopop(COUNTOVER, ret, counter, minreps);
	regtail(ret, nothing); /* chain countover -> nothing */
	next += (nothing - ret);
      }
      reginsert(BRANCH, ret); /* b3 */
      next += 3;
      nothing += 3;
      if (minreps || maxreps) {
	/* Initialize the iteration counter on entry: */
	br = reginsertwithop(COUNTINIT, ret, counter);
	regtail(ret, br); /* chain countinit to b3 */
	next += (br - ret);
	nothing += (br - ret);
      } else
	br = ret;
      regtail(br, next); /* chain b3 to next */
      x = next + next_to_x;
      regtail(x, regnode(BACK)); /* loop */
      regtail(x, br); /* back. */
      regtail(next, regnode(BACK)); /* chain next to nothing */
      regtail(next, nothing);
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
    if (ISMULT(regparsestr[regparse], parse_flags))
      FAIL("nested `*', `?', `+', or `{...}' in pattern");
  }

  if (flags & NEEDSAVECONST) {
    rxpos sv;
    sv = regnode(SAVECONST);
    regarg(origsavepos);
    regarg(origmatchmax);
    regtail(ret, sv);
    if (origmatchmax) {
      sv = reginsertwithop(MAYBECONST, ret, origsavepos);
      regtail(ret, sv);
    }
    *flagp &= ~SIMPLE;
  }

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
regatom(int *flagp, int parse_flags, int at_start)
{
  rxpos ret;
  int flags;

  *flagp = (WORST|SPFIXED);		/* Tentatively. */
  regmatchmin = regmatchmax = 1;
  regmaxlookback = 0;

  switch (regparsestr[regparse++]) {
  case '^':
    if (parse_flags & PARSE_SINGLE_LINE)
      ret = regnode(BOI);
    else
      ret = regnode(BOL);
    regmaxlookback = 1;
    regmatchmin = regmatchmax = 0;
    break;
  case '$':
    if (parse_flags & PARSE_SINGLE_LINE)
      ret = regnode(EOI);
    else
      ret = regnode(EOL);
    regmatchmin = regmatchmax = 0;
    break;
  case '.':
    --regparse;
    ret = regranges(parse_flags, at_start);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  case '[': 
    --regparse;
    ret = regranges(parse_flags, at_start);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  case '(':
    {
      if (regparsestr[regparse] == '?') {
	int moded = 0;

	while (1) {
	  if (regparsestr[regparse+1] == 'i') {
	    parse_flags &= ~PARSE_CASE_SENS;
	    regparse++;
	    moded = 1;
	  } else if (regparsestr[regparse+1] == 'm') {
	    parse_flags &= ~PARSE_SINGLE_LINE;
	    regparse++;
	    moded = 1;
	  } else if (regparsestr[regparse+1] == 's') {
	    parse_flags |= PARSE_SINGLE_LINE;
	    regparse++;
	    moded = 1;
	  } else if ((regparsestr[regparse+1] == '-') 
		     && (regparsestr[regparse+2] == 'i')) {
	    parse_flags |= PARSE_CASE_SENS;
	    regparse += 2;
	    moded = 1;
	  } else if ((regparsestr[regparse+1] == '-') 
		     && (regparsestr[regparse+2] == 'm')) {
	    parse_flags |= PARSE_SINGLE_LINE;
	    regparse += 2;
	    moded = 1;
	  } else if ((regparsestr[regparse+1] == '-') 
		     && (regparsestr[regparse+2] == 's')) {
	    parse_flags &= ~PARSE_SINGLE_LINE;
	    regparse += 2;
	    moded = 1;
	  } else {
	    break;
	  }
	}

	if (regparsestr[regparse+1] == ':') {
	  regparse += 2;
	  ret = reg(1, &flags, 0, 0, parse_flags);
	  *flagp = flags;
	} else if (moded) {
	  FAIL("expected `:' or another mode after `(?' and a mode sequence (where a mode is `i', `-i', `m', `-m', `s', or `-s')");
	} else if (regparsestr[regparse+1] == '(') {
	  /* Conditional */
	  if (((regparsestr[regparse+2] >= '0')
	       && (regparsestr[regparse+2] <= '9'))
	      || ((regparsestr[regparse+2] == '?')
		  && ((regparsestr[regparse+3] == '=')
		      || (regparsestr[regparse+3] == '!')
		      || (regparsestr[regparse+3] == '<')))) {
	    rxpos test, tbr, fbr, ender;
	    int flags, matchmin, matchmax;
	    Scheme_Hash_Table *backdepends;

	    regparse++;
	    ret = regnode(CONDITIONAL);
	    regarg(0); /* space for then */
	    regarg(0); /* space for else */
	    if (regparsestr[regparse+1] != '?') {
	      int posn;
	      regparse++;
	      posn = regdigit();
	      test = regnode(BACKREF);
	      regarg(posn);
	      if (regparsestr[regparse] == ')') {
		regparse++;
	      } else {
		FAIL("expected `)' after `(?(' followed by a digit");
	      }
	    } else {
	      test = regatom(&flags, parse_flags, 1);
	    }
	    if (test != OPERAND3(ret)) {
	      FAIL("test went to wrong place!?");
	    }
	    regtail(test, regnode(END));
	    if (regparsestr[regparse] == ')') {
	      FAIL("expected an expression after test in `(?(...))'");
	    }

	    regbackdepends = NULL;
	    *flagp |= HASWIDTH; /* tentatively */

	    tbr = regbranch(&flags, parse_flags, 1);

	    if (!(flags&HASWIDTH)) {
	      *flagp &= ~HASWIDTH;
	      backdepends = NULL;
	    } else if (regbackdepends) {
	      backdepends = regbackdepends;
	      regbackdepends = NULL;
	    } else
	      backdepends = NULL;
	      
	    if (!(flags & SPFIXED))
	      *flagp &= ~SPFIXED;
	    matchmin = regmatchmin;
	    matchmax = regmatchmax;

	    if (regparsestr[regparse] == ')') {
	      fbr = regnode(NOTHING);
	      *flagp &= ~HASWIDTH;
	      matchmin = 0;
	    } else if (regparsestr[regparse] != '|') {
	      FAIL("expected `)' or `|' after first branch of `(?(...)...)'");
	    } else {
	      regparse++;
	      fbr = regbranch(&flags, parse_flags, 1);
	      if (regparsestr[regparse] != ')') {
		FAIL("expected `)' to close `(?(...)...' after second branch");
	      }

	      if (!(flags&HASWIDTH)) {
		*flagp &= ~HASWIDTH;
		backdepends = NULL;
	      } else if (regbackdepends) {
		if (backdepends)
		  merge_tables(backdepends, regbackdepends);
		else
		  backdepends = regbackdepends;
	      }
	      
	      if (!(flags & SPFIXED))
		*flagp &= ~SPFIXED;
	      else {
		if (regmatchmin < matchmin)
		  matchmin = regmatchmin;
		if (regmatchmax > matchmax)
		  matchmax = regmatchmax;
	      }
	    }

	    regmatchmax = matchmax;
	    regmatchmin = matchmin;
	    regbackdepends = backdepends;

	    if (OPERAND2(ret) + 1 < regcodesize) {
	      int delta;
	      delta = tbr - ret;
	      regstr[OPERAND(ret)] = delta >> 8;
	      regstr[OPERAND(ret)+1] = delta & 255;
	      delta = fbr - ret;
	      regstr[OPERAND2(ret)] = delta >> 8;
	      regstr[OPERAND2(ret)+1] = delta & 255;
	    }
	    ender = regnode(NOTHING);
	    regtail(tbr, ender);
	    regtail(fbr, ender);
	    regtail(ret, ender);
	    regparse++;
	  } else
	    FAIL("expected `(?=', `(?!', `(?<', or digit after `(?('");
	} else if (regparsestr[regparse+1] == '>') {
	  regparse += 2;
	  ret = reg(1, &flags, 0, LOOKTX, parse_flags);
	  *flagp = flags;
	} else {
	  if (regparsestr[regparse+1] == '=') {
	    regparse += 2;
	    ret = reg(1, &flags, 0, LOOKT, parse_flags);
	  } else if (regparsestr[regparse+1] == '!') {
	    regparse += 2;
	    ret = reg(1, &flags, 0, LOOKF, parse_flags);
	  } else if ((regparsestr[regparse+1] == '<')
		     && (regparsestr[regparse+2] == '=')) {
	    regparse += 3;
	    ret = reg(1, &flags, 0, LOOKBT, parse_flags);
	  } else if ((regparsestr[regparse+1] == '<')
		     && (regparsestr[regparse+2] == '!')) {
	    regparse += 3;
	    ret = reg(1, &flags, 0, LOOKBF, parse_flags);
	  } else {
	    FAIL("expected `:', `=', `!', `<=', `<!', `i', `-i', `m', `-m', `s', or `-s' after `(?'");
	  }
	  regmatchmin = regmatchmax = 0;
	  *flagp = SPFIXED;
	  regbackdepends = NULL;
	}
      } else {
	ret = reg(1, &flags, 1, 0, parse_flags);
	if (flags & NEEDSAVECONST) {
	  *flagp = flags;
	} else {
	  *flagp |= flags&(HASWIDTH|SPSTART);
	  if (!(flags&SPFIXED))
	    *flagp &= ~SPFIXED;
	}
      }
      /* otherwise, regmatchmin/regmatchmax is set */
      if (ret == 0)
	FAIL("cluster failed!?");
    }
    break;
  case '|':
  case ')':
    FAIL("internal urp");	/* Supposed to be caught earlier. */
    break;
  case '?':
    FAIL("`?' follows nothing in pattern");
    break;
  case '+':
    FAIL("`+' follows nothing in pattern");
    break;
  case '*':
    FAIL("`*' follows nothing in pattern");
    break;
  case '\\':
    {
      int c;
      if (regparse == regparse_end)
	FAIL("trailing backslash in pattern");
      c = regparsestr[regparse++];
      if ((parse_flags & PARSE_PCRE) && (c == 'b')) {
	ret = regnode(WORDBOUND);
	regmatchmin = regmatchmax = 0;
        regmaxlookback = 1;
      } else if ((parse_flags & PARSE_PCRE) && (c == 'B')) {
	ret = regnode(NOTWORDBOUND);
	regmatchmin = regmatchmax = 0;
        regmaxlookback = 1;
      } else if ((parse_flags & PARSE_PCRE) && (c == 'p')) {
	ret = regunicode(0);
	regmatchmax = MAX_UTF8_CHAR_BYTES;
	*flagp |= HASWIDTH;
      } else if ((parse_flags & PARSE_PCRE) && (c == 'P')) {
	ret = regunicode(1);
	regmatchmax = MAX_UTF8_CHAR_BYTES;
	*flagp |= HASWIDTH;
      } else if ((parse_flags & PARSE_PCRE) && (c >= '0') && (c <= '9')) {
	int posn;
	--regparse;
	posn = regdigit();
	if (parse_flags & PARSE_CASE_SENS)
	  ret = regnode(BACKREF);
	else
	  ret = regnode(BACKREF_CI);
	regarg(posn);
	*flagp &= ~SPFIXED;
	/* Set HASWIDTH flag: */
	{
	  Scheme_Object *f;
	  if (regbackknown)
	    f = scheme_hash_get(regbackknown, scheme_make_integer(posn));
	  else
	    f = NULL;
	  if (f) {
	    if (SCHEME_TRUEP(f))
	      *flagp |= HASWIDTH;
	  } else {
	    *flagp |= HASWIDTH;
	    if (!regbackdepends)
	      regbackdepends = scheme_make_hash_table(SCHEME_hash_ptr);
	    scheme_hash_set(regbackdepends, scheme_make_integer(posn), scheme_true);
	  }
	}
      } else {
	regparse -= 2;
	ret = regranges(parse_flags, at_start);
	*flagp |= HASWIDTH|SIMPLE;
      }
    }
    break;
  default:
    {
      int len, ilen, c;
      char ender;

      regparse--;

      if (parse_flags & PARSE_PCRE) {
	if (regparsestr[regparse] == '{')
	  FAIL("`{' follows nothing in pattern");
	if (regparsestr[regparse] == '}')
	  FAIL("unmatched `}' in pattern");
	if (regparsestr[regparse] == ']')
	  FAIL("unmatched `]' in pattern");
      }

      for (len = ilen = 0; regparse + ilen < regparse_end; len++, ilen++) {
	if (regparsestr[regparse + ilen] == '\\') {
	  if (regparse + ilen + 1 >= regparse_end)
	    break;
	  c = regparsestr[regparse + ilen + 1];
	  if (((c >= 'a') && (c <= 'z'))
	      || ((c >= 'A') && (c <= 'Z'))
	      || ((c >= '0') && (c <= '9')))
	    break;
	  ilen++;
	} else if (regstrcspn(regparsestr + regparse + ilen, regparsestr + regparse + ilen + 1,
			      (parse_flags & PARSE_PCRE) ? PCRE_META : META) < 1)
	  break;
      }
      if (len <= 0)
	FAIL("internal disaster");

      if ((len == 1) && at_start) {
	/* Maybe convert "x|y" to "[xy]", etc.: */
	ret = regranges(parse_flags, at_start);
	*flagp |= HASWIDTH|SIMPLE;
      } else {
	if (!(parse_flags & PARSE_CASE_SENS)) {
	  /* Need case insensitivity? */
	  int i;
	  for (i = 0; i < ilen; i++) {
	    c = regparsestr[regparse + i];
	    if ((rx_toupper(c) != c)
		|| (rx_tolower(c) != c)) {
	      break;
	    }
	  }
	  if (i >= ilen)
	    parse_flags |= PARSE_CASE_SENS;
	}

	ender = regparsestr[regparse+ilen];
	if (len > 1 && ISMULT(ender, parse_flags)) {
	  /* Back off from ?+* operand. */
	  len--;
	  ilen--;
	  if (regparsestr[regparse + ilen] == '\\')
	    --ilen;
	}
	*flagp |= HASWIDTH;
	if (len == 1)
	  *flagp |= SIMPLE;
	regmatchmin = regmatchmax = len;
	ret = regnode((parse_flags & PARSE_CASE_SENS) ? EXACTLY : EXACTLY_CI);
	regarg(len);
	while (len > 0) {
	  c = regparsestr[regparse++];
	  if (c == '\\')
	    c = regparsestr[regparse++];
	  if (!(parse_flags & PARSE_CASE_SENS))
	    c = rx_tolower(c);
	  regc(c);
	  len--;
	}
      }
    }
    break;
  }

  if (!ret)
    FAIL("failed!?");
	
  return ret;
}

static int regcharclass(int c, char *map)
{
  switch(c) {
  case 'd':
    for (c = 0; c < 10; c++) {
      map['0' + c] = 1;
    }
    break;
  case 'D':
    for (c = 0; c < '0'; c++) {
      map[c] = 1;
    }
    for (c = '9' + 1; c < 256; c++) {
      map[c] = 1;
    }
    break;
  case 'w':
    for (c = 0; c < 26; c++) {
      map['a' + c] = 1;
      map['A' + c] = 1;
    }
    for (c = 0; c < 10; c++) {
      map['0' + c] = 1;
    }
    map['_'] = 1;
    break;
  case 'W':
    for (c = 0; c < '0'; c++) {
      map[c] = 1;
    }
    for (c = '9' + 1; c < 'A'; c++) {
      map[c] = 1;
    }
    for (c = 'Z' + 1; c < '_'; c++) {
      map[c] = 1;
    }
    for (c = 'z' + 1; c < 256; c++) {
      map[c] = 1;
    }
    break;
  case 's':
    map['\t'] = 1;
    map['\n'] = 1;
    map['\f'] = 1;
    map['\r'] = 1;
    map[' '] = 1;
    break;
  case 'S':
    for (c = 0; c < 256; c++) {
      switch (c) {
      case '\t':
      case '\n':
      case '\f':
      case '\r':
      case ' ':
	break;
      default:
	map[c] = 1;
	break;
      }
    }
    break;
  default:
    if (((c >= 'a') && (c <= 'z'))
	|| ((c >= 'A') && (c <= 'Z'))) {
      FAIL("illegal alphabetic escape");
    }
    map[c] = 1;
    break;
  }

  return 1;
}

static int is_posix_char_class(char *str, int pos, int len, char *map)
{
  int c;

  if (pos + 8 <= len) {
    if (!scheme_strncmp(":alnum:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        regcharclass('d', map);
        for (c = 'a'; c <= 'z'; c++) {
          map[c] = 1;
          map[c - ('a' - 'A')] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":alpha:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 'a'; c <= 'z'; c++) {
          map[c] = 1;
          map[c - ('a' - 'A')] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":ascii:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 0; c <= 127; c++) {
          map[c] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":blank:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        map[' '] = 1;
        map['\t'] = 1;
      }
      return 1;
    } else if (!scheme_strncmp(":cntrl:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 0; c <= 31; c++) {
          map[c] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":digit:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        regcharclass('d', map);
      }
      return 1;
    } else if (!scheme_strncmp(":graph:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 0; c <= 127; c++) {
          if (scheme_isgraphic(c))
            map[c] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":lower:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 'a'; c <= 'z'; c++) {
          map[c] = 1;
        }
      }
      return 1;
    } else if (!scheme_strncmp(":print:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 0; c <= 127; c++) {
          if (scheme_isgraphic(c))
            map[c] = 1;
        }
        map[' '] = 1;
        map['\t'] = 1;
      }
      return 1;
    } else if (!scheme_strncmp(":space:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        regcharclass('s', map);
      }
      return 1;
    } else if (!scheme_strncmp(":upper:]", str XFORM_OK_PLUS pos, 8)) {
      if (map) {
        for (c = 'A'; c <= 'Z'; c++) {
          map[c] = 1;
        }
      }
      return 1;
    }
  }
  
  if ((pos + 7 <= len) 
      && !scheme_strncmp(":word:]", str XFORM_OK_PLUS pos, 7)) {
    if (map) {
      regcharclass('w', map);
    }
    return 1;
  } 

  if ((pos + 9 <= len)
      && !scheme_strncmp(":xdigit:]", str XFORM_OK_PLUS pos, 9)) {
    if (map) {
      regcharclass('d', map);
      for (c = 'a'; c <= 'f'; c++) {
        map[c] = 1;
        map[c - ('a' - 'A')] = 1;
      }
    }
    return 1;
  }

  return 0;
}

static int is_posix_char_class_in_unicode(mzchar *str, int pos, int len, char *map)
{
  int ulen;
  int i;
  char buf[10];

  if (pos + 7 > len)
    return 0;

  ulen = len - pos;
  if (ulen > 9)
    ulen = 9;

  for (i = 0; i < ulen; i++) {
    if (str[pos + i] > 127)
      return 0;
    buf[i] = (char)str[pos + i];
  }

  return is_posix_char_class(buf, 0, ulen, map);
}

static char *regrange(int parse_flags, char *map)
/* [ is already consumed; result is an array of 256 bytes of included chars */
{
  int xclass, c;
  int classend, can_range = 0;
  int exclude = 0;

  if (regparsestr[regparse] == '^') { /* Complement of range. */
    exclude = 1;
    regparse++;
  }

  if (regparsestr[regparse] == ']' || regparsestr[regparse] == '-') {
    c = regparsestr[regparse];
    map[c] = 1;
    regparse++;
  }
  while (regparse != regparse_end && regparsestr[regparse] != ']') {
    if (regparsestr[regparse] == '-') {
      regparse++;
      if (regparsestr[regparse] == ']' || regparse == regparse_end) {
	map['-'] = 1;
      } else {
	if (!can_range) {
	  FAIL("misplaced hypen within square brackets in pattern");
	} else {
	  xclass = UCHAR(regparsestr[regparse-2])+1;
	  classend = UCHAR(regparsestr[regparse]);
	  if (classend == '-') {
	    FAIL("misplaced hypen within square brackets in pattern");
	  }
	  if ((classend == '\\') && (parse_flags & PARSE_PCRE)) {
	    if (regparse+1 == regparse_end) {
	      FAIL("escaping backslash at end pattern (within square brackets)");
	    }
	    regparse++;
	    classend = UCHAR(regparsestr[regparse]);
	    if (((classend >= 'a') && (classend <= 'z'))
		|| ((classend >= 'A') && (classend <= 'Z'))) {
	      FAIL("misplaced hypen within square brackets in pattern");
	    }
	  }
	  if (xclass > classend+1)
	    FAIL("invalid range within square brackets in pattern");
	  for (; xclass <= classend; xclass++) {
	    c = xclass;
	    map[c] = 1;
	    if (!(parse_flags & PARSE_CASE_SENS)) {
	      c = rx_toupper(c);
	      map[c] = 1;
	      c = rx_tolower(c);
	      map[c] = 1;
	    }
	  }
	  regparse++;
	}
      }
      can_range = 0;
    } else if ((regparsestr[regparse] == '\\') && (parse_flags & PARSE_PCRE)) {
      c = UCHAR(regparsestr[regparse + 1]);
      if (((c >= 'a') && (c <= 'z'))
	  || ((c >= 'A') && (c <= 'Z'))) {
	regcharclass(c, map);
	can_range = 0;
      } else {
	map[c] = 1;
	can_range = 1;
      }
      regparse += 2;
    } else if ((regparsestr[regparse] == '[') 
	       && (parse_flags & PARSE_PCRE)
	       && (regparsestr[regparse+1] == ':')
	       && is_posix_char_class(regparsestr, regparse + 1, regparse_end, map)) {
      regparse += 2;
      while (regparsestr[regparse] != ']') {
	regparse++;
      }
      regparse++;
      can_range = 0;
    } else {
      c = UCHAR(regparsestr[regparse++]);
      map[c] = 1;
      if (!(parse_flags & PARSE_CASE_SENS)) {
	c = rx_tolower(c); 
	map[c] = 1;
	c = rx_toupper(c); 
	map[c] = 1;
      }
      can_range = 1;
    }
  }

  if (exclude) {
    for (c = 0; c < 256; c++) {
      map[c] = !map[c];
    }
  }

  if (regparsestr[regparse] != ']')
    FAIL("missing closing square bracket in pattern");
  regparse++;

  return map;
}

static rxpos
regranges(int parse_flags, int at_start)
{
  int c;
  rxpos ret, save_regparse = 0;
  int count, off_ranges, on_ranges, now_on, last_on, prev_last_on;
#ifdef COUNT_CI_CHARS
  /* These could be used to pick an encoding as a _CI variant, but
     _CI variants are not picked currently: */
  int all_ci, num_ci;
#endif
  char *new_map = NULL, *accum_map = NULL;

  count = 0;
  while (1) {
    /* This loop can end up parsing a range and not using the result,
       so that the range is parsed twice. That's ok, because there's
       no nesting (and therefore no exponential explosion). */

    if (!new_map)
      new_map = (char *)scheme_malloc_atomic(256);
    memset(new_map, 0, 256);

    if (regparsestr[regparse] == '\\'
	&& (regparse + 1 < regparse_end)) {
      /* \<char> */
      c = UCHAR(regparsestr[++regparse]);
      if (parse_flags & PARSE_PCRE) {
	if ((c >= '0') && (c <= '9'))
	  break;
	if (((c >= 'a') && (c <= 'z'))
	    || ((c >= 'A') && (c <= 'Z'))) {
          if ((c == 'p') || (c == 'P')) {
            /* unicode char class; give up */
            break;
          }
	  regcharclass(regparsestr[regparse], new_map);
          
	} else
	  new_map[c] = 1;
      } else
	new_map[c] = 1;
      regparse++;
    } else if (regstrcspn(regparsestr + regparse, regparsestr + regparse + 1,
			  (parse_flags & PARSE_PCRE) ? PCRE_META : META)) {
      /* <char> */
      c = UCHAR(regparsestr[regparse]);
      new_map[c] = 1;
      if (!(parse_flags & PARSE_CASE_SENS))  {
	c = rx_tolower(c);
	new_map[c] = 1;
	c = rx_toupper(c);
	new_map[c] = 1;
      }
      regparse++;
    } else if (regparsestr[regparse] == '.') {
      /* . */
      for (c = 0; c < 256; c++) {
	new_map[c] = 1;
      }
      if (!(parse_flags & PARSE_SINGLE_LINE))
	new_map['\n'] = 0;
      regparse++;
    } else if (regparsestr[regparse] == '[') {
      /* [...] */
      regparse++;
      regrange(parse_flags, new_map);
    } else
      break;

    /* If the most recently parsed range is not 
       continued by a branch or the end of a sub-sequence,
       then abandon it, because it actually belongs
       with a new sequence. */
    if (accum_map
	&& (regparse < regparse_end)
	&& (regparsestr[regparse] != '|')
	&& (regparsestr[regparse] != ')'))
      break;
    
    /* We'll keep it. Merge char maps so far: */
    if (accum_map) {
      for (c = 0; c < 256; c++) {
	accum_map[c] |= new_map[c];
      }
    } else {
      accum_map = new_map;
      new_map = NULL;
    }
    save_regparse = regparse;
    
    /* If we're at the end, or if we can only do one, then we're done. */
    if (!at_start
	|| (regparsestr[regparse] != '|')
	|| (regparse >= regparse_end)
	|| (regparsestr[regparse] == ')'))
      break;

    regparse++;
    if (regparse == regparse_end)
      break;
  }

  regparse = save_regparse;

  if (!accum_map)
    FAIL("should have found one range!");

  while (1) {
    /* Collect stats to pick the best run-time implementation for a range.
       We may do this twice if we decide to use a _CI variant. */
    count = 0;
#ifdef COUNT_CI_CHARS
    num_ci = 0;
    all_ci = 1;
#endif
    on_ranges = 0;
    off_ranges = 0;
    now_on = 0;
    last_on = -1;
    prev_last_on = -1;
    for (c = 0; c < 256; c++) {
      if (accum_map[c]) {
	if (now_on < 0)
	  off_ranges++;
	now_on = 1;
	count++;
	prev_last_on = last_on;
	last_on = c;

#ifdef COUNT_CI_CHARS
	if (c != rx_tolower(c)) {
	  if (accum_map[rx_tolower(c)] != accum_map[c])
	    all_ci = 0;
	  num_ci++;
	} else if (c != rx_toupper(c)) {
	  if (accum_map[rx_toupper(c)] != accum_map[c])
	    all_ci = 0;
	  num_ci++;
	}
#endif
      } else {
	if (now_on > 0)
	  on_ranges++;
	now_on = -1;
      }
    }
    if (now_on > 0)
      on_ranges++;
    else
      off_ranges++;

    /* Pick the best run-time implementation for a range. */
    if (count == 256) {
      return regnode(ANY);
    } else if ((count == 255) && !accum_map['\n']) {
      return regnode(ANYL);
    } else if (count == 1) {
      ret = regnode(EXACTLY1);
      regc(last_on);
      return ret;
    } else if (count == 2) {
      ret = regnode(EXACTLY2);
      regc(last_on);
      regc(prev_last_on);
      return ret;
    } else if ((on_ranges == 1)
	       ||  (off_ranges == 1)) {
      int rs = 255, re = 255, on;

      if (on_ranges == 1)
	on = 1;
      else
	on = 0;

      for (c = 0; c < 256; c++) {
	if (!!accum_map[c] == on) {
	  rs = c;
	  break;
	}
      }
      for (c++; c < 256; c++) {
	if (!accum_map[c] == on) {
	  re = c - 1;
	  break;
	}
      }

      if (on)
	ret = regnode(RANGE);
      else
	ret = regnode(NOTRANGE);
      regc(rs);
      regc(re);
      return ret;
    } else {
      rxpos a;

      ret = regnode(ANYOF);
      a = regcode;
      for (c = 0; c < 32; c++) {
	regc(0);
      }

      if (regcode <= regcodesize) {
	for (c = 0; c < 256; c++) {
	  if (accum_map[c]) {
	    regstr[a + (c >> 3)] |= (1 << (c & 0x7));
	  }
	}
      }

      return ret;
    }
  }
}

READ_ONLY static const char *prop_names[] = { "Cn",
			      "Cc",
			      "Cf",
			      "Cs",
			      "Co",
			      "Ll",
			      "Lu",
			      "Lt",
			      "Lm",
			      "Lo",
			      "Nd",
			      "Nl",
			      "No",
			      "Ps",
			      "Pe",
			      "Pi",
			      "Pf",
			      "Pc",
			      "Pd",
			      "Po",
			      "Mn",
			      "Mc",
			      "Me",
			      "Sc",
			      "Sk",
			      "Sm",
			      "So",
			      "Zl",
			      "Zp",
			      "Zs",
			      NULL};

static rxpos
regunicode(int negate)
{
  rxpos ret;
  int len, bottom, top, i;

  if (regparsestr[regparse] != '{') {
    FAIL("expected { after \\p or \\P");
  }
  regparse++;
  if (regparsestr[regparse] == '^') {
    negate = !negate;
    regparse++;
  }
  
  len = 0;
  while ((regparsestr[regparse + len] != '}')
	 && (regparse + len < regparse_end)) {
    len++;
  }

  if (regparse + len >= regparse_end) {
    FAIL("missing } to close \\p{ or \\P{");
  }

  bottom = top = -1;
  if (len == 2) {
    for (i = 0; prop_names[i]; i++) {
      if ((regparsestr[regparse] == prop_names[i][0])
	  && (regparsestr[regparse+1] == prop_names[i][1])) {
	bottom = top = i;
	break;
      }
    }
    if (bottom == -1) {
      if ((regparsestr[regparse] == 'L')
	  && (regparsestr[regparse+1] == '&')) {
	bottom = mzu_Ll;
	top = mzu_Lm;
      }
    }
  } else if (len == 1) {
    if (regparsestr[regparse] == '.') {
      bottom = 0;
      top = mzu_LAST;
    } else {
      for (i = 0; prop_names[i]; i++) {
	if (regparsestr[regparse] == prop_names[i][0]) {
	  bottom = i;
	  while (prop_names[i+1]) {
	    if (regparsestr[regparse] != prop_names[i+1][0])
	      break;
	    i++;
	  }
	  top = i;
	  break;
	}
      }
    }
  }

  if (bottom < 0) {
    FAIL("unrecognized property name in \\p{} or \\P{}");
  }

  regparse += len + 1;

  ret = regnode(UNIPROP);
  /* This encoding accommodates up to 63 categories: */
  regarg((negate << 13) | (bottom << 6) | top);

  return ret;
}

static int regdigit()
{
  int posn, c;
  c = regparsestr[regparse++];
  posn = c - '0';
  while (regparse < regparse_end) {
    c = regparsestr[regparse];
    if ((c >= '0') && (c <= '9')) {
      posn = (posn * 10) + (c - '0');
      if (posn > 0x7FFF)
	FAIL("backreference number is too large");
      regparse++;
    } else
      break;
  }
  if (posn > regmaxbackposn)
    regmaxbackposn = posn;
  return posn;
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
  if (regcode + 3 >= regcodesize) {
    regcode += 3;
    if (regcode > regcodemax)
      regcodemax = regcode;
    return ret;
  }

  ptr = ret;
  regstr[ptr++] = op;
  regstr[ptr++] = '\0';		/* Null "next" pointer. */
  regstr[ptr++] = '\0';
  regcode = ptr;

  if (regcode > regcodemax)
    regcodemax = regcode;
    
  return ret;
}

/*
   - regc - emit (if appropriate) a byte of code
   */
static void
regc(char b)
{
  if (regcode + 1 < regcodesize)
    regstr[regcode] = b;
  regcode++;
  if (regcode > regcodemax)
    regcodemax = regcode;
}

static void
regarg(int v)
{
  regc(v >> 8);
  regc(v & 255);
}

/*
   - reginsert - insert an operator in front of already-emitted operand
   *
   * Means relocating the operand.
   */
static void
regshift(int amt, rxpos opnd)
{
  if (regcode + amt < regcodesize) {
    memmove(regstr XFORM_OK_PLUS opnd + amt, 
	    regstr XFORM_OK_PLUS opnd, 
	    regcode - opnd);
  }
  regcode += amt;
  if (regcode > regcodemax)
    regcodemax = regcode;
}

static void
reginsert(char op, rxpos opnd)
{
  regshift(3, opnd);

  if (opnd + 3 >= regcodesize) {
    return;
  }

  regstr[opnd++] = op;
  regstr[opnd++] = '\0'; /* tail */
  regstr[opnd++] = '\0';
}

static rxpos
reginsertwithop(char op, rxpos opnd, int arg)
{
  regshift(5, opnd);

  if (opnd + 5 >= regcodesize) {
    return opnd + 5;
  }

  regstr[opnd++] = op;
  regstr[opnd++] = '\0'; /* tail */
  regstr[opnd++] = '\0';
  regstr[opnd++] = (arg >> 8);
  regstr[opnd++] = (arg & 255);

  return opnd;
}

static rxpos
reginsertwithopop(char op, rxpos opnd, int arg, int arg2)
{
  regshift(7, opnd);

  if (opnd + 7 >= regcodesize) {
    return opnd + 7;
  }

  regstr[opnd++] = op;
  regstr[opnd++] = '\0'; /* tail */
  regstr[opnd++] = '\0';
  regstr[opnd++] = (arg >> 8);
  regstr[opnd++] = (arg & 255);
  regstr[opnd++] = (arg2 >> 8);
  regstr[opnd++] = (arg2 & 255);

  return opnd;
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

  /* Find last node. */
  scan = p;
  for (;;) {
    if (scan + 2 >= regcodesize) {
      return;
    }
    temp = regnext(scan);
    if (temp == 0)
      break;
    scan = temp;
  }

  if (scan + 2 >= regcodesize) {
    return;
  }
  
  if (rOP(scan) == BACK)
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
  if (p == 0 || (p >= regcodesize) || rOP(p) != BRANCH) {
    return;
  }
  regtail(OPERAND(p), val);
}

static int merge_tables(Scheme_Hash_Table *dest, Scheme_Hash_Table *src)
{
  int i;

  for (i = src->size; i--; ) {
    if (src->vals[i]) {
      scheme_hash_set(dest, src->keys[i], src->vals[i]);
    }
  }

  return 1;
}

static int check_and_propagate_depends(void)
{
  int i, j;
  Scheme_Hash_Table *backdepends = regbackdepends, *ht, *next_ht = NULL;
  Scheme_Object *v;
  
  while (backdepends) {
    for (i = backdepends->size; i--; ) {
      if (backdepends->vals[i]) {
	if (regbackknown)
	  v = scheme_hash_get(regbackknown, backdepends->keys[i]);
	else
	  v = NULL;
	if (v) {
	  /* Check assumption: */
	  if (SCHEME_FALSEP(v)) {
	    FAIL("*, +, or {...,} operand could be empty (via empty backreference)");
	  }
	  if (SCHEME_HASHTP(v)) {
	    /* Check/propagate assumption. The fixpoint direction is 
	       determined by assuming "true" while recursively checking. */
	    scheme_hash_set(regbackknown, backdepends->keys[i], scheme_true);
	    if (!next_ht)
	      next_ht = scheme_make_hash_table(SCHEME_hash_ptr);
	    ht = (Scheme_Hash_Table *)v;
	    for (j = ht->size; j--; ) {
	      if (ht->vals[j]) {
		scheme_hash_set(next_ht, ht->keys[j], ht->vals[j]);
	      }
	    }
	  }
	} else {
	  /* Add assumption */
	  if (!regbackknown)
	    regbackknown = scheme_make_hash_table(SCHEME_hash_ptr);
	  scheme_hash_set(regbackknown, backdepends->keys[i], scheme_true);
	}
      }
    }
    backdepends = next_ht;
    next_ht = NULL;
  }

  return 1;
}

static MZ_INLINE rxpos l_strchr(char *str, rxpos a, int l, int c)
{
  int i;

  for (i = 0; i < l; i++) {
    if (str[a + i] == c)
      return a + i;
  }

  return -1;
}

static MZ_INLINE rxpos l_strchr_ci(char *str, rxpos a, int l, int c)
{
  int i, ch;

  for (i = 0; i < l; i++) {
    ch = str[a + i];
    ch = rx_tolower(ch);
    if (ch == c)
      return a + i;
  }

  return -1;
}

#if 0
static MZ_INLINE int in_ranges(char *str, rxpos a, int l, int c)
{
  int i;

  l *= 2;

  for (i = 0; i < l; i += 2) {
    if ((UCHAR(str[a + i]) <= c) && (UCHAR(str[a + i + 1]) >= c))
      return 1;
  }

  return 0;
}

static MZ_INLINE int in_ranges_ci(char *str, rxpos a, int l, int c)
{
  int i;

  l *= 2;

  c = rx_tolower(c);

  for (i = 0; i < l; i += 2) {
    if ((UCHAR(str[a + i]) <= c) && (UCHAR(str[a + i + 1]) >= c))
      return 1;
  }

  return 0;
}
#endif

/*
 * regexec and friends
 */

/*
 * Forwards.
 */
static int regtry(regexp *, char *, int, int, rx_lazy_str_t *, rxpos *, rxpos *, rxpos *, rxpos *, int *, Regwork *rw, rxpos, 
                  char *, rxpos, rxpos, int);
static int regtry_port(regexp *, Scheme_Object *, Scheme_Object *, int nonblock,
		       rxpos *, rxpos *, rxpos *, rxpos *, int *,
		       char **, rxpos *, rxpos *, rxpos, Scheme_Object*, Scheme_Object*, rxpos, 
                       char*, rxpos, rxpos,
		       int, int *);
static int regmatch(Regwork *rw, rxpos);
static int regrepeat(Regwork *rw, rxpos, int);

static void stack_room(Regwork *rw, int amt)
{
  if (rw->rewind_stack_count + amt > rw->rewind_stack_size) {
    int sz;
    rxpos *p;
    sz = rw->rewind_stack_size * 2;
    if (!sz) sz = MATCH_STACK_SIZE;
    p = (rxpos *)scheme_malloc_atomic(sizeof(rxpos)*sz);
    memcpy(p, rw->rewind_stack, rw->rewind_stack_size * sizeof(rxpos));
    rw->rewind_stack = p;
    rw->rewind_stack_size = sz;
  }
}

static int match_push(Regwork *rw)
{
  if (rw->non_tail >= 0) {
    int pos;

    rw->non_tail++;
    pos = rw->rewind_stack_count;
    rw->rewind_stack_prompt = pos;

    return pos;
  } else
    return 0;
}

static void match_pop(Regwork *rw, int pos, int matched)
{
  if (rw->non_tail >= 0) {
    --rw->non_tail;

    if (matched) {
      /* Save elements on stack in case an enclosing match
         needs to rewind. Area between prompt and pos are
         mapping that don't need to be re-recorded. */
      rw->rewind_stack_prompt = pos;
    } else {
      int i, no;
      for (i = rw->rewind_stack_count; i > pos; i -= 3) {
        no = rw->rewind_stack[i-3];
        if (no < 0) {
          rw->maybep[-no] = rw->rewind_stack[i-2];
        } else {
          rw->startp[no] = rw->rewind_stack[i-2];
          rw->endp[no] = rw->rewind_stack[i-1];
        }
      }
      rw->rewind_stack_count = pos;
      rw->rewind_stack_prompt = pos;
    }
  }
}

static void match_set(Regwork *rw, int no, rxpos start, rxpos end)
{
  int i, count;

  if (rw->non_tail > 0) {
    count = rw->rewind_stack_count;
    for (i = rw->rewind_stack_prompt; i < count; i += 3) {
      if (rw->rewind_stack[i] == no)
        break;
    }
    
    if (i >= count) {
      stack_room(rw, 3);
      i = count;
      rw->rewind_stack[i++] = no;
      rw->rewind_stack[i++] = rw->startp[no];
      rw->rewind_stack[i++] = rw->endp[no];
      rw->rewind_stack_count = i;
    }
  }

  rw->startp[no] = start;
  rw->endp[no] = end;
}

static void match_maybe(Regwork *rw, int no, rxpos pos)
{
  int i, count;

  if (rw->non_tail > 0) {
    count = rw->rewind_stack_count;
    for (i = rw->rewind_stack_prompt; i < count; i += 3) {
      if (rw->rewind_stack[i] == (- no))
        break;
    }
    
    if (i >= count) {
      stack_room(rw, 3);
      i = count;
      rw->rewind_stack[i++] = (- no);
      rw->rewind_stack[i++] = rw->maybep[no];
      rw->rewind_stack[i++] = 0;
      rw->rewind_stack_count = i;
    }
  }

  rw->maybep[no] = pos;
}

#ifdef DEBUG
int regnarrate = 0;
void regdump();
static char *regprop();
#endif

#define REGPORT_FLUSH_THRESHOLD 256

/*
   - regexec - match a regexp against a string
   */
static int
regexec(const char *who,
	regexp *prog, char *string, 
	/* Used only for (bytes) strings: */
	int stringpos, int stringlen, int stringorigin,
        /* For lazy strings: */
        rx_lazy_str_t *lazy_string,
	/* Always used: */
	rxpos *startp, rxpos *maybep, rxpos *endp, rxpos *match_stack,
        /* For port mode: */
	Scheme_Object *port, Scheme_Object *unless_evt, int nonblock,
	/* Used only when port is non-NULL: */
	char **stringp, int peek, int get_offsets, intptr_t save_prior,
	Scheme_Object *discard_oport, 
	Scheme_Object *portstart, Scheme_Object *portend, Scheme_Object **_dropped,
        char *prefix, rxpos prefix_len, rxpos prefix_offset)
{
  int spos;
  int *counters;
  Scheme_Object *dropped = NULL, *peekskip = NULL; /* used for ports, only */
 
  /* Check validity of program. */
  if (UCHAR(prog->program[0]) != MAGIC) {
    regerror("corrupted program");
    return(0);
  }

  /* If there is a "must appear" string, look for it. */
  if (!port && !lazy_string && (prog->regmust >= 0)) {
    spos = stringpos;
    while (1) {
      int i, l = prog->regmlen, ch, pos;
      GC_CAN_IGNORE char *p;

      if ((spos - stringpos) + l <= stringlen) {
	if (prog->flags & REGEXP_MUST_CI)
	  pos = l_strchr_ci(string, spos, stringlen - (spos - stringpos) - (l - 1), 
			    (ITO(prog->program, (char *)prog) XFORM_OK_PLUS prog->regmust)[0]);
	else
	  pos = l_strchr(string, spos, stringlen - (spos - stringpos) - (l - 1), 
			 (ITO(prog->program, (char *)prog) XFORM_OK_PLUS prog->regmust)[0]);
	if (pos == -1)
	  return 0; /* Not present. */
      } else
	return 0; /* Not present, since there's not enough room left. */

      /* ASSUMING NO GC HERE! */
      p = (ITO(prog->program, (char *)prog) XFORM_OK_PLUS prog->regmust);
      if (prog->flags & REGEXP_MUST_CI) {
	for (i = 0; i < l; i++) {
	  ch = string[pos + i];
	  ch = rx_tolower(ch);
	  if (ch != p[i])
	    break;
	}
      } else {
	for (i = 0; i < l; i++) {
	  if (string[pos + i] != p[i])
	    break;
	}
      }
      if (i >= l)
	break; /* Found it. */
      spos = pos + 1;
    }
  }

  if (prog->ncounter) {
    counters = (int *)scheme_malloc_atomic(sizeof(int) * prog->ncounter);
  } else
    counters = NULL;

  if (port) {
    if (peek) {
      peekskip = portstart;
      dropped = portstart;
      /* Make sure that's there's not an EOF before peekskip: */
      if (!SAME_OBJ(peekskip, scheme_make_integer(0))) {
        char tmp[1];
        intptr_t got;
        got = scheme_get_byte_string_unless("regexp-match", port, 
                                            tmp, 0, 1, 1,
                                            1, scheme_bin_minus(peekskip, scheme_make_integer(1)),
                                            unless_evt);
        if (got == EOF) {
          /* Hit EOF before peekstart, so cannot match */
          return 0;
        }
      }
    } else {
      /* In non-peek port mode, skip over portstart chars: */
      intptr_t amt, got;

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
	    
	    dropped = scheme_bin_plus(dropped, scheme_make_integer(got));
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
  if (prog->flags & REGEXP_ANCH) {
    if (port) {
      rxpos len = 0, space = 0;
      int aborted = 0;

      *stringp = NULL;
      if (regtry_port(prog, port, unless_evt, nonblock, 
		      startp, maybep, endp, match_stack, counters, stringp, &len, &space, 0, 
		      portend, peekskip, 0, prefix, prefix_len, prefix_offset, 0, 
                      &aborted)) {
	if (!peek) {
	  /* Need to consume matched chars: */
	  char *drain;

	  if (discard_oport && *startp)
	    scheme_put_byte_string(who, discard_oport, *stringp, 0, *startp, 0);

	  if (get_offsets)
	    drain = *stringp;
	  else
	    /* Allocate fresh in case we get different results from previous peek: */
	    drain = (char *)scheme_malloc_atomic(*endp);
	  (void)scheme_get_byte_string(who, port, drain, 0, *endp, 0, 0, 0);
	}

	*_dropped = dropped;

	return 1;
      } else {
	if (!peek) {
	  /* Need to consume all chars, up to portend */
	  char *drain;
	  intptr_t got;
	  
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
      return regtry(prog, string, stringpos, stringlen, lazy_string, startp, maybep, endp, 
                    match_stack, counters, 0, 
		    stringorigin, prefix, prefix_len, prefix_offset, 0);
  }

  /* Messy cases:  unanchored match. */
  if (port) {
    rxpos len = 0, skip = 0, space = 0;
    *stringp = NULL;

    do {
      int discard = skip - prog->maxlookback;
      int aborted = 0;

      if (discard > skip - save_prior)
        discard = skip - save_prior;

      if (discard >= REGPORT_FLUSH_THRESHOLD) {
	if (!peek) {
	  if (discard_oport)
	    scheme_put_byte_string(who, discard_oport, *stringp, 0, discard, 0);
	    
	  scheme_get_byte_string(who, port, *stringp, 0, discard, 0, 0, 0);

	  if (portend)
	    portend = scheme_bin_minus(portend, scheme_make_integer(discard));
	} else {
	  peekskip = scheme_bin_plus(peekskip, scheme_make_integer(discard));
	}

	dropped = scheme_bin_plus(dropped, scheme_make_integer(discard));

	len -= discard;
	skip -= discard;
	memmove(*stringp, *stringp + discard, len);

        prefix = NULL;
        prefix_len = 0;
      }

      if (regtry_port(prog, port, unless_evt, nonblock,
		      startp, maybep, endp, match_stack, counters, stringp, &len, &space, skip, 
		      portend, peekskip, 0, prefix, prefix_len, prefix_offset, 1,
                      &aborted)) {
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
      } else if (aborted)
        return 0;
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
    if (regtry(prog, string, stringpos, stringlen, lazy_string,
	       startp, maybep, endp, match_stack, counters,
	       0, stringorigin, prefix, prefix_len, prefix_offset, 1))
      return 1;
  }

  /* Failure. */
  return 0;
}

#define NEED_INPUT(rw, v, n) if (rw->port && (((v) + (n)) > rw->input_end)) read_more_from_regport(rw, (v) + (n))
static void read_more_from_regport(Regwork *rw, rxpos need_total);

/*
   - regtry - try match at specific point
   */
static int			/* 0 failure, 1 success */
regtry(regexp *prog, char *string, int stringpos, int stringlen, rx_lazy_str_t *lazy_string,
       rxpos *startp, rxpos *maybep, rxpos *endp, rxpos *match_stack, int *counters,
       Regwork *rw, rxpos stringorigin, 
       char *prefix, rxpos prefix_len, rxpos prefix_offset,
       int unanchored)
/* stringpos: where to start looking;
   stringlen: available bytes, counting from stringpos;
   stringorigin: start of input, after prefix
   prefix: bytes to appear before the origin to count as input */
{
  int i;
  Regwork _rw;

  if (!rw) {
    rw = &_rw;
    rw->port = NULL;
  }
  rw->instr = string;
  rw->input = stringpos;
  rw->input_end = stringpos + stringlen;
  rw->input_start = stringorigin;
  rw->input_min = stringorigin - prefix_len;
  rw->startp = startp;
  rw->maybep = maybep;
  rw->endp = endp;
  rw->counters = counters;
  rw->prefix = prefix;
  rw->prefix_len = prefix_len;
  rw->prefix_delta = prefix_len + prefix_offset - stringorigin;
  rw->boi = stringorigin - prefix_len;
  rw->rewind_stack_size = (match_stack ? MATCH_STACK_SIZE : 0);
  rw->rewind_stack_count = 0;
  rw->rewind_stack_prompt = 0;
  rw->rewind_stack = match_stack;
  if (prog->nsubexp < 2)
    rw->non_tail = -1;
  else
    rw->non_tail = 0;
  rw->lazy_string = lazy_string;
  if (lazy_string)
    rw->port = scheme_true; /* hack to make NEED_INPUT() work */

  for (i = prog->nsubexp; i--; ) {
    startp[i] = rw->input_min - 1;
    endp[i] = rw->input_min - 1;
  }

#ifdef INDIRECT_TO_PROGRAM
  regstr = prog->program;
#else
  regstr = (char *)prog;
#endif

  while (1) {
    int found;

    found = regmatch(rw, N_ITO_DELTA(prog->program, 1, (char *)prog));

    if (found) {
      startp[0] = stringpos;
      endp[0] = rw->input;
      return 1;
    } else if (unanchored) {
      if (lazy_string) {
        NEED_INPUT(rw, stringpos, 1);
        stringlen = rw->input_end - stringpos;
      }
      if (!stringlen)
	return 0;
      stringpos++;
      --stringlen;
      if (prog->regstart) {
	unsigned char *rs = prog->regstart;
	int c;
	while (1) {
	  if (lazy_string) {
            NEED_INPUT(rw, stringpos, 1);
            stringlen = rw->input_end - stringpos;
            string = rw->instr;
          }

	  if (!stringlen)
	    return 0;

          c = UCHAR(string[stringpos]);
	  if (rs[c >> 3] & (1 << (c & 0x7)))
	    break;
	  stringpos++;
	  --stringlen;
	}
      }
      rw->input = stringpos;      
      for (i = prog->nsubexp; i--; ) {
        startp[i] = rw->input_min - 1;
        endp[i] = rw->input_min - 1;
      }
      /* try again... */
    } else
      return 0;
  }
}

#define LAZY_STRING_CHUNK_SIZE 1024

static void read_more_from_lazy_string(Regwork *rw, rxpos need_total)
{
  rx_lazy_str_t *ls = rw->lazy_string;

  if (ls->start + ls->done < ls->end) {
    intptr_t amt = ls->done, blen, tlen;
    char *s;

    amt = amt ? (2 * amt) : LAZY_STRING_CHUNK_SIZE;
    if (ls->done + amt < need_total)
      amt = need_total - ls->done;
    if (ls->start + ls->done + amt > ls->end)
      amt = ls->end - ls->start - ls->done;

    blen = scheme_utf8_encode(ls->chars, ls->start + ls->done, ls->start + ls->done + amt,
                              NULL, 0,
                              0 /* not UTF-16 */);
    tlen = blen + ls->blen;
    s = (char *)scheme_malloc_atomic(tlen);
    memcpy(s, ls->s, ls->blen);
    scheme_utf8_encode(ls->chars, ls->start + ls->done, ls->start + ls->done + amt,
                       (unsigned char *)s, ls->blen,
                       0 /* not UTF-16 */);

    ls->blen = tlen;
    ls->s = s;
    ls->done += amt;

    rw->instr = s;
    rw->input_end = tlen;
  } else {
    /* turn off further port reading */
    rw->port = NULL;
  }
}

static void read_more_from_regport(Regwork *rw, rxpos need_total)
     /* Called when we're about to look past our read-ahead */
{
  intptr_t got;
  Scheme_Object *peekskip;

  if (rw->lazy_string) {
    read_more_from_lazy_string(rw, need_total);
    return;
  }

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
    intptr_t size = rw->instr_size;
    
    size = size * 2;
    if (size < need_total)
      size += need_total;
    if (size < 16)
      size = 16;

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
    while (need_total > rw->input_end) {
      if (rw->peekskip)
        peekskip = scheme_bin_plus(scheme_make_integer(rw->input_end), rw->peekskip);
      else
        peekskip = scheme_make_integer(rw->input_end);
      
      rw->str = regstr; /* get_string can swap threads */
      got = scheme_get_byte_string_unless("regexp-match", rw->port, 
                                          rw->instr, rw->input_end, need_total - rw->input_end,
                                          (rw->nonblock ? 2 : 0), /* blocking mode */
                                          1, peekskip,
                                          rw->unless_evt);
      regstr = rw->str;
      
      if (!got && rw->nonblock) {
        rw->port = NULL; /* turn off further port reading */
        rw->unless_evt = NULL;
        rw->aborted = 1;
        break;
      } else if (got == EOF) {
        rw->port = NULL; /* turn off further port reading */
        rw->unless_evt = NULL;
        break;
      } else {
        rw->input_end += got;
        if (!rw->nonblock)
          break;
      }
    }
  }
}

/*
   - regtry - try match in a port
   */
static int
regtry_port(regexp *prog, Scheme_Object *port, Scheme_Object *unless_evt, int nonblock,
	    rxpos *startp, rxpos *maybep, rxpos *endp, rxpos *match_stack, int *counters,
	    char **work_string, rxpos *len, rxpos *size, rxpos skip, 
	    Scheme_Object *maxlen, Scheme_Object *peekskip, 
	    rxpos origin, char *prefix, rxpos prefix_len, rxpos prefix_offset,
	    int read_at_least_one, int *_aborted)
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

  m = regtry(prog, *work_string, skip, (*len) - skip, NULL,
	     startp, maybep, endp, match_stack, counters,
	     &rw, origin, prefix, prefix_len, prefix_offset, 0);

  if (read_at_least_one
      && !rw.aborted
      && (rw.input_end == skip)
      && rw.port) {
    read_more_from_regport(&rw, rw.input_end + 1);
  }
  
  *work_string = rw.instr;
  *len = rw.input_end;
  *size = rw.instr_size;

  if (rw.aborted) {
    *_aborted = 1;
    return 0;
  } else
    return m;
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

XFORM_NONGCING static MZ_INLINE char INPUT_REF_S(Regwork *rw, rxpos c, rxpos input_start)
{
  if (c < input_start)
    return rw->prefix[rw->prefix_delta + c];
  else
    return rw->instr[c];
}

XFORM_NONGCING static MZ_INLINE char INPUT_REF(Regwork *rw, int c)
{
  return INPUT_REF_S(rw, c, rw->input_start);
}

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
  rxpos is;		/* Input string pos */
  int the_op;

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

  is = rw->input;
  scan = prog;
  while (scan != 0) {
    the_op = rOP(scan);
    switch (the_op) {
    case BOI:
      if (is != rw->boi)
	return(0);
      scan = NEXT_OP(scan);
      break;
    case EOI:
      NEED_INPUT(rw, is, 1);
      if (is != rw->input_end)
	return(0);
      scan = NEXT_OP(scan);
      break;
    case BOL:
      if ((is != rw->boi)
	  && ((is <= rw->input_min)
	      || (INPUT_REF(rw, is - 1) != '\n')))
	return(0);
      scan = NEXT_OP(scan);
      break;
    case EOL:
      NEED_INPUT(rw, is, 1);
      if (is != rw->input_end) {
	if (INPUT_REF(rw, is) != '\n')
	  return(0);
      }
      scan = NEXT_OP(scan);
      break;
    case ANY:
      NEED_INPUT(rw, is, 1);
      if (is == rw->input_end)
	return(0);
      is++;
      scan = NEXT_OP(scan);
      break;
    case ANYL:
      NEED_INPUT(rw, is, 1);
      if (is == rw->input_end)
	return(0);
      if (INPUT_REF(rw, is) == '\n')
	return 0;
      is++;
      scan = NEXT_OP(scan);
      break;
    case EXACTLY:
      {
	int len, i;
	rxpos opnd;

	opnd = OPSTR(OPERAND(scan));
	len = rOPLEN(OPERAND(scan));
	if (rw->port) {
	  /* Like the other branch, but demand chars one at a time, as
	     we need them */
          rxpos input_start = rw->input_start;
	  for (i = 0; i < len; i++) {
	    NEED_INPUT(rw, is + i, 1);
	    if (is + i >= rw->input_end)
	      return 0;
	    if (regstr[opnd+i] != INPUT_REF_S(rw, is+i, input_start))
	      return 0;
	  }
	} else {
          rxpos input_start;
	  if (len > rw->input_end - is)
	    return 0;
          input_start = rw->input_start;
	  for (i = 0; i < len; i++) {
	    if (regstr[opnd+i] != INPUT_REF_S(rw, is+i, input_start))
	      return 0;
	  }
	}
	is += len;
      }
      scan = NEXT_OP(scan);
      break;
    case EXACTLY_CI:
      {
	int len, i;
	char c;
	rxpos opnd;

	opnd = OPSTR(OPERAND(scan));
	len = rOPLEN(OPERAND(scan));
	if (rw->port) {
	  /* Like the other branch, but demand chars one at a time, as
	     we need them */
	  for (i = 0; i < len; i++) {
	    NEED_INPUT(rw, is + i, 1);
	    if (is + i >= rw->input_end)
	      return 0;
	    c = INPUT_REF(rw, is+i);
	    c = rx_tolower(c);
	    if (regstr[opnd+i] != c)
	      return 0;
	  }
	} else {
          rxpos input_start;
	  if (len > rw->input_end - is)
	    return 0;
          input_start = rw->input_start;
	  for (i = 0; i < len; i++) {
	    c = INPUT_REF_S(rw, is+i, input_start);
	    c = rx_tolower(c);
	    if (regstr[opnd+i] != c)
	      return 0;
	  }
	}
	is += len;
      }
      scan = NEXT_OP(scan);
      break;
    case ANYOF:
      {
	int c;
	NEED_INPUT(rw, is, 1);
	if (is == rw->input_end)
	  return 0;
	c = UCHAR(INPUT_REF(rw, is));
	if (!(regstr[OPERAND(scan) + (c >> 3)] & (1 << (c & 0x7))))
	  return(0);
	is++;
	scan = NEXT_OP(scan);
      }
      break;
    case EXACTLY1:
      NEED_INPUT(rw, is, 1);
      if (is == rw->input_end)
	return 0;
      if (INPUT_REF(rw, is) != regstr[OPERAND(scan)])
	return 0;
      is++;
      scan = NEXT_OP(scan);
      break;
    case EXACTLY2:
      NEED_INPUT(rw, is, 1);
      if (is == rw->input_end)
	return 0;
      if (INPUT_REF(rw, is) != regstr[OPERAND(scan)])
        if (INPUT_REF(rw, is) != regstr[OPERAND(scan)+1])
          return 0;
      is++;
      scan = NEXT_OP(scan);
      break;
    case RANGE:
      {
	int c;
	NEED_INPUT(rw, is, 1);
	if (is == rw->input_end)
	  return 0;
	c = UCHAR(INPUT_REF(rw, is));
	if ((c < UCHAR(regstr[OPERAND(scan)]))
	    || (c > UCHAR(regstr[OPERAND(scan)+1])))
	  return(0);
	is++;
	scan = NEXT_OP(scan);
      }
      break;
    case NOTRANGE:
      {
	int c;
	NEED_INPUT(rw, is, 1);
	if (is == rw->input_end)
	  return 0;
	c = UCHAR(INPUT_REF(rw, is));
	if ((c >= UCHAR(regstr[OPERAND(scan)]))
	    && (c <= UCHAR(regstr[OPERAND(scan)+1])))
	  return(0);
	is++;
	scan = NEXT_OP(scan);
      }
      break;
    case NOTHING:
      scan = NEXT_OP(scan);
      break;
    case BACK:
      scan = scan - rNEXT(scan);
      break;
    case BRANCH:
      {
        rxpos delta;
	rxpos next;  /* Next node. */
        int stack_pos, ok;

	next = NEXT_OP(scan);

        if (rOP(next) != BRANCH) /* No choice. */
          scan = OPERAND(scan);	/* Avoid recursion. */
        else {
          do {
            rw->input = is;
            stack_pos = match_push(rw);
            ok = regmatch(rw, OPERAND(scan));
            match_pop(rw, stack_pos, ok);

            if (ok) return(1);

	    scan = next;
	    delta = rNEXT(scan);
            if (!delta)
	      break;
            next = scan + delta;
          } while (rOP(next) == BRANCH);
	  scan = OPERAND(scan);
        }
      }
      break;
    case STAR:
    case PLUS:
    case STAR2:
    case PLUS2:
    case STAR3:
    case STAR4:
      {
	char nextch;
	int no;
	rxpos save, body;
	int min, maxc;
	int nongreedy = (the_op == STAR2 || the_op == PLUS2 || the_op == STAR4);
	rxpos next;  /* Next node. */
        int stack_pos, ok;
	
	/*
	 * Lookahead to avoid useless match attempts
	 * when we know what character comes next.
	 */
	nextch = '\0';
	next = NEXT_OP(scan);
	if (rOP(next) == EXACTLY)
	  nextch = regstr[OPSTR(OPERAND(next))];
	if ((the_op == STAR3) || (the_op == STAR4)) {
	  min = rOPLEN(OPERAND(scan));
	  maxc = rOPLEN(OPERAND2(scan));
	  body = OPERAND3(scan);
	} else {
	  body = OPERAND(scan);
	  min = ((the_op == STAR) || (the_op == STAR2)) ? 0 : 1;
	  maxc = 0;
	}
	save = is;

	rw->input = is;
	if (nongreedy && rw->port) {
	  /* Get at least `min' bytes, but then don't
	     let regrepeat pull in arbitrary bytes: */
	  Scheme_Object *saveport;
	  NEED_INPUT(rw, save, min ? min : 1);
	  saveport = rw->port;
	  rw->port = NULL;
	  no = regrepeat(rw, body, maxc);
	  rw->port = saveport;
	  nongreedy = 2;
	} else
	  no = regrepeat(rw, body, maxc);

	if (!nongreedy) {
	  if (nextch)
	    NEED_INPUT(rw, save + no, 1);
	  while (no >= min) {
	    /* If it could work, try it. */
	    if (nextch == '\0' || ((save + no < rw->input_end)
				   && (INPUT_REF(rw, save + no) == nextch))) {
	      rw->input = is + no;
              stack_pos = match_push(rw);
	      ok = regmatch(rw, next);
              match_pop(rw, stack_pos, ok);
              if (ok) return(1);
	    }
	    /* Couldn't or didn't -- back up. */
	    no--;
	  }
	} else {
	  int i;
	  for (i = min; i <= no; i++) {
	    /* If it could work, try it. */
	    if (nextch)
	      NEED_INPUT(rw, save + i, 1);
	    if (nextch == '\0' || ((save+i < rw->input_end)
				   && (INPUT_REF(rw, save+i) == nextch))) {
	      rw->input = save + i;
              stack_pos = match_push(rw);
              ok = regmatch(rw, next);
              match_pop(rw, stack_pos, ok);
              if (ok) return(1);
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
		is = save + no;
		rw->input = is;
		moreno = regrepeat(rw, body, maxc ? maxc - no : 0);
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
    case LOOKE:
      rw->input = is;
      return(1);		/* Success! */
      break;
    case BACKREF:
      {
	int no, len, start, i;
	no = rOPLEN(OPERAND(scan));
	if (rw->endp[no] < rw->input_min)
	  return 0;

	start = rw->startp[no];
	len = rw->endp[no] - start;

	if (rw->port) {
	  /* Like the other branch, but demand chars one at a time, as
	     we need them */
          rxpos input_start = rw->input_start;
	  for (i = 0; i < len; i++) {
	    NEED_INPUT(rw, is + i, 1);
	    if (is + i >= rw->input_end)
	      return 0;
	    if (INPUT_REF_S(rw, start+i, input_start) != INPUT_REF_S(rw, is+i, input_start))
	      return 0;
	  }
	} else {
          rxpos input_start = rw->input_start;
	  if (len > rw->input_end - is)
	    return 0;
	  for (i = 0; i < len; i++) {
	    if (INPUT_REF_S(rw, start+i, input_start) != INPUT_REF_S(rw, is+i, input_start))
	      return 0;
	  }
	}
	is += len;
	scan = NEXT_OP(scan);
	break;
      }
    case BACKREF_CI:
      {
	int no, len, start, i, c1, c2;
	no = rOPLEN(OPERAND(scan));
	if (rw->endp[no] < rw->input_min)
	  return 0;

	start = rw->startp[no];
	len = rw->endp[no] - start;

	if (rw->port) {
	  /* Like the other branch, but demand chars one at a time, as
	     we need them */
          rxpos input_start = rw->input_start;
	  for (i = 0; i < len; i++) {
	    NEED_INPUT(rw, is + i, 1);
	    if (is + i >= rw->input_end)
	      return 0;
	    c1 = INPUT_REF_S(rw, start+i, input_start);
	    c1 = rx_tolower(c1);
	    c2 = INPUT_REF_S(rw, is+i, input_start);
	    c2 = rx_tolower(c2);
	    if (c1 != c2)
	      return 0;
	  }
	} else {
          rxpos input_start = rw->input_start;
	  if (len > rw->input_end - is)
	    return 0;
	  for (i = 0; i < len; i++) {
	    c1 = INPUT_REF_S(rw, start+i, input_start);
	    c1 = rx_tolower(c1);
	    c2 = INPUT_REF_S(rw, is+i, input_start);
	    c2 = rx_tolower(c2);
	    if (c1 != c2)
	      return 0;
	  }
	}
	is += len;
	scan = NEXT_OP(scan);
	break;
      }
    case LOOKT:
    case LOOKF:
    case LOOKTX:
    case LOOKBT:
    case LOOKBF:
      {
	int t, no, no_start, no_end;
	rxpos save, next;
        int stack_pos, ok;
	next = NEXT_OP(scan);
	t = ((the_op != LOOKF) && (the_op != LOOKBF));
	if ((the_op == LOOKBT)  || (the_op == LOOKBF)) {
	  no_start = rOPLEN(OPERAND2(scan));
	  no_end = rOPLEN(OPERAND3(scan));
	} else
	  no_start = no_end = 0;
	save = is;
	if (no_end) {
          /* lookbehind */
          int found = 0;
	  for (no = no_start; no <= no_end; no++) {
	    if (is - rw->input_min >= no) {
	      rw->input = save - no;
              stack_pos = match_push(rw);
              ok = regmatch(rw, next);
              match_pop(rw, stack_pos, ok);
	      if (ok) {
		if (rw->input == save) {
		  /* Match */
		  if (!t) return 0;
                  found = 1;
		  break;
		}
	      }
	    } else
	      break;
	  }
	  if (!found) {
	    /* No matches */
	    if (t) return 0;
	  }
	} else {
          /* lookahead */
	  rw->input = is;
          stack_pos = match_push(rw);
          ok = regmatch(rw, next);
          match_pop(rw, stack_pos, ok);
	  if (ok) {
	    if (!t) return 0;
	  } else {
	    if (t) return 0;
	  }
	  if (the_op == LOOKTX)
	    is = rw->input;
	}
	scan = scan + rOPLEN(OPERAND(scan));
	scan = NEXT_OP(scan);
      }
      break;
    case COUNTINIT:
      {
	int no;
	no = rOPLEN(OPERAND(scan));
	rw->counters[no] = 0;
	scan = NEXT_OP(scan);
      }
      break;
    case COUNTBACK:
      {
	int no;
	no = rOPLEN(OPERAND(scan));
	rw->counters[no] -= 1;
	scan = NEXT_OP(scan);
      }
      break;
    case COUNTBACKFAIL:
      {
	int no;
	no = rOPLEN(OPERAND(scan));
	rw->counters[no] -= 1;
	return 0;
      }
      break;
    case COUNTUNDER:
      {
	int no, maxreps;
	no = rOPLEN(OPERAND(scan));
	maxreps = rOPLEN(OPERAND2(scan));
	rw->counters[no]++;
	if (maxreps && (rw->counters[no] > maxreps))
	  return 0;
	scan = NEXT_OP(scan);
      }
      break;
    case COUNTOVER:
      {
	int no, minreps;
	no = rOPLEN(OPERAND(scan));
	minreps = rOPLEN(OPERAND2(scan));
	if (rw->counters[no] < minreps)
	  return 0;
	scan = NEXT_OP(scan);
      }
      break;
    case SAVECONST:
      {
	int no, len;
	no = rOPLEN(OPERAND(scan));
	len = rOPLEN(OPERAND2(scan));
	/* Check that the match happened more than 0 times: */
	if (!len || (is > rw->maybep[no])) {
          match_set(rw, no, is-len, is);
	} else {
          match_set(rw, no, rw->input_min - 1, rw->input_min - 1);
	}
	scan = NEXT_OP(scan);        
      }
      break;
    case MAYBECONST:
      {
	int no;
	no = rOPLEN(OPERAND(scan));
        match_maybe(rw, no, is);
	scan = NEXT_OP(scan);
      }
      break;
    case WORDBOUND:
      {
        int c, w1, w2;
	NEED_INPUT(rw, is, 1);
	if (is > rw->input_min) {
          c = INPUT_REF(rw, is - 1);
          w1 = rx_isword(c);
        } else
          w1 = 0;
        if (is < rw->input_end) {
          c = INPUT_REF(rw, is);
          w2 = rx_isword(c);
        } else
          w2 = 0;
        if (w1 == w2) return 0;
	scan = NEXT_OP(scan);
      }
      break;
    case NOTWORDBOUND:
      {
        int c, w1, w2;
	NEED_INPUT(rw, is, 1);
	if (is > rw->input_min) {
          c = INPUT_REF(rw, is - 1);
          w1 = rx_isword(c);
        } else
          w1 = 0;
        if (is < rw->input_end) {
          c = INPUT_REF(rw, is);
          w2 = rx_isword(c);
        } else
          w2 = 0;
        if (w1 != w2) return 0;
	scan = NEXT_OP(scan);
      }
      break;
    case UNIPROP:
      {
	unsigned char buf[MAX_UTF8_CHAR_BYTES];
	mzchar us[1];
	int c, data;
	int v, pos;
	int negate, bottom, top;
	
	data = rOPLEN(OPERAND(scan));

	negate = data >> 13;
	bottom = (data >> 6) & 0x3F;
	top = data & 0x3F;
	
	NEED_INPUT(rw, is, 1);
	if (is < rw->input_end) {
	  c = UCHAR(INPUT_REF(rw, is));
	  if (c < 128) {
	    v = c;
	    pos = 1;
	  } else {
	    pos = 1;
	    buf[0] = c;
	    while (1) {
	      v = scheme_utf8_decode_prefix(buf, pos, us, 0);
	      if (v == 1) {
		v = us[0];
		break;
	      } else if (v < -1)
		return 0;
	      NEED_INPUT(rw, is, pos+1);
	      if (is + pos < rw->input_end) {
		buf[pos] = INPUT_REF(rw, is + pos);
		pos++;
	      } else
		return 0;
	    }
	  }
	} else
	  return 0;
  
	is += pos;

	v = scheme_general_category(v);
	
	if (negate) {
	  if ((v >= bottom) && (v <= top))
	    return 0;
	} else {
	  if ((v < bottom) || (v > top))
	    return 0;
	}
	
	scan = NEXT_OP(scan);
      }
      break;
    case CONDITIONAL:
      {
	rxpos test = OPERAND3(scan);
	int t;
        int stack_pos;

	if (rOP(test) == BACKREF) {
	  int no;
	  no = rOPLEN(OPERAND(test));
	  t = (rw->endp[no] > rw->input_min);
	} else {
	  rw->input = is;
          stack_pos = match_push(rw);
	  t = regmatch(rw, test);
          match_pop(rw, stack_pos, t);
	}

	if (t)
	  scan = scan + rOPLEN(OPERAND(scan));
	else
	  scan = scan + rOPLEN(OPERAND2(scan));
      }
      break;
    default:
      {
	int isopen;
	int no;

	switch (the_op) {
	case OPENN:
	  isopen = 1;
	  no = rOPLEN(OPERAND(scan));
	  if (!no)
	    no = -1; /* => don't set in result array */
	  break;
	case CLOSEN:
	  isopen = 0;
	  no = rOPLEN(OPERAND(scan));
	  if (!no)
	    no = -1; /* => don't set in result array */
	  break;
	default:
	  if (the_op < CLOSE) {
	    isopen = 1;
	    no = the_op - OPEN;
	  } else {
	    isopen = 0;
	    no = the_op - CLOSE;
	  }
	}

	if (no < 0) {
	  /* Nothing to set */
	} else if (isopen) {
          /* Storing the position in maybep instead of startp
             allows a backreference to refer to a match from a
             previous iteration in patterns like `(a|\1x)*'. */
          match_maybe(rw, no, is);
        } else
          match_set(rw, no, rw->maybep[no], is);

        scan = NEXT_OP(scan);
      }
      break;
    }
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
regrepeat(Regwork *rw, rxpos p, int maxc)
{
  int count = 0;
  rxpos scan;
  rxpos opnd;

  scan = rw->input;
  opnd = OPERAND(p);
  switch (rOP(p)) {
  case ANY:
    if (rw->port) {
      if (maxc) {
	while (rw->port && (rw->input_end < scan + maxc)) {
	  read_more_from_regport(rw, scan + maxc);
	}
      } else {
	/* need all port input: */
	while (rw->port) {
	  read_more_from_regport(rw, rw->input_end + 4096);
	}
      }
    }
    count = rw->input_end - scan;
    if (maxc && (count > maxc))
      count = maxc;
    scan += count;
    break;
  case ANYL:
    {
      rxpos input_start = rw->input_start;
      NEED_INPUT(rw, scan, 1);
      while (scan != rw->input_end
	     && (INPUT_REF_S(rw, scan, input_start) != '\n')) {
	count++;
	scan++;
	if (maxc) { maxc--; if (!maxc) break; }
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  case EXACTLY:
    {
      rxpos input_start = rw->input_start;
      rxpos opnd2 = OPSTR(opnd);
      NEED_INPUT(rw, scan, 1);
      while (scan != rw->input_end
	     && (regstr[opnd2] == INPUT_REF_S(rw, scan, input_start))) {
	count++;
	scan++;
	if (maxc) { maxc--; if (!maxc) break; }
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  case EXACTLY_CI:
    {
      char c;
      rxpos input_start = rw->input_start;
      rxpos opnd2 = OPSTR(opnd);
      NEED_INPUT(rw, scan, 1);
      while (scan != rw->input_end) {
	c = INPUT_REF_S(rw, scan, input_start);
	c = rx_tolower(c);
	if (regstr[opnd2] != c)
	  break;
	count++;
	scan++;
	if (maxc) { maxc--; if (!maxc) break; }
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  case ANYOF:
    {
      int c;
      rxpos input_start = rw->input_start;
      rxpos init = scan;
      if (rw->port || maxc) {
	/* Slow but general version */
	NEED_INPUT(rw, scan, 1);
	while (scan != rw->input_end) {
	  c = UCHAR(INPUT_REF_S(rw, scan, input_start));
	  if (!(regstr[opnd + (c >> 3)] & (1 << (c & 0x7))))
	    break;
	  scan++;
	  if (maxc) { maxc--; if (!maxc) break; }
	  NEED_INPUT(rw, scan, 1);
	}
      } else {
	/* Fast version */
	int e = rw->input_end;
	while (scan != e) {
	  c = UCHAR(INPUT_REF_S(rw, scan, input_start));
	  if (!(regstr[opnd + (c >> 3)] & (1 << (c & 0x7))))
	    break;
	  scan++;
	}
      }
      count = scan - init;
    }
    break;
  case EXACTLY1:
    {
      rxpos init = scan;
      rxpos input_start = rw->input_start;
      char c;
      c = regstr[opnd];
      if (rw->port || maxc) {
	/* Slow but general version */
	NEED_INPUT(rw, scan, 1);
	while ((scan != rw->input_end)
	       && (INPUT_REF_S(rw, scan, input_start) == c)) {
	  scan++;
	  if (maxc) { maxc--; if (!maxc) break; }
	  NEED_INPUT(rw, scan, 1);
	}
      } else {
	/* Fast version */
	int e = rw->input_end;
	while ((scan != e)
	       && (INPUT_REF(rw, scan) == c)) {
	  scan++;
	}
      }
      count = scan - init;
    }
    break;
  case EXACTLY2:
    {
      rxpos init = scan;
      rxpos input_start = rw->input_start;
      char c1, c2;
      c1 = regstr[opnd];
      c2 = regstr[opnd+1];
      if (rw->port || maxc) {
	/* Slow but general version */
	NEED_INPUT(rw, scan, 1);
	while ((scan != rw->input_end)
	       && ((INPUT_REF_S(rw, scan, input_start) == c1)
                   || (INPUT_REF_S(rw, scan, input_start) == c2))) {
	  scan++;
	  if (maxc) { maxc--; if (!maxc) break; }
	  NEED_INPUT(rw, scan, 1);
	}
      } else {
	/* Fast version */
	int e = rw->input_end;
	while ((scan != e)
	       && ((INPUT_REF(rw, scan) == c1)
                   || (INPUT_REF(rw, scan) == c2))) {
	  scan++;
	}
      }
      count = scan - init;
    }
    break;
  case RANGE:
    {
      rxpos init = scan;
      rxpos input_start = rw->input_start;
      int c, sr, er;
      NEED_INPUT(rw, scan, 1);
      sr = UCHAR(regstr[opnd]);
      er = UCHAR(regstr[opnd + 1]);
      if (rw->port || maxc) {
	/* Slow but general version */
	while (scan != rw->input_end) {
	  c = UCHAR(INPUT_REF_S(rw, scan, input_start));
	  if ((c < sr) || (c > er))
	    break;
	  scan++;
	  if (maxc) { maxc--; if (!maxc) break; }
	  NEED_INPUT(rw, scan, 1);
	}
      } else {
	/* Fast version */
	int e = rw->input_end;
	while (scan != e) {
	  c = UCHAR(INPUT_REF(rw, scan));
	  if ((c < sr) || (c > er))
	    break;
	  scan++;
	}
      }
      count = scan - init;
    }
    break;
  case NOTRANGE:
    {
      rxpos input_start = rw->input_start;
      rxpos init = scan;
      int c, sr, er;
      NEED_INPUT(rw, scan, 1);
      sr = UCHAR(regstr[opnd]);
      er = UCHAR(regstr[opnd + 1]);
      if (rw->port || maxc) {
	/* Slow but general version */
	while (scan != rw->input_end) {
	  c = UCHAR(INPUT_REF_S(rw, scan, input_start));
	  if ((c >= sr) && (c <= er))
	    break;
	  scan++;
	  if (maxc) { maxc--; if (!maxc) break; }
	  NEED_INPUT(rw, scan, 1);
	}
      } else {
	/* Fast version */
	int e = rw->input_end;
	while (scan != e) {
	  c = UCHAR(INPUT_REF_S(rw, scan, input_start));
	  if ((c >= sr) && (c <= er))
	    break;
	  scan++;
	}
      }
      count = scan - init;
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

  if (p + 2 >= regcodesize)
    return 0;

  offset = rNEXT(p);
  if (offset == 0)
    return 0;

  if (rOP(p) == BACK)
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
char *regsub(regexp *prog, char *src, int sourcelen, intptr_t *lenout, char *insrc, 
             rxpos *startp, rxpos *endp, rxpos minpos,
             char *prefix, rxpos prefix_offset)
{
  char *dest;
  char c;
  intptr_t no;
  intptr_t len;
  intptr_t destalloc, destlen, srcpos;
	
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
    } else if ((startp[no] >= minpos) && (endp[no] >= minpos)) {
      len = endp[no] - startp[no];
      if (len + destlen >= destalloc) {
	char *old = dest;
	destalloc = 2 * destalloc + len + destlen;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      if (startp[no] >= 0)
        memcpy(dest + destlen, insrc + startp[no], len);
      else
        memcpy(dest + destlen, prefix + prefix_offset + (startp[no] - minpos), len);
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

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif
#include "../gc2/my_qsort.c"
#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static int compare_ranges(const void *a, const void *b)
{
  unsigned int av, bv;
  av = *(unsigned int *)a;
  bv = *(unsigned int *)b;
  if (av == bv)
    return 0;
  else if (av < bv)
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
    nr = (unsigned char *)scheme_malloc_atomic(nrs+1);
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
  const unsigned char *lowest = (unsigned char *)"\200\200\200\200\200";
  const unsigned char *highest = (unsigned char *)"\277\277\277\277\277";
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

static int need_ci_alternates(unsigned char *s, int delta, int len)
{
  mzchar us[1], c;

  scheme_utf8_decode(s, delta, len, us, 0, 1, NULL, 0, 0);
  c = us[0];

  return ((c != scheme_toupper(c))
	  || (c != scheme_tolower(c))
	  || (c != scheme_tofold(c))
	  || (c != scheme_totitle(c)));
}

static int translate(unsigned char *s, int len, char **result, int pcre)
{
  int j, parse_flags = PARSE_CASE_SENS | PARSE_SINGLE_LINE;
  RoomState rs;
  unsigned char *r;
  Scheme_Object *parse_params = NULL;

  rs.orig_len = len;
  rs.size = len;
  
  r = (unsigned char *)scheme_malloc_atomic(rs.size + 1);

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
	else if (pcre && (s[k] == '\\') && (k + 1 < len))
	  k++;
        else if (pcre 
                 && (s[k] == '[') 
                 && (k + 1 < len)
                 && (s[k+1] == ':')
                 && is_posix_char_class((char *)s, k + 1, len, NULL)) {
          while (s[k] != ']') {
            k++;
          }
        }
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
	      && us[p+1] == '-'
	      && (!pcre || ((us[p] != '\\') && (us[p+2] != '\\')))) {
	    int beg = us[p], end = us[p+2];
	    if (end == '-') {
	      FAIL("misplaced hypen within square brackets in pattern");
	    }
	    if (end < beg) {
	      /* Bad regexp */
	      FAIL("invalid range within square brackets in pattern");	      
	    }
	      
	    if ((beg > 127) || (end > 127)) {
	      /* A big-char range */
	      ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(beg),
							 scheme_make_integer_value_from_unsigned(end)),
					ranges);
	      if (!(parse_flags & PARSE_CASE_SENS)) {
		/* Try to build up parallel ranges, though they may
		   not turn out to be parallel. If the ranges overlap,
		   we'll clean them up in the final sort-and-merge
		   pass for the whole ranges list. */
		int c, beg2, end2, c2, mode;
		for (mode = 0; mode < 4; mode++) {
		  for (c = beg; c <= end; c++) {
		    switch (mode) {
		    case 0:
		      beg2 = scheme_tofold(c);
		      break;
		    case 1:
		      beg2 = scheme_tolower(c);
		      break;
		    case 2:
		      beg2 = scheme_toupper(c);
		      break;
		    case 3:
		    default:
		      beg2 = scheme_totitle(c);
		      break;
		    }
		    if (c != beg2) {
		      end2 = beg2;
		      for (; c <= end; c++) {
			switch (mode) {
			case 0:
			  c2 = scheme_tofold(c);
			  break;
			case 1:
			  c2 = scheme_tolower(c);
			  break;
			case 2:
			  c2 = scheme_toupper(c);
			  break;
			case 3:
			default:
			  c2 = scheme_totitle(c);
			  break;
			}
			if ((c2 == c) || (c2 != end2 + 1))
			  break;
		      }
		      ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(beg2),
								 scheme_make_integer_value_from_unsigned(end2)),
						ranges);
		    }
		  }
		}
	      }
	    } else {
	      /* Small range */
	      int w;
	      for (w = beg; w <= end; w++) {
		simple_on[w] = 1;
	      }
	    }
	    p += 3;
	  } else if (pcre && (us[p] == '\\')) {
	    if ((p + 1) < ulen) {
	      int c = us[p + 1];
	      if (((c >= 'a') && (c <= 'z'))
		  || ((c >= 'A') && (c <= 'Z'))) {
		regcharclass(c, simple_on);
		p += 2;
	      } else if (c < 128) {
		simple_on[c] = 1;
		p += 2;
	      } else {
		/* Let next iteration handle it.
		   (There's no danger of using it as a meta-character.) */
                p++;
	      }
	    } else
	      FAIL("trailing \\ in pattern");
	  } else if (us[p] > 127) {
	    int c = us[p];
	    ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(c),
						       scheme_make_integer_value_from_unsigned(c)),
				      ranges);
	    if (!(parse_flags & PARSE_CASE_SENS)) {
	      int mode, c2;
	      for (mode = 0; mode < 4; mode++) {
		switch (mode) {
		case 0:
		  c2 = scheme_tofold(c);
		  break;
		case 1:
		  c2 = scheme_tolower(c);
		  break;
		case 2:
		  c2 = scheme_toupper(c);
		  break;
		case 3:
		default:
		  c2 = scheme_totitle(c);
		  break;
		}
		if (c2 != c) {
		  ranges = scheme_make_pair(scheme_make_pair(scheme_make_integer_value_from_unsigned(c2),
							     scheme_make_integer_value_from_unsigned(c2)),
					    ranges);
		}
	      }
	    }
	    p++;
          } else if (pcre
                     && (us[p] == '[')
                     && ((p + 1) < ulen)
                     && (us[p+1] == ':')
                     && is_posix_char_class_in_unicode(us, p + 1, ulen, simple_on)) {
            while (us[p] != ']') {
              p++;
            }
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
	for (rp = 0; SCHEME_PAIRP(ranges); ranges = SCHEME_CDR(ranges), rp += 2) {
	  uintptr_t hi, lo;
	  scheme_get_unsigned_int_val(SCHEME_CAAR(ranges), &lo);
	  scheme_get_unsigned_int_val(SCHEME_CDR(SCHEME_CAR(ranges)), &hi);
	  range_array[rp] = (unsigned int)lo;
	  range_array[rp+1] = (unsigned int)hi;
	}
	range_len *= 2;
	/* Sort the ranges by the starting index. */
	my_qsort(range_array, range_len >> 1, 2 * sizeof(unsigned int), compare_ranges);
	
	/* If a range starts below 128, fill in the simple array */
	for (rp = 0; rp < range_len; rp += 2) {
	  if (range_array[rp] < 128) {
	    for (p = range_array[rp]; p < 128; p++) {
	      simple_on[p] = 1;
	    }
	    range_array[rp] = 128;
	  }
	}
	
	if (!(parse_flags & PARSE_CASE_SENS)) {
	  for (p = 'a'; p <= 'z'; p++) {
	    if (simple_on[p])
	      simple_on[rx_toupper(p)] = 1;
	    if (simple_on[rx_toupper(p)])
	      simple_on[p] = 1;
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
	  r = make_room(r, j, 6 + (128 - on_count) + ((pcre && !simple_on['\\']) ? 1 : 0), &rs);
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
		if ((p != '-') && (p != ']') && (!pcre || (p != '\\')))
		  if (!simple_on[p])
		    r[j++] = p;
	      }
	      if (pcre && !simple_on['\\']) {
		r[j++] = '\\';
		r[j++] = '\\';
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
	  r = make_room(r, j, 5 + on_count + ((pcre && simple_on['\\']) ? 1 : 0), &rs);
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
		if ((p != '-') && (p != ']') && (!pcre || (p != '\\')))
		  if (simple_on[p])
		    r[j++] = p;
	      }
	      if (pcre && simple_on['\\']) {
		r[j++] = '\\';
		r[j++] = '\\';
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
    } else if ((s[rs.i] == '.')
	       && (!pcre
		   || (rs.i < 3)
		   || (s[rs.i-1] != '{')
		   || ((s[rs.i-2] == 'p')
		       && (s[rs.i-2] == 'P'))
		   || (s[rs.i-3] != '\\'))) {
      /* "." has to be expanded. */
      r = make_room(r, j, (parse_flags & PARSE_SINGLE_LINE) ? 9 : 8, &rs);
      r[j++] = '(';
      r[j++] = '?';
      r[j++] = ':';
      r[j++] = '[';
      r[j++] = '\00';
      r[j++] = '-';
      if (!(parse_flags & PARSE_SINGLE_LINE)) {
	r[j++] = '\n' - 1;
	r[j++] = '\n' + 1;
	r[j++] = '-';
      }
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
			|| (s[k] == '?')
			|| (!(parse_flags & PARSE_CASE_SENS)
			    && need_ci_alternates(s, rs.i, k)))) {
	/* Need to translate; wrap char in (?: ...) */
	int orig_i;
	r = make_room(r, j, 4, &rs);
	r[j++] = '(';
	r[j++] = '?';
	r[j++] = ':';
	orig_i = rs.i;
	while (rs.i < k) {
	  r[j++] = s[rs.i++];
	}
	if (!(parse_flags & PARSE_CASE_SENS)) {
	  /* Add alternates for different cases: */
	  mzchar us[1], c0, c1, wrote[4];
	  int clen, ci, num_wrote = 1, mode;
	  unsigned char s2[MAX_UTF8_CHAR_BYTES];

	  scheme_utf8_decode(s, orig_i, k, us, 0, 1, NULL, 0, 0);
	  c0 = us[0];
	  wrote[0] = c0;
	  for (mode = 0; mode < 4; mode++) {
	    switch (mode) {
	    case 0:
	      c1 = scheme_tofold(c0);
	      break;
	    case 1:
	      c1 = scheme_tolower(c0);
	      break;
	    case 2:
	      c1 = scheme_toupper(c0);
	      break;
	    case 3:
	    default:
	      c1 = scheme_totitle(c0);
	      break;
	    }
	    for (ci = 0; ci < num_wrote; ci++) {
	      if (c1 == wrote[ci])
		break;
	    }
	    if (ci >= num_wrote) {
	      wrote[num_wrote++] = c1;
	      us[0] = c1;
	      clen = scheme_utf8_encode(us, 0, 1, s2, 0, 0);
	      r = make_room(r, j, clen + 1, &rs);
	      r[j++] = '|';
	      for (ci = 0; ci < clen; ci++) {
		r[j++] = s2[ci];
	      }
	    }
	  }
	}
	r[j++] = ')';
      } else {
	/* No translation. */
	while (rs.i < k) {
	  r[j++] = s[rs.i++];
	}
      }
    } else {
      /* The translation needs to know about case-insensitive
	 and single-line modes, so track parens: */
      if (s[rs.i] == '(') {
	int old_flags = parse_flags;
	if ((rs.i + 1 < len) && (s[rs.i + 1] == '?')) {
	  int k;
	  for (k = rs.i + 2; k < len; k++) {
	    if ((s[k] == ':')
		|| (s[k] == '<')
		|| (s[k] == '>')
		|| (s[k] == '=')
		|| (s[k] == '!'))
	      break;
	    if (s[k] == 'i') {
	      parse_flags &= ~PARSE_CASE_SENS;
	    } else if (s[k] == 's') {
	      parse_flags |= PARSE_SINGLE_LINE;
	    } else if (s[k] == 'm') {
	      parse_flags &= ~PARSE_SINGLE_LINE;
	    } else if (s[k] == '-') {
	      if (k + 1 < len) {
		k++;
		if (s[k] == 'i') {
		  parse_flags |= PARSE_CASE_SENS;
		} else if (s[k] == 's') {
		  parse_flags &= ~PARSE_SINGLE_LINE;
		} else if (s[k] == 'm') {
		  parse_flags |= PARSE_SINGLE_LINE;
		}
	      }
	    }
	  }
	}
	if (parse_params || (parse_flags != old_flags)) {
	  parse_params = scheme_make_raw_pair(scheme_make_integer(old_flags),
					      parse_params);
	}
      } else if (s[rs.i] == ')') {
	if (parse_params) {
	  parse_flags = SCHEME_INT_VAL(SCHEME_CAR(parse_params));
	  parse_params = SCHEME_CDR(parse_params);
	}
      }
      r[j++] = s[rs.i++];
    }
  }

  r[j] = 0;
  *result = (char *)r;
  return j;
}

/************************************************************/
/*                   Racket front end                       */
/************************************************************/

int scheme_is_pregexp(Scheme_Object *o)
{
  return !!(((regexp *)o)->flags & REGEXP_IS_PCRE);
}

static Scheme_Object *do_make_regexp(const char *who, int is_byte, int pcre, int argc, Scheme_Object *argv[])
{
  Scheme_Object *re, *bs;
  char *s;
  int slen;

  if (is_byte) {
    if (!SCHEME_BYTE_STRINGP(argv[0]))
      scheme_wrong_contract(who, "byte?", 0, argc, argv);
    bs = argv[0];
  } else {
    if (!SCHEME_CHAR_STRINGP(argv[0]))
      scheme_wrong_contract(who, "string?", 0, argc, argv);
    bs = scheme_char_string_to_byte_string(argv[0]);
  }

  s = SCHEME_BYTE_STR_VAL(bs);
  slen = SCHEME_BYTE_STRTAG_VAL(bs);

  if (!is_byte) {
    slen = translate((unsigned char *)s, slen, &s, pcre);
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
      printf("%d %s\n", slen, scheme_write_to_string(scheme_make_byte_string(cp), 0));
    }
#endif
  }

  re = (Scheme_Object *)regcomp(s, 0, slen, pcre);

  if (!is_byte)
    ((regexp *)re)->flags |= REGEXP_IS_UTF8;
  if (pcre)
    ((regexp *)re)->flags |= REGEXP_IS_PCRE;

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
  return do_make_regexp("byte-regexp", 1, 0, argc, argv);
}

static Scheme_Object *make_utf8_regexp(int argc, Scheme_Object *argv[])
{
  return do_make_regexp("regexp", 0, 0, argc, argv);
}

static Scheme_Object *make_pregexp(int argc, Scheme_Object *argv[])
{
  return do_make_regexp("byte-pregexp", 1, 1, argc, argv);
}

static Scheme_Object *make_utf8_pregexp(int argc, Scheme_Object *argv[])
{
  return do_make_regexp("pregexp", 0, 1, argc, argv);
}

Scheme_Object *scheme_make_regexp(Scheme_Object *str, int is_byte, int pcre, int * volatile result_is_err_string)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Object * volatile result;

  *result_is_err_string = 0;

  /* we rely on single-threaded, non-blocking regexp compilation: */
  save = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  failure_msg_for_read = "yes";
  if (!scheme_setjmp(newbuf)) {
    if (is_byte) {
      if (pcre)
	result = make_pregexp(1, &str);
      else
	result = make_regexp(1, &str);
    } else {
      if (pcre)
	result = make_utf8_pregexp(1, &str);
      else
	result = make_utf8_regexp(1, &str);
    }
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

void scheme_clear_rx_buffers(void)
{
  startp_buffer_cache = NULL;
  endp_buffer_cache = NULL;
  maybep_buffer_cache = NULL;
  match_stack_buffer_cache = NULL;
}

static Scheme_Object *gen_compare(char *name, int pos, 
				  int argc, Scheme_Object *argv[],
				  int peek, int nonblock, int last_bytes)
{
  regexp *r;
  char *full_s, *prefix = NULL;
  rxpos *startp, *maybep, *endp, *match_stack, prefix_len = 0, prefix_offset = 0, minpos;
  int offset = 0, orig_offset, endset, m, was_non_byte, last_bytes_count = last_bytes;
  Scheme_Object *iport, *oport = NULL, *startv = NULL, *endv = NULL, *dropped, *unless_evt = NULL;
  Scheme_Object *last_bytes_str = scheme_false, *srcin;
  rx_lazy_str_t *lazy_string = NULL;
  
  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_BYTE_STRINGP(argv[0])
      && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(name, "(or/c regexp? byte-regexp? string? bytes?)", 0, argc, argv);
  if ((peek || (!SCHEME_BYTE_STRINGP(argv[1]) && !SCHEME_CHAR_STRINGP(argv[1])))
      && !SCHEME_INPUT_PORTP(argv[1])
      && !SCHEME_PATHP(argv[1]))
    scheme_wrong_contract(name, peek ? "input-port?" : "(or/c string? bytes? path? input-port?)", 1, argc, argv);
  
  srcin = argv[1];
  if (SCHEME_PATHP(srcin)) {
    if (SCHEME_BYTE_STRINGP(argv[0])
        || (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_regexp_type)
            && !(((regexp *)argv[0])->flags & REGEXP_IS_UTF8)))
      srcin = scheme_make_sized_byte_string(SCHEME_PATH_VAL(srcin),
                                            SCHEME_PATH_LEN(srcin),
                                            1);
    else
      srcin = scheme_path_to_char_string(srcin);
  }

  if (SCHEME_CHAR_STRINGP(srcin)) {
    iport = NULL;
    endset = SCHEME_CHAR_STRLEN_VAL(srcin);
  } else if (SCHEME_INPUT_PORTP(srcin)) {
    iport = srcin;
    endset = -2;
  } else {
    iport = NULL;
    endset = SCHEME_BYTE_STRLEN_VAL(srcin);
  }

  if (argc > 2) {
    int len = endset;

    offset = scheme_extract_index(name, 2, argc, argv, len + 1, 0);

    if (!iport && (offset > len)) {
      scheme_out_of_range(name, NULL, "offset ", argv[2], srcin, 0, len);
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
            scheme_contract_error(name,
                                  "ending index is smaller than starting index",
                                  "starting index", 1, argv[2],
                                  "ending index", 1, argv[3],
                                  NULL);
	    return NULL;
	  }
	} else if (endset < offset || endset > len) {
	  scheme_out_of_range(name, NULL, "ending ", argv[3], srcin, offset, len);
	  return NULL;
	}
	endv = argv[3];
      }
      
      if (argc > 4) {
	if (peek) {
	  if (!SCHEME_FALSEP(argv[4])) {
	    unless_evt = argv[4];
	    if (!SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type)) {
	      scheme_wrong_contract(name, "(or/c progress-evt? #f)", 4, argc, argv);
	      return NULL;
	    }
	    if (!iport) {
	      scheme_contract_error(name, 
                                    "progress evt cannot be used with string input",
                                    "progress evt", 1, unless_evt,
                                    NULL);
	    } else if (!SAME_OBJ(iport, SCHEME_PTR1_VAL(unless_evt))) {
	      scheme_contract_error(name,
                                    "evt is not a progress evt for the given port",
                                    "progress evt", 1, unless_evt,
                                    "port", 1, iport,
                                    NULL);
	      return NULL;
	    }
	  }
	} else {
	  if (SCHEME_TRUEP(argv[4])) {
	    if (!SCHEME_OUTPUT_PORTP(argv[4]))
	      scheme_wrong_contract(name, "(or/c output-port? #f)", 4, argc, argv);
	    oport = argv[4];
	  }
	}
      }

      if (argc > 5) {
        if (!SCHEME_BYTE_STRINGP(argv[5]))
          scheme_wrong_contract(name, "bytes?", 5, argc, argv);
        prefix = SCHEME_BYTE_STR_VAL(argv[5]);
        prefix_len = SCHEME_BYTE_STRLEN_VAL(argv[5]);
        prefix_offset = 0;

        if (argc > 6) {
          if (!scheme_nonneg_exact_p(argv[6]))
            scheme_wrong_contract(name, "exact-nonnegative-integer?", 6, argc, argv);
          if (SCHEME_INTP(argv[6]))
            last_bytes_count = SCHEME_INT_VAL(argv[6]);
          else
            last_bytes_count = -1; /* => as many as available */
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
    if (SCHEME_BYTE_STRINGP(srcin))
      full_s = SCHEME_BYTE_STR_VAL(srcin);
    else {
      /* Extract substring and UTF-8 encode: */
      if (endset - offset < LAZY_STRING_CHUNK_SIZE) {
        /* String is short enough to decode in one go: */
        int blen;
        blen = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(srcin), offset, endset,
                                  NULL, 0,
                                  0 /* not UTF-16 */);
        full_s = (char *)scheme_malloc_atomic(blen);
        scheme_utf8_encode(SCHEME_CHAR_STR_VAL(srcin), offset, endset,
                           (unsigned char *)full_s, 0,
                           0 /* not UTF-16 */);
        orig_offset = offset;
        offset = 0;
        endset = blen;
      } else {
        /* Handle extremely long strings by decoding lazily: */
        lazy_string = MALLOC_ONE_RT(rx_lazy_str_t);
#ifdef MZTAG_REQUIRED
        lazy_string->type = scheme_rt_rx_lazy_string;
#endif
        lazy_string->start = offset;
        lazy_string->end = endset;
        lazy_string->done = 0;
        lazy_string->blen = 0;
        lazy_string->s = NULL;
        lazy_string->chars = SCHEME_CHAR_STR_VAL(srcin);
        full_s = NULL;
        orig_offset = offset;
        offset = 0;
        endset = 0;
      }
      if (r->flags & REGEXP_IS_UTF8)
	was_non_byte = 1;
      else {
	/* Convert orig_offset into encoded bytes */
	orig_offset = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(srcin), 0, orig_offset,
					 NULL, 0,
					 0);
      }
    }
  } else
    full_s = NULL;

  if (startp_buffer_cache && (r->nsubexp <= rx_buffer_size)) {
    startp = startp_buffer_cache;
    maybep = maybep_buffer_cache;
    endp = endp_buffer_cache;
    startp_buffer_cache = NULL;
  } else {
    startp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
    maybep = NULL;
    match_stack = NULL;
    endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  }
  if ((r->nsubexp > 1) && !maybep) {
    maybep = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

    if (match_stack_buffer_cache) {
      match_stack = match_stack_buffer_cache;
      match_stack_buffer_cache = NULL;
    } else
      match_stack = MALLOC_N_ATOMIC(rxpos, MATCH_STACK_SIZE);
  } else {
    match_stack = NULL;
  }

  dropped = scheme_make_integer(0);

  m = regexec(name, r, full_s, offset, endset - offset, offset, lazy_string,
              startp, maybep, endp, match_stack,
	      iport, unless_evt, nonblock,
	      &full_s, peek, pos, last_bytes_count, oport, 
	      startv, endv, &dropped, 
              prefix, prefix_len, prefix_offset);

  if (lazy_string) {
    full_s = lazy_string->s;
    endset = lazy_string->end - lazy_string->start;
  }

  if (iport) {
    minpos = -prefix_len;
    offset = 0;
  } else
    minpos = offset - prefix_len;

  if (m) {
    int i;
    Scheme_Object *l = scheme_null, *rs;

    if (oport && !iport)
      scheme_put_byte_string(name, oport, full_s, 0, *startp, 0);

    if (last_bytes) {
      rxpos frompos, tooffset;

      if ((last_bytes_count < 0)
          || (endp[0] - minpos < last_bytes_count))
        last_bytes_count = endp[0] - minpos;

      if (!last_bytes_count) {
        last_bytes_str = empty_byte_string;
      } else {
        frompos = endp[0] - last_bytes_count;
        tooffset = 0;

        last_bytes_str = scheme_alloc_byte_string(last_bytes_count, 0);
        if (frompos < offset) {
          /* draw from prefix: */
          rxpos amt = last_bytes_count;
          if (frompos + last_bytes_count > offset)
            amt = offset - frompos;
          memcpy(SCHEME_BYTE_STR_VAL(last_bytes_str) XFORM_OK_PLUS tooffset,
                 prefix + prefix_offset + prefix_len - (offset - frompos),
                 amt);
          frompos += amt;
          tooffset += amt;
          last_bytes_count -= amt;
        }
        memcpy(SCHEME_BYTE_STR_VAL(last_bytes_str) XFORM_OK_PLUS tooffset,
               full_s + frompos,
               last_bytes_count);
      }
    }

    if (pos > 1) {
      /* pos == 2 => just get true or false */
      dropped = scheme_true;
    } else {
      for (i = r->nsubexp; i--; ) {
	if (startp[i] >= minpos) {
	  if (pos) {
	    Scheme_Object *startpd, *endpd;

	    if (was_non_byte) {
	      /* Need to figure out how startpd and endpd correspond to
		 code points. Note that the input regexp matches only
		 unicode chars, so the start and end points can't be in
		 the middle of encoded characters. */
	      int uspd, uepd;
              if (startp[i] >= offset)
                uspd = scheme_utf8_decode((const unsigned char *)full_s, offset, startp[i],
                                          NULL, 0, -1,
                                          NULL, 0, 0);
              else {
                uspd = scheme_utf8_decode((const unsigned char *)prefix, 
                                          prefix_offset + prefix_len + (startp[i] - offset), 
                                          prefix_offset + prefix_len,
                                          NULL, 0, -1,
                                          NULL, 0, 0);
                uspd = offset - uspd;
              }
	      uspd += orig_offset;
	      startpd = scheme_make_integer(uspd);
              if (startp[i] >= offset) {
                uepd = scheme_utf8_decode((const unsigned char *)full_s, startp[i], endp[i],
                                          NULL, 0, -1,
                                          NULL, 0, 0);
                uepd += uspd;
              } else if (endp[i] < offset) {
                uepd = scheme_utf8_decode((const unsigned char *)prefix, 
                                          prefix_offset + prefix_len + (endp[i] - offset),
                                          prefix_offset + prefix_len,
                                          NULL, 0, -1,
                                          NULL, 0, 0);
                uepd = offset - uepd;
                uepd += orig_offset;
              } else {
                scheme_signal_error("internal error: how can a match span both prefix and input?");
                uepd = 0;
              }
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
	    intptr_t len;
	    len = endp[i] - startp[i];
            if (startp[i] >= offset) {
              if (was_non_byte) {
                rs = scheme_make_sized_offset_utf8_string(full_s, startp[i], len);
              } else {
                rs = scheme_make_sized_offset_byte_string(full_s, startp[i], len, 1);
              }
            } else if (endp[i] <= offset) {
              /* all in prefix */
              rs = scheme_make_sized_offset_byte_string(prefix, 
                                                        prefix_offset + (startp[i] - minpos),
                                                        endp[i] - startp[i],
                                                        1);
              if (was_non_byte)
                rs = scheme_byte_string_to_char_string(rs);
            } else {
              /* span both */
              scheme_signal_error("internal error: how can a match span both prefix and input?");
              rs = NULL;
            }
	    l = scheme_make_pair(rs, l);
	  }
	} else
	  l = scheme_make_pair(scheme_false, l);
      }
      dropped = l;
    }
  } else {
    if (oport && !iport)
      scheme_put_byte_string(name, oport, full_s, 0, endset, 0);

    dropped = scheme_false;
    last_bytes_str = scheme_false;
  }
  
  if (!startp_buffer_cache || (r->nsubexp > rx_buffer_size)) {
    rx_buffer_size = r->nsubexp;
    startp_buffer_cache = startp;
    maybep_buffer_cache = maybep;
    endp_buffer_cache = endp;
  } else if (maybep && !maybep_buffer_cache && (r->nsubexp == rx_buffer_size)) {
    maybep_buffer_cache = maybep;
  }
  if (match_stack && !match_stack_buffer_cache)
    match_stack_buffer_cache = match_stack;

  if (last_bytes) {
    Scheme_Object *a[2];
    a[0] = dropped;
    a[1] = last_bytes_str;
    return scheme_values(2, a);
  } else
    return dropped;
}

static Scheme_Object *compare(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match", 0, argc, argv, 0, 0, 0);
}

static Scheme_Object *compare_end(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match/end", 0, argc, argv, 0, 0, 1);
}

static Scheme_Object *positions(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-positions", 1, argc, argv, 0, 0, 0);
}

static Scheme_Object *positions_end(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-positions/end", 1, argc, argv, 0, 0, 1);
}

static Scheme_Object *compare_bool(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match?", 2, argc, argv, 0, 0, 0);
}

int scheme_regexp_match_p(Scheme_Object *regexp, Scheme_Object *target)
{
  Scheme_Object *a[2];
  a[0] = regexp;
  a[1] = target;
  return SCHEME_TRUEP(compare_bool(2, a));
}

static Scheme_Object *compare_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek", 0, argc, argv, 1, 0, 0);
}

static Scheme_Object *positions_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions", 1, argc, argv, 1, 0, 0);
}

static Scheme_Object *positions_peek_end(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions/end", 1, argc, argv, 1, 0, 1);
}

static Scheme_Object *compare_peek_nonblock(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-immediate", 0, argc, argv, 1, 1, 0);
}

static Scheme_Object *positions_peek_nonblock(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions-immediate", 1, argc, argv, 1, 1, 0);
}

static Scheme_Object *positions_peek_nonblock_end(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions-immediate/end", 1, argc, argv, 1, 1, 1);
}

static char *build_call_name(const char *n)
{
  char *m;
  int l;
  l = strlen(n);
  m = (char *)scheme_malloc_atomic(l + 42);
  memcpy(m, n, l);
  strcpy(m XFORM_OK_PLUS l, " (calling given filter procedure)");
  return m;
}

static int initial_char_len(unsigned char *source, intptr_t start, intptr_t end)
{
  intptr_t i;

  for (i = start + 1; i <= end; i++) {
    if (scheme_utf8_decode_count(source, start, i, NULL, 1, 1)) {
      return i - start;
    }
  }

  return 1;
}

static Scheme_Object *gen_replace(const char *name, int argc, Scheme_Object *argv[], int all)
{
  Scheme_Object *orig;
  regexp *r;
  char *source, *prefix = NULL, *deststr;
  rxpos *startp, *maybep, *endp, minpos;
  int prefix_len = 0, prefix_offset = 0, sourcelen, srcoffset = 0, was_non_byte, destlen;

  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_BYTE_STRINGP(argv[0])
      && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(name, "(or/c regexp? byte-regexp? string? bytes?)", 0, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[1])
      && !SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract(name, "(or/c string? bytes?)", 1, argc, argv);
  if (!SCHEME_BYTE_STRINGP(argv[2])
      && !SCHEME_CHAR_STRINGP(argv[2])
      && !SCHEME_PROCP(argv[2]))
    scheme_wrong_contract(name, "(or/c string? bytes? procedure?)", 2, argc, argv);

  if (SCHEME_BYTE_STRINGP(argv[2])) {
    if (SCHEME_CHAR_STRINGP(argv[0])
	|| ((SCHEME_TYPE(argv[0]) == scheme_regexp_type)
	    && (((regexp *)argv[0])->flags & REGEXP_IS_UTF8))) {
      if (SCHEME_CHAR_STRINGP(argv[1])) {
	scheme_contract_error(name, "cannot replace a string with a byte string",
                              "string-matching regexp", 1, argv[0],
                              "byte string", 1, argv[2],
                              NULL);
      }
    }
  }

  if (SCHEME_BYTE_STRINGP(argv[0])
      || SCHEME_CHAR_STRINGP(argv[0]))
    r = regcomp_object(argv[0]);
  else
    r = (regexp *)argv[0];

  if (SCHEME_PROCP(argv[2])) {
    if (!scheme_check_proc_arity(NULL, r->nsubexp, 2, argc, argv)) {
      scheme_contract_error(name,
                            "replace procedure's arity does not include regexp's match count",
                            "regexp", 1, r,
                            "regexp match count", 1, scheme_make_integer(r->nsubexp),
                            "replace procedure", 1, argv[2],
                            NULL);
    }
  }

  if (argc > 3) {
    if (!SCHEME_BYTE_STRINGP(argv[3]))
      scheme_wrong_contract(name, "bytes?", 3, argc, argv);
    prefix = SCHEME_BYTE_STR_VAL(argv[3]);
    prefix_len = SCHEME_BYTE_STRLEN_VAL(argv[3]);
    prefix_offset = 0;
  }

  if (SCHEME_CHAR_STRINGP(argv[1])) {
    orig = scheme_char_string_to_byte_string(argv[1]);
    if (r->flags & REGEXP_IS_UTF8)
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
  if (r->nsubexp > 1)
    maybep = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  else
    maybep = NULL;
  endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

  minpos = -prefix_len;

  while (1) {
    int m;

    do {
      m = regexec(name, r, source, srcoffset, sourcelen - srcoffset, 0, NULL,
                  startp, maybep, endp, NULL,
                  NULL, NULL, 0,
                  NULL, 0, 0, 0, NULL, NULL, NULL, NULL, 
                  prefix, prefix_len, prefix_offset);

      if (m && all && (startp[0] == endp[0])) {
        if (!startp[0] && sourcelen) {
          int amt;

          if (was_non_byte)
            amt = initial_char_len((unsigned char *)source, 0, sourcelen);
          else
            amt = 1;
          
          prefix = scheme_malloc_atomic(amt + 1);
          prefix_len = amt;
          memcpy(prefix, source, amt);
          srcoffset += amt;
          /* try again */
        } else {
          /* if it's the end of the input, the match should fail */
          if (startp[0] == sourcelen)
            m = 0;
          break;
        }
      } else
        break;
    } while (1);

    if (m) {
      char *insert;
      intptr_t len, end, startpd, endpd;

      if (SCHEME_PROCP(argv[2])) {
        int i;
        Scheme_Object *m, **args, *quick_args[5];

	if (r->nsubexp <= 5) {
	  args = quick_args;
	} else {
	  args = MALLOC_N(Scheme_Object*, r->nsubexp);
	}

        for (i = r->nsubexp; i--; ) {
          if (startp[i] < minpos) {
            args[i] = scheme_false;
          } else {
            intptr_t len;
            len = endp[i] - startp[i];
            if (was_non_byte) {
	      m = scheme_make_sized_offset_utf8_string(source, startp[i], len);
              args[i] = m;
            } else {
	      m = scheme_make_sized_offset_byte_string(source, startp[i], len, 1);
              args[i] = m;
            }
          }
        }

        m = _scheme_apply(argv[2], r->nsubexp, args);

	if (!was_non_byte) {
          if (!SCHEME_BYTE_STRINGP(m)) {
	    args[0] = m;
	    scheme_wrong_contract(build_call_name(name), "bytes?", -1, -1, args);
	  }
	  insert = SCHEME_BYTE_STR_VAL(m);
          len = SCHEME_BYTE_STRLEN_VAL(m);
        } else {
	  if (!SCHEME_CHAR_STRINGP(m)) {
	    args[0] = m;
	    scheme_wrong_contract(build_call_name(name), "string?", -1, -1, args);
	  }
          len = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(m), 0,
                                   SCHEME_CHAR_STRLEN_VAL(m),
                                   NULL, 0, 0 /* not UTF-16 */);
          insert = (char *)scheme_malloc_atomic(len);
          scheme_utf8_encode(SCHEME_CHAR_STR_VAL(m), 0,
                             SCHEME_CHAR_STRLEN_VAL(m),
                             (unsigned char *)insert, 0, 0 /* not UTF-16 */);
        }
      } else {
	if (!deststr) {
	  if (SCHEME_CHAR_STRINGP(argv[2])) {
	    Scheme_Object *bs;
	    bs = scheme_char_string_to_byte_string(argv[2]);
	    deststr = SCHEME_BYTE_STR_VAL(bs);
	    destlen = SCHEME_BYTE_STRTAG_VAL(bs);
	  } else {
	    deststr = SCHEME_BYTE_STR_VAL(argv[2]);
	    destlen = SCHEME_BYTE_STRTAG_VAL(argv[2]);
	  }
	}
	insert = regsub(r, deststr, destlen, &len, source, startp, endp, 
                        minpos, prefix, prefix_offset);
      }
      
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
	intptr_t total;
	
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
	intptr_t total;
        int more;

        if (startpd == endpd)  {
          if (was_non_byte)
            more = initial_char_len((unsigned char *)source, startpd, sourcelen);
          else
            more = 1;
        } else
          more = 0;

	total = len + prefix_len + (startpd - srcoffset);
	
	naya = (char *)scheme_malloc_atomic(total + more + 1);
	memcpy(naya, prefix, prefix_len);
	memcpy(naya + prefix_len, source + srcoffset, startpd - srcoffset);
	memcpy(naya + prefix_len + (startpd - srcoffset), insert, len);
        if (more) {
          memcpy(naya + prefix_len + (endpd - srcoffset) + len, source + startpd, more);
          total += more;
        }

	prefix = naya;
	prefix_len = total;

	srcoffset = endpd + more;
      }
    } else if (!prefix) {
      if (was_non_byte)
	return argv[1];
      else
	return orig;
    } else {
      char *result;
      intptr_t total, slen;
      
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
	   && (((regexp *)argv[0])->flags & REGEXP_IS_UTF8))
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *byte_regexp_p(int argc, Scheme_Object *argv[])
{
  return (((SCHEME_TYPE(argv[0]) == scheme_regexp_type) 
	   && !(((regexp *)argv[0])->flags & REGEXP_IS_UTF8))
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *pregexp_p(int argc, Scheme_Object *argv[])
{
  return (((SCHEME_TYPE(argv[0]) == scheme_regexp_type) 
	   && (((regexp *)argv[0])->flags & REGEXP_IS_UTF8)
	   && (((regexp *)argv[0])->flags & REGEXP_IS_PCRE))
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *byte_pregexp_p(int argc, Scheme_Object *argv[])
{
  return (((SCHEME_TYPE(argv[0]) == scheme_regexp_type) 
	   && !(((regexp *)argv[0])->flags & REGEXP_IS_UTF8)
	   && (((regexp *)argv[0])->flags & REGEXP_IS_PCRE))
	  ? scheme_true 
	  : scheme_false);
}

Scheme_Object *regexp_lookbehind(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_regexp_type))
    scheme_wrong_contract("regexp-max-lookbehind", "(or/c regexp? byte-regexp?)", 0, argc, argv);
  return scheme_make_integer(((regexp *)argv[0])->maxlookback);
}

Scheme_Object *scheme_regexp_source(Scheme_Object *re)
{
  return ((regexp *)re)->source;
}

int scheme_regexp_is_byte(Scheme_Object *re)
{
  return !(((regexp *)re)->flags & REGEXP_IS_UTF8);
}

int scheme_regexp_is_pregexp(Scheme_Object *re)
{
  return !!(((regexp *)re)->flags & REGEXP_IS_PCRE);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#include "mzmark_regexp.inc"
END_XFORM_SKIP;
#endif

void scheme_regexp_initialize(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  GC_REG_TRAV(scheme_regexp_type, mark_regexp);
  GC_REG_TRAV(scheme_rt_regwork, mark_regwork);
  GC_REG_TRAV(scheme_rt_rx_lazy_string, mark_lazy_string);
#endif

  REGISTER_SO(empty_byte_string);
  empty_byte_string = scheme_alloc_byte_string(0, 0);

  GLOBAL_PRIM_W_ARITY("byte-regexp",                           make_regexp,             1, 1, env);
  GLOBAL_PRIM_W_ARITY("regexp",                                make_utf8_regexp,        1, 1, env);
  GLOBAL_PRIM_W_ARITY("byte-pregexp",                          make_pregexp,            1, 1, env);
  GLOBAL_PRIM_W_ARITY("pregexp",                               make_utf8_pregexp,       1, 1, env);
  GLOBAL_PRIM_W_ARITY("regexp-match",                          compare,                 2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match/end",                      compare_end,             2, 7, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-positions",                positions,               2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-positions/end",            positions_end,           2, 7, env);
  GLOBAL_PRIM_W_ARITY("regexp-match?",                         compare_bool,            2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek",                     compare_peek,            2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek-positions",           positions_peek,          2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek-positions/end",       positions_peek_end,      2, 7, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek-immediate",           compare_peek_nonblock,   2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek-positions-immediate", positions_peek_nonblock, 2, 6, env);
  GLOBAL_PRIM_W_ARITY("regexp-match-peek-positions-immediate/end", positions_peek_nonblock_end, 2, 7, env);
  GLOBAL_PRIM_W_ARITY("regexp-replace",                        replace,                 3, 4, env);
  GLOBAL_PRIM_W_ARITY("regexp-replace*",                       replace_star,            3, 4, env);

  GLOBAL_FOLDING_PRIM("regexp?",                               regexp_p,        1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("byte-regexp?",                          byte_regexp_p,   1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("pregexp?",                              pregexp_p,       1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("byte-pregexp?",                         byte_pregexp_p,  1, 1, 1, env);

  GLOBAL_FOLDING_PRIM("regexp-max-lookbehind",                 regexp_lookbehind, 1, 1, 1, env);
}

void scheme_init_regexp_places()
{
  REGISTER_SO(regparsestr);
  REGISTER_SO(regstr);
  REGISTER_SO(regbackknown);
  REGISTER_SO(regbackdepends);
}
