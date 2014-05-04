/* libunwind - a platform-independent unwind library
   Copyright (c) 2003-2005 Hewlett-Packard Development Company, L.P.
	Contributed by David Mosberger-Tang <davidm@hpl.hp.com>

This file is several parts of libunwind concatenated.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  */

#include "../../sconfig.h"

#ifdef MZ_USE_DWARF_LIBUNWIND

#include <stddef.h>
#include "libunwind_i.h"

#ifdef CONFIG_DEBUG_FRAME
# include <stdio.h>
# include <unistd.h>
# include <fcntl.h>
# include <sys/mman.h>
# include "os-linux.h"
#endif

#define UNW_PI_FLAG_DEBUG_FRAME        32

#define UNW_UNUSED(x) /* empty*/

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                   region-based memory management                   */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

UNUSED static void *rgn_chain = NULL;

static void *malloc_in_rgn(unw_addr_space_t as, long sz)
{
  void *p;

  /* 16 to ensure alignment, definitely >= sizeof(void*) */
  p = malloc(sz + 16);

  *(void **)p = as->mem_pool;
  as->mem_pool = p;

  return (void *)((char *)p + 16);
}

static void free_all_allocated(unw_addr_space_t as)
{
  while (as->mem_pool) {
    void *next = *(void **)as->mem_pool;
    free(as->mem_pool);
    as->mem_pool = next;
  }
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                             Gexpr.c                                */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

/* The "pick" operator provides an index range of 0..255 indicating
   that the stack could at least have a depth of up to 256 elements,
   but the GCC unwinder restricts the depth to 64, which seems
   reasonable so we use the same value here.  */
#define MAX_EXPR_STACK_SIZE	64

#define NUM_OPERANDS(signature)	(((signature) >> 6) & 0x3)
#define OPND1_TYPE(signature)	(((signature) >> 3) & 0x7)
#define OPND2_TYPE(signature)	(((signature) >> 0) & 0x7)

#define OPND_SIGNATURE(n, t1, t2) (((n) << 6) | ((t1) << 3) | ((t2) << 0))
#define OPND1(t1)		OPND_SIGNATURE(1, t1, 0)
#define OPND2(t1, t2)		OPND_SIGNATURE(2, t1, t2)

#define VAL8	0x0
#define VAL16	0x1
#define VAL32	0x2
#define VAL64	0x3
#define ULEB128	0x4
#define SLEB128	0x5
#define OFFSET	0x6	/* 32-bit offset for 32-bit DWARF, 64-bit otherwise */

static uint8_t operands[256] =
  {
    [DW_OP_addr] =	OPND1 (sizeof (unw_word_t) == 4 ? VAL32 : VAL64),
    [DW_OP_const1u] =		OPND1 (VAL8),
    [DW_OP_const1s] =		OPND1 (VAL8),
    [DW_OP_const2u] =		OPND1 (VAL16),
    [DW_OP_const2s] =		OPND1 (VAL16),
    [DW_OP_const4u] =		OPND1 (VAL32),
    [DW_OP_const4s] =		OPND1 (VAL32),
    [DW_OP_const8u] =		OPND1 (VAL64),
    [DW_OP_const8s] =		OPND1 (VAL64),
    [DW_OP_pick] =		OPND1 (VAL8),
    [DW_OP_plus_uconst] =	OPND1 (ULEB128),
    [DW_OP_skip] =		OPND1 (VAL16),
    [DW_OP_bra] =		OPND1 (VAL16),
    [DW_OP_breg0 +  0] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  1] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  2] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  3] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  4] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  5] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  6] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  7] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  8] =	OPND1 (SLEB128),
    [DW_OP_breg0 +  9] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 10] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 11] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 12] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 13] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 14] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 15] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 16] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 17] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 18] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 19] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 20] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 21] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 22] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 23] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 24] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 25] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 26] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 27] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 28] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 29] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 30] =	OPND1 (SLEB128),
    [DW_OP_breg0 + 31] =	OPND1 (SLEB128),
    [DW_OP_regx] =		OPND1 (ULEB128),
    [DW_OP_fbreg] =		OPND1 (SLEB128),
    [DW_OP_bregx] =		OPND2 (ULEB128, SLEB128),
    [DW_OP_piece] =		OPND1 (ULEB128),
    [DW_OP_deref_size] =	OPND1 (VAL8),
    [DW_OP_xderef_size] =	OPND1 (VAL8),
    [DW_OP_call2] =		OPND1 (VAL16),
    [DW_OP_call4] =		OPND1 (VAL32),
    [DW_OP_call_ref] =		OPND1 (OFFSET)
  };

#define sword(X)	((unw_sword_t) (X))

static inline unw_word_t
read_operand (unw_addr_space_t as, unw_accessors_t *a,
	      unw_word_t *addr, int operand_type, unw_word_t *val, void *arg)
{
  uint8_t u8;
  uint16_t u16;
  uint32_t u32;
  uint64_t u64;
  int ret;

  switch (operand_type)
    {
    case VAL8:
      ret = dwarf_readu8 (as, a, addr, &u8, arg);
      *val = u8;
      break;

    case VAL16:
      ret = dwarf_readu16 (as, a, addr, &u16, arg);
      *val = u16;
      break;

    case VAL32:
      ret = dwarf_readu32 (as, a, addr, &u32, arg);
      *val = u32;
      break;

    case VAL64:
      ret = dwarf_readu64 (as, a, addr, &u64, arg);
      *val = u64;
      break;

    case ULEB128:
      ret = dwarf_read_uleb128 (as, a, addr, val, arg);
      break;

    case SLEB128:
      ret = dwarf_read_sleb128 (as, a, addr, val, arg);
      break;

    case OFFSET: /* only used by DW_OP_call_ref, which we don't implement */
    default:
      Debug (1, "Unexpected operand type %d\n", operand_type);
      ret = -UNW_EINVAL;
    }
  return ret;
}

HIDDEN int
dwarf_eval_expr (struct dwarf_cursor *c, unw_word_t *addr, unw_word_t len,
		 unw_word_t *valp, int *is_register)
{
  unw_word_t operand1 = 0, operand2 = 0, tmp1, tmp2, tmp3, end_addr;
  uint8_t opcode, operands_signature, u8;
  unw_addr_space_t as;
  unw_accessors_t *a;
  void *arg;
  unw_word_t stack[MAX_EXPR_STACK_SIZE];
  unsigned int tos = 0;
  uint16_t u16;
  uint32_t u32;
  uint64_t u64;
  int ret;
# define pop()					\
({						\
  if ((tos - 1) >= MAX_EXPR_STACK_SIZE)		\
    {						\
      Debug (1, "Stack underflow\n");		\
      return -UNW_EINVAL;			\
    }						\
  stack[--tos];					\
})
# define push(x)				\
do {						\
  if (tos >= MAX_EXPR_STACK_SIZE)		\
    {						\
      Debug (1, "Stack overflow\n");		\
      return -UNW_EINVAL;			\
    }						\
  stack[tos++] = (x);				\
} while (0)
# define pick(n)				\
({						\
  unsigned int _index = tos - 1 - (n);		\
  if (_index >= MAX_EXPR_STACK_SIZE)		\
    {						\
      Debug (1, "Out-of-stack pick\n");		\
      return -UNW_EINVAL;			\
    }						\
  stack[_index];				\
})

  as = c->as;
  arg = c->as_arg;
  a = unw_get_accessors (as);
  end_addr = *addr + len;
  *is_register = 0;

  Debug (14, "len=%lu, pushing cfa=0x%lx\n",
	 (unsigned long) len, (unsigned long) c->cfa);

  push (c->cfa);	/* push current CFA as required by DWARF spec */

  while (*addr < end_addr)
    {
      if ((ret = dwarf_readu8 (as, a, addr, &opcode, arg)) < 0)
	return ret;

      operands_signature = operands[opcode];

      if (unlikely (NUM_OPERANDS (operands_signature) > 0))
	{
	  if ((ret = read_operand (as, a, addr,
				   OPND1_TYPE (operands_signature),
				   &operand1, arg)) < 0)
	    return ret;
	  if (NUM_OPERANDS (operands_signature) > 1)
	    if ((ret = read_operand (as, a, addr,
				     OPND2_TYPE (operands_signature),
				     &operand2, arg)) < 0)
	      return ret;
	}

      switch ((dwarf_expr_op_t) opcode)
	{
	case DW_OP_lit0:  case DW_OP_lit1:  case DW_OP_lit2:
	case DW_OP_lit3:  case DW_OP_lit4:  case DW_OP_lit5:
	case DW_OP_lit6:  case DW_OP_lit7:  case DW_OP_lit8:
	case DW_OP_lit9:  case DW_OP_lit10: case DW_OP_lit11:
	case DW_OP_lit12: case DW_OP_lit13: case DW_OP_lit14:
	case DW_OP_lit15: case DW_OP_lit16: case DW_OP_lit17:
	case DW_OP_lit18: case DW_OP_lit19: case DW_OP_lit20:
	case DW_OP_lit21: case DW_OP_lit22: case DW_OP_lit23:
	case DW_OP_lit24: case DW_OP_lit25: case DW_OP_lit26:
	case DW_OP_lit27: case DW_OP_lit28: case DW_OP_lit29:
	case DW_OP_lit30: case DW_OP_lit31:
	  Debug (15, "OP_lit(%d)\n", (int) opcode - DW_OP_lit0);
	  push (opcode - DW_OP_lit0);
	  break;

	case DW_OP_breg0:  case DW_OP_breg1:  case DW_OP_breg2:
	case DW_OP_breg3:  case DW_OP_breg4:  case DW_OP_breg5:
	case DW_OP_breg6:  case DW_OP_breg7:  case DW_OP_breg8:
	case DW_OP_breg9:  case DW_OP_breg10: case DW_OP_breg11:
	case DW_OP_breg12: case DW_OP_breg13: case DW_OP_breg14:
	case DW_OP_breg15: case DW_OP_breg16: case DW_OP_breg17:
	case DW_OP_breg18: case DW_OP_breg19: case DW_OP_breg20:
	case DW_OP_breg21: case DW_OP_breg22: case DW_OP_breg23:
	case DW_OP_breg24: case DW_OP_breg25: case DW_OP_breg26:
	case DW_OP_breg27: case DW_OP_breg28: case DW_OP_breg29:
	case DW_OP_breg30: case DW_OP_breg31:
	  Debug (15, "OP_breg(r%d,0x%lx)\n",
		 (int) opcode - DW_OP_breg0, (unsigned long) operand1);
	  if ((ret = unw_get_reg (dwarf_to_cursor (c),
				  dwarf_to_unw_regnum (opcode - DW_OP_breg0),
				  &tmp1)) < 0)
	    return ret;
	  push (tmp1 + operand1);
	  break;

	case DW_OP_bregx:
	  Debug (15, "OP_bregx(r%d,0x%lx)\n",
		 (int) operand1, (unsigned long) operand2);
	  if ((ret = unw_get_reg (dwarf_to_cursor (c),
				  dwarf_to_unw_regnum (operand1), &tmp1)) < 0)
	    return ret;
	  push (tmp1 + operand2);
	  break;

	case DW_OP_reg0:  case DW_OP_reg1:  case DW_OP_reg2:
	case DW_OP_reg3:  case DW_OP_reg4:  case DW_OP_reg5:
	case DW_OP_reg6:  case DW_OP_reg7:  case DW_OP_reg8:
	case DW_OP_reg9:  case DW_OP_reg10: case DW_OP_reg11:
	case DW_OP_reg12: case DW_OP_reg13: case DW_OP_reg14:
	case DW_OP_reg15: case DW_OP_reg16: case DW_OP_reg17:
	case DW_OP_reg18: case DW_OP_reg19: case DW_OP_reg20:
	case DW_OP_reg21: case DW_OP_reg22: case DW_OP_reg23:
	case DW_OP_reg24: case DW_OP_reg25: case DW_OP_reg26:
	case DW_OP_reg27: case DW_OP_reg28: case DW_OP_reg29:
	case DW_OP_reg30: case DW_OP_reg31:
	  Debug (15, "OP_reg(r%d)\n", (int) opcode - DW_OP_reg0);
	  *valp = dwarf_to_unw_regnum (opcode - DW_OP_reg0);
	  *is_register = 1;
	  return 0;

	case DW_OP_regx:
	  Debug (15, "OP_regx(r%d)\n", (int) operand1);
	  *valp = dwarf_to_unw_regnum (operand1);
	  *is_register = 1;
	  return 0;

	case DW_OP_addr:
	case DW_OP_const1u:
	case DW_OP_const2u:
	case DW_OP_const4u:
	case DW_OP_const8u:
	case DW_OP_constu:
	case DW_OP_const8s:
	case DW_OP_consts:
	  Debug (15, "OP_const(0x%lx)\n", (unsigned long) operand1);
	  push (operand1);
	  break;

	case DW_OP_const1s:
	  if (operand1 & 0x80)
	    operand1 |= ((unw_word_t) -1) << 8;
	  Debug (15, "OP_const1s(%ld)\n", (long) operand1);
	  push (operand1);
	  break;

	case DW_OP_const2s:
	  if (operand1 & 0x8000)
	    operand1 |= ((unw_word_t) -1) << 16;
	  Debug (15, "OP_const2s(%ld)\n", (long) operand1);
	  push (operand1);
	  break;

	case DW_OP_const4s:
	  if (operand1 & 0x80000000)
	    operand1 |= (((unw_word_t) -1) << 16) << 16;
	  Debug (15, "OP_const4s(%ld)\n", (long) operand1);
	  push (operand1);
	  break;

	case DW_OP_deref:
	  Debug (15, "OP_deref\n");
	  tmp1 = pop ();
	  if ((ret = dwarf_readw (as, a, &tmp1, &tmp2, arg)) < 0)
	    return ret;
	  push (tmp2);
	  break;

	case DW_OP_deref_size:
	  Debug (15, "OP_deref_size(%d)\n", (int) operand1);
	  tmp1 = pop ();
	  switch (operand1)
	    {
            default:
	    case 0:
              tmp2 = 0;
	      break;

	    case 1:
	      if ((ret = dwarf_readu8 (as, a, &tmp1, &u8, arg)) < 0)
		return ret;
	      tmp2 = u8;
	      break;

	    case 2:
	      if ((ret = dwarf_readu16 (as, a, &tmp1, &u16, arg)) < 0)
		return ret;
	      tmp2 = u16;
	      break;

	    case 3:
	    case 4:
	      if ((ret = dwarf_readu32 (as, a, &tmp1, &u32, arg)) < 0)
		return ret;
	      tmp2 = u32;
	      if (operand1 == 3)
		{
		  if (dwarf_is_big_endian (as))
		    tmp2 >>= 8;
		  else
		    tmp2 &= 0xffffff;
		}
	      break;
	    case 5:
	    case 6:
	    case 7:
	    case 8:
	      if ((ret = dwarf_readu64 (as, a, &tmp1, &u64, arg)) < 0)
		return ret;
	      tmp2 = u64;
	      if (operand1 != 8)
		{
		  if (dwarf_is_big_endian (as))
		    tmp2 >>= 64 - 8 * operand1;
		  else
		    tmp2 &= (~ (unw_word_t) 0) << (8 * operand1);
		}
	      break;
	    }
	  push (tmp2);
	  break;

	case DW_OP_dup:
	  Debug (15, "OP_dup\n");
	  push (pick (0));
	  break;

	case DW_OP_drop:
	  Debug (15, "OP_drop\n");
	  pop ();
	  break;

	case DW_OP_pick:
	  Debug (15, "OP_pick(%d)\n", (int) operand1);
	  push (pick (operand1));
	  break;

	case DW_OP_over:
	  Debug (15, "OP_over\n");
	  push (pick (1));
	  break;

	case DW_OP_swap:
	  Debug (15, "OP_swap\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp1);
	  push (tmp2);
	  break;

	case DW_OP_rot:
	  Debug (15, "OP_rot\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  tmp3 = pop ();
	  push (tmp1);
	  push (tmp3);
	  push (tmp2);
	  break;

	case DW_OP_abs:
	  Debug (15, "OP_abs\n");
	  tmp1 = pop ();
	  if (tmp1 & ((unw_word_t) 1 << (8 * sizeof (unw_word_t) - 1)))
	    tmp1 = -tmp1;
	  push (tmp1);
	  break;

	case DW_OP_and:
	  Debug (15, "OP_and\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp1 & tmp2);
	  break;

	case DW_OP_div:
	  Debug (15, "OP_div\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  if (tmp1)
	    tmp1 = sword (tmp2) / sword (tmp1);
	  push (tmp1);
	  break;

	case DW_OP_minus:
	  Debug (15, "OP_minus\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  tmp1 = tmp2 - tmp1;
	  push (tmp1);
	  break;

	case DW_OP_mod:
	  Debug (15, "OP_mod\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  if (tmp1)
	    tmp1 = tmp2 % tmp1;
	  push (tmp1);
	  break;

	case DW_OP_mul:
	  Debug (15, "OP_mul\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  if (tmp1)
	    tmp1 = tmp2 * tmp1;
	  push (tmp1);
	  break;

	case DW_OP_neg:
	  Debug (15, "OP_neg\n");
	  push (-pop ());
	  break;

	case DW_OP_not:
	  Debug (15, "OP_not\n");
	  push (~pop ());
	  break;

	case DW_OP_or:
	  Debug (15, "OP_or\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp1 | tmp2);
	  break;

	case DW_OP_plus:
	  Debug (15, "OP_plus\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp1 + tmp2);
	  break;

	case DW_OP_plus_uconst:
	  Debug (15, "OP_plus_uconst(%lu)\n", (unsigned long) operand1);
	  tmp1 = pop ();
	  push (tmp1 + operand1);
	  break;

	case DW_OP_shl:
	  Debug (15, "OP_shl\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp2 << tmp1);
	  break;

	case DW_OP_shr:
	  Debug (15, "OP_shr\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp2 >> tmp1);
	  break;

	case DW_OP_shra:
	  Debug (15, "OP_shra\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp2) >> tmp1);
	  break;

	case DW_OP_xor:
	  Debug (15, "OP_xor\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (tmp1 ^ tmp2);
	  break;

	case DW_OP_le:
	  Debug (15, "OP_le\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) <= sword (tmp2));
	  break;

	case DW_OP_ge:
	  Debug (15, "OP_ge\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) >= sword (tmp2));
	  break;

	case DW_OP_eq:
	  Debug (15, "OP_eq\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) == sword (tmp2));
	  break;

	case DW_OP_lt:
	  Debug (15, "OP_lt\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) < sword (tmp2));
	  break;

	case DW_OP_gt:
	  Debug (15, "OP_gt\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) > sword (tmp2));
	  break;

	case DW_OP_ne:
	  Debug (15, "OP_ne\n");
	  tmp1 = pop ();
	  tmp2 = pop ();
	  push (sword (tmp1) != sword (tmp2));
	  break;

	case DW_OP_skip:
	  Debug (15, "OP_skip(%d)\n", (int16_t) operand1);
	  *addr += (int16_t) operand1;
	  break;

	case DW_OP_bra:
	  Debug (15, "OP_skip(%d)\n", (int16_t) operand1);
	  tmp1 = pop ();
	  if (tmp1)
	    *addr += (int16_t) operand1;
	  break;

	case DW_OP_nop:
	  Debug (15, "OP_nop\n");
	  break;

	case DW_OP_call2:
	case DW_OP_call4:
	case DW_OP_call_ref:
	case DW_OP_fbreg:
	case DW_OP_piece:
	case DW_OP_push_object_address:
	case DW_OP_xderef:
	case DW_OP_xderef_size:
	default:
	  Debug (1, "Unexpected opcode 0x%x\n", opcode);
	  return -UNW_EINVAL;
	}
    }
  *valp = pop ();
  Debug (14, "final value = 0x%lx\n", (unsigned long) *valp);
  return 0;
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                              Gfde.c                                */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

static inline int
is_cie_id (unw_word_t val, int is_debug_frame)
{
  /* The CIE ID is normally 0xffffffff (for 32-bit ELF) or
     0xffffffffffffffff (for 64-bit ELF).  However, .eh_frame
     uses 0.  */
  if (is_debug_frame)
    return (val == - (uint32_t) 1 || val == - (uint64_t) 1);
  else
    return (val == 0);
}

/* Note: we don't need to keep track of more than the first four
   characters of the augmentation string, because we (a) ignore any
   augmentation string contents once we find an unrecognized character
   and (b) those characters that we do recognize, can't be
   repeated.  */
static inline int
parse_cie (unw_addr_space_t as, unw_accessors_t *a, unw_word_t addr,
	   const unw_proc_info_t *pi, struct dwarf_cie_info *dci,
	   unw_word_t base, void *arg)
{
  uint8_t version, ch, augstr[5], fde_encoding, handler_encoding;
  unw_word_t len, cie_end_addr, aug_size;
  uint32_t u32val;
  uint64_t u64val;
  size_t i;
  int ret;
# define STR2(x)	#x
# define STR(x)		STR2(x)

  /* Pick appropriate default for FDE-encoding.  DWARF spec says
     start-IP (initial_location) and the code-size (address_range) are
     "address-unit sized constants".  The `R' augmentation can be used
     to override this, but by default, we pick an address-sized unit
     for fde_encoding.  */
  switch (sizeof (unw_word_t))
    {
    case 4:	fde_encoding = DW_EH_PE_udata4; break;
    case 8:	fde_encoding = DW_EH_PE_udata8; break;
    default:	fde_encoding = DW_EH_PE_omit; break;
    }

  dci->lsda_encoding = DW_EH_PE_omit;
  dci->handler = 0;

  if ((ret = dwarf_readu32 (as, a, &addr, &u32val, arg)) < 0)
    return ret;

  if (u32val != 0xffffffff)
    {
      /* the CIE is in the 32-bit DWARF format */
      uint32_t cie_id;
      /* DWARF says CIE id should be 0xffffffff, but in .eh_frame, it's 0 */
      const uint32_t expected_id = (base) ? 0xffffffff : 0;

      len = u32val;
      cie_end_addr = addr + len;
      if ((ret = dwarf_readu32 (as, a, &addr, &cie_id, arg)) < 0)
	return ret;
      if (cie_id != expected_id)
	{
	  Debug (1, "Unexpected CIE id %x\n", cie_id);
	  return -UNW_EINVAL;
	}
    }
  else
    {
      /* the CIE is in the 64-bit DWARF format */
      uint64_t cie_id;
      /* DWARF says CIE id should be 0xffffffffffffffff, but in
	 .eh_frame, it's 0 */
      const uint64_t expected_id = (base) ? 0xffffffffffffffffull : 0;

      if ((ret = dwarf_readu64 (as, a, &addr, &u64val, arg)) < 0)
	return ret;
      len = u64val;
      cie_end_addr = addr + len;
      if ((ret = dwarf_readu64 (as, a, &addr, &cie_id, arg)) < 0)
	return ret;
      if (cie_id != expected_id)
	{
	  Debug (1, "Unexpected CIE id %llx\n", (long long) cie_id);
	  return -UNW_EINVAL;
	}
    }
  dci->cie_instr_end = cie_end_addr;

  if ((ret = dwarf_readu8 (as, a, &addr, &version, arg)) < 0)
    return ret;

  if (version != 1 && version != DWARF_CIE_VERSION)
    {
      Debug (1, "Got CIE version %u, expected version 1 or "
	     STR (DWARF_CIE_VERSION) "\n", version);
      return -UNW_EBADVERSION;
    }

  /* read and parse the augmentation string: */
  memset (augstr, 0, sizeof (augstr));
  for (i = 0;;)
    {
      if ((ret = dwarf_readu8 (as, a, &addr, &ch, arg)) < 0)
	return ret;

      if (!ch)
	break;	/* end of augmentation string */

      if (i < sizeof (augstr) - 1)
	augstr[i++] = ch;
    }

  if ((ret = dwarf_read_uleb128 (as, a, &addr, &dci->code_align, arg)) < 0
      || (ret = dwarf_read_sleb128 (as, a, &addr, &dci->data_align, arg)) < 0)
    return ret;

  /* Read the return-address column either as a u8 or as a uleb128.  */
  if (version == 1)
    {
      if ((ret = dwarf_readu8 (as, a, &addr, &ch, arg)) < 0)
	return ret;
      dci->ret_addr_column = ch;
    }
  else if ((ret = dwarf_read_uleb128 (as, a, &addr, &dci->ret_addr_column,
				      arg)) < 0)
    return ret;

  i = 0;
  if (augstr[0] == 'z')
    {
      dci->sized_augmentation = 1;
      if ((ret = dwarf_read_uleb128 (as, a, &addr, &aug_size, arg)) < 0)
	return ret;
      i++;
    }

  for (; i < sizeof (augstr) && augstr[i]; ++i)
    switch (augstr[i])
      {
      case 'L':
	/* read the LSDA pointer-encoding format.  */
	if ((ret = dwarf_readu8 (as, a, &addr, &ch, arg)) < 0)
	  return ret;
	dci->lsda_encoding = ch;
	break;

      case 'R':
	/* read the FDE pointer-encoding format.  */
	if ((ret = dwarf_readu8 (as, a, &addr, &fde_encoding, arg)) < 0)
	  return ret;
	break;

      case 'P':
	/* read the personality-routine pointer-encoding format.  */
	if ((ret = dwarf_readu8 (as, a, &addr, &handler_encoding, arg)) < 0)
	  return ret;
	if ((ret = dwarf_read_encoded_pointer (as, a, &addr, handler_encoding,
					       pi, &dci->handler, arg)) < 0)
	  return ret;
	break;

      case 'S':
	/* This is a signal frame. */
	dci->signal_frame = 1;

	/* Temporarily set it to one so dwarf_parse_fde() knows that
	   it should fetch the actual ABI/TAG pair from the FDE.  */
	dci->have_abi_marker = 1;
	break;

      default:
	Debug (1, "Unexpected augmentation string `%s'\n", augstr);
	if (dci->sized_augmentation)
	  /* If we have the size of the augmentation body, we can skip
	     over the parts that we don't understand, so we're OK. */
	  goto done;
	else
	  return -UNW_EINVAL;
      }
 done:
  dci->fde_encoding = fde_encoding;
  dci->cie_instr_start = addr;
  Debug (15, "CIE parsed OK, augmentation = \"%s\", handler=0x%lx\n",
	 augstr, (long) dci->handler);
  return 0;
}

/* Extract proc-info from the FDE starting at adress ADDR.
   
   Pass BASE as zero for eh_frame behaviour, or a pointer to
   debug_frame base for debug_frame behaviour.  */

HIDDEN int
dwarf_extract_proc_info_from_fde (unw_addr_space_t as, unw_accessors_t *a,
				  unw_word_t *addrp, unw_proc_info_t *pi,
				  int need_unwind_info, unw_word_t base,
				  void *arg)
{
  unw_word_t fde_end_addr, cie_addr, cie_offset_addr, aug_end_addr = 0;
  unw_word_t start_ip, ip_range, aug_size, addr = *addrp;
  int ret, ip_range_encoding;
  struct dwarf_cie_info dci;
  uint64_t u64val;
  uint32_t u32val;

  Debug (12, "FDE @ 0x%lx\n", (long) addr);

  memset (&dci, 0, sizeof (dci));

  if ((ret = dwarf_readu32 (as, a, &addr, &u32val, arg)) < 0)
    return ret;

  if (u32val != 0xffffffff)
    {
      int32_t cie_offset;

      /* In some configurations, an FDE with a 0 length indicates the
	 end of the FDE-table.  */
      if (u32val == 0)
	return -UNW_ENOINFO;

      /* the FDE is in the 32-bit DWARF format */

      *addrp = fde_end_addr = addr + u32val;
      cie_offset_addr = addr;

      if ((ret = dwarf_reads32 (as, a, &addr, &cie_offset, arg)) < 0)
	return ret;

      if (is_cie_id (cie_offset, base != 0))
	/* ignore CIEs (happens during linear searches) */
	return 0;

      if (base != 0)
        cie_addr = base + cie_offset;
      else
	/* DWARF says that the CIE_pointer in the FDE is a
	   .debug_frame-relative offset, but the GCC-generated .eh_frame
	   sections instead store a "pcrelative" offset, which is just
	   as fine as it's self-contained.  */
	cie_addr = cie_offset_addr - cie_offset;
    }
  else
    {
      int64_t cie_offset;

      /* the FDE is in the 64-bit DWARF format */

      if ((ret = dwarf_readu64 (as, a, &addr, &u64val, arg)) < 0)
	return ret;

      *addrp = fde_end_addr = addr + u64val;
      cie_offset_addr = addr;

      if ((ret = dwarf_reads64 (as, a, &addr, &cie_offset, arg)) < 0)
	return ret;

      if (is_cie_id (cie_offset, base != 0))
	/* ignore CIEs (happens during linear searches) */
	return 0;

      if (base != 0)
	cie_addr = base + cie_offset;
      else
	/* DWARF says that the CIE_pointer in the FDE is a
	   .debug_frame-relative offset, but the GCC-generated .eh_frame
	   sections instead store a "pcrelative" offset, which is just
	   as fine as it's self-contained.  */
	cie_addr = (unw_word_t) ((uint64_t) cie_offset_addr - cie_offset);
    }

  Debug (15, "looking for CIE at address %lx\n", (long) cie_addr);

  if ((ret = parse_cie (as, a, cie_addr, pi, &dci, base, arg)) < 0)
    return ret;

  /* IP-range has same encoding as FDE pointers, except that it's
     always an absolute value: */
  ip_range_encoding = dci.fde_encoding & DW_EH_PE_FORMAT_MASK;

  if ((ret = dwarf_read_encoded_pointer (as, a, &addr, dci.fde_encoding,
					 pi, &start_ip, arg)) < 0
      || (ret = dwarf_read_encoded_pointer (as, a, &addr, ip_range_encoding,
					    pi, &ip_range, arg)) < 0)
    return ret;
  pi->start_ip = start_ip;
  pi->end_ip = start_ip + ip_range;
  pi->handler = dci.handler;

  if (dci.sized_augmentation)
    {
      if ((ret = dwarf_read_uleb128 (as, a, &addr, &aug_size, arg)) < 0)
	return ret;
      aug_end_addr = addr + aug_size;
    }

  if ((ret = dwarf_read_encoded_pointer (as, a, &addr, dci.lsda_encoding,
					 pi, &pi->lsda, arg)) < 0)
    return ret;

  Debug (15, "FDE covers IP 0x%lx-0x%lx, LSDA=0x%lx\n",
	 (long) pi->start_ip, (long) pi->end_ip, (long) pi->lsda);

  if (need_unwind_info)
    {
      pi->format = UNW_INFO_FORMAT_TABLE;
      pi->unwind_info_size = sizeof (dci);
      pi->unwind_info = malloc_in_rgn(as, sizeof(struct dwarf_cie_info));
      if (!pi->unwind_info)
	return -UNW_ENOMEM;

      if (dci.have_abi_marker)
	{
	  if ((ret = dwarf_readu16 (as, a, &addr, &dci.abi, arg)) < 0
	      || (ret = dwarf_readu16 (as, a, &addr, &dci.tag, arg)) < 0)
	    return ret;
	  Debug (13, "Found ABI marker = (abi=%u, tag=%u)\n",
		 dci.abi, dci.tag);
	}

      if (dci.sized_augmentation)
	dci.fde_instr_start = aug_end_addr;
      else
	dci.fde_instr_start = addr;
      dci.fde_instr_end = fde_end_addr;

      memcpy (pi->unwind_info, &dci, sizeof (dci));
    }
  return 0;
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                            Gparser.c                               */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

#define alloc_reg_state(as)	(malloc_in_rgn (as, sizeof(dwarf_reg_state_t)))
#define free_reg_state(rs)	/* empty */

static inline int
read_regnum (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	     unw_word_t *valp, void *arg)
{
  int ret;

  if ((ret = dwarf_read_uleb128 (as, a, addr, valp, arg)) < 0)
    return ret;

  if (*valp >= DWARF_NUM_PRESERVED_REGS)
    {
      Debug (1, "Invalid register number %u\n", (unsigned int) *valp);
      return -UNW_EBADREG;
    }
  return 0;
}

static inline void
set_reg (dwarf_state_record_t *sr, unw_word_t regnum, dwarf_where_t where,
	 unw_word_t val)
{
  sr->rs_current.reg[regnum].where = where;
  sr->rs_current.reg[regnum].val = val;
}

/* Run a CFI program to update the register state.  */
static int
run_cfi_program (struct dwarf_cursor *c, dwarf_state_record_t *sr,
		 unw_word_t ip, unw_word_t *addr, unw_word_t end_addr,
		 struct dwarf_cie_info *dci)
{
  unw_word_t curr_ip, operand = 0, regnum, val, len, fde_encoding;
  dwarf_reg_state_t *rs_stack = NULL, *new_rs;
  UNW_UNUSED(dwarf_reg_state_t *old_rs;)
  unw_addr_space_t as;
  unw_accessors_t *a;
  uint8_t u8, op;
  uint16_t u16;
  uint32_t u32;
  void *arg;
  int ret;

  as = c->as;
  arg = c->as_arg;
  if (c->pi.flags & UNW_PI_FLAG_DEBUG_FRAME)
    {
      /* .debug_frame CFI is stored in local address space.  */
      arg = NULL;
    }
  a = unw_get_accessors (as);
  curr_ip = c->pi.start_ip;

  while (curr_ip <= ip && *addr < end_addr)
    {
      if ((ret = dwarf_readu8 (as, a, addr, &op, arg)) < 0)
	return ret;

      if (op & DWARF_CFA_OPCODE_MASK)
	{
	  operand = op & DWARF_CFA_OPERAND_MASK;
	  op &= ~DWARF_CFA_OPERAND_MASK;
	}
      switch ((dwarf_cfa_t) op)
	{
	case DW_CFA_advance_loc:
	  curr_ip += operand * dci->code_align;
	  Debug (15, "CFA_advance_loc to 0x%lx\n", (long) curr_ip);
	  break;

	case DW_CFA_advance_loc1:
	  if ((ret = dwarf_readu8 (as, a, addr, &u8, arg)) < 0)
	    goto fail;
	  curr_ip += u8 * dci->code_align;
	  Debug (15, "CFA_advance_loc1 to 0x%lx\n", (long) curr_ip);
	  break;

	case DW_CFA_advance_loc2:
	  if ((ret = dwarf_readu16 (as, a, addr, &u16, arg)) < 0)
	    goto fail;
	  curr_ip += u16 * dci->code_align;
	  Debug (15, "CFA_advance_loc2 to 0x%lx\n", (long) curr_ip);
	  break;

	case DW_CFA_advance_loc4:
	  if ((ret = dwarf_readu32 (as, a, addr, &u32, arg)) < 0)
	    goto fail;
	  curr_ip += u32 * dci->code_align;
	  Debug (15, "CFA_advance_loc4 to 0x%lx\n", (long) curr_ip);
	  break;

	case DW_CFA_MIPS_advance_loc8:
#ifdef UNW_TARGET_MIPS
	  {
	    uint64_t u64;

	    if ((ret = dwarf_readu64 (as, a, addr, &u64, arg)) < 0)
	      goto fail;
	    curr_ip += u64 * dci->code_align;
	    Debug (15, "CFA_MIPS_advance_loc8\n");
	    break;
	  }
#else
	  Debug (1, "DW_CFA_MIPS_advance_loc8 on non-MIPS target\n");
	  ret = -UNW_EINVAL;
	  goto fail;
#endif

	case DW_CFA_offset:
	  regnum = operand;
	  if (regnum >= DWARF_NUM_PRESERVED_REGS)
	    {
	      Debug (1, "Invalid register number %u in DW_cfa_OFFSET\n",
		     (unsigned int) regnum);
	      ret = -UNW_EBADREG;
	      goto fail;
	    }
	  if ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0)
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_CFAREL, val * dci->data_align);
	  Debug (15, "CFA_offset r%lu at cfa+0x%lx\n",
		 (long) regnum, (long) (val * dci->data_align));
	  break;

	case DW_CFA_offset_extended:
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_CFAREL, val * dci->data_align);
	  Debug (15, "CFA_offset_extended r%lu at cf+0x%lx\n",
		 (long) regnum, (long) (val * dci->data_align));
	  break;

	case DW_CFA_offset_extended_sf:
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_sleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_CFAREL, val * dci->data_align);
	  Debug (15, "CFA_offset_extended_sf r%lu at cf+0x%lx\n",
		 (long) regnum, (long) (val * dci->data_align));
	  break;

	case DW_CFA_restore:
	  regnum = operand;
	  if (regnum >= DWARF_NUM_PRESERVED_REGS)
	    {
	      Debug (1, "Invalid register number %u in DW_CFA_restore\n",
		     (unsigned int) regnum);
	      ret = -UNW_EINVAL;
	      goto fail;
	    }
	  sr->rs_current.reg[regnum] = sr->rs_initial.reg[regnum];
	  Debug (15, "CFA_restore r%lu\n", (long) regnum);
	  break;

	case DW_CFA_restore_extended:
	  if ((ret = dwarf_read_uleb128 (as, a, addr, &regnum, arg)) < 0)
	    goto fail;
	  if (regnum >= DWARF_NUM_PRESERVED_REGS)
	    {
	      Debug (1, "Invalid register number %u in "
		     "DW_CFA_restore_extended\n", (unsigned int) regnum);
	      ret = -UNW_EINVAL;
	      goto fail;
	    }
	  sr->rs_current.reg[regnum] = sr->rs_initial.reg[regnum];
	  Debug (15, "CFA_restore_extended r%lu\n", (long) regnum);
	  break;

	case DW_CFA_nop:
	  break;

	case DW_CFA_set_loc:
	  fde_encoding = dci->fde_encoding;
	  if ((ret = dwarf_read_encoded_pointer (as, a, addr, fde_encoding,
						 &c->pi, &curr_ip,
						 arg)) < 0)
	    goto fail;
	  Debug (15, "CFA_set_loc to 0x%lx\n", (long) curr_ip);
	  break;

	case DW_CFA_undefined:
	  if ((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_UNDEF, 0);
	  Debug (15, "CFA_undefined r%lu\n", (long) regnum);
	  break;

	case DW_CFA_same_value:
	  if ((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_SAME, 0);
	  Debug (15, "CFA_same_value r%lu\n", (long) regnum);
	  break;

	case DW_CFA_register:
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_REG, val);
	  Debug (15, "CFA_register r%lu to r%lu\n", (long) regnum, (long) val);
	  break;

	case DW_CFA_remember_state:
	  new_rs = alloc_reg_state (as);
	  if (!new_rs)
	    {
	      Debug (1, "Out of memory in DW_CFA_remember_state\n");
	      ret = -UNW_ENOMEM;
	      goto fail;
	    }

	  memcpy (new_rs->reg, sr->rs_current.reg, sizeof (new_rs->reg));
	  new_rs->next = rs_stack;
	  rs_stack = new_rs;
	  Debug (15, "CFA_remember_state\n");
	  break;

	case DW_CFA_restore_state:
	  if (!rs_stack)
	    {
	      Debug (1, "register-state stack underflow\n");
	      ret = -UNW_EINVAL;
	      goto fail;
	    }
	  memcpy (&sr->rs_current.reg, &rs_stack->reg, sizeof (rs_stack->reg));
	  UNW_UNUSED(old_rs = rs_stack);
	  rs_stack = rs_stack->next;
	  free_reg_state (old_rs);
	  Debug (15, "CFA_restore_state\n");
	  break;

	case DW_CFA_def_cfa:
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, DWARF_CFA_REG_COLUMN, DWARF_WHERE_REG, regnum);
	  set_reg (sr, DWARF_CFA_OFF_COLUMN, 0, val);	/* NOT factored! */
	  Debug (15, "CFA_def_cfa r%lu+0x%lx\n", (long) regnum, (long) val);
	  break;

	case DW_CFA_def_cfa_sf:
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_sleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, DWARF_CFA_REG_COLUMN, DWARF_WHERE_REG, regnum);
	  set_reg (sr, DWARF_CFA_OFF_COLUMN, 0,
		   val * dci->data_align);		/* factored! */
	  Debug (15, "CFA_def_cfa_sf r%lu+0x%lx\n",
		 (long) regnum, (long) (val * dci->data_align));
	  break;

	case DW_CFA_def_cfa_register:
	  if ((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	    goto fail;
	  set_reg (sr, DWARF_CFA_REG_COLUMN, DWARF_WHERE_REG, regnum);
	  Debug (15, "CFA_def_cfa_register r%lu\n", (long) regnum);
	  break;

	case DW_CFA_def_cfa_offset:
	  if ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0)
	    goto fail;
	  set_reg (sr, DWARF_CFA_OFF_COLUMN, 0, val);	/* NOT factored! */
	  Debug (15, "CFA_def_cfa_offset 0x%lx\n", (long) val);
	  break;

	case DW_CFA_def_cfa_offset_sf:
	  if ((ret = dwarf_read_sleb128 (as, a, addr, &val, arg)) < 0)
	    goto fail;
	  set_reg (sr, DWARF_CFA_OFF_COLUMN, 0,
		   val * dci->data_align);	/* factored! */
	  Debug (15, "CFA_def_cfa_offset_sf 0x%lx\n",
		 (long) (val * dci->data_align));
	  break;

	case DW_CFA_def_cfa_expression:
	  /* Save the address of the DW_FORM_block for later evaluation. */
	  set_reg (sr, DWARF_CFA_REG_COLUMN, DWARF_WHERE_EXPR, *addr);

	  if ((ret = dwarf_read_uleb128 (as, a, addr, &len, arg)) < 0)
	    goto fail;

	  Debug (15, "CFA_def_cfa_expr @ 0x%lx [%lu bytes]\n",
		 (long) *addr, (long) len);
	  *addr += len;
	  break;

	case DW_CFA_expression:
	  if ((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	    goto fail;

	  /* Save the address of the DW_FORM_block for later evaluation. */
	  set_reg (sr, regnum, DWARF_WHERE_EXPR, *addr);

	  if ((ret = dwarf_read_uleb128 (as, a, addr, &len, arg)) < 0)
	    goto fail;

	  Debug (15, "CFA_expression r%lu @ 0x%lx [%lu bytes]\n",
		 (long) regnum, (long) addr, (long) len);
	  *addr += len;
	  break;

	case DW_CFA_GNU_args_size:
	  if ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0)
	    goto fail;
	  sr->args_size = val;
	  Debug (15, "CFA_GNU_args_size %lu\n", (long) val);
	  break;

	case DW_CFA_GNU_negative_offset_extended:
	  /* A comment in GCC says that this is obsoleted by
	     DW_CFA_offset_extended_sf, but that it's used by older
	     PowerPC code.  */
	  if (((ret = read_regnum (as, a, addr, &regnum, arg)) < 0)
	      || ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0))
	    goto fail;
	  set_reg (sr, regnum, DWARF_WHERE_CFAREL, -(val * dci->data_align));
	  Debug (15, "CFA_GNU_negative_offset_extended cfa+0x%lx\n",
		 (long) -(val * dci->data_align));
	  break;

	case DW_CFA_GNU_window_save:
#ifdef UNW_TARGET_SPARC
	  /* This is a special CFA to handle all 16 windowed registers
	     on SPARC.  */
	  for (regnum = 16; regnum < 32; ++regnum)
	    set_reg (sr, regnum, DWARF_WHERE_CFAREL,
		     (regnum - 16) * sizeof (unw_word_t));
	  Debug (15, "CFA_GNU_window_save\n");
	  break;
#else
	  /* FALL THROUGH */
#endif
	case DW_CFA_lo_user:
	case DW_CFA_hi_user:
	  Debug (1, "Unexpected CFA opcode 0x%x\n", op);
	  ret = -UNW_EINVAL;
	  goto fail;
	}
    }
  ret = 0;

 fail:
  UNW_UNUSED(
  /* Free the register-state stack, if not empty already.  */
  while (rs_stack)
    {
      old_rs = rs_stack;
      rs_stack = rs_stack->next;
      free_reg_state (old_rs);
    }
  )
  return ret;
}

static int
fetch_proc_info (struct dwarf_cursor *c, unw_word_t ip, int need_unwind_info)
{
  int ret;

  /* The 'ip' can point either to the previous or next instruction
     depending on what type of frame we have: normal call or a place
     to resume execution (e.g. after signal frame).

     For a normal call frame we need to back up so we point within the
     call itself; this is important because a) the call might be the
     very last instruction of the function and the edge of the FDE,
     and b) so that run_cfi_program() runs locations up to the call
     but not more.

     For execution resume, we need to do the exact opposite and look
     up using the current 'ip' value.  That is where execution will
     continue, and it's important we get this right, as 'ip' could be
     right at the function entry and hence FDE edge, or at instruction
     that manipulates CFA (push/pop). */
  if (c->use_prev_instr)
  --ip;

  if (c->pi_valid && !need_unwind_info)
    return 0;

  memset (&c->pi, 0, sizeof (c->pi));

  if ((ret = tdep_find_proc_info (c, ip, need_unwind_info)) < 0)
    return ret;

  c->pi_valid = 1;

  /* Update use_prev_instr for the next frame. */
  if (need_unwind_info)
  {
    assert(c->pi.unwind_info);
    struct dwarf_cie_info *dci = c->pi.unwind_info;
    c->use_prev_instr = ! dci->signal_frame;
  }

  return ret;
}

static inline void
put_unwind_info (struct dwarf_cursor *c, unw_proc_info_t *pi)
{
  if (!c->pi_valid)
    return;

  if (pi->unwind_info)
    {
      /* free (pi->unwind_info); */
      pi->unwind_info = NULL;
    }
}

static inline int
parse_fde (struct dwarf_cursor *c, unw_word_t ip, dwarf_state_record_t *sr)
{
  struct dwarf_cie_info *dci;
  unw_word_t addr;
  int ret;

  dci = c->pi.unwind_info;
  c->ret_addr_column = dci->ret_addr_column;

  addr = dci->cie_instr_start;
  if ((ret = run_cfi_program (c, sr, ~(unw_word_t) 0, &addr,
			      dci->cie_instr_end, dci)) < 0)
    return ret;

  memcpy (&sr->rs_initial, &sr->rs_current, sizeof (sr->rs_initial));

  addr = dci->fde_instr_start;
  if ((ret = run_cfi_program (c, sr, ip, &addr, dci->fde_instr_end, dci)) < 0)
    return ret;

  return 0;
}

static inline void
flush_rs_cache (struct dwarf_rs_cache *cache)
{
  int i;

  cache->lru_head = DWARF_UNW_CACHE_SIZE - 1;
  cache->lru_tail = 0;

  for (i = 0; i < DWARF_UNW_CACHE_SIZE; ++i)
    {
      if (i > 0)
	cache->buckets[i].lru_chain = (i - 1);
      cache->buckets[i].coll_chain = -1;
      cache->buckets[i].ip = 0;
    }
  for (i = 0; i<DWARF_UNW_HASH_SIZE; ++i)
    cache->hash[i] = -1;
}

static inline struct dwarf_rs_cache *
get_rs_cache (unw_addr_space_t as, intrmask_t *saved_maskp)
{
  struct dwarf_rs_cache *cache = &as->global_cache;
  unw_caching_policy_t caching = as->caching_policy;

  if (caching == UNW_CACHE_NONE)
    return NULL;

#ifndef UW_NO_SYNC
#ifdef HAVE_ATOMIC_H
  if (!spin_trylock_irqsave (&cache->busy, *saved_maskp))
    return NULL;
#else
# ifdef HAVE_ATOMIC_OPS_H
  if (AO_test_and_set (&cache->busy) == AO_TS_SET)
    return NULL;
# else
  sigprocmask (SIG_SETMASK, &unwi_full_mask, saved_maskp);
  if (likely (caching == UNW_CACHE_GLOBAL))
    {
      Debug (16, "%s: acquiring lock\n", __FUNCTION__);
      mutex_lock (&cache->lock);
    }
# endif
#endif
#endif

  if (atomic_read (&as->cache_generation) != atomic_read (&cache->generation))
    {
      flush_rs_cache (cache);
      cache->generation = as->cache_generation;
    }

  return cache;
}

static inline void
put_rs_cache (unw_addr_space_t as, struct dwarf_rs_cache *cache,
		  intrmask_t *saved_maskp)
{
  assert (as->caching_policy != UNW_CACHE_NONE);

  Debug (16, "unmasking signals/interrupts and releasing lock\n");
#ifndef UW_NO_SYNC
#ifdef HAVE_ATOMIC_H
  spin_unlock_irqrestore (&cache->busy, *saved_maskp);
#else
# ifdef HAVE_ATOMIC_OPS_H
  AO_CLEAR (&cache->busy);
# else
  if (likely (as->caching_policy == UNW_CACHE_GLOBAL))
     mutex_unlock (&cache->lock);
  sigprocmask (SIG_SETMASK, saved_maskp, NULL);
# endif
#endif
#endif
}

static inline unw_hash_index_t
hash (unw_word_t ip)
{
  /* based on (sqrt(5)/2-1)*2^64 */
# define magic	((unw_word_t) 0x9e3779b97f4a7c16ULL)

  return ip * magic >> ((sizeof(unw_word_t) * 8) - DWARF_LOG_UNW_HASH_SIZE);
}

static inline long
cache_match (dwarf_reg_state_t *rs, unw_word_t ip)
{
  if (rs->valid && (ip == rs->ip))
    return 1;
  return 0;
}

static dwarf_reg_state_t *
rs_lookup (struct dwarf_rs_cache *cache, struct dwarf_cursor *c)
{
  dwarf_reg_state_t *rs = cache->buckets + c->hint;
  unsigned short index;
  unw_word_t ip;

  ip = c->ip;

  if (cache_match (rs, ip))
    return rs;

  index = cache->hash[hash (ip)];
  if (index >= DWARF_UNW_CACHE_SIZE)
    return 0;

  rs = cache->buckets + index;
  while (1)
    {
      if (cache_match (rs, ip))
        {
          /* update hint; no locking needed: single-word writes are atomic */
          c->hint = cache->buckets[c->prev_rs].hint =
            (rs - cache->buckets);
          return rs;
        }
      if (rs->coll_chain >= DWARF_UNW_HASH_SIZE)
        return 0;
      if (!rs->coll_chain)
        /* Something went wrong */
        return 0;
      rs = cache->buckets + rs->coll_chain;
    }
}

static inline dwarf_reg_state_t *
rs_new (struct dwarf_rs_cache *cache, struct dwarf_cursor * c)
{
  dwarf_reg_state_t *rs, *prev, *tmp;
  unw_hash_index_t index;
  unsigned short head;

  head = cache->lru_head;
  rs = cache->buckets + head;
  cache->lru_head = rs->lru_chain;

  /* re-insert rs at the tail of the LRU chain: */
  cache->buckets[cache->lru_tail].lru_chain = head;
  cache->lru_tail = head;

  /* remove the old rs from the hash table (if it's there): */
  if (rs->ip)
    {
      index = hash (rs->ip);
      tmp = cache->buckets + cache->hash[index];
      prev = 0;
      while (1)
	{
	  if (tmp == rs)
	    {
	      if (prev)
		prev->coll_chain = tmp->coll_chain;
	      else
		cache->hash[index] = tmp->coll_chain;
	      break;
	    }
	  else
	    prev = tmp;
	  if (tmp->coll_chain >= DWARF_UNW_CACHE_SIZE)
	    /* old rs wasn't in the hash-table */
	    break;
	  tmp = cache->buckets + tmp->coll_chain;
	}
    }

  /* enter new rs in the hash table */
  index = hash (c->ip);
  rs->coll_chain = cache->hash[index];
  cache->hash[index] = rs - cache->buckets;

  rs->hint = 0;
  rs->ip = c->ip;
  rs->valid = 1;
  rs->ret_addr_column = c->ret_addr_column;
  rs->signal_frame = 0;

  return rs;
}

static int
create_state_record_for (struct dwarf_cursor *c, dwarf_state_record_t *sr,
			 unw_word_t ip)
{
  int i, ret;

  assert (c->pi_valid);

  memset (sr, 0, sizeof (*sr));
  for (i = 0; i < DWARF_NUM_PRESERVED_REGS + 2; ++i)
    set_reg (sr, i, DWARF_WHERE_SAME, 0);

  switch (c->pi.format)
    {
    case UNW_INFO_FORMAT_TABLE:
    case UNW_INFO_FORMAT_REMOTE_TABLE:
      ret = parse_fde (c, ip, sr);
      break;
#if 0
    case UNW_INFO_FORMAT_DYNAMIC:
      ret = parse_dynamic (c, ip, sr);
      break;
#endif

    default:
      Debug (1, "Unexpected unwind-info format %d\n", c->pi.format);
      ret = -UNW_EINVAL;
    }
  return ret;
}

static inline int
eval_location_expr (struct dwarf_cursor *c, unw_addr_space_t as,
		    unw_accessors_t *a, unw_word_t addr,
		    dwarf_loc_t *locp, void *arg)
{
  int ret, is_register;
  unw_word_t len, val;

  /* read the length of the expression: */
  if ((ret = dwarf_read_uleb128 (as, a, &addr, &len, arg)) < 0)
    return ret;

  /* evaluate the expression: */
  if ((ret = dwarf_eval_expr (c, &addr, len, &val, &is_register)) < 0)
    return ret;

  if (is_register)
    *locp = DWARF_REG_LOC (c, dwarf_to_unw_regnum (val));
  else
    *locp = DWARF_MEM_LOC (c, val);

  return 0;
}

static int
apply_reg_state (struct dwarf_cursor *c, struct dwarf_reg_state *rs)
{
  unw_word_t regnum, addr, cfa, ip;
  unw_word_t prev_ip, prev_cfa;
  unw_addr_space_t as;
  dwarf_loc_t cfa_loc;
  unw_accessors_t *a;
  int i, ret;
  void *arg;

  prev_ip = c->ip;
  prev_cfa = c->cfa;

  as = c->as;
  arg = c->as_arg;
  a = unw_get_accessors (as);

  /* Evaluate the CFA first, because it may be referred to by other
     expressions.  */

  if (rs->reg[DWARF_CFA_REG_COLUMN].where == DWARF_WHERE_REG)
    {
      /* CFA is equal to [reg] + offset: */

      /* As a special-case, if the stack-pointer is the CFA and the
	 stack-pointer wasn't saved, popping the CFA implicitly pops
	 the stack-pointer as well.  */
      if ((rs->reg[DWARF_CFA_REG_COLUMN].val == UNW_TDEP_SP)
	  && (rs->reg[UNW_TDEP_SP].where == DWARF_WHERE_SAME))
	  cfa = c->cfa;
      else
	{
	  regnum = dwarf_to_unw_regnum (rs->reg[DWARF_CFA_REG_COLUMN].val);
	  if ((ret = unw_get_reg ((unw_cursor_t *) c, regnum, &cfa)) < 0)
	    return ret;
	}
      cfa += rs->reg[DWARF_CFA_OFF_COLUMN].val;
    }
  else
    {
      /* CFA is equal to EXPR: */

      assert (rs->reg[DWARF_CFA_REG_COLUMN].where == DWARF_WHERE_EXPR);

      addr = rs->reg[DWARF_CFA_REG_COLUMN].val;
      if ((ret = eval_location_expr (c, as, a, addr, &cfa_loc, arg)) < 0)
	return ret;
      /* the returned location better be a memory location... */
      if (DWARF_IS_REG_LOC (cfa_loc))
	return -UNW_EBADFRAME;
      cfa = DWARF_GET_LOC (cfa_loc);
    }

  for (i = 0; i < DWARF_NUM_PRESERVED_REGS; ++i)
    {
      switch ((dwarf_where_t) rs->reg[i].where)
	{
	case DWARF_WHERE_UNDEF:
	  c->loc[i] = DWARF_NULL_LOC;
	  break;

	case DWARF_WHERE_SAME:
	  break;

	case DWARF_WHERE_CFAREL:
	  c->loc[i] = DWARF_MEM_LOC (c, cfa + rs->reg[i].val);
	  break;

	case DWARF_WHERE_REG:
	  c->loc[i] = DWARF_REG_LOC (c, dwarf_to_unw_regnum (rs->reg[i].val));
	  break;

	case DWARF_WHERE_EXPR:
	  addr = rs->reg[i].val;
	  if ((ret = eval_location_expr (c, as, a, addr, c->loc + i, arg)) < 0)
	    return ret;
	  break;
	}
    }
  c->cfa = cfa;
   /* DWARF spec says undefined return address location means end of stack. */
  if (DWARF_IS_NULL_LOC (c->loc[c->ret_addr_column]))
    c->ip = 0;
  else {
    ret = dwarf_get (c, c->loc[c->ret_addr_column], &ip);
    if (ret < 0)
      return ret;
    c->ip = ip;
  }
  /* XXX: check for ip to be code_aligned */

  if (c->ip == prev_ip && c->cfa == prev_cfa)
    {
      Dprintf ("%s: ip and cfa unchanged; stopping here (ip=0x%lx)\n",
	       __FUNCTION__, (long) c->ip);
      return -UNW_EBADFRAME;
    }
  return 0;
}

static int
uncached_dwarf_find_save_locs (struct dwarf_cursor *c)
{
  dwarf_state_record_t sr;
  int ret;

  if ((ret = fetch_proc_info (c, c->ip, 1)) < 0)
    return ret;

  if ((ret = create_state_record_for (c, &sr, c->ip)) < 0)
    return ret;

  if ((ret = apply_reg_state (c, &sr.rs_current)) < 0)
    return ret;

  put_unwind_info (c, &c->pi);
  return 0;
}

/* The function finds the saved locations and applies the register
   state as well. */
HIDDEN int
dwarf_find_save_locs (struct dwarf_cursor *c)
{
  dwarf_state_record_t sr;
  dwarf_reg_state_t *rs, *rs1;
  struct dwarf_rs_cache *cache;
  int ret = 0;
  intrmask_t saved_mask;

  if (c->as->caching_policy == UNW_CACHE_NONE)
    return uncached_dwarf_find_save_locs (c);

  cache = get_rs_cache(c->as, &saved_mask);
  if (!cache)
    return -UNW_ENOINFO;	/* cache is busy */
  rs = rs_lookup(cache, c);

  if (rs)
    {
      c->ret_addr_column = rs->ret_addr_column;
      c->use_prev_instr = ! rs->signal_frame;
      goto apply;
    }

  if ((ret = fetch_proc_info (c, c->ip, 1)) < 0)
    goto out;

  if ((ret = create_state_record_for (c, &sr, c->ip)) < 0)
    goto out;

  rs1 = &sr.rs_current;
  if (rs1)
    {
      rs = rs_new (cache, c);
      memcpy(rs, rs1, offsetof(struct dwarf_reg_state, ip));
      if (!rs)
        {
          Dprintf ("%s: failed to create unwind rs\n", __FUNCTION__);
          ret = -UNW_EUNSPEC;
	  goto out;
        }
    }
  cache->buckets[c->prev_rs].hint = rs - cache->buckets;

  c->hint = rs->hint;
  c->prev_rs = rs - cache->buckets;

  put_unwind_info (c, &c->pi);
  ret = apply_reg_state (c, rs);

out:
  put_rs_cache (c->as, cache, &saved_mask);
  return ret;

apply:
  put_rs_cache (c->as, cache, &saved_mask);
  if ((ret = apply_reg_state (c, rs)) < 0)
    return ret;

  return 0;
}

HIDDEN int
dwarf_step (struct dwarf_cursor *c)
{
  /* unw_word_t prev_cfa = c->cfa; */
  int ret;

  if ((ret = dwarf_find_save_locs (c)) >= 0) {
    c->pi_valid = 0;
    ret = (c->ip == 0) ? 0 : 1;
  }

  Debug (15, "returning %d\n", ret);
  return ret;
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                              Gpe.c                                 */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

HIDDEN int
dwarf_read_encoded_pointer (unw_addr_space_t as, unw_accessors_t *a,
			    unw_word_t *addr, unsigned char encoding,
			    const unw_proc_info_t *pi,
			    unw_word_t *valp, void *arg)
{
  return dwarf_read_encoded_pointer_inlined (as, a, addr, encoding,
					     pi, valp, arg);
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                      Gfind_proc_info-lsb.c                         */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

struct table_entry
  {
    int32_t start_ip_offset;
    int32_t fde_offset;
  };

#ifndef UNW_REMOTE_ONLY

struct callback_data
  {
    /* in: */
    unw_addr_space_t as;
    unw_word_t ip;		/* instruction-pointer we're looking for */
    unw_proc_info_t *pi;	/* proc-info pointer */
    int need_unwind_info;
    /* out: */
    int single_fde;		/* did we find a single FDE? (vs. a table) */
    unw_dyn_info_t di;		/* table info (if single_fde is false) */
  };

static int
linear_search (unw_addr_space_t as, unw_word_t ip,
	       unw_word_t eh_frame_start, unw_word_t eh_frame_end,
	       unw_word_t fde_count,
	       unw_proc_info_t *pi, int need_unwind_info, void *arg)
{
  unw_accessors_t *a = NULL;
  unw_word_t i = 0, fde_addr, addr = eh_frame_start;
  int ret;

  while (i++ < fde_count && addr < eh_frame_end)
    {
      fde_addr = addr;
      if ((ret = dwarf_extract_proc_info_from_fde (as, a, &addr, pi, 0, 0, arg))
	  < 0)
	return ret;

      if (ip >= pi->start_ip && ip < pi->end_ip)
	{
	  if (!need_unwind_info)
	    return 1;
	  addr = fde_addr;
	  if ((ret = dwarf_extract_proc_info_from_fde (as, a, &addr, pi,
						       need_unwind_info, 0, arg))
	      < 0)
	    return ret;
	  return 1;
	}
    }
  return -UNW_ENOINFO;
}

#ifdef CONFIG_DEBUG_FRAME
/* Load .debug_frame section from FILE.  Allocates and returns space
   in *BUF, and sets *BUFSIZE to its size.  IS_LOCAL is 1 if using the
   local process, in which case we can search the system debug file
   directory; 0 for other address spaces, in which case we do not; or
   -1 for recursive calls following .gnu_debuglink.  Returns 0 on
   success, 1 on error.  Succeeds even if the file contains no
   .debug_frame.  */
/* XXX: Could use mmap; but elf_map_image keeps tons mapped in.  */

static int
load_debug_frame (const char *file, char **buf, size_t *bufsize, int is_local)
{
  FILE *f;
  Elf_W (Ehdr) ehdr;
  Elf_W (Half) shstrndx;
  Elf_W (Shdr) *sec_hdrs = NULL;
  char *stringtab = NULL;
  unsigned int i;
  size_t linksize = 0;
  char *linkbuf = NULL;
  
  *buf = NULL;
  *bufsize = 0;
  
  f = fopen (file, "r");
  
  if (!f)
    return 1;
  
  if (fread (&ehdr, sizeof (Elf_W (Ehdr)), 1, f) != 1)
    goto file_error;
  
  shstrndx = ehdr.e_shstrndx;
  
  Debug (4, "opened file '%s'. Section header at offset %d\n",
         file, (int) ehdr.e_shoff);

  fseek (f, ehdr.e_shoff, SEEK_SET);
  sec_hdrs = calloc (ehdr.e_shnum, sizeof (Elf_W (Shdr)));
  if (fread (sec_hdrs, sizeof (Elf_W (Shdr)), ehdr.e_shnum, f) != ehdr.e_shnum)
    goto file_error;
  
  Debug (4, "loading string table of size %zd\n",
	   sec_hdrs[shstrndx].sh_size);
  stringtab = malloc (sec_hdrs[shstrndx].sh_size);
  fseek (f, sec_hdrs[shstrndx].sh_offset, SEEK_SET);
  if (fread (stringtab, 1, sec_hdrs[shstrndx].sh_size, f) != sec_hdrs[shstrndx].sh_size)
    goto file_error;
  
  for (i = 1; i < ehdr.e_shnum && *buf == NULL; i++)
    {
      char *secname = &stringtab[sec_hdrs[i].sh_name];

      if (strcmp (secname, ".debug_frame") == 0)
        {
	  *bufsize = sec_hdrs[i].sh_size;
	  *buf = malloc (*bufsize);

	  fseek (f, sec_hdrs[i].sh_offset, SEEK_SET);
	  if (fread (*buf, 1, *bufsize, f) != *bufsize)
	    goto file_error;

	  Debug (4, "read %zd bytes of .debug_frame from offset %zd\n",
		 *bufsize, sec_hdrs[i].sh_offset);
	}
      else if (strcmp (secname, ".gnu_debuglink") == 0)
	{
	  linksize = sec_hdrs[i].sh_size;
	  linkbuf = malloc (linksize);

	  fseek (f, sec_hdrs[i].sh_offset, SEEK_SET);
	  if (fread (linkbuf, 1, linksize, f) != linksize)
	    goto file_error;

	  Debug (4, "read %zd bytes of .gnu_debuglink from offset %zd\n",
		 linksize, sec_hdrs[i].sh_offset);
	}
    }

  free (stringtab);
  free (sec_hdrs);

  fclose (f);

  /* Ignore separate debug files which contain a .gnu_debuglink section. */
  if (linkbuf && is_local == -1)
    {
      free (linkbuf);
      return 1;
    }

  free (linkbuf);

  return 0;

/* An error reading image file. Release resources and return error code */
file_error:
  free(stringtab);
  free(sec_hdrs);
  free(linkbuf);
  fclose(f);

  return 1;
}

/* Locate the binary which originated the contents of address ADDR. Return
   the name of the binary in *name (space is allocated by the caller)
   Returns 0 if a binary is successfully found, or 1 if an error occurs.  */

static int
find_binary_for_address (unw_word_t ip, char *name, size_t name_size)
{
#if defined(__linux) && (!UNW_REMOTE_ONLY)
  struct map_iterator mi;
  int found = 0;
  int pid = getpid ();
  unsigned long segbase, mapoff, hi;

  maps_init (&mi, pid);
  while (maps_next (&mi, &segbase, &hi, &mapoff))
    if (ip >= segbase && ip < hi)
      {
	size_t len = strlen (mi.path);

	if (len + 1 <= name_size)
	  {
	    memcpy (name, mi.path, len + 1);
	    found = 1;
	  }
	break;
      }
  maps_close (&mi);
  return !found;
#endif

  return 1;
}

/* Locate and/or try to load a debug_frame section for address ADDR.  Return
   pointer to debug frame descriptor, or zero if not found.  */

static struct unw_debug_frame_list *
locate_debug_info (unw_addr_space_t as, unw_word_t addr, const char *dlname,
		   unw_word_t start, unw_word_t end)
{
#ifndef PATH_MAX
# define PATH_MAX 1024
#endif
  struct unw_debug_frame_list *w, *fdesc = 0;
  char path[PATH_MAX];
  char *name = path;
  int err;
  char *buf;
  size_t bufsize;

  /* First, see if we loaded this frame already.  */

  for (w = as->debug_frames; w; w = w->next)
    {
      Debug (4, "checking %p: %lx-%lx\n", w, (long)w->start, (long)w->end);
      if (addr >= w->start && addr < w->end)
	return w;
    }

  /* If the object name we receive is blank, there's still a chance of locating
     the file by parsing /proc/self/maps.  */

  if (strcmp (dlname, "") == 0)
    {
      err = find_binary_for_address (addr, name, sizeof(path));
      if (err)
        {
	  Debug (15, "tried to locate binary for 0x%" PRIx64 ", but no luck\n",
		 (uint64_t) addr);
          return 0;
	}
    }
  else
    name = (char*) dlname;

  err = load_debug_frame (name, &buf, &bufsize, 1);
  
  if (!err)
    {
      fdesc = malloc (sizeof (struct unw_debug_frame_list));

      fdesc->start = start;
      fdesc->end = end;
      fdesc->debug_frame = buf;
      fdesc->debug_frame_size = bufsize;
      fdesc->index = NULL;
      fdesc->next = as->debug_frames;
      
      as->debug_frames = fdesc;
    }
  
  return fdesc;
}

struct debug_frame_tab
  {
    struct table_entry *tab;
    uint32_t length;
    uint32_t size;
  };

static void
debug_frame_tab_append (struct debug_frame_tab *tab,
			unw_word_t fde_offset, unw_word_t start_ip)
{
  unsigned int length = tab->length;

  if (length == tab->size)
    {
      tab->size *= 2;
      tab->tab = realloc (tab->tab, sizeof (struct table_entry) * tab->size);
    }
  
  tab->tab[length].fde_offset = fde_offset;
  tab->tab[length].start_ip_offset = start_ip;
  
  tab->length = length + 1;
}

static void
debug_frame_tab_shrink (struct debug_frame_tab *tab)
{
  if (tab->size > tab->length)
    {
      tab->tab = realloc (tab->tab, sizeof (struct table_entry) * tab->length);
      tab->size = tab->length;
    }
}

static int
debug_frame_tab_compare (const void *a, const void *b)
{
  const struct table_entry *fa = a, *fb = b;
  
  if (fa->start_ip_offset > fb->start_ip_offset)
    return 1;
  else if (fa->start_ip_offset < fb->start_ip_offset)
    return -1;
  else
    return 0;
}

PROTECTED int
dwarf_find_debug_frame (unw_addr_space_t unw_local_addr_space, 
			int found, unw_dyn_info_t *di_debug, unw_word_t ip,
			unw_word_t segbase, const char* obj_name,
			unw_word_t start, unw_word_t end)
{
  unw_dyn_info_t *di;
  struct unw_debug_frame_list *fdesc = 0;
  unw_accessors_t *a;
  unw_word_t addr;

  Debug (15, "Trying to find .debug_frame for %s\n", obj_name);
  di = di_debug;

  fdesc = locate_debug_info (unw_local_addr_space, ip, obj_name, start, end);

  if (!fdesc)
    {
      Debug (15, "couldn't load .debug_frame\n");
      return found;
    }
  else
    {
      char *buf;
      size_t bufsize;
      unw_word_t item_start, item_end = 0;
      uint32_t u32val = 0;
      uint64_t cie_id = 0;
      struct debug_frame_tab tab;

      Debug (15, "loaded .debug_frame\n");

      buf = fdesc->debug_frame;
      bufsize = fdesc->debug_frame_size;

      if (bufsize == 0)
       {
         Debug (15, "zero-length .debug_frame\n");
         return found;
       }

      /* Now create a binary-search table, if it does not already exist.  */
      if (!fdesc->index)
       {
         addr = (unw_word_t) (uintptr_t) buf;

         a = unw_get_accessors (unw_local_addr_space);

         /* Find all FDE entries in debug_frame, and make into a sorted
            index.  */

         tab.length = 0;
         tab.size = 16;
         tab.tab = calloc (tab.size, sizeof (struct table_entry));

         while (addr < (unw_word_t) (uintptr_t) (buf + bufsize))
           {
             uint64_t id_for_cie;
             item_start = addr;

             dwarf_readu32 (unw_local_addr_space, a, &addr, &u32val, NULL);

             if (u32val == 0)
               break;
             else if (u32val != 0xffffffff)
               {
                 uint32_t cie_id32 = 0;
                 item_end = addr + u32val;
                 dwarf_readu32 (unw_local_addr_space, a, &addr, &cie_id32,
                                NULL);
                 cie_id = cie_id32;
                 id_for_cie = 0xffffffff;
               }
             else
               {
                 uint64_t u64val = 0;
                 /* Extended length.  */
                 dwarf_readu64 (unw_local_addr_space, a, &addr, &u64val, NULL);
                 item_end = addr + u64val;

                 dwarf_readu64 (unw_local_addr_space, a, &addr, &cie_id, NULL);
                 id_for_cie = 0xffffffffffffffffull;
               }

             /*Debug (1, "CIE/FDE id = %.8x\n", (int) cie_id);*/

             if (cie_id == id_for_cie)
               ;
             /*Debug (1, "Found CIE at %.8x.\n", item_start);*/
             else
               {
                 unw_word_t fde_addr = item_start;
                 unw_proc_info_t this_pi;
                 int err;

                 /*Debug (1, "Found FDE at %.8x\n", item_start);*/

                 err = dwarf_extract_proc_info_from_fde (unw_local_addr_space,
                                                         a, &fde_addr,
                                                         &this_pi, 0,
                                                         (uintptr_t) buf,
                                                         NULL);

                 if (err == 0)
                   {
                     Debug (15, "start_ip = %lx, end_ip = %lx\n",
                            (long) this_pi.start_ip, (long) this_pi.end_ip);
                     debug_frame_tab_append (&tab,
                                             item_start - (unw_word_t) (uintptr_t) buf,
                                             this_pi.start_ip);
                   }
                 /*else
                   Debug (1, "FDE parse failed\n");*/
               }

             addr = item_end;
           }

         debug_frame_tab_shrink (&tab);
         qsort (tab.tab, tab.length, sizeof (struct table_entry),
                debug_frame_tab_compare);
         /* for (i = 0; i < tab.length; i++)
            {
            fprintf (stderr, "ip %x, fde offset %x\n",
            (int) tab.tab[i].start_ip_offset,
            (int) tab.tab[i].fde_offset);
            }*/
         fdesc->index = tab.tab;
         fdesc->index_size = tab.length;
       }

      di->format = UNW_INFO_FORMAT_TABLE;
      di->start_ip = fdesc->start;
      di->end_ip = fdesc->end;
      di->u.rti.name_ptr = (unw_word_t) (uintptr_t) obj_name;
      di->u.rti.table_data = (unw_word_t) (uintptr_t) fdesc;
      di->u.rti.table_len = sizeof (*fdesc) / sizeof (unw_word_t);
      di->u.rti.segbase = segbase;

      found = 1;
      Debug (15, "found debug_frame table\n");
    }
  return found;
}

#endif /* CONFIG_DEBUG_FRAME */

/* Info is a pointer to a unw_dyn_info_t structure and, on entry,
   member u.rti.segbase contains the instruction-pointer we're looking
   for.  */
static int
callback (struct dl_phdr_info *info, size_t size, void *ptr)
{
  struct callback_data *cb_data = ptr;
  unw_dyn_info_t *di = &cb_data->di;
  const Elf_W(Phdr) *phdr, *p_eh_hdr, *p_dynamic, *p_text;
  unw_word_t addr, eh_frame_start, eh_frame_end, fde_count, ip;
  Elf_W(Addr) load_base, max_load_addr = 0;
  UNW_UNUSED(Elf_W(Addr) segbase = 0;)
  int ret, need_unwind_info = cb_data->need_unwind_info;
  unw_proc_info_t *pi = cb_data->pi;
  struct dwarf_eh_frame_hdr *hdr;
  unw_accessors_t *a;
  long n;

  ip = cb_data->ip;

  /* Make sure struct dl_phdr_info is at least as big as we need.  */
  if (size < offsetof (struct dl_phdr_info, dlpi_phnum)
	     + sizeof (info->dlpi_phnum))
    return -1;

  Debug (15, "checking %s, base=0x%lx)\n",
	 info->dlpi_name, (long) info->dlpi_addr);

  phdr = info->dlpi_phdr;
  load_base = info->dlpi_addr;
  p_text = NULL;
  p_eh_hdr = NULL;
  p_dynamic = NULL;

  /* See if PC falls into one of the loaded segments.  Find the
     eh-header segment at the same time.  */
  for (n = info->dlpi_phnum; --n >= 0; phdr++)
    {
      if (phdr->p_type == PT_LOAD)
	{
	  Elf_W(Addr) vaddr = phdr->p_vaddr + load_base;

          Debug(18, "check %lx versus %lx-%lx\n", ip, vaddr, vaddr + phdr->p_memsz);

	  if (ip >= vaddr && ip < vaddr + phdr->p_memsz) {
	    p_text = phdr;
	  }

	  if (vaddr + phdr->p_filesz > max_load_addr)
	    max_load_addr = vaddr + phdr->p_filesz;
	}
      else if (phdr->p_type == PT_GNU_EH_FRAME)
	p_eh_hdr = phdr;
      else if (phdr->p_type == PT_DYNAMIC)
	p_dynamic = phdr;
    }
  if (!p_text || !p_eh_hdr) {
#ifdef CONFIG_DEBUG_FRAME
    /* Find the start/end of the described region by parsing the phdr_info
       structure.  */
    unw_word_t start, end;
    start = (unw_word_t) -1;
    end = 0;

    for (n = 0; n < info->dlpi_phnum; n++)
      {
        if (info->dlpi_phdr[n].p_type == PT_LOAD)
          {
            unw_word_t seg_start = info->dlpi_addr + info->dlpi_phdr[n].p_vaddr;
            unw_word_t seg_end = seg_start + info->dlpi_phdr[n].p_memsz;
            
            if (seg_start < start)
              start = seg_start;
            
            if (seg_end > end)
              end = seg_end;
          }
      }
    
    return dwarf_find_debug_frame (cb_data->as,
				   0, &cb_data->di, ip,
                                   info->dlpi_addr, info->dlpi_name, start,
                                   end);
#else
    return 0;
#endif  /* CONFIG_DEBUG_FRAME */
  }

  /* segbase is currently unused: */
  UNW_UNUSED(
  if (likely (p_eh_hdr->p_vaddr >= p_text->p_vaddr
	      && p_eh_hdr->p_vaddr < p_text->p_vaddr + p_text->p_memsz)) {
    /* normal case: eh-hdr is inside text segment */
    segbase = p_text->p_vaddr + load_base;
  } else
    {
      /* Special case: eh-hdr is in some other segment; this may
	 happen, e.g., for the Linux kernel's gate DSO, for
	 example.  */
      phdr = info->dlpi_phdr;
      for (n = info->dlpi_phnum; --n >= 0; phdr++)
	{
	  if (phdr->p_type == PT_LOAD && p_eh_hdr->p_vaddr >= phdr->p_vaddr
	      && p_eh_hdr->p_vaddr < phdr->p_vaddr + phdr->p_memsz)
	    {
	      segbase = phdr->p_vaddr + load_base;
	      break;
	    }
	}
    }
  )

  if (p_dynamic)
    {
      /* For dynamicly linked executables and shared libraries,
	 DT_PLTGOT is the value that data-relative addresses are
	 relative to for that object.  We call this the "gp".  */
      Elf_W(Dyn) *dyn = (Elf_W(Dyn) *)(p_dynamic->p_vaddr + load_base);
      for (; dyn->d_tag != DT_NULL; ++dyn)
	if (dyn->d_tag == DT_PLTGOT)
	  {
	    /* Assume that _DYNAMIC is writable and GLIBC has
	       relocated it (true for x86 at least).  */
	    di->gp = dyn->d_un.d_ptr;
	    break;
	  }
    }
  else
    /* Otherwise this is a static executable with no _DYNAMIC.  Assume
       that data-relative addresses are relative to 0, i.e.,
       absolute.  */
    di->gp = 0;
  pi->gp = di->gp;

  hdr = (struct dwarf_eh_frame_hdr *) (p_eh_hdr->p_vaddr + load_base);
  if (hdr->version != DW_EH_VERSION)
    {
      Debug (1, "table `%s' has unexpected version %d\n",
	     info->dlpi_name, hdr->version);
      return 0;
    }

  a = unw_get_accessors (cb_data->as);
  addr = (unw_word_t) (hdr + 1);

  /* (Optionally) read eh_frame_ptr: */
  if ((ret = dwarf_read_encoded_pointer (cb_data->as, a,
					 &addr, hdr->eh_frame_ptr_enc, pi,
					 &eh_frame_start, NULL)) < 0)
    return ret;

  /* (Optionally) read fde_count: */
  if ((ret = dwarf_read_encoded_pointer (cb_data->as, a,
					 &addr, hdr->fde_count_enc, pi,
					 &fde_count, NULL)) < 0)
    return ret;

  if (hdr->table_enc != (DW_EH_PE_datarel | DW_EH_PE_sdata4))
    {
      /* If there is no search table or it has an unsupported
	 encoding, fall back on linear search.  */
      if (hdr->table_enc == DW_EH_PE_omit)
	Debug (4, "table `%s' lacks search table; doing linear search\n",
	       info->dlpi_name);
      else
	Debug (4, "table `%s' has encoding 0x%x; doing linear search\n",
	       info->dlpi_name, hdr->table_enc);

      eh_frame_end = max_load_addr;	/* XXX can we do better? */

      if (hdr->fde_count_enc == DW_EH_PE_omit)
	fde_count = ~0UL;
      if (hdr->eh_frame_ptr_enc == DW_EH_PE_omit)
	abort ();

      cb_data->single_fde = 1;
      return linear_search (cb_data->as, ip,
			    eh_frame_start, eh_frame_end, fde_count,
			    pi, need_unwind_info, NULL);
    }

  cb_data->single_fde = 0;
  di->format = UNW_INFO_FORMAT_REMOTE_TABLE;
  di->start_ip = p_text->p_vaddr + load_base;
  di->end_ip = p_text->p_vaddr + load_base + p_text->p_memsz;
  di->u.rti.name_ptr = (unw_word_t) info->dlpi_name;
  di->u.rti.table_data = addr;
  assert (sizeof (struct table_entry) % sizeof (unw_word_t) == 0);
  di->u.rti.table_len = (fde_count * sizeof (struct table_entry)
			 / sizeof (unw_word_t));
  /* For the binary-search table in the eh_frame_hdr, data-relative
     means relative to the start of that section... */
  di->u.rti.segbase = (unw_word_t) hdr;

  Debug (15, "found table `%s': segbase=0x%lx, len=%lu, gp=0x%lx, "
	 "table_data=0x%lx\n", (char *) di->u.rti.name_ptr,
	 (long) di->u.rti.segbase, (long) di->u.rti.table_len,
	 (long) di->gp, (long) di->u.rti.table_data);
  return 1;
}

struct addrs_callback_data {
  long size, count;
  unw_word_t *starts, *ends;
};

static void add_address_range(struct addrs_callback_data *cb_data,
			      unw_word_t start,
			      unw_word_t end) {
  if (cb_data->count == cb_data->size) {
    long size = (cb_data->size ? (cb_data->size * 2) : 32);
    unw_word_t *n;

    n = (unw_word_t *)malloc(sizeof(unw_word_t) * size);
    memcpy(n, cb_data->starts, sizeof(unw_word_t) * cb_data->size);
    if (cb_data->starts) free(cb_data->starts);
    cb_data->starts = n;

    n = (unw_word_t *)malloc(sizeof(unw_word_t) * size);
    memcpy(n, cb_data->ends, sizeof(unw_word_t) * cb_data->size);
    if (cb_data->ends) free(cb_data->ends);
    cb_data->ends = n;

    cb_data->size = size;
  }

  cb_data->starts[cb_data->count] = start;
  cb_data->ends[cb_data->count] = end;
  cb_data->count++;
}

static int
safe_addrs_callback (struct dl_phdr_info *info, size_t size, void *ptr)
{
  struct addrs_callback_data *cb_data = ptr;
  const Elf_W(Phdr) *phdr;
  Elf_W(Addr) load_base;
  long n;

  /* Make sure struct dl_phdr_info is at least as big as we need.  */
  if (size < offsetof (struct dl_phdr_info, dlpi_phnum)
	     + sizeof (info->dlpi_phnum))
    return -1;

  phdr = info->dlpi_phdr;
  load_base = info->dlpi_addr;

  /* See if PC falls into one of the loaded segments.  Find the
     eh-header segment at the same time.  */
  for (n = info->dlpi_phnum; --n >= 0; phdr++)
    {
      if (phdr->p_type == PT_LOAD)
	{
	  Elf_W(Addr) vaddr = phdr->p_vaddr + load_base;

	  add_address_range(cb_data, (unw_word_t)vaddr, (unw_word_t)(vaddr + phdr->p_memsz));
	}
    }

  return 0;
}

static struct addrs_callback_data safe_addrs;

HIDDEN int
initialize_safe_addresses (unw_addr_space_t as)
{
  if (!safe_addrs.count) {
    struct addrs_callback_data cb_data;
    memset(&cb_data, 0, sizeof(cb_data));
#ifndef UW_NO_SYNC
    sigprocmask (SIG_SETMASK, &unwi_full_mask, &saved_mask);
#endif
    (void)dl_iterate_phdr (safe_addrs_callback, &cb_data);
#ifndef UW_NO_SYNC
    sigprocmask (SIG_SETMASK, &saved_mask, NULL);
#endif
    memcpy(&safe_addrs, &cb_data, sizeof(cb_data));
  }

  as->num_safe_addresses = safe_addrs.count;
  as->safe_start_addresses = safe_addrs.starts;
  as->safe_end_addresses = safe_addrs.ends;
  return 0;
}

HIDDEN int
dwarf_find_proc_info (unw_addr_space_t as, unw_word_t ip,
		      unw_proc_info_t *pi, int need_unwind_info, void *arg)
{
  struct callback_data cb_data;
#ifndef UW_NO_SYNC
  intrmask_t saved_mask;
#endif
  int ret;

  Debug (14, "looking for IP=0x%lx\n", (long) ip);

  cb_data.as = as;
  cb_data.ip = ip;
  cb_data.pi = pi;
  cb_data.need_unwind_info = need_unwind_info;
  cb_data.single_fde = 0;

#ifndef UW_NO_SYNC
  sigprocmask (SIG_SETMASK, &unwi_full_mask, &saved_mask);
#endif
  ret = dl_iterate_phdr (callback, &cb_data);
#ifndef UW_NO_SYNC
  sigprocmask (SIG_SETMASK, &saved_mask, NULL);
#endif

  if (ret <= 0)
    {
      Debug (14, "IP=0x%lx not found\n", (long) ip);
      return -UNW_ENOINFO;
    }

  if (cb_data.single_fde)
    /* already got the result in *pi */
    return 0;
  else
    /* search the table: */
    return dwarf_search_unwind_table (as, ip, &cb_data.di,
				      pi, need_unwind_info, arg);
}

static inline const struct table_entry *
lookup (const struct table_entry *table, size_t table_size, int32_t rel_ip)
{
  unsigned long table_len = table_size / sizeof (struct table_entry);
  const struct table_entry *e = 0;
  unsigned long lo, hi, mid;

  /* do a binary search for right entry: */
  for (lo = 0, hi = table_len; lo < hi;)
    {
      mid = (lo + hi) / 2;
      e = table + mid;
      if (rel_ip < e->start_ip_offset)
	hi = mid;
      else
	lo = mid + 1;
    }
  if (hi <= 0)
	return NULL;
  e = table + hi - 1;
  return e;
}

#endif /* !UNW_REMOTE_ONLY */

int
dwarf_search_unwind_table (unw_addr_space_t as, unw_word_t ip,
			   unw_dyn_info_t *di, unw_proc_info_t *pi,
			   int need_unwind_info, void *arg)
{
  const struct table_entry *e = NULL, *table;
  unw_word_t segbase = 0, fde_addr;
  unw_accessors_t *a;
  int ret;
  size_t table_len;
  unw_word_t debug_frame_base;

  assert (ip >= di->start_ip && ip < di->end_ip);

  a = unw_get_accessors (as);

  if (di->format == UNW_INFO_FORMAT_REMOTE_TABLE)
    {
      table = (const struct table_entry *) (uintptr_t) di->u.rti.table_data;
      table_len = di->u.rti.table_len * sizeof (unw_word_t);
      debug_frame_base = 0;
    }
  else
    {
#ifdef CONFIG_DEBUG_FRAME
      struct unw_debug_frame_list *fdesc = (void *) di->u.rti.table_data;

      /* UNW_INFO_FORMAT_TABLE (i.e. .debug_frame) is read from local address
         space.  Both the index and the unwind tables live in local memory, but
         the address space to check for properties like the address size and
         endianness is the target one.  */
      table = fdesc->index;
      table_len = fdesc->index_size * sizeof (struct table_entry);
      debug_frame_base = (uintptr_t) fdesc->debug_frame;
#else
      assert(0);
#endif
    }

  segbase = di->u.rti.segbase;
  e = lookup (table, table_len, ip - segbase);

  if (!e)
    {
      /* IP is inside this table's range, but there is no explicit
	 unwind info.  */
      return -UNW_ENOINFO;
    }
  Debug (15, "ip=0x%lx, start_ip=0x%lx\n",
	 (long) ip, (long) (e->start_ip_offset + segbase));
  if (debug_frame_base)
    fde_addr = e->fde_offset + debug_frame_base;
  else
    fde_addr = e->fde_offset + segbase;
  if ((ret = dwarf_extract_proc_info_from_fde (as, a, &fde_addr, pi,
					       need_unwind_info, 
					       debug_frame_base, arg)) < 0)
    return ret;

  /* .debug_frame uses an absolute encoding that does not know about any
     shared library relocation.  */
  if (di->format == UNW_INFO_FORMAT_TABLE)
    {
      pi->start_ip += segbase;
      pi->end_ip += segbase;
      pi->flags = UNW_PI_FLAG_DEBUG_FRAME;
    }

  if (ip < pi->start_ip || ip >= pi->end_ip)
    return -UNW_ENOINFO;

  return 0;
}

/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/
/*                                glue                                */
/*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*/

unw_accessors_t *unw_get_accessors (unw_addr_space_t unw_local_addr_space)
{
  return NULL;
}

#ifdef OS_X

int dl_iterate_phdr (DL_Iter_Callback callback, void *p)
{
  int i, c, j, n, size;
  const struct mach_header *mh;
  struct load_command *cmd;
  char *data;
  struct dl_phdr_info info;
  Phdr *phdr;

  c = _dyld_image_count();

  for (i = 0; i < c; i++) {
    mh = _dyld_get_image_header(i);
    n = mh->ncmds;
    cmd = (struct load_command *)((char *)mh + sizeof(*mh));
    data = (char *)cmd + mh->sizeofcmds;
    phdr = (Phdr *)malloc(sizeof(Phdr) * n);

    info.dlpi_phnum = n;
    info.dlpi_addr = (long)_dyld_get_image_vmaddr_slide(i);
    info.dlpi_name = _dyld_get_image_name(i);
    info.dlpi_phdr = phdr;
      
    for (j = 0; j < n; j++) {
      phdr[j].p_type = cmd->cmd;
      if (cmd->cmd == LC_SEGMENT) {
        struct segment_command *scmd = (struct segment_command *)cmd;
        phdr[j].p_vaddr = scmd->vmaddr;
        phdr[j].p_memsz = scmd->vmsize;
        phdr[j].p_filesz = scmd->filesize;
      }
      
      size = (cmd->cmdsize + sizeof(long) - 1) & ~(sizeof(long) - 1);
      cmd = (struct load_command *)((char *)cmd + size);
    }

    if (callback(&info, sizeof(info), p))
      return 1;
  }

  return 0;
}

#endif

/***********************************************************************/

#if defined(PLAIN_X86)
static uint8_t dwarf_to_unw_regnum_map[19] =
  {
    UNW_X86_EAX, UNW_X86_ECX, UNW_X86_EDX, UNW_X86_EBX,
    UNW_X86_ESP, UNW_X86_EBP, UNW_X86_ESI, UNW_X86_EDI,
    UNW_X86_EIP, UNW_X86_EFLAGS, UNW_X86_TRAPNO,
    UNW_X86_ST0, UNW_X86_ST1, UNW_X86_ST2, UNW_X86_ST3,
    UNW_X86_ST4, UNW_X86_ST5, UNW_X86_ST6, UNW_X86_ST7
  };
#elif defined(UNW_X86_64)
static uint8_t dwarf_to_unw_regnum_map[17] =
  {
    UNW_X86_64_RAX,
    UNW_X86_64_RDX,
    UNW_X86_64_RCX,
    UNW_X86_64_RBX,
    UNW_X86_64_RSI,
    UNW_X86_64_RDI,
    UNW_X86_64_RBP,
    UNW_X86_64_RSP,
    UNW_X86_64_R8,
    UNW_X86_64_R9,
    UNW_X86_64_R10,
    UNW_X86_64_R11,
    UNW_X86_64_R12,
    UNW_X86_64_R13,
    UNW_X86_64_R14,
    UNW_X86_64_R15,
    UNW_X86_64_RIP
  };
#endif

#ifdef PLAIN_X86
/* DWARF column numbers: */
#define EAX	0
#define ECX	1
#define EDX	2
#define EBX	3
#define ESP	4
#define EBP	5
#define ESI	6
#define EDI	7
#define EIP	8
#define EFLAGS	9
#define TRAPNO	10
#define ST0	11
#endif

#ifdef UNW_X86_64
/* DWARF column numbers for x86_64: */
#define RAX	0
#define RDX	1
#define RCX	2
#define RBX	3
#define RSI	4
#define RDI	5
#define RBP	6
#define RSP	7
#define R8	8
#define R9	9
#define R10	10
#define R11	11
#define R12	12
#define R13	13
#define R14	14
#define R15	15
#define RIP	16
#endif

int
unw_get_reg (unw_cursor_t *cursor, int regnum, unw_word_t *valp)
{
  struct cursor *c = (struct cursor *)cursor;
  dwarf_loc_t loc = DWARF_NULL_LOC;
  
#ifdef UNW_ARM
  switch (regnum)
    {
    case UNW_ARM_R0:
    case UNW_ARM_R1:
    case UNW_ARM_R2:
    case UNW_ARM_R3:
    case UNW_ARM_R4:
    case UNW_ARM_R5:
    case UNW_ARM_R6:
    case UNW_ARM_R7:
    case UNW_ARM_R8:
    case UNW_ARM_R9:
    case UNW_ARM_R10:
    case UNW_ARM_R11:
    case UNW_ARM_R12:
    case UNW_ARM_R14:
    case UNW_ARM_R15:
      loc = c->dwarf.loc[regnum - UNW_ARM_R0];
      break;

    case UNW_ARM_R13:
      *valp = c->dwarf.cfa;
      return 0;

      /* FIXME: Initialise coprocessor & shadow registers?  */

    default:
      Debug (1, "bad register number %u\n", reg);
      return -UNW_EBADREG;
    }
#endif

#ifdef UNW_X86_64
  unsigned int mask;
  int arg_num;

  switch (regnum)
    {

    case UNW_X86_64_RIP:
      loc = c->dwarf.loc[RIP];
      break;

    case UNW_X86_64_CFA:
    case UNW_X86_64_RSP:
      *valp = c->dwarf.cfa;
      return 0;

    case UNW_X86_64_RAX:
    case UNW_X86_64_RDX:
      arg_num = regnum - UNW_X86_64_RAX;
      mask = (1 << arg_num);
      if ((c->dwarf.eh_valid_mask & mask) != 0)
	{
	  *valp = c->dwarf.eh_args[arg_num];
	  return 0;
	}
      else
	loc = c->dwarf.loc[(regnum == UNW_X86_64_RAX) ? RAX : RDX];
      break;

    case UNW_X86_64_RCX: loc = c->dwarf.loc[RCX]; break;
    case UNW_X86_64_RBX: loc = c->dwarf.loc[RBX]; break;

    case UNW_X86_64_RBP: loc = c->dwarf.loc[RBP]; break;
    case UNW_X86_64_RSI: loc = c->dwarf.loc[RSI]; break;
    case UNW_X86_64_RDI: loc = c->dwarf.loc[RDI]; break;
    case UNW_X86_64_R8: loc = c->dwarf.loc[R8]; break;
    case UNW_X86_64_R9: loc = c->dwarf.loc[R9]; break;
    case UNW_X86_64_R10: loc = c->dwarf.loc[R10]; break;
    case UNW_X86_64_R11: loc = c->dwarf.loc[R11]; break;
    case UNW_X86_64_R12: loc = c->dwarf.loc[R12]; break;
    case UNW_X86_64_R13: loc = c->dwarf.loc[R13]; break;
    case UNW_X86_64_R14: loc = c->dwarf.loc[R14]; break;
    case UNW_X86_64_R15: loc = c->dwarf.loc[R15]; break;

    default:
      Debug (1, "bad register number %u\n", regnum);
      return -UNW_EBADREG;
    }
#endif

#ifdef PLAIN_X86
  unsigned int mask;
  int arg_num;

  switch (regnum)
    {
    case UNW_X86_EIP:
      loc = c->dwarf.loc[EIP];
      break;

    case UNW_X86_CFA:
    case UNW_X86_ESP:
      *valp = c->dwarf.cfa;
      return 0;

    case UNW_X86_EAX:
    case UNW_X86_EDX:
      arg_num = regnum - UNW_X86_EAX;
      mask = (1 << arg_num);
      if ((c->dwarf.eh_valid_mask & mask) != 0)
	{
	  *valp = c->dwarf.eh_args[arg_num];
	  return 0;
	}
      else
	loc = c->dwarf.loc[(regnum == UNW_X86_EAX) ? EAX : EDX];
      break;

    case UNW_X86_ECX: loc = c->dwarf.loc[ECX]; break;
    case UNW_X86_EBX: loc = c->dwarf.loc[EBX]; break;

    case UNW_X86_EBP: loc = c->dwarf.loc[EBP]; break;
    case UNW_X86_ESI: loc = c->dwarf.loc[ESI]; break;
    case UNW_X86_EDI: loc = c->dwarf.loc[EDI]; break;
    case UNW_X86_EFLAGS: loc = c->dwarf.loc[EFLAGS]; break;
    case UNW_X86_TRAPNO: loc = c->dwarf.loc[TRAPNO]; break;

    case UNW_X86_FCW:
    case UNW_X86_FSW:
    case UNW_X86_FTW:
    case UNW_X86_FOP:
    case UNW_X86_FCS:
    case UNW_X86_FIP:
    case UNW_X86_FEA:
    case UNW_X86_FDS:
    case UNW_X86_MXCSR:
    case UNW_X86_GS:
    case UNW_X86_FS:
    case UNW_X86_ES:
    case UNW_X86_DS:
    case UNW_X86_SS:
    case UNW_X86_CS:
    case UNW_X86_TSS:
    case UNW_X86_LDT:
      /* loc = x86_scratch_loc (c, reg); */
      return -UNW_EBADREG;
      break;

    default:
      Debug (1, "bad register number %u\n", reg);
      return -UNW_EBADREG;
    }
#endif

  return dwarf_get (&c->dwarf, loc, valp);
}

void *
tdep_uc_addr (unw_context_t *uc, int reg)
{
  void *addr;

  switch (reg)
    {
#ifdef LINUX
# if defined(PLAIN_X86)
    case UNW_X86_GS:  addr = &uc->uc_mcontext.gregs[REG_GS]; break;
    case UNW_X86_FS:  addr = &uc->uc_mcontext.gregs[REG_FS]; break;
    case UNW_X86_ES:  addr = &uc->uc_mcontext.gregs[REG_ES]; break;
    case UNW_X86_DS:  addr = &uc->uc_mcontext.gregs[REG_DS]; break;
    case UNW_X86_EAX: addr = &uc->uc_mcontext.gregs[REG_EAX]; break;
    case UNW_X86_EBX: addr = &uc->uc_mcontext.gregs[REG_EBX]; break;
    case UNW_X86_ECX: addr = &uc->uc_mcontext.gregs[REG_ECX]; break;
    case UNW_X86_EDX: addr = &uc->uc_mcontext.gregs[REG_EDX]; break;
    case UNW_X86_ESI: addr = &uc->uc_mcontext.gregs[REG_ESI]; break;
    case UNW_X86_EDI: addr = &uc->uc_mcontext.gregs[REG_EDI]; break;
    case UNW_X86_EBP: addr = &uc->uc_mcontext.gregs[REG_EBP]; break;
    case UNW_X86_EIP: addr = &uc->uc_mcontext.gregs[REG_EIP]; break;
    case UNW_X86_ESP: addr = &uc->uc_mcontext.gregs[REG_ESP]; break;
    case UNW_X86_TRAPNO:  addr = &uc->uc_mcontext.gregs[REG_TRAPNO]; break;
    case UNW_X86_CS:  addr = &uc->uc_mcontext.gregs[REG_CS]; break;
    case UNW_X86_EFLAGS:  addr = &uc->uc_mcontext.gregs[REG_EFL]; break;
    case UNW_X86_SS:  addr = &uc->uc_mcontext.gregs[REG_SS]; break;
# elif defined(UNW_X86_64)
    case UNW_X86_64_R8: addr = &uc->uc_mcontext.gregs[REG_R8]; break;
    case UNW_X86_64_R9: addr = &uc->uc_mcontext.gregs[REG_R9]; break;
    case UNW_X86_64_R10: addr = &uc->uc_mcontext.gregs[REG_R10]; break;
    case UNW_X86_64_R11: addr = &uc->uc_mcontext.gregs[REG_R11]; break;
    case UNW_X86_64_R12: addr = &uc->uc_mcontext.gregs[REG_R12]; break;
    case UNW_X86_64_R13: addr = &uc->uc_mcontext.gregs[REG_R13]; break;
    case UNW_X86_64_R14: addr = &uc->uc_mcontext.gregs[REG_R14]; break;
    case UNW_X86_64_R15: addr = &uc->uc_mcontext.gregs[REG_R15]; break;
    case UNW_X86_64_RDI: addr = &uc->uc_mcontext.gregs[REG_RDI]; break;
    case UNW_X86_64_RSI: addr = &uc->uc_mcontext.gregs[REG_RSI]; break;
    case UNW_X86_64_RBP: addr = &uc->uc_mcontext.gregs[REG_RBP]; break;
    case UNW_X86_64_RBX: addr = &uc->uc_mcontext.gregs[REG_RBX]; break;
    case UNW_X86_64_RDX: addr = &uc->uc_mcontext.gregs[REG_RDX]; break;
    case UNW_X86_64_RAX: addr = &uc->uc_mcontext.gregs[REG_RAX]; break;
    case UNW_X86_64_RCX: addr = &uc->uc_mcontext.gregs[REG_RCX]; break;
    case UNW_X86_64_RSP: addr = &uc->uc_mcontext.gregs[REG_RSP]; break;
    case UNW_X86_64_RIP: addr = &uc->uc_mcontext.gregs[REG_RIP]; break;
# elif defined(UNW_ARM)
    case UNW_ARM_R0:
    case UNW_ARM_R1:
    case UNW_ARM_R2:
    case UNW_ARM_R3:
    case UNW_ARM_R4:
    case UNW_ARM_R5:
    case UNW_ARM_R6:
    case UNW_ARM_R7:
    case UNW_ARM_R8:
    case UNW_ARM_R9:
    case UNW_ARM_R10:
    case UNW_ARM_R11:
    case UNW_ARM_R12:
    case UNW_ARM_R13:
    case UNW_ARM_R14:
    case UNW_ARM_R15:
      addr = &(uc->regs[reg]); break;
# endif
#endif
#ifdef OS_X
    case UNW_X86_GS:  addr = &uc->uc_mcontext->__ss.__gs; break;
    case UNW_X86_FS:  addr = &uc->uc_mcontext->__ss.__fs; break;
    case UNW_X86_ES:  addr = &uc->uc_mcontext->__ss.__es; break;
    case UNW_X86_DS:  addr = &uc->uc_mcontext->__ss.__ds; break;
    case UNW_X86_EAX: addr = &uc->uc_mcontext->__ss.__eax; break;
    case UNW_X86_EBX: addr = &uc->uc_mcontext->__ss.__ebx; break;
    case UNW_X86_ECX: addr = &uc->uc_mcontext->__ss.__ecx; break;
    case UNW_X86_EDX: addr = &uc->uc_mcontext->__ss.__edx; break;
    case UNW_X86_ESI: addr = &uc->uc_mcontext->__ss.__esi; break;
    case UNW_X86_EDI: addr = &uc->uc_mcontext->__ss.__edi; break;
    case UNW_X86_EBP: addr = &uc->uc_mcontext->__ss.__ebp; break;
    case UNW_X86_EIP: addr = &uc->uc_mcontext->__ss.__eip; break;
    case UNW_X86_ESP: addr = &uc->uc_mcontext->__ss.__esp; break;
    case UNW_X86_CS:  addr = &uc->uc_mcontext->__ss.__cs; break;
    case UNW_X86_EFLAGS:  addr = &uc->uc_mcontext->__ss.__eflags; break;
    case UNW_X86_SS:  addr = &uc->uc_mcontext->__ss.__ss; break;
#endif

    default:
      addr = NULL;
    }
  return addr;
}

int dwarf_to_unw_regnum(reg)
{
#ifdef UNW_ARM
  return (((reg) < 16) ? (reg) : 0);
#else
  return (((reg) <= DWARF_REGNUM_MAP_LENGTH) ? dwarf_to_unw_regnum_map[reg] : 0);
#endif
}

#ifdef PLAIN_X86
static inline int
common_init (struct cursor *c)
{
  int ret, i;

  c->dwarf.loc[EAX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EAX);
  c->dwarf.loc[ECX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_ECX);
  c->dwarf.loc[EDX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EDX);
  c->dwarf.loc[EBX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EBX);
  c->dwarf.loc[ESP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_ESP);
  c->dwarf.loc[EBP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EBP);
  c->dwarf.loc[ESI] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EDI);
  c->dwarf.loc[EDI] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EDI);
  c->dwarf.loc[EIP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EIP);
  c->dwarf.loc[EFLAGS] = DWARF_REG_LOC (&c->dwarf, UNW_X86_EFLAGS);
  c->dwarf.loc[TRAPNO] = DWARF_REG_LOC (&c->dwarf, UNW_X86_TRAPNO);
  c->dwarf.loc[ST0] = DWARF_REG_LOC (&c->dwarf, UNW_X86_ST0);
  for (i = ST0 + 1; i < DWARF_NUM_PRESERVED_REGS; ++i)
    c->dwarf.loc[i] = DWARF_NULL_LOC;

  ret = dwarf_get (&c->dwarf, c->dwarf.loc[EIP], &c->dwarf.ip);
  if (ret < 0)
    return ret;

  ret = dwarf_get (&c->dwarf, DWARF_REG_LOC (&c->dwarf, UNW_X86_ESP),
		   &c->dwarf.cfa);
  if (ret < 0)
    return ret;

  c->sigcontext_format = X86_SCF_NONE;
  c->sigcontext_addr = 0;

  c->dwarf.args_size = 0;
  c->dwarf.ret_addr_column = EIP;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;

  return 0;
}
#endif

#ifdef UNW_X86_64
static inline int
common_init (struct cursor *c)
{
  int ret;

  c->dwarf.loc[RAX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RAX);
  c->dwarf.loc[RDX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RDX);
  c->dwarf.loc[RCX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RCX);
  c->dwarf.loc[RBX] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RBX);
  c->dwarf.loc[RSI] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RSI);
  c->dwarf.loc[RDI] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RDI);
  c->dwarf.loc[RBP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RBP);
  c->dwarf.loc[RSP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RSP);
  c->dwarf.loc[R8]  = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R8);
  c->dwarf.loc[R9]  = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R9);
  c->dwarf.loc[R10] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R10);
  c->dwarf.loc[R11] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R11);
  c->dwarf.loc[R12] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R12);
  c->dwarf.loc[R13] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R13);
  c->dwarf.loc[R14] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R14);
  c->dwarf.loc[R15] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_R15);
  c->dwarf.loc[RIP] = DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RIP);

  ret = dwarf_get (&c->dwarf, c->dwarf.loc[RIP], &c->dwarf.ip);
  if (ret < 0)
    return ret;

  ret = dwarf_get (&c->dwarf, DWARF_REG_LOC (&c->dwarf, UNW_X86_64_RSP),
		   &c->dwarf.cfa);
  if (ret < 0)
    return ret;

  c->sigcontext_format = X86_SCF_NONE;
  c->sigcontext_addr = 0;

  c->dwarf.args_size = 0;
  c->dwarf.ret_addr_column = RIP;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;

  return 0;
}
#endif

#ifdef UNW_ARM
static inline int
common_init (struct cursor *c)
{
  int i, ret;

  c->dwarf.loc[UNW_ARM_R0] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R0);
  c->dwarf.loc[UNW_ARM_R1] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R1);
  c->dwarf.loc[UNW_ARM_R2] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R2);
  c->dwarf.loc[UNW_ARM_R3] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R3);
  c->dwarf.loc[UNW_ARM_R4] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R4);
  c->dwarf.loc[UNW_ARM_R5] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R5);
  c->dwarf.loc[UNW_ARM_R6] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R6);
  c->dwarf.loc[UNW_ARM_R7] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R7);
  c->dwarf.loc[UNW_ARM_R8] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R8);
  c->dwarf.loc[UNW_ARM_R9] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R9);
  c->dwarf.loc[UNW_ARM_R10] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R10);
  c->dwarf.loc[UNW_ARM_R11] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R11);
  c->dwarf.loc[UNW_ARM_R12] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R12);
  c->dwarf.loc[UNW_ARM_R13] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R13);
  c->dwarf.loc[UNW_ARM_R14] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R14);
  c->dwarf.loc[UNW_ARM_R15] = DWARF_REG_LOC (&c->dwarf, UNW_ARM_R15);
  for (i = UNW_ARM_R15 + 1; i < DWARF_NUM_PRESERVED_REGS; ++i)
    c->dwarf.loc[i] = DWARF_NULL_LOC;

  ret = dwarf_get (&c->dwarf, c->dwarf.loc[UNW_ARM_R15], &c->dwarf.ip);
  if (ret < 0)
    return ret;

  /* FIXME: correct for ARM?  */
  ret = dwarf_get (&c->dwarf, DWARF_REG_LOC (&c->dwarf, UNW_ARM_R13),
		   &c->dwarf.cfa);
  if (ret < 0)
    return ret;

  c->sigcontext_format = ARM_SCF_NONE;
  c->sigcontext_addr = 0;

  /* FIXME: Initialisation for other registers.  */

  c->dwarf.args_size = 0;
  c->dwarf.ret_addr_column = 0;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;

  return 0;
}
#endif

#ifdef CONFIG_DEBUG_FRAME
struct unw_debug_frame_list *shared_debug_frames;
#endif

int unw_init_local (unw_cursor_t *cursor, unw_context_t *uc)
{
  struct cursor *c = (struct cursor *) cursor;
  unw_addr_space_t as;

  Debug (1, "(cursor=%p)\n", c);

  as = (unw_addr_space_t)malloc(sizeof(struct unw_addr_space));
  memset(as, 0, sizeof(struct unw_addr_space));

#ifdef CONFIG_DEBUG_FRAME
  as->debug_frames = shared_debug_frames;
#endif

  initialize_safe_addresses(as);

  c->dwarf.as = as;
  c->dwarf.as_arg = uc;
  return common_init (c);
}

void unw_destroy_local(unw_cursor_t *cursor)
{
  struct cursor *c = (struct cursor *) cursor;

#ifdef CONFIG_DEBUG_FRAME
  shared_debug_frames = c->dwarf.as->debug_frames;
#endif

  free_all_allocated(c->dwarf.as);
  free(c->dwarf.as);
}

int unw_step (unw_cursor_t *c)
{
  return dwarf_step(&((struct cursor *)c)->dwarf);
}

static char safe_space[8];

void unw_set_safe_pointer_range(unw_cursor_t *c, unw_word_t s, unw_word_t e)
{
  unw_addr_space_t as = ((struct dwarf_cursor *)c)->as;
  as->safe_start_address = s;
  as->safe_end_address = e;
}

int unw_reset_bad_ptr_flag(unw_cursor_t *c)
{
  unw_addr_space_t as = ((struct dwarf_cursor *)c)->as;
  int v = as->saw_bad_ptr;
  as->saw_bad_ptr = 0;
  return v;
}

static void *safe_pointer(unw_addr_space_t as, unw_word_t p)
{
  int i;

  for (i = as->num_safe_addresses; i--; ) {
    if ((p >= as->safe_start_addresses[i])
	&& (p <= as->safe_end_addresses[i]))
      return (void *)p;
  }

#ifdef CONFIG_DEBUG_FRAME
  {
    struct unw_debug_frame_list *df = as->debug_frames;
    while (df) {
      if ((p >= (unw_word_t)df->debug_frame)
	  && (p < (unw_word_t)(df->debug_frame
			       + df->debug_frame_size)))
	return (void *)p;
      df = df->next;
    }
  }
#endif

  if (as->safe_start_address != as->safe_end_address)
    if ((p < as->safe_start_address)
	|| (p >= as->safe_end_address)) {
      as->saw_bad_ptr = 1;
      return safe_space;
    }

  return (void *)p;
}

#if UNW_DEBUG
int unwi_debug_level = 100;
#endif

unw_word_t unw_get_ip(unw_cursor_t *c)
{
  return tdep_get_ip(((struct cursor *)c));
}

unw_word_t unw_get_frame_pointer(unw_cursor_t *c)
{
#ifdef UNW_ARM
# define JIT_FRAME_POINTER_ID 11
#endif
#ifdef UNW_X86_64
# define JIT_FRAME_POINTER_ID 6 /* = BP */
#endif
#ifdef PLAIN_X86
# define JIT_FRAME_POINTER_ID 5 /* = BP */
#endif
  return *(unw_word_t *)safe_pointer(((struct cursor *)c)->dwarf.as,
				     ((struct cursor *)c)->dwarf.loc[JIT_FRAME_POINTER_ID].val);
}

#if defined(PLAIN_X86)
void unw_manual_step(unw_cursor_t *_c, 
		     void *ip_addr,
		     void *bp_addr,
		     void *sp_addr,
		     void *bx_addr,
		     void *_si_addr,
		     void *di_addr)
{
  struct cursor *c = (struct cursor *)_c;

  c->dwarf.loc[3].val = (unw_word_t)bx_addr;
  c->dwarf.loc[4].val = (unw_word_t)sp_addr;
  c->dwarf.loc[5].val = (unw_word_t)bp_addr;
  c->dwarf.loc[6].val = (unw_word_t)_si_addr;
  c->dwarf.loc[7].val = (unw_word_t)di_addr;
  c->dwarf.loc[8].val = (unw_word_t)ip_addr;

  c->dwarf.ip = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)ip_addr);
  c->dwarf.cfa = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)sp_addr);
  c->dwarf.ret_addr_column = UNW_TDEP_IP;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;
}
#endif
#ifdef UNW_X86_64
void unw_manual_step(unw_cursor_t *_c, 
		     void *ip_addr,
		     void *bp_addr,
		     void *sp_addr,
		     void *bx_addr,
		     void *r12_addr,
		     void *r13_addr)
{
  struct cursor *c = (struct cursor *)_c;

  c->dwarf.loc[3].val = (unw_word_t)bx_addr;
  c->dwarf.loc[6].val = (unw_word_t)bp_addr;
  c->dwarf.loc[7].val = (unw_word_t)sp_addr;
  c->dwarf.loc[12].val = (unw_word_t)r12_addr;
  c->dwarf.loc[13].val = (unw_word_t)r13_addr;
  c->dwarf.loc[16].val = (unw_word_t)ip_addr;

  c->dwarf.ip = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)ip_addr);
  c->dwarf.cfa = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)sp_addr);
  c->dwarf.ret_addr_column = UNW_TDEP_IP;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;
}
#endif
#ifdef UNW_ARM
void unw_manual_step(unw_cursor_t *_c, 
		     void *ip_addr,
		     void *sp_addr,
                     void *r0_addr, void *r1_addr, void *r2_addr, void *r3_addr, 
                     void *r4_addr, void *r5_addr, void *r6_addr, void *r7_addr,
                     void *fp_addr)
{
  struct cursor *c = (struct cursor *)_c;

  c->dwarf.loc[0].val = (unw_word_t)r0_addr;
  c->dwarf.loc[1].val = (unw_word_t)r1_addr;
  c->dwarf.loc[2].val = (unw_word_t)r2_addr;
  c->dwarf.loc[3].val = (unw_word_t)r3_addr;
  c->dwarf.loc[4].val = (unw_word_t)r4_addr;
  c->dwarf.loc[5].val = (unw_word_t)r5_addr;
  c->dwarf.loc[6].val = (unw_word_t)r6_addr;
  c->dwarf.loc[7].val = (unw_word_t)r7_addr;

  c->dwarf.loc[UNW_ARM_RIP].val = (unw_word_t)ip_addr;
  c->dwarf.loc[UNW_ARM_RSP].val = (unw_word_t)sp_addr;
  c->dwarf.loc[UNW_ARM_R11].val = (unw_word_t)fp_addr;

  c->dwarf.ip = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)ip_addr);
  c->dwarf.cfa = *(unw_word_t *)safe_pointer(c->dwarf.as, (unw_word_t)sp_addr);
  c->dwarf.ret_addr_column = UNW_TDEP_IP;
  c->dwarf.pi_valid = 0;
  c->dwarf.hint = 0;
  c->dwarf.prev_rs = 0;
}
#endif

#endif
