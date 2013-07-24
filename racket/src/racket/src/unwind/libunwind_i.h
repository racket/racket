/* libunwind - a platform-independent unwind library
   Copyright (C) 2001-2005 Hewlett-Packard Co
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

/* This files contains libunwind-internal definitions which are
   subject to frequent change and are not to be exposed to
   libunwind-users.  */

#ifndef libunwind_i_h
#define libunwind_i_h

#ifdef HAVE___THREAD
  /* For now, turn off per-thread caching.  It uses up too much TLS
     memory per thread even when the thread never uses libunwind at
     all.  */
# undef HAVE___THREAD
#endif

/* Platform-independent libunwind-internal declarations.  */

#include <sys/types.h>	/* HP-UX needs this before include of pthread.h */

#include <assert.h>
#include "libunwind.h"
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* FIXME: hard-wired */
# define __LITTLE_ENDIAN	1234
# define __BIG_ENDIAN		4321
# define __BYTE_ORDER __LITTLE_ENDIAN


#   define __BYTE_ORDER __LITTLE_ENDIAN

#ifdef __GNUC__
# define UNUSED		__attribute__((unused))
# define NORETURN	__attribute__((noreturn))
# define ALIAS(name)	__attribute__((alias (#name)))
# if (__GNUC__ > 3) || (__GNUC__ == 3 && __GNUC_MINOR__ > 2)
#  define ALWAYS_INLINE	inline __attribute__((always_inline))
#  define HIDDEN	__attribute__((visibility ("hidden")))
#  define PROTECTED	__attribute__((visibility ("protected")))
# else
#  define ALWAYS_INLINE
#  define HIDDEN
#  define PROTECTED
# endif
# if (__GNUC__ >= 3)
#  define likely(x)	__builtin_expect ((x), 1)
#  define unlikely(x)	__builtin_expect ((x), 0)
# else
#  define likely(x)	(x)
#  define unlikely(x)	(x)
# endif
#else
# define ALWAYS_INLINE
# define UNUSED
# define NORETURN
# define ALIAS(name)
# define HIDDEN
# define PROTECTED
# define likely(x)	(x)
# define unlikely(x)	(x)
#endif

#undef HIDDEN
#define HIDDEN static

#define ARRAY_SIZE(a)	(sizeof (a) / sizeof ((a)[0]))

#define UNWI_OBJ(fn)	  UNW_PASTE(UNW_PREFIX,UNW_PASTE(I,fn))
#define UNWI_ARCH_OBJ(fn) UNW_PASTE(UNW_PASTE(UNW_PASTE(_UI,UNW_TARGET),_), fn)

#ifndef UW_NO_SYNC

/* Make it easy to write thread-safe code which may or may not be
   linked against libpthread.  The macros below can be used
   unconditionally and if -lpthread is around, they'll call the
   corresponding routines otherwise, they do nothing.  */

#pragma weak pthread_mutex_init
#pragma weak pthread_mutex_lock
#pragma weak pthread_mutex_unlock

#define mutex_init(l)							\
	(pthread_mutex_init != 0 ? pthread_mutex_init ((l), 0) : 0)
#define mutex_lock(l)							\
	(pthread_mutex_lock != 0 ? pthread_mutex_lock (l) : 0)
#define mutex_unlock(l)							\
	(pthread_mutex_unlock != 0 ? pthread_mutex_unlock (l) : 0)

#ifdef HAVE_ATOMIC_OPS_H
# include <atomic_ops.h>
static inline int
cmpxchg_ptr (void *addr, void *old, void *new)
{
  union
    {
      void *vp;
      AO_t *aop;
    }
  u;

  u.vp = addr;
  return AO_compare_and_swap(u.aop, (AO_t) old, (AO_t) new);
}
# define fetch_and_add1(_ptr)		AO_fetch_and_add1(_ptr)
   /* GCC 3.2.0 on HP-UX crashes on cmpxchg_ptr() */
#  if !(defined(__hpux) && __GNUC__ == 3 && __GNUC_MINOR__ == 2)
#   define HAVE_CMPXCHG
#  endif
# define HAVE_FETCH_AND_ADD1
#else
# ifdef HAVE_IA64INTRIN_H
#  include <ia64intrin.h>
static inline int
cmpxchg_ptr (void *addr, void *old, void *new)
{
  union
    {
      void *vp;
      long *vlp;
    }
  u;

  u.vp = addr;
  return __sync_bool_compare_and_swap(u.vlp, (long) old, (long) new);
}
#  define fetch_and_add1(_ptr)		__sync_fetch_and_add(_ptr, 1)
#  define HAVE_CMPXCHG
#  define HAVE_FETCH_AND_ADD1
# endif
#endif
#define atomic_read(ptr)	(*(ptr))

#define unwi_full_mask    UNWI_ARCH_OBJ(full_mask)

/* Type of a mask that can be used to inhibit preemption.  At the
   userlevel, preemption is caused by signals and hence sigset_t is
   appropriate.  In constrast, the Linux kernel uses "unsigned long"
   to hold the processor "flags" instead.  */
typedef sigset_t intrmask_t;

extern intrmask_t unwi_full_mask;

#define define_lock(name) \
  pthread_mutex_t name = PTHREAD_MUTEX_INITIALIZER
#define lock_init(l)		mutex_init (l)
#define lock_acquire(l,m)				\
do {							\
  sigprocmask (SIG_SETMASK, &unwi_full_mask, &(m));	\
  mutex_lock (l);					\
} while (0)
#define lock_release(l,m)			\
do {						\
  mutex_unlock (l);				\
  sigprocmask (SIG_SETMASK, &(m), NULL);	\
} while (0)

#else  /* UW_NO_SYNC */
# define atomic_read(ptr)	(*(ptr))
typedef int intrmask_t;
#endif /* UW_NO_SYNC */

#define GET_MEMORY(mem, size_in_bytes)				    \
do {									    \
  /* Hopefully, mmap() goes straight through to a system call stub...  */   \
  mem = mmap (0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, \
	      -1, 0);							    \
  if (mem == MAP_FAILED)						    \
    mem = NULL;								    \
} while (0)

#define UNW_DEBUG 0
#if UNW_DEBUG
#define unwi_debug_level		UNWI_ARCH_OBJ(debug_level)
extern int unwi_debug_level;

# include <stdio.h>
# define Debug(level,format...)						\
do {									\
  if (unwi_debug_level >= level)					\
    {									\
      int _n = level;							\
      if (_n > 16)							\
	_n = 16;							\
      fprintf (stderr, "%*c>%s: ", _n, ' ', __FUNCTION__);		\
      fprintf (stderr, format);						\
    }									\
} while (0)
# define Dprintf(format...) 	    fprintf (stderr, format)
# ifdef __GNUC__
#  undef inline
#  define inline	UNUSED
# endif
#else
# define Debug(level,format...)
# define Dprintf(format...)
#endif

static ALWAYS_INLINE int
print_error (const char *string)
{
  return write (2, string, strlen (string));
}

#define mi_init		UNWI_ARCH_OBJ(mi_init)

extern void mi_init (void);	/* machine-independent initializations */
extern unw_word_t _U_dyn_info_list_addr (void);

/* This is needed/used by ELF targets only.  */

struct elf_image
  {
    void *image;		/* pointer to mmap'd image */
    size_t size;		/* (file-) size of the image */
  };

/* Target-dependent definitions that are internal to libunwind but need
   to be shared with target-independent code.  */

/*XXXXXXXXXXXXXXXXXXXXXXXXX Start unwind_dl.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

#ifdef OS_X

#define elf_w(x) x
#define Elf_W(x) x

#include <mach-o/loader.h>
#include <mach-o/dyld.h>

#define PT_LOAD LC_SEGMENT
#define PT_GNU_EH_FRAME -1
#define PT_DYNAMIC -1

#define DT_NULL   0
#define DT_PLTGOT 1

#define DW_EH_VERSION 1

typedef long Addr;
typedef struct {
  long p_type;
  Addr p_vaddr;
  long p_memsz;
  long p_filesz;
} Phdr;

typedef struct {
  long d_tag;
  struct { long d_ptr; } d_un;
} Dyn;

struct dl_phdr_info {
  Phdr *dlpi_phdr;
  Addr dlpi_addr;
  long dlpi_phnum;
  char *dlpi_name;
};

typedef int (*DL_Iter_Callback)(struct dl_phdr_info *info, size_t size, void *ptr);
int dl_iterate_phdr (DL_Iter_Callback callback, void *p);

#else

#define __USE_GNU
#include <link.h>
#undef __USE_GNU

#define elf_w(x) elf64_ ## x
#define Elf_W(x) ElfW(x)

typedef int (*DL_Iter_Callback)(struct dl_phdr_info *info, size_t size, void *ptr);

#endif

extern int elf_w(get_proc_name) (pid_t pid, unw_word_t ip,
				 char *buf, size_t len,
				 unw_word_t *offp);

/*XXXXXXXXXXXXXXXXXXXXXXXXX End unwind_dl.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

/*XXXXXXXXXXXXXXXXXXXXXXXXX Start dwarf.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

struct dwarf_cursor;	/* forward-declaration */

/* This matches the value used by GCC (see
   gcc/config/i386.h:DWARF_FRAME_REGISTERS), which leaves plenty of
   room for expansion.  */
#define DWARF_NUM_PRESERVED_REGS	17

#ifdef PLAIN_X86
#define DWARF_REGNUM_MAP_LENGTH		19
#else
#define DWARF_REGNUM_MAP_LENGTH		17
#endif

/* Return TRUE if the ADDR_SPACE uses big-endian byte-order.  */
#define dwarf_is_big_endian(addr_space)	0

/* Convert a pointer to a dwarf_cursor structure to a pointer to
   unw_cursor_t.  */
#define dwarf_to_cursor(c)	((unw_cursor_t *) (c))

typedef struct dwarf_loc
  {
    unw_word_t val;
  }
dwarf_loc_t;

/* DWARF expression opcodes.  */

typedef enum
  {
    DW_OP_addr			= 0x03,
    DW_OP_deref			= 0x06,
    DW_OP_const1u		= 0x08,
    DW_OP_const1s		= 0x09,
    DW_OP_const2u		= 0x0a,
    DW_OP_const2s		= 0x0b,
    DW_OP_const4u		= 0x0c,
    DW_OP_const4s		= 0x0d,
    DW_OP_const8u		= 0x0e,
    DW_OP_const8s		= 0x0f,
    DW_OP_constu		= 0x10,
    DW_OP_consts		= 0x11,
    DW_OP_dup			= 0x12,
    DW_OP_drop			= 0x13,
    DW_OP_over			= 0x14,
    DW_OP_pick			= 0x15,
    DW_OP_swap			= 0x16,
    DW_OP_rot			= 0x17,
    DW_OP_xderef		= 0x18,
    DW_OP_abs			= 0x19,
    DW_OP_and			= 0x1a,
    DW_OP_div			= 0x1b,
    DW_OP_minus			= 0x1c,
    DW_OP_mod			= 0x1d,
    DW_OP_mul			= 0x1e,
    DW_OP_neg			= 0x1f,
    DW_OP_not			= 0x20,
    DW_OP_or			= 0x21,
    DW_OP_plus			= 0x22,
    DW_OP_plus_uconst		= 0x23,
    DW_OP_shl			= 0x24,
    DW_OP_shr			= 0x25,
    DW_OP_shra			= 0x26,
    DW_OP_xor			= 0x27,
    DW_OP_skip			= 0x2f,
    DW_OP_bra			= 0x28,
    DW_OP_eq			= 0x29,
    DW_OP_ge			= 0x2a,
    DW_OP_gt			= 0x2b,
    DW_OP_le			= 0x2c,
    DW_OP_lt			= 0x2d,
    DW_OP_ne			= 0x2e,
    DW_OP_lit0			= 0x30,
    DW_OP_lit1,  DW_OP_lit2,  DW_OP_lit3,  DW_OP_lit4,  DW_OP_lit5,
    DW_OP_lit6,  DW_OP_lit7,  DW_OP_lit8,  DW_OP_lit9,  DW_OP_lit10,
    DW_OP_lit11, DW_OP_lit12, DW_OP_lit13, DW_OP_lit14, DW_OP_lit15,
    DW_OP_lit16, DW_OP_lit17, DW_OP_lit18, DW_OP_lit19, DW_OP_lit20,
    DW_OP_lit21, DW_OP_lit22, DW_OP_lit23, DW_OP_lit24, DW_OP_lit25,
    DW_OP_lit26, DW_OP_lit27, DW_OP_lit28, DW_OP_lit29, DW_OP_lit30,
    DW_OP_lit31,
    DW_OP_reg0			= 0x50,
    DW_OP_reg1,  DW_OP_reg2,  DW_OP_reg3,  DW_OP_reg4,  DW_OP_reg5,
    DW_OP_reg6,  DW_OP_reg7,  DW_OP_reg8,  DW_OP_reg9,  DW_OP_reg10,
    DW_OP_reg11, DW_OP_reg12, DW_OP_reg13, DW_OP_reg14, DW_OP_reg15,
    DW_OP_reg16, DW_OP_reg17, DW_OP_reg18, DW_OP_reg19, DW_OP_reg20,
    DW_OP_reg21, DW_OP_reg22, DW_OP_reg23, DW_OP_reg24, DW_OP_reg25,
    DW_OP_reg26, DW_OP_reg27, DW_OP_reg28, DW_OP_reg29, DW_OP_reg30,
    DW_OP_reg31,
    DW_OP_breg0			= 0x70,
    DW_OP_breg1,  DW_OP_breg2,  DW_OP_breg3,  DW_OP_breg4,  DW_OP_breg5,
    DW_OP_breg6,  DW_OP_breg7,  DW_OP_breg8,  DW_OP_breg9,  DW_OP_breg10,
    DW_OP_breg11, DW_OP_breg12, DW_OP_breg13, DW_OP_breg14, DW_OP_breg15,
    DW_OP_breg16, DW_OP_breg17, DW_OP_breg18, DW_OP_breg19, DW_OP_breg20,
    DW_OP_breg21, DW_OP_breg22, DW_OP_breg23, DW_OP_breg24, DW_OP_breg25,
    DW_OP_breg26, DW_OP_breg27, DW_OP_breg28, DW_OP_breg29, DW_OP_breg30,
    DW_OP_breg31,
    DW_OP_regx			= 0x90,
    DW_OP_fbreg			= 0x91,
    DW_OP_bregx			= 0x92,
    DW_OP_piece			= 0x93,
    DW_OP_deref_size		= 0x94,
    DW_OP_xderef_size		= 0x95,
    DW_OP_nop			= 0x96,
    DW_OP_push_object_address	= 0x97,
    DW_OP_call2			= 0x98,
    DW_OP_call4			= 0x99,
    DW_OP_call_ref		= 0x9a,
    DW_OP_lo_user		= 0xe0,
    DW_OP_hi_user		= 0xff
  }
dwarf_expr_op_t;

#define DWARF_CIE_VERSION	3	/* GCC emits version 1??? */

#define DWARF_CFA_OPCODE_MASK	0xc0
#define DWARF_CFA_OPERAND_MASK	0x3f

typedef enum
  {
    DW_CFA_advance_loc		= 0x40,
    DW_CFA_offset		= 0x80,
    DW_CFA_restore		= 0xc0,
    DW_CFA_nop			= 0x00,
    DW_CFA_set_loc		= 0x01,
    DW_CFA_advance_loc1		= 0x02,
    DW_CFA_advance_loc2		= 0x03,
    DW_CFA_advance_loc4		= 0x04,
    DW_CFA_offset_extended	= 0x05,
    DW_CFA_restore_extended	= 0x06,
    DW_CFA_undefined		= 0x07,
    DW_CFA_same_value		= 0x08,
    DW_CFA_register		= 0x09,
    DW_CFA_remember_state	= 0x0a,
    DW_CFA_restore_state	= 0x0b,
    DW_CFA_def_cfa		= 0x0c,
    DW_CFA_def_cfa_register	= 0x0d,
    DW_CFA_def_cfa_offset	= 0x0e,
    DW_CFA_def_cfa_expression	= 0x0f,
    DW_CFA_expression		= 0x10,
    DW_CFA_offset_extended_sf	= 0x11,
    DW_CFA_def_cfa_sf		= 0x12,
    DW_CFA_def_cfa_offset_sf	= 0x13,
    DW_CFA_lo_user		= 0x1c,
    DW_CFA_MIPS_advance_loc8	= 0x1d,
    DW_CFA_GNU_window_save	= 0x2d,
    DW_CFA_GNU_args_size	= 0x2e,
    DW_CFA_GNU_negative_offset_extended	= 0x2f,
    DW_CFA_hi_user		= 0x3c
  }
dwarf_cfa_t;

/* DWARF Pointer-Encoding (PEs).

   Pointer-Encodings were invented for the GCC exception-handling
   support for C++, but they represent a rather generic way of
   describing the format in which an address/pointer is stored and
   hence we include the definitions here, in the main dwarf.h file.
   The Pointer-Encoding format is partially documented in Linux Base
   Spec v1.3 (http://www.linuxbase.org/spec/).  The rest is reverse
   engineered from GCC.

*/
#define DW_EH_PE_FORMAT_MASK	0x0f	/* format of the encoded value */
#define DW_EH_PE_APPL_MASK	0x70	/* how the value is to be applied */
/* Flag bit.  If set, the resulting pointer is the address of the word
   that contains the final address.  */
#define DW_EH_PE_indirect	0x80

/* Pointer-encoding formats: */
#define DW_EH_PE_omit		0xff
#define DW_EH_PE_ptr		0x00	/* pointer-sized unsigned value */
#define DW_EH_PE_uleb128	0x01	/* unsigned LE base-128 value */
#define DW_EH_PE_udata2		0x02	/* unsigned 16-bit value */
#define DW_EH_PE_udata4		0x03	/* unsigned 32-bit value */
#define DW_EH_PE_udata8		0x04	/* unsigned 64-bit value */
#define DW_EH_PE_sleb128	0x09	/* signed LE base-128 value */
#define DW_EH_PE_sdata2		0x0a	/* signed 16-bit value */
#define DW_EH_PE_sdata4		0x0b	/* signed 32-bit value */
#define DW_EH_PE_sdata8		0x0c	/* signed 64-bit value */

/* Pointer-encoding application: */
#define DW_EH_PE_absptr		0x00	/* absolute value */
#define DW_EH_PE_pcrel		0x10	/* rel. to addr. of encoded value */
#define DW_EH_PE_textrel	0x20	/* text-relative (GCC-specific???) */
#define DW_EH_PE_datarel	0x30	/* data-relative */
/* The following are not documented by LSB v1.3, yet they are used by
   GCC, presumably they aren't documented by LSB since they aren't
   used on Linux:  */
#define DW_EH_PE_funcrel	0x40	/* start-of-procedure-relative */
#define DW_EH_PE_aligned	0x50	/* aligned pointer */

typedef enum
  {
    DWARF_WHERE_UNDEF,		/* register isn't saved at all */
    DWARF_WHERE_SAME,		/* register has same value as in prev. frame */
    DWARF_WHERE_CFAREL,		/* register saved at CFA-relative address */
    DWARF_WHERE_REG,		/* register saved in another register */
    DWARF_WHERE_EXPR,		/* register saved */
  }
dwarf_where_t;

typedef struct
  {
    dwarf_where_t where;	/* how is the register saved? */
    unw_word_t val;		/* where it's saved */
  }
dwarf_save_loc_t;

/* For uniformity, we'd like to treat the CFA save-location like any
   other register save-location, but this doesn't quite work, because
   the CFA can be expressed as a (REGISTER,OFFSET) pair.  To handle
   this, we use two dwarf_save_loc structures to describe the CFA.
   The first one (CFA_REG_COLUMN), tells us where the CFA is saved.
   In the case of DWARF_WHERE_EXPR, the CFA is defined by a DWARF
   location expression whose address is given by member "val".  In the
   case of DWARF_WHERE_REG, member "val" gives the number of the
   base-register and the "val" member of DWARF_CFA_OFF_COLUMN gives
   the offset value.  */
#define DWARF_CFA_REG_COLUMN	DWARF_NUM_PRESERVED_REGS
#define DWARF_CFA_OFF_COLUMN	(DWARF_NUM_PRESERVED_REGS + 1)

typedef struct dwarf_reg_state
  {
    struct dwarf_reg_state *next;	/* for rs_stack */
    dwarf_save_loc_t reg[DWARF_NUM_PRESERVED_REGS + 2];
    unw_word_t ip;		          /* ip this rs is for */
    unw_word_t ret_addr_column;           /* indicates which column in the rule table represents return address */
    unsigned short lru_chain;	  /* used for least-recently-used chain */
    unsigned short coll_chain;	/* used for hash collisions */
    unsigned short hint;	      /* hint for next rs to try (or -1) */
    char valid, signal_frame;
  }
dwarf_reg_state_t;

typedef struct dwarf_cie_info
  {
    unw_word_t cie_instr_start;	/* start addr. of CIE "initial_instructions" */
    unw_word_t cie_instr_end;	/* end addr. of CIE "initial_instructions" */
    unw_word_t fde_instr_start;	/* start addr. of FDE "instructions" */
    unw_word_t fde_instr_end;	/* end addr. of FDE "instructions" */
    unw_word_t code_align;	/* code-alignment factor */
    unw_word_t data_align;	/* data-alignment factor */
    unw_word_t ret_addr_column;	/* column of return-address register */
    unw_word_t handler;		/* address of personality-routine */
    uint16_t abi;
    uint16_t tag;
    uint8_t fde_encoding;
    uint8_t lsda_encoding;
    unsigned int sized_augmentation : 1;
    unsigned int have_abi_marker : 1;
    unsigned int signal_frame : 1;
  }
dwarf_cie_info_t;

typedef struct dwarf_state_record
  {
    unsigned char fde_encoding;
    unw_word_t args_size;

    dwarf_reg_state_t rs_initial;	/* reg-state after CIE instructions */
    dwarf_reg_state_t rs_current;	/* current reg-state */
  }
dwarf_state_record_t;

typedef struct dwarf_cursor
  {
    void *as_arg;		/* argument to address-space callbacks */
    unw_addr_space_t as;	/* reference to per-address-space info */

    unw_word_t cfa;	/* canonical frame address; aka frame-/stack-pointer */
    unw_word_t ip;		/* instruction pointer */
    unw_word_t args_size;	/* size of arguments */
    unw_word_t ret_addr_column;	/* column for return-address */
    unw_word_t eh_args[UNW_TDEP_NUM_EH_REGS];
    unsigned int eh_valid_mask;

    dwarf_loc_t loc[DWARF_NUM_PRESERVED_REGS];

    unsigned int pi_valid :1;	/* is proc_info valid? */
    unw_proc_info_t pi;		/* info about current procedure */

    short hint; /* faster lookup of the rs cache */
    short prev_rs;

    int use_prev_instr;
  }
dwarf_cursor_t;

#define DWARF_LOG_UNW_CACHE_SIZE	7
#define DWARF_UNW_CACHE_SIZE	(1 << DWARF_LOG_UNW_CACHE_SIZE)

#define DWARF_LOG_UNW_HASH_SIZE	(DWARF_LOG_UNW_CACHE_SIZE + 1)
#define DWARF_UNW_HASH_SIZE	(1 << DWARF_LOG_UNW_HASH_SIZE)

typedef unsigned char unw_hash_index_t;

struct dwarf_rs_cache
  {
    unsigned short lru_head;	/* index of lead-recently used rs */
    unsigned short lru_tail;	/* index of most-recently used rs */

    /* hash table that maps instruction pointer to rs index: */
    unsigned short hash[DWARF_UNW_HASH_SIZE];

    uint32_t generation;	/* generation number */

    /* rs cache: */
    dwarf_reg_state_t buckets[DWARF_UNW_CACHE_SIZE];
  };

/* Convenience macros: */
#define dwarf_init			UNW_ARCH_OBJ (dwarf_init)
#define dwarf_find_proc_info		UNW_OBJ (dwarf_find_proc_info)
#define dwarf_search_unwind_table	UNW_OBJ (dwarf_search_unwind_table)
#define dwarf_put_unwind_info		UNW_OBJ (dwarf_put_unwind_info)
#define dwarf_put_unwind_info		UNW_OBJ (dwarf_put_unwind_info)
#define dwarf_eval_expr			UNW_OBJ (dwarf_eval_expr)
#define dwarf_extract_proc_info_from_fde \
		UNW_OBJ (dwarf_extract_proc_info_from_fde)
#define dwarf_find_save_locs		UNW_OBJ (dwarf_find_save_locs)
#define dwarf_create_state_record	UNW_OBJ (dwarf_create_state_record)
#define dwarf_make_proc_info		UNW_OBJ (dwarf_make_proc_info)
#define dwarf_read_encoded_pointer	UNW_OBJ (dwarf_read_encoded_pointer)
#define dwarf_step			UNW_OBJ (dwarf_step)

HIDDEN int dwarf_find_proc_info (unw_addr_space_t as, unw_word_t ip,
				 unw_proc_info_t *pi,
				 int need_unwind_info, void *arg);
HIDDEN int dwarf_search_unwind_table (unw_addr_space_t as,
				      unw_word_t ip,
				      unw_dyn_info_t *di,
				      unw_proc_info_t *pi,
				      int need_unwind_info, void *arg);
HIDDEN int dwarf_eval_expr (struct dwarf_cursor *c, unw_word_t *addr,
			    unw_word_t len, unw_word_t *valp,
			    int *is_register);
HIDDEN int dwarf_extract_proc_info_from_fde (unw_addr_space_t as,
					     unw_accessors_t *a,
					     unw_word_t *fde_addr,
					     unw_proc_info_t *pi,
					     int need_unwind_info,
                                             unw_word_t base,
					     void *arg);
HIDDEN int dwarf_find_save_locs (struct dwarf_cursor *c);
HIDDEN int dwarf_read_encoded_pointer (unw_addr_space_t as,
				       unw_accessors_t *a,
				       unw_word_t *addr,
				       unsigned char encoding,
				       const unw_proc_info_t *pi,
				       unw_word_t *valp, void *arg);
HIDDEN int dwarf_step (struct dwarf_cursor *c);

/*XXXXXXXXXXXXXXXXXXXXXXXXX End dwarf.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

#ifdef CONFIG_DEBUG_FRAME
struct unw_debug_frame_list
  {
    /* The start (inclusive) and end (exclusive) of the described region.  */
    unw_word_t start;
    unw_word_t end;
    /* The debug frame itself.  */
    char *debug_frame;
    size_t debug_frame_size;
    /* Index (for binary search).  */
    struct table_entry *index;
    size_t index_size;
    /* Pointer to next descriptor.  */
    struct unw_debug_frame_list *next;
  };
#endif

struct unw_addr_space
  {
    void *mem_pool;
    struct unw_accessors acc;
    unw_caching_policy_t caching_policy;
#ifdef HAVE_ATOMIC_OPS_H
    AO_t cache_generation;
#else
    uint32_t cache_generation;
#endif
    struct dwarf_rs_cache global_cache;
#ifdef CONFIG_DEBUG_FRAME
    struct unw_debug_frame_list *debug_frames;
#endif
    unw_word_t safe_start_address, safe_end_address;
    long num_safe_addresses;
    unw_word_t *safe_start_addresses, *safe_end_addresses;
    int saw_bad_ptr;
   };

struct cursor
  {
    struct dwarf_cursor dwarf;		/* must be first */

    /* Format of sigcontext structure and address at which it is
       stored: */
    enum
      {
	X86_SCF_NONE,			/* no signal frame encountered */
	X86_SCF_LINUX_SIGFRAME,		/* classic x86 sigcontext */
	X86_SCF_LINUX_RT_SIGFRAME,	/* POSIX ucontext_t */
        ARM_SCF_NONE
      }
    sigcontext_format;
    unw_word_t sigcontext_addr;
  };

#define DWARF_GET_LOC(l)	((l).val)

# define DWARF_NULL_LOC		DWARF_LOC (0, 0)
# define DWARF_IS_NULL_LOC(l)	(DWARF_GET_LOC (l) == 0)
# define DWARF_LOC(r, t)	((dwarf_loc_t) { .val = (r) })
# define DWARF_IS_REG_LOC(l)	0
# define DWARF_REG_LOC(c,r)	(DWARF_LOC((unw_word_t)			     \
				 tdep_uc_addr((c)->as_arg, (r)), 0))
# define DWARF_MEM_LOC(c,m)	DWARF_LOC ((m), 0)
# define DWARF_FPREG_LOC(c,r)	(DWARF_LOC((unw_word_t)			     \
				 tdep_uc_addr((c)->as_arg, (r)), 0))

static void *safe_pointer(unw_addr_space_t, unw_word_t);

static inline int
dwarf_getfp (struct dwarf_cursor *c, dwarf_loc_t loc, unw_fpreg_t *val)
{
  if (!DWARF_GET_LOC (loc))
    return -1;
  *val = *(unw_fpreg_t *) safe_pointer(c->as, DWARF_GET_LOC (loc));
  return 0;
}

static inline int
dwarf_putfp (struct dwarf_cursor *c, dwarf_loc_t loc, unw_fpreg_t val)
{
  if (!DWARF_GET_LOC (loc))
    return -1;
  *(unw_fpreg_t *) safe_pointer(c->as, DWARF_GET_LOC (loc)) = val;
  return 0;
}

static inline int
dwarf_get (struct dwarf_cursor *c, dwarf_loc_t loc, unw_word_t *val)
{
  if (!DWARF_GET_LOC (loc))
    return -1;
  *val = *(unw_word_t *) safe_pointer(c->as, DWARF_GET_LOC (loc));
  return 0;
}

static inline int
dwarf_put (struct dwarf_cursor *c, dwarf_loc_t loc, unw_word_t val)
{
  if (!DWARF_GET_LOC (loc))
    return -1;
  *(unw_word_t *) safe_pointer(c->as, DWARF_GET_LOC (loc)) = val;
  return 0;
}

#define tdep_needs_initialization	UNW_OBJ(needs_initialization)
#define tdep_init			UNW_OBJ(init)
/* Platforms that support UNW_INFO_FORMAT_TABLE need to define
   tdep_search_unwind_table.  */
#define tdep_search_unwind_table	dwarf_search_unwind_table
#define tdep_uc_addr			UNW_ARCH_OBJ(uc_addr)
#define tdep_get_elf_image		UNW_ARCH_OBJ(get_elf_image)
#define tdep_access_reg			UNW_OBJ(access_reg)
#define tdep_access_fpreg		UNW_OBJ(access_fpreg)

# define tdep_find_proc_info(c,ip,n)				\
	dwarf_find_proc_info((c)->as, (ip), &(c)->pi, (n),	\
				       (c)->as_arg)
# define tdep_put_unwind_info(as,pi,arg)		\
	dwarf_put_unwind_info((as), (pi), (arg))

#define tdep_get_as(c)			((c)->dwarf.as)
#define tdep_get_as_arg(c)		((c)->dwarf.as_arg)
#define tdep_get_ip(c)			((c)->dwarf.ip)
#define tdep_get_cfa(c)			((c)->dwarf.cfa)
#define tdep_big_endian(as)		0

extern int tdep_needs_initialization;

extern void tdep_init (void);
extern int tdep_search_unwind_table (unw_addr_space_t as, unw_word_t ip,
				     unw_dyn_info_t *di, unw_proc_info_t *pi,
				     int need_unwind_info, void *arg);
extern void *tdep_uc_addr (unw_context_t *uc, int reg);
extern int tdep_get_elf_image (struct elf_image *ei, pid_t pid, unw_word_t ip,
			       unsigned long *segbase, unsigned long *mapoff);
extern int tdep_access_reg (struct cursor *c, unw_regnum_t reg,
			    unw_word_t *valp, int write);
extern int tdep_access_fpreg (struct cursor *c, unw_regnum_t reg,
			      unw_fpreg_t *valp, int write);


/*XXXXXXXXXXXXXXXXXXXXXXXXX Start dwarf_i.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

#define dwarf_to_unw_regnum_map		UNW_OBJ (dwarf_to_unw_regnum_map)

HIDDEN int dwarf_to_unw_regnum(int reg);

/* In the local-only case, we can let the compiler directly access
   memory and don't need to worry about differing byte-order.  */

typedef union
  {
    int8_t s8;
    int16_t s16;
    int32_t s32;
    int64_t s64;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    unw_word_t w;
    void *ptr;
  }
  dwarf_misaligned_value_t;

static inline int
dwarf_reads8 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	      int8_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->s8;
  *addr += sizeof (mvp->s8);
  return 0;
}

static inline int
dwarf_reads16 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       int16_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->s16;
  *addr += sizeof (mvp->s16);
  return 0;
}

static inline int
dwarf_reads32 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       int32_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->s32;
  *addr += sizeof (mvp->s32);
  return 0;
}

static inline int
dwarf_reads64 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       int64_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->s64;
  *addr += sizeof (mvp->s64);
  return 0;
}

static inline int
dwarf_readu8 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	      uint8_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->u8;
  *addr += sizeof (mvp->u8);
  return 0;
}

static inline int
dwarf_readu16 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       uint16_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->u16;
  *addr += sizeof (mvp->u16);
  return 0;
}

static inline int
dwarf_readu32 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       uint32_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->u32;
  *addr += sizeof (mvp->u32);
  return 0;
}

static inline int
dwarf_readu64 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	       uint64_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->u64;
  *addr += sizeof (mvp->u64);
  return 0;
}

static inline int
dwarf_readw (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
	     unw_word_t *val, void *arg)
{
  dwarf_misaligned_value_t *mvp = (void *)safe_pointer(as, *addr);

  *val = mvp->w;
  *addr += sizeof (mvp->w);
  return 0;
}

/* Read an unsigned "little-endian base 128" value.  See Chapter 7.6
   of DWARF spec v3.  */

static inline int
dwarf_read_uleb128 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
		    unw_word_t *valp, void *arg)
{
  unw_word_t val = 0, shift = 0;
  unsigned char byte;
  int ret;

  do
    {
      if ((ret = dwarf_readu8 (as, a, addr, &byte, arg)) < 0)
	return ret;

      val |= ((unw_word_t) byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  *valp = val;
  return 0;
}

/* Read a signed "little-endian base 128" value.  See Chapter 7.6 of
   DWARF spec v3.  */

static inline int
dwarf_read_sleb128 (unw_addr_space_t as, unw_accessors_t *a, unw_word_t *addr,
		    unw_word_t *valp, void *arg)
{
  unw_word_t val = 0, shift = 0;
  unsigned char byte;
  int ret;

  do
    {
      if ((ret = dwarf_readu8 (as, a, addr, &byte, arg)) < 0)
	return ret;

      val |= ((unw_word_t) byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  if (shift < 8 * sizeof (unw_word_t) && (byte & 0x40) != 0)
    /* sign-extend negative value */
    val |= ((unw_word_t) -1) << shift;

  *valp = val;
  return 0;
}

static ALWAYS_INLINE int
dwarf_read_encoded_pointer_inlined (unw_addr_space_t as, unw_accessors_t *a,
				    unw_word_t *addr, unsigned char encoding,
				    const unw_proc_info_t *pi,
				    unw_word_t *valp, void *arg)
{
  unw_word_t val, initial_addr = *addr;
  uint16_t uval16;
  uint32_t uval32;
  uint64_t uval64;
  int16_t sval16;
  int32_t sval32;
  int64_t sval64;
  int ret;

  /* DW_EH_PE_omit and DW_EH_PE_aligned don't follow the normal
     format/application encoding.  Handle them first.  */
  if (encoding == DW_EH_PE_omit)
    {
      *valp = 0;
      return 0;
    }
  else if (encoding == DW_EH_PE_aligned)
    {
      *addr = (initial_addr + sizeof (unw_word_t) - 1) & -sizeof (unw_word_t);
      return dwarf_readw (as, a, addr, valp, arg);
    }

  switch (encoding & DW_EH_PE_FORMAT_MASK)
    {
    case DW_EH_PE_ptr:
      if ((ret = dwarf_readw (as, a, addr, &val, arg)) < 0)
	return ret;
      break;

    case DW_EH_PE_uleb128:
      if ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0)
	return ret;
      break;

    case DW_EH_PE_udata2:
      if ((ret = dwarf_readu16 (as, a, addr, &uval16, arg)) < 0)
	return ret;
      val = uval16;
      break;

    case DW_EH_PE_udata4:
      if ((ret = dwarf_readu32 (as, a, addr, &uval32, arg)) < 0)
	return ret;
      val = uval32;
      break;

    case DW_EH_PE_udata8:
      if ((ret = dwarf_readu64 (as, a, addr, &uval64, arg)) < 0)
	return ret;
      val = uval64;
      break;

    case DW_EH_PE_sleb128:
      if ((ret = dwarf_read_uleb128 (as, a, addr, &val, arg)) < 0)
	return ret;
      break;

    case DW_EH_PE_sdata2:
      if ((ret = dwarf_reads16 (as, a, addr, &sval16, arg)) < 0)
	return ret;
      val = sval16;
      break;

    case DW_EH_PE_sdata4:
      if ((ret = dwarf_reads32 (as, a, addr, &sval32, arg)) < 0)
	return ret;
      val = sval32;
      break;

    case DW_EH_PE_sdata8:
      if ((ret = dwarf_reads64 (as, a, addr, &sval64, arg)) < 0)
	return ret;
      val = sval64;
      break;

    default:
      Debug (1, "unexpected encoding format 0x%x\n",
	     encoding & DW_EH_PE_FORMAT_MASK);
      return -UNW_EINVAL;
    }

  if (val == 0)
    {
      /* 0 is a special value and always absolute.  */
      *valp = 0;
      return 0;
    }

  switch (encoding & DW_EH_PE_APPL_MASK)
    {
    case DW_EH_PE_absptr:
      break;

    case DW_EH_PE_pcrel:
      val += initial_addr;
      break;

    case DW_EH_PE_datarel:
      /* XXX For now, assume that data-relative addresses are relative
         to the global pointer.  */
      val += pi->gp;
      break;

    case DW_EH_PE_funcrel:
      val += pi->start_ip;
      break;

    case DW_EH_PE_textrel:
      /* XXX For now we don't support text-rel values.  If there is a
         platform which needs this, we probably would have to add a
         "segbase" member to unw_proc_info_t.  */
    default:
      Debug (1, "unexpected application type 0x%x\n",
	     encoding & DW_EH_PE_APPL_MASK);
      return -UNW_EINVAL;
    }

  if (encoding & DW_EH_PE_indirect)
    {
      unw_word_t indirect_addr = val;

      if ((ret = dwarf_readw (as, a, &indirect_addr, &val, arg)) < 0)
	return ret;
    }

  *valp = val;
  return 0;
}

/*XXXXXXXXXXXXXXXXXXXXXXXXX End dwarf_i.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

/*XXXXXXXXXXXXXXXXXXXXXXXXX Start dwarf-eh.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

/* This header file defines the format of a DWARF exception-header
   section (.eh_frame_hdr, pointed to by program-header
   PT_GNU_EH_FRAME).  The exception-header is self-describing in the
   sense that the format of the addresses contained in it is expressed
   as a one-byte type-descriptor called a "pointer-encoding" (PE).

   The exception header encodes the address of the .eh_frame section
   and optionally contains a binary search table for the
   Frame Descriptor Entries (FDEs) in the .eh_frame.  The contents of
   .eh_frame has the format described by the DWARF v3 standard
   (http://www.eagercon.com/dwarf/dwarf3std.htm), except that code
   addresses may be encoded in different ways.  Also, .eh_frame has
   augmentations that allow encoding a language-specific data-area
   (LSDA) pointer and a pointer to a personality-routine.

   Details:

    The Common Information Entry (CIE) associated with an FDE may
    contain an augmentation string.  Each character in this string has
    a specific meaning and either one or two associated operands.  The
    operands are stored in an augmentation body which appears right
    after the "return_address_register" member and before the
    "initial_instructions" member.  The operands appear in the order
    in which the characters appear in the string.  For example, if the
    augmentation string is "zL", the operand for 'z' would be first in
    the augmentation body and the operand for 'L' would be second.
    The following characters are supported for the CIE augmentation
    string:

     'z': The operand for this character is a uleb128 value that gives the
	  length of the CIE augmentation body, not counting the length
	  of the uleb128 operand itself.  If present, this code must
	  appear as the first character in the augmentation body.

     'L': Indicates that the FDE's augmentation body contains an LSDA
          pointer.  The operand for this character is a single byte
          that specifies the pointer-encoding (PE) that is used for
          the LSDA pointer.

     'R': Indicates that the code-pointers (FDE members
          "initial_location" and "address_range" and the operand for
          DW_CFA_set_loc) in the FDE have a non-default encoding.  The
          operand for this character is a single byte that specifies
          the pointer-encoding (PE) that is used for the
          code-pointers.  Note: the "address_range" member is always
	  encoded as an absolute value.  Apart from that, the specified
	  FDE pointer-encoding applies.

     'P': Indicates the presence of a personality routine (handler).
          The first operand for this character specifies the
	  pointer-encoding (PE) that is used for the second operand,
	  which specifies the address of the personality routine.

    If the augmentation string contains any other characters, the
    remainder of the augmentation string should be ignored.
    Furthermore, if the size of the augmentation body is unknown
    (i.e., 'z' is not the first character of the augmentation string),
    then the entire CIE as well all associated FDEs must be ignored.

    A Frame Descriptor Entries (FDE) may contain an augmentation body
    which, if present, appears right after the "address_range" member
    and before the "instructions" member.  The contents of this body
    is implicitly defined by the augmentation string of the associated
    CIE.  The meaning of the characters in the CIE's augmentation
    string as far as FDEs are concerned is as follows:

     'z': The first operand in the FDE's augmentation body specifies
          the total length of the augmentation body as a uleb128 (not
          counting the length of the uleb128 operand itself).

     'L': The operand for this character is an LSDA pointer, encoded
          in the format specified by the corresponding operand in the
          CIE's augmentation body.

*/

#define DW_EH_VERSION		1	/* The version we're implementing */

struct dwarf_eh_frame_hdr
  {
    unsigned char version;
    unsigned char eh_frame_ptr_enc;
    unsigned char fde_count_enc;
    unsigned char table_enc;
    /* The rest of the header is variable-length and consists of the
       following members:

	encoded_t eh_frame_ptr;
	encoded_t fde_count;
	struct
	  {
	    encoded_t start_ip;	// first address covered by this FDE
	    encoded_t fde_addr;	// address of the FDE
	  }
	binary_search_table[fde_count];  */
  };

/*XXXXXXXXXXXXXXXXXXXXXXXXX End dwarf-eh.h XXXXXXXXXXXXXXXXXXXXXXXXXX*/

#endif /* libunwind_i_h */
