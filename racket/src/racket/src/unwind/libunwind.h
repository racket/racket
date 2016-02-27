/* libunwind - a platform-independent unwind library
   Copyright (C) 2002-2004 Hewlett-Packard Co
	Contributed by David Mosberger-Tang <davidm@hpl.hp.com>

This file is part of libunwind.

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

#ifndef LIBUNWIND_H
#define LIBUNWIND_H

#if defined(__linux__)
# define LINUX
#endif
#if defined(__i386__)
# define PLAIN_X86
#endif
#if defined(__x86_64__)
# define UNW_X86_64
#endif
#if defined(__arm__)
# define UNW_ARM
#endif

#if defined(PLAIN_X86)
# define UNW_IP UNW_X86_EIP
#elif defined(UNW_X86_64)
# define UNW_IP UNW_X86_64_RIP
#elif defined(UNW_ARM)
# define UNW_IP UNW_ARM_RIP
#endif

#ifdef UNW_ARM
# define CONFIG_DEBUG_FRAME
#endif

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#include <inttypes.h>
#ifndef UNW_ARM
# define _XOPEN_SOURCE /* needed for Mac OS X */
# define __USE_GNU
# include <ucontext.h>
# undef __USE_GNU
#endif

  /* XXXXXXXXXXXXXXXXXXXX x86 Target XXXXXXXXXXXXXXXXXXXX */

#ifdef PLAIN_X86

#define UNW_TARGET	x86
#define UNW_TARGET_X86	1

/* This needs to be big enough to accommodate "struct cursor", while
   leaving some slack for future expansion.  Changing this value will
   require recompiling all users of this library.  Stack allocation is
   relatively cheap and unwind-state copying is relatively rare, so we
   want to err on making it rather too big than too small.  */
#define UNW_TDEP_CURSOR_LEN	127

typedef unsigned long unw_word_t;
typedef long unw_sword_t;

typedef long double unw_tdep_fpreg_t;

typedef enum
  {
    /* Note: general registers are expected to start with index 0.
       This convention facilitates architecture-independent
       implementation of the C++ exception handling ABI.  See
       _Unwind_SetGR() and _Unwind_GetGR() for details.

       The described register usage convention is based on "System V
       Application Binary Interface, Intel386 Architecture Processor
       Supplement, Fourth Edition" at

         http://www.linuxbase.org/spec/refspecs/elf/abi386-4.pdf

       It would have been nice to use the same register numbering as
       DWARF, but that doesn't work because the libunwind requires
       that the exception argument registers be consecutive, which the
       wouldn't be with the DWARF numbering.  */
    UNW_X86_EAX,	/* scratch (exception argument 1) */
    UNW_X86_EDX,	/* scratch (exception argument 2) */
    UNW_X86_ECX,	/* scratch */
    UNW_X86_EBX,	/* preserved */
    UNW_X86_ESI,	/* preserved */
    UNW_X86_EDI,	/* preserved */
    UNW_X86_EBP,	/* (optional) frame-register */
    UNW_X86_ESP,	/* (optional) frame-register */
    UNW_X86_EIP,	/* frame-register */
    UNW_X86_EFLAGS,	/* scratch (except for "direction", which is fixed */
    UNW_X86_TRAPNO,	/* scratch */

    /* MMX/stacked-fp registers */
    UNW_X86_ST0,	/* fp return value */
    UNW_X86_ST1,	/* scratch */
    UNW_X86_ST2,	/* scratch */
    UNW_X86_ST3,	/* scratch */
    UNW_X86_ST4,	/* scratch */
    UNW_X86_ST5,	/* scratch */
    UNW_X86_ST6,	/* scratch */
    UNW_X86_ST7,	/* scratch */

    UNW_X86_FCW,	/* scratch */
    UNW_X86_FSW,	/* scratch */
    UNW_X86_FTW,	/* scratch */
    UNW_X86_FOP,	/* scratch */
    UNW_X86_FCS,	/* scratch */
    UNW_X86_FIP,	/* scratch */
    UNW_X86_FEA,	/* scratch */
    UNW_X86_FDS,	/* scratch */

    /* SSE registers */
    UNW_X86_XMM0_lo,	/* scratch */
    UNW_X86_XMM0_hi,	/* scratch */
    UNW_X86_XMM1_lo,	/* scratch */
    UNW_X86_XMM1_hi,	/* scratch */
    UNW_X86_XMM2_lo,	/* scratch */
    UNW_X86_XMM2_hi,	/* scratch */
    UNW_X86_XMM3_lo,	/* scratch */
    UNW_X86_XMM3_hi,	/* scratch */
    UNW_X86_XMM4_lo,	/* scratch */
    UNW_X86_XMM4_hi,	/* scratch */
    UNW_X86_XMM5_lo,	/* scratch */
    UNW_X86_XMM5_hi,	/* scratch */
    UNW_X86_XMM6_lo,	/* scratch */
    UNW_X86_XMM6_hi,	/* scratch */
    UNW_X86_XMM7_lo,	/* scratch */
    UNW_X86_XMM7_hi,	/* scratch */

    UNW_X86_MXCSR,	/* scratch */

    /* segment registers */
    UNW_X86_GS,		/* special */
    UNW_X86_FS,		/* special */
    UNW_X86_ES,		/* special */
    UNW_X86_DS,		/* special */
    UNW_X86_SS,		/* special */
    UNW_X86_CS,		/* special */
    UNW_X86_TSS,	/* special */
    UNW_X86_LDT,	/* special */

    /* frame info (read-only) */
    UNW_X86_CFA,

    UNW_TDEP_LAST_REG = UNW_X86_LDT,

    UNW_TDEP_IP = UNW_X86_EIP,
    UNW_TDEP_SP = UNW_X86_CFA,
    UNW_TDEP_EH = UNW_X86_EAX
  }
x86_regnum_t;

/* On x86, we can directly use ucontext_t as the unwind context.  */
typedef ucontext_t unw_tdep_context_t;
#define unw_tdep_getcontext(uc)		(getcontext (uc), 0)

#endif

  /* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */

  /* XXXXXXXXXXXXXXXXXXXX x86_64 Target XXXXXXXXXXXXXXXXXXXX */

#ifdef UNW_X86_64

#define UNW_TARGET		x86_64
#define UNW_TARGET_X86_64	1

#define _U_TDEP_QP_TRUE	0	/* see libunwind-dynamic.h  */

/* This needs to be big enough to accommodate "struct cursor", while
   leaving some slack for future expansion.  Changing this value will
   require recompiling all users of this library.  Stack allocation is
   relatively cheap and unwind-state copying is relatively rare, so we
   want to err on making it rather too big than too small.  */
#define UNW_TDEP_CURSOR_LEN	127

typedef uint64_t unw_word_t;
typedef int64_t unw_sword_t;

typedef long double unw_tdep_fpreg_t;

typedef enum
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
    UNW_X86_64_RIP,

    /* XXX Add other regs here */

    /* frame info (read-only) */
    UNW_X86_64_CFA,

    UNW_TDEP_LAST_REG = UNW_X86_64_RIP,

    UNW_TDEP_IP = UNW_X86_64_RIP,
    UNW_TDEP_SP = UNW_X86_64_RSP,
    UNW_TDEP_BP = UNW_X86_64_RBP,
    UNW_TDEP_EH = UNW_X86_64_RAX
  }
x86_64_regnum_t;

/* On x86, we can directly use ucontext_t as the unwind context.  */
typedef ucontext_t unw_tdep_context_t;
#define unw_tdep_getcontext(uc)		(getcontext (uc), 0)

#endif

  /* XXXXXXXXXXXXXXXXXXXX ARM Target XXXXXXXXXXXXXXXXXXXX */

#ifdef UNW_ARM

#define UNW_TARGET		ARM
#define UNW_TARGET_ARM          1

#define _U_TDEP_QP_TRUE	0	/* see libunwind-dynamic.h  */

/* This needs to be big enough to accommodate "struct cursor", while
   leaving some slack for future expansion.  Changing this value will
   require recompiling all users of this library.  Stack allocation is
   relatively cheap and unwind-state copying is relatively rare, so we
   want to err on making it rather too big than too small.  */
#define UNW_TDEP_CURSOR_LEN	127

typedef uint32_t unw_word_t;
typedef int32_t unw_sword_t;

typedef double unw_tdep_fpreg_t;

typedef enum
  {
    UNW_ARM_R0,
    UNW_ARM_R1,
    UNW_ARM_R2,
    UNW_ARM_R3,
    UNW_ARM_R4,
    UNW_ARM_R5,
    UNW_ARM_R6,
    UNW_ARM_R7,
    UNW_ARM_R8,
    UNW_ARM_R9,
    UNW_ARM_R10,
    UNW_ARM_R11,
    UNW_ARM_R12,
    UNW_ARM_R13,
    UNW_ARM_RSP = UNW_ARM_R13,
    UNW_ARM_R14,
    UNW_ARM_R15,
    UNW_ARM_RIP = UNW_ARM_R15,

    UNW_TDEP_LAST_REG = UNW_ARM_R15,

    UNW_TDEP_IP = UNW_ARM_RIP,
    UNW_TDEP_SP = UNW_ARM_RSP,
    UNW_TDEP_EH = UNW_ARM_R0

  }
arm_regnum_t;

typedef struct {
  unsigned long regs[16];
} unw_tdep_context_t;
  
/* There is no getcontext() on ARM.  Use a stub version which only saves GP
   registers.  FIXME: Not ideal, may not be sufficient for all libunwind
   use cases.  Stores pc+8, which is only approximately correct, really.  */
#ifndef __thumb__
#define unw_tdep_getcontext(uc) (({					\
  unw_tdep_context_t *unw_ctx = (uc);					\
  register unsigned long *unw_base asm ("r0") = (unsigned long *)unw_ctx; \
  __asm__ __volatile__ (						\
    "stmia %[base], {r0-r15}"						\
    : : [base] "r" (unw_base) : "memory");				\
  }), 0)
#else /* __thumb__ */
#define unw_tdep_getcontext(uc) (({					\
  unw_tdep_context_t *unw_ctx = (uc);					\
  register unsigned long *unw_base asm ("r0") = (unsigned long *)unw_ctx; \
  __asm__ __volatile__ (						\
    ".align 2\nbx pc\nnop\n.code 32\n"					\
    "stmia %[base], {r0-r15}\n"						\
    "orr %[base], pc, #1\nbx %[base]"					\
    : [base] "+r" (unw_base) : : "memory", "cc");			\
  }), 0)
#endif

#ifndef PT_GNU_EH_FRAME
# define PT_GNU_EH_FRAME -1
#endif

#endif

  /* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */

#define UNW_TDEP_NUM_EH_REGS	2	/* eax and edx are exception args */

typedef struct unw_tdep_save_loc
  {
    /* Additional target-dependent info on a save location.  */
  }
unw_tdep_save_loc_t;


typedef struct unw_dyn_remote_table_info
  {
    unw_word_t name_ptr;	/* addr. of table name (e.g., library name) */
    unw_word_t segbase;		/* segment base */
    unw_word_t table_len;	/* must be a multiple of sizeof(unw_word_t)! */
    unw_word_t table_data;
  }
unw_dyn_remote_table_info_t;

typedef struct unw_dyn_info
  {
    /* doubly-linked list of dyn-info structures: */
    struct unw_dyn_info *next;
    struct unw_dyn_info *prev;
    unw_word_t start_ip;	/* first IP covered by this entry */
    unw_word_t end_ip;		/* first IP NOT covered by this entry */
    unw_word_t gp;		/* global-pointer in effect for this entry */
    int32_t format;		/* real type: unw_dyn_info_format_t */
    int32_t pad;
    union
      {
	unw_dyn_remote_table_info_t rti;
      }
    u;
  }
unw_dyn_info_t;

#define UNW_INFO_FORMAT_TABLE 1
#define UNW_INFO_FORMAT_REMOTE_TABLE 2

typedef struct
  {
    /* no x86-specific auxiliary proc-info */
  }
unw_tdep_proc_info_t;

#define UNW_VERSION_MAJOR	0
#define UNW_VERSION_MINOR	99
#define UNW_VERSION_EXTRA	0

#define UNW_VERSION_CODE(maj,min)	(((maj) << 16) | (min))
#define UNW_VERSION	UNW_VERSION_CODE(UNW_VERSION_MAJOR, UNW_VERSION_MINOR)

#define UNW_PASTE2(x,y)	x##y
#define UNW_PASTE(x,y)	UNW_PASTE2(x,y)
#define UNW_OBJ(fn)	UNW_PASTE(UNW_PREFIX, fn)
#define UNW_ARCH_OBJ(fn) UNW_PASTE(UNW_PASTE(UNW_PASTE(_U,UNW_TARGET),_), fn)

#define UW_NO_SYNC

#include <sys/types.h>

# define UNW_PREFIX	UNW_PASTE(UNW_PASTE(_UL,UNW_TARGET),_)

/* Error codes.  The unwind routines return the *negated* values of
   these error codes on error and a non-negative value on success.  */
typedef enum
  {
    UNW_ESUCCESS = 0,		/* no error */
    UNW_EUNSPEC,		/* unspecified (general) error */
    UNW_ENOMEM,			/* out of memory */
    UNW_EBADREG,		/* bad register number */
    UNW_EREADONLYREG,		/* attempt to write read-only register */
    UNW_ESTOPUNWIND,		/* stop unwinding */
    UNW_EINVALIDIP,		/* invalid IP */
    UNW_EBADFRAME,		/* bad frame */
    UNW_EINVAL,			/* unsupported operation or bad value */
    UNW_EBADVERSION,		/* unwind info has unsupported version */
    UNW_ENOINFO			/* no unwind info found */
  }
unw_error_t;

/* The following enum defines the indices for a couple of
   (pseudo-)registers which have the same meaning across all
   platforms.  (RO) means read-only.  (RW) means read-write.  General
   registers (aka "integer registers") are expected to start with
   index 0.  The number of such registers is architecture-dependent.
   The remaining indices can be used as an architecture sees fit.  The
   last valid register index is given by UNW_REG_LAST.  */
typedef enum
  {
    UNW_REG_IP = UNW_TDEP_IP,		/* (rw) instruction pointer (pc) */
    UNW_REG_SP = UNW_TDEP_SP,		/* (ro) stack pointer */
    UNW_REG_EH = UNW_TDEP_EH,		/* (rw) exception-handling reg base */
    UNW_REG_LAST = UNW_TDEP_LAST_REG
  }
unw_frame_regnum_t;

/* Number of exception-handler argument registers: */
#define UNW_NUM_EH_REGS		UNW_TDEP_NUM_EH_REGS

typedef enum
  {
    UNW_CACHE_NONE,			/* no caching */
    UNW_CACHE_GLOBAL,			/* shared global cache */
    UNW_CACHE_PER_THREAD		/* per-thread caching */
  }
unw_caching_policy_t;

typedef int unw_regnum_t;

/* The unwind cursor starts at the youngest (most deeply nested) frame
   and is used to track the frame state as the unwinder steps from
   frame to frame.  It is safe to make (shallow) copies of variables
   of this type.  */
typedef struct unw_cursor
  {
    unw_word_t opaque[UNW_TDEP_CURSOR_LEN];
  }
unw_cursor_t;

/* This type encapsulates the entire (preserved) machine-state.  */
typedef unw_tdep_context_t unw_context_t;

/* unw_getcontext() fills the unw_context_t pointed to by UC with the
   machine state as it exists at the call-site.  For implementation
   reasons, this needs to be a target-dependent macro.  It's easiest
   to think of unw_getcontext() as being identical to getcontext(). */
#define unw_getcontext(uc)		unw_tdep_getcontext(uc)

/* Return 1 if register number R is a floating-point register, zero
   otherwise.
   This routine is signal-safe.  */
#define unw_is_fpreg(r)			unw_tdep_is_fpreg(r)

typedef unw_tdep_fpreg_t unw_fpreg_t;

typedef struct unw_addr_space *unw_addr_space_t;

/* Each target may define its own set of flags, but bits 0-15 are
   reserved for general libunwind-use.  */
#define UNW_PI_FLAG_FIRST_TDEP_BIT	16

typedef struct unw_proc_info
  {
    unw_word_t start_ip;	/* first IP covered by this procedure */
    unw_word_t end_ip;		/* first IP NOT covered by this procedure */
    unw_word_t lsda;		/* address of lang.-spec. data area (if any) */
    unw_word_t handler;		/* optional personality routine */
    unw_word_t gp;		/* global-pointer value for this procedure */
    unw_word_t flags;		/* misc. flags */

    int format;			/* unwind-info format (arch-specific) */
    int unwind_info_size;	/* size of the information (if applicable) */
    void *unwind_info;		/* unwind-info (arch-specific) */
    unw_tdep_proc_info_t extra;	/* target-dependent auxiliary proc-info */
  }
unw_proc_info_t;

/* These are backend callback routines that provide access to the
   state of a "remote" process.  This can be used, for example, to
   unwind another process through the ptrace() interface.  */
typedef struct unw_accessors
  {
    /* REMOVED */
  }
unw_accessors_t;

typedef enum unw_save_loc_type
  {
    UNW_SLT_NONE,	/* register is not saved ("not an l-value") */
    UNW_SLT_MEMORY,	/* register has been saved in memory */
    UNW_SLT_REG		/* register has been saved in (another) register */
  }
unw_save_loc_type_t;

typedef struct unw_save_loc
  {
    unw_save_loc_type_t type;
    union
      {
	unw_word_t addr;	/* valid if type==UNW_SLT_MEMORY */
	unw_regnum_t regnum;	/* valid if type==UNW_SLT_REG */
      }
    u;
    unw_tdep_save_loc_t extra;	/* target-dependent additional information */
  }
unw_save_loc_t;

/* These routines work both for local and remote unwinding.  */

#define unw_local_addr_space	UNW_OBJ(local_addr_space)
#define unw_create_addr_space	UNW_OBJ(create_addr_space)
#define unw_destroy_addr_space	UNW_OBJ(destroy_addr_space)
#define unw_get_accessors	UNW_ARCH_OBJ(get_accessors)
#define unw_init_local		UNW_OBJ(init_local)
#define unw_destroy_local	UNW_OBJ(destroy_local)
#define unw_init_remote		UNW_OBJ(init_remote)
#define unw_step		UNW_OBJ(step)
#define unw_resume		UNW_OBJ(resume)
#define unw_get_proc_info	UNW_OBJ(get_proc_info)
#define unw_get_proc_info_by_ip	UNW_OBJ(get_proc_info_by_ip)
#define unw_get_reg		UNW_OBJ(get_reg)
#define unw_set_reg		UNW_OBJ(set_reg)
#define unw_get_fpreg		UNW_OBJ(get_fpreg)
#define unw_set_fpreg		UNW_OBJ(set_fpreg)
#define unw_get_save_loc	UNW_OBJ(get_save_loc)
#define unw_is_signal_frame	UNW_OBJ(is_signal_frame)
#define unw_get_proc_name	UNW_OBJ(get_proc_name)
#define unw_set_caching_policy	UNW_OBJ(set_caching_policy)
#define unw_regname		UNW_ARCH_OBJ(regname)
#define unw_flush_cache		UNW_ARCH_OBJ(flush_cache)
#define unw_strerror		UNW_ARCH_OBJ(strerror)

extern unw_addr_space_t unw_create_addr_space (unw_accessors_t *, int);
extern void unw_destroy_addr_space (unw_addr_space_t);
extern unw_accessors_t *unw_get_accessors (unw_addr_space_t);
extern void unw_flush_cache (unw_addr_space_t, unw_word_t, unw_word_t);
extern int unw_set_caching_policy (unw_addr_space_t, unw_caching_policy_t);
extern const char *unw_regname (unw_regnum_t);

extern int unw_init_local (unw_cursor_t *, unw_context_t *);
extern void unw_destroy_local(unw_cursor_t *);
extern int unw_init_remote (unw_cursor_t *, unw_addr_space_t, void *);
extern int unw_step (unw_cursor_t *);
extern int unw_resume (unw_cursor_t *);
extern int unw_get_proc_info (unw_cursor_t *, unw_proc_info_t *);
extern int unw_get_proc_info_by_ip (unw_addr_space_t, unw_word_t,
				    unw_proc_info_t *, void *);
extern int unw_get_reg (unw_cursor_t *, int, unw_word_t *);
extern int unw_set_reg (unw_cursor_t *, int, unw_word_t);
extern int unw_get_fpreg (unw_cursor_t *, int, unw_fpreg_t *);
extern int unw_set_fpreg (unw_cursor_t *, int, unw_fpreg_t);
extern int unw_get_save_loc (unw_cursor_t *, int, unw_save_loc_t *);
extern int unw_is_signal_frame (unw_cursor_t *);
extern int unw_get_proc_name (unw_cursor_t *, char *, size_t, unw_word_t *);
extern unw_word_t unw_get_ip(unw_cursor_t *);
extern unw_word_t unw_get_frame_pointer(unw_cursor_t *);
extern const char *unw_strerror (int);

#ifdef UNW_X86_64
void unw_manual_step(unw_cursor_t *_c, 
		     void *ip_addr,
		     void *bp_addr,
		     void *sp_addr,
		     void *bx_addr,
		     void *r12_addr,
		     void *r13_addr);
#endif
#ifdef UNW_ARM
void unw_manual_step(unw_cursor_t *_c, 
		     void *ip_addr,
		     void *sp_addr,
                     void *r0_addr, void *r1_addr, void *r2_addr, void *r3_addr, 
                     void *r4_addr, void *r5_addr, void *r6_addr, void *r7_addr,
                     void *fp_addr);
#endif

extern unw_addr_space_t unw_local_addr_space;

extern int unw_reset_bad_ptr_flag(unw_cursor_t *c);
extern void unw_set_safe_pointer_range(unw_cursor_t *c, unw_word_t s, unw_word_t e);

#define unw_tdep_is_fpreg		UNW_ARCH_OBJ(is_fpreg)
extern int unw_tdep_is_fpreg (int);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* LIBUNWIND_H */
