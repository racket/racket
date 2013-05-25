/******************************** -*- C -*- ****************************
 *
 *	lightning main include file
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/



#ifndef __lightning_h
#define __lightning_h

#ifdef __cplusplus
extern "C" {
#endif

#ifdef MZ_USE_JIT_PPC

#include "ppc/asm-common.h"
#include "ppc/asm.h"
#include "ppc/core.h"
#include "ppc/core-common.h"
#include "ppc/funcs.h"
#include "ppc/funcs-common.h"
#include "ppc/fp.h"
#include "ppc/fp-common.h"

#endif

#ifdef MZ_USE_JIT_I386

#include "i386/asm-common.h"
#include "i386/asm.h"
#include "i386/core.h"
#include "i386/core-common.h"
#include "i386/funcs.h"
#include "i386/funcs-common.h"
#include "i386/fp.h"
#include "i386/fp-common.h"

#endif

#ifdef MZ_USE_JIT_ARM

#include "arm/asm-common.h"
#include "arm/asm.h"
#include "arm/funcs.h"
#include "arm/funcs-common.h"
#include "arm/core.h"
#include "arm/core-common.h"
#include "arm/fp.h"
#include "arm/fp-common.h"

#endif

#ifndef JIT_R0
#error GNU lightning does not support the current target
#endif

#ifdef __cplusplus
}
#endif

#endif /* __lightning_h */
