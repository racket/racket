
#include "stdafx.h"

#include <stdio.h>
#include <malloc.h>
#include <float.h>
#include <limits.h>
#include <io.h>
#include <process.h>

#define _WIN32_DCOM

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <shellapi.h>
#include <htmlhelp.h>

#include "escheme.h"

#ifndef MZ_PRECISE_GC
# define XFORM_OK_PLUS +
# define XFORM_OK_MINUS -
# define GC_CAN_IGNORE /* empty */
# define HIDE_FROM_XFORM(x) x
#endif
