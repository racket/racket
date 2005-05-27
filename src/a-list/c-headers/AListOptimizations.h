/*
 *	AListOptimizations.h
 *
 *	The A List
 *  Optimization definitions for compiling The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
 *
 */
#if !defined( __ALISTOPTIMIZATIONS_H_ )
#define __ALISTOPTIMIZATIONS_H_

/* Set ALIST_USE_PASCAL_CALLING to 1 if you want to compile the library with Pascal calling conventions.

	Pascal calling conventions will be a slight performance hit in C/C++ applications as well as a slight code size
	increase on 68K, but it allows Pascal programmers to use The A List.

	Be sure your callbacks use ALIST_API for compliance with this flag.
*/
#if !defined( ALIST_USE_PASCAL_CALLING )
#define ALIST_USE_PASCAL_CALLING	0
#endif

/* Set ALIST_HAVE_CELLDATA to 0 if you do not want to put ANY data into the cells.

	You'll need to use custom callback routines to do everything based on the cell indices instead of cell data.
	This is useful to reduce the amount of RAM used by The A List if you're simply indexing into some other data structure.
*/
#if !defined( ALIST_HAVE_CELLDATA )
#define ALIST_HAVE_CELLDATA		1
#endif

/* Set ALIST_USEAPPEARANCEMGR to 0 if you want to strip out the Appearance Manager code.

	If this is non-zero, you need to link (or weak link) to AppearanceLib for a non-Carbon CFM target (CFM-68K or PPC).
*/
#if !defined( ALIST_USEAPPEARANCEMGR )
#define ALIST_USEAPPEARANCEMGR	1
#endif

/* Set ALIST_USECONTROLMGR2 to 0 if you want to strip out the Control Manager 2.0 (proportional scroll thumbs) code.

	If this is non-zero, you need to link (or weak link) to ControlsLib for a non-Carbon CFM target (CFM-68K or PPC).
*/
#if !defined( ALIST_USECONTROLMGR2 )
#define ALIST_USECONTROLMGR2		1
#endif

/* Set ALIST_HEIRARCHICAL to 0 if you do not want to allow heirarchical/disclosure/outline view lists.
*/
#if !defined( ALIST_HEIRARCHICAL )
#define ALIST_HEIRARCHICAL			1
#endif

/* If using wx-windows, set ALIST_FOR_WX_WINDOWS to 1. This prevents ALClick
   from using FindControl (because wx-windows abuses SetOrigin quite badly). */
#if !defined( ALIST_FOR_WX_WINDOWS )
#define ALIST_FOR_WX_WINDOWS			1
#endif


/* If we're using an old version of the Universal Interfaces, set the new macro
		TARGET_RT_MAC_CFM to be equal to the old macro GENERATINGCFM */
#ifdef __MWERKS__
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
#define TARGET_RT_MAC_CFM		GENERATINGCFM
#endif
#endif

/* If we're using an old version of the Universal Interfaces, set the new macro
		PRAGMA_STRUCT_ALIGN to be equal to the old macro PRAGMA_ALIGN_SUPPORTED */
#ifdef __MWERKS__
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0320)
#define PRAGMA_STRUCT_ALIGN	PRAGMA_ALIGN_SUPPORTED
#endif
#endif

/* Set ALIST_USE_UPPS to 0 if you don't need UPPs.
	Note: with Carbon, you don't need UPPs since it's all PPC code.  However, as a shared library under Carbon, you may need
	UPPs.  I haven't tested that configuration.
*/

#if !defined( ALIST_USE_UPPS )
	#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
		#define ALIST_USE_UPPS			TARGET_RT_MAC_CFM	/* Change this if you want to */
	#else
		#define ALIST_USE_UPPS			0	/* Leave this zero always for Carbon. */
	#endif
#endif

/* Set ALIST_SHARED_LIBRARY to 1 if you want to compile the A List as a shared library.
	Note: I still haven't tried this yet, but it seems like it should work.
*/
#if !defined( ALIST_SHARED_LIBRARY )
#define ALIST_SHARED_LIBRARY		0
#endif

#endif