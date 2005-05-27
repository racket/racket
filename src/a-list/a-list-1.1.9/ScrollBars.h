/*
	File:		ScrollBars.h

	Contains:	Header for scroll bars live scrolling implementation.

	Written by:	Chris White, Developer Technical Support
	
	Copyright:	© 1996 by Apple Computer, Inc., all rights reserved.
	
	Change History (most recent first):
	
			03/29/96			CW		First release

*/

#ifndef __SCROLLBARS__
#define __SCROLLBARS__

#if !defined(UNIVERSAL_INTERFACES_VERSION) || (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	#define TARGET_RT_MAC_CFM			GENERATINGCFM
#endif

enum
{
	// Misc scroll bar constants

//	kScrollBarWidth = 16,
//	kScrollBarWidthAdjust = kScrollBarWidth - 1,
	kScrollBarWidthAdjust = 15,
	
	kScrollArrowWidth = 16,
	kScrollThumbWidth = 16,
	kTotalWidthAdjust = (kScrollArrowWidth * 2) + kScrollThumbWidth,
	
	kPageOverlap = 10,
	kThumbTrackWidthSlop = 25,
	kThumbTrackLengthSlop = 113

};


typedef	pascal void (*IndicatorActionProc)(void);

#if TARGET_RT_MAC_CFM
typedef UniversalProcPtr IndicatorActionUPP;
#else
typedef IndicatorActionProc IndicatorActionUPP;
#endif

enum
{
	uppIndicatorActionProcInfo = kPascalStackBased
};

#if TARGET_RT_MAC_CFM
#define NewIndicatorActionProc(userRoutine)		\
		(IndicatorActionUPP) NewRoutineDescriptor((ProcPtr)(userRoutine), uppIndicatorActionProcInfo, GetCurrentArchitecture())
#else
#define NewIndicatorActionProc(userRoutine)		\
		((IndicatorActionUPP) (userRoutine))
#endif

#if TARGET_RT_MAC_CFM
#define CallIndicatorActionProc(userRoutine)	\
		CallUniversalProc((UniversalProcPtr)(userRoutine), uppIndicatorActionProcInfo)
#else
#define CallIndicatorActionProc(userRoutine)	\
		(*(userRoutine))()
#endif


#endif	// __SCROLLBARS__
