/*
 *	QDDrawingState.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#include "QDDrawingState.h"

// Could put a DebugStr( ) in here.
#define Assert( x )		if ( !x )	return;

static short local_AppearanceMgrVersion( void );

#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	// These are real MacOS API calls under Carbon.
	#define	GetPortTextFont( port )			local_GetPortTextFont( port )
	#define	GetPortTextFace( port )			local_GetPortTextFace( port )
	#define	GetPortTextMode( port )			local_GetPortTextMode( port )
	#define	GetPortTextSize( port )			local_GetPortTextSize( port )
	#define	GetPortBackPixPat( port, pat )		local_GetPortBackPixPat( port, pat )
	#define	GetPortPenPixPat( port, pat )		local_GetPortPenPixPat( port, pat )

	static short local_GetPortTextFont( const CGrafPtr port );
	static Style local_GetPortTextFace( const CGrafPtr port );
	static short local_GetPortTextMode( const CGrafPtr port );
	static short local_GetPortTextSize( const CGrafPtr port );
	static PixPatHandle local_GetPortBackPixPat( const CGrafPtr port, PixPatHandle ioPattern );
	static PixPatHandle local_GetPortPenPixPat( const CGrafPtr port, PixPatHandle ioPattern );
#endif

void SaveQDDrawingState( QDDrawingState *ioDrawingState, const Boolean inSaveTextState )
{	GWorldPtr		tempWorld;
	GDHandle		tempDevice;

	Assert( ioDrawingState != nil );

	// Get the current GWorld, so I can access it's fields.
	GetGWorld( &tempWorld, &tempDevice );

	// Get the background state.
	GetBackColor( &ioDrawingState->backColor );
/*#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	if ( tempWorld->bkPixPat == nil )
		ioDrawingState->backPixPatH = nil;
	else
#endif
		ioDrawingState->backPixPatH = NewPixPat( );

	if ( ioDrawingState->backPixPatH != nil )
		GetPortBackPixPat( tempWorld, ioDrawingState->backPixPatH );
*/
	// Get the pen/foreground state.
	GetForeColor( &ioDrawingState->foreColor );
/*#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	if ( tempWorld->pnPixPat == nil )
		ioDrawingState->penPixPatH = nil;
	else
#endif
		ioDrawingState->penPixPatH = NewPixPat( );

	if ( ioDrawingState->penPixPatH != nil )
		GetPortPenPixPat( tempWorld, ioDrawingState->penPixPatH );
*/
	GetPenState( &ioDrawingState->penState );

	// Optionally save the text font, face, size and mode.
	ioDrawingState->haveTextState = inSaveTextState;
	if ( inSaveTextState ) {
		ioDrawingState->textStyle.tsFont = GetPortTextFont( tempWorld );
		ioDrawingState->textStyle.tsFace = GetPortTextFace( tempWorld );
		ioDrawingState->textStyle.tsSize = GetPortTextSize( tempWorld );
		ioDrawingState->textMode = GetPortTextMode( tempWorld );
	}

#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	// If we're running under CFM and have Appearance Mgr 1.1 or later, use the ThemeDrawingState routines.
	if ( local_AppearanceMgrVersion( ) > 0x0110 )
		ioDrawingState->haveThemeState = ( GetThemeDrawingState( &ioDrawingState->themeState ) == noErr );
	else {
		ioDrawingState->haveThemeState = false;
		ioDrawingState->themeState = nil;
	}
#endif
}

void RestoreQDDrawingState( QDDrawingState *inDrawingState, const Boolean inDisposeNow )
{//	PixPatHandle	tempPixPat;

#if !(ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM )
	#pragma unused( inDisposeNow )	// Eliminate compiler warnings in 68K targets because PixPatHandle stuff is screwed up!
#endif

	Assert( inDrawingState != nil );

	// Restore the pen/foreground state.
/*	if ( !inDisposeNow && inDrawingState->penPixPatH != nil ) {
		// Make a copy of the penPixPatH so we can put it somewhere else later because we're not supposed to dispose of it now.
		tempPixPat = NewPixPat( );
		if ( tempPixPat != nil )
			CopyPixPat( inDrawingState->penPixPatH, tempPixPat );
	} else
		tempPixPat = nil;
	PenPixPat( inDrawingState->penPixPatH );
	inDrawingState->penPixPatH = tempPixPat;
*/	RGBForeColor( &inDrawingState->foreColor );
	SetPenState( &inDrawingState->penState );

	// Restore the background state.
/*	if ( !inDisposeNow && inDrawingState->backPixPatH != nil ) {
		// Make a copy of the backPixPatH so we can put it somewhere else later because we're not supposed to dispose of it now.
		tempPixPat = NewPixPat( );
		if ( tempPixPat != nil )
			CopyPixPat( inDrawingState->backPixPatH, tempPixPat );
	} else
		tempPixPat = nil;
	BackPixPat( inDrawingState->backPixPatH );
	inDrawingState->penPixPatH = tempPixPat;
*/	RGBBackColor( &inDrawingState->backColor );

	// Optionally restore the text state.
	if ( inDrawingState->haveTextState ) {
		TextFont( inDrawingState->textStyle.tsFont );
		TextFace( inDrawingState->textStyle.tsFace );
		TextSize( inDrawingState->textStyle.tsSize );
		TextMode( inDrawingState->textMode );
	}

#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	// If we're running under CFM and have Appearance Mgr 1.1 or later, use the ThemeDrawingState routines.
	if ( inDrawingState->haveThemeState ) {
		SetThemeDrawingState( inDrawingState->themeState, inDisposeNow );
		if ( inDisposeNow )
			inDrawingState->themeState = nil;
	}
#endif
}

void DisposeQDDrawingState( QDDrawingState *inDrawingState )
{	Assert( inDrawingState != nil );

/*	if ( inDrawingState->penPixPatH != nil ) {
		DisposePixPat( inDrawingState->penPixPatH );
		inDrawingState->penPixPatH = nil;
	}

	if ( inDrawingState->backPixPatH != nil ) {
		DisposePixPat( inDrawingState->backPixPatH );
		inDrawingState->backPixPatH = nil;
	}
*/
#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	if ( inDrawingState->themeState != nil ) {
		DisposeThemeDrawingState( inDrawingState->themeState );
		inDrawingState->themeState = nil;
		inDrawingState->haveThemeState = false;
	}
#endif
}

#pragma mark -

static short local_AppearanceMgrVersion( void )
{	static short	apMgrVersion = -1;

	if ( apMgrVersion == -1 ) {
		// The first time through, we'll check Gestalt.
		OSErr		err;
		long			result;

		err = Gestalt( gestaltAppearanceAttr, &result );
		if ( ( err == noErr ) && ( result & (1 << gestaltAppearanceExists) ) ) {
			err = Gestalt( gestaltAppearanceVersion, &result );
			if ( err == noErr )
				apMgrVersion = LoWord( result );
			else
				apMgrVersion = 0x0100;	// Version 1.0 didn't advertise it's version number.
		} else
			apMgrVersion = 0x0000;
	}

	return apMgrVersion;
}

#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	// These are MacOS API calls under Carbon.

	static short local_GetPortTextFont( const CGrafPtr port )
	{	return port->txFont;	}

	static Style local_GetPortTextFace( const CGrafPtr port )
	{	return port->txFace;	}

	static short local_GetPortTextMode( const CGrafPtr port )
	{	return port->txMode;	}
	
	static short local_GetPortTextSize( const CGrafPtr port )
	{	return port->txSize;	}

	static PixPatHandle local_GetPortBackPixPat( const CGrafPtr port, PixPatHandle ioPattern )
	{	CopyPixPat( port->bkPixPat, ioPattern );
		return ioPattern;
	}

	static PixPatHandle local_GetPortPenPixPat( const CGrafPtr port, PixPatHandle ioPattern )
	{	CopyPixPat( port->pnPixPat, ioPattern );
		return ioPattern;
	}

#endif
