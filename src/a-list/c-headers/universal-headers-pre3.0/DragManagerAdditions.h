#pragma once

#ifndef __DRAG__
	#include <Drag.h>
#endif

enum
{	_DragDispatch = 0xABED
};

enum
{	gestaltDragMgrHasImageSupport = 3
};

enum
{	unsupportedForPlatformErr	= -1858,
	// call is for PowerPC only
	noSuitableDisplaysErr		= -1859,
	// no displays support translucency
	badImageRgnErr			= -1860,
	// bad translucent image region
	badImageErr				= -1861
	// bad translucent image PixMap
};

typedef unsigned long DragImageFlags;

enum
{	kDragStandardTranslucency	= 0x00000000,
	kDragDarkTranslucency		= 0x00000001,
	kDragDarkerTranslucency		= 0x00000002,
	kDragOpaqueTranslucency		= 0x00000003,
	kDragRegionAndImage		= 0x00000010
};

#ifdef __cplusplus
extern "C" {
#endif

pascal OSErr SetDragImage(DragReference	theDragRef,
					PixMapHandle		imagePixMap,
					RgnHandle			imageRgn,
					Point				imageOffsetPt,
					DragImageFlags		theImageFlags)
TWOWORDINLINE (0x7027, 0xABED);

pascal OSErr GetDragHiliteColor(WindowPtr	window,
						RGBColor *	color)
TWOWORDINLINE (0x7026, 0xABED);

#ifdef __cplusplus
}
#endif
