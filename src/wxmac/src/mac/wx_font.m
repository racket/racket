
/* The easiest way to find out whether a font is fixed-width is to
   jump over the to Cocao world. The ATS and Cocoa worlds are
   connected through the PostScript name of a font. */

#import <Cocoa/Cocoa.h>

int wx_isFamilyFixedWidth(FMFontFamily fam)
{
  FMFont fnt;
  StyleParameter intrinsic;
  int is_fw = 0;
  id pool = [[NSAutoreleasePool alloc] init];

  if (!FMGetFontFromFontFamilyInstance(fam, 0, &fnt, &intrinsic)) {
    ATSFontRef ats;
    ats = FMGetATSFontRefFromFont(fnt);
    if (ats) {
      CFStringRef ref;
      NSFont *nsfnt;
      if (!ATSFontGetPostScriptName(ats, kATSOptionFlagsDefault, &ref)) {
        nsfnt = [NSFont fontWithName: (NSString *)ref size: 12];
        CFRelease(ref);
        is_fw = [nsfnt isFixedPitch];
      }
    }
  }

  [pool release];

  return is_fw;
}
