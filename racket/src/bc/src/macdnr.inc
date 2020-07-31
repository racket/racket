/*
    DNR.c - DNR library for MPW

    (c) Copyright 1988 by Apple Computer.  All rights reserved.
    PowerPC corrections by Eric Scouten, December 2, 1994.

*/

#define MPW3.0

#include <Carbon.h>
#if 0
#include <OSUtils.h>
#include <Errors.h>
#include <Files.h>
#include <Resources.h>
#include <Memory.h>
#include <Traps.h>
#include <Gestalt.h>
#include <Folders.h>
#include <MixedMode.h>
#endif

#define OPENRESOLVER    1L
#define CLOSERESOLVER   2L
#define STRTOADDR       3L
#define ADDRTOSTR       4L
#define ENUMCACHE       5L
#define ADDRTONAME      6L
#define HINFO           7L
#define MXINFO          8L

Handle              codeHndl = nil;
UniversalProcPtr    dnr = nil;

enum {
	NUM_ALT_ADDRS				= 4
};

struct hostInfo {
	long							rtnCode;
	char							cname[255];
	SInt8							filler;						/* Filler for proper byte alignment	 */
	unsigned long					addr[NUM_ALT_ADDRS];
};
typedef struct hostInfo hostInfo;

static pascal OSErr (*mzSysEnvirons)(int versionRequested, SysEnvRec *theworld);
static pascal OSErr (*mzGetWDInfo)(short wdRefNum, short *vRefNum, long *dirID, long *procID);
static pascal UniversalProcPtr (*mzNewRoutineDescriptor) (ProcPtr theProc, ProcInfoType theProcInfo, ISAType theISA);
static long (*mzCallUniversalProc)(UniversalProcPtr theProcPtr, ProcInfoType theProcInfo, ...);

#define NewRoutineDescriptor mzNewRoutineDescriptor 
#define CallUniversalProc mzCallUniversalProc

void GetSystemFolder(short *vRefNumP, long *dirIDP)
{
    SysEnvRec info;
    long wdProcID;

    mzSysEnvirons(1, &info);
    if (mzGetWDInfo(info.sysVRefNum, vRefNumP, dirIDP, &wdProcID) != noErr) {
        *vRefNumP = 0;
        *dirIDP = 0;
        }
    }

void GetCPanelFolder(short *vRefNumP, long *dirIDP)
{
    Boolean hasFolderMgr = false;
    long feature;

    if (Gestalt(gestaltFindFolderAttr, &feature) == noErr)
        hasFolderMgr = true;
    if (!hasFolderMgr) {
        GetSystemFolder(vRefNumP, dirIDP);
        return;
        }
    else {
        if (FindFolder(kOnSystemDisk, kControlPanelFolderType, kDontCreateFolder, vRefNumP, dirIDP) != noErr) {
            *vRefNumP = 0;
            *dirIDP = 0;
            }
        }
    }

/* SearchFolderForDNRP is called to search a folder for files that might
    contain the 'dnrp' resource */
short SearchFolderForDNRP(long targetType, long targetCreator, short vRefNum, long dirID)
{
    HParamBlockRec fi;
    Str255 filename;
    short refnum;

    fi.fileParam.ioCompletion = nil;
    fi.fileParam.ioNamePtr = filename;
    fi.fileParam.ioVRefNum = vRefNum;
    fi.fileParam.ioDirID = dirID;
    fi.fileParam.ioFDirIndex = 1;

    while (PBHGetFInfoSync(&fi) == noErr) {
        /* scan system folder for driver resource files of specific type & creator */
        if (fi.fileParam.ioFlFndrInfo.fdType == targetType &&
            fi.fileParam.ioFlFndrInfo.fdCreator == targetCreator) {
            /* found the MacTCP driver file? */
            refnum = HOpenResFile(vRefNum, dirID, filename, fsRdPerm);
            if (GetIndResource('dnrp', 1) == NULL)
                CloseResFile(refnum);
            else
                return refnum;
            }
        /* check next file in system folder */
        fi.fileParam.ioFDirIndex++;
        fi.fileParam.ioDirID = dirID;   /* PBHGetFInfo() clobbers ioDirID */
        }
    return(-1);
    }



/* OpenOurRF is called to open the MacTCP driver resources */

short OpenOurRF()
{
    short refnum;
    short vRefNum;
    long dirID;

    /* first search Control Panels for MacTCP 1.1 */
    GetCPanelFolder(&vRefNum, &dirID);
    refnum = SearchFolderForDNRP('cdev', 'ztcp', vRefNum, dirID);
    if (refnum != -1) return(refnum);

    /* next search System Folder for MacTCP 1.0.x */
    GetSystemFolder(&vRefNum, &dirID);
    refnum = SearchFolderForDNRP('cdev', 'mtcp', vRefNum, dirID);
    if (refnum != -1) return(refnum);

    /* finally, search Control Panels for MacTCP 1.0.x */
    GetCPanelFolder(&vRefNum, &dirID);
    refnum = SearchFolderForDNRP('cdev', 'mtcp', vRefNum, dirID);
    if (refnum != -1) return(refnum);

    return -1;
    }




typedef OSErr (*OpenResolverProcPtr)(long selector, char* fileName);

enum {
    uppOpenResolverProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
         | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(char *)))
};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr OpenResolverUPP;

#define NewOpenResolverProc(userRoutine)                        \
        (OpenResolverUPP) mzNewRoutineDescriptor(userRoutine, uppOpenResolverProcInfo, GetCurrentISA())
#define CallOpenResolverProc(userRoutine, selector, filename)   \
        CallUniversalProc(userRoutine, uppOpenResolverProcInfo, selector, filename)
#else
typedef OpenResolverProcPtr OpenResolverUPP;

#define NewOpenResolverProc(userRoutine)                    \
        (OpenResolverUPP)(userRoutine)
#define CallOpenResolverProc(userRoutine, selector, filename)   \
        (*userRoutine)(selector, filename)
#endif



pascal OSErr OpenResolver(fileName)
char *fileName;
{
    short           refnum;
    OSErr           rc;

    if (dnr != nil)
        /* resolver already loaded in */
        return(noErr);

    /* open the MacTCP driver to get DNR resources. Search for it based on
       creator & type rather than simply file name */
    refnum = OpenOurRF();

    /* ignore failures since the resource may have been installed in the
       System file if running on a Mac 512Ke */

    /* load in the DNR resource package */
    codeHndl = GetIndResource('dnrp', 1);
    if (codeHndl == nil) {
        /* can't open DNR */
        return(ResError());
    }

    DetachResource(codeHndl);
    if (refnum != -1) {
        CloseResFile(refnum);
    }

    /* lock the DNR resource since it cannot be reloated while opened */
    HLock(codeHndl);
    dnr = (UniversalProcPtr) *codeHndl;

    /* call open resolver */
    rc = CallOpenResolverProc((OpenResolverUPP) dnr, OPENRESOLVER, fileName);
    if (rc != noErr) {
        /* problem with open resolver, flush it */
        HUnlock(codeHndl);
        DisposeHandle(codeHndl);
        dnr = nil;
    }
    return(rc);
}



typedef OSErr (*CloseResolverProcPtr)(long selector);

enum {
    uppCloseResolverProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr CloseResolverUPP;

#define NewCloseResolverProc(userRoutine)                       \
        (CloseResolverUPP) mzNewRoutineDescriptor(userRoutine, uppCloseResolverProcInfo, GetCurrentISA())
#define CallCloseResolverProc(userRoutine, selector)    \
        CallUniversalProc(userRoutine, uppCloseResolverProcInfo, selector)
#else
typedef CloseResolverProcPtr CloseResolverUPP;

#define NewCloseResolverProc(userRoutine)                       \
        (CloseResolverUPP)(userRoutine)
#define CallCloseResolverProc(userRoutine, selector)    \
        (*userRoutine)(selector)
#endif



pascal OSErr CloseResolver()
{
    if (dnr == nil)
        /* resolver not loaded error */
        return(notOpenErr);

    /* call close resolver */
    CallCloseResolverProc((CloseResolverUPP) dnr, CLOSERESOLVER);

    /* release the DNR resource package */
    HUnlock(codeHndl);
    DisposeHandle(codeHndl);
    dnr = nil;
    return(noErr);
}




typedef OSErr (*StrToAddrProcPtr)(long selector, char* hostName, struct hostInfo* rtnStruct,
                                    long resultProc, char* userData);

enum {
    uppStrToAddrProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
         | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(char *)))
         | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(struct hostInfo *)))
         | STACK_ROUTINE_PARAMETER(4, SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(5, SIZE_CODE(sizeof(char *)))
};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr StrToAddrUPP;

#define NewStrToAddrProc(userRoutine)                       \
        (StrToAddrUPP) mzNewRoutineDescriptor(userRoutine, uppStrToAddrProcInfo, GetCurrentISA())
#define CallStrToAddrProc(userRoutine, selector, hostName, rtnStruct, resultProc, userData) \
        CallUniversalProc(userRoutine, uppStrToAddrProcInfo, selector, hostName, rtnStruct, resultProc, userData)
#else
typedef StrToAddrProcPtr StrToAddrUPP;

#define NewStrToAddrProc(userRoutine)                       \
        (StrToAddrUPP)(userRoutine)
#define CallStrToAddrProc(userRoutine, selector, hostName, rtnStruct, resultProc, userData) \
        (*userRoutine)(selector, hostName, rtnStruct, resultProc, userData)
#endif



pascal OSErr StrToAddr(hostName, rtnStruct, resultproc, userDataPtr)
char *hostName;
struct hostInfo *rtnStruct;
long resultproc;
char *userDataPtr;
{
    if (dnr == nil)
        /* resolver not loaded error */
        return(notOpenErr);

    return(CallStrToAddrProc((StrToAddrUPP) dnr, STRTOADDR, hostName, (void*) rtnStruct,
                            resultproc, userDataPtr));
}


typedef OSErr (*AddrToStrProcPtr)(long selector, long address, char* hostName);

enum {
    uppAddrToStrProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
         | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(unsigned long)))
         | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(char *)))
};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr AddrToStrUPP;

#define NewAddrToStrProc(userRoutine)                       \
        (AddrToStrUPP) mzNewRoutineDescriptor(userRoutine, uppAddrToStrProcInfo, GetCurrentISA())
#define CallAddrToStrProc(userRoutine, selector, address, hostName) \
        CallUniversalProc(userRoutine, uppAddrToStrProcInfo, selector, address, hostName)
#else
typedef AddrToStrProcPtr AddrToStrUPP;

#define NewAddrToStrProc(userRoutine)                       \
        (AddrToStrUPP)(userRoutine)
#define CallAddrToStrProc(userRoutine, selector, address, hostName) \
        (*userRoutine)(selector, address, hostName)
#endif


pascal OSErr AddrToStr(addr, addrStr)
unsigned long addr;
char *addrStr;
{
    if (dnr == nil)
        /* resolver not loaded error */
        return(notOpenErr);

    CallAddrToStrProc((AddrToStrUPP) dnr, ADDRTOSTR, addr, addrStr);

    return(noErr);
}



typedef OSErr (*EnumCacheProcPtr)(long selector, long result, char* userData);

enum {
    uppEnumCacheProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
         | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(char *)))
};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr EnumCacheUPP;

#define NewEnumCacheProc(userRoutine)                       \
        (EnumCacheUPP) mzNewRoutineDescriptor(userRoutine, uppEnumCacheProcInfo, GetCurrentISA())
#define CallEnumCacheProc(userRoutine, selector, result, userData)  \
        CallUniversalProc(userRoutine, uppEnumCacheProcInfo, selector, result, userData)
#else
typedef EnumCacheProcPtr EnumCacheUPP;

#define NewEnumCacheProc(userRoutine)                       \
        (EnumCacheUPP)(userRoutine)
#define CallEnumCacheProc(userRoutine, selector, result, userData)  \
        (*userRoutine)(selector, result, userData)
#endif



pascal OSErr EnumCache(resultproc, userDataPtr)
long resultproc;
char *userDataPtr;
{
    if (dnr == nil)
        /* resolver not loaded error */
        return(notOpenErr);

    return(CallEnumCacheProc((EnumCacheUPP) dnr, ENUMCACHE, resultproc, userDataPtr));
}



typedef OSErr (*AddrToNameProcPtr)(long selector, unsigned long addr, struct hostInfo* rtnStruct,
                                    long resultProc, char* userData);

enum {
    uppAddrToNameProcInfo = kCStackBased
         | RESULT_SIZE(SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(short)))
         | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(unsigned long)))
         | STACK_ROUTINE_PARAMETER(3, SIZE_CODE(sizeof(struct hostInfo *)))
         | STACK_ROUTINE_PARAMETER(4, SIZE_CODE(sizeof(long)))
         | STACK_ROUTINE_PARAMETER(5, SIZE_CODE(sizeof(char *)))

};

#if USESROUTINEDESCRIPTORS
typedef UniversalProcPtr AddrToNameUPP;

#define NewAddrToNameProc(userRoutine)                      \
        (AddrToNameUPP) mzNewRoutineDescriptor(userRoutine, uppAddrToNameProcInfo, GetCurrentISA())
#define CallAddrToNameProc(userRoutine, selector, addr, rtnStruct, resultProc, userData)    \
        CallUniversalProc(userRoutine, uppAddrToNameProcInfo, selector, addr, rtnStruct, resultProc, userData)
#else
typedef AddrToNameProcPtr AddrToNameUPP;

#define NewAddrToNameProc(userRoutine)                      \
        (AddrToNameUPP)(userRoutine)
#define CallAddrToNameProc(userRoutine, selector, addr, rtnStruct, resultProc, userData)    \
        (*userRoutine)(selector, addr, rtnStruct, resultProc, userData)
#endif



pascal OSErr AddrToName(addr, rtnStruct, resultproc, userDataPtr)
unsigned long addr;
struct hostInfo *rtnStruct;
long resultproc;
char *userDataPtr;
{
    if (dnr == nil)
        /* resolver not loaded error */
        return(notOpenErr);

    return(CallAddrToNameProc((AddrToNameUPP) dnr, ADDRTONAME, addr, rtnStruct, resultproc, userDataPtr));
}

typedef long ResultUPP;
enum {
	uppResultProcInfo = kPascalStackBased
		 | STACK_ROUTINE_PARAMETER(1, SIZE_CODE(sizeof(struct hostInfo*)))
		 | STACK_ROUTINE_PARAMETER(2, SIZE_CODE(sizeof(Ptr))),
      };
#define NewResultProc(proc) (ResultUPP)mzNewRoutineDescriptor((ProcPtr)proc, uppResultProcInfo, GetCurrentArchitecture())
