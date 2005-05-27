# Makefile for srpersist.so on Unix boxes

# change to ODBC version supported by driver manager
ODBCVER=0x0351

# static library that implements driver manager
DRIVER_MGR_LIB=odbc

# driver manager library directory
LIBDIR=/usr/site/unixODBC/lib

# gcc 3.2 needs libstdc++.a
# see also srpmain.so target, below
STDCPP_LIBDIR=/usr/lib/gcc-lib/i386-redhat-linux/3.2

# driver manager include directory
INCDIR=/usr/local/include

MZC=../../bin/mzc 

CPP=gcc 

# Add -DNO_SQLUCODE if driver manager does not have SQLUCODE.H
# Remove HAVE_CHAR, etc. if CHAR, etc. not defined by compiler
#  or driver manager 
CPP_FLAGS=-g -DODBCVER=$(ODBCVER) \
	-DHAVE_CHAR -DHAVE_LPWSTR -DHAVE_WCHAR \
	-I. -I../../include -I$(INCDIR) -c

OBJS=srpersist.o srpbuffer.o srptypes.o

ARCH=`../../bin/mzscheme -vm -e '(display (system-library-subpath))'`
 
NATIVE_DIR=../../collects/srpersist/private/compiled/native/$(ARCH)

.cxx.o ::
	$(CPP) $(CPP_FLAGS) $<  

all : srpmain.so

install : srpmain.so 
	mkdir -p $(NATIVE_DIR)
	cp -p srpmain.so $(NATIVE_DIR)/

clean :
	rm -f *.o
	rm -f *.so

srpmain.so : $(OBJS)
	$(MZC) --ld srpmain.so $(OBJS) \
	-L$(LIBDIR) -l$(DRIVER_MGR_LIB) \
#	-L$(STDCPP_LIBDIR) -lstdc++
# uncomment previous line for gcc 3.2

srpersist.o : srpersist.cxx srptypes.h srpbuffer.h srpersist.h srpprims.tbl \
	srpstructs.tbl srpexns.tbl srpconsts.tbl srpinfo.tbl srpbitmask.tbl
	$(CPP) $(CPP_FLAGS) srpersist.cxx

srpbuffer.o : srpbuffer.cxx srptypes.h srpbuffer.h srpersist.h
	$(CPP) $(CPP_FLAGS) srpbuffer.cxx

srptypes.o : srptypes.cxx srptypes.h srpersist.h
	$(CPP) $(CPP_FLAGS) srptypes.cxx


