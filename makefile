#
# $Revision: ?.? $
#
# $Date: 1994/02/25
#
# Purpose: Makefile for conjtest, conjugat and wildcard.cc modules
# Usage: dmake
#
#----------------------------------------------------------------------
# (COMMON)
#----------------------------------------------------------------------
.INCLUDE: $(MKHEADER)
.KEEP_STATE:=

#foo:makefile
#   $(mktmp $(OLDLIB))
#----------------------------------------------------------------------
# Flag settings
#
# format = <SYS><AREA><BUILDMODE>
#
# where:
#   <SYS> is one of [NT|DOS|WIN] from SYS environment variable
#   <AREA> is one of [DEFINES|CFLAGS|CPPFLAGS|LFLAGS|LIBS] 
#   <BUILDMODE> is one of [_G|_D|_R] _G=global,_D=debug, _R=retail
#----------------------------------------------------------------------

#--------
# Global
#--------
DEFINES_G += -DUSEROGUE -DAPP_Main=main
DEFINES_D += 
DEFINES_R += 
RESFILE = 
DEFFILE = 

#--------
# DOS
#--------
DOSLFLAGS_G +=

#--------
# Windows
#--------

#--------
# NT 
#--------

#--------
# sun4 
#--------
#sun4DEFINES_G += -DSTUFF_LIKE_THIS
#sun4LLIBS_G += names_of_libraries (without lib-prefix and .a suffix)
#sun4LFLAGS_G += -Lpath_of_library_directory


#----------------------------------------------------------------------
# Component lists
#----------------------------------------------------------------------

#--------
# Global
#--------
CMOBJS = conjtest$O conjugat$O wildcard$O 
PROMOTE = 

#--------
# DOS
#--------
DOSOBJS = 
DOSPROMOTE = 

#--------
# Windows
#--------
WINOBJS= $(CMOBJS)
WINPROMOTE = 

#--------
# NT 
#--------
NTOBJS = $(CMOBJS)
NTPROMOTE = 

#--------
# sun4 
#--------
sun4OBJS = $(CMOBJS)
sun4PROMOTE = 


#----------------------------------------------------------------------
# Build output
#----------------------------------------------------------------------
TARGET = conjtest$(EXESFX)

#----------------------------------------------------------------------
# (COMMON)
#----------------------------------------------------------------------
.INCLUDE: $(MKTRAILER)
