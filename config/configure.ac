(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.loria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

AC_INIT([ACG DTK],[0.1],[sylvain.pogodalla@loria.fr])

#AC_COPYRIGHT([Cecill])

OCAML_REQUIRED=3.07

# First look for bytecode compilers (ocamlc.opt or ocamlc) in the path
# Fail if not present
AC_CHECK_PROGS(OCAMLC,ocamlc.opt ocamlc,no)
if test "$OCAMLC" = no ; then
        AC_MSG_ERROR(Cannot find ocamlc.)
fi

# Then look at native code compilers (ocamlopt.opt or ocamlopt) in the path
# Fail if not present
# we first look for ocaml in the path; if not present, we fail
AC_CHECK_PROGS(OCAMLCOPT,ocamlopt.opt ocamlopt,no)
if test "$OCAMLCOPT" = no ; then
        AC_MSG_ERROR(Cannot find ocamlc.opt.)
fi

# Look for the interpreter
AC_CHECK_PROG(OCAML,ocaml,ocaml,no)
if test "$OCAML" = no ; then
        AC_MSG_ERROR(Cannot find ocaml.)
fi

# Look for camlp4o
AC_CHECK_PROG(OCAMLP4,camlp4o,camlp4o,no)
if test "$OCAMLP4" = no ; then
        AC_MSG_ERROR(Cannot find camlp4o.)
fi

# Look for documentation generators (ocamldoc.opt or ocamldoc)
AC_CHECK_PROGS(OCAMLDOC,ocamldoc.opt ocamldoc,no)
if test "$OCAMLDOC" = no ; then
        AC_MSG_ERROR(Cannot find ocamldoc.)
fi

# Look for dependency generator (ocamldep.opt or ocamldep)
AC_CHECK_PROGS(OCAMLDEP,ocamldep.opt ocamldep,no)
if test "$OCAMLDEP" = no ; then
        AC_MSG_ERROR(Cannot find ocamldep.)
fi

# Look for camllex
AC_CHECK_PROGS(OCAMLLEX,ocamllex.opt ocamllex,no)
if test "$OCAMLLEX" = no ; then
	AC_MSG_ERROR(Cannot find ocamllex)
fi


# Look for ocamlfind
AC_PATH_PROG(OCAMLFIND,ocamlfind,no)

#Look for dypgen
DYPGEN_NEEDED=20080925
AC_ARG_VAR(DYPGEN_PATH,[Directory where to find dypgen if not in a standard location])
if test -n "$DYPGEN_PATH" ; then
   AC_CHECK_PROG(DYPGEN,dypgen,$DYPGEN_PATH/dypgen,no,$DYPGEN_PATH)
else
	AC_CHECK_PROGS(DYPGEN,dypgen.opt dypgen,no)
fi
if test "$DYPGEN" = no ; then
   AC_MSG_ERROR(Cannot find dypgen)
else
	DYPGEN_VERSION=`$DYPGEN --version | grep version | sed 's/^.* \(.*\)$/\1/'`
	AC_MSG_CHECKING([for $DYPGEN version])
	if test $DYPGEN_VERSION -ge $DYPGEN_NEEDED ; then
		   AC_MSG_RESULT($DYPGEN ($DYPGEN_VERSION) is ok)
	else
		   AC_MSG_ERROR($DYPGEN version $DYPGEN_VERSION found ; version $DYPGEN_NEEDED or greater is needed)
	fi
fi

# Look for DYPGEN_LIB with or without ocamlfind
AC_MSG_CHECKING([dypgen library])
AC_ARG_VAR(DYPGENLIB_PATH,[Directory where to find dypgen library if not in a standard location])
if test -n "$DYPGENLIB_PATH" ; then
   DYPGEN_PLACE=$DYPGENLIB_PATH
   DYPGEN_INCLUDE="-I $DYPGEN_PLACE"
   if test -f $DYPGEN_PLACE/dyp.cma ; then
      AC_MSG_RESULT(dypgen library is $DYPGEN_PLACE/dyp.cma)
   else
      AC_MSG_ERROR(Could not find dypgen library in $DYPGEN_PLACE)
   fi
else
	if test "$OCAMLFIND" != no ; then
	   if $OCAMLFIND query dypgen > /dev/null 2>&1 ; then
	         DYPGEN_INCLUDE_DIR=`$OCAMLFIND query dypgen`
		 DYPGEN_PLACE=$DYPGEN_INCLUDE_DIR
	   	 DYPGEN_INCLUDE="-I $DYPGEN_INCLUDE_DIR"
	   else
		if $OCAMLFIND query dyp > /dev/null 2>&1 ; then
		   DYPGEN_INCLUDE_DIR=`$OCAMLFIND query dyp`
		   DYPGEN_PLACE=$DYPGEN_INCLUDE_DIR
	   	   DYPGEN_INCLUDE="-I $DYPGEN_INCLUDE_DIR"
		else
		   AC_MSG_RESULT(dypgen library was not installed by ocamlfind)
	        fi
	   fi
	else
	   OCAML_LIB=`$OCAMLC -where`
	   # Old versions of dypgen where put in a dypgen directory
	   if test -d $OCAML_LIB/dypgen ; then
	      DYPGEN_PLACE=$OCAML_LIB/dypgen
	      DYPGEN_INCLUDE="-I +dypgen"
	   else
	   # New versions of dypgen where put in a dyp directory
	     if test -d $OCAML_LIB/dyp ; then
	      	DYPGEN_PLACE=$OCAML_LIB/dyp
	      	DYPGEN_INCLUDE="-I +dyp"
	     else
		AC_MSG_ERROR(Could not find a suitable place for dypgen library in $OCAML_LIB)
	     fi
	   fi  
	fi
fi
if test -f $DYPGEN_PLACE/dyp.cma ; then
   AC_MSG_RESULT(dypgen library is $DYPGEN_PLACE/dyp.cma)
else
   AC_MSG_ERROR(Could not find dypgen library "dyp.cma" in $DYPGEN_PLACE)
fi

# Look for KAPUTT_LIB with ocamlfind
AC_MSG_CHECKING([(optional) kaputt library])
if test "$OCAMLFIND" != no ; then
   if $OCAMLFIND query kaputt > /dev/null 2>&1 ; then
      KAPUTT_INCLUDE_DIR=`$OCAMLFIND query kaputt`
      KAPUTT_PLACE=$KAPUTT_INCLUDE_DIR
      KAPUTT_INCLUDE="-I $KAPUTT_INCLUDE_DIR"
   else
      AC_MSG_RESULT(kaputt library was not installed by ocamlfind)
   fi
else
   OCAML_LIB=`$OCAMLC -where`
   if test -d $OCAML_LIB/kaputt ; then
	      KAPUTT_PLACE=$OCAML_LIB/kaputt
	      KAPUTT_INCLUDE="-I +kaputt"
   else
   AC_MSG_RESULT(Could not find a suitable place for kaputt library in $OCAML_LIB. No link to the kaputt library will be allowed in compilation)
   fi
fi  
if test -f $KAPUTT_PLACE/kaputt.cma ; then
   KAPUTT_LIB=kaputt.cma
   AC_MSG_RESULT(kaputt library is $KAPUTT_PLACE/kaputt.cma)
else
   AC_MSG_RESULT(Could not find kaputt library "kaputt.cma" in $KAPUTT_PLACE. No link to the kaputt library will be allowed in compilation)
fi

if test -f $KAPUTT_PLACE/kaputt_pp.byte ; then
   KAPUTT_PP=$KAPUTT_PLACE/kaputt_pp.byte
   AC_MSG_RESULT(kaputt_pp.byte preprocessor is $KAPUTT_PP)
else
   AC_MSG_RESULT(Could not find kaputt_pp.byte preprocessor in $KAPUTT_PLACE.)
fi




# What is the OCAML version  ?
# we extract Ocaml version number
OCAML_VERSION=`$OCAMLC -version`
AC_MSG_CHECKING([for $OCAMLC version])
AC_MSG_RESULT([$OCAMLC version is $OCAML_VERSION])

# Check whether the caml version checker is provided
ML_CHECK_FILE=config/ml_check.ml
AC_CHECK_FILE($ML_CHECK_FILE,,AC_MSG_ERROR(Cannot check versions))

# check that this version is at least the one we need
if ($OCAML $ML_CHECK_FILE -ref $OCAML_REQUIRED $OCAML_VERSION > /dev/null) ; then
        AC_MSG_RESULT($OCAMLC version $OCAMLVERSION is ok)
else
        AC_MSG_ERROR($OCAMLC version $OCAMLVERSION found ; version $OCAML_NEEDED or greater is needed)
fi

# check if the dtypes option is present
if ($OCAML $ML_CHECK_FILE -ref 3.07 $OCAML_VERSION> /dev/null) ; then
	AC_MSG_RESULT(Compilation will be done with the -dtypes option)
        TYPES=-dtypes
else
	AC_MSG_RESULT(Compilation will NOT be done with the -dtypes option)
	TYPES=
fi


# check if the -w yz option is present
# and set the _loc variable of campl4 to loc
if ($OCAML $ML_CHECK_FILE -ref 3.09 $OCAML_VERSION > /dev/null) ; then
        OCAML09WARNINGS="-w yz"
	OCAMLP4_LOC="-loc loc"
	AC_MSG_RESULT($OCAMLP4 calls will be done with the $OCAMLP4_LOC option)
else
	OCAML09WARNINGS=
fi
AC_MSG_RESULT(Compilation will be done with the $OCAML09WARNINGS option)

if ($OCAML $ML_CHECK_FILE -ref 3.10 $OCAML_VERSION > /dev/null) ; then
	CAMLP4_LIB="camlp4lib.cma"
else
	CAMLP4_LIB="camlp4.cma"
fi
AC_MSG_RESULT($OCAMLP4 calls will be done with the $CAMLP4_LIB library)




############################
# substitutions to perform
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLCOPT)
AC_SUBST(OCAMLP4)
AC_SUBST(CAMLP4_LIB)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLDEP)

AC_SUBST(OCAMLLEX)
#AC_SUBST(OCAMLYACC)
AC_SUBST(DYPGEN)
AC_SUBST(DYPGEN_INCLUDE)
AC_SUBST(KAPUTT_INCLUDE)
AC_SUBST(KAPUTT_LIB)
AC_SUBST(KAPUTT_PP)

AC_SUBST(TYPES)
AC_SUBST(OCAML09WARNINGS)
AC_SUBST(OCAMLP4_LOC)

AC_SUBST(SET_MAKE)

AC_CONFIG_FILES([./Makefile config/Makefile src/Makefile.master src/Makefile.common src/Makefile src/utils/Makefile src/logic/Makefile src/grammars/Makefile src/acg-data/Makefile src/scripting/Makefile src/datalog/Makefile src/reduction/Makefile src/s_datalog/Makefile])

AC_PROG_MAKE_SET

AC_OUTPUT

