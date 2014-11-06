AC_PREREQ([2.69])
AC_INIT([projet_GL_2014_2015], [V0.1], [ens-info-genielog@lists.crans.org])

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AC_CONFIG_MACRO_DIR([m4])
#echo "Testing for an Ocaml compiler"
#Check that the Ocaml compiler is installed
AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the Ocaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install Ocaml findlib (the ocamlfindlib command)])
fi

#for interface
AC_CHECK_OCAML_PKG([ocsfml])
if test "$OCAML_PKG_ocsfml" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'ocsfml'.])
fi

#for interface and engine
AC_CHECK_OCAML_PKG([atdgen])
if test "$OCAML_PKG_atdgen" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'atdgen'.])
fi


#for interface (music dependencies)
AC_CHECK_OCAML_PKG([pulseaudio])
if test "$OCAML_PKG_pulseaudio" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'pulseaudio'.])
fi


AC_CHECK_OCAML_PKG([mm])
if test "$OCAML_PKG_mm" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'mm'.])
fi




AC_CHECK_OCAML_PKG([num])
if test "$OCAML_PKG_num" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'num'.])
fi


AC_CHECK_OCAML_PKG([threads])
if test "$OCAML_PKG_threads" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'threads'.])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
