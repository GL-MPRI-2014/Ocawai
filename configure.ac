AC_PREREQ([2.68])
AC_INIT([OCAWAI], [1.0], [ens-info-genielog@lists.crans.org])

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
   AC_MSG_ERROR([You must install an Ocaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install Ocamlfind. Please refer to the section 'dependences' of the README file to get more informations.])
fi

#for interface
AC_CHECK_OCAML_PKG([ocsfml])
if test "$OCAML_PKG_ocsfml" = "no"; then
   AC_MSG_ERROR(['ocsfml' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi

#for interface and engine
AC_CHECK_OCAML_PKG([atdgen])
if test "$OCAML_PKG_atdgen" = "no"; then
   AC_MSG_ERROR(['atdgen' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi

#for everyone's logging
AC_CHECK_OCAML_PKG([dolog])
if test "$OCAML_PKG_dolog" = "no"; then
   AC_MSG_ERROR(['dolog' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi


#mm for music
AC_CHECK_OCAML_PKG([mm])
if test "$OCAML_PKG_mm" = "no"; then
   AC_MSG_ERROR(['mm' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi

#pulseaudio for music
AC_CHECK_OCAML_PKG([pulseaudio])
if test "$OCAML_PKG_pulseaudio" = "no"; then
   AC_MSG_ERROR(['pulseaudio' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi




AC_CHECK_OCAML_PKG([num])
if test "$OCAML_PKG_num" = "no"; then
   AC_MSG_ERROR(['num' module is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi


AC_CHECK_OCAML_PKG([threads])
if test "$OCAML_PKG_threads" = "no"; then
   AC_MSG_ERROR(['threads' is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi

##dependency for the tests

AC_CHECK_OCAML_PKG([oUnit])
if test "$OCAML_PKG_oUnit" = "no"; then
   AC_MSG_ERROR(['ounit' is missing. Please refer to the section 'dependences' of the README file to get more informations.])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
