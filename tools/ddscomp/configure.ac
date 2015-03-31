AC_INIT(ddscomp.cc)

PACKAGE="ddscomp"
VERSION="1.1.0"
TARGET="ddscomp"

AC_SUBST(Version, $VERSION)
AC_SUBST(Target, $TARGET)

AC_PROG_MAKE_SET

AC_HEADER_STDC

AC_PROG_CXX

AC_ARG_WITH(core, [  --with-core=dir           Core code base directory. Default: ../.. ],,[with_core="../.."])
AC_SUBST(CORE_DIR, $with_core)

AC_ARG_WITH(priv, [  --with-priv=dir           Priv code base directory. Default: ../.. ],,[with_priv="../../priv"])
AC_SUBST(PRIV_DIR, $with_priv)

AC_ARG_WITH(ffmpeg, [  --with-ffmpeg=dir       Where to look for ffmpeg. Default: ../../../extern ],,[with_ffmpeg="../../extern"])
AC_SUBST(FFMPEG_DIR, $with_ffmpeg)


AC_ARG_WITH(libungif, [  --with-libungif=dir       Where to look for ungiflib. Default: /usr/local ],,[with_libungif="/usr/local"])
AC_SUBST(LIBUNGIF_DIR, $with_libungif)

AC_OUTPUT(Makefile)
