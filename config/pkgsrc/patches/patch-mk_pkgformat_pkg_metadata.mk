$NetBSD$

Strip ${DESTDIR} from ldd-derived REQUIRES entries so packaged
libraries installed under PREFIX are not misrecorded as absolute
paths back into the build destdir. Without this, pkg_add rejects
packages like voicevox_core with a bogus dependency on
/root/pkgsrc/.../.destdir/opt/qrpc/pkg/share/voicevox_core/...

--- mk/pkgformat/pkg/metadata.mk.orig	2025-10-24 10:30:53.000000000 +0000
+++ mk/pkgformat/pkg/metadata.mk
@@ -81,7 +81,7 @@
 	ELF)								\
 		libs=`${AWK} '/\/lib.*\.so(\.[0-9]+)*$$/ { print "${DESTDIR}${PREFIX}/" $$0 } END { exit 0 }' ${_PLIST_NOKEYWORDS}`; \
 		if ${TEST} -n "$$bins" -o -n "$$libs"; then		\
-			requires=`(${PKGSRC_SETENV} ${LDD_ENV:U} $$ldd $$bins $$libs 2>/dev/null || ${TRUE}) | ${AWK} '$$2 == "=>" && $$3 ~ "/" { print $$3 }' | ${SORT} -u`; \
+			requires=`(${PKGSRC_SETENV} ${LDD_ENV:U} $$ldd $$bins $$libs 2>/dev/null || ${TRUE}) | ${AWK} '$$2 == "=>" && $$3 ~ "/" { print $$3 }' | ${SED} -e 's,^${DESTDIR},,' | ${SORT} -u`; \
 		fi;							\
 		linklibs=`${AWK} '/.*\.so(\.[0-9]+)*$$/ { print "${DESTDIR}${PREFIX}/" $$0 }' ${_PLIST_NOKEYWORDS}`; \
 		for i in $$linklibs; do					\
