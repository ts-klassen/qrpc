# $NetBSD$

DISTNAME=       voicevox_core-0.16.3
CATEGORIES=     audio
MASTER_SITES=   https://github.com/VOICEVOX/voicevox_core/releases/download/${PKGVERSION_NOREV}/
DIST_SUBDIR=    voicevox_core

MAINTAINER=     qrpc@su-shiki.com
COMMENT=        VOICEVOX Core runtime assets with CUDA (onnxruntime, dict, models)
LICENSE=        mit

RESTRICTED=     License acceptance required by upstream downloader
NO_BIN_ON_CDROM=yes
NO_SRC_ON_CDROM=yes
NO_BIN_ON_FTP=  yes
NO_SRC_ON_FTP=  yes

USE_LANGUAGES=  # none
USE_TOOLS+=     pax tar unzip

WRKSRC=         ${WRKDIR}
NO_CONFIGURE=   yes

# These bundled CUDA/onnxruntime libs live under share/, not the normal lib
# directories, so shlib scanning would otherwise record destdir paths.
CHECK_SHLIBS_SKIP+=    share/voicevox_core/additional_libraries/*
CHECK_SHLIBS_SKIP+=    share/voicevox_core/onnxruntime/lib/*

VV_OUTPUT=              ${WRKSRC}/voicevox_core
VV_OS=                  linux
VV_CPU_ARCH=            x64
VV_DEVICE=              cuda
VV_MODELS_VERSION=      0.16.2
VV_ONNXRUNTIME_VERSION= voicevox_onnxruntime-1.17.3
VV_ADDITIONAL_LIBS_VER= 0.2.0
VV_DICT_DISTFILE=       open_jtalk_dic_utf_8-1.11.tar.gz
VV_DICT_DIR=            open_jtalk_dic_utf_8-1.11
VV_DICT_SRC=            ${WRKSRC}/${VV_DICT_DIR}
VV_MODELS_VVMS= \
	0.vvm \
	1.vvm \
	2.vvm \
	3.vvm \
	4.vvm \
	5.vvm \
	6.vvm \
	7.vvm \
	8.vvm \
	9.vvm \
	10.vvm \
	11.vvm \
	12.vvm \
	13.vvm \
	14.vvm \
	15.vvm \
	16.vvm \
	17.vvm \
	18.vvm \
	19.vvm \
	20.vvm \
	21.vvm \
	s0.vvm \
	n0.vvm
VV_MODELS_DOCS= \
	README.txt \
	TERMS.txt
VV_MODELS_FILES=        ${VV_MODELS_VVMS} ${VV_MODELS_DOCS}
VV_MODELS_SITES=        https://github.com/VOICEVOX/voicevox_vvm/releases/download/${VV_MODELS_VERSION}/
VV_MODELS_DISTDIR=      ${DISTDIR}/${DIST_SUBDIR}
VV_MAIN_FILTER=         ${FILESDIR}/main_filter.jq
VV_MAIN_NAME_OVERRIDES= ${FILESDIR}/main_name_overrides.json
VV_MAIN_TERMS_OVERRIDES=${FILESDIR}/main_terms_overrides.json
VV_MAIN_ORDER_OVERRIDES=${FILESDIR}/main_order_overrides.json

DISTFILES=              download-linux-x64 ${VV_DICT_DISTFILE} ${VV_MODELS_FILES}
SITES.${VV_DICT_DISTFILE}= \
	https://packages.qrpc.jp/
EXTRACT_ONLY=           ${VV_DICT_DISTFILE}
DOWNLOAD_BIN=   ${DISTDIR}/${DIST_SUBDIR}/download-linux-x64
.for f in ${VV_MODELS_FILES}
SITES.${f}= ${VV_MODELS_SITES}
.endfor

BUILD_DEPENDS+= ca-certificates-[0-9]*:../../security/ca-certificates
BUILD_DEPENDS+= jq:../../devel/jq

do-build:
	${RUN} ${RM} -rf ${VV_OUTPUT}
	${RUN} ${MKDIR} ${VV_OUTPUT}
	${RUN} ${CHMOD} +x ${DOWNLOAD_BIN}
	${RUN} printf 'y\n' | env HOME=${WRKDIR}/home LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
		PAGER=cat ${DOWNLOAD_BIN} \
			--output ${VV_OUTPUT} \
			--tries 0 \
			--exclude c-api \
			--exclude dict \
			--exclude models \
			--models-version ${VV_MODELS_VERSION} \
			--additional-libraries-version ${VV_ADDITIONAL_LIBS_VER} \
			--onnxruntime-version ${VV_ONNXRUNTIME_VERSION} \
			--devices ${VV_DEVICE} \
			--cpu-arch ${VV_CPU_ARCH} \
			--os ${VV_OS} || ${FALSE}
	${RUN} if [ ! -d ${VV_DICT_SRC} ]; then \
		${TAR} -xzf ${DISTDIR}/${DIST_SUBDIR}/${VV_DICT_DISTFILE} -C ${WRKDIR} || ${FALSE}; \
	fi
	${RUN} test -d ${VV_OUTPUT} || { ${ECHO} "voicevox_core download failed: ${VV_OUTPUT} missing"; ${FALSE}; }
	${RUN} test -d ${VV_DICT_SRC} || { ${ECHO} "Open JTalk dictionary missing: ${VV_DICT_SRC}"; ${FALSE}; }
	${RUN} ${MKDIR} ${VV_OUTPUT}/dict/${VV_DICT_DIR}
	cd ${VV_DICT_SRC} && ${PAX} -rw . ${VV_OUTPUT}/dict/${VV_DICT_DIR}
	${RUN} ${INSTALL_DATA_DIR} ${VV_OUTPUT}/models/vvms
	${RUN} for vvm in ${VV_MODELS_VVMS}; do \
		src=${VV_MODELS_DISTDIR}/$$vvm; \
		[ -f $$src ] || { ${ECHO} "Missing VVM distfile: $$src"; ${FALSE}; }; \
		${INSTALL_DATA} $$src ${VV_OUTPUT}/models/vvms/; \
	done
	${RUN} for doc in ${VV_MODELS_DOCS}; do \
		src=${VV_MODELS_DISTDIR}/$$doc; \
		[ -f $$src ] || { ${ECHO} "Missing VVM distfile: $$src"; ${FALSE}; }; \
		${INSTALL_DATA} $$src ${VV_OUTPUT}/models/; \
	done

do-install:
	${RUN} ${INSTALL_DATA_DIR} ${DESTDIR}${PREFIX}/share/voicevox_core
	cd ${VV_OUTPUT} && ${PAX} -rw . ${DESTDIR}${PREFIX}/share/voicevox_core
	${RUN} ${INSTALL_DATA_DIR} ${DESTDIR}${PREFIX}/etc/voicevox_core
	${RUN} ${SH} -c '\
		set -e; \
		out=${DESTDIR}${PREFIX}/etc/voicevox_core/models.json; \
		${MKDIR} -p ${WRKDIR}/tmp; \
		tmpdir=${WRKDIR}/tmp/models.$$; \
		${RM} -rf $$tmpdir; \
		${MKDIR} $$tmpdir; \
		found=0; \
		for vvm in ${DESTDIR}${PREFIX}/share/voicevox_core/models/vvms/*.vvm; do \
			[ -f $$vvm ] || continue; \
			found=1; \
			base=$$(basename $$vvm .vvm); \
			manifest=$$tmpdir/$$base.manifest.json; \
			metas=$$tmpdir/$$base.metas.json; \
			entry=$$tmpdir/$$base.entry.json; \
			${TOOLS_PATH.unzip} -p $$vvm manifest.json > $$manifest; \
			${TOOLS_PATH.unzip} -p $$vvm metas.json > $$metas; \
			${PREFIX}/bin/jq -n --arg key "$$base" \
				--slurpfile manifest $$manifest \
				--slurpfile metas $$metas \
				"{(\$$key): {manifest: \$$manifest[0], metas: \$$metas[0]}}" \
				> $$entry; \
		done; \
		if [ $$found -eq 0 ]; then ${ECHO} "No vvm models found in ${DESTDIR}${PREFIX}/share/voicevox_core/models/vvms" 1>&2; exit 1; fi; \
		${PREFIX}/bin/jq -s "add" $$tmpdir/*.entry.json > $$out; \
		${RM} -rf $$tmpdir; \
	'
	${RUN} ${SH} -c '\
		set -e; \
		in=${DESTDIR}${PREFIX}/etc/voicevox_core/models.json; \
		out=${DESTDIR}${PREFIX}/etc/voicevox_core/main.json; \
		filter=${VV_MAIN_FILTER}; \
		name_overrides=${VV_MAIN_NAME_OVERRIDES}; \
		terms_overrides=${VV_MAIN_TERMS_OVERRIDES}; \
		order_overrides=${VV_MAIN_ORDER_OVERRIDES}; \
		${PREFIX}/bin/jq -c -f $$filter --slurpfile name_overrides $$name_overrides \
			--slurpfile terms_overrides $$terms_overrides \
			--slurpfile order_overrides $$order_overrides $$in > $$out; \
	'

.include "../../mk/bsd.pkg.mk"
