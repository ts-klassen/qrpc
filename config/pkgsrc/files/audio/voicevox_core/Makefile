# $NetBSD$

DISTNAME=       voicevox_core-0.16.2
CATEGORIES=     audio
MASTER_SITES=   https://github.com/VOICEVOX/voicevox_core/releases/download/${PKGVERSION_NOREV}/
DISTFILES=      download-linux-x64
EXTRACT_ONLY=   # empty
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
USE_TOOLS+=     pax

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
VV_MODELS_VERSION=      0.16.0
VV_ONNXRUNTIME_VERSION= voicevox_onnxruntime-1.17.3
VV_ADDITIONAL_LIBS_VER= 0.2.0

DOWNLOAD_BIN=   ${DISTDIR}/${DIST_SUBDIR}/download-linux-x64

BUILD_DEPENDS+= ca-certificates-[0-9]*:../../security/ca-certificates

do-build:
	${RUN} ${RM} -rf ${VV_OUTPUT}
	${RUN} ${MKDIR} ${VV_OUTPUT}
	${RUN} ${CHMOD} +x ${DOWNLOAD_BIN}
	${RUN} printf 'y\n' | env HOME=${WRKDIR}/home LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
		PAGER=cat ${DOWNLOAD_BIN} \
			--output ${VV_OUTPUT} \
			--tries 0 \
			--exclude c-api \
			--models-version ${VV_MODELS_VERSION} \
			--additional-libraries-version ${VV_ADDITIONAL_LIBS_VER} \
			--onnxruntime-version ${VV_ONNXRUNTIME_VERSION} \
			--devices ${VV_DEVICE} \
			--cpu-arch ${VV_CPU_ARCH} \
			--os ${VV_OS} || ${FALSE}
	${RUN} test -d ${VV_OUTPUT} || { ${ECHO} "voicevox_core download failed: ${VV_OUTPUT} missing"; ${FALSE}; }

do-install:
	${RUN} ${INSTALL_DATA_DIR} ${DESTDIR}${PREFIX}/share/voicevox_core
	cd ${VV_OUTPUT} && ${PAX} -rw . ${DESTDIR}${PREFIX}/share/voicevox_core

.include "../../mk/bsd.pkg.mk"
