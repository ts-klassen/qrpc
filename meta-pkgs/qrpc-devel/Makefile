DISTNAME=       qrpc-devel-0.1.0
CATEGORIES=     meta-pkgs www
MASTER_SITES=   # empty

MAINTAINER=     qrpc@su-shiki.com
COMMENT=        All you need to build qrpc

DEPENDS+=       rust-bin>=1.90.0:../../lang/rust-bin
DEPENDS+=       rebar3>=3.24.0:../../devel/rebar3
DEPENDS+=       qrpc-prod>=0.1.0:../../meta-pkgs/qrpc-prod
DEPENDS+=       pkg_tarup>=1.9.1:../../pkgtools/pkg_tarup
DEPENDS+=       ansible>=12.1.0:../../sysutils/ansible
DEPENDS+=       nodejs>=22.21.1:../../lang/nodejs22

LICENSE=        apache-2.0

META_PACKAGE=   yes

.include "../../mk/bsd.pkg.mk"
