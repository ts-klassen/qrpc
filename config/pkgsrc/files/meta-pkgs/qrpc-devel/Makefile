DISTNAME=       qrpc-devel-0.2.0
CATEGORIES=     meta-pkgs www
MASTER_SITES=   # empty

MAINTAINER=     qrpc@su-shiki.com
COMMENT=        All you need to build qrpc

DEPENDS+=       rust-bin>=1.90.0:../../lang/rust-bin
DEPENDS+=       rebar3>=3.24.0:../../devel/rebar3
DEPENDS+=       pkgconf>=2.4.3:../../devel/pkgconf
DEPENDS+=       zip>=3.0:../../archivers/zip
DEPENDS+=       unzip>=6.0:../../archivers/unzip
DEPENDS+=       jq>=1.8.1:../../devel/jq
DEPENDS+=       qrpc-prod>=0.1.0:../../meta-pkgs/qrpc-prod
DEPENDS+=       pkg_tarup>=1.9.1:../../pkgtools/pkg_tarup
DEPENDS+=       ansible>=12.1.0:../../sysutils/ansible
# FIXME: need more fix. ommit for qrpc-devel-0.1.0
# DEPENDS+=       gcc12-libs>=12.1.0:../../lang/gcc12-libs
# DEPENDS+=       nodejs>=22.21.1:../../lang/nodejs22
DEPENDS+=       mit-krb5>=1.21.3:../../security/mit-krb5

LICENSE=        apache-2.0

META_PACKAGE=   yes

.include "../../mk/bsd.pkg.mk"
