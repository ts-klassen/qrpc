DISTNAME=       qrpc-prod-0.3.0
CATEGORIES=     meta-pkgs www
MASTER_SITES=   # empty

MAINTAINER=     qrpc@su-shiki.com
COMMENT=        All you need to run qrpc as prod

DEPENDS+=       nawk>=20250726:../../lang/nawk
DEPENDS+=       cronolog>=1.6.2:../../www/cronolog
DEPENDS+=       lmdb>=0.9.33:../../databases/lmdb
DEPENDS+=       mit-krb5>=1.21.3nb2:../../security/mit-krb5
DEPENDS+=       py313-gssapi>=1.10.1:../../security/py-gssapi
DEPENDS+=       py313-paramiko>=4.0.0:../../security/py-paramiko
DEPENDS+=       duplicity>=3.0.5.1:../../sysutils/duplicity
DEPENDS+=       openssl>=3.6.0:../../security/openssl
DEPENDS+=       ca-certificates>=20250419:../../security/ca-certificates
DEPENDS+=       rabbitmq>=3.13.7:../../net/rabbitmq
DEPENDS+=       voicevox_core>=0.16.3nb1:../../audio/voicevox_core
DEPENDS+=       pkg_install>=20250417:../../pkgtools/pkg_install

LICENSE=        apache-2.0

META_PACKAGE=   yes

.include "../../mk/bsd.pkg.mk"
