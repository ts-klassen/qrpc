qrpc
=====

An OTP application

Build
-----

    $ rebar3 compile


How To Add Subsystem
--------------------

```
subsystem="q_tut"

cd apps
/opt/qrpc/pkg/bin/rebar3 new app "$subsystem"
rm "${subsystem}/LICENSE"*
echo "$(date --iso)  ts-klassen  <qrpc@su-shiki.com>" > "${subsystem}/ChangeLog"
cat >> "${subsystem}/ChangeLog" << EOF

	* VERSION: 0.1.0
	Initial setup.
EOF
```

Don't forget to add it to `relx` of `rebar3.config`.
