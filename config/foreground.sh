#!/usr/bin/env bash

export PATH=/opt/qrpc/pkg/bin:/usr/bin

exec /opt/qrpc/rel/bin/qrpc foreground \
  > >(exec /opt/qrpc/pkg/sbin/cronolog /opt/qrpc/var/log/qrpc/stdout/%Y%m/%d-%H.log) \
  2> >(exec /opt/qrpc/pkg/sbin/cronolog /opt/qrpc/var/log/qrpc/stderr/%Y%m/%d-%H.log)
