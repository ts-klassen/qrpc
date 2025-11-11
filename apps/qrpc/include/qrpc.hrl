-ifndef(QRPC_HRL).
-define(QRPC_HRL,true).

-define(QRPC_ERROR(Payload), qrpc_error:macro_QRPC_ERROR({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?FILE, ?LINE, ?OTP_RELEASE}, Payload)).

-define(QRPC_CATCH(Error), error:Error=#{ qrpc_map_calls := #{ module := qrpc_error, type := error, pos := head} }).

-define(QRPC_SUBCONF_GET(Key), qrpc_conf:get([subsystem, element(2, application:get_application(?MODULE)), ?MODULE, Key])).

-define(QRPC_SUBCONF_GET(Key, Default), qrpc_conf:get([subsystem, element(2, application:get_application(?MODULE)), ?MODULE, Key], Default)).

-define(QRPC_SUBCONF_LOOKUP(Key), qrpc_conf:lookup([subsystem, element(2, application:get_application(?MODULE)), ?MODULE, Key])).

-endif.
