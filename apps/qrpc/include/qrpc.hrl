-ifndef(QRPC_HRL).
-define(QRPC_HRL,true).

-define(QRPC_ERROR(Payload), qrpc_error:macro_QRPC_ERROR({?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?FILE, ?LINE, ?OTP_RELEASE}, Payload)).

-define(QRPC_CATCH(Error), error:Error=#{ qrpc_map_calls := #{ module := qrpc_error, type := error, pos := head} }).

-endif.
