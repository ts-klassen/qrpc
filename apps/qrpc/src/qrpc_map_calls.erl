-module(qrpc_map_calls).

%% Public API
-export([parse_transform/2, map_call/3]).

%%
%% parse_transform/2
%%
%% Rewrites any remote call where the module expression is not an atom,
%% from:   ModExpr:Fun(Args...)
%% to:     qrpc_map_calls:map_call(ModExpr, Fun, [Args...])
%%
%% This keeps atom-module calls untouched. We do not attempt fallbacks;
%% if the value is not a map with the expected metadata at runtime,
%% the helper will crash fast as requested.

-spec parse_transform(Forms :: [erl_parse:abstract_form()], _Options :: term()) ->
          [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    lists:map(fun transform_form/1, Forms).

%% Transform top-level form nodes. Only function bodies need traversal.
transform_form({function, A, Name, Arity, Clauses}) ->
    {function, A, Name, Arity, [transform_clause(C) || C <- Clauses]};
transform_form(Other) ->
    Other.

transform_clause({clause, A, Pats, Guards, Body}) ->
    {clause, A, Pats, Guards, [transform_expr(E) || E <- Body]}.

%% Recursively transform expressions
transform_expr({call, A, {remote, AR, ModExpr, {atom, AF, Fun}}, Args}) ->
    case ModExpr of
        {atom, _, _} ->
            %% Atom module: keep as-is
            {call, A, {remote, AR, ModExpr, {atom, AF, Fun}}, [transform_expr(E) || E <- Args]};
        _ ->
            %% Dynamic module expr: rewrite to qrpc_map_calls:map_call(ModExpr, Fun, [Args...])
            MapMod = {atom, AR, ?MODULE},
            MapFun = {atom, AF, map_call},
            TransformedArgs = [transform_expr(E) || E <- Args],
            ArgsList = list_ast(TransformedArgs, A),
            {call, A, {remote, AR, MapMod, MapFun}, [transform_expr(ModExpr), {atom, AF, Fun}, ArgsList]}
    end;
transform_expr({call, A, F, Args}) ->
    {call, A, transform_expr(F), [transform_expr(E) || E <- Args]};
transform_expr({lc, A, Expr, Quals}) ->
    {lc, A, transform_expr(Expr), [transform_expr(Q) || Q <- Quals]};
transform_expr({bc, A, Expr, Quals}) ->
    {bc, A, transform_expr(Expr), [transform_expr(Q) || Q <- Quals]};
transform_expr({'if', A, Clauses}) ->
    {'if', A, [transform_clause(C) || C <- Clauses]};
transform_expr({'case', A, Expr, Clauses}) ->
    {'case', A, transform_expr(Expr), [transform_clause(C) || C <- Clauses]};
transform_expr({'fun', A, {clauses, Clauses}}) ->
    {'fun', A, {clauses, [transform_clause(C) || C <- Clauses]}};
transform_expr({'receive', A, Clauses}) ->
    {'receive', A, [transform_clause(C) || C <- Clauses]};
transform_expr({'receive', A, Clauses, TimeoutExpr, TimeoutBody}) ->
    {'receive', A,
     [transform_clause(C) || C <- Clauses],
     transform_expr(TimeoutExpr),
     [transform_expr(E) || E <- TimeoutBody]};
transform_expr({'try', A, Body, Clauses, Handlers, After}) ->
    {'try', A,
     [transform_expr(E) || E <- Body],
     [transform_clause(C) || C <- Clauses],
     [transform_clause(C) || C <- Handlers],
     [transform_expr(E) || E <- After]};
transform_expr({tuple, A, Es}) -> {tuple, A, [transform_expr(E) || E <- Es]};
transform_expr({cons, A, H, T}) -> {cons, A, transform_expr(H), transform_expr(T)};
transform_expr({list, A, Es}) -> {list, A, [transform_expr(E) || E <- Es]};
transform_expr({op, A, Op, E}) -> {op, A, Op, transform_expr(E)};
transform_expr({op, A, Op, L, R}) -> {op, A, Op, transform_expr(L), transform_expr(R)};
transform_expr({match, A, L, R}) -> {match, A, transform_expr(L), transform_expr(R)};
transform_expr({remote, A, L, R}) -> {remote, A, transform_expr(L), transform_expr(R)};
transform_expr({block, A, Es}) -> {block, A, [transform_expr(E) || E <- Es]};
transform_expr({map, A, Assocs}) -> {map, A, [transform_expr(E) || E <- Assocs]};
transform_expr({map, A, Base, Assocs}) -> {map, A, transform_expr(Base), [transform_expr(E) || E <- Assocs]};
transform_expr({map_field_assoc, A, K, V}) -> {map_field_assoc, A, transform_expr(K), transform_expr(V)};
transform_expr({map_field_exact, A, K, V}) -> {map_field_exact, A, transform_expr(K), transform_expr(V)};
transform_expr({record, A, Name, Fields}) -> {record, A, Name, [transform_expr(E) || E <- Fields]};
transform_expr({record_field, A, Name, Val}) -> {record_field, A, Name, transform_expr(Val)};
transform_expr({record_field, A, Rec, Name, Val}) -> {record_field, A, transform_expr(Rec), Name, transform_expr(Val)};
transform_expr({record, A, Rec, Name, Fields}) -> {record, A, transform_expr(Rec), Name, [transform_expr(E) || E <- Fields]};
transform_expr({bin, A, Elems}) -> {bin, A, [transform_expr(E) || E <- Elems]};
transform_expr({bin_element, A, E, S, T}) -> {bin_element, A, transform_expr(E), S, T};
transform_expr({clause, _, _, _, _}=C) -> transform_clause(C);
transform_expr(Other) -> Other.

list_ast([], A) -> {nil, A};
list_ast([H|T], A) -> {cons, A, H, list_ast(T, A)}.

%%
%% Runtime helper invoked by transformed calls.
%%
-spec map_call(Map :: map(), Fun :: atom(), Args :: [term()]) -> term().
map_call(Map, Fun, Args) when is_map(Map), is_atom(Fun), is_list(Args) ->
    Cfg = maps:get(qrpc_map_calls, Map),
    Mod = maps:get(module, Cfg),
    Pos = maps:get(pos, Cfg),
    case Pos of
        head -> apply(Mod, Fun, [Map | Args]);
        tail -> apply(Mod, Fun, Args ++ [Map])
    end.
