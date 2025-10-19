-module(qrpc_map_calls).

-export([parse_transform/2]).

%% Parse transform that allows expressions like M:Fun(A1,...) where M can be a
%% map at runtime. It rewrites such calls to a runtime-guarded case that:
%%  - If M is a map: calls local Fun(Map, A1, ...)
%%  - If M is an atom: performs the normal remote call M:Fun(A1,...)
%%  - Otherwise: raises badarg
%%
%% Calls with a literal atom module (e.g., lists:reverse/1) are left unchanged.

-spec parse_transform(Forms :: [erl_parse:abstract_form()], _Options :: term()) ->
          [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    [transform_form(F) || F <- Forms].

transform_form({function, L, Name, Arity, Clauses}) ->
    {function, L, Name, Arity, [transform_clause(C) || C <- Clauses]};
transform_form(Other) ->
    Other.

transform_clause({clause, Lc, Pats, Guards, Body}) ->
    {clause, Lc, Pats, Guards, [transform_expr(E) || E <- Body]}.

transform_expr({call, Lc, {remote, Lr, ModExpr, {atom, Lf, Fun}}, Args}) ->
    case is_atom_ast(ModExpr) of
        true ->
            %% Normal atom module call: leave untouched, but still transform args
            {call, Lc, {remote, Lr, ModExpr, {atom, Lf, Fun}},
             [transform_expr(A) || A <- Args]};
        false ->
            %% Non-atom module position: emit runtime-guarded dispatch
            ModT = transform_expr(ModExpr),
            ArgsT = [transform_expr(A) || A <- Args],
            build_runtime_dispatch(Lc, Lf, Lr, ModT, Fun, ArgsT)
    end;
transform_expr({call, Lc, Target, Args}) ->
    %% Other calls: transform recursively inside
    {call, Lc, transform_expr(Target), [transform_expr(A) || A <- Args]};
transform_expr({tuple, L, Es}) ->
    {tuple, L, [transform_expr(E) || E <- Es]};
transform_expr({cons, L, H, T}) ->
    {cons, L, transform_expr(H), transform_expr(T)};
transform_expr({lc, L, Expr, Qs}) ->
    {lc, L, transform_expr(Expr), [transform_expr(Q) || Q <- Qs]};
transform_expr({bc, L, Expr, Qs}) ->
    {bc, L, transform_expr(Expr), [transform_expr(Q) || Q <- Qs]};
transform_expr({block, L, Es}) ->
    {block, L, [transform_expr(E) || E <- Es]};
transform_expr({'fun', L, {clauses, Clauses}}) ->
    {'fun', L, {clauses, [transform_clause(C) || C <- Clauses]}};
transform_expr({'if', L, Cs}) ->
    {'if', L, [transform_clause(C) || C <- Cs]};
transform_expr({'case', L, E, Cs}) ->
    {'case', L, transform_expr(E), [transform_clause(C) || C <- Cs]};
transform_expr({'receive', L, Cs}) ->
    {'receive', L, [transform_clause(C) || C <- Cs]};
transform_expr({'receive', L, Cs, To, Te}) ->
    {'receive', L, [transform_clause(C) || C <- Cs], transform_expr(To), transform_expr(Te)};
transform_expr({'try', L, Es, Cs, Hs, Af}) ->
    {'try', L,
     [transform_expr(E) || E <- Es],
     [transform_clause(C) || C <- Cs],
     [transform_clause(H) || H <- Hs],
     [transform_expr(A) || A <- Af]};
transform_expr({op, L, Op, A}) ->
    {op, L, Op, transform_expr(A)};
transform_expr({op, L, Op, A, B}) ->
    {op, L, Op, transform_expr(A), transform_expr(B)};
transform_expr({match, L, P, E}) ->
    {match, L, P, transform_expr(E)};
transform_expr(List) when is_list(List) ->
    [transform_expr(E) || E <- List];
transform_expr(Other) ->
    Other.

is_atom_ast({atom, _, _}) -> true;
is_atom_ast(_) -> false.

build_runtime_dispatch(Lc, Lf, Lr, ModT, Fun, ArgsT) ->
    %% Build:
    %% case {M, A1, ...} of
    %%   {Map, AA1,...} when is_map(Map) -> Fun(Map, AA1,...);
    %%   {Mod, AA1,...} when is_atom(Mod) -> Mod:Fun(AA1,...);
    %%   _ -> erlang:error(badarg)
    %% end
    Suf = erlang:unique_integer([monotonic, positive]),
    VarM = mk_var(Lc, "__qrpc_m_", Suf),
    VarAs = [mk_var(Lc, "__qrpc_a" ++ integer_to_list(I) ++ "_", Suf)
             || I <- lists:seq(1, length(ArgsT))],

    TupleExpr = mk_tuple(Lc, [ModT | ArgsT]),
    PatTuple = mk_tuple(Lc, [VarM | VarAs]),

    GuardIsMap = mk_guard_call(Lc, is_map, [VarM]),
    GuardIsAtom = mk_guard_call(Lc, is_atom, [VarM]),

    %% Clause 1 body: Fun(Map, AA1,...)
    LocalFun = {atom, Lf, Fun},
    CallLocal = {call, Lc, LocalFun, [VarM | VarAs]},
    Cl1 = {clause, Lc, [PatTuple], [[GuardIsMap]], [CallLocal]},

    %% Clause 2 body: Mod:Fun(AA1,...)
    CallRemote = {call, Lc, {remote, Lr, VarM, {atom, Lf, Fun}}, VarAs},
    Cl2 = {clause, Lc, [PatTuple], [[GuardIsAtom]], [CallRemote]},

    %% Clause 3: badarg
    Err = {call, Lc, {remote, Lr, {atom, Lr, erlang}, {atom, Lr, error}},
           [{atom, Lc, badarg}]},
    Cl3 = {clause, Lc, [{var, Lc, '_'}], [], [Err]},

    {'case', Lc, TupleExpr, [Cl1, Cl2, Cl3]}.

mk_var(L, Prefix, Suf) ->
    Name = list_to_atom([$_ | Prefix] ++ integer_to_list(Suf)),
    {var, L, Name}.

mk_tuple(L, Es) ->
    {tuple, L, Es}.

mk_guard_call(L, Name, Args) ->
    {call, L, {atom, L, Name}, Args}.
