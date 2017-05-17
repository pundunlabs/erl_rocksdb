-module(rocksdb_utils_test).

-revision('$Revision: $ ').
-modified('$Date: $ ').

-include_lib("eunit/include/eunit.hrl").
-include("rocksdb.hrl").

-export([time_compare/1,
	 time_compare_min/2,
	 merge_sorted_n/1,
	 merge_sorted_nl/2,
	 merge_sorted_kvls/1,
	 merge_keymerge_n/1,
	 merge_keymerge_nl/2,
	 merge_keymerge_kvls/1,
	 get_kvls/3]).

-define(DEFAULT_LENGTH, 10000).
-define(DEFAULT_START, 1000000).
-define(DESCENDING, 0).
-define(ASCENDING, 1).

%%--------------------------------------------------------------------
%% @doc Time compare rocksdb_utils:merge_sorted_kvls/2 with lists:keymerge/3.
%% @end
%%--------------------------------------------------------------------
-spec time_compare(KVLs :: [kvl()]) -> {T1 :: integer(), T2 :: integer()}.
time_compare(KVLs) ->
    {T1, _} = timer:tc(?MODULE, merge_sorted_kvls, [KVLs]),
    {T2, _} = timer:tc(?MODULE, merge_keymerge_kvls, [KVLs]),
    {T1, T2}.

%%--------------------------------------------------------------------
%% @doc Time compare rocksdb_utils:merge_sorted_kvls/2 with lists:keymerge/3
%% N times and return shortest times.
%% @end
%%--------------------------------------------------------------------
-spec time_compare_min(KVLs :: [kvl()], N :: pos_integer()) -> {T1 :: integer(), T2 :: integer()}.
time_compare_min(KVLs, N) ->
    Fun =
	fun(_, {Min1, Min2}) ->
	    {T1, T2} = time_compare(KVLs),
	    Acc1 = if Min1 == undefined -> T1;
			Min1 < T1 -> Min1;
			true -> T1
		    end,
	    Acc2 = if Min2 == undefined -> T2;
			Min2 < T2 -> Min2;
			true -> T2
		    end,
	    {Acc1, Acc2}
	end,
    lists:foldl(Fun, {undefined, undefined}, lists:seq(1,N)).

%%--------------------------------------------------------------------
%% @doc Test merging N number of sorted key/value lists with each length 10000.
%% @end
%%--------------------------------------------------------------------
-spec merge_sorted_n(N :: pos_integer()) ->
    {time(), {ok, kvl()}} | {error, Reason :: any()}.
merge_sorted_n(N) ->
    merge_sorted_nl(N, ?DEFAULT_LENGTH).

%%--------------------------------------------------------------------
%% @doc Test merging N number of sorted key/value lists with each length L.
%% @end
%%--------------------------------------------------------------------
-spec merge_sorted_nl(N :: pos_integer(), L :: pos_integer()) ->
    {time(), {ok, kvl()}} | {error, Reason :: any()}.
merge_sorted_nl(N, L) ->
    KVLs = get_kvls(?DEFAULT_START, ?DEFAULT_START + L, N),
    timer:tc(?MODULE, merge_sorted_kvls, [KVLs]).

%%--------------------------------------------------------------------
%% @doc Test merging the provided sorted KVL lists.
%% @end
%%--------------------------------------------------------------------
-spec merge_sorted_kvls(KVLs :: [kvl()]) ->
    {ok, kvl()} | {error, Reason :: any()}.
merge_sorted_kvls(KVLs) ->
    rocksdb_utils:merge_sorted_kvls(?DESCENDING, KVLs).

%%--------------------------------------------------------------------
%% @doc Get Mod number of sorted key/value lists in squence Stop to Start
%% where each list contains same remainders of element indexes.
%% @end
%%--------------------------------------------------------------------
-spec get_kvls(Start :: integer(), Stop :: integer(), Mod :: pos_integer()) -> [kvl()].
get_kvls(Start, Stop, Mod) ->
    [get_modular_seq(Start, Stop, Mod, R)|| R <- lists:seq(0, Mod-1)].

%%--------------------------------------------------------------------
%% @doc Get a key/value list where keys are in range Stop to Start
%% and for each element Key, Key rem Mod = Rem where Mod and Rem
%% are provided arguments.
%% @end
%%--------------------------------------------------------------------
-spec get_modular_seq(Start :: integer(), Stop :: integer(),
		Mod :: pos_integer(), Rem :: non_neg_integer()) -> kvl().
get_modular_seq(Start, Stop, Mod, Rem) ->
    [ begin
	Str = list_to_binary(integer_to_list(X)),
	Data = list_to_binary("Data "++Str),
	{Str, Data}
      end || X <- lists:seq(Stop, Start, -1), X rem Mod == Rem].

%%--------------------------------------------------------------------
%% @doc Test merging N number of sorted key/value lists with each length 10000
%% with erlang lists:keymerge/3.
%% @end
%%--------------------------------------------------------------------
-spec merge_keymerge_n(N :: pos_integer()) ->
    {time(), {ok, kvl()}} | {error, Reason :: any()}.
merge_keymerge_n(N) ->
    merge_keymerge_nl(N, ?DEFAULT_LENGTH).

%%--------------------------------------------------------------------
%% @doc Test merging N number of sorted key/value lists with each length L
%% with erlang lists:keymerge/3.
%% @end
%%--------------------------------------------------------------------
-spec merge_keymerge_nl(N :: pos_integer(), L :: pos_integer()) ->
    {time(), {ok, kvl()}} | {error, Reason :: any()}.
merge_keymerge_nl(N, L) ->
    KVLs = get_kvls(?DEFAULT_START, ?DEFAULT_START+L, N),
    timer:tc(?MODULE, merge_keymerge_kvls, [KVLs]).

%%--------------------------------------------------------------------
%% @doc Test merging the provided sorted KVL lists using lists:keymerge/3.
%% @end
%%--------------------------------------------------------------------
-spec merge_keymerge_kvls(KVLs :: [kvl()]) -> kvl().
merge_keymerge_kvls(KVLs) ->
    merge_keymerge_kvls(KVLs, []).

%%--------------------------------------------------------------------
%% @doc Test merging the provided sorted KVL lists using lists:keymerge/3,
%% Result KVL is accumulated into given Acc.
%% @end
%%--------------------------------------------------------------------
-spec merge_keymerge_kvls(KVLs :: [kvl()], Acc :: kvl()) -> kvl().
merge_keymerge_kvls([H | Rest], [])->
    merge_keymerge_kvls(Rest, H);
merge_keymerge_kvls([H | Rest], Acc) ->
    merge_keymerge_kvls(Rest, lists:keymerge(1, H, Acc));
merge_keymerge_kvls([], Acc)->
    Acc.
