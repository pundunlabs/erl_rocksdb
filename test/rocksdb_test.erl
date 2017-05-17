-module(rocksdb_test).

-revision('$Revision: $ ').
-modified('$Date: $ ').

-define(DEBUG, true).

-include_lib("eunit/include/eunit.hrl").
-include("rocksdb.hrl").

%% API
-export([concurrency_basic/2]).

-export([open_close/0,
         open_close_destroy/0,
	 put_get_delete/1,
         put_n/1,
         get_key/1,
         write_n/1]).

-export([open_db/1,
         close_db/1,
         get/2,
         put/3,
         delete/2]).

-export([options/0,
         readoptions/0,
         writeoptions/0]).

-export([approximate_sizes/0,
         approximate_size/0,
	 read_range/0,
	 read_range_n/0,
         destroy_db/1,
         repair_db/1]).

-export([resource_test_n/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test creating a rocksdb options resource.
%% @end
%%--------------------------------------------------------------------
-spec options_test() -> any().
options_test()->
    ?assertMatch({ok, _Options},
		 rocksdb:options(#rocksdb_options{comparator=0,
						  create_if_missing=true})).

%%--------------------------------------------------------------------
%% @doc Test creating a rocksdb read options resource.
%% @end
%%--------------------------------------------------------------------
-spec readoptions_test() -> any().
readoptions_test()->
    ?assertMatch({ok, _ReadOptions},
		 rocksdb:readoptions(#rocksdb_readoptions{})).

%%--------------------------------------------------------------------
%% @doc Test creating a rocksdb write options resource.
%% @end
%%--------------------------------------------------------------------
-spec writeoptions_test() -> any().
writeoptions_test()->
    ?assertMatch({ok, _WriteOptions},
		 rocksdb:writeoptions(#rocksdb_writeoptions{})).

%%--------------------------------------------------------------------
%% @doc Test openining and closing a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec open_close_db_test() -> any().
open_close_db_test()->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
                                                     create_if_missing=true}),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test puting a key value entry into a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec put_test() -> any().
put_test()->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
						     create_if_missing=true}),
    {ok, WriteOptions} = rocksdb:writeoptions(#rocksdb_writeoptions{}),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    Key = term_to_binary("key"),
    Data = term_to_binary("data"),
    ?assertEqual(ok, rocksdb:put(DB, WriteOptions, Key, Data)),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test geting an entry by key from a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec get_test() -> any().
get_test()->
    ?_test(
       begin
	   {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
	                                                    create_if_missing=true}),
	   {ok, ReadOptions} = rocksdb:readoptions(#rocksdb_readoptions{}),
	   {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
	   Key = term_to_binary("key"),
	   Data = term_to_binary("data"),
	   ?assertEqual(Data, rocksdb:get(DB, ReadOptions, Key)),
	   ?assertEqual(ok, close_db(DB))
       end).

%%--------------------------------------------------------------------
%% @doc Test deleting an entry by key from a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec delete_test() -> any().
delete_test()->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
                                                     create_if_missing=true}),
    {ok, WriteOptions} = rocksdb:writeoptions(#rocksdb_writeoptions{}),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    Key = term_to_binary("key"),
    ?assertEqual(ok, rocksdb:delete(DB, WriteOptions, Key)),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test putting, getting and deleting an entry to a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec put_get_delete_test() -> any().
put_get_delete_test() ->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
                                                     create_if_missing=true}),
    {ok, ReadOptions} = rocksdb:readoptions(#rocksdb_readoptions{}),
    {ok, WriteOptions} = rocksdb:writeoptions(#rocksdb_writeoptions{}),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    Key = term_to_binary("key"),
    Data = term_to_binary("data"),
    ?assertEqual(ok, rocksdb:put(DB, WriteOptions, Key, Data)),
    Data = term_to_binary("data"),
    ?assertEqual({ok, Data}, rocksdb:get(DB, ReadOptions, Key)),
    ?assertEqual(ok, rocksdb:delete(DB, WriteOptions, Key)),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test function that opens a rocksdb database under "/tmp/basicdb" and
%% performs N number of batch writes on it.
%% @end
%%--------------------------------------------------------------------
-spec batch_test() -> ok.
batch_test()->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	fun(X, {DK,PKV})->
		BK = erlang:term_to_binary(integer_to_list(X)),
		String = lists:concat(["some example data number: ", X]),
		BV = erlang:term_to_binary(String),
		{[BK|DK],[{BK,BV}|PKV]}
	end,
    N = 100000,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,N)),
    ?assertEqual(ok, rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS)),
    [begin {ok, _Value} = get(DB, Keys) end || Keys <- DeleteKeys],
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test aproximate byte size for the data stored for a range of keys
%% from a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec approximate_sizes_test() -> any().
approximate_sizes_test() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,100000)),
    ok = rocksdb:write(DB, WriteOptions,
		               DeleteKeys, PutKVS),
    Ranges = [{erlang:term_to_binary("1000"),erlang:term_to_binary("2500")}],
    {ok, [Size]} = rocksdb:approximate_sizes(DB, Ranges),
    true = is_integer(Size),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test aproximate byte size for the data stored in a rocksdb
%% database.
%% @end
%%--------------------------------------------------------------------
-spec approximate_size_test() -> any().
approximate_size_test() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, ReadOptions} = readoptions(),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,100000)),
    ok = rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS),
    {ok, Size} = rocksdb:approximate_size(DB, ReadOptions),
    %%?debugFmt("Approximate db size: ~p Bytes", [Size]),
    true = is_integer(Size),
    ?assertEqual(ok, close_db(DB)).

%%--------------------------------------------------------------------
%% @doc Test reading a range of keys from a rocksdb database where
%% range is defined by Start and End keys.
%% @end
%%--------------------------------------------------------------------
-spec read_range_test() -> any().
read_range_test() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, ReadOptions} = readoptions(),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,100000)),
    ok = rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS),
    Range = {erlang:term_to_binary("2500"), erlang:term_to_binary("1000")},
    {ok, KVL, _} = rocksdb:read_range(DB, Options, ReadOptions, Range, 1000),
    1000 = length(KVL),

    Range2 = {erlang:term_to_binary("500"), erlang:term_to_binary("500")},
    {ok, [_KVP], complete} = rocksdb:read_range(DB, Options, ReadOptions, Range2, 1),
    Range3 = {erlang:term_to_binary("501"), erlang:term_to_binary("500")},
    {ok, [_], <<_/binary>>} = rocksdb:read_range(DB, Options, ReadOptions, Range3, 1),
    {ok, [_,_], complete} = rocksdb:read_range(DB, Options, ReadOptions, Range3, 2),

    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc Test reading a range of keys from a rocksdb database where
%% range is defined by a Start Key and number of keys to read.
%% @end
%%--------------------------------------------------------------------
-spec read_range_n_test() -> any().
read_range_n_test() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, ReadOptions} = readoptions(),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,100000)),
    ok = rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS),
    StartKey = erlang:term_to_binary("2500"),
    {ok, KVL} = rocksdb:read_range_n(DB, ReadOptions, StartKey, 1000),
    1000 = length(KVL),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc Test creating an iterator and using first, last, seek, next and
%% prev functions.
%% @end
%%--------------------------------------------------------------------
-spec iteration_test() -> any().
iteration_test() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, ReadOptions} = readoptions(),
    {ok, It} = rocksdb:iterator(DB, ReadOptions),
    {ok, {FK, _}} = rocksdb:first(It),
    %%?debugFmt("100000 =:=, ~n", []),
    ?assert(erlang:binary_to_term(FK) =:= "100000"),
    {ok, {_, _}} = rocksdb:next(It),
    {ok, {K, _}} = rocksdb:next(It),
    {ok, {_, _}} = rocksdb:next(It),
    {ok, {K, _}} = rocksdb:prev(It),
    {ok, {LK, _}} = rocksdb:last(It),
    ?assert(erlang:binary_to_term(LK) =:= "1"),
    {ok, {SK, _}} = rocksdb:prev(It),
    ?assert(erlang:binary_to_term(SK) =:= "2"),
    {ok, {_, _}} = rocksdb:prev(It),
    {ok, {_, _}} = rocksdb:seek(It, erlang:term_to_binary(integer_to_list(2000))),
    {ok, {S2001, _}} = rocksdb:prev(It),
    ?assert(erlang:binary_to_term(S2001) =:= "2001"),
    {ok, {_, _}} = rocksdb:last(It),
    {error, invalid} = rocksdb:next(It),
    ok = rocksdb:delete_iterator(It),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc Test creating an iterator and using first, last, seek, next and
%% prev functions.
%% @end
%%--------------------------------------------------------------------
-spec iterator_delete_test() -> any().
iterator_delete_test() ->
    {ok, Options} = options(),
    {ok, DB1} = rocksdb:open_db(Options, "/tmp/erl_rocksdb_test"),
    {ok, ReadOptions} = readoptions(),
    {ok, It1} = rocksdb:iterator(DB1, ReadOptions),
    ok = rocksdb:delete_iterator(It1),
    ok = close_db(DB1).

%%--------------------------------------------------------------------
%% @doc Test repairing a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec repair_db_test() -> any().
repair_db_test()->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
                                                     create_if_missing=true}),
    ?assertEqual(ok, rocksdb:repair_db("/tmp/erl_rocksdb_test", Options)).

%%--------------------------------------------------------------------
%% @doc Test destroying a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec destroy_db_test() -> any().
destroy_db_test()->
    {ok, Options} = rocksdb:options(#rocksdb_options{comparator=0,
                                                     create_if_missing=true}),
    ?assertEqual(ok, rocksdb:destroy_db("/tmp/erl_rocksdb_test", Options)).

%%--------------------------------------------------------------------
%% @doc Test Count number of concurrent writes per process by N number of processes on rocksdb.
%% @end
%%--------------------------------------------------------------------
-spec concurrency_basic(N :: pos_integer(), Count :: pos_integer()) -> {ok, L1 :: [{pid(), pos_integer()}]}.
concurrency_basic(N, Count)->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, WriteOptions} = writeoptions(),
    F1 = fun(Pid, L) ->
		 Put =
		     fun(X)->
			     Data = "some example data " ++ integer_to_list(L)
				 ++ integer_to_list(X),
			     ok = rocksdb:put(DB, WriteOptions,
					      erlang:term_to_binary(X),
					      erlang:term_to_binary(Data))
		     end,
		 lists:map(Put, lists:seq(1,Count)),
		 Pid ! {self(), Count}
	 end,
    F2 = fun(Key, Vals, Acc) -> [{Key, Vals} | Acc] end,
    List = lists:seq(1, N),
    L1 = phofs:mapreduce(F1, F2, [], List),
    ok = close_db(DB),
    {ok, L1}.

%%--------------------------------------------------------------------
%% @doc A simple test funtion that opens and closes a rocksdb database
%% under "/tmp/basicdb"
%% @end
%%--------------------------------------------------------------------
-spec open_close() -> ok.
open_close()->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc A simple test funtion that opens, closes, destroys and again
%% opens, closes a rocksdb database under "/tmp/basicdb"
%% @end
%%--------------------------------------------------------------------
-spec open_close_destroy() -> ok.
open_close_destroy()->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    ok = close_db(DB),
    ok = rocksdb:destroy_db("/tmp/basicdb", Options),
    {ok, New_DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    ok = close_db(New_DB).

%%--------------------------------------------------------------------
%% @doc Test function that opens a rocksdb database under "/tmp/basicdb" and
%% performs N number of put, get and delete operations on it.
%% @end
%%--------------------------------------------------------------------
-spec put_get_delete(N :: pos_integer()) -> ok.
put_get_delete(N)->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    PGD =
	fun(X)->
		ok = put(DB, erlang:term_to_binary(X),
			 erlang:term_to_binary("some example data")),
		{ok, _Value} = get(DB, erlang:term_to_binary(X)),
		ok = delete(DB, erlang:term_to_binary(X))
	end,
    lists:map(PGD, lists:seq(1,N)),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc Test function that opens a rocksdb database under "/tmp/basicdb" and
%% performs N number of put operations on it.
%% @end
%%--------------------------------------------------------------------
-spec put_n(N :: pos_integer()) -> ok.
put_n(N)->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, WriteOptions} = writeoptions(),
    Put =
	fun(X)->
		ok = rocksdb:put(DB, WriteOptions,
				 erlang:term_to_binary(X),
				 erlang:term_to_binary("some example data " ++ integer_to_list(X))
				)
	end,
    lists:map(Put, lists:seq(1,N)),
    erlang:garbage_collect(self()),
    erlang:garbage_collect(),
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc Test function that opens a rocksdb database under "/tmp/basicdb" and
%% performs get operation on it using provided Key.
%% @end
%%--------------------------------------------------------------------
-spec get_key(Key :: term()) -> term().
get_key(Key)->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, ReadOptions} = readoptions(),
    Result = rocksdb:get(DB, ReadOptions,
			erlang:term_to_binary(Key)),
    ok = close_db(DB),
    Result.

%%--------------------------------------------------------------------
%% @doc Test function that opens a rocksdb database under "/tmp/basicdb" and
%% performs N number of batch writes on it.
%% @end
%%--------------------------------------------------------------------
-spec write_n(N :: pos_integer()) -> ok.
write_n(N)->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, WriteOptions} = writeoptions(),
    Batch =
	fun(X, {DK,PKV})->
		BK = erlang:term_to_binary(integer_to_list(X)),
		String = lists:concat(["some example data number: ", X]),
		BV = erlang:term_to_binary(String),
		{[BK|DK],[{BK,BV}|PKV]}
	end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,N)),
    ok = rocksdb:write(DB, WriteOptions,
		       DeleteKeys, PutKVS),
    [begin {ok, _Value} = get(DB, Keys) end || Keys <- DeleteKeys],
    ok = close_db(DB).

%%--------------------------------------------------------------------
%% @doc  Test function that opens a rocksdb database under "/tmp/basicdb" and
%% returns the DB reference that corresponds to a NIF resource object.
%% @end
%%--------------------------------------------------------------------
-spec open_db(Path :: string()) -> {ok, DB :: binary()} | {error, Reason :: any()}.
open_db(Path)->
    {ok, Options} = options(),
    case rocksdb:open_db(Options, Path) of
	{error, Reason} ->
	    {error, Reason};
	{ok, DB} ->
	    {ok, DB}
    end.
%%--------------------------------------------------------------------
%% @doc Test function that closes a rocksdb database referenced by the resource
%% that is returned by open_db/1
%% @end
%%--------------------------------------------------------------------
-spec close_db(DB :: binary())-> ok.
close_db(DB)->
    ok = rocksdb:close_db(DB).

%%--------------------------------------------------------------------
%% @doc Basic test that performs get operation on a rocksdb database
%% referenced by a DB resource object using provided Key.
%% @end
%%--------------------------------------------------------------------
-spec get(DB :: binary(), Key :: term()) -> {ok, Value :: term()} | {error, Reason :: any()}.
get(DB, Key)->
    {ok, ReadOptions} = readoptions(),
    {ok, _Value} = rocksdb:get(DB, ReadOptions, Key).

%%--------------------------------------------------------------------
%% @doc Basic test that performs put operation on a rocksdb database
%% referenced by a DB resource object using provided Key and Value.
%% @end
%%--------------------------------------------------------------------
-spec put(DB :: binary(), Key :: term(), Value :: term()) -> ok | {error, Reason :: any()}.
put(DB, Key, Value)->
    {ok, WriteOptions} = writeoptions(),
    ok = rocksdb:put(DB, WriteOptions, Key, Value).

%%--------------------------------------------------------------------
%% @doc Basic test that performs delete operation on a rocksdb database
%% referenced by a DB resource object using provided Key.
%% @end
%%--------------------------------------------------------------------
-spec delete(DB :: binary(), Key :: term()) -> ok | {error, Reason :: any()}.
delete(DB, Key)->
    {ok, WriteOptions} = writeoptions(),
    ok = rocksdb:delete(DB, WriteOptions, Key).

%%--------------------------------------------------------------------
%% @doc Simple test function that gets and returns a rocksdb options resource
%% object with default values except create_if_missing=true.
%% @end
%%--------------------------------------------------------------------
-spec options() -> {ok, binary()} | {error, Reason :: any()}.
options()->
    rocksdb:options(#rocksdb_options{comparator=0,
				     create_if_missing=true}).

%%--------------------------------------------------------------------
%% @doc Simple test function that gets and returns a rocksdb readoptions
%% resource object with default values.
%% @end
%%--------------------------------------------------------------------
-spec readoptions() -> {ok, binary()} | {error, Reason :: any()}.
readoptions()->
    rocksdb:readoptions(#rocksdb_readoptions{}).

%%--------------------------------------------------------------------
%% @doc Simple test function that gets and returns a rocksdb writeoptions
%% resource object with default values.
%% @end
%%--------------------------------------------------------------------
-spec writeoptions() -> {ok, binary()} | {error, Reason :: any()}.
writeoptions()->
    rocksdb:writeoptions(#rocksdb_writeoptions{}).


%%--------------------------------------------------------------------
%% @doc  Test function that detroys the contents of rocksdb database
%% that is specified by Path and returns ok.
%% @end
%%--------------------------------------------------------------------
-spec destroy_db(Path :: string()) -> ok | {error, Reason :: any()}.
destroy_db(Path)->
    {ok, Options} = options(),
    case rocksdb:destroy_db(Path, Options) of
	    {error, Reason} ->
	        {error, Reason};
	    ok ->
	        ok
    end.

%%--------------------------------------------------------------------
%% @doc  Test function that tries to repair a rocksdb database that
%% is specified by Path and returns ok.
%% @end
%%--------------------------------------------------------------------
-spec repair_db(Path :: string()) -> ok | {error, Reason :: any()}.
repair_db(Path)->
    {ok, Options} = options(),
    case rocksdb:repair_db(Path, Options) of
	    {error, Reason} ->
	        {error, Reason};
	    ok ->
	        ok
    end.

%%--------------------------------------------------------------------
%% @doc Test aproximate byte size for the data stored for a range of keys
%% from a rocksdb database.
%% @end
%%--------------------------------------------------------------------
-spec approximate_sizes() -> any().
approximate_sizes() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    Ranges = [{erlang:term_to_binary("1000"),erlang:term_to_binary("2500")},
              {erlang:term_to_binary("18000"),erlang:term_to_binary("18500")}],
    {ok, Sizes} = rocksdb:approximate_sizes(DB, Ranges),
    ok = close_db(DB),
    {ok, Sizes}.

%%--------------------------------------------------------------------
%% @doc Test aproximate byte size for the data stored in a rocksdb
%% database.
%% @end
%%--------------------------------------------------------------------
-spec approximate_size() -> any().
approximate_size() ->
    {ok, Options} = options(),
    {ok, ReadOptions} = readoptions(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, Size} = rocksdb:approximate_sizes(DB, ReadOptions),
    ok = close_db(DB),
    {ok, Size}.

%%--------------------------------------------------------------------
%% @doc Test reading a range of keys from a rocksdb database where
%% range is defined by a start and end key.
%% @end
%%--------------------------------------------------------------------
-spec read_range() -> any().
read_range() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, ReadOptions} = readoptions(),
    {ok, WriteOptions} = writeoptions(),
    Limit = 1000000,
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,1500000)),
    ok = rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS),
    Range = {erlang:term_to_binary("1000"),erlang:term_to_binary("1200000")},
    {ok, KVL, _} = rocksdb:read_range(DB, Options, ReadOptions, Range, Limit),

    ok = close_db(DB),
    {ok, [{binary_to_term(K), binary_to_term(V)} || {K, V} <- KVL]}.

%%--------------------------------------------------------------------
%% @doc Test reading a range of keys from a rocksdb database where
%% range is defined by a start key and number of keys to read.
%% @end
%%--------------------------------------------------------------------
-spec read_range_n() -> any().
read_range_n() ->
    {ok, Options} = options(),
    {ok, DB} = rocksdb:open_db(Options, "/tmp/basicdb"),
    {ok, ReadOptions} = readoptions(),
    {ok, WriteOptions} = writeoptions(),
    Limit = 1000000,
    Batch =
	    fun(X, {DK,PKV})->
		    BK = erlang:term_to_binary(integer_to_list(X)),
		    String = lists:concat(["some example data number: ", X]),
		    BV = erlang:term_to_binary(String),
		    {[BK|DK],[{BK,BV}|PKV]}
	    end,
    {DeleteKeys, PutKVS} = lists:foldr(Batch, {[],[]}, lists:seq(1,1500000)),
    ok = rocksdb:write(DB, WriteOptions, DeleteKeys, PutKVS),
    StartKey = erlang:term_to_binary("1400000"),
    {ok, KVL} = rocksdb:read_range_n(DB, Options, ReadOptions, StartKey, Limit),

    ok = close_db(DB),
    {ok, [{binary_to_term(K), binary_to_term(V)} || {K, V} <- KVL]}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc This function is obsolete and has no consistent behavior.
%% @end
%%--------------------------------------------------------------------
resource_test_n(N)->
    Put =
	fun(_)->
		rocksdb:resource_test()
	end,
    lists:map(Put, lists:seq(1,N)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
