-module(rocksdb).

-export([open_db/3,
	 close_db/1,
         get/3,
         put/4,
         delete/3,
         write/4,
	 index_merge/4,
	 term_index/4,
	 add_index_ttl/2,
	 remove_index_ttl/2]).

-export([options/1,
	 readoptions/1,
         writeoptions/1]).

-export([destroy_db/2,
         repair_db/2]).

-export([approximate_sizes/2,
         approximate_size/2,
	 read_range/5,
	 read_range_n/4]).

-export([iterator/2,
	 delete_iterator/1,
         first/1,
         last/1,
	 seek/2,
	 next/1,
	 prev/1]).

-export([compact_db/1,
	 compact_index/1,
	 backup_db/2,
	 get_backup_info/1,
	 restore_db/3,
	 restore_db/4,
	 create_checkpoint/2]).

-export([resource_test/0]).

-export_type([db/0,
              it/0,
	      options/0,
              writeoptions/0,
              readoptions/0]).

-on_load(init/0).

-include("rocksdb.hrl").

-opaque db() :: binary().
-opaque it() :: binary().
-opaque options() :: binary().
-opaque writeoptions() :: binary().
-opaque readoptions() :: binary().

-type start() :: key().
-type limit() :: key().
-type range() :: {start(), limit()}.

init() ->
    Dir = "../priv",
    PrivDir =
        case code:priv_dir(rocksdb) of
            {error, _} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        filename:join([filename:dirname(Filename), Dir]);
                    _ ->
                        Dir
                end;
            Path -> Path
        end,
    Lib = filename:join(PrivDir, "rocksdb_nif"),
    erlang:load_nif(Lib, 0).

%% rocksdb operations

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Open a rocksdb database with provided Options on provided Path.
%% Returns {ok, DB} where DB is an NIF Resource, or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec open_db(Options :: options(), Path :: string(), CFOpts :: [term()]) ->
    {ok, DB :: db()} | {error, Reason :: any()}.
open_db(_options, _Path, _CFOpts)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Close a rocksdb database that is referenced by provided NIF Resource, DB.
%% @end
%%--------------------------------------------------------------------
-spec close_db(DB :: db()) ->
    ok | {error, Reason :: any()}.
close_db(_db)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Get the value of the key provided by Key from rocksdb database referenced by NIF Resource DB.
%% Operation performed using provided ReadOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec get(DB :: db(), ReadOptions :: readoptions(), Key :: key()) ->
    {ok, value()} | {error, Reason :: any()}.
get(_db, _readoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Put key/value pair provided by Key and Value to rocksdb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec put(DB :: db(), WriteOptions :: writeoptions(),
	  Key :: key(), Value :: value()) ->
    ok | {error, Reason :: any()}.
put(_db, _writeoptions, _Key, _Value)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Delete the key/value pair refered by the provided Key from rocksdb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec delete(DB :: db(), WriteOptions :: writeoptions(), Key :: key()) ->
    ok | {error, Reason :: any()}.
delete(_db, _writeoptions, _Key)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Write a batch of keys to delete and key/value pairs to put provided by DeleteKeys and PutKeyValuePairs to rocksdb database referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec write(DB :: db(), WriteOptions :: writeoptions(), DeleteKeys :: [key()], PutKeyValuePairs :: [{key(), value()}]) ->
    ok | {error, Reason :: any()}.
write(_db, _writeoptions, _Delete_Ks, _Put_KVs)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Put key/index_term pair provided to rocksdb database's index
%% column family referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec index_merge(DB :: db(), WriteOptions :: writeoptions(),
	  Key :: binary(), Value :: binary()) ->
    ok | {error, Reason :: any()}.
index_merge(_db, _writeoptions, _Key, _Value)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Put key/index_term pair provided to rocksdb database's index
%% column family referenced by NIF Resource DB.
%% Operation performed using provided WriteOptions NIF Resource.
%% @end
%%--------------------------------------------------------------------
-spec term_index(DB :: db(), WriteOptions :: writeoptions(),
	  Term :: key(), Key :: key()) ->
    ok | {error, Reason :: any()}.
term_index(_db, _writeoptions, _Term, _Key) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Report a new ttl for table id Tid to term indexer.
%% @end
%%--------------------------------------------------------------------
-spec add_index_ttl(DB :: db(), [{Tid :: integer(), Ttl :: integer()}]) ->
    ok | {error, Reason :: any()}.
add_index_ttl(_db, _add_list) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Report removal of ttl for table id Tid to term indexer.
%% @end
%%--------------------------------------------------------------------
-spec remove_index_ttl(DB :: db(), Tid :: integer()) ->
    ok | {error, Reason :: any()}.
remove_index_ttl(_db, _tid) ->
    erlang:nif_error(nif_library_not_loaded).

%% Declaring structs and constructing objects

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided RocksdbOptions.
%% This resource can be used for first arity in open_db/2 function.
%% @end
%%--------------------------------------------------------------------
-spec options(RocksdbOptions :: [{string(), string()}]) ->
    {ok, options()} | {error, Reason :: any()}.
options(_rocksdb_options_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided RocksdbReadOptions.
%% This resource can be used for second arity in get/3 function.
%% @end
%%--------------------------------------------------------------------
-spec readoptions(RocksdbReadOptions :: #rocksdb_readoptions{}) ->
    {ok, readoptions()} | {error, Reason :: any()}.
readoptions(_rocksdb_readoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Make and get a NIF resource constructed by the provided RocksdbWriteOptions.
%% This resource can be used for second arity in put/4. delete/3, write/4 functions.
%% @end
%%--------------------------------------------------------------------
-spec writeoptions(RocksdbWriteOptions :: #rocksdb_writeoptions{}) ->
    {ok, writeoptions()} | {error, Reason :: any()}.
writeoptions(_rocksdb_writeoptions_record) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Destroy the contents of the specified rocksdb database with provided Path and Options.
%% Returns ok or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec destroy_db(Path :: string(), Options :: options()) ->
    ok | {error, Reason :: any()}.
destroy_db(_Path, _Options)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Try to repair the specified rocksdb database with provided Path and Options.
%% Returns ok or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec repair_db(Path :: string(), Options :: options()) ->
    ok | {error, Reason :: any()}.
repair_db(_Name, _Options)->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Approximate the file system space used by keys in given Range for a database.
%% Returned size is compressed size and might be smaller than user data.
%% The result might not include the size of recently written data.
%% @end
%%--------------------------------------------------------------------
-spec approximate_sizes(DB :: db(), Ranges :: [range()]) ->
    {ok, [Bytes :: integer()]} | {error, Reason :: any()}.
approximate_sizes(_DB, _Ranges) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Approximate the file system space used a database.
%% Returned size is compressed size and might be smaller than user data.
%% The result might not include the size of recently written data.
%% @end
%%--------------------------------------------------------------------
-spec approximate_size(DB :: db(), ReadOptions :: readoptions()) ->
    {ok, Bytes :: integer()} | {error, Reason :: any()}.
approximate_size(_DB, _ReadOptions) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Read a set of Key/Value Pairs in between key range that is sopecified by Range.
%% Limit the number of pairs to be read by Limit.
%% @end
%%--------------------------------------------------------------------
-spec read_range(DB :: db(), Options :: options(), ReadOptions :: readoptions(),
                 Range :: range(), Limit :: pos_integer()) ->
    {ok, [{key(), value()}], Cont :: complete | key()} |
    {error, Reason :: any()}.
read_range(_DB, _Options, _ReadOptions, _Range, _Limit) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Read N number of Key/Value Pairs from start key.
%% @end
%%--------------------------------------------------------------------
-spec read_range_n(DB :: db(), ReadOptions :: readoptions(),
                 StartKey :: key(), N :: pos_integer()) ->
    {ok, [{key(), value()}]} | {error, Reason :: any()}.
read_range_n(_DB, _ReadOptions, _StartKey, _N) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Create a RocksDB Iterator and get NIF resource.
%% @end
%%--------------------------------------------------------------------
-spec iterator(DB :: db(), ReadOptions :: readoptions()) ->
    {ok, binary()} | {error, Reason :: any()}.
iterator(_DB, _ReadOptions) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Delete a RocksDB Iterator.
%% @end
%%--------------------------------------------------------------------
-spec delete_iterator(It :: it()) ->
    ok | {error, Reason :: any()}.
delete_iterator(_It) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Iterate to first entry in RocksDB database and return first
%% key/value pair.
%% @end
%%--------------------------------------------------------------------
-spec first(It :: it()) ->
    {ok, {key(), value()}} | {error, Reason :: any()}.
first(_It) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Iterate to last entry in RocksDB database and return last
%% key/value pair.
%% @end
%%--------------------------------------------------------------------
-spec last(It :: it()) ->
    {ok, {key(), value()}} | {error, Reason :: any()}.
last(_It) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Iterate to the entry specified by StartKey in RocksDB database
%% and return seeked key/value pair.
%% @end
%%--------------------------------------------------------------------
-spec seek(It :: it(), StartKey :: key()) ->
    {ok, {key(), value()}} | {error, Reason :: any()}.
seek(_DB, _StartKey) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Iterate to next entry in RocksDB database and return
%% the next key/value pair.
%% @end
%%--------------------------------------------------------------------
-spec next(It :: it()) ->
    {ok, {key(), value()}} | {error, Reason :: any()}.
next(_It) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Iterate to previous entry in RocksDB database and return
%% the previous key/value pair.
%% @end
%%--------------------------------------------------------------------
-spec prev(It :: it()) ->
    {ok, {key(), value()}} | {error, Reason :: any()}.
prev(_It) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Start manual compaction on full range of a RocksDB database.
%% @end
%%--------------------------------------------------------------------
-spec compact_db(DB :: db()) ->
    ok.
compact_db(_db) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Start manual compaction on full range of a Index
%% Columns family.
%% @end
%%--------------------------------------------------------------------
-spec compact_index(DB :: db()) ->
    ok.
compact_index(_db) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Create backup for a RocksDB database.
%% @end
%%--------------------------------------------------------------------
-spec backup_db(DB :: db(), Path :: string()) ->
    ok | {error, Reason :: any()}.
backup_db(_db, _path) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Get backup information tuples.
%% @end
%%--------------------------------------------------------------------
-spec get_backup_info(Path :: string()) ->
    {ok, [term()]} | {error, Reason :: any()}.
get_backup_info(_path) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Restore a rocksdb database from latest backup.
%% @end
%%--------------------------------------------------------------------
-spec restore_db(BkpPath :: string(),
		 DbPath :: string(),
		 WalPath :: string()) ->
    ok | {error, Reason :: any()}.
restore_db(_bkp_path, _db_path, _wal_path) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Restore a rocksdb database from backup that is specified by
%% id.
%% @end
%%--------------------------------------------------------------------
-spec restore_db(BkpPath :: string(),
		 DbPath :: string(),
		 WalPath :: string(),
		 BackupId :: pos_integer()) ->
    ok | {error, Reason :: any()}.
restore_db(_bkp_path, _db_path, _wal_path, _backup_id) ->
    erlang:nif_error(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @doc Create a persistent snapshot for a RocksDB database.
%% @end
%%--------------------------------------------------------------------
-spec create_checkpoint(DB :: db(), Path :: string()) ->
    ok | {error, Reason :: any()}.
create_checkpoint(_db, _path) ->
    erlang:nif_error(nif_library_not_loaded).

%%NIF test to allocate resources

%%--------------------------------------------------------------------
%% @doc This function is used by developers for simple functionality tests and will be removed.
%% @end
%%--------------------------------------------------------------------
-spec resource_test() -> any().
resource_test()->
    erlang:nif_error(nif_library_not_loaded).
