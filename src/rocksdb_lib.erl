%%%-------------------------------------------------------------------
%%% @author erdem aksu <erdem@sitting>
%%% @copyright (C) 2017, Mobile Arts AB
%%% @doc
%%% Rocksdb Library functions.
%%% @end
%%% Created :  10 Apr 2017 by erdem <erdem@sitting>
%%%-------------------------------------------------------------------
-module(rocksdb_lib).

%%API
-export([build_readoptions/1,
         build_writeoptions/1]).

-include("rocksdb.hrl").

%%%===================================================================
%%% API
%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_readptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_readoptions(OptionsPL :: [{atom(), term()}]) ->
    #rocksdb_readoptions{}.
build_readoptions(OptionsPL) ->
    build_readoptions(OptionsPL, #rocksdb_readoptions{}).

-spec build_readoptions(OptionsPL :: [{atom(), term()}],
			ReadOptions::#rocksdb_readoptions{}) ->
    #rocksdb_readoptions{}.
build_readoptions([], ReadOptions) ->
    ReadOptions;
build_readoptions([{verify_checksums, B} | T], O) when is_boolean(B)  ->
    build_readoptions(T, O#rocksdb_readoptions{verify_checksums = B});
build_readoptions([{fill_cache, B} | T], O) when is_boolean(B) ->
    build_readoptions(T, O#rocksdb_readoptions{fill_cache = B});
build_readoptions([{tailing, B} | T], O) when is_boolean(B)  ->
    build_readoptions(T, O#rocksdb_readoptions{tailing = B});
build_readoptions([{managed, B} | T], O) when is_boolean(B) ->
    build_readoptions(T, O#rocksdb_readoptions{managed = B});
build_readoptions([_|Rest], Options) ->
    build_readoptions(Rest, Options).

%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_writeptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_writeoptions(OptionsPL :: [{atom(), term()}]) ->
    #rocksdb_writeoptions{}.
build_writeoptions(OptionsPL) ->
    build_writeoptions(OptionsPL, #rocksdb_writeoptions{}).

-spec build_writeoptions(OptionsPL :: [{atom(), term()}],
                         RocksdbiReadOptions::#rocksdb_writeoptions{}) ->
    #rocksdb_writeoptions{}.
build_writeoptions([], WriteOptions) ->
    WriteOptions;
build_writeoptions([{sync, B} | T], O) when is_boolean(B) ->
    build_writeoptions(T, O#rocksdb_writeoptions{sync= B});
build_writeoptions([{disableWAL, B} | T], O) when is_boolean(B) ->
    build_writeoptions(T, O#rocksdb_writeoptions{disableWAL = B});
build_writeoptions([{ignore_missing_column_families, B} | T], O) when is_boolean(B) ->
    build_writeoptions(T, O#rocksdb_writeoptions{ignore_missing_column_families = B});
build_writeoptions([{no_slowdown, B} | T], O) when is_boolean(B) ->
    build_writeoptions(T, O#rocksdb_writeoptions{no_slowdown = B});
build_writeoptions([_|Rest], WriteOptions) ->
    build_writeoptions(Rest, WriteOptions).
