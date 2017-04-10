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
-export([build_rocksdb_options/1,
         build_rocksdb_readoptions/1,
         build_rocksdb_writeoptions/1]).

-include("rocksdb.hrl").

%%%===================================================================
%%% API
%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_options{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_rocksdb_options(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_rocksdb_options(OptionsPL) ->
    build_rocksdb_options(OptionsPL, #rocksdb_options{}).

-spec build_rocksdb_options(OptionsPL::[{atom(), term()}],
                            RocksdbOptions::#rocksdb_options{}) -> ok | {error, Reason::term()}.
build_rocksdb_options([], RocksdbOptions) ->
    RocksdbOptions;
build_rocksdb_options([{comparator, ascending}|Rest], RocksdbOptions) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{comparator = 1});
build_rocksdb_options([{comparator, descending}|Rest], RocksdbOptions) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{comparator = 0});
build_rocksdb_options([{create_if_missing, Bool}|Rest], RocksdbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{create_if_missing = Bool});
build_rocksdb_options([{error_if_exists, Bool}|Rest], RocksdbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{error_if_exists = Bool});
build_rocksdb_options([{paranoid_checks, Bool}|Rest], RocksdbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{paranoid_checks = Bool});
build_rocksdb_options([{write_buffer_size, Int}|Rest], RocksdbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{write_buffer_size = Int});
build_rocksdb_options([{max_open_files, Int}|Rest], RocksdbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{max_open_files = Int});
build_rocksdb_options([{block_size, Int}|Rest], RocksdbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{block_size = Int});
build_rocksdb_options([{block_restart_interval, Int}|Rest], RocksdbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, RocksdbOptions#rocksdb_options{block_restart_interval = Int});
build_rocksdb_options([E|Rest], RocksdbOptions) ->
    error_logger:info_msg("Unsupported rocksdb options parameter: ~p", [E]),
    build_rocksdb_options(Rest, RocksdbOptions).


%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_readptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_rocksdb_readoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_rocksdb_readoptions(OptionsPL) ->
    build_rocksdb_readoptions(OptionsPL, #rocksdb_readoptions{}).

-spec build_rocksdb_readoptions(OptionsPL::[{atom(), term()}],
                            RocksdbiReadOptions::#rocksdb_readoptions{}) -> ok | {error, Reason::term()}.
build_rocksdb_readoptions([], RocksdbReadOptions) ->
    RocksdbReadOptions;
build_rocksdb_readoptions([{verify_checksums, Bool}|Rest],
                          RocksdbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_rocksdb_readoptions(Rest,
                              RocksdbReadOptions#rocksdb_readoptions{verify_checksums = Bool});
build_rocksdb_readoptions([{fill_cache, Bool}|Rest],
                          RocksdbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_rocksdb_readoptions(Rest,
                              RocksdbReadOptions#rocksdb_readoptions{fill_cache = Bool});
build_rocksdb_readoptions([_|Rest], RocksdbReadOptions) -> 
    build_rocksdb_readoptions(Rest, RocksdbReadOptions).

%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_writeptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_rocksdb_writeoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_rocksdb_writeoptions(OptionsPL) ->
    build_rocksdb_writeoptions(OptionsPL, #rocksdb_writeoptions{}).

-spec build_rocksdb_writeoptions(OptionsPL::[{atom(), term()}],
                                 RocksdbiReadOptions::#rocksdb_writeoptions{}) -> ok | {error, Reason::term()}.
build_rocksdb_writeoptions([], RocksdbWriteOptions) ->
    RocksdbWriteOptions;
build_rocksdb_writeoptions([{sync, Bool}|Rest],
                           RocksdbWriteOptions) when Bool == true;
                                                     Bool == false ->
    build_rocksdb_writeoptions(Rest,
                               RocksdbWriteOptions#rocksdb_writeoptions{sync = Bool});
build_rocksdb_writeoptions([_|Rest], RocksdbWriteOptions) -> 
    build_rocksdb_writeoptions(Rest, RocksdbWriteOptions).
