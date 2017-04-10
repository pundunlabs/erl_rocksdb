%%%-------------------------------------------------------------------
%%% @author erdem aksu <erdem@sitting>
%%% @copyright (C) 2017, Mobile Arts AB
%%% @doc
%%% Leveldb Library functions.
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
                            LeveldbOptions::#rocksdb_options{}) -> ok | {error, Reason::term()}.
build_rocksdb_options([], LeveldbOptions) ->
    LeveldbOptions;
build_rocksdb_options([{comparator, ascending}|Rest], LeveldbOptions) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{comparator = 1});
build_rocksdb_options([{comparator, descending}|Rest], LeveldbOptions) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{comparator = 0});
build_rocksdb_options([{create_if_missing, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{create_if_missing = Bool});
build_rocksdb_options([{error_if_exists, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{error_if_exists = Bool});
build_rocksdb_options([{paranoid_checks, Bool}|Rest], LeveldbOptions)
    when Bool == false; Bool == true ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{paranoid_checks = Bool});
build_rocksdb_options([{write_buffer_size, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{write_buffer_size = Int});
build_rocksdb_options([{max_open_files, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{max_open_files = Int});
build_rocksdb_options([{block_size, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{block_size = Int});
build_rocksdb_options([{block_restart_interval, Int}|Rest], LeveldbOptions)
    when is_integer(Int) ->
    build_rocksdb_options(Rest, LeveldbOptions#rocksdb_options{block_restart_interval = Int});
build_rocksdb_options([E|Rest], LeveldbOptions) ->
    error_logger:info_msg("Unsupported rocksdb options parameter: ~p", [E]),
    build_rocksdb_options(Rest, LeveldbOptions).


%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_readptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_rocksdb_readoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_rocksdb_readoptions(OptionsPL) ->
    build_rocksdb_readoptions(OptionsPL, #rocksdb_readoptions{}).

-spec build_rocksdb_readoptions(OptionsPL::[{atom(), term()}],
                            LeveldbiReadOptions::#rocksdb_readoptions{}) -> ok | {error, Reason::term()}.
build_rocksdb_readoptions([], LeveldbReadOptions) ->
    LeveldbReadOptions;
build_rocksdb_readoptions([{verify_checksums, Bool}|Rest],
                          LeveldbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_rocksdb_readoptions(Rest,
                              LeveldbReadOptions#rocksdb_readoptions{verify_checksums = Bool});
build_rocksdb_readoptions([{fill_cache, Bool}|Rest],
                          LeveldbReadOptions) when Bool == true;
                                                   Bool == false ->
    build_rocksdb_readoptions(Rest,
                              LeveldbReadOptions#rocksdb_readoptions{fill_cache = Bool});
build_rocksdb_readoptions([_|Rest], LeveldbReadOptions) -> 
    build_rocksdb_readoptions(Rest, LeveldbReadOptions).

%%--------------------------------------------------------------------
%% @doc
%% Build a #rocksdb_writeptions{} record with the provided proplist OptionsPL
%% @end
%%--------------------------------------------------------------------
-spec build_rocksdb_writeoptions(OptionsPL::[{atom(), term()}]) -> ok | {error, Reason::term()}.
build_rocksdb_writeoptions(OptionsPL) ->
    build_rocksdb_writeoptions(OptionsPL, #rocksdb_writeoptions{}).

-spec build_rocksdb_writeoptions(OptionsPL::[{atom(), term()}],
                                 LeveldbiReadOptions::#rocksdb_writeoptions{}) -> ok | {error, Reason::term()}.
build_rocksdb_writeoptions([], LeveldbWriteOptions) ->
    LeveldbWriteOptions;
build_rocksdb_writeoptions([{sync, Bool}|Rest],
                           LeveldbWriteOptions) when Bool == true;
                                                     Bool == false ->
    build_rocksdb_writeoptions(Rest,
                               LeveldbWriteOptions#rocksdb_writeoptions{sync = Bool});
build_rocksdb_writeoptions([_|Rest], LeveldbWriteOptions) -> 
    build_rocksdb_writeoptions(Rest, LeveldbWriteOptions).
