%% rocksdb related macros and records.

-type value() :: binary() | string().
-type key() :: binary() | string().
-type kvp() :: {key(), value()}.
-type kvl() :: [kvp()].
-type time() :: integer().

-record(rocksdb_options,{
    %% -------------------
    %% Parameters that affect behavior

    %% Comparator used to define the order of keys in the table.
    %% Default: a comparator that uses lexicographic byte-wise ordering
    %%
    %% REQUIRES: The client must ensure that the comparator supplied
    %% here has the same name and orders keys *exactly* the same as the
    %% comparator provided to previous open calls on the same DB.
    comparator = 0 :: integer(),%% 0 | 1
				%% 0 -> Default Comparator
				%% 1 -> Reverse Lexicographical Comparator

    %% If true, the database will be created if it is missing.
    %% Default: false
    create_if_missing = false :: boolean(), %% true | false

    %% If true, an error is raised if the database already exists.
    %% Default: false
    error_if_exists = false :: boolean(), %% true | false

    %% If true, the implementation will do aggressive checking of the
    %% data it is processing and will stop early if it detects any
    %% errors.  This may have unforeseen ramifications: for example, a
    %% corruption of one DB entry may cause a large number of entries to
    %% become unreadable or for the entire DB to become unopenable.
    %% Default: false
    paranoid_checks = false :: boolean(), % true | false

    %% Use the specified object to interact with the environment,
    %% e.g. to read/write files, schedule background work, etc.
    %% Default: Env::Default()
    env :: binary(), %% ERL NIF TERM (Resource)

    %% Use to control write rate of flush and compaction. Flush has higher
    %% priority than compaction. Rate limiting is disabled if nullptr.
    %% If rate limiter is enabled, bytes_per_sync is set to 1MB by default.
    %% Default: nullptr
    rate_limiter :: binary(),

    %% Use to track SST files and control their file deletion rate.
    %%
    %% Features:
    %%  - Throttle the deletion rate of the SST files.
    %%  - Keep track the total size of all SST files.
    %%  - Set a maximum allowed space limit for SST files that when reached
    %%    the DB wont do any further flushes or compactions and will set the
    %%    background error.
    %%  - Can be shared between multiple dbs.
    %% Limitations:
    %%  - Only track and throttle deletes of SST files in
    %%    first db_path (db_name if db_paths is empty).
    %%
    %% Default: nullptr
    sst_file_manager :: binary(),

    %% Any internal progress/error information generated by the db will
    %% be written to info_log if it is non-nullptr, or to a file stored
    %% in the same directory as the DB contents if info_log is nullptr.
    %% Default: nullptr
    info_log :: binary(),

    %% Number of open files that can be used by the DB.  You may need to
    %% increase this if your database has a large working set. Value -1 means
    %% files opened are always kept open. You can estimate number of files based
    %% on target_file_size_base and target_file_size_multiplier for level-based
    %% compaction. For universal-style compaction, you can usually set it to -1.
    %% Default: -1
    max_open_files = -1 :: integer(),

    %% If max_open_files is -1, DB will open all files on DB::Open(). You can
    %% use this option to increase the number of threads used to open the files.
    %% Default: 16
    max_file_opening_threads = 16 :: integer(),

    %% Once write-ahead logs exceed this size, we will start forcing the flush of
    %% column families whose memtables are backed by the oldest live WAL file
    %% (i.e. the ones that are causing all the space amplification). If set to 0
    %% (default), we will dynamically choose the WAL size limit to be
    %% [sum of all write_buffer_size * max_write_buffer_number] * 4
    %% Default: 0
    max_total_wal_size = 0 :: integer(),

    %% If non-null, then we should collect metrics about database operations
    statistics :: binary(),

    %% If true, then every store to stable storage will issue a fsync.
    %% If false, then every store to stable storage will issue a fdatasync.
    %% This parameter should be set to true while storing data to
    %% filesystem like ext3 that can lose files after a reboot.
    %% Default: false
    use_fsync = false :: boolean(),

    %% A list of paths where SST files can be put into, with its target size.
    %% Newer data is placed into paths specified earlier in the vector while
    %% older data gradually moves to paths specified later in the vector.
    %%
    %% For example, you have a flash device with 10GB allocated for the DB,
    %% as well as a hard drive of 2TB, you should config it to be:
    %%   [{"/flash_path", 10GB}, {"/hard_drive", 2TB}]
    %%
    %% The system will try to guarantee data under each path is close to but
    %% not larger than the target size. But current and future file sizes used
    %% by determining where to place a file are based on best-effort estimation,
    %% which means there is a chance that the actual size under the directory
    %% is slightly more than target size under some workloads. User should give
    %% some buffer room for those cases.
    %%
    %% If none of the paths has sufficient room to place a file, the file will
    %% be placed to the last path anyway, despite to the target size.
    %%
    %% Placing newer data to earlier paths is also best-efforts. User should
    %% expect user files to be placed in higher levels in some extreme cases.
    %%
    %% If left empty, only one path will be used, which is db_name passed when
    %% opening the DB.
    %% Default: empty
    db_paths :: binary(),

    %% This specifies the info LOG dir.
    %% If it is empty, the log files will be in the same dir as data.
    %% If it is non empty, the log files will be in the specified dir,
    %% and the db data dir's absolute path will be used as the log file
    %% name's prefix.
    db_log_dir = "" :: string(),

    %% This specifies the absolute dir path for write-ahead logs (WAL).
    %% If it is empty, the log files will be in the same dir as data,
    %%   dbname is used as the data dir by default
    %% If it is non empty, the log files will be in kept the specified dir.
    %% When destroying the db,
    %%   all log files in wal_dir and the dir itself is deleted
    wal_dir = "" :: string(),

    %% The periodicity when obsolete files get deleted. The default
    %% value is 6 hours. The files that get out of scope by compaction
    %% process will still get automatically delete on every compaction,
    %% regardless of this setting
    delete_obsolete_files_period_micros :: integer(),

    %% Suggested number of concurrent background compaction jobs, submitted to
    %% the default LOW priority thread pool.
    %%
    %% Default: 1
    base_background_compactions = 1 :: integer(),

    %% Maximum number of concurrent background compaction jobs, submitted to
    %% the default LOW priority thread pool.
    %% We first try to schedule compactions based on
    %% `base_background_compactions`. If the compaction cannot catch up , we
    %% will increase number of compaction threads up to
    %% `max_background_compactions`.
    %%
    %% If you're increasing this, also consider increasing number of threads in
    %% LOW priority thread pool. For more information, see
    %% Env::SetBackgroundThreads
    %% Default: 1
    max_background_compactions = 1 :: integer(),

    %% This value represents the maximum number of threads that will
    %% concurrently perform a compaction job by breaking it into multiple,
    %% smaller ones that are run simultaneously.
    %% Default: 1 (i.e. no subcompactions)
    max_subcompactions = 1 :: integer(),

    %% Maximum number of concurrent background memtable flush jobs, submitted to
    %% the HIGH priority thread pool.
    %%
    %% By default, all background jobs (major compaction and memtable flush) go
    %% to the LOW priority pool. If this option is set to a positive number,
    %% memtable flush jobs will be submitted to the HIGH priority pool.
    %% It is important when the same Env is shared by multiple db instances.
    %% Without a separate pool, long running major compaction jobs could
    %% potentially block memtable flush jobs of other db instances, leading to
    %% unnecessary Put stalls.
    %%
    %% If you're increasing this, also consider increasing number of threads in
    %% HIGH priority thread pool. For more information, see
    %% Env::SetBackgroundThreads
    %% Default: 1
    max_background_flushes = 1 :: integer(),

    %% Specify the maximal size of the info log file. If the log file
    %% is larger than `max_log_file_size`, a new info log file will
    %% be created.
    %% If max_log_file_size == 0, all logs will be written to one
    %% log file.
    max_log_file_size = 0 :: integer(),

    %% Time for the info log file to roll (in seconds).
    %% If specified with non-zero value, log file will be rolled
    %% if it has been active longer than `log_file_time_to_roll`.
    %% Default: 0 (disabled)
    %% Not supported in ROCKSDB_LITE mode!
    log_file_time_to_roll = 0 :: integer(),

    %% Maximal info log files to be kept.
    %% Default: 1000
    keep_log_file_num = 1000 :: integer(),

    %% Recycle log files.
    %% If non-zero, we will reuse previously written log files for new
    %% logs, overwriting the old data.  The value indicates how many
    %% such files we will keep around at any point in time for later
    %% use.  This is more efficient because the blocks are already
    %% allocated and fdatasync does not need to update the inode after
    %% each write.
    %% Default: 0
    recycle_log_file_num = 0 :: integer(),

    %% manifest file is rolled over on reaching this limit.
    %% The older manifest file be deleted.
    %% The default value is MAX_INT so that roll-over does not take place.
    max_manifest_file_size :: integer(),

    %% Number of shards used for table cache.
    table_cache_numshardbits = 6 :: integer(),

    %% Number of bytes to preallocate (via fallocate) the manifest
    %% files.  Default is 4mb, which is reasonable to reduce random IO
    %% as well as prevent overallocation for mounts that preallocate
    %% large amounts of data (such as xfs's allocsize option).
    manifest_preallocation_size = 4 * 1024 * 1024 :: integer(),

    %% Allow the OS to mmap file for reading sst tables. Default: false
    allow_mmap_reads = false :: boolean(),

    %% Allow the OS to mmap file for writing.
    %% DB::SyncWAL() only works if this is set to false.
    %% Default: false
    allow_mmap_writes = false :: boolean(),

    %% Enable direct I/O mode for read/write
    %% they may or may not improve performance depending on the use case
    %%
    %% Files will be opened in "direct I/O" mode
    %% which means that data r/w from the disk will not be cached or
    %% bufferized. The hardware buffer of the devices may however still
    %% be used. Memory mapped files are not impacted by these parameters.

    %% Use O_DIRECT for reading file
    %% Default: false
    %% Not supported in ROCKSDB_LITE mode!
    use_direct_reads = false :: boolean(),

    %% Use O_DIRECT for writing file
    %% Default: false
    %% Not supported in ROCKSDB_LITE mode!
    use_direct_writes = false :: boolean(),

    %% If false, fallocate() calls are bypassed
    allow_fallocate = true :: boolean(),

    %% Disable child process inherit open files. Default: true
    is_fd_close_on_exec = true :: boolean(),

    %% NOT SUPPORTED ANYMORE -- this options is no longer used
    skip_log_error_on_recovery = false :: boolean(),

    %% if not zero, dump rocksdb.stats to LOG every stats_dump_period_sec
    %% Default: 600 (10 min)
    stats_dump_period_sec = 600 :: integer(),

    %% If set true, will hint the underlying file system that the file
    %% access pattern is random, when a sst file is opened.
    %% Default: true
    advise_random_on_open = true :: boolean(),

    %% Amount of data to build up in memtables across all column
    %% families before writing to disk.
    %%
    %% This is distinct from write_buffer_size, which enforces a limit
    %% for a single memtable.
    %%
    %% This feature is disabled by default. Specify a non-zero value
    %% to enable it.
    %%
    %% Default: 0 (disabled)
    db_write_buffer_size = 0 :: integer(),

    %% If true, always create a new file descriptor and new table reader
    %% for compaction inputs. Turn this parameter on may introduce extra
    %% memory usage in the table reader, if it allocates extra memory
    %% for indexes. This will allow file descriptor prefetch options
    %% to be set for compaction input files and not to impact file
    %% descriptors for the same file used by user queries.
    %% Suggest to enable BlockBasedTableOptions.cache_index_and_filter_blocks
    %% for this mode if using block-based table.
    %%
    %% Default: false
    new_table_reader_for_compaction_inputs = false :: boolean(),

    %% If non-zero, we perform bigger reads when doing compaction. If you're
    %% running RocksDB on spinning disks, you should set this to at least 2MB.
    %% That way RocksDB's compaction is doing sequential instead of random reads.
    %%
    %% When non-zero, we also force new_table_reader_for_compaction_inputs to
    %% true.
    %%
    %% Default: 0
    compaction_readahead_size = 0 :: integer(),

    %% This is a maximum buffer size that is used by WinMmapReadableFile in
    %% unbuffered disk I/O mode. We need to maintain an aligned buffer for
    %% reads. We allow the buffer to grow until the specified value and then
    %% for bigger requests allocate one shot buffers. In unbuffered mode we
    %% always bypass read-ahead buffer at ReadaheadRandomAccessFile
    %% When read-ahead is required we then make use of compaction_readahead_size
    %% value and always try to read ahead. With read-ahead we always
    %% pre-allocate buffer to the size instead of growing it up to a limit.
    %%
    %% This option is currently honored only on Windows
    %%
    %% Default: 1 Mb
    random_access_max_buffer_size = 1024 * 1024 :: integer(),

    %% This is the maximum buffer size that is used by WritableFileWriter.
    %% On Windows, we need to maintain an aligned buffer for writes.
    %% We allow the buffer to grow until it's size hits the limit.
    %%
    %% Default: 1024 * 1024 (1 MB)
    writable_file_max_buffer_size = 1024 * 1024 :: integer(),


    %% Use adaptive mutex, which spins in the user space before resorting
    %% to kernel. This could reduce context switch when the mutex is not
    %% heavily contended. However, if the mutex is hot, we could end up
    %% wasting spin time.
    %% Default: false
    use_adaptive_mutex = false :: boolean(),

    %% Allows OS to incrementally sync files to disk while they are being
    %% written, asynchronously, in the background. This operation can be used
    %% to smooth out write I/Os over time. Users shouldn't rely on it for
    %% persistency guarantee.
    %% Issue one request for every bytes_per_sync written. 0 turns it off.
    %% Default: 0
    %%
    %% You may consider using rate_limiter to regulate write rate to device.
    %% When rate limiter is enabled, it automatically enables bytes_per_sync
    %% to 1MB.
    %%
    %% This option applies to table files
    bytes_per_sync = 0 :: integer(),

    %% Same as bytes_per_sync, but applies to WAL files
    %% Default: 0, turned off
    wal_bytes_per_sync = 0 :: integer(),

    %% A vector of EventListeners which call-back functions will be called
    %% when specific RocksDB event happens.
    listeners :: binary(),

    %% If true, then the status of the threads involved in this DB will
    %% be tracked and available via GetThreadList() API.
    %%
    %% Default: false
    enable_thread_tracking = false :: boolean(),

    %% The limited write rate to DB if soft_pending_compaction_bytes_limit or
    %% level0_slowdown_writes_trigger is triggered, or we are writing to the
    %% last mem table allowed and we allow more than 3 mem tables. It is
    %% calculated using size of user write requests before compression.
    %% RocksDB may decide to slow down more if the compaction still
    %% gets behind further.
    %% Unit: byte per second.
    %%
    %% Default: 16MB/s
    delayed_write_rate :: integer(),

    %% If true, allow multi-writers to update mem tables in parallel.
    %% Only some memtable_factory-s support concurrent writes; currently it
    %% is implemented only for SkipListFactory.  Concurrent memtable writes
    %% are not compatible with inplace_update_support or filter_deletes.
    %% It is strongly recommended to set enable_write_thread_adaptive_yield
    %% if you are going to use this feature.
    %%
    %% Default: true
    allow_concurrent_memtable_write = true :: boolean(),

    %% If true, threads synchronizing with the write batch group leader will
    %% wait for up to write_thread_max_yield_usec before blocking on a mutex.
    %% This can substantially improve throughput for concurrent workloads,
    %% regardless of whether allow_concurrent_memtable_write is enabled.
    %%
    %% Default: true
    enable_write_thread_adaptive_yield = true :: boolean(),

    %% The maximum number of microseconds that a write operation will use
    %% a yielding spin loop to coordinate with other write threads before
    %% blocking on a mutex.  (Assuming write_thread_slow_yield_usec is
    %% set properly) increasing this value is likely to increase RocksDB
    %% throughput at the expense of increased CPU usage.
    %%
    %% Default: 100
    write_thread_max_yield_usec = 100 :: integer(),

    %% The latency in microseconds after which a std::this_thread::yield
    %% call (sched_yield on Linux) is considered to be a signal that
    %% other processes or threads would like to use the current core.
    %% Increasing this makes writer threads more likely to take CPU
    %% by spinning, which will show up as an increase in the number of
    %% involuntary context switches.
    %%
    %% Default: 3
    write_thread_slow_yield_usec = 3 :: integer(),

    %% If true, then DB::Open() will not update the statistics used to optimize
    %% compaction decision by loading table properties from many files.
    %% Turning off this feature will improve DBOpen time especially in
    %% disk environment.
    %%
    %% Default: false
    skip_stats_update_on_db_open = false :: boolean(),

    %% Recovery mode to control the consistency while replaying WAL
    %% Default: kPointInTimeRecovery
    wal_recovery_mode :: binary(),

    %% if set to false then recovery will fail when a prepared
    %% transaction is encountered in the WAL
    allow_2pc = false :: boolean(),

    %% If true, then DB::Open / CreateColumnFamily / DropColumnFamily
    %% / SetOptions will fail if options file is not detected or properly
    %% persisted.
    %%
    %% DEFAULT: false
    fail_if_options_file_error = false :: boolean(),

    %% If true, then print malloc stats together with rocksdb.stats
    %% when printing to LOG.
    %% DEFAULT: false
    dump_malloc_stats = false :: boolean(),

    %% By default RocksDB replay WAL logs and flush them on DB open, which may
    %% create very small SST files. If this option is enabled, RocksDB will try
    %% to avoid (but not guarantee not to) flush during recovery. Also, existing
    %% WAL logs will be kept, so that if crash happened before flush, we still
    %% have logs to recover from.
    %%
    %% DEFAULT: false
    avoid_flush_during_recovery = false :: boolean(),

    %% By default RocksDB will flush all memtables on DB close if there are
    %% unpersisted data (i.e. with WAL disabled) The flush can be skip to speedup
    %% DB close. Unpersisted data WILL BE LOST.
    %%
    %% DEFAULT: false
    %%
    %% Dynamically changeable through SetDBOptions() API.
    avoid_flush_during_shutdown = false :: boolean()
}).

%% Options that control read operations
-record(rocksdb_readoptions, {
    %% If true, all data read from underlying storage will be
    %% verified against corresponding checksums.
    %% Default: true
    verify_checksums = true :: boolean(), %% true | false

    %% Should the "data block"/"index block"/"filter block" read for this
    %% iteration be cached in memory?
    %% Callers may wish to set this field to false for bulk scans.
    %% Default: true
    fill_cache = true :: boolean(), %% true | false

    %% If this option is set and memtable implementation allows, Seek
    %% might only return keys with the same prefix as the seek-key
    %%
    %% ! NOT SUPPORTED ANYMORE: prefix_seek is on by default when prefix_extractor
    %% is configured
    %% bool prefix_seek;

    %% If "snapshot" is non-nullptr, read as of the supplied snapshot
    %% (which must belong to the DB that is being read and which must
    %% not have been released).  If "snapshot" is nullptr, use an implicit
    %% snapshot of the state at the beginning of this read operation.
    %% Default: nullptr
    snapshot :: binary(), %% ERL NIF TERM (Resource: Snapshot*)

    %% "iterate_upper_bound" defines the extent upto which the forward iterator
    %% can returns entries. Once the bound is reached, Valid() will be false.
    %% "iterate_upper_bound" is exclusive ie the bound value is
    %% not a valid entry.  If iterator_extractor is not null, the Seek target
    %% and iterator_upper_bound need to have the same prefix.
    %% This is because ordering is not guaranteed outside of prefix domain.
    %% There is no lower bound on the iterator. If needed, that can be easily
    %% implemented
    %%
    %% Default: nullptr
    iterate_upper_bound :: binary(),  %% ERL NIF TERM (Resource: Slice*)

    %% Specify if this read request should process data that ALREADY
    %% resides on a particular cache. If the required data is not
    %% found at the specified cache, then Status::Incomplete is returned.
    %% Default: kReadAllTier
    read_tier :: string(),

    %% Specify to create a tailing iterator -- a special iterator that has a
    %% view of the complete database (i.e. it can also be used to read newly
    %% added data) and is optimized for sequential reads. It will return records
    %% that were inserted into the database after the creation of the iterator.
    %% Default: false
    %% Not supported in ROCKSDB_LITE mode!
    tailing = false :: boolean(),

    %% Specify to create a managed iterator -- a special iterator that
    %% uses less resources by having the ability to free its underlying
    %% resources on request.
    %% Default: false
    %% Not supported in ROCKSDB_LITE mode!
    managed = false :: boolean()
}).

%% Options that control write operations
-record(rocksdb_writeoptions, {
    %% If true, the write will be flushed from the operating system
    %% buffer cache (by calling WritableFile::Sync()) before the write
    %% is considered complete.  If this flag is true, writes will be
    %% slower.
    %%
    %% If this flag is false, and the machine crashes, some recent
    %% writes may be lost.  Note that if it is just the process that
    %% crashes (i.e., the machine does not reboot), no writes will be
    %% lost even if sync==false.
    %%
    %% In other words, a DB write with sync==false has similar
    %% crash semantics as the "write()" system call.  A DB write
    %% with sync==true has similar crash semantics to a "write()"
    %% system call followed by "fdatasync()".
    %%
    %% Default: false
    sync = false :: boolean(), %%true | false

    %% If true, writes will not first go to the write ahead log,
    %% and the write may got lost after a crash.
    disableWAL = false :: boolean(), %%true | false

    %% If true and if user is trying to write to column families that don't exist
    %% (they were dropped),  ignore the write (don't return an error). If there
    %% are multiple writes in a WriteBatch, other writes will succeed.
    %% Default: false
    ignore_missing_column_families = false :: boolean(), %%true | false

    %% If true and we need to wait or sleep for the write request, fails
    %% immediately with Status::Incomplete().
    no_slowdown = false :: boolean() %%true | false
}).
