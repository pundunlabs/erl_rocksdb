#include "rocksdb_nif.h"
#include "rocksdb/convenience.h"
#include "table/block_based_table_factory.h"

namespace {
    /* atoms */
    ERL_NIF_TERM atom_error;
    ERL_NIF_TERM atom_not_found;
    ERL_NIF_TERM atom_corruption;
    //ERL_NIF_TERM atom_io_error;
    //ERL_NIF_TERM atom_invalid_argument;
    ERL_NIF_TERM atom_merge_in_progress;
    ERL_NIF_TERM atom_incomplete;
    ERL_NIF_TERM atom_shutdown_in_progress;
    ERL_NIF_TERM atom_timed_out;
    ERL_NIF_TERM atom_aborted;
    ERL_NIF_TERM atom_lock_limit;
    ERL_NIF_TERM atom_busy;
    ERL_NIF_TERM atom_deadlock;
    ERL_NIF_TERM atom_expired;
    ERL_NIF_TERM atom_try_again;
    ERL_NIF_TERM atom_no_space;
    ERL_NIF_TERM atom_mem_total;
    ERL_NIF_TERM atom_mem_unflushed;
    ERL_NIF_TERM atom_mem_cached;

    int get_bool(ErlNifEnv* env, ERL_NIF_TERM term) {
	char buf[6];

	if(enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1)) {
	    if (strcmp("false", buf) == 0)
		return 0;
	    if (strcmp("true", buf) == 0)
		return 1;
	}
	return -1;
    }
}

int parse_kvl_to_map(ErlNifEnv* env, ERL_NIF_TERM kvl, unordered_map<string,string>& map);

void init_lib_atoms(ErlNifEnv* env) {
    atom_error = enif_make_atom(env, "error");
    atom_not_found = enif_make_atom(env, "not_found");
    atom_corruption = enif_make_atom(env, "corruption");
    //atom_invalid_argument = enif_make_atom(env, "invalid_argument");
    atom_merge_in_progress = enif_make_atom(env, "merge_in_progress");
    atom_incomplete = enif_make_atom(env, "incomplete");
    atom_shutdown_in_progress = enif_make_atom(env, "shutdown_in_progress");
    atom_timed_out = enif_make_atom(env, "timed_out");
    atom_aborted = enif_make_atom(env, "aborted");
    atom_lock_limit = enif_make_atom(env, "lock_limit");
    atom_busy = enif_make_atom(env, "busy");
    atom_deadlock = enif_make_atom(env, "deadlock");
    atom_expired = enif_make_atom(env, "expired");
    atom_try_again = enif_make_atom(env, "try_again");
    atom_no_space = enif_make_atom(env, "no_space");
    atom_mem_total = enif_make_atom(env, "mem_total");
    atom_mem_unflushed = enif_make_atom(env, "mem_unflushed");
    atom_mem_cached = enif_make_atom(env, "mem_cached");
}

void delete_db(db_obj_resource* rdb) {
    rdb->mtx->lock();

    delete rdb->cfd_options;
    delete rdb->cfi_options;
    delete rdb->cfr_options;

    if( rdb->handles ) {
        for( auto h : *(rdb->handles) ) { delete h; }
	delete rdb->handles;
    }

    delete rdb->link_set;

    if(rdb->type == DB_WITH_TTL && rdb->db_open) {
	rocksdb::DBWithTTL *db;
	db = (rocksdb::DBWithTTL*) rdb->object;
	delete db;
    }
    else if(rdb->db_open) {
	rocksdb::DB *db;
	db = (rocksdb::DB*) rdb->object;
	delete db;
    }

    rdb->allocated = 0;
    rdb->mtx->unlock();
}

void delete_rit(it_obj_resource* rit) {
    db_obj_resource *rdb;
    unordered_set<void*> *set;

    rdb = (db_obj_resource*) rit->linked_obj;
    set = rdb->link_set;
    rdb->mtx->lock();
    set->erase(rit);
    rdb->mtx->unlock();

    rocksdb::Iterator *it;
    it = (rocksdb::Iterator*) rit->object;
    rit->mtx->lock();
    delete it;
    rit->allocated = 0;
    rit->mtx->unlock();

}

int fix_cf_options(ErlNifEnv* env, ERL_NIF_TERM kvl,
		   db_obj_resource* rdb,
		   rocksdb::DBOptions* options,
		   rocksdb::Status& status,
		   int num_threads,
		   std::shared_ptr<rocksdb::Cache>* shared_lru) {
    unsigned int kvl_len;
    char temp[MAXPATHLEN];

    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* tuple;
    int arity;
    
    rocksdb::BlockBasedTableOptions bbto;

    if (!enif_get_list_length(env, kvl, &kvl_len)) {
	return -1;
    }

    while(enif_get_list_cell(env, kvl, &head, &tail)) {

	if(!enif_get_tuple(env, head, &arity, &tuple)) {
	    return -1;
	}

	if(arity != 2 || enif_get_string(env, tuple[0], temp, sizeof(temp), ERL_NIF_LATIN1) < 1 ) {
	    return -1;
	}

	if(strcmp(temp, "comparator") == 0) {
	    if(enif_get_string(env, tuple[1], temp, sizeof(temp), ERL_NIF_LATIN1) < 1) {
		return -1;
	    }
	    if(strcmp(temp, "descending") == 0) {
		rdb->cfd_options->comparator = rocksdb::ReverseBytewiseComparator();
	    }
	}

	if(strcmp(temp, "ttl") == 0) {
	    int int_ttl;
	    if(!enif_get_int(env, tuple[1], &int_ttl)) {
		return -1;
	    }
	    rdb->ttl = (int32_t)int_ttl;
	    rdb->type = DB_WITH_TTL;
	    rdb->cfr_options->merge_operator.reset(new rocksdb::TermIndexMerger(rdb->ttl));
	} else {
	    rdb->cfr_options->merge_operator.reset(new rocksdb::TermIndexMerger(0));
	}

	if(strcmp(temp, "cf_raw_opts") == 0) {
	    unordered_map<string,string> opts_map;
	    parse_kvl_to_map(env, tuple[1], opts_map);

	    status = rocksdb::GetColumnFamilyOptionsFromMap(*rdb->cfd_options, // base
							    opts_map,
							    rdb->cfd_options, // update
							    true);
	    if(!status.ok()) {
		return -1;
	    }
	    status = rocksdb::GetColumnFamilyOptionsFromMap(*rdb->cfi_options, // base
							    opts_map,
							    rdb->cfi_options, // update
							    true);
	    if(!status.ok()) {
		return -1;
	    }
	    status = rocksdb::GetColumnFamilyOptionsFromMap(*rdb->cfr_options, // base
							    opts_map,
							    rdb->cfr_options, // update
							    true);
	    if(!status.ok()) {
		return -1;
	    }

	}

	bbto.no_block_cache = false;
	bbto.cache_index_and_filter_blocks = false;
	bbto.pin_top_level_index_and_filter = true;
	bbto.index_type = rocksdb::BlockBasedTableOptions::IndexType::kTwoLevelIndexSearch;
	bbto.filter_policy.reset(rocksdb::NewBloomFilterPolicy(10, false));
	bbto.partition_filters = true;
	bbto.metadata_block_size = 4096;
	bbto.cache_index_and_filter_blocks_with_high_priority = true;
	bbto.block_size = 32 * 1024;

	if(strcmp(temp, "cache_size") == 0 && shared_lru == nullptr) {
	    int cache_size; // cache size in MB

	    if(!enif_get_int(env, tuple[1], &cache_size)) {
		return -1;
	    }

	    // convert to bytes
	    cache_size *= 1024*1024;
	    bbto.block_cache = rocksdb::NewLRUCache(cache_size);
	}

	if(strcmp(temp, "write_buffer_size") == 0) {
	    int write_buffer_size; // write buffer size in MB

	    if(!enif_get_int(env, tuple[1], &write_buffer_size)) {
		return -1;
	    }
	    // convert to bytes
	    write_buffer_size *= 1024*1024;

	    rdb->cfd_options->write_buffer_size = write_buffer_size;
	    rdb->cfi_options->write_buffer_size = write_buffer_size;
	    rdb->cfr_options->write_buffer_size = write_buffer_size;
	}

	if(strcmp(temp, "fifo_ttl") == 0) {
	    const ERL_NIF_TERM* fifo_tuple;
	    int fifo_arity;
	    uint64_t ttl; // time to live in seconds
	    uint64_t size; // max table files size
	    if(!enif_get_tuple(env, tuple[1], &fifo_arity, &fifo_tuple)) {
		return -1;
	    }

	    if(!enif_get_uint64(env, fifo_tuple[0], (unsigned long*) &ttl)) {
		return -1;
	    }

	    if(!enif_get_uint64(env, fifo_tuple[1], (unsigned long*) &size)) {
		return -1;
	    }

	    // convert to bytes, input in MB
	    size *= 1024*1024;
	    rdb->cfd_options->compaction_style = rocksdb::kCompactionStyleFIFO;
	    rdb->cfd_options->compaction_options_fifo.max_table_files_size = size;
	    rdb->cfd_options->compaction_options_fifo.allow_compaction = true;
	    rdb->cfd_options->compaction_options_fifo.ttl = ttl;
	}
	kvl = tail;
    }

    if (shared_lru != nullptr) {
	    bbto.block_cache = *shared_lru;
    }

    /* set block based table options */
    rdb->cfd_options->table_factory.reset(NewBlockBasedTableFactory(bbto));
    rdb->cfi_options->table_factory.reset(NewBlockBasedTableFactory(bbto));
    rdb->cfr_options->table_factory.reset(NewBlockBasedTableFactory(bbto));

    rdb->cfi_options->max_write_buffer_number=5;
    rdb->cfr_options->max_write_buffer_number=5;
    rdb->cfi_options->min_write_buffer_number_to_merge=2;
    rdb->cfr_options->min_write_buffer_number_to_merge=2;
    // Enable some features supporting prefix extraction
    // 2 Bytes for encoded Cid. Used when deleting by cid.
    rdb->cfi_options->prefix_extractor.reset(rocksdb::NewFixedPrefixTransform(2));
    rdb->cfr_options->prefix_extractor.reset(rocksdb::NewFixedPrefixTransform(2));

    options->env->SetBackgroundThreads(num_threads);
    options->max_background_jobs = num_threads;

    options->skip_stats_update_on_db_open=true;

    //options->level0_stop_writes_trigger=36
    //options->level0_slowdown_writes_trigger=20
    return 0;
}

int init_readoptions(ErlNifEnv* env,
		     const ERL_NIF_TERM* readoptions_array,
		     rocksdb::ReadOptions **_readoptions) {
    int temp;
    rocksdb::ReadOptions *readoptions = new rocksdb::ReadOptions;
    *_readoptions = readoptions;

    /* Representation of erlang tuple:
    // 0. rocksdb_options,
    // 1. verify_checksums,
    // 2. fill_cache,
    // 3. snapshot,
    // 4. iterate_upper_bound,
    // 5. read_tier,
    // 6. tailing,
    // 7. managed
    */

    //Set verify_checksums
    temp = get_bool(env, readoptions_array[1]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->verify_checksums = temp;
    }

    //Set fill_cache
    temp = get_bool(env, readoptions_array[2]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->fill_cache = temp;
    }

    //Set tailing
    temp = get_bool(env, readoptions_array[6]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->tailing = temp;
    }

    //Set managed
    temp = get_bool(env, readoptions_array[7]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->managed = temp;
    }
    return 0;
}

int init_writeoptions(ErlNifEnv* env,
		      const ERL_NIF_TERM* writeoptions_array,
		      rocksdb::WriteOptions **_writeoptions) {
    int temp;
    rocksdb::WriteOptions *writeoptions = new rocksdb::WriteOptions;
    *_writeoptions = writeoptions;

    /* Representation of erlang tuple:
    // 0. rocksdb_writeoptions
    // 1. sync,
    // 2. disableWAL,
    // 3. ignore_missing_column_families,
    // 4. no_slowdown
    */

    temp = get_bool(env, writeoptions_array[1]);
    if (temp == -1) {
	return -1;
    }
    else{
	writeoptions->sync = temp;
    }

    //Set disableWAL
    temp = get_bool(env, writeoptions_array[2]);
    if (temp == -1) {
	return -1;
    }
    else{
	writeoptions->disableWAL = temp;
    }

    //Set ignore_missing_column_families,
    temp = get_bool(env, writeoptions_array[3]);
    if (temp == -1) {
	return -1;
    }
    else{
	writeoptions->ignore_missing_column_families = temp;
    }

    //Set no_slowdown
    temp = get_bool(env, writeoptions_array[4]);
    if (temp == -1) {
	return -1;
    }
    else{
	writeoptions->no_slowdown = temp;
    }
    return 0;
}

void open_db(rocksdb::DBOptions* options,
	     char* path,
	     db_obj_resource* rdb,
	     rocksdb::Status* status) {
    string path_string(path);
    vector<rocksdb::ColumnFamilyDescriptor> column_families;
    column_families.push_back(rocksdb::ColumnFamilyDescriptor(rocksdb::kDefaultColumnFamilyName, *(rdb->cfd_options)));
    column_families.push_back(rocksdb::ColumnFamilyDescriptor("index", *(rdb->cfi_options)));
    column_families.push_back(rocksdb::ColumnFamilyDescriptor("reverse_index", *(rdb->cfr_options)));
    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db;
	bool read_only = false;
	std::vector<int32_t> ttl_values (3, rdb->ttl);
	*status = rocksdb::DBWithTTL::Open(*options, path_string,
					   column_families, rdb->handles, &db,
					   ttl_values, read_only);
	rdb->object = db;
    } else {
	rocksdb::DB* db;
	*status = rocksdb::DB::Open(*options, path_string,
				    column_families, rdb->handles, &db);
	rdb->object = db;
	rdb->type = DB_DEFAULT;
    }
}

rocksdb::Status Get(db_obj_resource* rdb,
		    rocksdb::ReadOptions* readoptions,
		    int cf,
		    rocksdb::Slice* key,
		    rocksdb::PinnableSlice* value) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Get(*readoptions, rdb->handles->at(cf), *key, value);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Get(*readoptions, rdb->handles->at(cf), *key, value);
    }
    return status;
}

rocksdb::Status Put(db_obj_resource* rdb,
		    rocksdb::WriteOptions* writeoptions,
		    rocksdb::Slice* key,
		    rocksdb::Slice* value) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Put(*writeoptions, *key, *value);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Put(*writeoptions, *key, *value);
    }
    return status;
}

rocksdb::Status PutTerms(db_obj_resource* rdb,
			 rocksdb::WriteOptions* writeoptions,
			 rocksdb::Slice* key,
			 rocksdb::Slice* value,
			 std::vector<std::pair<Term, std::vector<Term>>> indices
			) {
    TermPrep tp = TermPrep(indices, key);
    rocksdb::Status status;
    rocksdb::WriteBatch batch;
    //Put value
    batch.Put(rdb->handles->at(0), *key, *value);
    //Merge for keeping index history
    for (auto it = tp.index_.begin(); it != tp.index_.end(); ++it){
	batch.Put(rdb->handles->at(1),
		  rocksdb::Slice(it->first),
		  rocksdb::Slice(it->second));
    }
    //Merges for reverse indexes
    for (auto it = tp.rev_index_.begin(); it != tp.rev_index_.end(); ++it){
	batch.Merge(rdb->handles->at(2),
		    rocksdb::Slice(it->first),
		    rocksdb::Slice(it->second));
    }
    //Apply batch according to db type
    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db = static_cast<rocksdb::DBWithTTL*>(rdb->object);
	status = db->Write(*writeoptions, &batch);
    } else {
	rocksdb::DB* db = static_cast<rocksdb::DB*>(rdb->object);
	status = db->Write(*writeoptions, &batch);
    }
    return status;
}

rocksdb::Status Write(db_obj_resource* rdb,
		      rocksdb::WriteOptions* writeoptions,
		      rocksdb::WriteBatch* batch) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Write(*writeoptions, batch);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Write(*writeoptions, batch);
    }
    return status;
}

rocksdb::Status Delete(db_obj_resource* rdb,
		       rocksdb::WriteOptions* writeoptions,
		       rocksdb::Slice* key) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Delete(*writeoptions, *key);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Delete(*writeoptions, *key);
    }
    return status;
}

rocksdb::Status DeleteTerms(db_obj_resource* rdb,
			    rocksdb::WriteOptions* writeoptions,
			    rocksdb::Slice* key,
			    std::vector<Term> cids) {
    TermDelete td = TermDelete(cids, key);
    rocksdb::Status status;
    rocksdb::WriteBatch batch;
    rocksdb::ReadOptions readoptions;
    //Delete value
    batch.Delete(rdb->handles->at(0), *key);
    //Delete index history
    for (auto it = td.index_.begin(); it != td.index_.end(); ++it) {
	rocksdb::Slice key2term(*it);
	rocksdb::PinnableSlice value;
	status = Get(rdb, &readoptions, 1, &key2term, &value);
	if (status.ok()) {
	    td.ParseReveseIndex(*it, &value);
	}
	batch.Delete(rdb->handles->at(1), key2term);
    }
    //Merges for reverse indexes
    for (auto it = td.rev_index_.begin(); it != td.rev_index_.end(); ++it){
	batch.Merge(rdb->handles->at(2),
		    rocksdb::Slice(*it),
		    rocksdb::Slice(td.posting_));
    }
    //Apply batch according to db type
    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db = static_cast<rocksdb::DBWithTTL*>(rdb->object);
	status = db->Write(*writeoptions, &batch);
    } else {
	rocksdb::DB* db = static_cast<rocksdb::DB*>(rdb->object);
	status = db->Write(*writeoptions, &batch);
    }
    return status;
}

void GetApproximateSizes(db_obj_resource* rdb,
			 rocksdb::Range* ranges,
			 unsigned int ranges_size,
			 uint64_t* size) {
    if (rdb->type == DB_WITH_TTL) {
	static_cast<rocksdb::DBWithTTL*>(rdb->object)->GetApproximateSizes(ranges, ranges_size, size);
    } else {
	static_cast<rocksdb::DB*>(rdb->object)->GetApproximateSizes(ranges, ranges_size, size);
    }
}

rocksdb::Iterator* NewIterator(db_obj_resource* rdb,
			       rocksdb::ReadOptions* readoptions) {
    rocksdb::Iterator* it;
    if (rdb->type == DB_WITH_TTL) {
	it = static_cast<rocksdb::DBWithTTL*>(rdb->object)->NewIterator(*readoptions);
    } else {
	it = static_cast<rocksdb::DB*>(rdb->object)->NewIterator(*readoptions);
    }
    return it;
}

void CompactDB(db_obj_resource* rdb) {
    if (rdb->type == DB_WITH_TTL) {
	static_cast<rocksdb::DBWithTTL*>(rdb->object)->CompactRange(rocksdb::CompactRangeOptions(), rdb->handles->at(0), nullptr, nullptr);
    } else {
	static_cast<rocksdb::DB*>(rdb->object)->CompactRange(rocksdb::CompactRangeOptions(), rdb->handles->at(0), nullptr, nullptr);
    }
}

void CompactIndex(db_obj_resource* rdb) {
    if (rdb->type == DB_WITH_TTL) {
	static_cast<rocksdb::DBWithTTL*>(rdb->object)->CompactRange(rocksdb::CompactRangeOptions(), rdb->handles->at(1), nullptr, nullptr);
    } else {
	static_cast<rocksdb::DB*>(rdb->object)->CompactRange(rocksdb::CompactRangeOptions(), rdb->handles->at(1), nullptr, nullptr);
    }
}

rocksdb::Status BackupDB(db_obj_resource* rdb,
			 char* path) {
    string path_string(path);
    rocksdb::Status status;

    rocksdb::BackupEngine* backup_engine;
    status = rocksdb::BackupEngine::Open(rocksdb::Env::Default(), rocksdb::BackupableDBOptions(path_string), &backup_engine);

    if(status.ok()) {
	if (rdb->type == DB_WITH_TTL) {
	    status = backup_engine->CreateNewBackup(static_cast<rocksdb::DBWithTTL*>(rdb->object));
	} else {
	    status = backup_engine->CreateNewBackup(static_cast<rocksdb::DB*>(rdb->object));
	}
    }
    delete backup_engine;
    return status;
}

rocksdb::Status RestoreDB(char* bkp_path,
			  char* db_path,
			  char* wal_path) {
    string bkp_path_str(bkp_path);
    string db_path_str(db_path);
    string wal_path_str(wal_path);
    //rocksdb::StderrLogger logger;
    //options.info_log = &logger;
    rocksdb::BackupEngineReadOnly* backup_engine;
    rocksdb::Status status =
	rocksdb::BackupEngineReadOnly::Open(rocksdb::Env::Default(),
					    rocksdb::BackupableDBOptions(bkp_path_str),
					    &backup_engine);
    if(status.ok()) {
	rocksdb::RestoreOptions restore_options(false);
	status = backup_engine->RestoreDBFromLatestBackup(db_path_str, wal_path_str, restore_options);
    }
    delete backup_engine;
    return status;
}

rocksdb::Status RestoreDB(char* bkp_path,
			  char* db_path,
			  char* wal_path,
			  uint32_t backup_id) {
    string bkp_path_str(bkp_path);
    string db_path_str(db_path);
    string wal_path_str(wal_path);
    //rocksdb::StderrLogger logger;
    //options.info_log = &logger;

    rocksdb::BackupEngineReadOnly* backup_engine;
    rocksdb::Status status =
	rocksdb::BackupEngineReadOnly::Open(rocksdb::Env::Default(),
					    rocksdb::BackupableDBOptions(bkp_path_str),
					    &backup_engine);

    if(status.ok()) {
	status = backup_engine->RestoreDBFromBackup(backup_id,
						    db_path_str,
						    wal_path_str);
    }
    delete backup_engine;
    return status;
}

rocksdb::Status CreateCheckpoint(db_obj_resource* rdb,
				 char* path) {
    const string path_string(path);
    rocksdb::Status status;

    rocksdb::Checkpoint* checkpoint;
    if (rdb->type == DB_WITH_TTL) {
	status = rocksdb::Checkpoint::Create(static_cast<rocksdb::DBWithTTL*>(rdb->object), &checkpoint);
    } else {
	status = rocksdb::Checkpoint::Create(static_cast<rocksdb::DB*>(rdb->object), &checkpoint);
    }

    if(status.ok()) {
	status = checkpoint->CreateCheckpoint(path_string);
    }

    return status;
}

ERL_NIF_TERM make_status_tuple(ErlNifEnv* env,
			       rocksdb::Status* status) {
    ERL_NIF_TERM type;
    if(status->IsNotFound()) {
	type = atom_not_found;
    }
    else if(status->IsCorruption()) {
	type = atom_corruption;
    }
    else if(status->IsIOError()) {
	type = enif_make_string(env, status->ToString().c_str(), ERL_NIF_LATIN1);
	//type = atom_io_error;
    }
    else if(status->IsInvalidArgument()) {
	type = enif_make_string(env, status->ToString().c_str(), ERL_NIF_LATIN1);
	//type = atom_invalid_argument;
    }
    else if(status->IsMergeInProgress()) {
	type = atom_merge_in_progress;
    }
    else if(status->IsIncomplete()) {
	type = atom_incomplete;
    }
    else if(status->IsShutdownInProgress()) {
	type = atom_shutdown_in_progress;
    }
    else if(status->IsTimedOut()) {
	type = atom_timed_out;
    }
    else if(status->IsAborted()) {
	type = atom_aborted;
    }
    else if(status->IsLockLimit()) {
	type = atom_lock_limit;
    }
    else if(status->IsBusy()) {
	type = atom_busy;
    }
    else if(status->IsDeadlock()) {
	type = atom_deadlock;
    }
    else if(status->IsExpired()) {
	type = atom_expired;
    }
    else if(status->IsTryAgain()) {
	type = atom_try_again;
    }
    else if(status->IsNoSpace()) {
	type = atom_no_space;
    }
    else {
	type = enif_make_string(env, status->ToString().c_str(), ERL_NIF_LATIN1);
    }
    //const char* stString = status->ToString().c_str();
    return enif_make_tuple2(env, atom_error, type);
}

int parse_kvl_to_map(ErlNifEnv* env, ERL_NIF_TERM kvl, unordered_map<string,string>& map) {
    unsigned int kvl_len;
    char key[1024];
    char value[1024];

    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* tuple;
    int arity;

    if(!enif_get_list_length(env, kvl, &kvl_len)) {
	return 0;
    }

    map.reserve(kvl_len);

    while(enif_get_list_cell(env, kvl, &head, &tail)) {
	if(!enif_get_tuple(env, head, &arity, &tuple)) {
	    return 0;
	}
	if(arity != 2 || !enif_get_string(env, tuple[0], key, sizeof(key), ERL_NIF_LATIN1)) {
	    return 0;
	}
	if(!enif_get_string(env, tuple[1], value, sizeof(value), ERL_NIF_LATIN1)) {
	    return 0;
	}
	pair<string, string> p ((const char*)key, (const char*)value);
	map.insert( p );
        kvl = tail;
    }

    return 1;
}

void SetTtl(db_obj_resource* rdb, int32_t ttl) {
    if (rdb->type == DB_WITH_TTL) {
	if( rdb->handles ) {
	    for( auto h : *(rdb->handles) ) { 
		static_cast<rocksdb::DBWithTTLImpl*>(rdb->object)->SetTtl(h, ttl);
	    }
	}
    }
}

ERL_NIF_TERM rocksdb_memory_usage(ErlNifEnv* env, db_obj_resource* rdb) {
    rocksdb::DB* db;
    uint64_t mem_total = 0;
    uint64_t mem_unflushed = 0;
    uint64_t mem_cached = 0;
    const rocksdb::BlockBasedTableFactory* bbtfd;
    const rocksdb::BlockBasedTableFactory* bbtfi;
    const rocksdb::BlockBasedTableFactory* bbtfr;

    db = static_cast<rocksdb::DB*>(rdb->object);

    //MemTable Total
    db->GetAggregatedIntProperty(rocksdb::DB::Properties::kSizeAllMemTables,
                                 &mem_total);
    //MemTable UnFlushed Mem
    db->GetAggregatedIntProperty(rocksdb::DB::Properties::kCurSizeAllMemTables,
				 &mem_unflushed);

    //Cache Memory Data
    bbtfd =
	static_cast<const rocksdb::BlockBasedTableFactory *>
	    (rdb->cfd_options->table_factory.get());
    const auto bbt_optsd = bbtfd->table_options();
    mem_cached = bbt_optsd.block_cache->GetUsage();

    //Cache Memory Index
    bbtfi =
	static_cast<const rocksdb::BlockBasedTableFactory *>
	    (rdb->cfi_options->table_factory.get());
    const auto bbt_optsi = bbtfi->table_options();
    mem_cached = bbt_optsi.block_cache->GetUsage();

    //Cache Memory Reverse Index
    bbtfr =
	static_cast<const rocksdb::BlockBasedTableFactory *>
	    (rdb->cfr_options->table_factory.get());
    const auto bbt_optsr = bbtfr->table_options();
    mem_cached = bbt_optsr.block_cache->GetUsage();

    return enif_make_list3(env,
		enif_make_tuple2(env, atom_mem_total,
			enif_make_int64(env, (ErlNifSInt64) mem_total)),

		enif_make_tuple2(env, atom_mem_unflushed,
			enif_make_int64(env, (ErlNifSInt64) mem_unflushed)),

		enif_make_tuple2(env, atom_mem_cached,
			enif_make_int64(env, (ErlNifSInt64) mem_cached)));
}
