#include <iostream>
#include "rocksdb_nif.h"
#include "index_merger.h"
#include "index_filter.h"
#include <assert.h>
//#include "util/stderr_logger.h"
//#include <iostream>

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

    int get_bool(ErlNifEnv* env, ERL_NIF_TERM term)
    {
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
}

void delete_db(db_obj_resource* rdb) {
    rdb->mtx->lock();
    delete rdb->pid;
    delete rdb->env_box;
    delete rdb->cfd_options;
    delete rdb->cfi_options;
    if( rdb->handles ) {
        for( auto h : *(rdb->handles) ) { delete h; }
    }
    delete rdb->link_set;

    if(rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL *db;
	db = (rocksdb::DBWithTTL*) rdb->object;
	delete db;
    }
    else {
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
		   rocksdb::ColumnFamilyOptions* cfd_options,
		   rocksdb::ColumnFamilyOptions* cfi_options,
		   db_obj_resource* rdb) {
    rocksdb::EnvBox* env_box = rdb->env_box;
    unsigned int kvl_len;
    char temp[MAXPATHLEN];

    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* tuple;
    int arity;

    ErlNifPid *pid = (ErlNifPid*)malloc( sizeof(ErlNifPid));

    if (!enif_get_list_length(env, kvl, &kvl_len)) {
	return -1;
    }

    while(enif_get_list_cell(env, kvl, &head, &tail)) {
	if(!enif_get_tuple(env, head, &arity, &tuple)) {
	    return enif_make_badarg(env);
	}
	if(arity != 2 || enif_get_string(env, tuple[0], temp, sizeof(temp), ERL_NIF_LATIN1) < 1 ) {
	    return enif_make_badarg(env);
	}
	if(strcmp(temp, "pid") == 0) {
	    if(!enif_get_local_pid(env, tuple[1], pid)) {
		return -1;
	    }
	    rdb->pid = pid;
	    cfi_options->merge_operator.reset(new rocksdb::IndexMerger(pid, env_box));
	    //rocksdb::IndexFilter *filter = new rocksdb::IndexFilter();
	    //cfi_options->compaction_filter = filter;
	}
	if(strcmp(temp, "term_index") == 0) {
	    cfd_options->merge_operator.reset(new rocksdb::TermIndexMerger());
	    //rocksdb::IndexFilter *filter = new rocksdb::IndexFilter();
	    //cfd_options->compaction_filter = filter;
	}
	if(strcmp(temp, "comparator") == 0) {
	    if(enif_get_string(env, tuple[1], temp, sizeof(temp), ERL_NIF_LATIN1) < 1) {
		return -1;
	    }
	    if(strcmp(temp, "descending") == 0) {
		cfi_options->comparator = rocksdb::ReverseBytewiseComparator();
		cfd_options->comparator = rocksdb::ReverseBytewiseComparator();
	    }
	}
	if(strcmp(temp, "ttl") == 0) {
	    int int_ttl;
	    if(!enif_get_int(env, tuple[1], &int_ttl)) {
		return -1;
	    }
	    rdb->ttl = (int32_t)int_ttl;
	    rdb->type = DB_WITH_TTL;
	}
        kvl = tail;
    }
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
    return 0;

    //Set tailing
    temp = get_bool(env, readoptions_array[6]);
    if (temp == -1) {
	return -1;
    }
    else{
	readoptions->tailing = temp;
    }
    return 0;

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

    //Set sync
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

    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db;
	bool read_only = false;
	std::vector<int32_t> ttl_values (2, rdb->ttl);
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
		    rocksdb::Slice* key,
		    string* value) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Get(*readoptions, *key, value);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Get(*readoptions, *key, value);
    }
    return status;
}

rocksdb::Status Put(db_obj_resource* rdb,
		    rocksdb::WriteOptions* writeoptions,
		    rocksdb::Slice* key, rocksdb::Slice* value) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	status = static_cast<rocksdb::DBWithTTL*>(rdb->object)->Put(*writeoptions, *key, *value);
    } else {
	status = static_cast<rocksdb::DB*>(rdb->object)->Put(*writeoptions, *key, *value);
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

rocksdb::Status IndexMerge(db_obj_resource* rdb,
			   rocksdb::WriteOptions* writeoptions,
			   rocksdb::Slice* key,
			   rocksdb::Slice* value) {
    rocksdb::Status status;
    std::string val;
    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db = static_cast<rocksdb::DBWithTTL*>(rdb->object);
	status = db->Merge(*writeoptions, rdb->handles->at(1), *key, *value);
	status = db->Get(rocksdb::ReadOptions(), rdb->handles->at(1), *key, &val);
    } else {
	rocksdb::DB* db = static_cast<rocksdb::DB*>(rdb->object);
	status = db->Merge(*writeoptions, rdb->handles->at(1), *key, *value);
	status = db->Get(rocksdb::ReadOptions(), rdb->handles->at(1), *key, &val);
    }
    return status;
}

rocksdb::Status TermIndex(db_obj_resource* rdb,
			  rocksdb::WriteOptions* writeoptions,
			  rocksdb::Slice* term,
			  rocksdb::Slice* key) {
    rocksdb::Status status;
    if (rdb->type == DB_WITH_TTL) {
	rocksdb::DBWithTTL* db = static_cast<rocksdb::DBWithTTL*>(rdb->object);
	status = db->Merge(*writeoptions, *term, *key);
    } else {
	rocksdb::DB* db = static_cast<rocksdb::DB*>(rdb->object);
	status = db->Merge(*writeoptions, *term, *key);
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
