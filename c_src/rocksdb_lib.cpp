#include "rocksdb_nif.h"
#include <assert.h>

#include <iostream>

using namespace rocksdb;

namespace {
  DescendingComparator descendingcomparator;

  int get_bool(ErlNifEnv* env, ERL_NIF_TERM term)
  {
      char buf[6];

      if(enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1)){
	  if (strcmp("false", buf) == 0)
	      return 0;
	  if (strcmp("true", buf) == 0)
	      return 1;
      }
      return -1;
  }
}

void delete_db(db_obj_resource* rdb){
    rocksdb::DB *db;
    db = (rocksdb::DB*) rdb->object;
    rdb->mtx->lock();
    delete rdb->link_set;
    delete db;
    rdb->allocated = 0;
    rdb->mtx->unlock();
}

void delete_rit(it_obj_resource* rit){
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

int init_readoptions(ErlNifEnv* env, const ERL_NIF_TERM* readoptions_array, rocksdb::ReadOptions **_readoptions) {
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

int init_writeoptions(ErlNifEnv* env, const ERL_NIF_TERM* writeoptions_array, rocksdb::WriteOptions **_writeoptions) {
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

rocksdb::DB* open_db(rocksdb::Options* options, char* path, rocksdb::Status* status) {
    rocksdb::DB* db;
    string path_string(path);
    *status = rocksdb::DB::Open(*options, path_string, &db);
    //Prints "Status: OK" if successful
    //cout << "Status: " << status->ToString() << "\n" << endl;
    //assert(status.ok());
    return db;
}

extern ERL_NIF_TERM make_status_tuple(ErlNifEnv* env, rocksdb::Status status){
    const char* type;
    if(status.IsNotFound()){
	type = "not_found";
    }
    else if(status.IsCorruption()){
	type = "corruption";
    }
    else if(status.IsIOError()){
	type = "io_error";
    }
    else if(status.IsInvalidArgument()){
	type = "invalid_argument";
    }
    else if(status.IsMergeInProgress()){
	type = "merge_in_progress";
    }
    else if(status.IsIncomplete()){
	type = "incomplete";
    }
    else if(status.IsShutdownInProgress()){
	type = "shutdown_in_progress";
    }
    else if(status.IsTimedOut()){
	type = "timed_out";
    }
    else if(status.IsAborted()){
	type = "aborted";
    }
    else if(status.IsLockLimit()){
	type = "lock_limit";
    }
    else if(status.IsBusy()){
	type = "busy";
    }
    else if(status.IsDeadlock()){
	type = "deadlock";
    }
    else if(status.IsExpired()){
	type = "expired";
    }
    else if(status.IsTryAgain()){
	type = "try_again";
    }
    else if(status.IsNoSpace()){
	type = "no_space";
    }
    else{
	type = status.ToString().c_str();
    }
    //const char* stString = status.ToString().c_str();
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
			    enif_make_atom(env, type));
}
