/* rocksdb_nif.h */
#ifndef LEVELDB_NIF_H
#define LEVELDB_NIF_H

#include <mutex>          // std::mutex
#include <unordered_set>  //std::unordered_set

#include "rocksdb/db.h"
#include "rocksdb/utilities/db_ttl.h"
#include "rocksdb/compaction_filter.h"
#include "rocksdb/comparator.h"
#include "rocksdb/write_batch.h"
#include "rocksdb/utilities/backupable_db.h"
#include "rocksdb/utilities/checkpoint.h"

#include "descendingcomparator.h"
#include "env_box.h"
#include "erl_nif.h"

#define MAXPATHLEN  255

#define DB_DEFAULT	0
#define DB_WITH_TTL	1

using namespace std;

typedef struct _opt_obj_resource {
  void *object;
} opt_obj_resource;

typedef struct _db_obj_resource {
  char allocated;
  unordered_set<void*> *link_set;
  mutex *mtx;
  void *object;
  ErlNifPid* pid;
  char type;
  rocksdb::EnvBox *env_box;
  rocksdb::ColumnFamilyOptions *cfi_options;
  vector<rocksdb::ColumnFamilyHandle*> *handles;
} db_obj_resource;

typedef struct _it_obj_resource {
  char allocated;
  mutex *mtx;
  void *linked_obj;
  void *object;
} it_obj_resource;

extern void init_lib_atoms(ErlNifEnv* env);

extern void delete_db(db_obj_resource* rdb);

extern void delete_rit(it_obj_resource* rit);

extern rocksdb::DB* open_db(rocksdb::DBOptions* options,
			    char* path,
			    rocksdb::ColumnFamilyOptions *cfd_options,
			    rocksdb::ColumnFamilyOptions *cfi_options,
			    vector<rocksdb::ColumnFamilyHandle*>* handle,
			    rocksdb::Status* status);

extern rocksdb::DBWithTTL* open_db_with_ttl(rocksdb::DBOptions* options,
					    char* path, int32_t* ttl,
					    rocksdb::Status* status);

extern int fix_cf_options(ErlNifEnv* env, ERL_NIF_TERM kvl,
			  rocksdb::ColumnFamilyOptions* cfd_options,
			  rocksdb::ColumnFamilyOptions* cfi_options,
			  db_obj_resource* rdb);

extern int init_readoptions(ErlNifEnv* env,
			    const ERL_NIF_TERM* readoptions_array,
			    rocksdb::ReadOptions **readoptions);

extern int init_writeoptions(ErlNifEnv* env,
			     const ERL_NIF_TERM* writeoptions_array,
			     rocksdb::WriteOptions **writeoptions);

extern void init_db(rocksdb::DB* db);

extern rocksdb::Status Get(db_obj_resource* rdb,
			   rocksdb::ReadOptions* readoptions,
			   rocksdb::Slice* key,
			   string* value);

extern rocksdb::Status Put(db_obj_resource* rdb,
			   rocksdb::WriteOptions* writeoptions,
			   rocksdb::Slice* key,
			   rocksdb::Slice* value);

extern rocksdb::Status Delete(db_obj_resource* rdb,
			      rocksdb::WriteOptions* writeoptions,
			      rocksdb::Slice* key);

extern rocksdb::Status Write(db_obj_resource* rdb,
			     rocksdb::WriteOptions* writeoptions,
			     rocksdb::WriteBatch* batch);

extern rocksdb::Status IndexMerge(db_obj_resource* rdb,
				  rocksdb::WriteOptions* writeoptions,
				  rocksdb::Slice* key,
				  rocksdb::Slice* value);

extern void GetApproximateSizes(db_obj_resource* rdb,
				rocksdb::Range* ranges,
				unsigned int ranges_size,
				uint64_t* size);

extern rocksdb::Iterator* NewIterator(db_obj_resource* rdb,
				      rocksdb::ReadOptions* readoptions);

extern void CompactDB(db_obj_resource* rdb);

extern void CompactIndex(db_obj_resource* rdb);

extern rocksdb::Status BackupDB(db_obj_resource* rdb,
				char* path);

extern rocksdb::Status RestoreDB(char* bkp_path,
				 char* db_path,
				 char* wal_path);

extern rocksdb::Status RestoreDB(char* bkp_path,
				 char* db_path,
				 char* wal_path,
				 uint32_t backup_id);

extern rocksdb::Status CreateCheckpoint(db_obj_resource* rdb,
					char* path);

extern ERL_NIF_TERM make_status_tuple(ErlNifEnv* env,
				      rocksdb::Status* status);
#endif /*LEVELDB_NIF_H*/
