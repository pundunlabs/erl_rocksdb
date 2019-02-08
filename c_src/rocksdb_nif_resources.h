#ifndef ROCKSDB_NIF_RESOURCES_H
#define ROCKSDB_NIF_RESOURCES_H
#include "rocksdb/db.h"
#include "erl_nif.h"

using namespace std;

struct Term {
  const char* data;
  size_t size;
};

typedef struct _opt_obj_resource {
  void *object;
} opt_obj_resource;

typedef struct _db_obj_resource {
  char allocated;
  unordered_set<void*>* link_set;
  mutex* mtx;
  void* object;
  int32_t ttl;
  char type;
  char db_open;
  rocksdb::ColumnFamilyOptions* cfd_options;
  rocksdb::ColumnFamilyOptions* cfi_options;
  rocksdb::ColumnFamilyOptions* cfr_options;
  vector<rocksdb::ColumnFamilyHandle*>* handles;
} db_obj_resource;

typedef struct _it_obj_resource {
  char allocated;
  mutex *mtx;
  void *linked_obj;
  void *object;
} it_obj_resource;

typedef struct _lru_obj_resource {
  void *object;
} lru_obj_resource;

#endif /*ROCKSDB_NIF_RESOUCES_H*/
