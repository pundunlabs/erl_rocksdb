#include "rocksdb/c.h"
#include "options/options_parser.h"
#include "options/options_helper.h"
#include "rocksdb/convenience.h"
#include "util/string_util.h"
#include "util/sync_point.h"

#include "port/port.h"
#include "rocksdb_nif.h"

#include <string>
#include <unistd.h>

namespace  { /* anonymous namespace starts */

ErlNifResourceFlags resource_flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

ErlNifResourceType* optionResource;
ErlNifResourceType* readoptionResource;
ErlNifResourceType* writeoptionResource;
ErlNifResourceType* dbResource;
ErlNifResourceType* iteratorResource;
ErlNifResourceType* lruResource;


/* atoms */
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_invalid;
ERL_NIF_TERM atom_key_conflict;
ERL_NIF_TERM atom_complete;


void db_destructor(ErlNifEnv* env, void *db);
void option_destructor(ErlNifEnv* env, void *opts);
void readoption_destructor(ErlNifEnv* env, void *ropts);
void writeoption_destructor(ErlNifEnv* env, void *wopts);
void iterator_destructor(ErlNifEnv* env, void *it);
void lru_destructor(ErlNifEnv* env, void *lru);

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    dbResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "mydb_resource",
	    db_destructor,
	    resource_flags,
	    NULL);

    optionResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "options_resource",
	    option_destructor,
	    resource_flags,
	    0);

    readoptionResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "read_options_resource",
	    readoption_destructor,
	    resource_flags,
	    0);

    writeoptionResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "write_options_resource",
	    writeoption_destructor,
	    resource_flags,
	    0);

    iteratorResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "iterator_resource",
	    iterator_destructor,
	    resource_flags,
	    0);
    lruResource = enif_open_resource_type(env,
	    "rocksdb_nif",
	    "lru_resource",
	    lru_destructor,
	    resource_flags,
	    0);

    atom_ok	 = enif_make_atom(env, "ok");
    atom_error	 = enif_make_atom(env, "error");
    atom_invalid = enif_make_atom(env, "invalid");
    atom_key_conflict = enif_make_atom(env, "key_conflict");
    atom_complete = enif_make_atom(env, "complete");

    init_lib_atoms(env);

    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data,  void** old_priv_data,ERL_NIF_TERM load_info){
    *priv_data = *old_priv_data;
    load(env, priv_data, load_info);
    return 0;
}

void iterator_destructor(ErlNifEnv* env, void* _it) {
    it_obj_resource *rit = (it_obj_resource*) _it;
    if (rit->allocated){
	delete_rit( rit );
    }
    delete rit->mtx;
}
void db_destructor(ErlNifEnv* env, void* _db) {
    db_obj_resource *rdb = (db_obj_resource*) _db;
    if(rdb->allocated){
	delete_db( rdb );
    }
    delete rdb->mtx;
}
void option_destructor(ErlNifEnv* env, void* _opts) {
    opt_obj_resource *options = (opt_obj_resource*) _opts;
    delete (rocksdb::DBOptions*) options->object;
}
void readoption_destructor(ErlNifEnv* env, void* _ropts) {
    opt_obj_resource *ropts = (opt_obj_resource*) _ropts;
    delete (rocksdb::ReadOptions*) ropts->object;
}
void writeoption_destructor(ErlNifEnv* env, void* _wopts) {
    opt_obj_resource *wopts = (opt_obj_resource*) _wopts;
    delete (rocksdb::WriteOptions*) wopts->object;
}
void lru_destructor(ErlNifEnv* env, void* _lru) {
    lru_obj_resource *lru = (lru_obj_resource*) _lru;
    delete (std::shared_ptr<rocksdb::Cache>*) lru->object;
}

/* change ttl value for open db */
ERL_NIF_TERM set_ttl_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    int32_t ttl;

    /*get db_ptr resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get ttl integer*/
    if (!enif_get_int(env, argv[1], &ttl)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "ttl"));
    }
    SetTtl(rdb, ttl);
    return atom_ok;
}

/*Test NIFs for experimenting*/
ERL_NIF_TERM resource_test_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    /* ERL_NIF_TERM status_term; */
    /*return  atom_ok;*/
    if(!dbResource){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "resource_type"));
    }

    void* db_ptr = enif_alloc_resource(dbResource, 1024);

    term = enif_make_resource(env, db_ptr);

    enif_release_resource(db_ptr);
    return term;
}

/*rocksdb operations*/
ERL_NIF_TERM open_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    rocksdb::DBOptions* options;
    opt_obj_resource* opts;
    ERL_NIF_TERM kvl;
    rocksdb::Status status;
    int threads;
    lru_obj_resource* lru;
    std::shared_ptr<rocksdb::Cache>* lru_cache;

    if (argc < 3 || argc > 5) {
	return enif_make_badarg(env);
    }

    /*get options resource*/
    if (!enif_get_resource(env, argv[0], optionResource, (void **)&opts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "options"));
    }

    /*set will hold the iterators for this db*/
    options = (rocksdb::DBOptions*) opts->object;

    /*get path*/
    if(enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "path"));
    }
    /* third argument the key value list */
    kvl = argv[2];
     
    /* forth argument can be number of threads */
    if (argc >= 4) {
	if (!enif_get_int(env, argv[3], &threads)) {
	    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "threads"));
	}
    } else {
      threads = 8;
    }

    /* fifth argument can be shared/preallocated LRU cache if to be used for the tables */
    if (argc >= 5) {
	if(!enif_get_resource(env, argv[4], lruResource, (void **)&lru)) {
	    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "lru_cache"));
	}
	lru_cache = (std::shared_ptr<rocksdb::Cache>*) lru->object;
    } else {
	lru_cache = nullptr;
    }

    db_obj_resource* rdb = (db_obj_resource *) enif_alloc_resource(dbResource, sizeof(db_obj_resource));

    rdb->type = DB_DEFAULT;
    rdb->allocated = 1;
    rdb->db_open = 0;
    rdb->handles = new vector<rocksdb::ColumnFamilyHandle*>;
    rdb->link_set = new unordered_set<void*>;
    rdb->mtx = new mutex;
    rdb->cfd_options = new rocksdb::ColumnFamilyOptions();
    rdb->cfi_options = new rocksdb::ColumnFamilyOptions();
    rdb->cfr_options = new rocksdb::ColumnFamilyOptions();

    if ( fix_cf_options(env, kvl, rdb, options, status, threads, lru_cache) != 0 ) {
	enif_release_resource(rdb);
	if(!status.ok()) {
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
	return enif_make_badarg(env);
    } else {

	ERL_NIF_TERM db_term;

	open_db(options, path, rdb, &status);
	rdb->db_open = 1;

	if(status.ok()) {
	    db_term = enif_make_resource(env, rdb);
	    enif_release_resource(rdb);
	    /* resource now only owned by "Erlang" */
	    return enif_make_tuple2(env, atom_ok, db_term);
	} else {
	    enif_release_resource(rdb);
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

ERL_NIF_TERM close_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource* rdb;
    unordered_set<void*> *set;

    it_obj_resource *rit;
    /*get db_ptr resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    else{
	set = rdb->link_set;
	while (!set->empty()){
	    auto elem = set->begin();
	    rit = (it_obj_resource *) *elem;
	    delete_rit( rit );
	}

	delete_db(rdb);
	return atom_ok;
    }
}

ERL_NIF_TERM get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    opt_obj_resource* ropts;
    rocksdb::ReadOptions* readoptions;
    ErlNifBinary binkey;
    rocksdb::PinnableSlice value;
    ErlNifBinary binvalue;
    ERL_NIF_TERM value_term;

    /* get db_ptr resource */
    if (argc != 3 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get readoptions resource */
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }

    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /* get key resource */
    if (!enif_inspect_binary(env, argv[2], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "key"));
    }

    rocksdb::Slice key((const char*)binkey.data, (size_t) binkey.size);
    rocksdb::Status status = Get(rdb, readoptions, 0, &key, &value);

    if (status.ok()) {
	enif_alloc_binary(value.size(), &binvalue);
	memcpy(binvalue.data, value.data(), value.size());
	value_term = enif_make_binary(env, &binvalue);
	/* not calling enif_release_binary since enif_make_binary transfers ownership */
	return enif_make_tuple2(env, atom_ok, value_term);
    }
    else {
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM put_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    opt_obj_resource* wopts;
    rocksdb::WriteOptions* writeoptions;
    ErlNifBinary binkey;
    ErlNifBinary binvalue;

    /* get db_ptr resource */
    if (argc != 5 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get writeoptions resource */
    if (!enif_get_resource(env, argv[1], writeoptionResource, (void **) &wopts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "writeoptions"));
    }

    writeoptions = (rocksdb::WriteOptions *) wopts->object;

    /* get key resource */
    if (!enif_inspect_binary(env, argv[2], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "key"));
    }

    /*get value resource*/
    if (!enif_inspect_binary(env, argv[3], &binvalue)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "value"));
    }

    /* get indexed terms resource */
    ERL_NIF_TERM tuple_list = argv[4];
    ERL_NIF_TERM terms_list;
    ErlNifBinary binterm;

    ERL_NIF_TERM head, tail, th, tt;
    const ERL_NIF_TERM* tuple;
    int arity;
    std::vector<std::pair<Term, std::vector<Term>>> indices;
    while(enif_get_list_cell(env, tuple_list, &head, &tail)){
	if(enif_get_tuple(env, head, &arity, &tuple)) {
	    if( arity != 2) { return enif_make_badarg(env); }
	    /*get cid bytes*/
	    Term cid;
	    if( enif_inspect_binary(env, tuple[0], &binterm) ) {
		cid.data = (const char*) binterm.data;
		cid.size = (size_t) binterm.size;
	    } else { return enif_make_badarg(env); }
	    /*get terms*/
	    std::vector<Term> terms;
	    terms_list = tuple[1];
	    while(enif_get_list_cell(env, terms_list, &th, &tt)){
		if(enif_inspect_binary(env, th, &binterm)) {
		    Term t;
		    t.data = (const char*) binterm.data;
		    t.size = (size_t) binterm.size;
		    terms.push_back( t );
		} else { return enif_make_badarg(env); }
		terms_list = tt;
	    }
	    indices.push_back( std::make_pair (cid, terms) );
	}
	else { return enif_make_badarg(env); }
	tuple_list = tail;
    }

    rocksdb::Slice key((const char*)binkey.data, (size_t) binkey.size);
    rocksdb::Slice value((const char*)binvalue.data, (size_t) binvalue.size);

    rocksdb::Status status;
    if (indices.empty()) {
	status = Put(rdb, writeoptions, &key, &value);
    } else {
	status = PutTerms(rdb, writeoptions, &key, &value, indices);
    }

    if (status.ok()) {
	return atom_ok;
    }
    else {
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM delete_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    opt_obj_resource* wopts;
    rocksdb::WriteOptions* writeoptions;
    ErlNifBinary binkey;

    /* get db_ptr resource */
    if (argc != 4 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get writeoptions resource */
    if (!enif_get_resource(env, argv[1], writeoptionResource, (void **) &wopts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "writeoptions"));
    }

    writeoptions = (rocksdb::WriteOptions *) wopts->object;

    /* get key resource */
    if (!enif_inspect_binary(env, argv[2], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "key"));
    }

    /* get indexed cid resource */
    ERL_NIF_TERM cid_list = argv[3];
    ErlNifBinary binterm;
    ERL_NIF_TERM head, tail;
    std::vector<Term> cids;
    while(enif_get_list_cell(env, cid_list, &head, &tail)){
	/*get cid bytes*/
	if(enif_inspect_binary(env, head, &binterm)) {
	    Term cid;
	    cid.data = (const char*) binterm.data;
	    cid.size = (size_t) binterm.size;
	    cids.push_back( cid );
	} else { return enif_make_badarg(env); }
	cid_list = tail;
    }

    rocksdb::Slice key((const char*)binkey.data, (size_t) binkey.size);
    rocksdb::Status status;
    if (cids.empty()) {
	status = Delete(rdb, writeoptions, &key);
    } else {
	status = DeleteTerms(rdb, writeoptions, &key, cids);
    }

    if (status.ok()) {
	return atom_ok;
    }
    else {
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    opt_obj_resource* wopts;
    rocksdb::WriteOptions *writeoptions;
    unsigned int delete_keys_size;
    unsigned int put_kvs_size;

    ERL_NIF_TERM head, tail;
    ErlNifBinary bin;
    ERL_NIF_TERM delete_list = argv[2];
    ERL_NIF_TERM put_list = argv[3];

    /*get db_ptr resource*/
    if (argc != 4 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    /* get writeoptions resource */
    if (!enif_get_resource(env, argv[1], writeoptionResource, (void **) &wopts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "writeoptions"));
    }

    writeoptions = (rocksdb::WriteOptions *) wopts->object;

    /* get delete keys resource */
    if (!enif_get_list_length(env, delete_list, &delete_keys_size)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "delete_ks"));
    }

    /* get put key/values resource */
    if (!enif_get_list_length(env, put_list, &put_kvs_size)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "put_kvs"));
    }

    vector<rocksdb::Slice> delete_keys;
    vector<rocksdb::Slice> put_keys;
    vector<rocksdb::Slice> put_values;

    while(enif_get_list_cell(env, delete_list, &head, &tail)) {
	if(!enif_inspect_binary(env, head, &bin)) {
	    return enif_make_badarg(env);
	}
	rocksdb::Slice key((const char*)bin.data, (size_t) bin.size);
	delete_keys.push_back(key);
	delete_list = tail;
    }

    int arity;
    const ERL_NIF_TERM* put_kv_array;
    while (enif_get_list_cell(env, put_list, &head, &tail)) {
	if (!enif_get_tuple(env, head, &arity, &put_kv_array)) {
	    return enif_make_badarg(env);
	}
	if (arity != 2 || !enif_inspect_binary(env, put_kv_array[0], &bin)) {
	    return enif_make_badarg(env);
	}
	rocksdb::Slice key((const char*)bin.data, (size_t) bin.size);
	put_keys.push_back(key);
	if (!enif_inspect_binary(env, put_kv_array[1], &bin)) {
	    return enif_make_badarg(env);
	}
	rocksdb::Slice value((const char*)bin.data, (size_t) bin.size);
	put_values.push_back(value);
	put_list = tail;
    }

    rocksdb::WriteBatch batch;

    while (!delete_keys.empty()) {
	batch.Delete(delete_keys.back());
	delete_keys.pop_back();
    }

    while (!put_keys.empty()) {
	batch.Put(put_keys.back(), put_values.back());
	put_keys.pop_back();
	put_values.pop_back();
    }

    rocksdb::Status status = Write(rdb, writeoptions, &batch);

    if(status.ok()){
	return atom_ok;
    }
    else{
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM index_get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    opt_obj_resource* ropts;
    rocksdb::ReadOptions* readoptions;
    ErlNifBinary binkey;
    rocksdb::PinnableSlice value;
    ErlNifBinary binvalue;
    ERL_NIF_TERM value_term;

    /* get db_ptr resource */
    if (argc != 3 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get readoptions resource */
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }

    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /* get key resource */
    if (!enif_inspect_binary(env, argv[2], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "key"));
    }

    rocksdb::Slice key((const char*)binkey.data, (size_t) binkey.size);
    rocksdb::Status status = Get(rdb, readoptions, 2, &key, &value);

    if (status.ok()) {
	enif_alloc_binary(value.size(), &binvalue);
	memcpy(binvalue.data, value.data(), value.size());
	value_term = enif_make_binary(env, &binvalue);
	/* not calling enif_release_binary since enif_make_binary transfers ownership */
	return enif_make_tuple2(env, atom_ok, value_term);
    }
    else {
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM delete_indices_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    rocksdb::DB* db;
    /* get db_ptr resource */
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    //Cast resource to rocksdb::DB*, we are sure it is not a ttl db.
    db = static_cast<rocksdb::DB*>(rdb->object);
    /* get cids resource */
    ERL_NIF_TERM cid_list = argv[1];
    ERL_NIF_TERM head, tail;
    ErlNifBinary cidterm;
    std::vector<std::string> cids;
    while(enif_get_list_cell(env, cid_list, &head, &tail)){
	if( enif_inspect_binary(env, head, &cidterm) ) {
	    std::string cid((const char*) cidterm.data, (size_t) cidterm.size);
	    cids.push_back( cid );
	} else { return enif_make_badarg(env); }
	cid_list = tail;
    }
    rocksdb::ReadOptions ro;
    rocksdb::WriteOptions wo;
    for (auto cid_it = cids.begin(); cid_it != cids.end(); ++cid_it) {
	std::string cid = *cid_it;
	//Define the range (begin, end) to be removed.
	unsigned char bytes[4] = {0xff, 0xff, 0xff, 0xff};
	std::string end_string = cid;
	end_string.append((const char*)bytes, 4);
	rocksdb::Slice begin(cid);
	rocksdb::Slice end(end_string);
	auto handles = *(rdb->handles);
	std::vector<rocksdb::ColumnFamilyHandle*> ix_handles(handles.begin()+1, handles.end());
	//First delete files in range to reclaim the space faster.
	rocksdb::DeleteFilesInRange(db, ix_handles.at(0), &begin, &end);
	rocksdb::DeleteFilesInRange(db, ix_handles.at(1), &begin, &end);
	//Then delete remaining entries on other sst files one by one
	/*Create rocksdb iterators*/
	std::vector<rocksdb::Iterator*> iterators;
	db->NewIterators(ro, ix_handles, &iterators);
	int cf = 0;
	for (auto it : iterators) {
	    for (it->Seek(begin);
		 it->Valid() && it->key().starts_with(begin);
		 it->Next()) {
		db->Delete(wo, ix_handles.at(cf), it->key());
	    }
	    cf++;
	}
	for (auto del_it : iterators) { delete del_it; }
	//Last, compact the range to reclaim space.
	db->CompactRange(rocksdb::CompactRangeOptions(), ix_handles.at(0), &begin, &end);
	db->CompactRange(rocksdb::CompactRangeOptions(), ix_handles.at(1), &begin, &end);
    }
    return atom_ok;
}

/*Resource making*/
ERL_NIF_TERM lru_cache_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    lru_obj_resource *opts;
    uint64_t size;
    ERL_NIF_TERM term;

    // pointer to shared_ptr
    std::shared_ptr<rocksdb::Cache>* lru_cache;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if (!enif_get_uint64(env, argv[0], (unsigned long *)&size)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "size"));
    }

    // memory freed by erlang destructor for the resource
    // and if rocksdb doesn't have any referece to the cache
    // the cache is destructed (shared_ptr ref count)
    lru_cache = new std::shared_ptr<rocksdb::Cache>;

    // size is MB convert to bytes
    size *= 1024 * 1024;
    *lru_cache = rocksdb::NewLRUCache(size, 80.0, false);
    opts = (lru_obj_resource*) enif_alloc_resource(lruResource, sizeof(lru_obj_resource));
    opts->object = lru_cache;

    term = enif_make_resource(env, opts);
    enif_release_resource(opts);

    return enif_make_tuple2(env, atom_ok, term);
}

/* get shared_ptr reference count  */
ERL_NIF_TERM get_lru_cache_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    lru_obj_resource *lru;
    ERL_NIF_TERM count_term;

    // pointer to shared_ptr
    std::shared_ptr<rocksdb::Cache>* lru_cache;

    if (argc != 1) {
	return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], lruResource, (void **)&lru)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "lru_cache"));
    }

    lru_cache = (std::shared_ptr<rocksdb::Cache>*) lru->object;
    count_term = enif_make_uint(env, (unsigned int) lru_cache->use_count());
    return enif_make_tuple2(env, atom_ok, count_term);
}

ERL_NIF_TERM options_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;

    /*kvl: the pointer to list of key/value pairs passed from erlang*/
    ERL_NIF_TERM kvl = argv[0];
    unsigned int kvl_len;
    char key[MAXPATHLEN];
    char value[MAXPATHLEN];

    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* tuple;
    int arity;

    opt_obj_resource *opts;
    rocksdb::DBOptions *options = new rocksdb::DBOptions;

    if (argc != 1 || !enif_get_list_length(env, kvl, &kvl_len)) {
	return enif_make_badarg(env);
    }

    unordered_map<string, string> db_options_map;
    db_options_map.reserve(kvl_len);

    while(enif_get_list_cell(env, kvl, &head, &tail)){
	if(!enif_get_tuple(env, head, &arity, &tuple)) {
	    return enif_make_badarg(env);
	}
	if(arity != 2 || !enif_get_string(env, tuple[0], key, sizeof(key), ERL_NIF_LATIN1)) {
	    return enif_make_badarg(env);
	}
	if(!enif_get_string(env, tuple[1], value, sizeof(value), ERL_NIF_LATIN1)) {
	    return enif_make_badarg(env);
	}
	pair<string, string> p ((const char*)key, (const char*)value);
	db_options_map.insert( p );
        kvl = tail;
    }

    rocksdb::Status status = rocksdb::GetDBOptionsFromMap(rocksdb::DBOptions(), db_options_map, options, true);

    opts = (opt_obj_resource*) enif_alloc_resource(optionResource, sizeof(opt_obj_resource));
    opts->object = options;

    /* if status is OK then return {ok, term} */
    if (status.ok()) {
	term = enif_make_resource(env, opts);
	enif_release_resource(opts);
	return enif_make_tuple2(env, atom_ok, term);
    } else {
	enif_release_resource(opts);
	ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	return status_tuple;
    }
}

ERL_NIF_TERM readoptions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    int arity;
    const ERL_NIF_TERM* readoptions_array;
    int result;
    rocksdb::ReadOptions *readoptions = NULL;
    opt_obj_resource *ropts;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &readoptions_array))
	return enif_make_badarg(env);

    ropts = (opt_obj_resource*) enif_alloc_resource(readoptionResource, sizeof(opt_obj_resource));

    result = init_readoptions(env, readoptions_array, &readoptions);
    ropts->object = readoptions;


    /*if result is 0 then return {ok, term}*/
    if (result == 0) {
	term = enif_make_resource(env, ropts);
	enif_release_resource(ropts);
	return enif_make_tuple2(env, atom_ok, term);
    }

    enif_release_resource(ropts);
    return enif_make_badarg(env);
}

ERL_NIF_TERM writeoptions_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term;
    int arity;
    const ERL_NIF_TERM* writeoptions_array;
    int result;
    rocksdb::WriteOptions *writeoptions = NULL;
    opt_obj_resource *wopts;

    if (argc != 1 || !enif_get_tuple(env, argv[0], &arity, &writeoptions_array)) {
	return enif_make_badarg(env);
    }

    wopts = (opt_obj_resource*) enif_alloc_resource(writeoptionResource, sizeof(opt_obj_resource));

    result = init_writeoptions(env, writeoptions_array, &writeoptions);
    wopts->object = writeoptions;


    /*if result is 0 then return {ok, term}*/
    if (result == 0) {
	term = enif_make_resource(env, wopts);
	enif_release_resource(wopts);
	return enif_make_tuple2(env, atom_ok, term);
    }

    enif_release_resource(wopts);
    return enif_make_badarg(env);
}

/*rocksdb destroy*/
ERL_NIF_TERM destroy_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    opt_obj_resource *opts;
    rocksdb::DBOptions *options;

    /* get path */
    if (argc != 2 || enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) < 1) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "path"));
    }
    /* get options resource */
    if (!enif_get_resource(env, argv[1], optionResource, (void **) &opts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "options"));
    }
    options = (rocksdb::DBOptions *) opts->object;
    auto cf_opts= rocksdb::ColumnFamilyOptions();
    auto destroy_options = rocksdb::Options(*options, cf_opts);
    rocksdb::Status status = rocksdb::DestroyDB(path, destroy_options);

    if (status.ok()) {
	return atom_ok;
    }

    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
    return status_tuple;
}

/*rocksdb repair*/
ERL_NIF_TERM repair_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    opt_obj_resource *opts;
    rocksdb::DBOptions* options;

    /* get path */
    if(argc != 2 || enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "path"));
    }

    /* get options resource */
    if (!enif_get_resource(env, argv[1], optionResource, (void **) &opts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "options"));
    }
    options = (rocksdb::DBOptions *) opts->object;
    rocksdb::Status status;
    auto* opt = reinterpret_cast<rocksdb::Options*>(options);
    status = RepairDB(path, *opt);

    if (status.ok())
	return atom_ok;

    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
    return status_tuple;
}

ERL_NIF_TERM approximate_sizes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    unsigned int ranges_size;

    ERL_NIF_TERM head, tail;
    ErlNifBinary bin;

    ERL_NIF_TERM range_list = argv[1];

    /*get db_ptr resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get ranges resource*/
    if (!enif_get_list_length(env, range_list, &ranges_size)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "ranges"));
    }
    vector <rocksdb::Range>  ranges;
    ranges.reserve( ranges_size );

    int arity;
    const ERL_NIF_TERM* range_array;
    while(enif_get_list_cell(env, range_list, &head, &tail)) {
	if(!enif_get_tuple(env, head, &arity, &range_array)) {
	    return enif_make_badarg(env);
	}
	if(arity != 2 || !enif_inspect_binary(env, range_array[0], &bin)) {
	    return enif_make_badarg(env);
	}
	rocksdb::Slice start((const char*)bin.data, (size_t) bin.size);
	if(!enif_inspect_binary(env, range_array[1], &bin)) {
	    return enif_make_badarg(env);
	}
	rocksdb::Slice limit((const char*)bin.data, (size_t) bin.size);
	ranges.push_back( rocksdb::Range(start, limit) );
	range_list = tail;
    }

    uint64_t sizes[ranges_size];

    GetApproximateSizes(rdb, &ranges[0], ranges_size, sizes);

    ERL_NIF_TERM size_list = enif_make_list(env, 0);
    while (ranges_size > 0) {
	ranges_size--;
	size_list = enif_make_list_cell(env, enif_make_uint64(env, sizes[ranges_size]), size_list);
    }

    return enif_make_tuple2(env, atom_ok, size_list);
}

ERL_NIF_TERM approximate_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource* ropts;
    rocksdb::ReadOptions* readoptions;
    db_obj_resource *rdb;

    /*get db_ptr resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    rocksdb::Iterator* it = NewIterator(rdb, readoptions);
    it->SeekToFirst();
    if ( it->Valid() ){
	string start = it->key().ToString();
	it->SeekToLast();
	string end = it->key().ToString();
	rocksdb::Range ranges[1];
	ranges[0] = rocksdb::Range(start, end);
	uint64_t size[1];

	GetApproximateSizes(rdb, ranges, 1, size);

	delete it;
	return enif_make_tuple2(env, atom_ok,
				enif_make_uint64(env, size[0]));
    }

    delete it;
    return enif_make_tuple2(env, atom_ok,
			    enif_make_uint64(env, 0));
}

ERL_NIF_TERM read_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource *opts;
    rocksdb::DBOptions *options;
    opt_obj_resource *ropts;
    rocksdb::ReadOptions *readoptions;
    db_obj_resource *rdb;
    int arity;
    const ERL_NIF_TERM* range_array;
    ErlNifBinary bin;

    int max_keys;

    /*get db_ptr resource*/
    if (argc != 5 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    auto comparator = rdb->cfd_options->comparator;
    /*get options resource*/
    if (!enif_get_resource(env, argv[1], optionResource, (void **) &opts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "options"));
    }
    options = (rocksdb::DBOptions *) opts->object;

    /*get readoptions resource*/
    if (!enif_get_resource(env, argv[2], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /*get range tuple*/
    if (!enif_get_tuple(env, argv[3], &arity, &range_array)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "range"));
    }

    /*get limit integer*/
    if (!enif_get_int(env, argv[4], &max_keys)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "limit"));
    }

    /*Read range tuple into start and limit slices*/
    if(arity != 2 || !enif_inspect_binary(env, range_array[0], &bin)) {
	return enif_make_badarg(env);
    }
    rocksdb::Slice start((const char*)bin.data, (size_t) bin.size);

    if(!enif_inspect_binary(env, range_array[1], &bin)) {
	return enif_make_badarg(env);
    }
    rocksdb::Slice limit((const char*)bin.data, (size_t) bin.size);

    /*Create rocksdb iterator*/
    rocksdb::Iterator* it = NewIterator(rdb, readoptions);

    /*Create empty list to store key/value pairs*/
    //ERL_NIF_TERM kvl = enif_make_list(env, 0);
    ERL_NIF_TERM kvl;

    /*Declare key and value erlang resources*/
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*declare vector to keep key/value pairs*/
    vector<ERL_NIF_TERM> kvl_vector;

    /*Iterate through start to limit*/
    int i = 0;
    for (it->Seek(start);
	 it->Valid() && ( comparator->Compare( it->key(), limit ) <= 0 ) && i < max_keys;
	 it->Next()) {

	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);
	/*Construct value_term*/
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	//kvl = enif_make_list_cell(env, enif_make_tuple2(env, key_term, value_term), kvl);
	kvl_vector.push_back(enif_make_tuple2(env, key_term, value_term));
	i++;
    }
    ERL_NIF_TERM cont;

    if ( i == max_keys && it->Valid() && comparator->Compare( it->key(), limit ) <= 0 ){
	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	cont = enif_make_binary(env, &binkey);
    }
    else {
	cont = atom_complete;
    }

    delete it;
    kvl = enif_make_list_from_array(env, &kvl_vector[0], kvl_vector.size());
    return enif_make_tuple3(env, atom_ok, kvl, cont);
}

ERL_NIF_TERM read_range_n_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource *ropts;
    rocksdb::ReadOptions *readoptions;
    db_obj_resource *rdb;

    ErlNifBinary binkey;

    int max_keys;

    /*get db_ptr resource*/
    if (argc != 4 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /*get range start key*/
    if (!enif_inspect_binary(env, argv[2], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "start_key"));
    }

    /*get limit integer*/
    if (!enif_get_int(env, argv[3], &max_keys)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "limit"));
    }

    rocksdb::Slice start((const char*)binkey.data, (size_t) binkey.size);

    /*Create rocksdb iterator*/
    rocksdb::Iterator* it = NewIterator(rdb, readoptions);

    /*Create empty list to store key/value pairs*/
    ERL_NIF_TERM kvl;

    /*Declare key and value erlang resources*/
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*declare vector to keep key/value pairs*/
    vector<ERL_NIF_TERM> kvl_vector;

    /*Iterate through start to limit*/
    int i = 0;
    for (it->Seek(start);
	 it->Valid() && i < max_keys;
	 it->Next()) {

	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);
	/*Construct value_term*/
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	kvl_vector.push_back(enif_make_tuple2(env, key_term, value_term));
	i++;
    }

    delete it;
    kvl = enif_make_list_from_array(env, &kvl_vector[0], kvl_vector.size());
    return enif_make_tuple2(env, atom_ok, kvl);
}

ERL_NIF_TERM
read_range_prefix_stop_n_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource *ropts;
    rocksdb::ReadOptions *readoptions;
    db_obj_resource *rdb;

    ErlNifBinary prefixkey;
    ErlNifBinary binkey;

    int max_keys;
    int i = 0;

    /*get db_ptr resource*/
    if (argc != 6  || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /*get prefix key*/
    if (!enif_inspect_binary(env, argv[2], &prefixkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "prefix_key"));
    }

    /*get range start key*/
    if (!enif_inspect_binary(env, argv[3], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "start_key"));
    }
    rocksdb::Slice start((const char*)binkey.data, (size_t) binkey.size);

    /*get range stop key if present*/
    if (!enif_inspect_binary(env, argv[4], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "stop_key"));
    }
    rocksdb::Slice stop((const char*)binkey.data, (size_t) binkey.size);

    /*get limit integer*/
    if (!enif_get_int(env, argv[5], &max_keys)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "limit"));
    }

    /*Create rocksdb iterator*/
    rocksdb::Iterator* it = NewIterator(rdb, readoptions);

    /*Create rocksdb halt iterator*/
    rocksdb::Iterator* halt_it = NewIterator(rdb, readoptions);

    /*Create empty list to store key/value pairs*/
    ERL_NIF_TERM kvl;
    /* Continuation or atom complete */
    ERL_NIF_TERM cont;

    /*Declare key and value erlang resources*/
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*declare vector to keep key/value pairs*/
    vector<ERL_NIF_TERM> kvl_vector;

    /*Iterate from start to limit*/
    for (it->Seek(start), halt_it->Seek(stop), i=0;
	 it->Valid() && i < max_keys && it->key() != halt_it->key();
	 it->Next()) {

	/* stop if key is not prefixed */
	/* prefixkey.data+5 since we are conding tuples 1 byte type; 4 bytes size */
	if (memcmp(prefixkey.data+5, it->key().data()+5, min(it->key().size()-5, prefixkey.size-5))) {
	    // no more data to read
	    i = -1;
	    break;
	}

	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);
	/*Construct value_term*/
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	kvl_vector.push_back(enif_make_tuple2(env, key_term, value_term));
	i++;
    }

    if (i == max_keys && it->Valid() &&
	/* prefixkey.size+5 since sext encoding tuples are 1 byte type; 4 bytes size */
	!memcmp(it->key().data()+5, prefixkey.data+5, min(it->key().size()-5, prefixkey.size-5)) ) {
	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	cont = enif_make_binary(env, &binkey);
    }
    else {
	cont = atom_complete;
    }

    delete it;
    delete halt_it;

    kvl = enif_make_list_from_array(env, &kvl_vector[0], kvl_vector.size());
    return enif_make_tuple3(env, atom_ok, kvl, cont);
}

ERL_NIF_TERM
read_range_prefix_n_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource *ropts;
    rocksdb::ReadOptions *readoptions;
    db_obj_resource *rdb;

    ErlNifBinary prefixkey;
    ErlNifBinary binkey;

    int max_keys;
    int i = 0;

    /*get db_ptr resource*/
    if (argc != 5  || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get readoptions resource*/
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /*get prefix key*/
    if (!enif_inspect_binary(env, argv[2], &prefixkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "prefix_key"));
    }

    /*get range start key*/
    if (!enif_inspect_binary(env, argv[3], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "start_key"));
    }
    rocksdb::Slice start((const char*)binkey.data, (size_t) binkey.size);

    /*get limit integer*/
    if (!enif_get_int(env, argv[4], &max_keys)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "limit"));
    }

    /*Create rocksdb iterator*/
    rocksdb::Iterator* it = NewIterator(rdb, readoptions);

    /*Create empty list to store key/value pairs*/
    ERL_NIF_TERM kvl;
    /* Continuation or atom complete */
    ERL_NIF_TERM cont;

    /*Declare key and value erlang resources*/
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*declare vector to keep key/value pairs*/
    vector<ERL_NIF_TERM> kvl_vector;

    /*Iterate from start to limit*/
    for (it->Seek(start);
	 it->Valid() && i < max_keys;
	 it->Next()) {

	/* stop if key is not prefixed */
	/* prefixkey.data+5 since sext is ending the tuple with type 1 byte & size 4 bytes */
	if (memcmp(prefixkey.data+5, it->key().data()+5, min(it->key().size()-5, prefixkey.size-5))) {
	    // no more data to read
	    i = -1;
	    break;
	}

	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);
	/*Construct value_term*/
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	kvl_vector.push_back(enif_make_tuple2(env, key_term, value_term));
	i++;
    }

    if (i == max_keys && it->Valid() &&
	/* prefixkey.data+5 since sext is ending tuple with type 1 byte and size 4 bytes
	   which shouldn't be compared */
	!memcmp(it->key().data()+5, prefixkey.data+5, min(it->key().size()-5, prefixkey.size-5)) ) {
	/*Construct key_term*/
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	cont = enif_make_binary(env, &binkey);
    }
    else {
	cont = atom_complete;
    }

    delete it;
    kvl = enif_make_list_from_array(env, &kvl_vector[0], kvl_vector.size());
    return enif_make_tuple3(env, atom_ok, kvl, cont);
}

ERL_NIF_TERM iterator_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    opt_obj_resource *ropts;
    rocksdb::ReadOptions *readoptions;
    rocksdb::Iterator *it;
    db_obj_resource *rdb;
    it_obj_resource *rit;
    ERL_NIF_TERM it_term;

    unordered_set<void*> *set;
    mutex *mtx = new mutex;

    /*get db_ptr resource*/
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get readoptions resource */
    if (!enif_get_resource(env, argv[1], readoptionResource, (void **) &ropts)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "readoptions"));
    }
    readoptions = (rocksdb::ReadOptions *) ropts->object;

    /*Create rocksdb iterator*/
    rit = (it_obj_resource*)  enif_alloc_resource(iteratorResource, sizeof(it_obj_resource));

    it = NewIterator(rdb, readoptions);
    rit->mtx = mtx;
    rit->allocated = 1;
    rit->object = it;

    rit->linked_obj = rdb;

    set = rdb->link_set;
    rdb->mtx->lock();
    set->insert(rit);
    rdb->mtx->unlock();

    it_term = enif_make_resource(env, rit);
    enif_release_resource(rit);

    rocksdb::Status status = it->status();
    if(status.ok()){
	/* resource now only owned by "Erlang" */
	return enif_make_tuple2(env, atom_ok, it_term);
    }

    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
    return status_tuple;
}

ERL_NIF_TERM delete_iterator_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;

    /*get it_ptr resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], iteratorResource, (void **) &rit)) {
	return enif_make_badarg(env);
    }
    else{
	delete_rit( rit );
	return atom_ok;
    }
}

ERL_NIF_TERM first_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;
    rocksdb::Iterator *it;
    /* Declare key and value erlang resources */
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*get it_ptr resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], iteratorResource, (void **)&rit)) {
	return enif_make_badarg(env);
    }
    it = (rocksdb::Iterator*) rit->object;

    /*Check if resource is still allocated*/
    if(!rit->allocated) {
	return enif_make_badarg(env);
    }

    it->SeekToFirst();
    if(it->Valid()) {
	/* Construct key_term */
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);

	/* Construct value_term */
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	return enif_make_tuple2(env, atom_ok, enif_make_tuple2(env, key_term, value_term));
    }

    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "invalid"));
}

ERL_NIF_TERM last_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;
    rocksdb::Iterator* it;
    /* Declare key and value erlang resources */
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /* get it_ptr resource */
    if (argc != 1 || !enif_get_resource(env, argv[0], iteratorResource, (void **) &rit)) {
	return enif_make_badarg(env);
    }

    it = (rocksdb::Iterator*) rit->object;

    /*Check if resource is still allocated*/
    if(!rit->allocated) {
	return enif_make_badarg(env);
    }

    it->SeekToLast();
    if (it->Valid()) {
	/* Construct key_term */
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);
	/* Construct value_term */
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	return enif_make_tuple2(env, atom_ok, enif_make_tuple2(env, key_term, value_term));
    }

    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "invalid"));
}

ERL_NIF_TERM seek_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;
    rocksdb::Iterator* it;

    /* Declare key and value erlang resources */
    ErlNifBinary binkey, binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;


    /* get it_ptr resource */
    if (argc != 2 || !enif_get_resource(env, argv[0], iteratorResource, (void **) &rit)) {
	return enif_make_badarg(env);
    }
    it = (rocksdb::Iterator*) rit->object;

    /*Check if resource is still allocated*/
    if(!rit->allocated) {
	return enif_make_badarg(env);
    }

    /*get key resource*/
    if (!enif_inspect_binary(env, argv[1], &binkey)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "key"));
    }

    rocksdb::Slice start((const char*)binkey.data, (size_t) binkey.size);

    it->Seek(start);
    if( it->Valid() ){
	/* Construct key_term */
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);

	/* Construct value_term */
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	return enif_make_tuple2(env, atom_ok, enif_make_tuple2(env, key_term, value_term));
    }

    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "invalid"));
}

ERL_NIF_TERM next_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;
    rocksdb::Iterator *it;
    /* Declare key and value erlang resources */
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*get it_ptr resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], iteratorResource, (void **) &rit)) {
	return enif_make_badarg(env);
    }
    it = (rocksdb::Iterator*) rit->object;

    /*Check if resource is still allocated*/
    if(!rit->allocated) {
	return enif_make_badarg(env);
    }

    it->Next();
    if (it->Valid()) {
	/* Construct key_term */
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);

	/* Construct value_term */
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	return enif_make_tuple2(env, atom_ok, enif_make_tuple2(env, key_term, value_term));
    }

    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "invalid"));
}


ERL_NIF_TERM prev_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    it_obj_resource *rit;
    rocksdb::Iterator *it;
    /*Declare key and value erlang resources*/
    ErlNifBinary binkey;
    ErlNifBinary binvalue;
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*get resource*/
    if (argc != 1 || !enif_get_resource(env, argv[0], iteratorResource, (void **) &rit)) {
	return enif_make_badarg(env);
    }
    it = (rocksdb::Iterator*) rit->object;

    /*Check if resource is still allocated*/
    if(!rit->allocated) {
	return enif_make_badarg(env);
    }

    it->Prev();
    if (it->Valid()) {
	/* Construct key_term */
	enif_alloc_binary(it->key().size(), &binkey);
	memcpy(binkey.data, it->key().data(), it->key().size());
	key_term = enif_make_binary(env, &binkey);

	/* Construct value_term */
	enif_alloc_binary(it->value().size(), &binvalue);
	memcpy(binvalue.data, it->value().data(), it->value().size());
	value_term = enif_make_binary(env, &binvalue);

	return enif_make_tuple2(env, atom_ok, enif_make_tuple2(env, key_term, value_term));
    }

    return enif_make_tuple2(env, atom_error, enif_make_atom(env, "invalid"));
}

ERL_NIF_TERM compact_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    /* get db_ptr resource */
    if (argc != 1 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    CompactDB(rdb);
    return atom_ok;
}

ERL_NIF_TERM compact_index_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;
    /* get db_ptr resource */
    if (argc != 1 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }
    CompactIndex(rdb);
    return atom_ok;
}

ERL_NIF_TERM backup_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    db_obj_resource *rdb;

    /* get db_ptr resource */
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get path*/
    if(enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "path"));
    }
    else{
	rocksdb::Status status = BackupDB(rdb, path);
	if(status.ok()){
	    return atom_ok;
	}
	else{
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

ERL_NIF_TERM get_backup_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];

    /* get path  */
    if (argc != 1 || enif_get_string(env, argv[0], path, sizeof(path), ERL_NIF_LATIN1) < 1) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "bkp_path"));
    }
    else{
	const string path_string(path);
	rocksdb::BackupEngine* backup_engine;
	rocksdb::Status status = rocksdb::BackupEngine::Open(rocksdb::Env::Default(), rocksdb::BackupableDBOptions(path_string), &backup_engine);

	if(status.ok()) {
	    std::vector<rocksdb::BackupInfo> backup_info;
	    backup_engine->GetBackupInfo(&backup_info);

	    ERL_NIF_TERM list;
	    ERL_NIF_TERM id_term;
	    ERL_NIF_TERM ts_term;
	    ERL_NIF_TERM size_term;
	    ERL_NIF_TERM number_files_term;
	    ERL_NIF_TERM app_metadata_term;
	    vector<ERL_NIF_TERM> info_tuples;
	    size_t str_len;
	    for(auto const& info: backup_info) {
		id_term = enif_make_uint(env, (unsigned int) info.backup_id);
		ts_term = enif_make_int64(env, (ErlNifSInt64) info.timestamp);
		size_term = enif_make_uint64(env, (ErlNifUInt64) info.size);
		number_files_term = enif_make_uint(env, (unsigned int) info.number_files);
		str_len = info.app_metadata.length();
		char * cstr = new char [str_len+1];
		strcpy (cstr, info.app_metadata.c_str());
		app_metadata_term = enif_make_string_len(env, cstr, str_len, ERL_NIF_LATIN1);
		info_tuples.push_back(enif_make_tuple5(env, id_term, ts_term, size_term, number_files_term, app_metadata_term));
		delete[] cstr;
	    }
	    list = enif_make_list_from_array(env, &info_tuples[0], info_tuples.size());
	    return enif_make_tuple2(env, atom_ok, list);
	}
	else{
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

ERL_NIF_TERM restore_db_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char bkp_path[MAXPATHLEN];
    char db_path[MAXPATHLEN];
    char wal_path[MAXPATHLEN];

    /* get bkp path  */
    if (argc != 3 || enif_get_string(env, argv[0], bkp_path, sizeof(bkp_path), ERL_NIF_LATIN1) < 1) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "bkp_path"));
    }
    /*get db path*/
    if(enif_get_string(env, argv[1], db_path, sizeof(db_path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "db_path"));
    }
    /*get wal path*/
    if(enif_get_string(env, argv[2], wal_path, sizeof(wal_path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "wal_path"));
    }
    else{
	rocksdb::Status status = RestoreDB(bkp_path, db_path, wal_path);
	if(status.ok()){
	    return atom_ok;
	}
	else{
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

ERL_NIF_TERM restore_db_by_id_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char bkp_path[MAXPATHLEN];
    char db_path[MAXPATHLEN];
    char wal_path[MAXPATHLEN];
    int backup_id;
    /* get bkp path  */
    if (argc != 4 || enif_get_string(env, argv[0], bkp_path, sizeof(bkp_path), ERL_NIF_LATIN1) < 1) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "bkp_path"));
    }
    /*get db path*/
    if(enif_get_string(env, argv[1], db_path, sizeof(db_path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "db_path"));
    }
    /*get wal path*/
    if(enif_get_string(env, argv[2], wal_path, sizeof(wal_path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "wal_path"));
    }

    /*get backup id*/
    if (!enif_get_int(env, argv[3], &backup_id)) {
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "backup_id"));
    }
    else{
	rocksdb::Status status = RestoreDB(bkp_path, db_path, wal_path, (uint32_t) backup_id);
	if(status.ok()){
	    return atom_ok;
	}
	else{
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

ERL_NIF_TERM create_checkpoint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char path[MAXPATHLEN];
    db_obj_resource *rdb;

    /* get db_ptr resource */
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /*get path*/
    if(enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1){
	return enif_make_tuple2(env, atom_error, enif_make_atom(env, "path"));
    }
    else{

	rocksdb::Status status = CreateCheckpoint(rdb, path);
	if(status.ok()){
	    return atom_ok;
	}
	else{
	    ERL_NIF_TERM status_tuple = make_status_tuple(env, &status);
	    return status_tuple;
	}
    }
}

/* get memory usage*/
ERL_NIF_TERM memory_usage_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    db_obj_resource *rdb;

    /* get db_ptr resource */
    if (argc != 1 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    return rocksdb_memory_usage(env, rdb);
}

/* get property */
ERL_NIF_TERM get_property_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char arg_str[MAXARGLEN];
    db_obj_resource *rdb;

    /* get db_ptr resource */
    if (argc != 2 || !enif_get_resource(env, argv[0], dbResource, (void **) &rdb)) {
	return enif_make_badarg(env);
    }

    /* get arg string */
    if(enif_get_string(env, argv[1], arg_str, sizeof(arg_str), ERL_NIF_LATIN1) < 1 ) {
	return enif_make_badarg(env);
    }

    return get_property(env, rdb, arg_str);
}


ErlNifFunc nif_funcs[] = {
    {"open_db", 3, open_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"open_db", 4, open_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"open_db", 5, open_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close_db", 1, close_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get", 3, get_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"put", 5, put_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"delete", 4, delete_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"write", 4, write_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"index_get", 3, index_get_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"delete_indices", 2, delete_indices_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"lru_cache", 1, lru_cache_nif},
    {"get_lru_cache", 1, get_lru_cache_nif},
    {"options", 1, options_nif},
    {"readoptions", 1, readoptions_nif},
    {"writeoptions", 1, writeoptions_nif},

    {"destroy_db", 2, destroy_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"repair_db", 2, repair_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"approximate_sizes", 2, approximate_sizes_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"approximate_size", 2, approximate_size_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_range", 5, read_range_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_range_n", 4, read_range_n_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_range_prefix_n", 5, read_range_prefix_n_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_range_prefix_stop_n", 6, read_range_prefix_stop_n_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"iterator", 2, iterator_nif},
    {"delete_iterator", 1, delete_iterator_nif},
    {"first", 1, first_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"last", 1, last_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"seek", 2, seek_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"next", 1, next_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"prev", 1, prev_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"compact_db", 1, compact_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"compact_index", 1, compact_index_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"backup_db", 2, backup_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get_backup_info", 1, get_backup_info_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"restore_db", 3, restore_db_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"restore_db", 4, restore_db_by_id_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"create_checkpoint", 2, create_checkpoint_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    
    {"set_ttl", 2, set_ttl_nif},

    {"resource_test", 0, resource_test_nif},

    {"memory_usage", 1, memory_usage_nif},
    {"get_property", 2, get_property_nif}

};
} /* anonymouse namespace ends */

ERL_NIF_INIT(rocksdb, nif_funcs, load, NULL, upgrade, NULL)
