#include <rocksdb/merge_operator.h>
#include "rocksdb_nif.h"
#include "erl_nif.h"
#include <algorithm>
#include <thread>

namespace rocksdb {
class IndexMerger : public MergeOperator {
    public:
	IndexMerger(ErlNifPid* pid, EnvBox *env_box)
	: pid_(pid), env_box_(env_box){
	    env_ = enif_alloc_env();
	    atom_index_update = enif_make_atom(env_, "index_update");
	    atom_undefined = enif_make_atom(env_, "undefined");
	  }

	~IndexMerger() {
	    enif_free_env(env_);
	}
	ERL_NIF_TERM atom_index_update;
	ERL_NIF_TERM atom_undefined;
	virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				 MergeOperationOutput* merge_out) const override {
	    update_term_index(merge_in.key, merge_in.operand_list);
	    if ( merge_in.operand_list.back().size() == 0 ) {
		return true;
	    }

	    merge_out->new_value.clear();
	    if ( merge_in.existing_value != nullptr ) {
		merge_out->new_value.assign(merge_in.existing_value->data(),
			merge_in.existing_value->size());
	    }
	    for ( const rocksdb::Slice& m : merge_in.operand_list ) {
		merge_out->new_value.assign(m.data(), m.size());
	    }
	    return true;
	}

	virtual const char* Name() const override { return "IndexMerger"; }
    private:
	ErlNifEnv* env_;
	ErlNifPid* pid_;
	EnvBox* env_box_;
		
	void update_term_index(const rocksdb::Slice& key,
			       const std::vector<Slice> list) const {
	    ErlNifBinary binkey;
	    ERL_NIF_TERM tuple;
	    enif_alloc_binary(key.size(), &binkey);
	    memcpy(binkey.data, key.data(), key.size());
	    ERL_NIF_TERM keyTerm = enif_make_binary(env_, &binkey);
	    size_t size = list.size();
	    ERL_NIF_TERM addTerm, removeTerm;

	    auto add = list[size-1];
	    if ( add.size() == 0 ) {
		addTerm = atom_undefined;
	    } else {
		addTerm = enif_make_string_len(env_, add.ToString().c_str(), add.size(), ERL_NIF_LATIN1);
	    }

	    auto remove = list[size-2];
	    if ( size > 1 && remove.size() > 0 ) {
		removeTerm = enif_make_string_len(env_, remove.ToString().c_str(), remove.size(), ERL_NIF_LATIN1);
	    } else {
		removeTerm = atom_undefined;
	    }

	    tuple = enif_make_tuple4(env_, atom_index_update, keyTerm, addTerm, removeTerm);
	    ErlNifEnv* cp_env = env_box_->get(key);
	    enif_send(cp_env, pid_, env_, tuple);
	    enif_clear_env(env_);
	}
};
}
