#include <rocksdb/merge_operator.h>
#include "erl_nif.h"
#include <algorithm>
#include <thread>

namespace rocksdb {
class IndexMerger : public MergeOperator {
    public:
	IndexMerger(ErlNifPid* pid, ErlNifEnv **cp_env)
	: pid_(pid), cp_env_(cp_env){
	    env_ = enif_alloc_env();
	    atom_index_update = enif_make_atom(env_, "index_update");
	  }

	~IndexMerger() {
	    enif_free_env(env_);
	}
	ERL_NIF_TERM atom_index_update;
	virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				 MergeOperationOutput* merge_out) const override {
	    merge_out->new_value.clear();
	    if (merge_in.existing_value != nullptr) {
		merge_out->new_value.assign(merge_in.existing_value->data(),
			merge_in.existing_value->size());
	    }
	    for (const rocksdb::Slice& m : merge_in.operand_list) {
		merge_out->new_value.assign(m.data(), m.size());
	    }
	    update_term_index(merge_in.key, merge_in.operand_list);
	    return true;
	}

	virtual const char* Name() const override { return "IndexMerger"; }
    private:
	ErlNifEnv* env_;
	ErlNifPid* pid_;
	ErlNifEnv** cp_env_;
		
	void update_term_index(const rocksdb::Slice& key,
			       const std::vector<Slice> list) const {
	    ErlNifBinary binkey;
	    ERL_NIF_TERM tuple;
	    enif_alloc_binary(key.size(), &binkey);
	    memcpy(binkey.data, key.data(), key.size());
	    ERL_NIF_TERM keyTerm = enif_make_binary(env_, &binkey);
	    size_t size = list.size();
	    auto add = list[size-1];
	    ERL_NIF_TERM addTerm = enif_make_string_len(env_, add.ToString().c_str(), add.size(), ERL_NIF_LATIN1);
	    if (size > 1) {
		auto remove = list[size-2];
		ERL_NIF_TERM removeTerm = enif_make_string_len(env_, remove.ToString().c_str(), remove.size(), ERL_NIF_LATIN1);
		tuple = enif_make_tuple4(env_, atom_index_update, keyTerm, addTerm, removeTerm);
	    } else{
		tuple = enif_make_tuple3(env_, atom_index_update, keyTerm, addTerm);
	    }
	    enif_send(*cp_env_, pid_, env_, tuple);
	    enif_clear_env(env_);
	}
};
}
