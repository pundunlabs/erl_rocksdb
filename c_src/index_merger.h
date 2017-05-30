#include <rocksdb/merge_operator.h>
#include "rocksdb_nif.h"
#include "erl_nif.h"

namespace rocksdb {
class IndexMerger : public MergeOperator {
    public:
	explicit IndexMerger(ErlNifPid* pid);

	~IndexMerger();

	virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				 MergeOperationOutput* merge_out) const override;

	virtual const char* Name() const override;

	ERL_NIF_TERM atom_index_update;
	ERL_NIF_TERM atom_undefined;

    private:
	ErlNifEnv* env_;
	ErlNifPid* pid_;

	void update_term_index(const rocksdb::Slice& key,
			       const std::vector<Slice> list) const;

	ERL_NIF_TERM make_add_term(Slice* s) const;

	ERL_NIF_TERM make_remove_term(size_t size, Slice* s) const;

	std::pair<ERL_NIF_TERM, ERL_NIF_TERM> diff_terms(Slice* add,
							 Slice* remove) const;
	string term_prep(Slice * s) const;
};
}
