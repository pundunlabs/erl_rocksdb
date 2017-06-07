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

	    virtual bool PartialMergeMulti(const Slice& key,
		    const std::deque<Slice>& operand_list,
		    std::string* new_value, Logger* logger) const
		override;

	    virtual const char* Name() const override;

	    ERL_NIF_TERM atom_remove_term;
	    ERL_NIF_TERM atom_undefined;

	private:
	    ErlNifEnv* env_;
	    ErlNifPid* pid_;
	    TermPrep tp_;

	    std::pair<std::string, std::string> do_merge(const Slice* remove,
							 const Slice& add) const;

	    std::string make_add_term(const Slice* s) const;

	    std::string make_remove_term(const Slice* s) const;

	    std::pair<std::string, std::string> diff_terms(const Slice* add,
							   const Slice* remove) const;

	    void update_term_index(const rocksdb::Slice& key,
				   const std::string remove) const;

	    string term_prep(const Slice * s) const;
    };
}
