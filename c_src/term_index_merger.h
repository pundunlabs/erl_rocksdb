#pragma once
#include "rocksdb/merge_operator.h"
#include "utilities/ttl/db_ttl_impl.h"
#include "term_index_macros.h"

namespace rocksdb {
    class KeyComp
    {
	public:
	    bool operator()(const std::string& a, const std::string& b) {
		auto lex_comp = a.compare(pPrefixLen, a.size()-pExtLen,
					  b, pPrefixLen, b.size()-pExtLen);
		return lex_comp < 0;
	    }
    };

    class TermIndexMerger : public MergeOperator {
	public:
	    explicit TermIndexMerger();
	    explicit TermIndexMerger( int32_t ttl );
	    virtual ~TermIndexMerger();
	    virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				     MergeOperationOutput* merge_out) const override;

	    virtual bool PartialMergeMulti(const Slice& key,
		    const std::deque<Slice>& operand_list,
		    std::string* new_value, Logger* logger) const
		override;

	    virtual const char* Name() const override;

	    virtual bool IsDelete(const char* chars, size_t len) const;
	    static bool PostingComp(const std::string& a,
				    const std::string& b) {
		auto freq_comp = a.compare(a.size()-pFreqOffset, pFreqLen,
					   b, b.size()-pFreqOffset, pFreqLen);
		if( freq_comp == 0 ) {
		    auto pos_comp = a.compare(a.size()-pPosOffset, pPosLen,
					      b, b.size()-pPosOffset, pPosLen);
		    return pos_comp < 0;
		} else {
		    return freq_comp > 0;
		}
	    }

	private:
	    rocksdb::Env* env_;
	    int32_t ttl_;
	    const char remove_[pTSLen] = {0};
    };

} // namespace rocksdb
