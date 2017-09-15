#pragma once
#include <vector>
#include <deque>
#include <string>
#include <unordered_map>

#include "rocksdb/merge_operator.h"
#include "rocksdb/slice.h"
#include "utilities/ttl/db_ttl_impl.h"

namespace rocksdb {
    class PostingComp
    {
	public:
	    bool operator()(const std::string& a, const std::string& b) {
		auto lex_comp = a.compare(0, a.size()-12, b, 0, b.size()-12);
		if( lex_comp == 0 ) { return false; }
		auto freq_comp = a.compare(a.size()-8, 4, b, b.size()-8, 4);
		auto pos_comp = a.compare(a.size()-4, 4, b, b.size()-4, 4);
		if( freq_comp == 0 ) {
		    if(pos_comp == 0) { return lex_comp < 0; }
		    else { return pos_comp < 0; }
		}
		else { return freq_comp > 0; }
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

	private:
	    rocksdb::Env* env_;
	    int32_t ttl_;
	    unsigned char remove_[4] = {0xff, 0xff, 0xff, 0xff};
    };

} // namespace rocksdb
