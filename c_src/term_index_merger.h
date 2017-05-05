#pragma once
#include <deque>
#include <string>

#include "rocksdb/merge_operator.h"
#include "rocksdb/slice.h"

namespace rocksdb {
    class TermIndexMerger : public MergeOperator {
	public:
	    // Constructor with delimiter
	    explicit TermIndexMerger();

	    virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				     MergeOperationOutput* merge_out) const override;

	    virtual bool PartialMergeMulti(const Slice& key,
		    const std::deque<Slice>& operand_list,
		    std::string* new_value, Logger* logger) const
		override;

	    virtual const char* Name() const override;

	private:
	    bool _AssocPartialMergeMulti(const Slice& key,
		    const std::deque<Slice>& operand_list,
		    std::string* new_value, Logger* logger) const;
    };

} // namespace rocksdb
