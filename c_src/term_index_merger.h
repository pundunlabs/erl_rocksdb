#pragma once
#include <deque>
#include <string>
#include <unordered_map>

#include "rocksdb/merge_operator.h"
#include "rocksdb/slice.h"
#include "utilities/ttl/db_ttl_impl.h"

namespace rocksdb {
    class TermIndexMerger : public MergeOperator {
	public:
	    explicit TermIndexMerger();
	    virtual ~TermIndexMerger();
	    virtual bool FullMergeV2(const MergeOperationInput& merge_in,
				     MergeOperationOutput* merge_out) const override;

	    virtual bool PartialMergeMulti(const Slice& key,
		    const std::deque<Slice>& operand_list,
		    std::string* new_value, Logger* logger) const
		override;

	    virtual const char* Name() const override;

	    virtual void AddTtlMapping(int tid, int32_t ttl) const;

	    virtual void RemoveTtlMapping(int tid) const;

	private:
	    rocksdb::DBWithTTLImpl* db_;
	    std::unordered_map<int, int32_t>* ttlmap_;
    };

} // namespace rocksdb
