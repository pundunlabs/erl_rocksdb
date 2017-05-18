#pragma once
#include <vector>
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
	    explicit TermIndexMerger( std::vector<std::pair<int,int>>* list );
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

	    virtual uint32_t DecodeUnsigned(const char* ptr, int bytes) const;

	    virtual bool IsStale(const Slice& s, int32_t ttl) const;

	private:
	    rocksdb::Env* env_;
	    std::unordered_map<int, int32_t>* ttlmap_;
	    std::vector<std::pair<int,int>>* list_;
    };

} // namespace rocksdb
