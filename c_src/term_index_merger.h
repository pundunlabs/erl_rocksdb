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
		auto stat_comp = a.compare(a.size()-8, 8, b, b.size()-8, 8);
		if( stat_comp == 0 ) {return lex_comp < 0; }
		else { return stat_comp > 0;}
	    }
    };

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
