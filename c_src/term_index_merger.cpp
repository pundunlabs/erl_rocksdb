#include "term_index_merger.h"
#include <memory>
#include <string>
#include <set>
#include <assert.h>
#include "rocksdb/slice.h"
#include "rocksdb/merge_operator.h"
#include "utilities/merge_operators.h"
#include "utilities/ttl/db_ttl_impl.h"
#include "rocksdb/convenience.h"
#include "rocksdb/env.h"

namespace rocksdb {
    TermIndexMerger::TermIndexMerger()
    {
	env_ = Env::Default();
    }

    TermIndexMerger::TermIndexMerger( int32_t ttl )
    : ttl_(ttl)
    {
	env_ = Env::Default();
    }

    TermIndexMerger::~TermIndexMerger() {}

    bool TermIndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				      MergeOperationOutput* merge_out) const {
	merge_out->new_value.clear();
	if (merge_in.existing_value == nullptr && merge_in.operand_list.size() == 1) {
	    // Only one operand
	    Slice back = merge_in.operand_list.back();
	    //If operand is expired
	    if ( DBWithTTLImpl::IsStale(back, ttl_, env_) || IsDelete(back.data(), back.size()) ) {
		return true;
	    }
	    merge_out->new_value.append(back.data(), back.size());
	    return true;
	}

	// Compute the space needed for the final result.
	size_t numBytes = 0;
	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    numBytes += it->size();
	}

	std::set< std::string, rocksdb::PostingComp > postings;

	// Parse and emplace portions of the *existing_value if one exists.
	if (merge_in.existing_value) {
	    const char* existing = merge_in.existing_value->data();
	    auto size = merge_in.existing_value->size();
	    uint32_t i = 0;
	    //Remove already compacted but expired postings
	    while( i < size ) {
		auto pos = existing + i;
		auto portion = rocksdb::DecodeFixed32(pos);
		if ( !DBWithTTLImpl::IsStale(Slice(pos + portion), ttl_, env_) )
		{
		    postings.emplace(std::string(pos, portion));
		}
		i += portion;
	    }
	    merge_out->new_value.reserve(numBytes + size);
	} else if (numBytes) {
	    merge_out->new_value.reserve(numBytes);
	}

	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    bool is_delete = IsDelete(it->data(), it->size());
	    std::string str;
	    str.append(it->data(), it->size());
	    if ( is_delete ) {
		postings.erase(str);
	    } else if ( !DBWithTTLImpl::IsStale(*it, ttl_, env_) ) {
		postings.erase(str);
		postings.emplace(str);
	    }
	}

	for (auto it = postings.begin(); it != postings.end(); ++it) {
	    merge_out->new_value.append(*it);
	}

	return true;
    }

    bool TermIndexMerger::PartialMergeMulti(const Slice& key,
					    const std::deque<Slice>& operand_list,
					    std::string* new_value,
					    Logger* logger) const {
	return false;
    }

    const char* TermIndexMerger::Name() const  {
	return "TermIndexMerger";
    }

    bool TermIndexMerger::IsDelete(const char* chars, size_t len) const {
	if (memcmp(remove_, chars+(len-4), 4) == 0) { return true; }
	else { return false; }
    }

    std::shared_ptr<MergeOperator>
	CreateTermIndexMerger() {
	    return std::make_shared<TermIndexMerger>();
	}
} // namespace rocksdb
