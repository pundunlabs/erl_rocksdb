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
#include <iostream>

namespace rocksdb {
    TermIndexMerger::TermIndexMerger() {
	ttlmap_ = new std::unordered_map<int,int32_t>;
	env_ = Env::Default();
    }

    TermIndexMerger::~TermIndexMerger() {
	delete ttlmap_;
    }

    bool TermIndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				      MergeOperationOutput* merge_out) const {
	auto key = merge_in.key.ToString();
	auto tid = DecodeUnsigned(key.data(), 2);
	auto ttl = ttlmap_->at((int)tid);
	merge_out->new_value.clear();

	Slice back = merge_in.operand_list.back();

	//If last write operation ttl was expired than omit all
	if ( IsStale(back, (int32_t) ttl) ) {
	    return true;
	}

	if (merge_in.existing_value == nullptr && merge_in.operand_list.size() == 1) {
	    // Only one operand
	    const char* chars = back.data();
	    size_t buf_len = back.size()-1;
	    char* buf = (char*) malloc (sizeof(char)*(buf_len));
	    memcpy(buf, chars, 4);
	    memcpy(buf+4, chars+5, buf_len-4);
	    merge_out->existing_operand = Slice(buf, buf_len);
	    return true;
	}

	// Compute the space needed for the final result.
	size_t numBytes = 0;
	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    numBytes += it->size()-1;
	}

	// Prepend the *existing_value if one exists.
	if (merge_in.existing_value) {
	    merge_out->new_value.reserve(numBytes + merge_in.existing_value->size());
	    merge_out->new_value.append(merge_in.existing_value->data(),
					merge_in.existing_value->size());
	} else if (numBytes) {
	    merge_out->new_value.reserve(numBytes);
	}

	std::set< std::string > postings;
	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    const char* chars = it->data();
	    char  op = chars[4];
	    //buf_len is size - 1 since we remove op char.
	    size_t buf_len = it->size() - 1;
	    char* buf = (char*) malloc (sizeof(char)*(buf_len));
	    
	    memcpy(buf, chars, 4);
	    memcpy(buf+4, chars+5, buf_len-4);
	    std::string str = std::string(buf, buf_len);

	    if (op == 43) {
		postings.emplace(str);
	    } else if( op == 45 ) {
		postings.erase(str);
	    }
	    delete buf;
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

    void TermIndexMerger::AddTtlMapping(int tid, int32_t ttl) const {
	ttlmap_->erase(tid);
	ttlmap_->emplace(tid, ttl);
    }

    void TermIndexMerger::RemoveTtlMapping(int tid) const {
	ttlmap_->erase(tid);
    }

    uint32_t TermIndexMerger::DecodeUnsigned(const char* ptr, int bytes) const{
	uint32_t usi = 0;
	int shift = 0;
	for (int i = bytes; i > 0 ; --i) {
	    usi = usi | static_cast<uint32_t>(static_cast<unsigned char>(ptr[i-1])) << shift;
	    shift += 8;
	}
	return usi;
    }

    bool TermIndexMerger::IsStale(const Slice& s, int32_t ttl) const {
	if (ttl <= 0) {
	    return false;
	}
	int64_t curtime;
	if (!env_->GetCurrentTime(&curtime).ok()) {
	    return false;
	}
	int32_t timestamp_value =
	    DecodeUnsigned(s.data() + s.size() - 4, 4);
	return (timestamp_value + ttl) < curtime;

    }

    std::shared_ptr<MergeOperator>
	CreateTermIndexMerger() {
	    return std::make_shared<TermIndexMerger>();
	}
} // namespace rocksdb
