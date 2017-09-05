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
	ttlmap_ = new std::unordered_map<int,int32_t>;
	env_ = Env::Default();
    }

    TermIndexMerger::TermIndexMerger( std::vector<std::pair<int,int>>* list )
    : list_(list)
    {
	ttlmap_ = new std::unordered_map<int,int32_t>;
	for (auto it = list_->begin() ; it != list_->end(); ++it){
	    ttlmap_->emplace(it->first, (int32_t)it->second);
	}
	env_ = Env::Default();

    }

    TermIndexMerger::~TermIndexMerger() {
	delete ttlmap_;
    }

    bool TermIndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				      MergeOperationOutput* merge_out) const {
	auto tid = DecodeUnsigned(merge_in.key.data(), 2);
	auto ttl = ttlmap_->find((int)tid);
	merge_out->new_value.clear();
	if ( ttl == ttlmap_->end() ){
	    // Table does not exists anymore
	    return true;
	}
	if (merge_in.existing_value == nullptr && merge_in.operand_list.size() == 1) {
	    // Only one operand
	    Slice back = merge_in.operand_list.back();
	    const char* chars = back.data();
	    //If operand is expired
	    if ( IsStale(back, ttl->second) ) {
		return true;
	    }
	    //buf_len is size - 1 since we remove op char.
	    size_t buf_len = back.size()-1;
	    merge_out->new_value.append(chars, buf_len);
	    return true;
	}

	// Compute the space needed for the final result.
	size_t numBytes = 0;
	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    numBytes += it->size()-1;
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
		auto len = DecodeUnsigned(pos, 4);
		auto portion = len + 11;//+4 bytes of encoded len
					//-1 byte of removed Op char
					//+8 bytes of encoded Freq and Pos
		if ( !IsStale(Slice(pos + portion), ttl->second) ) {
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
	    //If operand is not expired
	    if ( !IsStale(*it, ttl->second) ) {
		const char* chars = it->data();
		char  op = chars[4];
		//buf_len is size - 1 since we remove op char.
		size_t buf_len = it->size() - 1;

		std::string str;
		str.append(chars, 4);
		str.append(chars+5, buf_len-4);

		if (op == 43) {
		    postings.emplace(str);
		} else if( op == 45 ) {
		    postings.erase(str);
		}
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
	    DecodeUnsigned(s.data() + s.size() - 12, 4);
	return (timestamp_value + ttl) < curtime;

    }

    std::shared_ptr<MergeOperator>
	CreateTermIndexMerger() {
	    return std::make_shared<TermIndexMerger>();
	}
} // namespace rocksdb
