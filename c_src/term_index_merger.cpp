#include "term_index_merger.h"

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
	    if ( DBWithTTLImpl::IsStale(back, ttl_, env_->GetSystemClock().get()) || IsDelete(back.data(), back.size()) ) {
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

	std::set< std::string, rocksdb::KeyComp > posting_set;
	// Parse and emplace portions of the *existing_value if one exists.
	if (merge_in.existing_value) {
	    const char* existing = merge_in.existing_value->data();
	    auto size = merge_in.existing_value->size();
	    uint32_t i = 0;
	    //Remove already compacted but expired postings
	    while( i < size ) {
		auto pos = existing + i;
		auto portion = rocksdb::DecodeFixed32(pos);
		if ( !DBWithTTLImpl::IsStale(Slice(pos + portion), ttl_, env_->GetSystemClock().get()) )
		{
		    std::string posting = std::string(pos, portion);
		    posting_set.emplace(std::move(posting));
		}
		i += portion;
	    }
	    merge_out->new_value.reserve(numBytes + size);
	} else if (numBytes) {
	    merge_out->new_value.reserve(numBytes);
	}

	for (auto it = merge_in.operand_list.begin();
		it != merge_in.operand_list.end(); ++it) {
	    const char* pos = it->data();
	    size_t size = it->size();
	    bool is_delete = IsDelete(pos, size);
	    std::string posting = std::string(pos, size);
	    if ( is_delete ) {
		auto iter = posting_set.find(posting);
		if (iter != posting_set.end()) {
		    posting_set.erase(iter);
		}
	    } else if ( !DBWithTTLImpl::IsStale(*it, ttl_, env_->GetSystemClock().get()) ) {
		auto iter = posting_set.find(posting);
		if (iter != posting_set.end()) {
		    auto hint = posting_set.erase(iter);
		    posting_set.emplace_hint(hint, std::move(posting));
		} else{
		    posting_set.emplace(std::move(posting));
		}
	    }
	}
	std::vector< std::string > postings;
	for (auto it = posting_set.begin(); it != posting_set.end(); ++it) {
	    postings.push_back( std::move(*it) );
	}
	std::sort(postings.begin(), postings.end(), PostingComp);
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
	if (memcmp(remove_, chars+(len-pTSLen), pTSLen) == 0) { return true; }
	else { return false; }
    }

    std::shared_ptr<MergeOperator> CreateTermIndexMerger() {
	return std::make_shared<TermIndexMerger>();
    }
} // namespace rocksdb
