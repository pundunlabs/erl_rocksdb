#include "term_index_merger.h"
#include <memory>
#include <string>
#include <set>
#include <assert.h>
#include "rocksdb/slice.h"
#include "rocksdb/merge_operator.h"
#include "utilities/merge_operators.h"

namespace rocksdb {
    TermIndexMerger::TermIndexMerger() {}

    bool TermIndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				      MergeOperationOutput* merge_out) const {
	merge_out->new_value.clear();

	if (merge_in.existing_value == nullptr && merge_in.operand_list.size() == 1) {
	    // Only one operand
	    Slice back = merge_in.operand_list.back();
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
	    size_t buf_len = it->size()-1;
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

    std::shared_ptr<MergeOperator>
	CreateTermIndexMerger() {
	    return std::make_shared<TermIndexMerger>();
	}

} // namespace rocksdb
