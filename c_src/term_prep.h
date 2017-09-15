#include <string>
#include <utility>
#include "rocksdb/slice.h"
#include "utilities/ttl/db_ttl_impl.h"
#include <iostream>

struct Term {
  const char* data;
  size_t size;
};

class TermPrep {
    public:
	TermPrep(const std::vector<std::pair<Term, std::vector<Term>>> indices,
		 const rocksdb::Slice* k) {
	    env_ = rocksdb::Env::Default();
	    /*positng_len is:
		4 bytes of encoded computed size (posting_len) +
		key's size +
		8 bytes for Freq and Pos stats +
		4 bytes of timestamp
	    */
	    auto posting_len = k->size() + 16;
	    int64_t curtime;
	    if (!env_->GetCurrentTime(&curtime).ok()) { curtime = 0; }
	    char ts_str[4];
	    char len_str[4];
	    rocksdb::EncodeFixed32(ts_str, (uint32_t)curtime);
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    for (auto it = indices.begin(); it != indices.end(); ++it){
		Term cid = it->first;
		std::vector<Term> terms = it->second;
		std::string terms_str;
		for (auto it = terms.begin(); it != terms.end(); ++it){
		    //Prepare posting 'p'
		    char* p = (char *) malloc( sizeof(char) * ( posting_len ) );
		    //Copy key->data to 'p' and append Freq and Pos bytes.
		    memcpy(p, len_str, 4);
		    memcpy(p + 4, k->data(), k->size());
		    memcpy(p + 4 + k->size(), it->data + (it->size - 8), 8);
		    memcpy(p + (posting_len - 4), ts_str, 4);
		    //Prepare term 't'
		    size_t term_len = it->size - 8; //-8 Bytes for Freq and Pos
		    char* t = (char *) malloc( sizeof(char) * ( term_len+2 ) );
		    //First 2 bytes are encoding Cid
		    memcpy(t, cid.data, 2);
		    //Last 8 bytes are encoding Freq and Pos statistics
		    //Do not copy the last 8 bytes to 't' and 'terms_str_'
		    memcpy(t+2, it->data, it->size-8);
		    rev_index_.push_back(
			std::make_pair(rocksdb::Slice(t,term_len+2),
				       rocksdb::Slice(p,posting_len)));
		    char term_len_str[4];
		    rocksdb::EncodeFixed32(term_len_str, (uint32_t)term_len);
		    terms_str.append( term_len_str, 4 );
		    terms_str.append( it->data, it->size - 8 );
		}
		std::string key2term;
		key2term.append(cid.data, 2);
		key2term.append(k->data(), k->size());
		index_.push_back(std::make_pair(rocksdb::Slice(key2term),
						rocksdb::Slice(terms_str)));
	    }
	};

	~TermPrep() {
	    for (auto it = rev_index_.begin(); it != rev_index_.end(); ++it){
		free ((void*) it->first.data());
		free ((void*) it->second.data());
	    }
	};

	std::vector<std::pair<rocksdb::Slice, rocksdb::Slice>> rev_index_;
	std::vector<std::pair<rocksdb::Slice, rocksdb::Slice>> index_;
    private:
	rocksdb::Env* env_;
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};

class TermDelete {
    public:
	TermDelete(const std::vector<Term> cids,
		   const rocksdb::Slice* k) {
	    env_ = rocksdb::Env::Default();
	    for (auto it = cids.begin(); it != cids.end(); ++it){
		Term cid = *it;
		std::string key2term;
		key2term.append(cid.data, 2);
		key2term.append(k->data(), k->size());
		index_.push_back( rocksdb::Slice(key2term) );
	    }
	};

	~TermDelete() {
	};

	void ParseReveseIndices(const std::vector<Term> cids,
				const std::vector<rocksdb::PinnableSlice> term_strs,
				const rocksdb::Slice *k) {
	    std::string posting;
	    auto posting_len = k->size() + 16;
	    char len_str[4];
	    char zeroes[8];
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    rocksdb::EncodeFixed64(zeroes, (uint64_t)0);
	    posting.append(len_str);
	    posting.append(k->data(), k->size());
	    posting.append(zeroes);
	    for (size_t i = 0; i < cids.size(); ++i) {
		Term cid = cids[i];
		const char* str = term_strs[i].data();
		auto size = term_strs[i].size();
		uint32_t j = 0;
		// Parse terms until last 4 bytes (timestamp)
		while( j < size ) {
		    auto pos =  str + j;
		    auto term_len = rocksdb::DecodeFixed32(pos);
		    std::string term2key;
		    term2key.append(cid.data, 2);
		    std::cout << "pos: " << j << " term_len: " << term_len << std::endl;
		    term2key.append(pos+4, term_len);
		    rev_index_.push_back(std::make_pair(rocksdb::Slice(term2key),
							rocksdb::Slice(posting)));
		    j += (term_len+4);
		}
	    }
	}

	std::vector<rocksdb::Slice> index_;
	std::vector<std::pair<rocksdb::Slice, rocksdb::Slice>> rev_index_;
    private:
	rocksdb::Env* env_;
};
