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
	    /* positng_len is:
		4 bytes of encoded size (posting_len) +
		key's size +
		8 bytes for Freq and Pos stats +
		4 bytes of timestamp */
	    auto posting_len = k->size() + 16;
	    int64_t curtime;
	    if (!env_->GetCurrentTime(&curtime).ok()) { curtime = 0; }
	    char ts_str[4];
	    char len_str[4];
	    rocksdb::EncodeFixed32(ts_str, (uint32_t)curtime);
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    for (auto it = indices.begin(); it != indices.end(); ++it){
		std::string terms_str;
		Term cid = it->first;
		std::vector<Term> terms = it->second;
		for (auto tit = terms.begin(); tit != terms.end(); ++tit){
		    //Prepare posting 'p'
		    std::string p;
		    p.reserve(posting_len);
		    //Copy key->data to 'p' and append Freq and Pos bytes.
		    p.append( len_str, 4 );
		    p.append( k->data(), k->size() );
		    p.append( tit->data + (tit->size - 8), 8 );
		    p.append( ts_str, 4 );
		    //Prepare term 't'
		    size_t term_len = tit->size - 8; //-8 Bytes for Freq and Pos
		    std::string t;
		    t.reserve(term_len + 2);
		    //First 2 bytes are encoding Cid
		    t.append(cid.data, 2);
		    //Last 8 bytes are encoding Freq and Pos statistics
		    //Do not copy the last 8 bytes to 't' and 'terms_str_'
		    t.append(tit->data, term_len);
		    rev_index_.push_back( std::make_pair(t, p) );
		    char term_len_str[4];
		    rocksdb::EncodeFixed32(term_len_str, (uint32_t)term_len);
		    terms_str.append( term_len_str, 4 );
		    terms_str.append( tit->data, term_len );
		}
		std::string key2term;
		key2term.append(cid.data, 2);
		key2term.append(k->data(), k->size());
		index_.push_back( std::make_pair(key2term, terms_str) );
	    }
	};

	~TermPrep() {};

	std::vector<std::pair<std::string, std::string>> rev_index_;
	std::vector<std::pair<std::string, std::string>> index_;
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
		index_.push_back( key2term );
	    }
	};

	~TermDelete() {};

	void ParseReveseIndices(const std::vector<std::pair<std::string, rocksdb::PinnableSlice>> key_index,
				const rocksdb::Slice *k) {
	    std::string posting;
	    auto posting_len = k->size() + 16;
	    char len_str[4];
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    posting.append(len_str, 4);
	    posting.append(k->data(), k->size());
	    posting.append(zeroes_, 12);
	    for (auto it = key_index.begin(); it != key_index.end(); ++it){
		std::string cid_key = it->first;
		const char* str = it->second.data();
		auto size = it->second.size();
		uint32_t j = 0;
		// Parse terms until last 4 bytes (timestamp)
		while( j < size ) {
		    auto pos =  str + j;
		    auto term_len = rocksdb::DecodeFixed32(pos);
		    std::string term2key;
		    term2key.append(cid_key, 0, 2);
		    term2key.append(pos+4, term_len);
		    rev_index_.push_back(std::make_pair(term2key, posting));
		    j += (term_len+4);
		}
	    }
	}
	std::vector<std::string> index_;
	std::vector<std::pair<std::string, std::string>> rev_index_;
    private:
	rocksdb::Env* env_;
	const char zeroes_[12] = {0};
};
