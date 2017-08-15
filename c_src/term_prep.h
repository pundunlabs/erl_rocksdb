#include <string>
#include <utility>
#include "rocksdb/slice.h"

struct Term {
  const char* data;
  size_t size;
};

class TermPrep {
    public:
	TermPrep(const char* tidcid,
		 const std::vector<Term> terms,
		 const rocksdb::Slice* k) {
	    auto posting_len = k->size() + 8;
	    for (auto it = terms.begin(); it != terms.end(); ++it){
		//Prepare posting 'p'
		char* p = (char *) malloc( sizeof(char) * ( posting_len ) );
		//Copy key->data to 'p' and append Freq and Pos bytes.
		memcpy(p, k->data(), k->size());
		memcpy(p + k->size(), it->data + (it->size - 8), 8);
		//Prepare term 't'
		size_t term_len = it->size - 4; //+4 bytes for TidCid,
						//-8 Bytes for Freq and Pos
		char* t = (char *) malloc( sizeof(char) * ( term_len ) );
		//First 4 bytes are encoding Tid and Cid
		memcpy(t, tidcid, 4);
		//Last 8 bytes are encoding Freq and Pos statistics
		//Do not copy the last 8 bytes to 't' and 'terms_str_'
		memcpy(t+4, it->data, it->size-8);
		terms_.push_back(std::make_pair(rocksdb::Slice(t,term_len),
						rocksdb::Slice(p,posting_len)));
		terms_str_.append( it->data, it->size-8 ).append(" ");
	    }
	    auto i_k_size = k->size() - 5;
	    index_key_ = (char *) malloc( sizeof(char) * ( i_k_size ) );
	    //Copy encoded Tid and Cid to first 4 bytes of 'index_key_'
	    memcpy(index_key_, tidcid, 4);
	    //Copy the data starting from 5th byte of 'index_key_'
	    memcpy(index_key_+4, k->data()+5, i_k_size-4);
	    op_ = *(k->data()+4);

	};

	~TermPrep() {
	    for (auto it = terms_.begin(); it != terms_.end(); ++it){
		free ((void*) it->first.data());
		free ((void*) it->second.data());
	    }
	    free (index_key_);
	};

	std::string Normalize(const rocksdb::Slice* s) const {
	    //Remove punctuation characters from Slice add
	    std::string text = s->ToString();
	    std::string str;
	    std::remove_copy_if(text.begin(), text.end(),
				std::back_inserter(str),
				std::ptr_fun<int, int>(&IsNotAlfaNumericOrSpace));
	    // To lower case
	    std::transform(str.begin(), str.end(), str.begin(),
			       [](unsigned char c) { return std::tolower(c); });
	    return str;
	}

	std::vector<std::pair<rocksdb::Slice, rocksdb::Slice>> terms_;
	char* index_key_;
	std::string terms_str_;
	char op_;
	const char ADD = 43;

    private:
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};
