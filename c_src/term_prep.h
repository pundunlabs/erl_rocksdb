#include <string>
#include "rocksdb/slice.h"

struct Term {
  const char* data;
  size_t size;
  int32_t freq;
  int32_t position;
};

class TermPrep {
    public:
	TermPrep(const char* tidcid,
		 const std::vector<Term> t,
		 const rocksdb::Slice* k) {
	    for (auto it = t.begin(); it != t.end(); ++it){
		size_t term_len = it->size + 4;
		char* t = (char *) malloc( sizeof(char) * ( term_len ) );
		memcpy(t, tidcid, 4);
		memcpy(t+4, it->data, it->size);
		terms_.push_back(rocksdb::Slice(t, term_len));
		terms_str_.append( it->data, it->size ).append(" ");
	    }
	    auto i_k_size = k->size() - 5;
	    index_key_ = (char *) malloc( sizeof(char) * ( i_k_size ) );
	    memcpy(index_key_, tidcid, 4);
	    memcpy(index_key_+4, k->data()+5, i_k_size-4);
	    op_ = *(k->data()+4);
	};

	~TermPrep() {
	    for (auto it = terms_.begin(); it != terms_.end(); ++it){
		free ((void*) it->data());
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

	std::vector<rocksdb::Slice> terms_;
	char* index_key_;
	std::string terms_str_;
	char op_;
	const char ADD = 43;

    private:
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};
