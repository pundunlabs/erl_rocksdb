#include <string>
#include <vector>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include "rocksdb/slice.h"
#include "rocksdb/comparator.h"

struct TermComparator {
    explicit TermComparator(const rocksdb::Comparator* c = rocksdb::BytewiseComparator())
	: cmp(c) {}

    bool operator()(const rocksdb::Slice& a,
		    const rocksdb::Slice& b) const {
	return cmp->Compare(a, b) < 0;
    }

    const rocksdb::Comparator* cmp;
};

class TermPrep {
    public:
	TermPrep(const rocksdb::Slice* t,
		 const rocksdb::Slice* k,
		 std::unordered_set<std::string>* stopwords) {
	    const char* c = t->data();
	    auto terms_len = t->size() - 4;
	    const rocksdb::Slice add = rocksdb::Slice(c+4, terms_len);
	    auto normalized = Normalize(&add);
	    // Populate a vector of incoming terms
	    std::string delim = " \t\n\v\f\r";
	    auto head = normalized.find_first_not_of(delim, 0);
	    auto tail = normalized.find_first_of(delim, head);
	    std::string term;
	    while ( std::string::npos != head ) {
		size_t substr_len = ((std::string::npos == tail) ? terms_len - head : tail - head);
		size_t term_len = substr_len + 4;
		char* t = (char *) malloc( sizeof(char) * ( term_len ) );
		memcpy(t, c, 4);
		term.clear();
		term = normalized.substr(head, substr_len);
		auto got = stopwords->find( term );
		if ( got == stopwords->end() ){
		    normalized.copy(t+4, substr_len, head);
		    terms_.emplace(rocksdb::Slice(t, term_len));
		    terms_str_.append(term.append(" "));
		}
		head = normalized.find_first_not_of(delim, tail);
		tail = normalized.find_first_of(delim, head);
	    }
	    auto i_k_size = k->size() - 5;
	    index_key_ = (char *) malloc( sizeof(char) * ( i_k_size ) );
	    memcpy(index_key_, c, 4);
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

	std::set<rocksdb::Slice, TermComparator> terms_;
	char* index_key_;
	std::string terms_str_ = "";
	char op_;
	char ADD = 43;

    private:	
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};
