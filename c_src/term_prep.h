#include <string>
#include <vector>
#include <unordered_set>
#include <algorithm>
#include <iostream>
#include "rocksdb/slice.h"

class TermPrep {
    public:
	TermPrep() {};
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

	std::vector<rocksdb::Slice> GetTerms(const rocksdb::Slice * s) const {
	    const char* c = s->data();
	    auto terms_len = s->size() - 4;
	    const rocksdb::Slice add = rocksdb::Slice(c+4, terms_len);
	    std::string str = Normalize(&add);
	    // Populate a vector of incoming terms
	    std::vector<rocksdb::Slice> terms;
	    std::string delim = " \t\n\v\f\r";
	    auto head = str.find_first_not_of(delim, 0);
	    auto tail = str.find_first_of(delim, head);
	    while ( std::string::npos != head ) {
		size_t substr_len = ((std::string::npos == tail) ? terms_len - head : tail - head);
		size_t term_len = substr_len + 4;
		char* t = (char *) malloc( sizeof(char) * ( term_len ) );
		memcpy(t, c, 4);
		str.copy(t+4, substr_len, head);
		terms.push_back(rocksdb::Slice(t, term_len));
		head = str.find_first_not_of(delim, tail);
		tail = str.find_first_of(delim, head);
	    }
	    return terms;
	}
	
	void FreeTerms(std::vector<rocksdb::Slice> terms) const {
	    for (auto it = terms.begin(); it != terms.end(); ++it){
		free ((void *) it->data());
	    }
	}

    private:	
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};

