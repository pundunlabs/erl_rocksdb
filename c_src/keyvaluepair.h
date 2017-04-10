
#include "rocksdb/db.h"
#include "erl_nif.h"

namespace rocksdb {

class KeyValuePair : public Slice {
    public:
        /* Create an empty KeyValuePair. */
        KeyValuePair()
	: Slice (), tag_(-1) { }
        /* Create an KeyValuePair for comp function. */
        KeyValuePair(bool a)
	: Slice (), tag_(-1), descending_(a) { }
        /* Create a KeyValuePair */
        KeyValuePair (int tag, const char* d, size_t n,
                      ErlNifBinary v)
        : Slice (d, n), tag_(tag), value_(v) { }
        //~ KeyValuePair ();
        /* Return the integer value of tag */
        const int tag() const { return tag_; }
        /* Return the bool value of descending */
        const bool descending() const { return descending_; }
        /* Return a pointer to the referenced key */
        const char* key() const { return data(); }
        /* Return the length (in bytes) of the referenced key */
        size_t key_size() const { return size(); }
        /* Return the referenced binary value */
        ErlNifBinary value() const { return value_; }
        /*Compare function to be used by STL: Algorithm */
        bool operator()(const KeyValuePair& a, const KeyValuePair& b) const {
            int c;
	    if (descending()){
		c = a.compare(b);
		if (c == 0) {return a.tag() > b.tag();}
		else {return c < 0;}
	    }
	    else{
		c = a.compare(b);
		if (c == 0) {return a.tag() > b.tag();}
		else {return c > 0;}
	    }
        }
    private:
        unsigned short int tag_;
	bool descending_;
	ErlNifBinary value_;
};
}
