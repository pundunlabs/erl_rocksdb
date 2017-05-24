#include <map>
#include "rocksdb/db.h"
#include "erl_nif.h"

namespace rocksdb {

struct scomp {
    bool operator()(const Slice& a, const Slice& b) const {
	return a.compare(b) > 0;
    }
};

class EnvBox {
    public:
	bool put(Slice slice, ErlNifEnv* env) {
	    std::pair< std::map< Slice, ErlNifEnv* >::iterator, bool > pair = map.emplace( slice, env );
	    return pair.second;
	}
	ErlNifEnv* get(const Slice slice) {
	    return map.at(slice);
	}
	void erase (const Slice slice) {
	    map.erase(slice);
	}
    private:
	std::map< Slice, ErlNifEnv*, scomp> map;
};

}
