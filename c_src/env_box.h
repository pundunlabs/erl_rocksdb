#include <unordered_map>
#include "rocksdb/db.h"
#include "erl_nif.h"
#include <string>

namespace rocksdb {

struct SComp {
  explicit SComp(const Comparator* c = BytewiseComparator())
      : cmp(c) {}

  bool operator()(const Slice& a, const Slice& b) const {
    return cmp->Compare(a, b) < 0;
  }

  const Comparator* cmp;
};

class EnvBox {
    public:
	bool put(Slice slice, ErlNifEnv* env) {
	    auto pair = map.emplace( slice.ToString(), env );
	    return pair.second;
	}

	ErlNifEnv* get(const Slice slice) {
	    try {
		auto res = map.at( slice.ToString() );
		return res;
	    } catch (const std::out_of_range& oor) {
		auto it = map.find(slice.ToString());
		if (it != map.end()){
		    return it->second;
		}
		return NULL;
	    }
	}

	void erase (const Slice slice) {
	    map.erase(slice.ToString());
	}

    private:
	std::unordered_map< std::string, ErlNifEnv* > map;
};

}
