#include "rocksdb/comparator.h"

namespace rocksdb {

class DescendingComparator : public Comparator {
    public:
	// Three-way comparison function:
	//   if a > b: negative result
	//   if a < b: positive result
	//   else: zero result
	
	int Compare(const Slice& a, const Slice& b) const {
	    return a.compare(b) * -1;
	}
	// Ignore the following methods for now:
	
	const char* Name() const { return "DescendingComparator"; }
	void FindShortestSeparator(std::string*, const Slice&) const { }
	void FindShortSuccessor(std::string*) const { }
};
}
