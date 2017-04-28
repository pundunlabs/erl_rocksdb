#include <rocksdb/compaction_filter.h>
namespace rocksdb {
class IndexFilter : public rocksdb::CompactionFilter {
    public:
	bool Filter(int level, const rocksdb::Slice& key,
		const rocksdb::Slice& existing_value, std::string* new_value,
		bool* value_changed) const override {
	    assert(*value_changed == false);
	    return false;
	}

	bool FilterMergeOperand(int level, const rocksdb::Slice& key,
		const rocksdb::Slice& existing_value) const override {
	    return true;
	}

	const char* Name() const override { return "IndexFilter"; }
};
}
