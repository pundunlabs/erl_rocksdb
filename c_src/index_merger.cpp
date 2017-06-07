#include "index_merger.h"
#include <utility>
#include <tuple>
#include <string>
#include <sstream>
#include <unordered_set>
#include <vector>

namespace rocksdb {

    IndexMerger::IndexMerger(ErlNifPid* pid)
    : pid_(pid) {
	env_ = enif_alloc_env();
	atom_remove_term = enif_make_atom(env_, "remove_term");
      }

    IndexMerger::~IndexMerger() {
	enif_free_env(env_);
    }

    bool IndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				  MergeOperationOutput* merge_out) const {
	//If there is no operand
	if ( merge_in.operand_list.size() == 0 ) {
	    return true;
	}

	//Generate added and removed postings strings.
	std::string add, remove;
	std::tie(add, remove) = do_merge(merge_in.existing_value,
					 merge_in.operand_list.back());
	//There is at least one operand so clear new_value
	merge_out->new_value.clear();
	if ( !add.empty() ) {
	    merge_out->new_value.assign(add);
	}

	if ( remove.size() > 0 ) {
	    //Communicate with erlang node for removed postings.
	    update_term_index(merge_in.key, remove);
	}


	return true;
    }

    bool IndexMerger::PartialMergeMulti(const Slice& key,
					const std::deque<Slice>& operand_list,
					std::string* new_value,
					Logger* logger) const {
	return false;
    }

    const char* IndexMerger::Name() const { return "IndexMerger"; }

    std::pair<std::string, std::string>
	IndexMerger::do_merge(const Slice* remove,
			      const Slice& add) const {
	//If there is a new operand and an existing value
	if ( add.size() > 0 && remove != nullptr ) {
	    return diff_terms(&add, remove);
	} else { //Otherwise, one will be empty string
	    std::string addStr = make_add_term(&add);
	    std::string removeStr = make_remove_term(remove);
	    return std::make_pair(addStr, removeStr);
	}
    }

    std::string IndexMerger::make_add_term(const Slice* s) const {
	if (s->size() > 0 ){
	     return s->ToString();
	} else{
	    return std::string();
	}
    }

    std::string IndexMerger::make_remove_term(const Slice* s) const {
	if ( s != nullptr && s->size() > 0 ){
	    return s->ToString();
	} else {
	    return std::string();
	}
    }

    std::pair<std::string, std::string>
	IndexMerger::diff_terms(const Slice* add,
				const Slice* remove) const {
	string str = add->ToString();
	// Populate a set of incoming terms
	std::unordered_set<std::string> terms;
	std::string delim = " \t\n\v\f\r";
	auto head = str.find_first_not_of(delim, 0);
	auto tail = str.find_first_of(delim, head);
	while ( string::npos != tail || string::npos != head ) {
	    terms.insert(str.substr(head, tail - head));
	    head = str.find_first_not_of(delim, tail);
	    tail = str.find_first_of(delim, head);
	}

	str.clear();
	str = remove->ToString();

	// DO NOT Remove already existing terms from set
	// We update term index with new TS to overwrite old TS
	std::vector<std::string> removeTerms;
	head = str.find_first_not_of(delim, 0);
	tail = str.find_first_of(delim, head);
	while ( string::npos != tail || string::npos != head ) {
	    auto rem = str.substr(head, tail - head);
	    auto it = terms.find(rem);
	    if ( it == terms.end() ) {
		removeTerms.push_back(rem);
	    }
	    head = str.find_first_not_of(delim, tail);
	    tail = str.find_first_of(delim, head);
	}

	// Populate added terms from remaining set
	std::stringstream ss;
	for( auto it = terms.begin(); it != terms.end(); ++it ) {
	      ss << (*it) << " ";
	}
	auto at = ss.str();

	// Populate removed terms from remove unkept terms
	ss.clear();
	ss.str("");
	for( auto it = removeTerms.begin(); it != removeTerms.end(); ++it ) {
	      ss << (*it) << " ";
	}
	auto rt = ss.str();

	return std::make_pair(at, rt);
    }

    void IndexMerger::update_term_index(const Slice& key,
					const std::string remove) const {
	ErlNifEnv* env = enif_alloc_env();
	ErlNifBinary binkey;
	enif_alloc_binary(key.size(), &binkey);
	memcpy(binkey.data, key.data(), key.size());

	ERL_NIF_TERM keyTerm = enif_make_binary(env, &binkey);
	ERL_NIF_TERM removeTerm = enif_make_string(env, remove.c_str(), ERL_NIF_LATIN1);

	ERL_NIF_TERM tuple;
	tuple = enif_make_tuple3(env, atom_remove_term, keyTerm, removeTerm);

	enif_send(NULL, pid_, env, tuple);
	enif_free_env(env);
    }
}
