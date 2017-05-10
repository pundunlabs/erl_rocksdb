#include "index_merger.h"
#include <algorithm>
#include <utility>
#include <tuple>
#include <string>
#include <sstream>
#include <unordered_set>
#include <vector>

namespace rocksdb {

    IndexMerger::IndexMerger(ErlNifPid* pid, EnvBox *env_box)
    : pid_(pid), env_box_(env_box){
	env_ = enif_alloc_env();
	atom_index_update = enif_make_atom(env_, "index_update");
	atom_undefined = enif_make_atom(env_, "undefined");
      }

    IndexMerger::~IndexMerger() {
	enif_free_env(env_);
    }
    
    bool IndexMerger::FullMergeV2(const MergeOperationInput& merge_in,
				  MergeOperationOutput* merge_out) const {
	update_term_index(merge_in.key, merge_in.operand_list);
	if ( merge_in.operand_list.back().size() == 0 ) {
	    return true;
	}

	merge_out->new_value.clear();
	if ( merge_in.existing_value != nullptr ) {
	    merge_out->new_value.assign(merge_in.existing_value->data(),
		    merge_in.existing_value->size());
	}
	for ( const Slice& m : merge_in.operand_list ) {
	    merge_out->new_value.assign(m.data(), m.size());
	}
	return true;
    }

    const char* IndexMerger::Name() const { return "IndexMerger"; }
    
    void IndexMerger::update_term_index(const Slice& key,
					const std::vector<Slice> list) const {
	ErlNifBinary binkey;
	ERL_NIF_TERM tuple;
	enif_alloc_binary(key.size(), &binkey);
	memcpy(binkey.data, key.data(), key.size());
	ERL_NIF_TERM keyTerm = enif_make_binary(env_, &binkey);
	ERL_NIF_TERM addTerm, removeTerm;
	
	size_t size = list.size();
	auto add = list[size-1];
	auto remove = list[size-2];
	
	if ( size > 1 && add.size() > 0 && remove.size() > 0 ) {
	    std::tie(addTerm, removeTerm) = diff_terms(&add, &remove);
	}else {
	    addTerm = make_add_term(&add);
	    removeTerm = make_remove_term(size, &remove);
	}
	
	tuple = enif_make_tuple4(this->env_,
				 atom_index_update,
				 keyTerm,
				 addTerm,
				 removeTerm);

	ErlNifEnv* cp_env = this->env_box_->get(key);
	enif_send(cp_env, this->pid_, this->env_, tuple);
	enif_clear_env(this->env_);
    }

    ERL_NIF_TERM IndexMerger::make_add_term(Slice* s) const {
	if (s->size() > 0 ){
	    return enif_make_string_len(this->env_,
					s->ToString().c_str(),
					s->size(), ERL_NIF_LATIN1);
	} else{
	    return this->atom_undefined;
	}
    }
    
    ERL_NIF_TERM IndexMerger::make_remove_term(size_t size, Slice* s) const {
	if ( size > 1 && s->size() > 0 ){
	    return enif_make_string_len(this->env_,
					s->ToString().c_str(),
					s->size(), ERL_NIF_LATIN1);
	} else {
	    return this->atom_undefined;
	}
    }
    
    std::pair<ERL_NIF_TERM, ERL_NIF_TERM> IndexMerger::diff_terms(Slice* add,
								  Slice* remove) const {
	//Remove punctuation characters from Slice add
	string text = add->ToString();
	string str;
	std::remove_copy_if(text.begin(), text.end(),            
			    std::back_inserter(str),
			    std::ptr_fun<int, int>(&std::ispunct));
	// To lower case
	std::transform(str.begin(), str.end(), str.begin(),
	                   [](unsigned char c) { return std::tolower(c); });
	// Populate a set of incoming terms
	std::unordered_multiset<std::string> terms;
	std::string delim = " \t\n\v\f\r";
	auto head = str.find_first_not_of(delim, 0);
	auto tail = str.find_first_of(delim, head);
	while ( string::npos != tail || string::npos != head ) {
	    terms.insert(str.substr(head, tail - head));
	    head = str.find_first_not_of(delim, tail);
	    tail = str.find_first_of(delim, head);
	}

	//Remove punctuation characters from Slice remove
	text.clear();
	text = remove->ToString();
	str.clear();
	std::remove_copy_if(text.begin(), text.end(),            
			    std::back_inserter(str),
			    std::ptr_fun<int, int>(&std::ispunct));	
	// To lower case
	std::transform(str.begin(), str.end(), str.begin(),
	                   [](unsigned char c) { return std::tolower(c); });
	// Remove already existing terms from set
	std::vector<std::string> removeTerms;
	head = str.find_first_not_of(delim, 0);
	tail = str.find_first_of(delim, head);
	while ( string::npos != tail || string::npos != head ) {
	    auto rem = str.substr(head, tail - head);
	    auto it = terms.find(rem);
	    if ( it!= terms.end() ) {
		terms.erase(it);
	    } else {
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
	ERL_NIF_TERM at;
	auto sstr = ss.str();
	if(sstr.empty()) {
	    at = this->atom_undefined;
	} else {
	    at = enif_make_string_len(this->env_, sstr.c_str(), sstr.size()-1, ERL_NIF_LATIN1);
	}

	// Populate removed terms from remove unkept terms
	ss.clear();
	ss.str("");
	sstr.clear();
	for( auto it = removeTerms.begin(); it != removeTerms.end(); ++it ) {
	      ss << (*it) << " ";
	}
	ERL_NIF_TERM rt;
	sstr = ss.str();
	if(sstr.empty()) {
	    rt = this->atom_undefined;
	} else {
	    rt = enif_make_string_len(this->env_, sstr.c_str(), sstr.size()-1, ERL_NIF_LATIN1);
	}

	return std::make_pair(at, rt);
    }
}
