
#include <string>

#include <iostream>
#include <vector>
#include <deque>
#include <algorithm>    // std::make_heap, std::pop_heap, std::push_heap, std::sort_heap

#include "erl_nif.h"
#include "keyvaluepair.h"

using namespace std;
using namespace rocksdb;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,  void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static ERL_NIF_TERM merge_sorted_kvls_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    /*kvls: the pointer to list of key/value lists passed from erlang*/
    ERL_NIF_TERM kvls = argv[1];
    /*kvl: the resulting erlang nif term*/
    ERL_NIF_TERM kvl;
    unsigned int kvls_len, len;
    ERL_NIF_TERM head, tail, h, t;
    int arity;
    const ERL_NIF_TERM* tuple;
    ErlNifBinary keybin, valuebin;
    int total_kvps = 0;

    int dir;
    /*get direction integer*/
    if (argc !=2 || !enif_get_int(env, argv[0], &dir)) {
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "direction"));
    }

    bool descending = (dir == 0);

    /*get list of lists*/
    if (!enif_get_list_length(env, kvls, &kvls_len)) {
        return enif_make_badarg(env);
    }

    /*Declare a vector of deques of type kvp instances (Key/Value Pair)*/
    vector < deque <KeyValuePair> > kvlq;
    kvlq.reserve( kvls_len );
    /*Declare a deque of type kvp struct (Key/Value Pair) to use as max heap*/
    deque<KeyValuePair> maxheap;

    unsigned short int i = 0;
    while(enif_get_list_cell(env, kvls, &head, &tail)){
        if(!enif_get_list_length(env, head, &len)) {
            return enif_make_badarg(env);
        }
	deque <KeyValuePair> kvl; 
        bool at_head = true;
        while(enif_get_list_cell(env, head, &h, &t)){
            if(!enif_get_tuple(env, h, &arity, &tuple)) {
                return enif_make_badarg(env);
            }
            if(arity != 2 || !enif_inspect_binary(env, tuple[0], &keybin)) {
                return enif_make_badarg(env);
            }
            if(!enif_inspect_binary(env, tuple[1], &valuebin)) {
                return enif_make_badarg(env);
            }

	    KeyValuePair kvp(i, (const char*)keybin.data, (size_t) keybin.size, valuebin);

            if (at_head) {
                maxheap.push_back( kvp );
                at_head = false;    
            }
            else {
                kvl.push_back( kvp );
            }
            total_kvps++;
            head = t;
        }
	kvlq.push_back( kvl );
	i++;
        kvls = tail;
    }
   
    /*Make the vector containing first element of each list a heap*/
    KeyValuePair comp(descending);
    make_heap (maxheap.begin(), maxheap.end(), comp);
    
    /*Declare a vector that keeps Erlang NIF Term representations*/
    vector<ERL_NIF_TERM> merged_kvls;
    merged_kvls.reserve(total_kvps);

    /*Declare key and value erlang resources*/
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    /*Use int tag to keep track of an heap elements original vector*/
    unsigned short int tag = 0;
    while ( !maxheap.empty() ) {
        /*Get root elemenet of the heap and put into merged_kvls*/
        KeyValuePair kvp = maxheap.front();
        tag = kvp.tag();
        /*Construct key_term*/
        enif_alloc_binary(kvp.key_size(), &keybin);
        memcpy(keybin.data, kvp.key(), kvp.key_size());
        key_term = enif_make_binary(env, &keybin);
        /*Copy value_term*/
        valuebin = kvp.value();
	value_term = enif_make_binary(env, &valuebin);
        /*Push root of heap to merged_kvls vector */
        //merged_kvls.push_back( enif_make_tuple2(env, key_term, value_term) );
        merged_kvls.push_back( enif_make_tuple2(env, key_term, value_term) );
	/*Pop root element of the heap*/
        pop_heap ( maxheap.begin(), maxheap.end(), comp ); maxheap.pop_back();
        /*Push new element from kvl list of tag if not empty*/
        if ( !kvlq.at(tag).empty() ) {
            maxheap.push_back( kvlq.at(tag).front() ); 
            push_heap ( maxheap.begin(), maxheap.end(), comp );
            kvlq.at(tag).pop_front();
        }
    }
    kvl = enif_make_list_from_array(env, &merged_kvls[0], merged_kvls.size());
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), kvl);
}

static ERL_NIF_TERM sort_kvl_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    /*kvl: the pointer to list of key/value pairs passed from erlang*/
    ERL_NIF_TERM kvl = argv[1];
    unsigned int kvl_len;
    /*kvl: the resulting erlang nif term*/
    ERL_NIF_TERM sorted_kvl;

    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM* tuple;
    ErlNifBinary keybin, valuebin;
    int dir;

    /*get direction integer*/
    if (argc !=2 || !enif_get_int(env, argv[0], &dir)) {
	return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "direction"));
    }

    if (!enif_get_list_length(env, kvl, &kvl_len)) {
        return enif_make_badarg(env);
    }
    vector <KeyValuePair> kvp_vector;
    kvp_vector.reserve ( kvl_len );

    bool ascending = (dir != 0);
    unsigned short int i = 0;

    while(enif_get_list_cell(env, kvl, &head, &tail)){
	if(!enif_get_tuple(env, head, &arity, &tuple)) {
            return enif_make_badarg(env);
        }
        if(arity != 2 || !enif_inspect_binary(env, tuple[0], &keybin)) {
            return enif_make_badarg(env);
        }
        if(!enif_inspect_binary(env, tuple[1], &valuebin)) {
            return enif_make_badarg(env);
        }
	KeyValuePair kvp(i, (const char*)keybin.data, (size_t) keybin.size, valuebin);

        kvp_vector.push_back( kvp );
	kvl = tail;
    }
    /*Define comparator instance*/
    KeyValuePair comp(ascending);
    sort (kvp_vector.begin(), kvp_vector.end(), comp);

    /*Declare a vector that keeps Erlang NIF Term representations*/
    vector<ERL_NIF_TERM> kvp_terms;
    kvp_terms.reserve( kvl_len );

    /*Declare key and value erlang resources*/
    ERL_NIF_TERM key_term;
    ERL_NIF_TERM value_term;

    for (unsigned j=0; j<kvp_vector.size(); j++) {
        KeyValuePair kvp = kvp_vector.at(j);
        /*Construct key_term*/
        enif_alloc_binary(kvp.key_size(), &keybin);
        memcpy(keybin.data, kvp.key(), kvp.key_size());
        key_term = enif_make_binary(env, &keybin);
        /*Copy value_term*/
        valuebin = kvp.value();
	value_term = enif_make_binary(env, &valuebin);
        kvp_terms.push_back( enif_make_tuple2(env, key_term, value_term) );
    }

    sorted_kvl = enif_make_list_from_array(env, &kvp_terms[0], kvp_terms.size());

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), sorted_kvl);
}

static ErlNifFunc nif_funcs[] = {
    {"merge_sorted_kvls", 2, merge_sorted_kvls_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"sort_kvl", 2, sort_kvl_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(rocksdb_utils, nif_funcs, load, reload, upgrade, NULL)

