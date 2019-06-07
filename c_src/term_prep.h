#pragma once
#include "utilities/ttl/db_ttl_impl.h"
#include "term_index_macros.h"
#include "rocksdb_nif_resources.h"
#include "rocksdb_nif.h"

class TermPrep {
    public:
	TermPrep(const std::vector<std::pair<Term, std::vector<Term>>> indices,
		 const rocksdb::Slice* k) {
	    env_ = rocksdb::Env::Default();
	    auto posting_len = k->size() + pExtLen;
	    int64_t curtime;
	    if (!env_->GetCurrentTime(&curtime).ok()) { curtime = 0; }
	    char ts_str[pTSLen];
	    char len_str[pPrefixLen];
	    rocksdb::EncodeFixed32(ts_str, (uint32_t)curtime);
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    for (auto it = indices.begin(); it != indices.end(); ++it){
		std::string terms_str;
		Term cid = it->first;
		cids.push_back(cid);
		std::vector<Term> terms = it->second;
		for (auto tit = terms.begin(); tit != terms.end(); ++tit){
		    //Prepare posting 'p'
		    std::string p;
		    p.reserve(posting_len);
		    //Copy key->data to 'p' and append Freq and Pos bytes.
		    p.append( len_str, pPrefixLen );
		    p.append( k->data(), k->size() );
		    p.append( tit->data + (tit->size - pStatsLen), pStatsLen );
		    p.append( ts_str, pTSLen );
		    //Prepare term 't'
		    size_t term_len = tit->size - pStatsLen;
		    std::string t;
		    t.reserve(term_len + pCIDLen);
		    //First 2 bytes are encoding Cid
		    t.append(cid.data, pCIDLen);
		    //Last 8 bytes are encoding Freq and Pos statistics
		    //Do not copy the last 8 bytes to 't' and 'terms_str_'
		    t.append(tit->data, term_len);
		    rev_index_.push_back( std::make_pair(std::move(t), std::move(p)) );
		    char term_len_str[pPrefixLen];
		    rocksdb::EncodeFixed32(term_len_str, (uint32_t)term_len);
		    terms_str.append( term_len_str, pPrefixLen );
		    terms_str.append( tit->data, term_len );
		}
		std::string key2term;
		key2term.append(cid.data, pCIDLen);
		key2term.append(k->data(), k->size());
		index_.push_back( std::make_pair(std::move(key2term), std::move(terms_str)) );
	    }
	};

	~TermPrep() {};

	void add_to_batch(db_obj_resource* rdb, rocksdb::WriteBatch &batch) {
	    //Merge for keeping index history
	    for (auto it = index_.begin(); it != index_.end(); ++it){
		batch.Put(rdb->handles->at(1),
			  rocksdb::Slice(it->first),
			  rocksdb::Slice(it->second));
	    }
	    //Merges for reverse indexes
	    for (auto it = rev_index_.begin(); it != rev_index_.end(); ++it){
		batch.Merge(rdb->handles->at(2),
			    rocksdb::Slice(it->first),
			    rocksdb::Slice(it->second));
	    }
	}

	std::vector<Term> cids;

    private:
	rocksdb::Env* env_;
	std::vector<std::pair<std::string, std::string>> rev_index_;
	std::vector<std::pair<std::string, std::string>> index_;
	static int IsNotAlfaNumericOrSpace (int c) {
	    return !(std::isalnum(c) || std::isspace(c));
	}
};

class TermDelete {
    public:
	TermDelete(const std::vector<Term> cids,
		   const rocksdb::Slice* k) {
	    auto posting_len = k->size() + pExtLen;
	    char len_str[pPrefixLen];
	    rocksdb::EncodeFixed32(len_str, (uint32_t)posting_len);
	    posting_.append(len_str, pPrefixLen);
	    posting_.append(k->data(), k->size());
	    posting_.append(remove_, pSuffixLen);

	    for (auto it = cids.begin(); it != cids.end(); ++it){
		Term cid = *it;
		std::string key2term;
		key2term.append(cid.data, pCIDLen);
		key2term.append(k->data(), k->size());
		index_.push_back( std::move(key2term) );
	    }
	};

	~TermDelete() {};

	void ParseReveseIndex(const std::string cid_key,
			      const rocksdb::PinnableSlice* terms) {
	    const char* str = terms->data();
	    auto size = terms->size();
	    uint32_t j = 0;
	    // Parse terms until last 4 bytes (timestamp)
	    while( j < size ) {
		auto pos =  str + j;
		auto term_len = rocksdb::DecodeFixed32(pos);
		std::string term2key;
		term2key.append(cid_key, 0, pCIDLen);
		term2key.append(pos+pPrefixLen, term_len);
		rev_index_.push_back(std::move(term2key));
		j += (pPrefixLen+term_len);
	    }
	}

	void add_to_batch(db_obj_resource* rdb, rocksdb::WriteBatch &batch) {
	    rocksdb::Status status;
	    rocksdb::ReadOptions readoptions;

	    //Delete any index history
	    for (auto it = index_.begin(); it != index_.end(); ++it) {
		rocksdb::Slice key2term(*it);
		rocksdb::PinnableSlice value;
		status = Get(rdb, &readoptions, 1, &key2term, &value);
		if (status.ok()) {
		    ParseReveseIndex(*it, &value);
		}
		batch.Delete(rdb->handles->at(1), key2term);
	    }
	    //Merges for reverse indexes (deleted)
	    for (auto it = rev_index_.begin(); it != rev_index_.end(); ++it){
		batch.Merge(rdb->handles->at(2),
			    rocksdb::Slice(*it),
			    rocksdb::Slice(posting_));
	    }
	}

    private:
	const char remove_[pSuffixLen] = {0};
	std::string posting_;
	std::vector<std::string> index_;
	std::vector<std::string> rev_index_;
};
