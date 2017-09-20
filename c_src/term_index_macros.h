#pragma once
static const uint32_t pCIDLen = 2;
static const uint32_t pTSLen = 4;
static const uint32_t pSortLen = 8;
static const uint32_t pFreqLen = 4;
static const uint32_t pPosLen = 4;
static const uint32_t pPosOffset =  pTSLen + pPosLen;
static const uint32_t pFreqOffset = pPosOffset + pFreqLen;
static const uint32_t pSortOffset = pFreqOffset + pSortLen;
static const uint32_t pPrefixLen = 4;
static const uint32_t pStatsLen = pFreqLen + pPosLen;
static const uint32_t pSuffixLen = pStatsLen + pTSLen;
static const uint32_t pExtLen = pPrefixLen + pSuffixLen;
