
#if defined(DEBUG_ASSERTIONS)
	// #undef DEBUG_ASSERTIONS
#else // defined(DEBUG_ASSERTIONS)
	#define DEBUG_ASSERTIONS 1
#endif // defined(DEBUG_ASSERTIONS)

#ifndef DEBUG_H
#define DEBUG_H

#include "cat_utils.h"

#include <string>
#include <exception>

#include <cassert>

namespace ngpl::util::debug {

class AssertionError: public cat::Exception {
public:
	AssertionError(const std::string& message, const std::string& filePath, uint32_t lineNo)
	: Exception(message + "\n  AssertionError in file: '" + filePath + "', at line " + std::to_string(lineNo) + ".")
	{}
};

#if defined(_UNICODE) || defined(UNICODE)
static inline void ngpl_assert(bool condition, const std::string& msg, const wchar_t* fileName, unsigned lineNo) {
	if (not condition) {
	const std::wstring fileNameWs(fileName);
	const std::string fileNameS(fileNameWs.begin(), fileNameWs.end());
	throw AssertionError(msg, fileNameS, lineNo);
	}
}
#else // defined(_UNICODE) || defined(UNICODE)
static inline void ngpl_assert(bool condition, const std::string& msg, const char* fileName, unsigned lineNo) {
	if (not condition) {
	const std::string fileNameS(fileName);
	throw AssertionError(msg, fileNameS, lineNo);
	}
}
#endif // defined(_UNICODE) || defined(UNICODE)

static inline void NOP() {} // null operation

}

#endif // DEBUG_H


#undef NGPL_ASSERT
#if DEBUG_ASSERTIONS
	#if defined(_UNICODE) || defined(UNICODE)
	#define NGPL_ASSERT(condition) ngpl::util::debug::ngpl_assert(condition, #condition, _CRT_WIDE(__FILE__), __LINE__);
	#define NGPL_ASSERT2(condition, msg) ngpl::util::debug::ngpl_assert(condition, msg, _CRT_WIDE(__FILE__), __LINE__);
	#else // defined(_UNICODE) || defined(UNICODE)
	#define NGPL_ASSERT(condition) ngpl::util::debug::ngpl_assert(condition, #condition, __FILE__, __LINE__);
	#define NGPL_ASSERT2(condition, msg) ngpl::util::debug::ngpl_assert(condition, msg, __FILE__, __LINE__);
	#endif // defined(_UNICODE) || defined(UNICODE)

#else // DEBUG_ASSERTIONS
	#define NGPL_ASSERT(condition, msg) ngpl::util::debug::NOP();
#endif // DEBUG_ASSERTIONS












