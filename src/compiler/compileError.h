#ifndef NGPL_COMPILEERROR_H
#define NGPL_COMPILEERROR_H

#include "../language/position.h"
#include "cat_exception.h"


#if defined(DEBUG_ASSERTIONS)
	// #undef DEBUG_ASSERTIONS
#else // defined(DEBUG_ASSERTIONS)
	#define DEBUG_ASSERTIONS 1
#endif // defined(DEBUG_ASSERTIONS)

namespace ngpl {

PTRS_FOR_CLASS(CompileError)
class CompileError : public cat::Exception
{
protected:
	CompileError(const std::optional<Position>& pos);

public:
	const std::optional<Position>& pos() const { return _pos; }

	const cat::String& message() const override;
	virtual const cat::String& rawMessage() const = 0;

	virtual CompileError* makeCopy() const override {
		throw std::logic_error("cannot make copy of an abstract class CompileError. You must override this method in subclasses");
	}

protected:
	std::optional<Position> _pos;
};


class WrappingCompileError : public CompileError
{
public:
	WrappingCompileError(cat::OwningPtr<std::exception>&& ex, const std::optional<Position>& pos);

	const cat::String& rawMessage() const override;
	const cat::WeakPtr<const std::exception> pos() const { return _ex.weak(); }

	virtual WrappingCompileError* makeCopy() const override {
		return new WrappingCompileError(*this);
	}

protected:
	cat::SharedPtr<std::exception> _ex;
	mutable cat::SharedPtr<cat::String> _rawMessage;
};


class CompileAssertionError final : public CompileError {
public:
	CompileAssertionError(const cat::String& message, const std::optional<Position>& pos, const cat::String& filePath, uint32_t lineNo)
	: CompileError(pos),
	  _rawMessage({}, message + "\n  AssertionError in file: '" + filePath + "', at line " + std::to_string(lineNo) + ".")
	{}

public:
	const cat::String& rawMessage() const override { return *_rawMessage; }

	virtual CompileAssertionError* makeCopy() const override {
		return new CompileAssertionError(*this);
	}
protected:
	mutable cat::SharedPtr<cat::String> _rawMessage;
};


#if defined(_UNICODE) || defined(UNICODE)
static inline void ngpl_compiler_assert(bool condition, const cat::String& msg, const std::optional<Position>& pos, const wchar_t* fileName, unsigned lineNo) {
	if (not condition) {
		const std::wstring fileNameWs(fileName);
		const cat::String fileNameS(fileNameWs.begin(), fileNameWs.end());
		throw CompileAssertionError(msg, pos, fileNameS, lineNo);
	}
}
#else // defined(_UNICODE) || defined(UNICODE)
static inline void ngpl_compiler_assert(bool condition, const cat::String& msg, const std::optional<Position>& pos, const char* fileName, unsigned lineNo) {
	if (not condition) {
		const cat::String fileNameS(fileName);
		throw CompileAssertionError(msg, pos, fileNameS, lineNo);
	}
}
#endif // defined(_UNICODE) || defined(UNICODE)

static inline void NOP() {} // null operation

} // namespace ngpl

#endif // NGPL_COMPILEERROR_H


#undef NGPL_COMPILER_ASSERT
#if DEBUG_ASSERTIONS
	#if defined(_UNICODE) || defined(UNICODE)
		#define NGPL_COMPILER_ASSERT(condition, pos) ngpl::ngpl_compiler_assert(condition, #condition, pos, _CRT_WIDE(__FILE__), __LINE__)
		#define NGPL_COMPILER_ASSERT2(condition, msg, pos) ngpl::ngpl_compiler_assert(condition, msg, pos, _CRT_WIDE(__FILE__), __LINE__)
	#else // defined(_UNICODE) || defined(UNICODE)
		#define NGPL_COMPILER_ASSERT(condition, pos) ngpl::ngpl_compiler_assert(condition, #condition, pos, __FILE__, __LINE__);
		#define NGPL_COMPILER_ASSERT2(condition, msg, pos) ngpl::ngpl_compiler_assert(condition, msg, pos, __FILE__, __LINE__);
	#endif // defined(_UNICODE) || defined(UNICODE)

#else // DEBUG_ASSERTIONS
	#define NGPL_COMPILER_ASSERT(condition, pos) ngpl::NOP();
	#define NGPL_COMPILER_ASSERT2(condition, msg, pos) ngpl::NOP();
#endif // DEBUG_ASSERTIONS

