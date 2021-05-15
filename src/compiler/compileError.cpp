#include "compileError.h"

#include "toStringUtils.h"

namespace ngpl {

CompileError::CompileError(const std::optional<Position>& pos)
	: Exception(nullptr),
	  _pos(pos)
{}

const cat::String& CompileError::message() const
{
	if (_message == nullptr) {
		cat::SW s;
		s << rawMessage();
		if (_pos) {
			s << " at line " << _pos->line() + 1 << ", col " << _pos->column() + 1;
		}
		s << ".";
		_message = decltype (_message){{}, std::move(s)};
	}

	return *_message;
}

WrappingCompileError::WrappingCompileError(cat::OwningPtr<std::exception>&& ex, const std::optional<Position>& pos)
	: CompileError(pos),
	  _ex(ex.extractPtr()),
	  _rawMessage(nullptr)
{}

const cat::String& WrappingCompileError::rawMessage() const
{
	if (_rawMessage == nullptr) {
		_rawMessage = decltype (_rawMessage){{}, _ex->what()};
	}
	return *_rawMessage;
}

} // namespace ngpl
