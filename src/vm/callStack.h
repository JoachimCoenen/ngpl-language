#ifndef NGPL_CALLSTACK_H
#define NGPL_CALLSTACK_H

#include "value.h"

#include "cat_exception.h"
#include "cat_DynArray.h"

namespace ngpl {


class BadStackError: public cat::Exception {
public:
	BadStackError(const cat::String& message)
		: Exception(message)
	{}
};

class StackUnderflowError: public BadStackError {
public:
	StackUnderflowError(const cat::String& message)
		: BadStackError("Stack Underflow: " + message)
	{}
};

class StackOverflowError: public BadStackError {
public:
	StackOverflowError(const cat::String& message)
		: BadStackError("Stack Overflow: " + message)
	{}
};

class CallStack
{
public:
	CallStack(size_t size)
		: _values({}, cat::DynArray<Value>(size, None{}))
	{}


private:
	cat::SharedPtr<cat::DynArray<Value>> _values;
	size_t _top = 0;

public:
	bool empty() const {
		return _top == 0;
	}

	size_t size() const {
		return _top;
	}

//	StackFrame& current() {
//		return _stackFrames.peek();
//	}

//	const StackFrame& current() const {
//		return _stackFrames.peek();
//	}
	cat::WeakPtr<cat::DynArray<Value>> values() {
		return _values.weak();
	}

	Value pop() {
		if (size() <= 0) {
			throw StackUnderflowError("The last stack frame cannot be removed. Use CallStack::clear() to clear the call stack.");
		}
		--_top;
		return _values.get()[_top];
	}

	Value& peek() { return _values.get()[_top-1]; }
	const Value& peek() const { return _values.get()[_top-1]; }

	void push(const Value& value) {
		_values.get()[_top] = value;
		++_top;
	}

	void push(Value&& value) {
		_values.get()[_top] = std::move(value);
		++_top;
	}

	void clear() {
		_top = 0;
	}

		  Value& operator[](size_t i)       { return _values.get()[i]; }
	const Value& operator[](size_t i) const { return _values.get()[i]; }

	Value& at(size_t i)
	{
		if (!(i < size())) {
			throw std::out_of_range("Index (" + std::to_string(i) + ") out of current stack bounds(" + std::to_string(i) + ")!");
		}
		return _values.get()[i];
	}

	const Value& at(size_t i) const
	{
		if (!(i < size())) {
			throw std::out_of_range("Index (" + std::to_string(i) + ") out of current stack bounds(" + std::to_string(i) + ")!");
		}
		return _values.get()[i];
	}

	auto begin() { return _values.get().begin(); }
	auto end() { return _values.get().begin() + size(); }

protected:
};


} // namespace ngpl

#endif // NGPL_CALLSTACK_H
