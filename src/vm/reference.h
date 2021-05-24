#ifndef NGPL_REFERENCE_H
#define NGPL_REFERENCE_H

#include "value.h"
#include "util/types.h"

#include "toStringUtils.h"
#include "cat_DynArray.h"

namespace ngpl {

class Reference
{
private:
	using SrcPtr = cat::WeakPtr<cat::DynArray<Value>>;

private:
	SrcPtr _source;
	Address _offset;

public:

	explicit Reference(const SrcPtr& array)
		: Reference(array, 0)
	{}

	Reference(const SrcPtr& array, size_t offset)
		: _source(array),
		  _offset(offset)
	{}

	SrcPtr source() const { return _source; }
	Address offset() const { return _offset; }

	Value& get() const { return _source.get()[_offset]; }

	Reference& operator ++() { ++_offset; return *this; }
	Reference& operator --() { --_offset; return *this; }

	Reference operator ++(int) { Reference tmp(*this); operator++(); return tmp; }
	Reference operator --(int) { Reference tmp(*this); operator--(); return tmp; }

	Reference operator +(Address rhs) const { return Reference(_source, _offset + rhs); }
	Reference operator -(Address rhs) const { return Reference(_source, _offset - rhs); }

	Reference& operator +=(Address rhs) { _offset += rhs; return *this; }
	Reference& operator -=(Address rhs) { _offset -= rhs; return *this; }

	Value& at(Address n) const { return ((*this) + n).get(); } // TODO: bounds checking
	Value& operator[](Address n) const { return ((*this) + n).get(); }
};

inline bool operator == (const Reference& lhs, const Reference& rhs) {
	return lhs.source() == rhs.source() and lhs.offset() == rhs.offset();
}

inline bool operator != (const Reference& lhs, const Reference& rhs) {
	return not (lhs == rhs);
}

inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Reference& ref) {
	s += "Ref(";
	s += "0x" + cat::intToHex(reinterpret_cast<uintptr_t>(ref.source().getPtr()), 8);
	s += ", ";
	s += ref.offset();
	s += ")";
	return s;
}

} // namespace ngpl

#endif // NGPL_REFERENCE_H
