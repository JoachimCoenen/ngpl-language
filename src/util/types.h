#ifndef TYPES_H
#define TYPES_H
// TODO: rename this file in order to avoid confusion.

#include "cat_DynArray.h"
#include "catPointers.h"
#include "cat_string.h"
#include "cat_variant.h"
//#include "toStringUtils.h"


/*  // forward declarations:
namespace std { // forward declarations:

template<typename _CharT, typename _Traits, typename _Alloc>
  class basic_string;

}
// */

namespace cat {

// forward decl:
class WriterObjectABC;


template <class T>
auto& operator += (WriterObjectABC &s, const cat::DynArray<T> &v) {
	formatListLike2(s, v.cbegin(), v.cend(), {"[", "]"});
	return s;
}
}

namespace ngpl {


using InstructionPos = int64_t;


using Address = int64_t;


struct StackAddr
{
private:
	int64_t _v;

public:
	StackAddr(): _v(0) {}
	explicit StackAddr(Address v): _v(v) {}

	//explicit operator Address() const { return _v; }
	explicit operator const Address&() const { return _v; }

	inline StackAddr& operator+=(Address other) { _v += other; return *this; }
	inline StackAddr& operator-=(Address other) { _v -= other; return *this; }
	inline StackAddr& operator++() { _v++; return *this; }
	inline StackAddr operator++(int) { auto r = *this; _v++; return r; }
	inline StackAddr& operator--() { _v--; return *this; }
	inline StackAddr operator--(int) { auto r = *this; _v--; return r; }
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const StackAddr& v);
inline StackAddr operator "" _sa(unsigned long long int v) { return StackAddr(v); }
inline StackAddr operator+(const StackAddr& lhs, Address rhs) { return StackAddr(Address(lhs) + rhs); }
inline StackAddr operator-(const StackAddr&lhs, Address rhs) { return StackAddr(Address(lhs) - rhs); }
inline Address operator-(const StackAddr&lhs, StackAddr rhs) { return Address(lhs) - Address(rhs); }
inline bool operator==(const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) == Address(rhs); }
inline bool operator!=(const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) != Address(rhs); }
inline bool operator< (const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) <  Address(rhs); }
inline bool operator> (const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) >  Address(rhs); }
inline bool operator<=(const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) <= Address(rhs); }
inline bool operator>=(const StackAddr&lhs, const StackAddr&rhs) { return Address(lhs) >= Address(rhs); }


struct FrameAddr
{
private:
	int64_t _v;

public:
	FrameAddr(): _v(0) {}
	explicit FrameAddr(Address v): _v(v) {}

	//explicit operator Address() const { return _v; }
	explicit operator const Address&() const { return _v; }

	inline FrameAddr& operator+=(const FrameAddr& other) { _v += other._v; return *this; } // special only for FrameAddr!
	inline FrameAddr& operator+=(Address other) { _v += other; return *this; }
	inline FrameAddr& operator-=(Address other) { _v -= other; return *this; }
	inline FrameAddr& operator++() { _v++; return *this; }
	inline FrameAddr operator++(int) { auto r = *this; _v++; return r; }
	inline FrameAddr& operator--() { _v--; return *this; }
	inline FrameAddr operator--(int) { auto r = *this; _v--; return r; }
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const FrameAddr& v);
inline FrameAddr operator "" _fa(unsigned long long int v) { return FrameAddr(v); }
inline FrameAddr operator+(const FrameAddr& lhs, Address rhs) { return FrameAddr(Address(lhs) + rhs); }
inline FrameAddr operator+(const FrameAddr& lhs, FrameAddr rhs) { return FrameAddr(Address(lhs) + Address(rhs)); } // special only for FrameAddr!
inline FrameAddr operator-(const FrameAddr&lhs, Address rhs) { return FrameAddr(Address(lhs) - rhs); }
inline Address operator-(const FrameAddr&lhs, FrameAddr rhs) { return Address(lhs) - Address(rhs); }
inline bool operator==(const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) == Address(rhs); }
inline bool operator!=(const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) != Address(rhs); }
inline bool operator< (const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) <  Address(rhs); }
inline bool operator> (const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) >  Address(rhs); }
inline bool operator<=(const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) <= Address(rhs); }
inline bool operator>=(const FrameAddr&lhs, const FrameAddr&rhs) { return Address(lhs) >= Address(rhs); }


struct HeapAddr
{
private:
	int64_t _v;

public:
	HeapAddr(): _v(0) {}
	explicit HeapAddr(Address v): _v(v) {}

	//explicit operator Address() const { return _v; }
	explicit operator const Address&() const { return _v; }

	inline HeapAddr& operator+=(Address other) { _v += other; return *this; }
	inline HeapAddr& operator-=(Address other) { _v -= other; return *this; }
	inline HeapAddr& operator++() { _v++; return *this; }
	inline HeapAddr operator++(int) { auto r = *this; _v++; return r; }
	inline HeapAddr& operator--() { _v--; return *this; }
	inline HeapAddr operator--(int) { auto r = *this; _v--; return r; }
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const HeapAddr& v);
inline HeapAddr operator "" _ha(unsigned long long int v) { return HeapAddr(v); }
inline HeapAddr operator+(const HeapAddr& lhs, Address rhs) { return HeapAddr(Address(lhs) + rhs); }
inline HeapAddr operator-(const HeapAddr&lhs, Address rhs) { return HeapAddr(Address(lhs) - rhs); }
inline Address operator-(const HeapAddr&lhs, HeapAddr rhs) { return Address(lhs) - Address(rhs); }
inline bool operator==(const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) == Address(rhs); }
inline bool operator!=(const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) != Address(rhs); }
inline bool operator< (const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) <  Address(rhs); }
inline bool operator> (const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) >  Address(rhs); }
inline bool operator<=(const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) <= Address(rhs); }
inline bool operator>=(const HeapAddr&lhs, const HeapAddr&rhs) { return Address(lhs) >= Address(rhs); }


struct None_ {};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const None_&);


// void* is a non-owning ptr to a ngpl::BuiltinFunction object.
using InstructionData = cat::Variant<None_, int64_t, cat::String, StackAddr, HeapAddr, const void*>;

class IIntermediateCodePrintable
{ // TODO: find better name for IIntermediateCodePrintable.
public:
	virtual ~IIntermediateCodePrintable() {}
	virtual cat::WriterObjectABC& print(cat::WriterObjectABC& s) const = 0;

};

PTRS_FOR_CLASS(NGPLPrivateBase);
class NGPLPrivateBase
{
protected:
	NGPLPrivateBase() {};
public:
	virtual ~NGPLPrivateBase() {};
};

}

#endif // TYPES_H
