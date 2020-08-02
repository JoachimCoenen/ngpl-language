#ifndef TYPES_H
#define TYPES_H
// TODO: rename this file inorder to avoid confusion.


#include "cat_typing.h"

#include <string>
#include <vector>

/*  // forward declarations:
namespace std { // forward declarations:

template<typename _CharT, typename _Traits, typename _Alloc>
  class basic_string;

}
// */

namespace cat {

// forward decl:
class WriterObjectABC;

}

namespace ngpl {

using char_t = wchar_t;
using stdString_t = std::basic_string<char>;


template <class T>
using OwningPtrVector = std::vector<cat::OwningPtr<T>>;


template <class T>
using WeakPtrVector = std::vector<cat::WeakPtr<T>>;

#define ADD_PTR_TYPES(Cls)                          \
	using Cls##Ptr = cat::OwningPtr<Cls>;           \
	using Cls##CPtr = cat::OwningPtr<const Cls>;    \
	using Cls##WeakPtr = cat::WeakPtr<Cls>;         \
	using Cls##CWeakPtr = cat::WeakPtr<const Cls>;  \

#define PTRS_FOR_STRUCT(Cls)\
	struct Cls;             \
	ADD_PTR_TYPES(Cls)

#define PTRS_FOR_CLASS(Cls) \
	class Cls;              \
	ADD_PTR_TYPES(Cls)

#define STRUCT_WITH_PTR(Cls)\
	PTRS_FOR_STRUCT(Cls)    \
	struct Cls


class IIntermediateCodePrintable { // TODO: find better name for IIntermediateCodePrintable.
public:
	virtual ~IIntermediateCodePrintable() {}
	virtual cat::WriterObjectABC& print(cat::WriterObjectABC& s) const = 0;

};

}

#endif // TYPES_H
