#ifndef TYPES_H
#define TYPES_H
// TODO: rename this file inorder to avoid confusion.


#include "cat_DynArray.h"
#include "cat_string.h"
#include "toStringUtils.h"

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


template <class T>
auto& operator += (WriterObjectABC &s, const cat::DynArray<T> &v) {
	formatListLike2(s, v.cbegin(), v.cend(), {"[", "]"});
	return s;
}
}



namespace ngpl {

using Address = int64_t;

class IIntermediateCodePrintable { // TODO: find better name for IIntermediateCodePrintable.
public:
	virtual ~IIntermediateCodePrintable() {}
	virtual cat::WriterObjectABC& print(cat::WriterObjectABC& s) const = 0;

};

}

#endif // TYPES_H
