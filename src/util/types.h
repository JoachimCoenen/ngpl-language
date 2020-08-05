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

class IIntermediateCodePrintable { // TODO: find better name for IIntermediateCodePrintable.
public:
	virtual ~IIntermediateCodePrintable() {}
	virtual cat::WriterObjectABC& print(cat::WriterObjectABC& s) const = 0;

};

}

#endif // TYPES_H
