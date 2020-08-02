#ifndef UNITNATURE_H
#define UNITNATURE_H

#include "toStringUtils.h"

namespace ngpl {

enum class UnitNature {
	UNIT,
	PROGRAM,
	PACKAGE,
	LIBRARY,
};
inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const UnitNature& v) {
	switch (v) {
		FORMAT_ENUM_VAL_CASE(UnitNature, UNIT);
		FORMAT_ENUM_VAL_CASE(UnitNature, PROGRAM);
		FORMAT_ENUM_VAL_CASE(UnitNature, PACKAGE);
		FORMAT_ENUM_VAL_CASE(UnitNature, LIBRARY);
	}
	return s;
}
}

#endif // UNITNATURE_H
