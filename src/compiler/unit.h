#ifndef UNIT_H
#define UNIT_H

#include "intermediate/intermediateCode.h"
#include "scope.h"
#include "../language/unitNature.h"
//#include "../util/types.h"

#include "cat_utils.h"
#include "cat_hash.h"

#include <string>

namespace ngpl {

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const UnitNature& v);



PTRS_FOR_CLASS(Unit)
class Unit: public IIntermediateCodePrintable
{
public:
	Unit();
	Unit(const std::string& name, ScopePtr&& scope, UnitNature unitNature);

	const std::string& name() const { return _name; }
	ScopePtr& scope() { return _scope; }
	ScopeCWeakPtr scope() const { return _scope.getRaw(); }
	const intermediate::IntermediateCodeContainer& body() const { return _body; }
	intermediate::IntermediateCodeContainer& body() { return _body; }
	UnitNature unitNature() const { return _unitNature; }

//	bool isGlobal() const override {
//		return true;
//	}

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;

protected:
	std::string _name;
	ScopePtr _scope;
	intermediate::IntermediateCodeContainer _body;
	UnitNature _unitNature;
};

}

#endif // UNIT_H
