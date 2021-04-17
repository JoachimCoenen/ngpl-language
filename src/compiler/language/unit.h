#ifndef UNIT_H
#define UNIT_H

#include "intermediate/intermediateCode.h"
#include "scope.h"
#include "language/unitNature.h"
//#include "util/types.h"

#include "cat_hash.h"
#include "cat_utils.h"
#include "cat_string.h"

namespace ngpl {

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const UnitNature& v);



PTRS_FOR_CLASS(Unit)
class Unit: public IIntermediateCodePrintable
{
public:
	Unit();
	Unit(const cat::String& name, ScopeSharedPtr&& scope, UnitNature unitNature);

	const cat::String& name() const { return _name; }
	ScopeWeakPtr scope() { return _scope.weak(); }
	ScopeCWeakPtr scope() const { return _scope.weak(); }
	void setScope(const ScopeSharedPtr& newScope) { _scope = newScope; }
	const intermediate::IntermediateCodeContainer& body() const { return _body; }
	intermediate::IntermediateCodeContainer& body() { return _body; }
	UnitNature unitNature() const { return _unitNature; }

//	bool isGlobal() const override {
//		return true;
//	}

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;

protected:
	cat::String _name;
	ScopeSharedPtr _scope;
	intermediate::IntermediateCodeContainer _body;
	UnitNature _unitNature;
};

}

#endif // UNIT_H
