#ifndef UNIT_H
#define UNIT_H

#include "instructionsContainer.h"
#include "scope.h"
#include "../language/unitNature.h"

//#include "../util/types.h"

#include "cat_utils.h"
#include "cat_hash.h"

#include <string>

namespace ngpl {

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const UnitNature& v);



PTRS_FOR_CLASS(Unit)
class Unit: public InstructionsContainer
{
public:
	Unit();
	Unit(const std::string& name, cat::OwningPtr<Scope>&& scope, UnitNature unitNature);

	const std::string& name() const { return _name; }
	cat::OwningPtr<Scope>& scope() { return _scope; }
	cat::WeakPtr<const Scope> scope() const { return _scope.getRaw(); }
	UnitNature unitNature() const { return _unitNature; }

	bool isGlobal() const override {
		return true;
	}

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;

protected:
	std::string _name;
	cat::OwningPtr<Scope> _scope;
	UnitNature _unitNature;
};

}

#endif // UNIT_H
