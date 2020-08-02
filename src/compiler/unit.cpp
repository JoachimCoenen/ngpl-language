#include "unit.h"

namespace ngpl {

Unit::Unit()
{

}

Unit::Unit(const std::string& name, cat::OwningPtr<Scope>&& scope, UnitNature unitNature)
	: _name(name),
	  _scope(std::move(scope)),
	  _unitNature(unitNature)
{ }

cat::WriterObjectABC& Unit::print(cat::WriterObjectABC& s) const
{
	s += "unit ";
	s += name();

	s.incIndent();
	{
		s+= cat::nlIndent;
		scope()->print(s);
		s += cat::nl;
		InstructionsContainer::print(s);
	}
	s.decIndent();

   return s;
}

}
