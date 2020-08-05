#include "unit.h"

namespace ngpl {

Unit::Unit()
{

}

Unit::Unit(const std::string& name, ScopePtr&& scope, UnitNature unitNature)
	: _name(name),
	  _scope(std::move(scope)),
	  _unitNature(unitNature)
{ }

cat::WriterObjectABC& Unit::print(cat::WriterObjectABC& s) const
{
	s+= cat::nlIndent;
	s += "unit ";
	s += name();

	s.incIndent();
	{
		scope()->print(s);
		s += cat::nl;
		InstructionsContainer::print(s);
	}
	s.decIndent();

   return s;
}

}
