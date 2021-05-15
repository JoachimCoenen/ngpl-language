#include "unit.h"

namespace ngpl {

Unit::Unit() : _scope(nullptr)
{

}

Unit::Unit(const cat::String& name, ScopeSharedPtr&& scope, UnitNature unitNature)
	: _name(name),
	  _scope(std::move(scope)),
	  _unitNature(unitNature)
{ }

cat::WriterObjectABC& Unit::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	s << "unit " << name();

	WITH(cat::Indentation(s)) {
		scope()->print(s);
		s += cat::nl;
		body().print(s);
	} END_WITH;
	return s;
}

}
