#ifndef NGPL_TYPEDEFINITION_H
#define NGPL_TYPEDEFINITION_H

#include "language/ast.h"
#include "intermediate/intermediateCode.h"
#include "member.h"
#include "type.h"

namespace ngpl {
class TypeReference;

class TypeParameterPtr {

};

class TypeDefinition: public IIntermediateCodePrintable, public Member
{
private:
	std::vector<TypeParameterPtr> _parameters;
	std::vector<DeclarationPtr> _members;
public:
	TypeDefinition(std::vector<DeclarationPtr> _members);


	std::vector<DeclarationPtr>& members() { return _members; }

	TypePtr construct(std::vector<TypeReference>& typeArguments);

};

} // namespace ngpl

#endif // NGPL_TYPEDEFINITION_H
