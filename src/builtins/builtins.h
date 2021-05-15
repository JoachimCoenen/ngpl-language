#ifndef BUILTINS_H
#define BUILTINS_H

#include "compiler/language/function.h"
#include "compiler/language/type.h"
#include "compiler/language/typeReference.h"

#include "cat_string.h"

#include <unordered_map>

namespace ngpl {

extern const Scope builtins;


TypeReference getBuiltinTypeRef(const Scope& scope, const cat::String& name, std::vector<TypeReference>&& arguments, bool isReference);
inline TypeReference getBuiltinTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, bool isReference)
{
	return getBuiltinTypeRef(builtins, name, std::move(arguments), isReference);
}
TypeReference getBuiltinTypeRef(const Scope& scope, const cat::String& name, std::vector<TypeReference>&& arguments);
inline TypeReference getBuiltinTypeRef(const cat::String& name)
{
	return getBuiltinTypeRef(builtins, name, {});
}

// creates shorthands for all builtin types. i.e.:
// inline TypeReference NONE_TYPE() { return getBuiltinTypeRef("None"); }
// inline TypeReference FLOAT_TYPE() { return getBuiltinTypeRef("Float"); }
namespace builtinShorthands {
#define BUILTIN_TYPE_FACTORY(fixedSize, typeName, refereceMacroName) \
inline auto refereceMacroName##_TYPE() { return getBuiltinTypeRef(typeName); }
#include "builtinTypes_inc.h"
}

}
#endif // BUILTINS_H
