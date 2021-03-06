//#ifndef BUILTINTYPES_INC_H
//#define BUILTINTYPES_INC_H

#ifndef BUILTIN_TYPE_FACTORY
	#define __BUILTIN_TYPE_FACTORY_WAS_DEFINED__ 0
	#define BUILTIN_TYPE_FACTORY(fixedSize, typeName, refereceMacroName)
	#error you gotta define BUILTIN_TYPE_FACTORY
#else
	#define __BUILTIN_TYPE_FACTORY_WAS_DEFINED__ 1
#endif

#if 0 == (__BUILTIN_TYPE_FACTORY_WAS_DEFINED__)
	#define __JUST_FOR_HIGHLIGHTING__ 1
#endif

BUILTIN_TYPE_FACTORY(0, "None",     NONE    )
BUILTIN_TYPE_FACTORY(0, "Any",      ANY     )

BUILTIN_TYPE_FACTORY(1, "Bool",     BOOL    )

BUILTIN_TYPE_FACTORY(1,  "Int8",     INT8   )
BUILTIN_TYPE_FACTORY(1, "UInt8",    UINT8   )
BUILTIN_TYPE_FACTORY(1,  "Int16",    INT16  )
BUILTIN_TYPE_FACTORY(1, "UInt16",   UINT16  )
BUILTIN_TYPE_FACTORY(1,  "Int",      INT    )
BUILTIN_TYPE_FACTORY(1, "UInt",     UINT    )
BUILTIN_TYPE_FACTORY(1,  "Int64",    INT64  )
BUILTIN_TYPE_FACTORY(1, "UInt64",   UINT64  )

BUILTIN_TYPE_FACTORY(1, "Float",    FLOAT   )
BUILTIN_TYPE_FACTORY(1, "Float64",  FLOAT64 )
BUILTIN_TYPE_FACTORY(1, "Float128", FLOAT128)

BUILTIN_TYPE_FACTORY(1, "String",   STRING  )


#ifdef __JUST_FOR_HIGHLIGHTING__
	#undef __JUST_FOR_HIGHLIGHTING__
#endif

#undef BUILTIN_TYPE_FACTORY

//#endif // BUILTINTYPES_INC_H
