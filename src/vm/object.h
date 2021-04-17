#ifndef OBJECT_H
#define OBJECT_H

#include "value.h"

#include "catPointers.h"
#include "cat_DynArray.h"

namespace ngpl {


class Object
{
public:
	Object(size_t size);

protected:
	cat::DynArray<Value> _content;
};


}

#endif // OBJECT_H
