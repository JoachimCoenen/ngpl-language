#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "../util/types.h"

namespace ngpl {

PTRS_FOR_CLASS(Unit)

class Optimizer
{
public:
	Optimizer() {}

	void optimize(const UnitWeakPtr& unit);

	//UnitWeakPtr _unit;
};

}

#endif // OPTIMIZER_H
