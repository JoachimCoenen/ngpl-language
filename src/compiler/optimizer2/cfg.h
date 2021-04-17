#ifndef CFG_H
#define CFG_H

#include "cat_typing.h"

#include <vector>

namespace ngpl::optimizer {

PTRS_FOR_CLASS(CFG)
class CFG
{

	std::vector<CFGPtr> _children;
public:
	CFG();

	const std::vector<CFGWeakPtr>& parent();
	const std::vector<CFGPtr>& children();

	void removeChild(size_t index) {

	}

};

}

#endif // CFG_H
