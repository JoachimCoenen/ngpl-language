#include "member.h"
#include "toStringUtils.h"

namespace ngpl {

//Member::Member()
//{

//}

std::string Member::asCodeString() const
{
	return cat::SW() << name();
}

std::string Member::asQualifiedCodeString() const
{
	return cat::SW() << qualifier() << name();
}

}
