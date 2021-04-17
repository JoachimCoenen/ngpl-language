#include "member.h"
#include "toStringUtils.h"

namespace ngpl {

//Member::Member()
//{

//}

cat::String Member::asCodeString() const
{
	return cat::SW() << name();
}

cat::String Member::asQualifiedCodeString() const
{
	return cat::SW() << qualifier() << name();
}

}
