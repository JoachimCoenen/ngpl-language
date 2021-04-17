#ifndef VALUE_H
#define VALUE_H

#include "util/types.h"

#include "cat_typing.h"
#include "toStringUtils.h"
#include "cat_variant.h"
#include "cat_DynArray.h"

namespace ngpl {
//#define DEBUG_VALUE 1

//#if DEBUG_VALUE
//#endif

struct None {};
inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const None&) {
	return s += "none";
}

class Reference;

using Value = cat::Variant<None, int64_t, double, cat::String, Reference>;
ADD_PTR_TYPES(Value)

}

#include "reference.h"

namespace ngpl {

//template <class... _Ts>
//struct Value_: public cat::Variant<_Ts...> {
//	using Variant = cat::Variant<_Ts...>;
//	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
//	constexpr Value_(const _T& v): Variant(v)
//	{ }

//	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
//	constexpr Value_(_T&& v): Variant(std::move(v))
//	{ }


//	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
//	constexpr auto& operator =(const _T& v) {
//		this->content = v;
//	}

//	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
//	constexpr auto& operator =(_T&& v) {
//		this->content = std::move(v);
//	}
//};

}

#endif // VALUE_H
