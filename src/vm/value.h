#ifndef VALUE_H
#define VALUE_H

#include "../util/types.h"

#include "cat_typing.h"
#include "toStringUtils.h"
#include "cat_variant.h"

namespace ngpl {
//#define DEBUG_VALUE 1

//#if DEBUG_VALUE
//#endif

struct None {};
inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const None&) {
	return s += "none";
}

/*
template <class... _Ts>
struct Value_: public cat::Variant<_Ts...> {
	using Variant = cat::Variant<_Ts...>;
	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr Value_(const _T& v): Variant(v)
  #if DEBUG_VALUE
	  , repr(cat::formatVal(*this))
  #endif
	{ }

	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr Value_(_T&& v): Variant(std::move(v))
  #if DEBUG_VALUE
	  , repr(cat::formatVal(*this))
  #endif
	{ }


	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr auto& operator =(const _T& v) {
		this->content = v;
#if DEBUG_VALUE
		repr = cat::formatVal(*this);
#endif
	}

	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr auto& operator =(_T&& v) {
		this->content = std::move(v);
#if DEBUG_VALUE
		repr = cat::formatVal(*this);
#endif
	}

#if DEBUG_VALUE
	std::string repr;
#endif


};
*/

template <class... _Ts>
struct Value_: public cat::Variant<_Ts...> {
	using Variant = cat::Variant<_Ts...>;
	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr Value_(const _T& v): Variant(v)
	{ }

	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr Value_(_T&& v): Variant(std::move(v))
	{ }


	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr auto& operator =(const _T& v) {
		this->content = v;
	}

	template <class _T, cat::EnableIf_t<cat::IsConvertileToAnyOf_v<_T, _Ts...>> = 0>
	constexpr auto& operator =(_T&& v) {
		this->content = std::move(v);
	}


};


using Value = Value_<None, int64_t, double, std::string>;
ADD_PTR_TYPES(Value)


//PTRS_FOR_CLASS(Value)
//class Value
//{
//public:
//    Value();
//};

}

#endif // VALUE_H
