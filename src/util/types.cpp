#include "types.h"

#include "toStringUtils.h"

namespace  {

template <class T>
cat::WriterObjectABC& _writeAddressLike(cat::WriterObjectABC& s, const T& v, const char* name) {
	s << name;
	s << "(";
	auto hexStr = cat::intToHexSigned2(ngpl::Address(v));
	//s << cat::String("0x") << hexStr;
	s << hexStr[0] + cat::String("0x") << hexStr.c_str()+1;
	s << ")";
	return s;
}

}

namespace ngpl {

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const StackAddr& v) {
	return _writeAddressLike(s, v, "StackAddr");
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const FrameAddr& v) {
	return _writeAddressLike(s, v, "FrameAddr");
}


cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const HeapAddr& v) {
	return _writeAddressLike(s, v, "HeapAddr");
}

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const None_&) {
	return s += "   ";
}




}
