template <class T> CStrT<T>::CStrT() : 
  m_pData(NULL), 
  m_nLength(0), 
  m_nMemSize(0)
{
}

template <class T> CStrT<T>::CStrT_(const T* pStr, int nLength) :
  m_pData(NULL), 
  #ifdef aaa
  m_nLength(0), //comment
  m_nMemSize("test")
{
    if (pStr && (nLength != 0))  Append(pStr, nLength);
}


template <class T> aa::bb<T>::bb() : 
  m_pData(NULL), 
  m_nLength(0), 
  m_nMemSize(0)
{
}

template <class T> aa::bb<T>::bb_(const T* pStr, int nLength) :
  m_pData(NULL), 
  #ifdef aaa
  m_nLength(0), //comment
  m_nMemSize("test")
{
    if (pStr && (nLength != 0))  Append(pStr, nLength);
}


class PrintMacroVarFunc
{
    public:
        typedef const CNppExecMacroVars::tMacroVars container_type;
        typedef container_type::const_iterator iterator_type;

        PrintMacroVarFunc(CNppExec* pNppExec) : m_pNppExec(pNppExec)
        {
        }
};

class SubstituteMacroVarFunc
{
    public:
        typedef const CNppExecMacroVars::tMacroVars container_type;
        typedef container_type::const_iterator iterator_type;

        SubstituteMacroVarFunc(tstr& Value, int& pos) : m_Value(Value), m_Pos(pos)
        {
            m_ValueUpper = Value;
            NppExecHelpers::StrUpper(m_ValueUpper);
        }
};
