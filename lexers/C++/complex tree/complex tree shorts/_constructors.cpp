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
}


template <class T> Aa::Bb<T>::Bb() : 
  m_pData(NULL), 
  m_nLength(0), 
  m_nMemSize(0)
{
}

template <class T> Aa::Bb<T>::Bb_(const T* pStr, int nLength) :
  m_pData(NULL), 
  #ifdef aaa
  m_nLength(0), //comment
  m_nMemSize("test")
{
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
        }
};

//---------------------------
natpmp::natpmp(io_context& ios
	, aux::portmap_callback& cb
	, listen_socket_handle ls)
	: m_callback(cb)
	, m_socket(ios)
	, m_send_timer(ios)
	, m_refresh_timer(ios)
	, m_ioc(ios)
	, m_listen_handle(std::move(ls))
{
}

void natpmp::start(ip_interface const& ip)
{
}
//--------------------------
