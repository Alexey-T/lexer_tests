CNppExecMacroVars::StrCalc::StrCalc(tstr& varValue, CNppExec* pNppExec)
  : m_varValue(varValue), m_pNppExec(pNppExec), m_calcType(CT_FPARSER), m_pVar(0)
{}

bool CNppExecMacroVars::substituteMacroVar(const tstr& Cmd, tstr& S, int& pos,
                                           const TCHAR* varName,
                                           tstr (*getValue)(CNppExec* pNppExec) )
{}

CNppExecMacroVars::CNppExecMacroVars() : m_pNppExec(0)
{}

template<> bool OperandComparator<tstr>::eq_i() const
{}

template<class MacroVarFunc> bool IterateUserMacroVars(
    typename MacroVarFunc::container_type& userMacroVars,
    typename MacroVarFunc::container_type& userLocalMacroVars,
    MacroVarFunc func)
{}
