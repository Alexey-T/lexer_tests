//hard to catch code-tree nodes
//1 level of nesting
;
CScriptEngine::CScriptEngine(CNppExec* pNppExec, const CListT<tstr>& CmdList, const tstr& id){}
;
CScriptEngine::~CScriptEngine(){}
bool CScriptEngine::IsParentOf(const std::shared_ptr<CScriptEngine> pScriptEngine) const{}
bool CScriptEngine::IsChildOf(const std::shared_ptr<CScriptEngine> pScriptEngine) const{}
bool CScriptEngine::ContinueExecution() const{}
std::shared_ptr<CChildProcess> CScriptEngine::GetRunningChildProcess(){}

//2 levels of nesting
;
Aaa::CScriptEngine::CScriptEngine(CNppExec* pNppExec, const CListT<tstr>& CmdList, const tstr& id){}
;
Aaa::CScriptEngine::~CScriptEngine(){}
bool Aaa::CScriptEngine::IsParentOf(const std::shared_ptr<CScriptEngine> pScriptEngine) const{}
bool Aaa::CScriptEngine::IsChildOf(const std::shared_ptr<CScriptEngine> pScriptEngine) const{}
bool Aaa::CScriptEngine::ContinueExecution() const{}
std::shared_ptr<CChildProcess> Aaa::CScriptEngine::GetRunningChildProcess(){}
