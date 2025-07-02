package cangjieWeb.mysql

public let maxPacketSize = 1<<24 - 1

public let clientLongPassword: UInt32  = 1
public let clientLongFlag: UInt32  = 4
public let clientConnectWithDB: UInt32  = 8
public let clientLocalFiles: UInt32 = 128
public let clientProtocol41: UInt32 = 512
public let clientTransactions: UInt32 = 8192
public let clientSecureConn: UInt32 = 32768
public let clientMultiResults:UInt32 = 131072
public let clientPluginAuth:UInt32 = 524288
public let clientConnectAttrs:UInt32 = 1048576
public let clientPluginAuthLenEncClientData:UInt32 = 2097152


public let iOK: Byte = UInt8(0)
public let iAuthMoreData: Byte = UInt8(1)
public let iLocalInFile: Byte = UInt8(251)
public let iEOF: Byte = UInt8(254)
public let iERR: Byte = UInt8(255)

public let cachingSha2PasswordRequestPublicKey: Byte = UInt8(2)
public let cachingSha2PasswordPerformFullAuthentication: Byte = UInt8(4)



public let comQuit: Byte = 1
public let comQuery: Byte = 3
public let comPing: Byte = 14
public let comStmtPrepare: Byte = 22
public let comStmtExecute: Byte = 23
public let comStmtSendLongData: Byte = 24
public let comStmtClose: Byte = 25


public let fieldTypeLong: Byte = 3
public let fieldTypeNULL: Byte = 6
public let fieldTypeLongLong: Byte = 8
public let fieldTypeVarString: Byte = 253