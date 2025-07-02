// Windows 相关数据结构和接口声明
// 仓颉语言团队 刘俊杰 2024.11.04

type Handle = CPointer<Unit>
type WindowProc = CFunc<(Handle, UInt32, UIntNative, UIntNative) -> Int64>
let NULL = Handle()
let EMPTY_STRING = CString(CPointer<UInt8>())

@C
struct WNDCLASSEX {
    public WNDCLASSEX(
        let cbSize!: UInt32 = UInt32(sizeOf<WNDCLASSEX>()),
        let style!: UInt32 = CS_HREDRAW | CS_VREDRAW,
        let lpfnWndProc!: WindowProc = DefWindowProcA,
        let cbClsExtra!: Int32 = 0,
        let cbWndExtra!: Int32 = 0,
        let hInstance!: Handle = NULL,
        let hIcon!: Handle = NULL,
        let hCursor!: Handle = NULL,
        let hbrBackground!: Handle = NULL,
        let lpszMenuName!: CString = EMPTY_STRING,
        let lpszClassName!: CString = EMPTY_STRING,
        let hIconSm!: Handle = NULL
    ) {}
}

@C
struct POINT {
    public var x: Int32 = 0
    public var y: Int32 = 0
}

@C
struct MSG {
    public MSG(
        let hWnd!: Handle = NULL,
        let message!: UInt32 = 0,
        let wParam!: UInt16 = 0,
        let lParam!: UInt32 = 0,
        let time!: UInt32 = 0,
        let pt!: POINT = POINT(),
        let lPrivate!: UInt32 = 0
    ) {}
}

foreign func CreateWindowExA(
    dwExStyle: UInt32,
    lpClassName: CString,
    lpWindowName: CString,
    dwStyle: UInt32,
    x: UInt32, y: UInt32,
    nWidth: UInt32, nHeight: UInt32,
    hWndParent: Handle,
    hMenu: Handle,
    hInstance: Handle,
    lpParam: Handle
): Handle

foreign func RegisterClassExA(winClass: CPointer<WNDCLASSEX>): UInt16
foreign func ShowWindow(hWnd: Handle, nCmdShow: Int32): Bool
foreign func UpdateWindow(hWnd: Handle): Bool
foreign func GetMessageA(lpMsg: CPointer<MSG>, hWnd: Handle,
    wMsgFilterMin: UInt32, wMsgFilterMax: UInt32): Bool
foreign func TranslateMessage(lpMsg: CPointer<MSG>): Bool
foreign func DispatchMessageA(lpMsg: CPointer<MSG>): UInt64
foreign func PostQuitMessage(nExitCode: Int32): Unit
foreign func DestroyWindow(hWnd: Handle): Bool
foreign func DefWindowProcA(hWnd: Handle, msg: UInt32,
    wParam: UIntNative, lParam: UIntNative): Int64
foreign func GetModuleHandleA(lpModuleName: CString): Handle
foreign func GetLastError(): UInt32
foreign func CreateSolidBrush(color: UInt32): Handle