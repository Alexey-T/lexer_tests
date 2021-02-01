//! We do this instead of @cImport because the self-hosted compiler is easier
//! to bootstrap if it does not depend on translate-c.

const LLVMBool = bool;
pub const AttributeIndex = c_uint;

/// Make sure to use the *InContext functions instead of the global ones.
pub const Context = opaque {
    pub const create = LLVMContextCreate;
    extern fn LLVMContextCreate() *const Context;

    pub const dispose = LLVMContextDispose;
    extern fn LLVMContextDispose(C: *const Context) void;

    pub const createEnumAttribute = LLVMCreateEnumAttribute;
    extern fn LLVMCreateEnumAttribute(*const Context, KindID: c_uint, Val: u64) *const Attribute;

    pub const intType = LLVMIntTypeInContext;
    extern fn LLVMIntTypeInContext(C: *const Context, NumBits: c_uint) *const Type;

    pub const voidType = LLVMVoidTypeInContext;
    extern fn LLVMVoidTypeInContext(C: *const Context) *const Type;

    pub const constString = LLVMConstStringInContext;
    extern fn LLVMConstStringInContext(C: *const Context, Str: [*]const u8, Length: c_uint, DontNullTerminate: LLVMBool) *const Value;

    pub const createBasicBlock = LLVMCreateBasicBlockInContext;
    extern fn LLVMCreateBasicBlockInContext(C: *const Context, Name: [*:0]const u8) *const BasicBlock;

    pub const appendBasicBlock = LLVMAppendBasicBlockInContext;
    extern fn LLVMAppendBasicBlockInContext(C: *const Context, Fn: *const Value, Name: [*:0]const u8) *const BasicBlock;

    pub const createBuilder = LLVMCreateBuilderInContext;
    extern fn LLVMCreateBuilderInContext(C: *const Context) *const Builder;
};

pub const Value = opaque {
    pub const addAttributeAtIndex = LLVMAddAttributeAtIndex;
    extern fn LLVMAddAttributeAtIndex(*const Value, Idx: AttributeIndex, A: *const Attribute) void;

    pub const getFirstBasicBlock = LLVMGetFirstBasicBlock;
    extern fn LLVMGetFirstBasicBlock(Fn: *const Value) ?*const BasicBlock;

    pub const appendExistingBasicBlock = LLVMAppendExistingBasicBlock;
    extern fn LLVMAppendExistingBasicBlock(Fn: *const Value, BB: *const BasicBlock) void;

    pub const addIncoming = LLVMAddIncoming;
    extern fn LLVMAddIncoming(PhiNode: *const Value, IncomingValues: [*]*const Value, IncomingBlocks: [*]*const BasicBlock, Count: c_uint) void;

    pub const getNextInstruction = LLVMGetNextInstruction;
    extern fn LLVMGetNextInstruction(Inst: *const Value) ?*const Value;
};

pub const Type = opaque {
    pub const functionType = LLVMFunctionType;
    extern fn LLVMFunctionType(ReturnType: *const Type, ParamTypes: ?[*]*const Type, ParamCount: c_uint, IsVarArg: LLVMBool) *const Type;

    pub const constNull = LLVMConstNull;
    extern fn LLVMConstNull(Ty: *const Type) *const Value;

    pub const constAllOnes = LLVMConstAllOnes;
    extern fn LLVMConstAllOnes(Ty: *const Type) *const Value;

    pub const constInt = LLVMConstInt;
    extern fn LLVMConstInt(IntTy: *const Type, N: c_ulonglong, SignExtend: LLVMBool) *const Value;

    pub const constArray = LLVMConstArray;
    extern fn LLVMConstArray(ElementTy: *const Type, ConstantVals: ?[*]*const Value, Length: c_uint) *const Value;

    pub const getUndef = LLVMGetUndef;
    extern fn LLVMGetUndef(Ty: *const Type) *const Value;

    pub const pointerType = LLVMPointerType;
    extern fn LLVMPointerType(ElementType: *const Type, AddressSpace: c_uint) *const Type;

    pub const arrayType = LLVMArrayType;
    extern fn LLVMArrayType(ElementType: *const Type, ElementCount: c_uint) *const Type;
};

pub const Module = opaque {
    pub const createWithName = LLVMModuleCreateWithNameInContext;
    extern fn LLVMModuleCreateWithNameInContext(ModuleID: [*:0]const u8, C: *const Context) *const Module;

    pub const dispose = LLVMDisposeModule;
    extern fn LLVMDisposeModule(*const Module) void;

    pub const verify = LLVMVerifyModule;
    extern fn LLVMVerifyModule(*const Module, Action: VerifierFailureAction, OutMessage: *[*:0]const u8) LLVMBool;

    pub const addFunction = LLVMAddFunction;
    extern fn LLVMAddFunction(*const Module, Name: [*:0]const u8, FunctionTy: *const Type) *const Value;

    pub const getNamedFunction = LLVMGetNamedFunction;
    extern fn LLVMGetNamedFunction(*const Module, Name: [*:0]const u8) ?*const Value;

    pub const getIntrinsicDeclaration = LLVMGetIntrinsicDeclaration;
    extern fn LLVMGetIntrinsicDeclaration(Mod: *const Module, ID: c_uint, ParamTypes: ?[*]*const Type, ParamCount: usize) *const Value;

    pub const printToString = LLVMPrintModuleToString;
    extern fn LLVMPrintModuleToString(*const Module) [*:0]const u8;

    pub const addGlobal = LLVMAddGlobal;
    extern fn LLVMAddGlobal(M: *const Module, Ty: *const Type, Name: [*:0]const u8) *const Value;

    pub const getNamedGlobal = LLVMGetNamedGlobal;
    extern fn LLVMGetNamedGlobal(M: *const Module, Name: [*:0]const u8) ?*const Value;
};

pub const lookupIntrinsicID = LLVMLookupIntrinsicID;
extern fn LLVMLookupIntrinsicID(Name: [*]const u8, NameLen: usize) c_uint;

pub const disposeMessage = LLVMDisposeMessage;
extern fn LLVMDisposeMessage(Message: [*:0]const u8) void;

pub const VerifierFailureAction = extern enum {
    AbortProcess,
    PrintMessage,
    ReturnStatus,
};

pub const constNeg = LLVMConstNeg;
extern fn LLVMConstNeg(ConstantVal: *const Value) *const Value;

pub const setInitializer = LLVMSetInitializer;
extern fn LLVMSetInitializer(GlobalVar: *const Value, ConstantVal: *const Value) void;

pub const getParam = LLVMGetParam;
extern fn LLVMGetParam(Fn: *const Value, Index: c_uint) *const Value;

pub const getEnumAttributeKindForName = LLVMGetEnumAttributeKindForName;
extern fn LLVMGetEnumAttributeKindForName(Name: [*]const u8, SLen: usize) c_uint;

pub const Attribute = opaque {};

pub const Builder = opaque {
    pub const dispose = LLVMDisposeBuilder;
    extern fn LLVMDisposeBuilder(Builder: *const Builder) void;

    pub const positionBuilder = LLVMPositionBuilder;
    extern fn LLVMPositionBuilder(Builder: *const Builder, Block: *const BasicBlock, Instr: *const Value) void;

    pub const positionBuilderAtEnd = LLVMPositionBuilderAtEnd;
    extern fn LLVMPositionBuilderAtEnd(Builder: *const Builder, Block: *const BasicBlock) void;

    pub const getInsertBlock = LLVMGetInsertBlock;
    extern fn LLVMGetInsertBlock(Builder: *const Builder) *const BasicBlock;

    pub const buildCall = LLVMBuildCall;
    extern fn LLVMBuildCall(*const Builder, Fn: *const Value, Args: ?[*]*const Value, NumArgs: c_uint, Name: [*:0]const u8) *const Value;

    pub const buildCall2 = LLVMBuildCall2;
    extern fn LLVMBuildCall2(*const Builder, *const Type, Fn: *const Value, Args: [*]*const Value, NumArgs: c_uint, Name: [*:0]const u8) *const Value;

    pub const buildRetVoid = LLVMBuildRetVoid;
    extern fn LLVMBuildRetVoid(*const Builder) *const Value;

    pub const buildRet = LLVMBuildRet;
    extern fn LLVMBuildRet(*const Builder, V: *const Value) *const Value;

    pub const buildUnreachable = LLVMBuildUnreachable;
    extern fn LLVMBuildUnreachable(*const Builder) *const Value;

    pub const buildAlloca = LLVMBuildAlloca;
    extern fn LLVMBuildAlloca(*const Builder, Ty: *const Type, Name: [*:0]const u8) *const Value;

    pub const buildStore = LLVMBuildStore;
    extern fn LLVMBuildStore(*const Builder, Val: *const Value, Ptr: *const Value) *const Value;

    pub const buildLoad = LLVMBuildLoad;
    extern fn LLVMBuildLoad(*const Builder, PointerVal: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildNot = LLVMBuildNot;
    extern fn LLVMBuildNot(*const Builder, V: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildNSWAdd = LLVMBuildNSWAdd;
    extern fn LLVMBuildNSWAdd(*const Builder, LHS: *const Value, RHS: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildNUWAdd = LLVMBuildNUWAdd;
    extern fn LLVMBuildNUWAdd(*const Builder, LHS: *const Value, RHS: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildNSWSub = LLVMBuildNSWSub;
    extern fn LLVMBuildNSWSub(*const Builder, LHS: *const Value, RHS: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildNUWSub = LLVMBuildNUWSub;
    extern fn LLVMBuildNUWSub(*const Builder, LHS: *const Value, RHS: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildIntCast2 = LLVMBuildIntCast2;
    extern fn LLVMBuildIntCast2(*const Builder, Val: *const Value, DestTy: *const Type, IsSigned: LLVMBool, Name: [*:0]const u8) *const Value;

    pub const buildBitCast = LLVMBuildBitCast;
    extern fn LLVMBuildBitCast(*const Builder, Val: *const Value, DestTy: *const Type, Name: [*:0]const u8) *const Value;

    pub const buildInBoundsGEP = LLVMBuildInBoundsGEP;
    extern fn LLVMBuildInBoundsGEP(B: *const Builder, Pointer: *const Value, Indices: [*]*const Value, NumIndices: c_uint, Name: [*:0]const u8) *const Value;

    pub const buildICmp = LLVMBuildICmp;
    extern fn LLVMBuildICmp(*const Builder, Op: IntPredicate, LHS: *const Value, RHS: *const Value, Name: [*:0]const u8) *const Value;

    pub const buildBr = LLVMBuildBr;
    extern fn LLVMBuildBr(*const Builder, Dest: *const BasicBlock) *const Value;

    pub const buildCondBr = LLVMBuildCondBr;
    extern fn LLVMBuildCondBr(*const Builder, If: *const Value, Then: *const BasicBlock, Else: *const BasicBlock) *const Value;

    pub const buildPhi = LLVMBuildPhi;
    extern fn LLVMBuildPhi(*const Builder, Ty: *const Type, Name: [*:0]const u8) *const Value;
};

pub const IntPredicate = extern enum {
    EQ = 32,
    NE = 33,
    UGT = 34,
    UGE = 35,
    ULT = 36,
    ULE = 37,
    SGT = 38,
    SGE = 39,
    SLT = 40,
    SLE = 41,
};

pub const BasicBlock = opaque {
    pub const deleteBasicBlock = LLVMDeleteBasicBlock;
    extern fn LLVMDeleteBasicBlock(BB: *const BasicBlock) void;

    pub const getFirstInstruction = LLVMGetFirstInstruction;
    extern fn LLVMGetFirstInstruction(BB: *const BasicBlock) ?*const Value;
};

pub const TargetMachine = opaque {
    pub const create = LLVMCreateTargetMachine;
    extern fn LLVMCreateTargetMachine(
        T: *const Target,
        Triple: [*:0]const u8,
        CPU: [*:0]const u8,
        Features: [*:0]const u8,
        Level: CodeGenOptLevel,
        Reloc: RelocMode,
        CodeModel: CodeMode,
    ) *const TargetMachine;

    pub const dispose = LLVMDisposeTargetMachine;
    extern fn LLVMDisposeTargetMachine(T: *const TargetMachine) void;

    pub const emitToFile = LLVMTargetMachineEmitToFile;
    extern fn LLVMTargetMachineEmitToFile(*const TargetMachine, M: *const Module, Filename: [*:0]const u8, codegen: CodeGenFileType, ErrorMessage: *[*:0]const u8) LLVMBool;
};

pub const CodeMode = extern enum {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
};

pub const CodeGenOptLevel = extern enum {
    None,
    Less,
    Default,
    Aggressive,
};

pub const RelocMode = extern enum {
    Default,
    Static,
    PIC,
    DynamicNoPic,
    ROPI,
    RWPI,
    ROPI_RWPI,
};

pub const CodeGenFileType = extern enum {
    AssemblyFile,
    ObjectFile,
};

pub const Target = opaque {
    pub const getFromTriple = LLVMGetTargetFromTriple;
    extern fn LLVMGetTargetFromTriple(Triple: [*:0]const u8, T: **const Target, ErrorMessage: *[*:0]const u8) LLVMBool;
};

extern fn LLVMInitializeAArch64TargetInfo() void;
extern fn LLVMInitializeAMDGPUTargetInfo() void;
extern fn LLVMInitializeARMTargetInfo() void;
extern fn LLVMInitializeAVRTargetInfo() void;
extern fn LLVMInitializeBPFTargetInfo() void;
extern fn LLVMInitializeHexagonTargetInfo() void;
extern fn LLVMInitializeLanaiTargetInfo() void;
extern fn LLVMInitializeMipsTargetInfo() void;
extern fn LLVMInitializeMSP430TargetInfo() void;
extern fn LLVMInitializeNVPTXTargetInfo() void;
extern fn LLVMInitializePowerPCTargetInfo() void;
extern fn LLVMInitializeRISCVTargetInfo() void;
extern fn LLVMInitializeSparcTargetInfo() void;
extern fn LLVMInitializeSystemZTargetInfo() void;
extern fn LLVMInitializeWebAssemblyTargetInfo() void;
extern fn LLVMInitializeX86TargetInfo() void;
extern fn LLVMInitializeXCoreTargetInfo() void;
extern fn LLVMInitializeAArch64Target() void;
extern fn LLVMInitializeAMDGPUTarget() void;
extern fn LLVMInitializeARMTarget() void;
extern fn LLVMInitializeAVRTarget() void;
extern fn LLVMInitializeBPFTarget() void;
extern fn LLVMInitializeHexagonTarget() void;
extern fn LLVMInitializeLanaiTarget() void;
extern fn LLVMInitializeMipsTarget() void;
extern fn LLVMInitializeMSP430Target() void;
extern fn LLVMInitializeNVPTXTarget() void;
extern fn LLVMInitializePowerPCTarget() void;
extern fn LLVMInitializeRISCVTarget() void;
extern fn LLVMInitializeSparcTarget() void;
extern fn LLVMInitializeSystemZTarget() void;
extern fn LLVMInitializeWebAssemblyTarget() void;
extern fn LLVMInitializeX86Target() void;
extern fn LLVMInitializeXCoreTarget() void;
extern fn LLVMInitializeAArch64TargetMC() void;
extern fn LLVMInitializeAMDGPUTargetMC() void;
extern fn LLVMInitializeARMTargetMC() void;
extern fn LLVMInitializeAVRTargetMC() void;
extern fn LLVMInitializeBPFTargetMC() void;
extern fn LLVMInitializeHexagonTargetMC() void;
extern fn LLVMInitializeLanaiTargetMC() void;
extern fn LLVMInitializeMipsTargetMC() void;
extern fn LLVMInitializeMSP430TargetMC() void;
extern fn LLVMInitializeNVPTXTargetMC() void;
extern fn LLVMInitializePowerPCTargetMC() void;
extern fn LLVMInitializeRISCVTargetMC() void;
extern fn LLVMInitializeSparcTargetMC() void;
extern fn LLVMInitializeSystemZTargetMC() void;
extern fn LLVMInitializeWebAssemblyTargetMC() void;
extern fn LLVMInitializeX86TargetMC() void;
extern fn LLVMInitializeXCoreTargetMC() void;
extern fn LLVMInitializeAArch64AsmPrinter() void;
extern fn LLVMInitializeAMDGPUAsmPrinter() void;
extern fn LLVMInitializeARMAsmPrinter() void;
extern fn LLVMInitializeAVRAsmPrinter() void;
extern fn LLVMInitializeBPFAsmPrinter() void;
extern fn LLVMInitializeHexagonAsmPrinter() void;
extern fn LLVMInitializeLanaiAsmPrinter() void;
extern fn LLVMInitializeMipsAsmPrinter() void;
extern fn LLVMInitializeMSP430AsmPrinter() void;
extern fn LLVMInitializeNVPTXAsmPrinter() void;
extern fn LLVMInitializePowerPCAsmPrinter() void;
extern fn LLVMInitializeRISCVAsmPrinter() void;
extern fn LLVMInitializeSparcAsmPrinter() void;
extern fn LLVMInitializeSystemZAsmPrinter() void;
extern fn LLVMInitializeWebAssemblyAsmPrinter() void;
extern fn LLVMInitializeX86AsmPrinter() void;
extern fn LLVMInitializeXCoreAsmPrinter() void;
extern fn LLVMInitializeAArch64AsmParser() void;
extern fn LLVMInitializeAMDGPUAsmParser() void;
extern fn LLVMInitializeARMAsmParser() void;
extern fn LLVMInitializeAVRAsmParser() void;
extern fn LLVMInitializeBPFAsmParser() void;
extern fn LLVMInitializeHexagonAsmParser() void;
extern fn LLVMInitializeLanaiAsmParser() void;
extern fn LLVMInitializeMipsAsmParser() void;
extern fn LLVMInitializeMSP430AsmParser() void;
extern fn LLVMInitializePowerPCAsmParser() void;
extern fn LLVMInitializeRISCVAsmParser() void;
extern fn LLVMInitializeSparcAsmParser() void;
extern fn LLVMInitializeSystemZAsmParser() void;
extern fn LLVMInitializeWebAssemblyAsmParser() void;
extern fn LLVMInitializeX86AsmParser() void;

pub const initializeAllTargetInfos = LLVMInitializeAllTargetInfos;
fn LLVMInitializeAllTargetInfos() callconv(.C) void {
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAMDGPUTargetInfo();
    LLVMInitializeARMTargetInfo();
    LLVMInitializeAVRTargetInfo();
    LLVMInitializeBPFTargetInfo();
    LLVMInitializeHexagonTargetInfo();
    LLVMInitializeLanaiTargetInfo();
    LLVMInitializeMipsTargetInfo();
    LLVMInitializeMSP430TargetInfo();
    LLVMInitializeNVPTXTargetInfo();
    LLVMInitializePowerPCTargetInfo();
    LLVMInitializeRISCVTargetInfo();
    LLVMInitializeSparcTargetInfo();
    LLVMInitializeSystemZTargetInfo();
    LLVMInitializeWebAssemblyTargetInfo();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeXCoreTargetInfo();
}
pub const initializeAllTargets = LLVMInitializeAllTargets;
fn LLVMInitializeAllTargets() callconv(.C) void {
    LLVMInitializeAArch64Target();
    LLVMInitializeAMDGPUTarget();
    LLVMInitializeARMTarget();
    LLVMInitializeAVRTarget();
    LLVMInitializeBPFTarget();
    LLVMInitializeHexagonTarget();
    LLVMInitializeLanaiTarget();
    LLVMInitializeMipsTarget();
    LLVMInitializeMSP430Target();
    LLVMInitializeNVPTXTarget();
    LLVMInitializePowerPCTarget();
    LLVMInitializeRISCVTarget();
    LLVMInitializeSparcTarget();
    LLVMInitializeSystemZTarget();
    LLVMInitializeWebAssemblyTarget();
    LLVMInitializeX86Target();
    LLVMInitializeXCoreTarget();
}
pub const initializeAllTargetMCs = LLVMInitializeAllTargetMCs;
fn LLVMInitializeAllTargetMCs() callconv(.C) void {
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAMDGPUTargetMC();
    LLVMInitializeARMTargetMC();
    LLVMInitializeAVRTargetMC();
    LLVMInitializeBPFTargetMC();
    LLVMInitializeHexagonTargetMC();
    LLVMInitializeLanaiTargetMC();
    LLVMInitializeMipsTargetMC();
    LLVMInitializeMSP430TargetMC();
    LLVMInitializeNVPTXTargetMC();
    LLVMInitializePowerPCTargetMC();
    LLVMInitializeRISCVTargetMC();
    LLVMInitializeSparcTargetMC();
    LLVMInitializeSystemZTargetMC();
    LLVMInitializeWebAssemblyTargetMC();
    LLVMInitializeX86TargetMC();
    LLVMInitializeXCoreTargetMC();
}
pub const initializeAllAsmPrinters = LLVMInitializeAllAsmPrinters;
fn LLVMInitializeAllAsmPrinters() callconv(.C) void {
    LLVMInitializeAArch64AsmPrinter();
    LLVMInitializeAMDGPUAsmPrinter();
    LLVMInitializeARMAsmPrinter();
    LLVMInitializeAVRAsmPrinter();
    LLVMInitializeBPFAsmPrinter();
    LLVMInitializeHexagonAsmPrinter();
    LLVMInitializeLanaiAsmPrinter();
    LLVMInitializeMipsAsmPrinter();
    LLVMInitializeMSP430AsmPrinter();
    LLVMInitializeNVPTXAsmPrinter();
    LLVMInitializePowerPCAsmPrinter();
    LLVMInitializeRISCVAsmPrinter();
    LLVMInitializeSparcAsmPrinter();
    LLVMInitializeSystemZAsmPrinter();
    LLVMInitializeWebAssemblyAsmPrinter();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeXCoreAsmPrinter();
}
pub const initializeAllAsmParsers = LLVMInitializeAllAsmParsers;
fn LLVMInitializeAllAsmParsers() callconv(.C) void {
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAMDGPUAsmParser();
    LLVMInitializeARMAsmParser();
    LLVMInitializeAVRAsmParser();
    LLVMInitializeBPFAsmParser();
    LLVMInitializeHexagonAsmParser();
    LLVMInitializeLanaiAsmParser();
    LLVMInitializeMipsAsmParser();
    LLVMInitializeMSP430AsmParser();
    LLVMInitializePowerPCAsmParser();
    LLVMInitializeRISCVAsmParser();
    LLVMInitializeSparcAsmParser();
    LLVMInitializeSystemZAsmParser();
    LLVMInitializeWebAssemblyAsmParser();
    LLVMInitializeX86AsmParser();
}

extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool) c_int;
extern fn ZigLLDLinkELF(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool) c_int;
extern fn ZigLLDLinkMachO(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool) c_int;
extern fn ZigLLDLinkWasm(argc: c_int, argv: [*:null]const ?[*:0]const u8, can_exit_early: bool) c_int;

pub const LinkCOFF = ZigLLDLinkCOFF;
pub const LinkELF = ZigLLDLinkELF;
pub const LinkMachO = ZigLLDLinkMachO;
pub const LinkWasm = ZigLLDLinkWasm;

pub const ObjectFormatType = extern enum(c_int) {
    Unknown,
    COFF,
    ELF,
    MachO,
    Wasm,
    XCOFF,
};

pub const GetHostCPUName = LLVMGetHostCPUName;
extern fn LLVMGetHostCPUName() ?[*:0]u8;

pub const GetNativeFeatures = ZigLLVMGetNativeFeatures;
extern fn ZigLLVMGetNativeFeatures() ?[*:0]u8;

pub const WriteArchive = ZigLLVMWriteArchive;
extern fn ZigLLVMWriteArchive(
    archive_name: [*:0]const u8,
    file_names_ptr: [*]const [*:0]const u8,
    file_names_len: usize,
    os_type: OSType,
) bool;

pub const OSType = extern enum(c_int) {
    UnknownOS = 0,
    Ananas = 1,
    CloudABI = 2,
    Darwin = 3,
    DragonFly = 4,
    FreeBSD = 5,
    Fuchsia = 6,
    IOS = 7,
    KFreeBSD = 8,
    Linux = 9,
    Lv2 = 10,
    MacOSX = 11,
    NetBSD = 12,
    OpenBSD = 13,
    Solaris = 14,
    Win32 = 15,
    Haiku = 16,
    Minix = 17,
    RTEMS = 18,
    NaCl = 19,
    CNK = 20,
    AIX = 21,
    CUDA = 22,
    NVCL = 23,
    AMDHSA = 24,
    PS4 = 25,
    ELFIAMCU = 26,
    TvOS = 27,
    WatchOS = 28,
    Mesa3D = 29,
    Contiki = 30,
    AMDPAL = 31,
    HermitCore = 32,
    Hurd = 33,
    WASI = 34,
    Emscripten = 35,
};

pub const ArchType = extern enum(c_int) {
    UnknownArch = 0,
    arm = 1,
    armeb = 2,
    aarch64 = 3,
    aarch64_be = 4,
    aarch64_32 = 5,
    arc = 6,
    avr = 7,
    bpfel = 8,
    bpfeb = 9,
    hexagon = 10,
    mips = 11,
    mipsel = 12,
    mips64 = 13,
    mips64el = 14,
    msp430 = 15,
    ppc = 16,
    ppc64 = 17,
    ppc64le = 18,
    r600 = 19,
    amdgcn = 20,
    riscv32 = 21,
    riscv64 = 22,
    sparc = 23,
    sparcv9 = 24,
    sparcel = 25,
    systemz = 26,
    tce = 27,
    tcele = 28,
    thumb = 29,
    thumbeb = 30,
    x86 = 31,
    x86_64 = 32,
    xcore = 33,
    nvptx = 34,
    nvptx64 = 35,
    le32 = 36,
    le64 = 37,
    amdil = 38,
    amdil64 = 39,
    hsail = 40,
    hsail64 = 41,
    spir = 42,
    spir64 = 43,
    kalimba = 44,
    shave = 45,
    lanai = 46,
    wasm32 = 47,
    wasm64 = 48,
    renderscript32 = 49,
    renderscript64 = 50,
    ve = 51,
};

pub const ParseCommandLineOptions = ZigLLVMParseCommandLineOptions;
extern fn ZigLLVMParseCommandLineOptions(argc: usize, argv: [*]const [*:0]const u8) void;

pub const WriteImportLibrary = ZigLLVMWriteImportLibrary;
extern fn ZigLLVMWriteImportLibrary(
    def_path: [*:0]const u8,
    arch: ArchType,
    output_lib_path: [*c]const u8,
    kill_at: bool,
) bool;
