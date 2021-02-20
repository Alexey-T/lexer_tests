// SPDX-License-Identifier: MIT
// Copyright (c) 2015-2021 Zig Contributors
// This file is part of [zig](https://ziglang.org/), which is MIT licensed.
// The MIT license requires this copyright notice to be included in all copies
// and substantial portions of the software.
pub const EPERM = 1;
pub const ENOENT = 2;
pub const ESRCH = 3;
pub const EINTR = 4;
pub const EIO = 5;
pub const ENXIO = 6;
pub const E2BIG = 7;
pub const ENOEXEC = 8;
pub const EBADF = 9;
pub const ECHILD = 10;
pub const EAGAIN = 11;
pub const ENOMEM = 12;
pub const EACCES = 13;
pub const EFAULT = 14;
pub const ENOTBLK = 15;
pub const EBUSY = 16;
pub const EEXIST = 17;
pub const EXDEV = 18;
pub const ENODEV = 19;
pub const ENOTDIR = 20;
pub const EISDIR = 21;
pub const EINVAL = 22;
pub const ENFILE = 23;
pub const EMFILE = 24;
pub const ENOTTY = 25;
pub const ETXTBSY = 26;
pub const EFBIG = 27;
pub const ENOSPC = 28;
pub const ESPIPE = 29;
pub const EROFS = 30;
pub const EMLINK = 31;
pub const EPIPE = 32;
pub const EDOM = 33;
pub const ERANGE = 34;
pub const ENOMSG = 35;
pub const EIDRM = 36;
pub const ECHRNG = 37;
pub const EL2NSYNC = 38;
pub const EL3HLT = 39;
pub const EL3RST = 40;
pub const ELNRNG = 41;
pub const EUNATCH = 42;
pub const ENOCSI = 43;
pub const EL2HLT = 44;
pub const EDEADLK = 45;
pub const ENOLCK = 46;
pub const EBADE = 50;
pub const EBADR = 51;
pub const EXFULL = 52;
pub const ENOANO = 53;
pub const EBADRQC = 54;
pub const EBADSLT = 55;
pub const EDEADLOCK = 56;
pub const EBFONT = 59;
pub const ENOSTR = 60;
pub const ENODATA = 61;
pub const ETIME = 62;
pub const ENOSR = 63;
pub const ENONET = 64;
pub const ENOPKG = 65;
pub const EREMOTE = 66;
pub const ENOLINK = 67;
pub const EADV = 68;
pub const ESRMNT = 69;
pub const ECOMM = 70;
pub const EPROTO = 71;
pub const EDOTDOT = 73;
pub const EMULTIHOP = 74;
pub const EBADMSG = 77;
pub const ENAMETOOLONG = 78;
pub const EOVERFLOW = 79;
pub const ENOTUNIQ = 80;
pub const EBADFD = 81;
pub const EREMCHG = 82;
pub const ELIBACC = 83;
pub const ELIBBAD = 84;
pub const ELIBSCN = 85;
pub const ELIBMAX = 86;
pub const ELIBEXEC = 87;
pub const EILSEQ = 88;
pub const ENOSYS = 89;
pub const ELOOP = 90;
pub const ERESTART = 91;
pub const ESTRPIPE = 92;
pub const ENOTEMPTY = 93;
pub const EUSERS = 94;
pub const ENOTSOCK = 95;
pub const EDESTADDRREQ = 96;
pub const EMSGSIZE = 97;
pub const EPROTOTYPE = 98;
pub const ENOPROTOOPT = 99;
pub const EPROTONOSUPPORT = 120;
pub const ESOCKTNOSUPPORT = 121;
pub const EOPNOTSUPP = 122;
pub const ENOTSUP = EOPNOTSUPP;
pub const EPFNOSUPPORT = 123;
pub const EAFNOSUPPORT = 124;
pub const EADDRINUSE = 125;
pub const EADDRNOTAVAIL = 126;
pub const ENETDOWN = 127;
pub const ENETUNREACH = 128;
pub const ENETRESET = 129;
pub const ECONNABORTED = 130;
pub const ECONNRESET = 131;
pub const ENOBUFS = 132;
pub const EISCONN = 133;
pub const ENOTCONN = 134;
pub const EUCLEAN = 135;
pub const ENOTNAM = 137;
pub const ENAVAIL = 138;
pub const EISNAM = 139;
pub const EREMOTEIO = 140;
pub const ESHUTDOWN = 143;
pub const ETOOMANYREFS = 144;
pub const ETIMEDOUT = 145;
pub const ECONNREFUSED = 146;
pub const EHOSTDOWN = 147;
pub const EHOSTUNREACH = 148;
pub const EWOULDBLOCK = EAGAIN;
pub const EALREADY = 149;
pub const EINPROGRESS = 150;
pub const ESTALE = 151;
pub const ECANCELED = 158;
pub const ENOMEDIUM = 159;
pub const EMEDIUMTYPE = 160;
pub const ENOKEY = 161;
pub const EKEYEXPIRED = 162;
pub const EKEYREVOKED = 163;
pub const EKEYREJECTED = 164;
pub const EOWNERDEAD = 165;
pub const ENOTRECOVERABLE = 166;
pub const ERFKILL = 167;
pub const EHWPOISON = 168;
pub const EDQUOT = 1133;