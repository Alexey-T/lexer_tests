package cangjieWeb.utils
import std.collection.{ArrayList}
import crypto.digest.{SHA256, SHA1}
import crypto.keys.{RSAPublicKey, PadOption, OAEPOption}
import std.io.ByteArrayStream
public class ByteArrayUtils {
    private static var CACHING_SHA2_DIGEST_LENGTH = 32

    public static func indexByte(source: Array<Byte>, target: Byte): Int64 {
        for (i in 0 .. source.size) {
            if (source[i] == target) {
                return i
            }
        }
        return -1
    }

    public static func littleEndianUint16(b: Array<Byte>):UInt16 {
        return UInt16(b[0]) | UInt16(b[1])<<8
    }

    public static func littleEndianUint32(b: Array<Byte>):UInt32 {
        return UInt32(b[0]) | UInt32(b[1])<<8 | UInt32(b[2])<<16 | UInt32(b[3]) << 24
    }

    public static func scrambleCachingSha2(cipher: Array<Byte>, passwd: Array<Byte>):Array<Byte> {
        // 使用本包需要外部依赖 OpenSSL 3 的 crypto 动态库文件，故使用前需安装相关工具。
        // 并配置环境变量
        var sha2Instance = SHA256()
        var dig1 = Array<Byte>(CACHING_SHA2_DIGEST_LENGTH, item: 0)
        var dig2 = Array<Byte>(CACHING_SHA2_DIGEST_LENGTH, item: 0)
        var scramble1 = Array<Byte>(CACHING_SHA2_DIGEST_LENGTH, item: 0)
        
        sha2Instance.write(passwd)
        dig1 = sha2Instance.finish()
        sha2Instance.reset()
        sha2Instance.write(dig1)
        dig2 = sha2Instance.finish()
        sha2Instance.reset()
        sha2Instance.write(dig2)
        sha2Instance.write(cipher)
        scramble1 = sha2Instance.finish()
        sha2Instance.reset()
        var mysqlScrambleBuff = Array<Byte>(CACHING_SHA2_DIGEST_LENGTH, item: 0)
        xorString(dig1, mysqlScrambleBuff, scramble1, CACHING_SHA2_DIGEST_LENGTH)
        return mysqlScrambleBuff
    }

    public static func scrambleRSA(password: String, publicKey: Array<Byte>, cipher: Array<Byte>): Array<Byte> {
        var input: Array<Byte>
        if(password.toArray().size == 0) {
            input = Array<Byte>(1, item: 0)
        } else {
            input = Array<Byte>(password.toArray().size + 1, item: 0)
            password.toArray().copyTo(input, 0, 0, password.toArray().size)
            input[password.toArray().size] = 0
        }

        var mysqlScrambleBuff = Array<Byte>(input.size, item: 0)
        xorString(input, mysqlScrambleBuff, cipher, input.size)
        var rsaPub = RSAPublicKey.decodeFromPem(String.fromUtf8(publicKey))
       
        var byteArraySteam1 = ByteArrayStream()
        byteArraySteam1.write(mysqlScrambleBuff)
        
        var byteArraySteam2 = ByteArrayStream()
        var opt = OAEPOption(SHA1(),SHA1())
        rsaPub.encrypt(byteArraySteam1,byteArraySteam2,padType:OAEP(opt))
        // rsaPub.encrypt(byteArraySteam1, byteArraySteam2, padType: PadOption.PKCS1)
        var buffer = Array<Byte>(1024, item: 0)
        var len = byteArraySteam2.read(buffer)
        var res = Array<Byte>(len, item: 0)
        buffer.copyTo(res, 0, 0, len)
        return res
    }

    public static func xorString(src: Array<Byte>, to: Array<Byte>,scramble:Array<Byte>,len:Int64) {
        var pos = 0
        var scrambleLen = scramble.size
        while (pos < len) {
            to[pos] = src[pos] ^ scramble[pos % scrambleLen]
            pos++
        }
    }  

    public static func appendLengthEncodedInteger(n: UInt64): Array<Byte> {
        let result = ArrayList<Byte>()
        if (n <= 250) {
            result.append(UInt8(n))
        } else if (n <= 0xffff) {
            result.append(0xfc)
            result.append(UInt8(n & 0xff))
            result.append(UInt8(n >> 8 & 0xff))
        } else if (n <= 0xffffff) {
            result.append(0xfd)
            result.append(UInt8(n & 0xff))
            result.append(UInt8(n >> 8 & 0xff))
            result.append(UInt8(n >> 16 & 0xff))
        } else {
            result.append(0xfe)
            result.append(UInt8(n & 0xff))
            result.append(UInt8(n >> 8 & 0xff))
            result.append(UInt8(n >> 16 & 0xff))
            result.append(UInt8(n >> 24 & 0xff))
            result.append(UInt8(n >> 32 & 0xff))
            result.append(UInt8(n >> 40 & 0xff))
            result.append(UInt8(n >> 48 & 0xff))
            result.append(UInt8(n >> 56 & 0xff))
        }
        return result.toArray()
    }

    public static func uint64ToBytes(n: UInt64): Array<Byte> {
        let result = ArrayList<Byte>()
        result.append(UInt8(n & 0xff))
        result.append(UInt8(n >> 8 & 0xff))
        result.append(UInt8(n >> 16 & 0xff))
        result.append(UInt8(n >> 24 & 0xff))
        result.append(UInt8(n >> 32 & 0xff))
        result.append(UInt8(n >> 40 & 0xff))
        result.append(UInt8(n >> 48 & 0xff))
        result.append(UInt8(n >> 56 & 0xff))
        return result.toArray()
    }

        public static func uint32ToBytes(n: UInt32): Array<Byte> {
        let result = ArrayList<Byte>()
        result.append(UInt8(n & 0xff))
        result.append(UInt8(n >> 8 & 0xff))
        result.append(UInt8(n >> 16 & 0xff))
        result.append(UInt8(n >> 24 & 0xff))
        return result.toArray()
    }

    public static func readLengthEncodedString(data: Array<Byte>) {
        let result = readLengthEncodedInteger(data)
        var num = result[0]
        var isNull = result[1]
        var n = result[2]
        if (num < 1) {
            return (data[n..n], isNull, n)
        }

        n += Int64(num)
        if (data.size >= n) {
            return (data[n - Int64(num) .. n], false, n)
        }
        return (Array<Byte>(), false, n)
    }
    public static func lengthEncoderInteger(number: UInt64): Array<Byte> {
        let result = ArrayList<Byte>()
        if (number <= 250) {
            result.append(UInt8(number))
        } else if (number <= 0xffff) {
             result.append(UInt8(0xfc))

        }

        return result.toArray()
    }

    /*
        第一个参数，the number
        第二个参数，数值是否为NULL
        第三个参数，所占字节数
    */
    public static func readLengthEncodedInteger(data: Array<Byte>): (UInt64, Bool, Int64) {
        match(data[0]) {
            case 251 => 
                return (0, true, 1)
            case 252 => 
                return (
                    UInt64(data[1]) | UInt64(data[2])<<8, false, 3
                )
            case 253 => 
                return (
                    UInt64(data[1]) | UInt64(data[2])<<8 | UInt64(data[3])<<16, false, 4
                )
            case 254 => 
                return (
                    UInt64(data[1]) | UInt64(data[2])<<8 | UInt64(data[3])<<16 | UInt64(data[4]) << 24 |
                    UInt64(data[5]) << 32 | UInt64(data[6]) << 40 | UInt64(data[7]) << 48 | UInt64(data[8]) << 56,
                    false, 9
                )
            case _ => return (UInt64(data[0]), false , 1)
        }
    }


}