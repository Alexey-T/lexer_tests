class Writer
  """
  A buffer for building messages.

  `Writer` provides an way to create byte sequences using common
  data encodings. The `Writer` manages the underlying arrays and
  sizes. It is useful for encoding data to send over a network or
  store in a file. Once a message has been built you can call `done()`
  to get the message's `ByteSeq`s, and you can then reuse the
  `Writer` for creating a new message.

  For example, suppose we have a TCP-based network data protocol where
  messages consist of the following:

  * `message_length` - the number of bytes in the message as a
    big-endian 32-bit integer
  * `list_size` - the number of items in the following list of items
    as a big-endian 32-bit integer
  * zero or more items of the following data:
    * a big-endian 64-bit floating point number
    * a string that starts with a big-endian 32-bit integer that
      specifies the length of the string, followed by a number of
      bytes that represent the string

  A message would be something like this:

  ```
  [message_length][list_size][float1][string1][float2][string2]...
  ```

  The following program uses a write buffer to encode an array of
  tuples as a message of this type:

  ```pony
  use "buffered"

  actor Main
    new create(env: Env) =>
      let wb = Writer
      let messages = [[(F32(3597.82), "Anderson"); (F32(-7979.3), "Graham")]
                      [(F32(3.14159), "Hopper"); (F32(-83.83), "Jones")]]
      for items in messages.values() do
        wb.i32_be((items.size() / 2).i32())
        for (f, s) in items.values() do
          wb.f32_be(f)
          wb.i32_be(s.size().i32())
          wb.write(s.array())
        end
        let wb_msg = Writer
        wb_msg.i32_be(wb.size().i32())
        wb_msg.writev(wb.done())
        env.out.writev(wb_msg.done())
      end
  ```
  """
  var _chunks: Array[ByteSeq] iso = recover Array[ByteSeq] end
  var _current: Array[U8] iso = recover Array[U8] end
  var _size: USize = 0

  fun ref reserve_chunks(size': USize) =>
    """
    Reserve space for size' chunks.

    This needs to be recalled after every call to `done`
    as `done` resets the chunks.
    """
    _chunks.reserve(size')

  fun ref reserve_current(size': USize) =>
    """
    Reserve space for size bytes in `_current`.
    """
    _current.reserve(_current.size() + size')

  fun size(): USize =>
    _size

  fun ref u8(data: U8) =>
    """
    Write a byte to the buffer.
    """
    let num_bytes = U8(0).bytewidth()
    _current.push_u8(data)
    _size = _size + num_bytes

  fun ref u16_le(data: U16) =>
    """
    Write a U16 to the buffer in little-endian byte order.
    """
    let num_bytes = U16(0).bytewidth()
    ifdef littleendian then
      _current.push_u16(data)
    else
      _current.push_u16(data.bswap())
    end
    _size = _size + num_bytes

  fun ref u16_be(data: U16) =>
    """
    Write a U16 to the buffer in big-endian byte order.
    """
    let num_bytes = U16(0).bytewidth()
    ifdef bigendian then
      _current.push_u16(data)
    else
      _current.push_u16(data.bswap())
    end
    _size = _size + num_bytes

  fun ref i16_le(data: I16) =>
    """
    Write an I16 to the buffer in little-endian byte order.
    """
    u16_le(data.u16())

  fun ref i16_be(data: I16) =>
    """
    Write an I16 to the buffer in big-endian byte order.
    """
    u16_be(data.u16())

  fun ref u32_le(data: U32) =>
    """
    Write a U32 to the buffer in little-endian byte order.
    """
    let num_bytes = U32(0).bytewidth()
    ifdef littleendian then
      _current.push_u32(data)
    else
      _current.push_u32(data.bswap())
    end
    _size = _size + num_bytes

  fun ref u32_be(data: U32) =>
    """
    Write a U32 to the buffer in big-endian byte order.
    """
    let num_bytes = U32(0).bytewidth()
    ifdef bigendian then
      _current.push_u32(data)
    else
      _current.push_u32(data.bswap())
    end
    _size = _size + num_bytes

  fun ref i32_le(data: I32) =>
    """
    Write an I32 to the buffer in little-endian byte order.
    """
    u32_le(data.u32())

  fun ref i32_be(data: I32) =>
    """
    Write an I32 to the buffer in big-endian byte order.
    """
    u32_be(data.u32())

  fun ref f32_le(data: F32) =>
    """
    Write an F32 to the buffer in little-endian byte order.
    """
    u32_le(data.bits())

  fun ref f32_be(data: F32) =>
    """
    Write an F32 to the buffer in big-endian byte order.
    """
    u32_be(data.bits())

  fun ref u64_le(data: U64) =>
    """
    Write a U64 to the buffer in little-endian byte order.
    """
    let num_bytes = U64(0).bytewidth()
    ifdef littleendian then
      _current.push_u64(data)
    else
      _current.push_u64(data.bswap())
    end
    _size = _size + num_bytes

  fun ref u64_be(data: U64) =>
    """
    Write a U64 to the buffer in big-endian byte order.
    """
    let num_bytes = U64(0).bytewidth()
    ifdef bigendian then
      _current.push_u64(data)
    else
      _current.push_u64(data.bswap())
    end
    _size = _size + num_bytes

  fun ref i64_le(data: I64) =>
    """
    Write an I64 to the buffer in little-endian byte order.
    """
    u64_le(data.u64())

  fun ref i64_be(data: I64) =>
    """
    Write an I64 to the buffer in big-endian byte order.
    """
    u64_be(data.u64())

  fun ref f64_le(data: F64) =>
    """
    Write an F64 to the buffer in little-endian byte order.
    """
    u64_le(data.bits())

  fun ref f64_be(data: F64) =>
    """
    Write an F64 to the buffer in big-endian byte order.
    """
    u64_be(data.bits())

  fun ref u128_le(data: U128) =>
    """
    Write a U128 to the buffer in little-endian byte order.
    """
    let num_bytes = U128(0).bytewidth()
    ifdef littleendian then
      _current.push_u128(data)
    else
      _current.push_u128(data.bswap())
    end
    _size = _size + num_bytes

  fun ref u128_be(data: U128) =>
    """
    Write a U128 to the buffer in big-endian byte order.
    """
    let num_bytes = U128(0).bytewidth()
    ifdef bigendian then
      _current.push_u128(data)
    else
      _current.push_u128(data.bswap())
    end
    _size = _size + num_bytes

  fun ref i128_le(data: I128) =>
    """
    Write an I128 to the buffer in little-endian byte order.
    """
    u128_le(data.u128())

  fun ref i128_be(data: I128) =>
    """
    Write an I128 to the buffer in big-endian byte order.
    """
    u128_be(data.u128())

  fun ref write(data: ByteSeq) =>
    """
    Write a ByteSeq to the buffer.
    """
    // if `data` is 1 cacheline or less in size
    // copy it into the existing `_current` array
    // to coalesce multiple tiny arrays
    // into a single bigger array
    if data.size() <= 64 then
      match data
      | let d: String =>
         let a = d.array()
         _current.copy_from(a, 0, _current.size(), a.size())
      | let d: Array[U8] val =>
         _current.copy_from(d, 0, _current.size(), d.size())
      end
      _size = _size + data.size()
    else
      _append_current()
      _chunks.push(data)
      _size = _size + data.size()
    end

  fun ref writev(data: ByteSeqIter) =>
    """
    Write ByteSeqs to the buffer.
    """
    for chunk in data.values() do
      write(chunk)
    end

  fun ref done(): Array[ByteSeq] iso^ =>
    """
    Return an array of buffered ByteSeqs and reset the Writer's buffer.
    """
    _append_current()
    _size = 0
    _chunks = recover Array[ByteSeq] end

  fun ref _append_current() =>
    if _current.size() > 0 then
      _chunks.push(_current = recover Array[U8] end)
    end
