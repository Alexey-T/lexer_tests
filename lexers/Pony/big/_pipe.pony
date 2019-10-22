use @pony_asio_event_create[AsioEventID](owner: AsioEventNotify, fd: U32,
      flags: U32, nsec: U64, noisy: Bool)
use @pony_asio_event_unsubscribe[None](event: AsioEventID)
use @pony_asio_event_destroy[None](event: AsioEventID)

primitive _FSETFL
  fun apply(): I32 => 4

primitive _FGETFL
  fun apply(): I32 => 3

primitive _FSETFD
  fun apply(): I32 => 2

primitive _FGETFD
  fun apply(): I32 => 1

primitive _FDCLOEXEC
  fun apply(): I32 => 1

// Non-blocking IO file status flag
primitive _ONONBLOCK
  fun apply(): I32 =>
    ifdef bsd or osx then 4
    elseif linux then 2048
    else compile_error "no O_NONBLOCK" end

// The pipe has been ended.
primitive _ERRORBROKENPIPE
  fun apply(): I32 =>
    ifdef windows then 109
    else compile_error "no ERROR_BROKEN_PIPE" end

// The pipe is being closed.
primitive _ERRORNODATA
  fun apply(): I32 =>
    ifdef windows then 232
    else compile_error "no ERROR_NO_DATA" end

class _Pipe
  """
  A pipe is a unidirectional data channel that can be used for interprocess
  communication. Outgoing pipes are written to by this process, incoming pipes
  are read from by this process.
  """
  let _outgoing: Bool
  var near_fd: U32 = -1
  var far_fd: U32 = -1
  var event: AsioEventID = AsioEvent.none()

  new none() =>
    """
    Creates a nil pipe for use as a placeholder.
    """
    _outgoing = true

  new outgoing() ? =>
    """
    Creates an outgoing pipe.
    """
    _outgoing = true
    _create()?

  new incoming() ? =>
    """
    Creates an incoming pipe.
    """
    _outgoing = false
    _create()?

  fun ref _create() ? =>
    """
    Do the actual system object creation for the pipe.
    """
    ifdef posix then
      var fds = (U32(0), U32(0))
      if @pipe[I32](addressof fds) < 0 then
        error
      end
      if _outgoing then
        near_fd = fds._2
        far_fd = fds._1
      else
        near_fd = fds._1
        far_fd = fds._2
      end
      // We need to set the flag on the two file descriptors here to prevent
      // capturing by another thread.
      _set_fd(near_fd, _FDCLOEXEC())?
      _set_fd(far_fd, _FDCLOEXEC())?
      _set_fl(near_fd, _ONONBLOCK())? // and non-blocking on parent side of pipe

    elseif windows then
      near_fd = 0
      far_fd = 0
      // create the pipe and set one handle to not inherit. That needs
      // to be done with the knowledge of which way this pipe goes.
      if @ponyint_win_pipe_create[U32](addressof near_fd, addressof far_fd,
          _outgoing) == 0 then
        error
      end
    else
      compile_error "unsupported platform"
    end

  fun _set_fd(fd: U32, flags: I32) ? =>
    let result = @fcntl[I32](fd, _FSETFD(), flags)
    if result < 0 then error end

  fun _set_fl(fd: U32, flags: I32) ? =>
    let result = @fcntl[I32](fd, _FSETFL(), flags)
    if result < 0 then error end

  fun ref begin(owner: AsioEventNotify) =>
    """
    Prepare the pipe for read or write, and listening, after the far end has
    been handed to the other process.
    """
    ifdef posix then
      let flags = if _outgoing then AsioEvent.write() else AsioEvent.read() end
      event = @pony_asio_event_create(owner, near_fd, flags, 0, true)
    end
    close_far()

  fun ref close_far() =>
    """
    Close the far end of the pipe--the end that the other process will be
    using. This is used to cleanup this process' handles that it wont use.
    """
    if far_fd != -1 then
      @close[I32](far_fd)
      far_fd = -1
    end

  fun ref read(read_buf: Array[U8] iso, offset: USize):
    (Array[U8] iso^, ISize, I32)
  =>
    ifdef posix then
      let len =
        @read[ISize](near_fd, read_buf.cpointer().usize() + offset,
          read_buf.size() - offset)
      if len == -1 then // OS signals write error
        (consume read_buf, len, @pony_os_errno())
      else
        (consume read_buf, len, 0)
      end
    else // windows
      let hnd: USize = @_get_osfhandle[USize](near_fd)
      var bytes_to_read: U32 = (read_buf.size() - offset).u32()
      // Peek ahead to see if there is anything to read, return if not
      var bytes_avail: U32 = 0
      let okp = @PeekNamedPipe[Bool](hnd, USize(0), bytes_to_read, USize(0),
          addressof bytes_avail, USize(0))
      let winerrp = @GetLastError[I32]()
      if not okp then
        if (winerrp == _ERRORBROKENPIPE()) or (winerrp == _ERRORNODATA()) then
          return (consume read_buf, 0, 0) // Pipe is done & ready to close.
        else
          // Some other error, map to invalid arg
          return (consume read_buf, -1, _EINVAL())
        end
      elseif bytes_avail == 0 then
        // Peeked ok, but nothing to read. Return and try again later.
        return (consume read_buf, -1, _EAGAIN())
      end
      if bytes_to_read > bytes_avail then
        bytes_to_read = bytes_avail
      end
      // Read up to the bytes available
      var bytes_read: U32 = 0
      let ok = @ReadFile[Bool](hnd, read_buf.cpointer().usize() + offset,
          bytes_to_read, addressof bytes_read, USize(0))
      let winerr = @GetLastError[I32]()
      if not ok then
        if (winerr == _ERRORBROKENPIPE()) or (winerr == _ERRORNODATA()) then
          (consume read_buf, 0, 0) // Pipe is done & ready to close.
        else
          (consume read_buf, -1, _EINVAL()) // Some other error, map to invalid arg
        end
      else
        // We know bytes_to_read is > 0, and can assume bytes_read is as well
        (consume read_buf, bytes_read.isize(), 0) // buffer back, bytes read, no error
      end
    end

  fun ref write(data: ByteSeq box, offset: USize): (ISize, I32) =>
    ifdef posix then
      let len = @write[ISize](
          near_fd, data.cpointer().usize() + offset, data.size() - offset)
      if len == -1 then // OS signals write error
        (len, @pony_os_errno())
      else
        (len, 0)
      end
    else // windows
      let hnd: USize = @_get_osfhandle[USize](near_fd)
      let bytes_to_write: U32 = (data.size() - offset).u32()
      var bytes_written: U32 = 0
      let ok = @WriteFile[Bool](hnd, data.cpointer().usize() + offset,
          bytes_to_write, addressof bytes_written, USize(0))
      let winerr = @GetLastError[I32]()
      if not ok then
        if (winerr == _ERRORBROKENPIPE()) or (winerr == _ERRORNODATA()) then
          (0, 0) // Pipe is done & ready to close.
        else
          (-1, _EINVAL()) // Some other error, map to invalid arg
        end
      elseif bytes_written == 0 then
        (-1, _EAGAIN())
      else
        (bytes_written.isize(), 0)
      end
    end

  fun ref is_closed(): Bool =>
    near_fd == -1

  fun ref close_near() =>
    """
    Close the near end of the pipe--the end that this process is using
    directly. Also handle unsubscribing the asio event (if there was one). File
    descriptors should always be closed _after_ unsubscribing its event,
    otherwise there is the possibility of reusing the file descriptor in
    another thread and then unsubscribing the reused file descriptor here!
    Unsubscribing and closing the file descriptor should be treated as one
    operation.
    """
    if near_fd != -1 then
      if event isnt AsioEvent.none() then
        @pony_asio_event_unsubscribe(event)
      end
      @close[I32](near_fd)
      near_fd = -1
    end

  fun ref close() =>
    close_far()
    close_near()

  fun ref dispose() =>
    @pony_asio_event_destroy(event)
    event = AsioEvent.none()
