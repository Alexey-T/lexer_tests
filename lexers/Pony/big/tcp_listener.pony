type TCPListenerAuth is (AmbientAuth | NetAuth | TCPAuth | TCPListenAuth)

actor TCPListener
  """
  Listens for new network connections.

  The following program creates an echo server that listens for
  connections on port 8989 and echoes back any data it receives.

  ```pony
  use "net"

  class MyTCPConnectionNotify is TCPConnectionNotify
    fun ref received(
      conn: TCPConnection ref,
      data: Array[U8] iso,
      times: USize)
      : Bool
    =>
      conn.write(String.from_array(consume data))
      true

    fun ref connect_failed(conn: TCPConnection ref) =>
      None

  class MyTCPListenNotify is TCPListenNotify
    fun ref connected(listen: TCPListener ref): TCPConnectionNotify iso^ =>
      MyTCPConnectionNotify

    fun ref not_listening(listen: TCPListener ref) =>
      None

  actor Main
    new create(env: Env) =>
      try
        TCPListener(env.root as AmbientAuth,
          recover MyTCPListenNotify end, "", "8989")
      end
  ```
  """
  var _notify: TCPListenNotify
  var _fd: U32
  var _event: AsioEventID = AsioEvent.none()
  var _closed: Bool = false
  let _limit: USize
  var _count: USize = 0
  var _paused: Bool = false
  let _read_buffer_size: USize
  let _yield_after_reading: USize
  let _yield_after_writing: USize

  new create(
    auth: TCPListenerAuth,
    notify: TCPListenNotify iso,
    host: String = "",
    service: String = "0",
    limit: USize = 0,
    read_buffer_size: USize = 16384,
    yield_after_reading: USize = 16384,
    yield_after_writing: USize = 16384)
  =>
    """
    Listens for both IPv4 and IPv6 connections.
    """
    _limit = limit
    _notify = consume notify
    _event =
      @pony_os_listen_tcp[AsioEventID](this,
        host.cstring(), service.cstring())
    _read_buffer_size = read_buffer_size
    _yield_after_reading = yield_after_reading
    _yield_after_writing = yield_after_writing
    _fd = @pony_asio_event_fd(_event)
    _notify_listening()

  new ip4(
    auth: TCPListenerAuth,
    notify: TCPListenNotify iso,
    host: String = "",
    service: String = "0",
    limit: USize = 0,
    read_buffer_size: USize = 16384,
    yield_after_reading: USize = 16384,
    yield_after_writing: USize = 16384)
  =>
    """
    Listens for IPv4 connections.
    """
    _limit = limit
    _notify = consume notify
    _event =
      @pony_os_listen_tcp4[AsioEventID](this, host.cstring(),
        service.cstring())
    _read_buffer_size = read_buffer_size
    _yield_after_reading = yield_after_reading
    _yield_after_writing = yield_after_writing
    _fd = @pony_asio_event_fd(_event)
    _notify_listening()

  new ip6(
    auth: TCPListenerAuth,
    notify: TCPListenNotify iso,
    host: String = "",
    service: String = "0",
    limit: USize = 0,
    read_buffer_size: USize = 16384,
    yield_after_reading: USize = 16384,
    yield_after_writing: USize = 16384)
  =>
    """
    Listens for IPv6 connections.
    """
    _limit = limit
    _notify = consume notify
    _event =
      @pony_os_listen_tcp6[AsioEventID](this, host.cstring(),
        service.cstring())
    _read_buffer_size = read_buffer_size
    _yield_after_reading = yield_after_reading
    _yield_after_writing = yield_after_writing
    _fd = @pony_asio_event_fd(_event)
    _notify_listening()

  be set_notify(notify: TCPListenNotify iso) =>
    """
    Change the notifier.
    """
    _notify = consume notify

  be dispose() =>
    """
    Stop listening.
    """
    close()

  fun local_address(): NetAddress =>
    """
    Return the bound IP address.
    """
    let ip = recover NetAddress end
    @pony_os_sockname[Bool](_fd, ip)
    ip

  be _event_notify(event: AsioEventID, flags: U32, arg: U32) =>
    """
    When we are readable, we accept new connections until none remain.
    """
    if event isnt _event then
      return
    end

    if AsioEvent.readable(flags) then
      _accept(arg)
    end

    if AsioEvent.disposable(flags) then
      @pony_asio_event_destroy(_event)
      _event = AsioEvent.none()
    end

  be _conn_closed() =>
    """
    An accepted connection has closed. If we have dropped below the limit, try
    to accept new connections.
    """
    _count = _count - 1

    if _paused and (_count < _limit) then
      _paused = false
      _accept()
    end

  fun ref _accept(ns: U32 = 0) =>
    """
    Accept connections as long as we have spawned fewer than our limit.
    """
    ifdef windows then
      if ns == -1 then
        // Unsubscribe when we get an invalid socket in the event.
        @pony_asio_event_unsubscribe(_event)
        return
      end

      if ns > 0 then
        if _closed then
          @pony_os_socket_close[None](ns)
          return
        end

        _spawn(ns)
      end

      // Queue an accept if we're not at the limit.
      if (_limit == 0) or (_count < _limit) then
        @pony_os_accept[U32](_event)
      else
        _paused = true
      end
    else
      if _closed then
        return
      end

      while (_limit == 0) or (_count < _limit) do
        var fd = @pony_os_accept[U32](_event)

        match fd
        | -1 =>
          // Something other than EWOULDBLOCK, try again.
          None
        | 0 =>
          // EWOULDBLOCK, don't try again.
          return
        else
          _spawn(fd)
        end
      end

      _paused = true
    end

  fun ref _spawn(ns: U32) =>
    """
    Spawn a new connection.
    """
    try
      TCPConnection._accept(this, _notify.connected(this)?, ns,
        _read_buffer_size, _yield_after_reading, _yield_after_writing)
      _count = _count + 1
    else
      @pony_os_socket_close[None](ns)
    end

  fun ref _notify_listening() =>
    """
    Inform the notifier that we're listening.
    """
    if not _event.is_null() then
      _notify.listening(this)
    else
      _closed = true
      _notify.not_listening(this)
    end

  fun ref close() =>
    """
    Dispose of resources.
    """
    if _closed then
      return
    end

    _closed = true

    if not _event.is_null() then
      // When not on windows, the unsubscribe is done immediately.
      ifdef not windows then
        @pony_asio_event_unsubscribe(_event)
      end

      @pony_os_socket_close[None](_fd)
      _fd = -1

      _notify.closed(this)
    end
