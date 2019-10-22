use "net"

class Ping is UDPNotify
  let _env: Env
  let _ip: NetAddress

  new create(env: Env, ip: NetAddress) =>
    _env = env
    _ip = ip

  fun ref listening(sock: UDPSocket ref) =>
    try
      (let host, let service) = sock.local_address().name()?
      _env.out.print("Ping: listening on " + host + ":" + service)
      sock.write("ping!", _ip)
    else
      _env.out.print("Ping: couldn't get local name")
      sock.dispose()
    end

  fun ref not_listening(sock: UDPSocket ref) =>
    _env.out.print("Ping: not listening")
    sock.dispose()

  fun ref received(sock: UDPSocket ref, data: Array[U8] iso, from: NetAddress)
  =>
    try
      (let host, let service) = from.name()?
      _env.out.print("from " + host + ":" + service)
    end

    _env.out.print(consume data)
    sock.dispose()

  fun ref closed(sock: UDPSocket ref) =>
    _env.out.print("Ping: closed")
