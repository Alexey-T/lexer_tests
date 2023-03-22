natpmp::natpmp(io_context& ios
	, aux::portmap_callback& cb
	, listen_socket_handle ls)
	: m_callback(cb)
	, m_socket(ios)
	, m_send_timer(ios)
	, m_refresh_timer(ios)
	, m_ioc(ios)
	, m_listen_handle(std::move(ls))
{
	// unfortunately async operations rely on the storage
	// for this array not to be reallocated, by passing
	// around pointers to its elements. so reserve size for now
	m_mappings.reserve(10);
}

void natpmp::start(ip_interface const& ip)
{
	TORRENT_ASSERT(is_single_thread());

		if (i->protocol == portmap_protocol::none
			|| i->act != portmap_action::none)
			continue;
		i->act = portmap_action::add;
		update_mapping(port_mapping_t(int(i - m_mappings.begin())));
	}
}
