namespace libtorrent {

	m_socket.open(local_address.is_v4() ? udp::v4() : udp::v6(), ec);
	if (ec)
	{
		disable(ec);
		return;
	}
	m_socket.bind({local_address, 0}, ec);
	if (ec)
	{
		disable(ec);
		return;
	}

	ADD_OUTSTANDING_ASYNC("natpmp::on_reply");
	m_socket.async_receive_from(boost::asio::buffer(&m_response_buffer[0]
		, sizeof(m_response_buffer))
		, m_remote, std::bind(&natpmp::on_reply, self(), _1, _2));
	if (m_version == version_natpmp)
		send_get_ip_address_request();

	for (auto i = m_mappings.begin(), end(m_mappings.end()); i != end; ++i)
	{
		if (i->protocol == portmap_protocol::none
			|| i->act != portmap_action::none)
			continue;
		i->act = portmap_action::add;
		update_mapping(port_mapping_t(int(i - m_mappings.begin())));
	}
}
