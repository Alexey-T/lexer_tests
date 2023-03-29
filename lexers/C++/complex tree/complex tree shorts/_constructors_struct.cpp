	struct piece_refcount
	{
		piece_refcount(piece_picker& p, piece_index_t piece)
			: m_picker(p)
			, m_piece(piece)
		{
			m_picker.inc_refcount(m_piece, nullptr);
		}

		piece_refcount(piece_refcount const&) = delete;
		piece_refcount& operator=(piece_refcount const&) = delete;

		~piece_refcount()
		{
			m_picker.dec_refcount(m_piece, nullptr);
		}

	private:
		piece_picker& m_picker;
		piece_index_t m_piece;
	};
