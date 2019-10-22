class SAMPLE3

creation {ANY}
   make

feature {ANY}

   make is
      local
         chat1, chat2: CHAT;
         t_chat: ARRAY[CHAT];
         medor: CHIEN;
         t_chien: ARRAY[CHIEN];
      do
         !!chat1;
         !!chat2;
         t_chat := <<chat1,chat2>>;
         t_chat.item(1).crier;
         !!medor;
         t_chien := <<medor,medor>>;
         t_chien.item(1).crier;
      end;

end -- SAMPLE3
