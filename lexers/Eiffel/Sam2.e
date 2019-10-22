class SAMPLE2

creation {ANY}
   make

feature {ANY}

   chat: CHAT;

   mille_pattes: MILLE_PATTES;

   make is
      do
         !!chat;
         !!mille_pattes;
         chat.felicitations(chien);
         mille_pattes.felicitations(chat);
      end;

end -- SAMPLE2
