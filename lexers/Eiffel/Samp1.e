class SAMPLE1

creation {ANY}
   make

feature {ANY}

   chien: CHIEN;

   chat: CHAT;

   mille_pattes: MILLE_PATTES;

   make is
      do
         !!chat;
         !!chien;
         !!mille_pattes;
         chat.felicitations(chien);
         mille_pattes.felicitations(chat);
      end;

end -- SAMPLE1
