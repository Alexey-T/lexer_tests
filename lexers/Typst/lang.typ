// Test setting the document language.

---
// Ensure that setting the language does have effects.
#set text(hyphenate: true)
#grid(
  columns: 2 * (20pt,),
  gutter: 1fr,
  text(lang: "en")["Eingabeaufforderung"],
  text(lang: "de")["Eingabeaufforderung"],
)

---
// Test that the language passed to the shaper has an effect.
#set text(font: "Ubuntu")

// Some lowercase letters are different in Serbian Cyrillic compared to other
// Cyrillic languages. Since there is only one set of Unicode codepoints for
// Cyrillic, these can only be seen when setting the language to Serbian and
// selecting one of the few fonts that support these letterforms.
Бб
#text(lang: "uk")[Бб]
#text(lang: "sr")[Бб]

---
// Error: 17-21 expected string, found none
#set text(lang: none)

---
// Error: 17-20 expected two or three letter language code (ISO 639-1/2/3)
#set text(lang: "ӛ")

---
// Error: 17-20 expected two or three letter language code (ISO 639-1/2/3)
#set text(lang: "😃")

---
// Error: 19-24 expected two letter region code (ISO 3166-1 alpha-2)
#set text(region: "hey")
