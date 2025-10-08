#let note(
  title: none,
  author: none,
  paper: "a4",
  body,
) = {
  set document(
    title: title, 
    author: author
  ) // set metadata (seen in document properties)

  set page(
    paper: paper, // enables user-definable in main.typ
    numbering: "1", // another example: "1 of 1"
  ) // page properties

  align(center)[
    #text(1.8em)[*#title*]
    #v(2em, weak: true)
    #text(1em, author)
    #v(1em, weak: true)
    #datetime.today().display("[month repr:long] [day], [year]")
    #v(5em, weak: true)
  ] // title block

  body
}
