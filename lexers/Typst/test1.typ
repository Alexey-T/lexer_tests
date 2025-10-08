set page(
  header: context {
    // set custom header: make title appear on even pages
    if calc.even(counter(page).get().first()) { 
      emph(title) 
    } else { none }

    // make section appear on odd pages other than the first
    let page-num = counter(page).get().first()
    if page-num > 1 and calc.odd(page-num) {
      let headings = query(heading)
      let curr-heading = none
      let found = false

      for heading-elem in headings {
        if heading-elem.location() != none and heading-elem.location().page() == page-num {
          curr-heading = heading-elem.body
          found = true
        } else if heading-elem.location() != none and heading-elem.location().page() < page-num {
          curr-heading = heading-elem.body // keep track of the last heading on a prev page
        } else if found { break } // stop once we have moved past the curr page
      }
      align(right, emph(curr-heading))
    } else { none }
  }, // context ends
) // page ends
