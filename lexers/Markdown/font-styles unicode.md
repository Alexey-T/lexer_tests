- - -
Good style markers:
*пitalic textп* and **пboldп**

*пitalic text <span>HTML element</span> end of italic textп*
_пitalic text <SPAN>HTML element</SPAN> end of italic textп_
**пbold text <span>HTML element</span> end of bold textп**
__пbold text <span>HTML element</span> end of bold textп__

This is ***пbold italicп***
This is ***пbold italicп* and just boldп**
This is **_пbold italicп_**
This is __*пbold italicп*__
This is ___пbold italicп___
This is ___пbold italicп_ and just boldп__
This is _**пitalic boldп**_
This is *__пitalic boldп__*

__~~пbold strikedп~~__
**~~пbold strikedп~~**
_~~пitalic strikedп~~_
*~~пitalic strikedп~~*
___~~пbold italic strikedп~~___
***~~пbold italic strikedп~~***
~пHiп~ Hello, world!

- - - -
Bad style markers:
*begin * end*
**begin ** end**
***begin *** end***
_begin _ end_
__begin __ end__
___begin ___ end___

---
layout: post
title: Some text
---
[![Test](link here)](link here)

Additional {++[Link](https://foo.bar)++} and {++![Image](images/image.png)++}.
This is a {-- deletion --} and {~~substitute~>with~~striked~~text~~}
This is a {>> comment <<}.
This is an {== information ==} {>> comment <<}.

# Header
<!-- This is a comment -->
/* This, too */ <kbd>Ctrl+C</kbd> and <tagbegin test="test">
$$ math here $$

`get_prop` id "token_type"; optional `get_prop` param

`Inline Code` inside a paragraph with *italic*, 
**bold** and ***bold italic***, [^footnote] and + 
[reference][ref] and [reference] [ref]. <HTML> syntax and special &harr; 
chars are highlighted, [Hyperlink text](url "title") 
and ![alternative text](image address "title"). 
test: ~~~~crossed~ and ~crossed~~~ is supported 

- - -
*  *  *
_  _  _  _

```c
void F(int s) { return 20; }
```
and

   ~~~php
void N(n) { return 20; }
   ~~~

    Codeblock by indent

	1. But not for numbered
	-  or unordered lists
    	+ with several indentions
        	* leading digit 
123. note: unordered needs a sign & blank
	(a) extended Pandoc interpretation is supported
    	i) with roman numbering (i)    
- [ ] check box
- [x] check box
- [X] check box
12. [x] list item
a) [x] list item

(@) Pandoc Numbered example lists
(@ref) with or without reference, where (@ref) is
handled as reference

> quoted **text**
> > *markup* in quote
>        > code in quote, not 2nd quote
  >= quote

Definition lists
:   In a single line

    indented definition lists

	:   paragraph (blank line between)

[ref]: url "title"
[^footnote]: this is a footnote *italic* 

/* Optional */
header
=

Test {++added++} and {--deleted--} and {~~edited text~~} end.    
