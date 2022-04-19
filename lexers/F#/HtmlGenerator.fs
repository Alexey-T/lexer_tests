// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// This code released under the terms of the Microsoft Public License (MS-PL)
// --------------------------------------------------------------------------------------
namespace FSharp.IntelliSense

open System
open System.IO
open System.Web
open System.Text
open System.Collections.Generic

open FSharp.IntelliSense
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

// --------------------------------------------------------------------------------------
// Formats information obtained from 'SourceParser' as a nice HTML
// --------------------------------------------------------------------------------------

/// Formats tool tips in a HTML format
/// The 'TipElements' property returns all generated HTML <div> elements
/// with tips (and is accumulated along the way, to avoid duplicating tips)
type ToolTipFormatter(prefix) = 
  let tips = new Dictionary<_, _>()
  let mutable count = 0
  let mutable uniqueId = 0

  /// Formats tip and returns assignments for 'onmouseover' and 'onmouseout'
  member x.FormatTip(tip : ToolTip) = 
    uniqueId <- uniqueId + 1
    let stringIndex =
      match tips.TryGetValue(tip.ToolTipHtml) with
      | true, idx -> idx
      | _ -> 
        count <- count + 1
        tips.Add(tip.ToolTipHtml, count)
        count
    String.Format
      ( "onmouseout=\"hideTip(event, '{0}{1}', {2})\" onmouseover=\"showTip(event, '{0}{1}', {2})\" ",
        prefix, stringIndex, uniqueId )
  
  /// Returns all generated tool tip elements
  member x.TipElements = 
    [ for (KeyValue(html, index)) in tips do
        yield sprintf "<div class=\"tip\" id=\"%s%d\">%s</div>" prefix index html ]


/// Implements the main HTML generation
module SourceFormatter = 

  /// Generate HTML with the specified snippets
  let Format (wr:TextWriter, prefix, addLines, snippets:SnippetInfo list) =
    let tipf = new ToolTipFormatter(prefix) 

    for { Lines = info; Title = title } in snippets do
      wr.WriteLine("<h2>{0}</h2>", title)
      wr.WriteLine("<pre class=\"fssnip\">")
      let length = info.Length.ToString().Length
      for last, line in info |> List.mapi (fun i el -> i = info.Length - 1, el) do
        if addLines then
          wr.Write("<span class=\"l\">{0}: </span>", line.Index.ToString().PadLeft(length))
        for token in line.Tokens do
          // Generate additional attributes if there is a Tool Tip
          let tipAttributes = 
            match token.Tip with
            | Some(tip) -> tipf.FormatTip(tip)
            | None -> ""
          match token.Color with 
          | Some(clr) ->
              // Colorize token & add tool tip
              wr.Write("<span {2}class=\"{0}\">{1}</span>", clr, HttpUtility.HtmlEncode(token.Text), tipAttributes)
          | _ -> wr.Write(HttpUtility.HtmlEncode(token.Text))
        if not last then wr.WriteLine()
        else wr.WriteLine("<a target=\"_blank\" class=\"fssniplink\" href=\"http://tomasp.net/fswebsnippets\">F# Web Snippets</a>")
          
      wr.WriteLine("</pre>")

    // Generate HTML <div> elements for tips
    wr.WriteLine("\n\n<!-- HTML code for ToolTips -->")
    for el in tipf.TipElements do
      wr.WriteLine(el)