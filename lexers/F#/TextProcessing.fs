// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// This code released under the terms of the Microsoft Public License (MS-PL)
// --------------------------------------------------------------------------------------
namespace FSharp.IntelliSense

open System
open System.IO
open System.Text
open System.Web

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

// --------------------------------------------------------------------------------------
// Implements formatting of tool tips (based on MonoDevelop integration code)
// --------------------------------------------------------------------------------------
module TipFormatter =

  let private buildFormatComment cmt (sb:StringBuilder) = 
    match cmt with
    | XmlCommentText(s) -> 
        let encoded = HttpUtility.HtmlEncode(s.Trim([| ' '; '\r'; '\n' |]))
        sb.AppendLine("\n<em>" + encoded + "</em>")
    // TODO: For 'XmlCommentSignature' we could get documentation from 'xml' files
    | _ -> sb

  // If 'isSingle' is true (meaning that this is the only tip displayed)
  // then we add first line "Multiple overloads" because MD prints first
  // int in bold (so that no overload is highlighted)
  let private buildFormatElement isSingle el (sb:StringBuilder) =
    match el with 
    | DataTipElementNone -> sb
    | DataTipElement(it, comment) -> 
        sb.AppendLine(HttpUtility.HtmlEncode(it)) 
        |> buildFormatComment comment
    | DataTipElementGroup(items) -> 
        let items, msg = 
          if items.Length > 10 then 
            (items |> Seq.take 10 |> List.ofSeq), 
              sprintf "   <em>(+%d other overloads)</em>" (items.Length - 10) 
          else items, null
        if (isSingle && items.Length > 1) then
          sb.AppendLine("Multiple overloads") |> ignore
        for (it, comment) in items do
          sb.AppendLine(HttpUtility.HtmlEncode(it)) |> buildFormatComment comment |> ignore
        if msg <> null then sb.AppendFormat(msg) else sb
    | DataTipElementCompositionError(err) -> 
        sb.Append("Composition error: " + HttpUtility.HtmlEncode(err))
      
  let private buildFormatTip tip (sb:StringBuilder) = 
    match tip with
    | DataTipText([single]) -> sb |> buildFormatElement true single
    | DataTipText(its) -> 
        sb.AppendLine("Multiple items") |> ignore
        its |> Seq.mapi (fun i it -> i = 0, it) |> Seq.fold (fun sb (first, item) ->
          if not first then sb.AppendLine("\n--------------------\n") |> ignore
          sb |> buildFormatElement false item) sb

  /// Format tool-tip that we get from the language service as string        
  let formatTip tip = 
    (buildFormatTip tip (new StringBuilder())).ToString().Trim('\n', '\r')

  /// Formats tool-tip and turns the first line into heading
  /// MonoDevelop does this automatically for completion data, 
  /// so we do the same thing explicitly for hover tool-tips
  let formatTipWithHeader tip = 
    let str = formatTip tip
    let parts = str.Split([| '\n' |], 2)
    "<strong>" + parts.[0] + "</strong>" +
      (if parts.Length > 1 then "<small>\n" + parts.[1] + "</small>" else "")

// --------------------------------------------------------------------------------------
// Implements formatting of tool tips (based on MonoDevelop integration code)
// --------------------------------------------------------------------------------------
module TextProcessing =

  /// Tests whether string starts with the given value (and returns rest of it)
  let (|StartsWith|_|) arg (s:string) =
    if s.StartsWith(arg) then Some(s.Substring(arg.Length).Trim())
    else None

  /// Tests whether string starts with the given value (and returns rest of it)
  let (|StartsAndEndsWith|_|) (starts, ends) (s:string) =
    if s.StartsWith(starts) && s.EndsWith(ends) && s.Length >= starts.Length + ends.Length then 
      Some(s.Substring(starts.Length, s.Length - starts.Length - ends.Length).Trim())
    else None

  /// Finds special commands (comments) in the source code. If there are commands, then
  /// we only generate HTML for parts of source (snippets). This function returns a list
  /// of snippets. The commands should be:
  ///    // [snippet:Some title]
  ///    ... some F# code ...
  ///    // [/snippet]
  let rec getSnippets state (snippets:_ list) source (lines:string[]) =
    match source with 
    | [] -> snippets
    | (line, tokens)::rest ->
      let text = lines.[line].Trim()
      match state, text with

      // We're not inside a snippet and we found a beginning of one
      | None, StartsWith "//" (StartsWith "[snippet:" title) ->
          let title = title.Substring(0, title.IndexOf(']'))
          getSnippets (Some(title, [])) snippets rest lines
      // Not inside a snippet and there is a usual line
      | None, _ -> 
          getSnippets state snippets rest lines

      // We're inside a snippet and it ends
      | Some(title, acc), StartsWith "//" (StartsWith "[/snippet]" _) ->
          getSnippets None ((title, acc |> List.rev)::snippets) rest lines
      // We're inside snippet - add current line to it
      | Some(title, acc), _ -> 
          getSnippets (Some(title, (line, tokens)::acc)) snippets rest lines


  /// Preprocesses a line and merges all subsequent comments on a line 
  /// into a single long comment (so that we can parse it as snippet command)
  let rec mergeComments line cmt acc = 
    match line, cmt with 
    | [], Some(cmt) -> cmt::acc |> List.rev
    | [], None -> acc |> List.rev
    | (str, (tok:TokenInformation))::line, None when tok.TokenName = "COMMENT" ->
        mergeComments line (Some(str, tok)) acc
    | (str, tok)::line, Some(scmt, cmt) when tok.TokenName = "COMMENT" ->
        let ncmt = cmt.WithRightColumn(tok.RightColumn)
        mergeComments line (Some(scmt+str, ncmt)) acc
    | (str, tok)::line, None ->
        mergeComments line None ((str, tok)::acc)
    | (str, tok)::line, Some(cmt) ->
        mergeComments line None ((str, tok)::cmt::acc)


  /// Continue reading shrinked code until we reach the end (*[/omit]*) tag
  /// (see the function below for more information and beginning of shrinking)
  let rec shrinkOmittedCode (text:StringBuilder) line content source = 
    match content, source with 
    // Take the next line, merge comments and continue looking for end
    | [], (line, content)::source -> 
        shrinkOmittedCode (text.Append("\n")) line (mergeComments content None []) source
    | (StartsAndEndsWith ("(*", "*)") "[/omit]", tok)::rest, source 
          when tok.TokenName = "COMMENT" ->
        line, rest, source, text
    | (str, tok)::rest, _ -> 
        shrinkOmittedCode (text.Append(str)) line rest source
    | [], [] -> line, [], [], text


  /// Find all code marked using the (*[omit:<...>]*) tags and replace it with 
  /// a special token (named "OMIT...." where "...." is a replacement string)
  let rec shrinkLine line content source = 
    match content with 
    | [] -> [], source
    | (StartsAndEndsWith ("(*", "*)") (StartsAndEndsWith ("[omit:", "]") body), (tok:TokenInformation))::rest
          when tok.TokenName = "COMMENT" -> 
        let line, remcontent, source, text = 
          shrinkOmittedCode (StringBuilder()) line rest source
        let line, source = shrinkLine line remcontent source
        (body, tok.WithTokenName("OMIT" + (text.ToString()) ))::line, source
    | (str, tok)::rest -> 
        let line, source = shrinkLine line rest source
        (str, tok)::line, source

  /// Process the whole source file and shrink all blocks marked using
  /// special 'omit' meta-comments (see the two functions above)
  let rec shrinkOmittedParts source = seq {
    match source with 
    | [] -> ()
    | (line, content)::source -> 
      let content, source = shrinkLine line (mergeComments content None []) source 
      yield line, content
      yield! shrinkOmittedParts source }


