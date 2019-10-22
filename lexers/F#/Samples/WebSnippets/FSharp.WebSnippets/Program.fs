// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// This code released under the terms of the Microsoft Public License (MS-PL)
// --------------------------------------------------------------------------------------
open System
open System.IO
open System.Xml
open System.Text
open System.Threading
open System.Windows.Forms

open FSharp.IntelliSense.Gui
open FSharp.IntelliSense
open Microsoft.FSharp.Compiler

// --------------------------------------------------------------------------------------
// Main application program - handles the GUI interaction
// --------------------------------------------------------------------------------------

type Program() = 
  let form = new MainForm()
  let mutable cts = new CancellationTokenSource()

  do
    form.webHelp.Navigate(Path.Combine(Application.StartupPath, "help.html"))
    form.txtFile.Text <- @"C:\Temp\Untitled.fsx"
    form.btnCancel.Enabled <- false

    // Load default compiler configurations from XML file
    let doc = new XmlDocument()
    doc.Load("standard.xml")
    form.comboStandard.DisplayMember <- "Item1"
    form.comboStandard.ValueMember <- "Item2"
    for nd in doc.SelectNodes("//item") do
      form.comboStandard.Items.Add(nd.Attributes.["name"].Value, nd.InnerText) |> ignore

    // When a configuration is selected, copy it to the text box
    form.comboStandard.SelectedIndexChanged.Add(fun _ ->
      if form.comboStandard.SelectedIndex <> -1 then
        let value = form.comboStandard.Items.[form.comboStandard.SelectedIndex]
        form.txtSettings.Text <- snd (unbox<string*string> value) )

    // Load file from the disk & update path label
    form.btnLoad.Click.Add(fun _ ->
      if form.openFileDialog.ShowDialog() = DialogResult.OK then
        form.txtFile.Text <- form.openFileDialog.FileName
        form.txtSource.Text <- File.ReadAllText(form.openFileDialog.FileName) )

    // Cancel the processing
    form.btnCancel.Click.Add(fun _ ->
      form.btnCancel.Enabled <- false
      form.btnParse.Enabled <- true
      form.Cursor <- Cursors.Arrow
      cts.Cancel() )

    form.btnParse.Click.Add(fun _ ->
      // Read options (so that they can be accessed from background thread)
      let file = form.txtFile.Text
      let text = form.txtSource.Text
      let lines = form.txtSource.Text.Replace("\r\n", "\n").Split([| '\n' |])
      let config, symbols = form.txtSettings.Text, form.txtSymbols.Text
      let prefix = form.txtPrefix.Text
      let addLines = form.checkLineNumbers.Checked

      // Create an asynchronous workflow to do the work
      let ctx = SynchronizationContext.Current
      let work = async {
        // Type-check & tokenize the source input
        try
          // Wrap the body in try-catch expression to simplify debugging
          let source = new SourceFile(file, text, lines, config, symbols)
          let types, errors = source.RunTypeCheck()
        
          let tokens = source.TokenizeSource()
          let info = source.ProcessSourceTokens(types, tokens)
          
          // Switch back to the user interface thread & display results
          do! Async.SwitchToContext(ctx)

          // Format errors
          form.txtErrors.Text <-
            [ for e in errors do
                yield sprintf "%d:%d %s" e.StartLine e.StartColumn e.Message ]
            |> String.concat "\r\n"

          // Generate HTML output 
          let sb = new StringBuilder()
          let wr = new StringWriter(sb)
          wr.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"> ")
          wr.WriteLine("<html xmlns=\"http://www.w3.org/1999/xhtml\">")        
          wr.WriteLine("<head><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")
          wr.WriteLine("<script type=\"text/javascript\" src=\"tips.js\"></script></head><body>")
          SourceFormatter.Format(wr, prefix, addLines, info)
          wr.WriteLine("</body></html>")
          wr.Close()
          // Show it on the form
          form.txtHtml.Text <- sb.ToString()

          // Save output to a temp file & display it in the browser
          let temp = Path.GetTempFileName()
          let dir = Path.GetDirectoryName(temp)
          File.Copy("tips.js", Path.Combine(dir, "tips.js"), true)
          File.Copy("style.css", Path.Combine(dir, "style.css"), true)
          File.WriteAllText(temp, form.txtHtml.Text)
          form.webPreview.Navigate(temp + "?inapp") 
          form.tabControl.SelectTab(1) 
          form.webPreview.Focus() |> ignore 
        with e ->  
          do! Async.SwitchToContext(ctx)
          let msg = 
            match e with
            | CompilerMissingException name -> 
              sprintf "The assembly %s (version 2.0) was not found.\n%s"
                      name "The tool requires F# CTP to be installed on the machine."
            | _ -> Utils.formatException e
          MessageBox.Show(msg, "F# Web Snippets Error", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore 

        // Enable user interface
        form.btnCancel.Enabled <- false
        form.btnParse.Enabled <- true
        form.Cursor <- Cursors.Arrow }
      
      // Display busy cursor & start the workflow
      cts <- new CancellationTokenSource()  
      form.Cursor <- Cursors.WaitCursor
      form.btnCancel.Enabled <- true
      form.btnParse.Enabled <- false

      Async.Start(work, cts.Token) )

  member x.Form = form

// --------------------------------------------------------------------------------------
// Run the program...
// --------------------------------------------------------------------------------------

[<STAThread>]
do
  Application.EnableVisualStyles()
  let prog = new Program()
  Application.Run(prog.Form)