using System;
using System.Linq;
using System.Collections.Generic;
using System.Web;
using System.Text;
using System.Web.UI;
using System.Web.UI.WebControls;

using FSharp.IntelliSense;
using Microsoft.FSharp.Core;
using System.IO;

namespace FSharpIntelliSense.Web
{
  public partial class _Default : System.Web.UI.Page
  {
    /// <summary>
    /// Gets or sets the number of times the tool has been used (it keeps the number in a file
    /// which is pretty simple and probably not-quite-correct way of doing that, but anyway...)
    /// </summary>
    public int Counter {
      get {
        var obj = Application["toolcounter"];
        if (obj == null) {
          // Load from file if we don't have the value yet
          obj = Int32.Parse(File.ReadAllText(Server.MapPath("~/App_Data/Count.txt")));
          Application["toolcounter"] = obj;
        }
        return (int)obj;
      }
      set {
        Application["toolcounter"] = value;
        // Save to file in case application stops
        File.WriteAllText(Server.MapPath("~/App_Data/Count.txt"), value.ToString());
      }
    }

    protected void Page_Load(object sender, EventArgs e)
    {
      // Load default script file if the page is laoded for the first time
      // & update the displayed use-counter number
      if (!Page.IsPostBack && txtSource.Text == "")
        txtSource.Text = File.ReadAllText(Server.MapPath("~/App_Data/Script.fsx"));
      lblCounter.InnerText = Counter.ToString();
    }

    protected void btnParse_Click(object sender, EventArgs e)
    {
      // Override default command line settings.
      // This adds reference to 'FSharp.Core.dll' in 'App_Data' to make sure that the
      // website can be deployed on a server that doesn't have F# installed
      var stg = 
        "--noframework -r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\mscorlib.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Xml.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Runtime.Remoting.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Runtime.Serialization.Formatters.Soap.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Data.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Drawing.dll " +
        "-r:\"C:\\Program Files\\Reference Assemblies\\Microsoft\\Framework\\v3.5\\System.Core.dll\" " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Web.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Web.Services.dll " +
        "-r:C:\\Windows\\Microsoft.NET\\Framework\\v2.0.50727\\System.Windows.Forms.dll " +
        "-r:\"" + Server.MapPath("~/App_Data/FSharp.Core.dll") + "\"";

      // Run the formatting tool...
      var file = "C:\\Temp\\Demo.fsx";
      var text = txtSource.Text;
      var lines = txtSource.Text.Replace("\r\n", "\n").Split('\n');
      var prefix = txtPrefix.Text;

      var source = new SourceFile(file, text, lines, FSharpOption<string>.Some(stg), FSharpOption<string>.None);
      var res = source.RunTypeCheck(FSharpOption<int>.None);
      var tokens = source.TokenizeSource();
      var info = source.ProcessSourceTokens(res.Item1, tokens);
      
      // Format errors reported from the tool
      lblErrors.Text = 
        res.Item2.Count() == 0 ? "Source code processed successfully." :
        String.Concat
          (res.Item2.Select(er => 
             String.Format("{0}:{1} {2}<br />", 
               er.StartLine, er.StartColumn, er.Message)).ToArray());
      
      // Format & display the obtained information 
      var sb = new StringBuilder();
      var wr = new StringWriter(sb);
      SourceFormatter.Format(wr, prefix, checkLines.Checked, info);
      var html = sb.ToString();

      // Display the whole web page in the textarea and only source in <div>
      plcHtmlOutput.InnerHtml = html;
      html = 
        "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"> " +
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">" +
        "<head><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n" +
        "<script type=\"text/javascript\" src=\"tips.js\"></script></head><body>\n" +
        html + "\n</body></html>";
      txtHtmlOutput.Text = html;
      
      // Increment the counter
      var cnt = Counter + 1;
      lblCounter.InnerText = cnt.ToString();
      Counter = cnt;
    }
  }
}