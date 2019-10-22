<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Default.aspx.cs" Inherits="FSharpIntelliSense.Web._Default" ValidateRequest="false" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"> 
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>F# Web Snippets - Online source code formatting | TomasP.Net</title>
    <link rel="stylesheet" type="text/css" href="http://tomasp.net/style.css" />
    <script type="text/javascript" src="http://tomasp.net/script/common.js"></script> 
</head>
<body>
 <div id="body"> 
  <div id="body1"> 
  <div id="body2"> 
  
  <div id="header"> 
    <a href="http://tomasp.net"><img src="http://tomasp.net/design2/blank.gif" id="ctl00_Img1" /></a> 
    <div id="header2"> 
      <div><a href="http://tomasp.net/blog">Blog</a></div> 
      <div><a href="http://tomasp.net/academic">Academia</a></div> 
      <div><a href="http://tomasp.net/work">Work</a></div> 
      <div><a href="http://tomasp.net/about">More...</a></div> 
    </div> 
  </div> 
  
  <div id="page"> 
    <div id="wholecontent"><div id="content"> 
    
  <h1 class="hp">F# Web Snippets</h1> 
    <p>F# Web Snippets is a tool for formatting your F# source code for web pages or blogs. It does colorization
      of the code and, more interestingly, adds <strong>tool tips with types</strong> and other information. 
      Immediate feedback from the IDE is verey important when reading and understanding F# code, but when you
      post a code snippet to a web, the type information is gone! Thanks to F# Web Snippets, you can get
      tool tips with type signatures on the web as well.</p>
    <p>This page is a simple (and limited) web based version of the tool. It doesn't allow you to reference
      additional files or libraries. It is also quite slow (it runs F# compiler under the cover), so please
      download a Windows Forms based version if you plan to use the tool more frequently.</p>
    <img src="logo.png" style="float:right;margin:5px 0px 20px 10px"/>
    <ul>
      <li>For more information, visit <a href="http://tomasp.net/projects/fswebsnippets.aspx"><strong>F# Web Snippets</strong></a> homepage</li>
      <li>Download a stand-alone Windows Forms based <a href="http://tomasp.net/fswebsnippets/fswebsnippets.zip"><strong>F# Web Snippets</strong></a> application.</li>
      <li>Read more about the tool in a <a href="http://tomasp.net/blog/introducing-fswebsnippets.aspx">blog post</a> (and get the source code too)</li>
      <li>Download <a href="http://tomasp.net/fswebsnippets/templates.zip">JavaScript and CSS</a> templates for your blog or web</li>
    </ul>
    <p>The tool was used <strong id="lblCounter" runat="server"></strong> times so far and counting...</p>

    <form id="form1" runat="server">
    <div>
      <h2>F# Source code</h2>
      <table>
      <tr><td colspan="2">
        <asp:TextBox runat="server" Rows="15" Columns="80" TextMode="MultiLine" ID="txtSource"></asp:TextBox> 
      </td></tr>
      <tr>
        <td>Prefix for generated HTML 'id' attributes: <asp:TextBox Text="fstip" runat="server" ID="txtPrefix"></asp:TextBox><br />
          <asp:CheckBox Checked="true" Text="Add line numbers" ID="checkLines" runat="server" /></td>
        <td align="right"><asp:Button runat="server" Text="  Parse  " ID="btnParse" onclick="btnParse_Click" /></td>
      </tr>
      <tr>
      <td colspan="2">
        <asp:Label ForeColor="#800000" ID="lblErrors"  runat="server"/>
      </td>
      </tr>
      </table>

      <div runat="server" ID="plcHtmlOutput"></div>
      <h2>Generated HTML code</h2>
      <table><tr><td>
        <asp:TextBox runat="server" Rows="15" Columns="80" TextMode="MultiLine" ID="txtHtmlOutput"></asp:TextBox>
      </td></tr></table>
    </div>
    </form>


    </div></div>
  </div>

 <div id="footer"> 
    © Tomas Petricek (<a href="mailto:tomas@tomasp.net">tomas@tomasp.net</a>), 2010,
 		<script src="http://c1.navrcholu.cz/code?site=77870;t=lb14" type="text/javascript"></script> 
 		<noscript><div><a href="http://navrcholu.cz/"><img src="http://c1.navrcholu.cz/hit?site=77870;t=lb14;ref=;jss=0" width="14" height="14" alt="NAVRCHOLU.cz" style="border:none" /></a></div></noscript> 
		<script src="http://www.google-analytics.com/urchin.js" type="text/javascript"></script><script type="text/javascript">		                                                                                          _uacct = "UA-1561220-1"; urchinTracker();</script> 
  </div> 
  </div></div>
 </div>
</body>
</html>
