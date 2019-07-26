Public Class AddNote
    Public Sock As Integer
    Public Yy As String = "|SPX|"

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Me.Close()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Form1.S.Send(Sock, "noteupdate" & Yy & TextBox1.Text)
        Me.Close()
    End Sub
End Class