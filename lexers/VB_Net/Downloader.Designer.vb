<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Downloader
    Inherits System.Windows.Forms.Form

    'Form esegue l'override del metodo Dispose per pulire l'elenco dei componenti.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Richiesto da Progettazione Windows Form
    Private components As System.ComponentModel.IContainer

    'NOTA: la procedura che segue è richiesta da Progettazione Windows Form
    'Può essere modificata in Progettazione Windows Form.  
    'Non modificarla mediante l'editor del codice.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Downloader))
        Me.Button1 = New System.Windows.Forms.Button()
        Me.SFD = New System.Windows.Forms.SaveFileDialog()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.DotNetBarTabcontrol1 = New RT101V001.DotNetBarTabcontrol()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.CheckBox5 = New System.Windows.Forms.CheckBox()
        Me.CheckBox6 = New System.Windows.Forms.CheckBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Divider2 = New RT101V001.Divider()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.CheckBox3 = New System.Windows.Forms.CheckBox()
        Me.CheckBox4 = New System.Windows.Forms.CheckBox()
        Me.CheckBox2 = New System.Windows.Forms.CheckBox()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.TextBox3 = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Divider1 = New RT101V001.Divider()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Divider3 = New RT101V001.Divider()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.DotNetBarTabcontrol1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.Location = New System.Drawing.Point(501, 508)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(153, 34)
        Me.Button1.TabIndex = 11
        Me.Button1.Text = "Build Downloader"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        Me.PictureBox1.Anchor = System.Windows.Forms.AnchorStyles.Right
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(453, 277)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(34, 21)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 60
        Me.PictureBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox1, "Admin Privileges Requested")
        '
        'DotNetBarTabcontrol1
        '
        Me.DotNetBarTabcontrol1.Alignment = System.Windows.Forms.TabAlignment.Left
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage1)
        Me.DotNetBarTabcontrol1.Dock = System.Windows.Forms.DockStyle.Top
        Me.DotNetBarTabcontrol1.ItemSize = New System.Drawing.Size(44, 136)
        Me.DotNetBarTabcontrol1.Location = New System.Drawing.Point(0, 0)
        Me.DotNetBarTabcontrol1.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.DotNetBarTabcontrol1.Multiline = True
        Me.DotNetBarTabcontrol1.Name = "DotNetBarTabcontrol1"
        Me.DotNetBarTabcontrol1.SelectedIndex = 0
        Me.DotNetBarTabcontrol1.Size = New System.Drawing.Size(658, 501)
        Me.DotNetBarTabcontrol1.SizeMode = System.Windows.Forms.TabSizeMode.Fixed
        Me.DotNetBarTabcontrol1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.BackColor = System.Drawing.Color.White
        Me.TabPage1.Controls.Add(Me.CheckBox5)
        Me.TabPage1.Controls.Add(Me.CheckBox6)
        Me.TabPage1.Controls.Add(Me.Label8)
        Me.TabPage1.Controls.Add(Me.Divider2)
        Me.TabPage1.Controls.Add(Me.Label7)
        Me.TabPage1.Controls.Add(Me.PictureBox1)
        Me.TabPage1.Controls.Add(Me.CheckBox3)
        Me.TabPage1.Controls.Add(Me.CheckBox4)
        Me.TabPage1.Controls.Add(Me.CheckBox2)
        Me.TabPage1.Controls.Add(Me.CheckBox1)
        Me.TabPage1.Controls.Add(Me.Label18)
        Me.TabPage1.Controls.Add(Me.TextBox3)
        Me.TabPage1.Controls.Add(Me.Label6)
        Me.TabPage1.Controls.Add(Me.Label5)
        Me.TabPage1.Controls.Add(Me.Divider1)
        Me.TabPage1.Controls.Add(Me.Label4)
        Me.TabPage1.Controls.Add(Me.Label3)
        Me.TabPage1.Controls.Add(Me.Divider3)
        Me.TabPage1.Controls.Add(Me.Label9)
        Me.TabPage1.Controls.Add(Me.Label2)
        Me.TabPage1.Controls.Add(Me.Button2)
        Me.TabPage1.Controls.Add(Me.TextBox2)
        Me.TabPage1.Controls.Add(Me.Label1)
        Me.TabPage1.Controls.Add(Me.Label10)
        Me.TabPage1.Controls.Add(Me.TextBox1)
        Me.TabPage1.Location = New System.Drawing.Point(140, 4)
        Me.TabPage1.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.TabPage1.Size = New System.Drawing.Size(514, 493)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Settings"
        '
        'CheckBox5
        '
        Me.CheckBox5.AutoSize = True
        Me.CheckBox5.Checked = True
        Me.CheckBox5.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox5.Location = New System.Drawing.Point(398, 438)
        Me.CheckBox5.Name = "CheckBox5"
        Me.CheckBox5.Size = New System.Drawing.Size(49, 23)
        Me.CheckBox5.TabIndex = 65
        Me.CheckBox5.Text = "No"
        Me.CheckBox5.UseVisualStyleBackColor = True
        '
        'CheckBox6
        '
        Me.CheckBox6.AutoSize = True
        Me.CheckBox6.Location = New System.Drawing.Point(339, 438)
        Me.CheckBox6.Name = "CheckBox6"
        Me.CheckBox6.Size = New System.Drawing.Size(52, 23)
        Me.CheckBox6.TabIndex = 64
        Me.CheckBox6.Text = "Yes"
        Me.CheckBox6.UseVisualStyleBackColor = True
        '
        'Label8
        '
        Me.Label8.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(46, 439)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(147, 19)
        Me.Label8.TabIndex = 63
        Me.Label8.Text = "Run as administrator : "
        '
        'Divider2
        '
        Me.Divider2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider2.ForeColor = System.Drawing.SystemColors.ActiveBorder
        Me.Divider2.Location = New System.Drawing.Point(117, 413)
        Me.Divider2.Name = "Divider2"
        Me.Divider2.Size = New System.Drawing.Size(389, 23)
        Me.Divider2.TabIndex = 62
        Me.Divider2.Text = "Divider2"
        '
        'Label7
        '
        Me.Label7.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(19, 401)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(91, 19)
        Me.Label7.TabIndex = 61
        Me.Label7.Text = "Admin Rights"
        '
        'CheckBox3
        '
        Me.CheckBox3.AutoSize = True
        Me.CheckBox3.Location = New System.Drawing.Point(398, 306)
        Me.CheckBox3.Name = "CheckBox3"
        Me.CheckBox3.Size = New System.Drawing.Size(49, 23)
        Me.CheckBox3.TabIndex = 59
        Me.CheckBox3.Text = "No"
        Me.CheckBox3.UseVisualStyleBackColor = True
        '
        'CheckBox4
        '
        Me.CheckBox4.AutoSize = True
        Me.CheckBox4.Checked = True
        Me.CheckBox4.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox4.Location = New System.Drawing.Point(339, 306)
        Me.CheckBox4.Name = "CheckBox4"
        Me.CheckBox4.Size = New System.Drawing.Size(52, 23)
        Me.CheckBox4.TabIndex = 58
        Me.CheckBox4.Text = "Yes"
        Me.CheckBox4.UseVisualStyleBackColor = True
        '
        'CheckBox2
        '
        Me.CheckBox2.AutoSize = True
        Me.CheckBox2.Location = New System.Drawing.Point(398, 277)
        Me.CheckBox2.Name = "CheckBox2"
        Me.CheckBox2.Size = New System.Drawing.Size(49, 23)
        Me.CheckBox2.TabIndex = 57
        Me.CheckBox2.Text = "No"
        Me.CheckBox2.UseVisualStyleBackColor = True
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.Checked = True
        Me.CheckBox1.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox1.Location = New System.Drawing.Point(339, 277)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(52, 23)
        Me.CheckBox1.TabIndex = 56
        Me.CheckBox1.Text = "Yes"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'Label18
        '
        Me.Label18.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label18.AutoSize = True
        Me.Label18.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label18.Location = New System.Drawing.Point(46, 239)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(266, 19)
        Me.Label18.TabIndex = 55
        Me.Label18.Text = "* Highly recommended settings to enable."
        '
        'TextBox3
        '
        Me.TextBox3.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox3.Location = New System.Drawing.Point(209, 354)
        Me.TextBox3.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.TextBox3.Name = "TextBox3"
        Me.TextBox3.Size = New System.Drawing.Size(293, 26)
        Me.TextBox3.TabIndex = 54
        '
        'Label6
        '
        Me.Label6.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(46, 357)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(128, 19)
        Me.Label6.TabIndex = 53
        Me.Label6.Text = "Registry key name :"
        '
        'Label5
        '
        Me.Label5.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(46, 310)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(179, 19)
        Me.Label5.TabIndex = 50
        Me.Label5.Text = "Run on computer startup :  "
        '
        'Divider1
        '
        Me.Divider1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider1.ForeColor = System.Drawing.SystemColors.ActiveBorder
        Me.Divider1.Location = New System.Drawing.Point(205, 213)
        Me.Divider1.Name = "Divider1"
        Me.Divider1.Size = New System.Drawing.Size(301, 23)
        Me.Divider1.TabIndex = 49
        Me.Divider1.Text = "Divider1"
        '
        'Label4
        '
        Me.Label4.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(19, 201)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(180, 19)
        Me.Label4.TabIndex = 48
        Me.Label4.Text = "Antivirus && Startup Settings"
        '
        'Label3
        '
        Me.Label3.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(46, 40)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(404, 19)
        Me.Label3.TabIndex = 47
        Me.Label3.Text = "Enter the link to download the file and specify an encryption key."
        '
        'Divider3
        '
        Me.Divider3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider3.ForeColor = System.Drawing.SystemColors.ActiveBorder
        Me.Divider3.Location = New System.Drawing.Point(117, 26)
        Me.Divider3.Name = "Divider3"
        Me.Divider3.Size = New System.Drawing.Size(389, 23)
        Me.Divider3.TabIndex = 46
        Me.Divider3.Text = "Divider3"
        '
        'Label9
        '
        Me.Label9.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(19, 14)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(92, 19)
        Me.Label9.TabIndex = 45
        Me.Label9.Text = "Basic Settings"
        '
        'Label2
        '
        Me.Label2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(46, 281)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(98, 19)
        Me.Label2.TabIndex = 42
        Me.Label2.Text = "Antivirus kill :  "
        '
        'Button2
        '
        Me.Button2.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button2.Location = New System.Drawing.Point(361, 157)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(141, 27)
        Me.Button2.TabIndex = 41
        Me.Button2.Text = "Randomize Key"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'TextBox2
        '
        Me.TextBox2.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox2.BackColor = System.Drawing.Color.WhiteSmoke
        Me.TextBox2.ForeColor = System.Drawing.Color.Gray
        Me.TextBox2.Location = New System.Drawing.Point(209, 124)
        Me.TextBox2.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.ReadOnly = True
        Me.TextBox2.Size = New System.Drawing.Size(293, 26)
        Me.TextBox2.TabIndex = 40
        Me.TextBox2.Text = "春論t苑O8語k0t白家Pkch"
        '
        'Label1
        '
        Me.Label1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(46, 127)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(140, 19)
        Me.Label1.TabIndex = 39
        Me.Label1.Text = "Link Encryption Key : "
        '
        'Label10
        '
        Me.Label10.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(46, 93)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(85, 19)
        Me.Label10.TabIndex = 38
        Me.Label10.Text = "Direct Link : "
        '
        'TextBox1
        '
        Me.TextBox1.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox1.Location = New System.Drawing.Point(209, 90)
        Me.TextBox1.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(293, 26)
        Me.TextBox1.TabIndex = 0
        '
        'Downloader
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 19.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(658, 554)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.DotNetBarTabcontrol1)
        Me.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Margin = New System.Windows.Forms.Padding(3, 4, 3, 4)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Downloader"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Downloader"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.DotNetBarTabcontrol1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents DotNetBarTabcontrol1 As DotNetBarTabcontrol
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents Button1 As Button
    Friend WithEvents Label2 As Label
    Friend WithEvents Button2 As Button
    Friend WithEvents TextBox2 As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents Label10 As Label
    Friend WithEvents TextBox3 As TextBox
    Friend WithEvents Label6 As Label
    Friend WithEvents Label5 As Label
    Friend WithEvents Divider1 As Divider
    Friend WithEvents Label4 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents Divider3 As Divider
    Friend WithEvents Label9 As Label
    Friend WithEvents Label18 As Label
    Friend WithEvents CheckBox3 As CheckBox
    Friend WithEvents CheckBox4 As CheckBox
    Friend WithEvents CheckBox2 As CheckBox
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents SFD As SaveFileDialog
    Friend WithEvents CheckBox5 As CheckBox
    Friend WithEvents CheckBox6 As CheckBox
    Friend WithEvents Label8 As Label
    Friend WithEvents Divider2 As Divider
    Friend WithEvents Label7 As Label
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents ToolTip1 As ToolTip
End Class
