<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Builder
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Builder))
        Me.Button1 = New System.Windows.Forms.Button()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.DeleteProfileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FlushToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DotNetBarTabcontrol1 = New RT101V001.DotNetBarTabcontrol()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Label31 = New System.Windows.Forms.Label()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.Divider8 = New RT101V001.Divider()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.notesclient = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Divider4 = New RT101V001.Divider()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Divider3 = New RT101V001.Divider()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.version = New System.Windows.Forms.TextBox()
        Me.botidtxt = New System.Windows.Forms.TextBox()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.Label32 = New System.Windows.Forms.Label()
        Me.TextBox6 = New System.Windows.Forms.TextBox()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.Divider6 = New RT101V001.Divider()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.passbuild = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.ListBox1 = New System.Windows.Forms.ListBox()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Divider5 = New RT101V001.Divider()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.TextBox2 = New System.Windows.Forms.TextBox()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.startup = New System.Windows.Forms.CheckBox()
        Me.Divider2 = New RT101V001.Divider()
        Me.Divider1 = New RT101V001.Divider()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.CheckBox2 = New System.Windows.Forms.CheckBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.instshit = New System.Windows.Forms.CheckBox()
        Me.sysfold = New System.Windows.Forms.RadioButton()
        Me.prgfold = New System.Windows.Forms.RadioButton()
        Me.usrfold = New System.Windows.Forms.RadioButton()
        Me.installfnametxt = New System.Windows.Forms.TextBox()
        Me.installsubdir = New System.Windows.Forms.TextBox()
        Me.InstallLocationPre = New System.Windows.Forms.TextBox()
        Me.TextBox5 = New System.Windows.Forms.TextBox()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.DotNetBarTabcontrol1.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.Location = New System.Drawing.Point(581, 491)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(153, 34)
        Me.Button1.TabIndex = 10
        Me.Button1.Text = "Build Client"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'PictureBox2
        '
        Me.PictureBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.PictureBox2.Image = CType(resources.GetObject("PictureBox2.Image"), System.Drawing.Image)
        Me.PictureBox2.Location = New System.Drawing.Point(492, 120)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(83, 21)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox2.TabIndex = 33
        Me.PictureBox2.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox2, "Admin Rights Needed")
        '
        'PictureBox1
        '
        Me.PictureBox1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(492, 93)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(83, 21)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 32
        Me.PictureBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox1, "Admin Rights Needed")
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DeleteProfileToolStripMenuItem, Me.FlushToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(137, 56)
        '
        'DeleteProfileToolStripMenuItem
        '
        Me.DeleteProfileToolStripMenuItem.Image = CType(resources.GetObject("DeleteProfileToolStripMenuItem.Image"), System.Drawing.Image)
        Me.DeleteProfileToolStripMenuItem.Name = "DeleteProfileToolStripMenuItem"
        Me.DeleteProfileToolStripMenuItem.Size = New System.Drawing.Size(136, 26)
        Me.DeleteProfileToolStripMenuItem.Text = "Remove"
        '
        'FlushToolStripMenuItem
        '
        Me.FlushToolStripMenuItem.Image = CType(resources.GetObject("FlushToolStripMenuItem.Image"), System.Drawing.Image)
        Me.FlushToolStripMenuItem.Name = "FlushToolStripMenuItem"
        Me.FlushToolStripMenuItem.Size = New System.Drawing.Size(136, 26)
        Me.FlushToolStripMenuItem.Text = "Flush"
        '
        'DotNetBarTabcontrol1
        '
        Me.DotNetBarTabcontrol1.Alignment = System.Windows.Forms.TabAlignment.Left
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage3)
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage1)
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage2)
        Me.DotNetBarTabcontrol1.Dock = System.Windows.Forms.DockStyle.Top
        Me.DotNetBarTabcontrol1.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        Me.DotNetBarTabcontrol1.ItemSize = New System.Drawing.Size(44, 136)
        Me.DotNetBarTabcontrol1.Location = New System.Drawing.Point(0, 0)
        Me.DotNetBarTabcontrol1.Multiline = True
        Me.DotNetBarTabcontrol1.Name = "DotNetBarTabcontrol1"
        Me.DotNetBarTabcontrol1.SelectedIndex = 0
        Me.DotNetBarTabcontrol1.Size = New System.Drawing.Size(738, 478)
        Me.DotNetBarTabcontrol1.SizeMode = System.Windows.Forms.TabSizeMode.Fixed
        Me.DotNetBarTabcontrol1.TabIndex = 0
        '
        'TabPage3
        '
        Me.TabPage3.BackColor = System.Drawing.Color.White
        Me.TabPage3.Controls.Add(Me.Label31)
        Me.TabPage3.Controls.Add(Me.Label28)
        Me.TabPage3.Controls.Add(Me.Label29)
        Me.TabPage3.Controls.Add(Me.Divider8)
        Me.TabPage3.Controls.Add(Me.Label30)
        Me.TabPage3.Controls.Add(Me.notesclient)
        Me.TabPage3.Controls.Add(Me.Label20)
        Me.TabPage3.Controls.Add(Me.Label13)
        Me.TabPage3.Controls.Add(Me.Divider4)
        Me.TabPage3.Controls.Add(Me.Label12)
        Me.TabPage3.Controls.Add(Me.Label11)
        Me.TabPage3.Controls.Add(Me.Label10)
        Me.TabPage3.Controls.Add(Me.Divider3)
        Me.TabPage3.Controls.Add(Me.Label9)
        Me.TabPage3.Controls.Add(Me.version)
        Me.TabPage3.Controls.Add(Me.botidtxt)
        Me.TabPage3.Location = New System.Drawing.Point(140, 4)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Size = New System.Drawing.Size(594, 470)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Basic Settings"
        '
        'Label31
        '
        Me.Label31.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label31.AutoSize = True
        Me.Label31.Location = New System.Drawing.Point(40, 263)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(67, 19)
        Me.Label31.TabIndex = 48
        Me.Label31.Text = "on logon."
        '
        'Label28
        '
        Me.Label28.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label28.AutoSize = True
        Me.Label28.Location = New System.Drawing.Point(40, 296)
        Me.Label28.Name = "Label28"
        Me.Label28.Size = New System.Drawing.Size(52, 19)
        Me.Label28.TabIndex = 47
        Me.Label28.Text = "Notes :"
        '
        'Label29
        '
        Me.Label29.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label29.AutoSize = True
        Me.Label29.Location = New System.Drawing.Point(40, 244)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(470, 19)
        Me.Label29.TabIndex = 46
        Me.Label29.Text = "You can add some notes to the client, they will be available on the client list"
        '
        'Divider8
        '
        Me.Divider8.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider8.Location = New System.Drawing.Point(113, 229)
        Me.Divider8.Name = "Divider8"
        Me.Divider8.Size = New System.Drawing.Size(473, 23)
        Me.Divider8.TabIndex = 45
        Me.Divider8.Text = "Divider8"
        '
        'Label30
        '
        Me.Label30.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label30.AutoSize = True
        Me.Label30.Location = New System.Drawing.Point(13, 217)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(84, 19)
        Me.Label30.TabIndex = 44
        Me.Label30.Text = "Client Notes"
        '
        'notesclient
        '
        Me.notesclient.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.notesclient.Location = New System.Drawing.Point(335, 296)
        Me.notesclient.Multiline = True
        Me.notesclient.Name = "notesclient"
        Me.notesclient.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.notesclient.Size = New System.Drawing.Size(251, 108)
        Me.notesclient.TabIndex = 43
        '
        'Label20
        '
        Me.Label20.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label20.AutoSize = True
        Me.Label20.Location = New System.Drawing.Point(40, 171)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(62, 19)
        Me.Label20.TabIndex = 42
        Me.Label20.Text = "Version :"
        '
        'Label13
        '
        Me.Label13.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label13.AutoSize = True
        Me.Label13.Location = New System.Drawing.Point(40, 134)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(257, 19)
        Me.Label13.TabIndex = 41
        Me.Label13.Text = "You can change the version of the client."
        '
        'Divider4
        '
        Me.Divider4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider4.Location = New System.Drawing.Point(113, 119)
        Me.Divider4.Name = "Divider4"
        Me.Divider4.Size = New System.Drawing.Size(473, 23)
        Me.Divider4.TabIndex = 40
        Me.Divider4.Text = "Divider4"
        '
        'Label12
        '
        Me.Label12.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label12.AutoSize = True
        Me.Label12.Location = New System.Drawing.Point(13, 107)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(94, 19)
        Me.Label12.TabIndex = 39
        Me.Label12.Text = "Client Version"
        '
        'Label11
        '
        Me.Label11.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label11.AutoSize = True
        Me.Label11.Location = New System.Drawing.Point(40, 55)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(38, 19)
        Me.Label11.TabIndex = 38
        Me.Label11.Text = "Tag :"
        '
        'Label10
        '
        Me.Label10.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(40, 31)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(204, 19)
        Me.Label10.TabIndex = 37
        Me.Label10.Text = "Enter a id to identify your client."
        '
        'Divider3
        '
        Me.Divider3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider3.ForeColor = System.Drawing.SystemColors.ActiveBorder
        Me.Divider3.Location = New System.Drawing.Point(89, 17)
        Me.Divider3.Name = "Divider3"
        Me.Divider3.Size = New System.Drawing.Size(497, 23)
        Me.Divider3.TabIndex = 36
        Me.Divider3.Text = "Divider3"
        '
        'Label9
        '
        Me.Label9.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label9.AutoSize = True
        Me.Label9.Location = New System.Drawing.Point(13, 5)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(70, 19)
        Me.Label9.TabIndex = 35
        Me.Label9.Text = "Client Tag"
        '
        'version
        '
        Me.version.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.version.Location = New System.Drawing.Point(335, 168)
        Me.version.Name = "version"
        Me.version.Size = New System.Drawing.Size(251, 26)
        Me.version.TabIndex = 16
        Me.version.Text = "0.0.1"
        '
        'botidtxt
        '
        Me.botidtxt.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.botidtxt.Location = New System.Drawing.Point(335, 52)
        Me.botidtxt.Name = "botidtxt"
        Me.botidtxt.Size = New System.Drawing.Size(251, 26)
        Me.botidtxt.TabIndex = 15
        Me.botidtxt.Text = "RT-101-Bot"
        '
        'TabPage1
        '
        Me.TabPage1.BackColor = System.Drawing.Color.White
        Me.TabPage1.Controls.Add(Me.Label32)
        Me.TabPage1.Controls.Add(Me.TextBox6)
        Me.TabPage1.Controls.Add(Me.CheckBox1)
        Me.TabPage1.Controls.Add(Me.Label24)
        Me.TabPage1.Controls.Add(Me.Label25)
        Me.TabPage1.Controls.Add(Me.Divider6)
        Me.TabPage1.Controls.Add(Me.Label23)
        Me.TabPage1.Controls.Add(Me.Label22)
        Me.TabPage1.Controls.Add(Me.passbuild)
        Me.TabPage1.Controls.Add(Me.Label21)
        Me.TabPage1.Controls.Add(Me.ListBox1)
        Me.TabPage1.Controls.Add(Me.Button2)
        Me.TabPage1.Controls.Add(Me.Label19)
        Me.TabPage1.Controls.Add(Me.Label18)
        Me.TabPage1.Controls.Add(Me.Label17)
        Me.TabPage1.Controls.Add(Me.Label16)
        Me.TabPage1.Controls.Add(Me.Label15)
        Me.TabPage1.Controls.Add(Me.Divider5)
        Me.TabPage1.Controls.Add(Me.Label14)
        Me.TabPage1.Controls.Add(Me.TextBox2)
        Me.TabPage1.Controls.Add(Me.TextBox1)
        Me.TabPage1.Location = New System.Drawing.Point(140, 4)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(594, 470)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Connection Settings"
        '
        'Label32
        '
        Me.Label32.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label32.AutoSize = True
        Me.Label32.Location = New System.Drawing.Point(47, 113)
        Me.Label32.Name = "Label32"
        Me.Label32.Size = New System.Drawing.Size(152, 19)
        Me.Label32.TabIndex = 57
        Me.Label32.Text = "IP / Hostname Backup :"
        '
        'TextBox6
        '
        Me.TextBox6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox6.Location = New System.Drawing.Point(321, 110)
        Me.TextBox6.Name = "TextBox6"
        Me.TextBox6.Size = New System.Drawing.Size(251, 26)
        Me.TextBox6.TabIndex = 56
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.Location = New System.Drawing.Point(321, 427)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(126, 23)
        Me.CheckBox1.TabIndex = 55
        Me.CheckBox1.Text = "Show Password"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'Label24
        '
        Me.Label24.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label24.AutoSize = True
        Me.Label24.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label24.Location = New System.Drawing.Point(110, 351)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(473, 19)
        Me.Label24.TabIndex = 54
        Me.Label24.Text = "Remember that the passwords between client and server must be the same."
        '
        'Label25
        '
        Me.Label25.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label25.AutoSize = True
        Me.Label25.Location = New System.Drawing.Point(47, 332)
        Me.Label25.Name = "Label25"
        Me.Label25.Size = New System.Drawing.Size(236, 19)
        Me.Label25.TabIndex = 53
        Me.Label25.Text = "Insert a password for the connection."
        '
        'Divider6
        '
        Me.Divider6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider6.Location = New System.Drawing.Point(76, 311)
        Me.Divider6.Name = "Divider6"
        Me.Divider6.Size = New System.Drawing.Size(493, 23)
        Me.Divider6.TabIndex = 52
        Me.Divider6.Text = "Divider6"
        '
        'Label23
        '
        Me.Label23.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label23.AutoSize = True
        Me.Label23.Location = New System.Drawing.Point(13, 299)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(57, 19)
        Me.Label23.TabIndex = 51
        Me.Label23.Text = "Security"
        '
        'Label22
        '
        Me.Label22.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label22.AutoSize = True
        Me.Label22.Location = New System.Drawing.Point(46, 395)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(74, 19)
        Me.Label22.TabIndex = 50
        Me.Label22.Text = "Password :"
        '
        'passbuild
        '
        Me.passbuild.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.passbuild.Location = New System.Drawing.Point(321, 395)
        Me.passbuild.Name = "passbuild"
        Me.passbuild.Size = New System.Drawing.Size(248, 26)
        Me.passbuild.TabIndex = 49
        Me.passbuild.Text = "password"
        Me.passbuild.UseSystemPasswordChar = True
        '
        'Label21
        '
        Me.Label21.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label21.AutoSize = True
        Me.Label21.Location = New System.Drawing.Point(47, 246)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(216, 19)
        Me.Label21.TabIndex = 48
        Me.Label21.Text = "Select the profile you want to use."
        '
        'ListBox1
        '
        Me.ListBox1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ListBox1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.ListBox1.FormattingEnabled = True
        Me.ListBox1.ItemHeight = 19
        Me.ListBox1.Location = New System.Drawing.Point(321, 216)
        Me.ListBox1.Name = "ListBox1"
        Me.ListBox1.Size = New System.Drawing.Size(251, 80)
        Me.ListBox1.TabIndex = 47
        '
        'Button2
        '
        Me.Button2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button2.Location = New System.Drawing.Point(321, 182)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(251, 28)
        Me.Button2.TabIndex = 46
        Me.Button2.Text = "Add Host"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Label19
        '
        Me.Label19.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label19.AutoSize = True
        Me.Label19.ForeColor = System.Drawing.Color.FromArgb(CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer), CType(CType(64, Byte), Integer))
        Me.Label19.Location = New System.Drawing.Point(47, 216)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(100, 19)
        Me.Label19.TabIndex = 44
        Me.Label19.Text = "Saved Profiles :"
        '
        'Label18
        '
        Me.Label18.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label18.AutoSize = True
        Me.Label18.ForeColor = System.Drawing.Color.SteelBlue
        Me.Label18.Location = New System.Drawing.Point(47, 52)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(212, 19)
        Me.Label18.TabIndex = 42
        Me.Label18.Text = "Remember to check out the port."
        '
        'Label17
        '
        Me.Label17.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label17.AutoSize = True
        Me.Label17.Location = New System.Drawing.Point(47, 33)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(270, 19)
        Me.Label17.TabIndex = 41
        Me.Label17.Text = "Insert the data for the reverse connection ."
        '
        'Label16
        '
        Me.Label16.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label16.AutoSize = True
        Me.Label16.Location = New System.Drawing.Point(47, 145)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(42, 19)
        Me.Label16.TabIndex = 40
        Me.Label16.Text = "Port :"
        '
        'Label15
        '
        Me.Label15.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label15.AutoSize = True
        Me.Label15.Location = New System.Drawing.Point(47, 81)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(155, 19)
        Me.Label15.TabIndex = 39
        Me.Label15.Text = "IP / Hostname Primary :"
        '
        'Divider5
        '
        Me.Divider5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider5.Location = New System.Drawing.Point(131, 17)
        Me.Divider5.Name = "Divider5"
        Me.Divider5.Size = New System.Drawing.Size(441, 23)
        Me.Divider5.TabIndex = 38
        Me.Divider5.Text = "Divider5"
        '
        'Label14
        '
        Me.Label14.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label14.AutoSize = True
        Me.Label14.Location = New System.Drawing.Point(13, 5)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(112, 19)
        Me.Label14.TabIndex = 37
        Me.Label14.Text = "Connection Host"
        '
        'TextBox2
        '
        Me.TextBox2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox2.Location = New System.Drawing.Point(321, 142)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.Size = New System.Drawing.Size(251, 26)
        Me.TextBox2.TabIndex = 12
        Me.TextBox2.Text = "4783"
        '
        'TextBox1
        '
        Me.TextBox1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox1.Location = New System.Drawing.Point(321, 78)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(251, 26)
        Me.TextBox1.TabIndex = 11
        Me.TextBox1.Text = "127.0.0.1"
        '
        'TabPage2
        '
        Me.TabPage2.BackColor = System.Drawing.Color.White
        Me.TabPage2.Controls.Add(Me.startup)
        Me.TabPage2.Controls.Add(Me.Divider2)
        Me.TabPage2.Controls.Add(Me.Divider1)
        Me.TabPage2.Controls.Add(Me.Label8)
        Me.TabPage2.Controls.Add(Me.Label7)
        Me.TabPage2.Controls.Add(Me.Label6)
        Me.TabPage2.Controls.Add(Me.CheckBox2)
        Me.TabPage2.Controls.Add(Me.Label5)
        Me.TabPage2.Controls.Add(Me.Label4)
        Me.TabPage2.Controls.Add(Me.Label3)
        Me.TabPage2.Controls.Add(Me.Label2)
        Me.TabPage2.Controls.Add(Me.Label1)
        Me.TabPage2.Controls.Add(Me.instshit)
        Me.TabPage2.Controls.Add(Me.sysfold)
        Me.TabPage2.Controls.Add(Me.prgfold)
        Me.TabPage2.Controls.Add(Me.usrfold)
        Me.TabPage2.Controls.Add(Me.installfnametxt)
        Me.TabPage2.Controls.Add(Me.installsubdir)
        Me.TabPage2.Controls.Add(Me.InstallLocationPre)
        Me.TabPage2.Controls.Add(Me.TextBox5)
        Me.TabPage2.Controls.Add(Me.PictureBox2)
        Me.TabPage2.Controls.Add(Me.PictureBox1)
        Me.TabPage2.Location = New System.Drawing.Point(140, 4)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(594, 470)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Installation Settings"
        '
        'startup
        '
        Me.startup.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.startup.AutoSize = True
        Me.startup.Enabled = False
        Me.startup.Location = New System.Drawing.Point(308, 375)
        Me.startup.Name = "startup"
        Me.startup.Size = New System.Drawing.Size(166, 23)
        Me.startup.TabIndex = 36
        Me.startup.Text = "Add To Startup Folder"
        Me.startup.UseVisualStyleBackColor = True
        '
        'Divider2
        '
        Me.Divider2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider2.Location = New System.Drawing.Point(87, 346)
        Me.Divider2.Name = "Divider2"
        Me.Divider2.Size = New System.Drawing.Size(486, 23)
        Me.Divider2.TabIndex = 35
        Me.Divider2.Text = "Divider2"
        '
        'Divider1
        '
        Me.Divider1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Divider1.Location = New System.Drawing.Point(120, 15)
        Me.Divider1.Name = "Divider1"
        Me.Divider1.Size = New System.Drawing.Size(453, 23)
        Me.Divider1.TabIndex = 34
        Me.Divider1.Text = "Divider1"
        '
        'Label8
        '
        Me.Label8.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label8.AutoSize = True
        Me.Label8.Location = New System.Drawing.Point(13, 335)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(68, 19)
        Me.Label8.TabIndex = 31
        Me.Label8.Text = "AutoStart"
        '
        'Label7
        '
        Me.Label7.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(13, 5)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(101, 19)
        Me.Label7.TabIndex = 29
        Me.Label7.Text = "Install Location"
        '
        'Label6
        '
        Me.Label6.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(23, 414)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(129, 19)
        Me.Label6.TabIndex = 27
        Me.Label6.Text = "StartUp Key Name :"
        '
        'CheckBox2
        '
        Me.CheckBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CheckBox2.AutoSize = True
        Me.CheckBox2.Enabled = False
        Me.CheckBox2.Location = New System.Drawing.Point(22, 377)
        Me.CheckBox2.Name = "CheckBox2"
        Me.CheckBox2.Size = New System.Drawing.Size(106, 23)
        Me.CheckBox2.TabIndex = 26
        Me.CheckBox2.Text = "Registry Key"
        Me.CheckBox2.UseVisualStyleBackColor = True
        '
        'Label5
        '
        Me.Label5.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(23, 264)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(135, 19)
        Me.Label5.TabIndex = 25
        Me.Label5.Text = "Install Path Preview :"
        '
        'Label4
        '
        Me.Label4.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(537, 205)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(36, 19)
        Me.Label4.TabIndex = 24
        Me.Label4.Text = ".exe "
        '
        'Label3
        '
        Me.Label3.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(21, 205)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(116, 19)
        Me.Label3.TabIndex = 23
        Me.Label3.Text = "Install FileName : "
        '
        'Label2
        '
        Me.Label2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(21, 68)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(116, 19)
        Me.Label2.TabIndex = 22
        Me.Label2.Text = "Install Directory : "
        '
        'Label1
        '
        Me.Label1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(21, 173)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(139, 19)
        Me.Label1.TabIndex = 21
        Me.Label1.Text = "Install SubDirectory : "
        '
        'instshit
        '
        Me.instshit.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.instshit.AutoSize = True
        Me.instshit.Location = New System.Drawing.Point(22, 36)
        Me.instshit.Name = "instshit"
        Me.instshit.Size = New System.Drawing.Size(106, 23)
        Me.instshit.TabIndex = 20
        Me.instshit.Text = "Install Client"
        Me.instshit.UseVisualStyleBackColor = True
        '
        'sysfold
        '
        Me.sysfold.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.sysfold.AutoSize = True
        Me.sysfold.Enabled = False
        Me.sysfold.Location = New System.Drawing.Point(308, 122)
        Me.sysfold.Name = "sysfold"
        Me.sysfold.Size = New System.Drawing.Size(74, 23)
        Me.sysfold.TabIndex = 19
        Me.sysfold.Text = "System"
        Me.sysfold.UseVisualStyleBackColor = True
        '
        'prgfold
        '
        Me.prgfold.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.prgfold.AutoSize = True
        Me.prgfold.Enabled = False
        Me.prgfold.Location = New System.Drawing.Point(308, 93)
        Me.prgfold.Name = "prgfold"
        Me.prgfold.Size = New System.Drawing.Size(113, 23)
        Me.prgfold.TabIndex = 18
        Me.prgfold.Text = "Program Files"
        Me.prgfold.UseVisualStyleBackColor = True
        '
        'usrfold
        '
        Me.usrfold.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.usrfold.AutoSize = True
        Me.usrfold.Checked = True
        Me.usrfold.Enabled = False
        Me.usrfold.Location = New System.Drawing.Point(308, 64)
        Me.usrfold.Name = "usrfold"
        Me.usrfold.Size = New System.Drawing.Size(163, 23)
        Me.usrfold.TabIndex = 17
        Me.usrfold.TabStop = True
        Me.usrfold.Text = "User Application Data"
        Me.usrfold.UseVisualStyleBackColor = True
        '
        'installfnametxt
        '
        Me.installfnametxt.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.installfnametxt.Enabled = False
        Me.installfnametxt.Location = New System.Drawing.Point(308, 202)
        Me.installfnametxt.Name = "installfnametxt"
        Me.installfnametxt.Size = New System.Drawing.Size(223, 26)
        Me.installfnametxt.TabIndex = 16
        '
        'installsubdir
        '
        Me.installsubdir.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.installsubdir.Enabled = False
        Me.installsubdir.Location = New System.Drawing.Point(308, 170)
        Me.installsubdir.Name = "installsubdir"
        Me.installsubdir.Size = New System.Drawing.Size(265, 26)
        Me.installsubdir.TabIndex = 15
        '
        'InstallLocationPre
        '
        Me.InstallLocationPre.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.InstallLocationPre.ForeColor = System.Drawing.Color.Black
        Me.InstallLocationPre.Location = New System.Drawing.Point(27, 286)
        Me.InstallLocationPre.Name = "InstallLocationPre"
        Me.InstallLocationPre.ReadOnly = True
        Me.InstallLocationPre.Size = New System.Drawing.Size(546, 26)
        Me.InstallLocationPre.TabIndex = 14
        '
        'TextBox5
        '
        Me.TextBox5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox5.Enabled = False
        Me.TextBox5.Location = New System.Drawing.Point(308, 411)
        Me.TextBox5.Name = "TextBox5"
        Me.TextBox5.Size = New System.Drawing.Size(265, 26)
        Me.TextBox5.TabIndex = 13
        '
        'Builder
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(738, 537)
        Me.Controls.Add(Me.DotNetBarTabcontrol1)
        Me.Controls.Add(Me.Button1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Builder"
        Me.ShowIcon = False
        Me.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Builder"
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.DotNetBarTabcontrol1.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage3.PerformLayout()
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents DotNetBarTabcontrol1 As DotNetBarTabcontrol
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents TextBox2 As TextBox
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents Button1 As Button
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents version As TextBox
    Friend WithEvents botidtxt As TextBox
    Friend WithEvents sysfold As RadioButton
    Friend WithEvents prgfold As RadioButton
    Friend WithEvents usrfold As RadioButton
    Friend WithEvents installfnametxt As TextBox
    Friend WithEvents installsubdir As TextBox
    Friend WithEvents InstallLocationPre As TextBox
    Friend WithEvents TextBox5 As TextBox
    Friend WithEvents PictureBox2 As PictureBox
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents Label8 As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents Label6 As Label
    Friend WithEvents CheckBox2 As CheckBox
    Friend WithEvents Label5 As Label
    Friend WithEvents Label4 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Label1 As Label
    Friend WithEvents instshit As CheckBox
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents Label13 As Label
    Friend WithEvents Divider4 As Divider
    Friend WithEvents Label12 As Label
    Friend WithEvents Label11 As Label
    Friend WithEvents Label10 As Label
    Friend WithEvents Divider3 As Divider
    Friend WithEvents Label9 As Label
    Friend WithEvents Label19 As Label
    Friend WithEvents Label18 As Label
    Friend WithEvents Label17 As Label
    Friend WithEvents Label16 As Label
    Friend WithEvents Label15 As Label
    Friend WithEvents Divider5 As Divider
    Friend WithEvents Label14 As Label
    Friend WithEvents Divider2 As Divider
    Friend WithEvents Divider1 As Divider
    Friend WithEvents startup As CheckBox
    Friend WithEvents Label20 As Label
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents FlushToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents Button2 As Button
    Friend WithEvents ListBox1 As ListBox
    Friend WithEvents DeleteProfileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents Label21 As Label
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents Label24 As Label
    Friend WithEvents Label25 As Label
    Friend WithEvents Divider6 As Divider
    Friend WithEvents Label23 As Label
    Friend WithEvents Label22 As Label
    Friend WithEvents passbuild As TextBox
    Friend WithEvents Label28 As Label
    Friend WithEvents Label29 As Label
    Friend WithEvents Divider8 As Divider
    Friend WithEvents Label30 As Label
    Friend WithEvents notesclient As TextBox
    Friend WithEvents Label31 As Label
    Friend WithEvents Label32 As Label
    Friend WithEvents TextBox6 As TextBox
End Class
