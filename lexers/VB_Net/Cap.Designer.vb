<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Cap
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Cap))
        Me.P1 = New System.Windows.Forms.PictureBox()
        Me.C1 = New System.Windows.Forms.ComboBox()
        Me.C2 = New System.Windows.Forms.ComboBox()
        Me.C = New System.Windows.Forms.NumericUpDown()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.setpanel = New System.Windows.Forms.Panel()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.paneltop = New System.Windows.Forms.Panel()
        Me.Button6 = New System.Windows.Forms.Button()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button4 = New System.Windows.Forms.Button()
        CType(Me.P1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.C, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.setpanel.SuspendLayout()
        Me.paneltop.SuspendLayout()
        Me.SuspendLayout()
        '
        'P1
        '
        Me.P1.BackColor = System.Drawing.Color.Black
        Me.P1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.P1.Location = New System.Drawing.Point(0, 0)
        Me.P1.Margin = New System.Windows.Forms.Padding(4)
        Me.P1.Name = "P1"
        Me.P1.Size = New System.Drawing.Size(940, 653)
        Me.P1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.P1.TabIndex = 0
        Me.P1.TabStop = False
        '
        'C1
        '
        Me.C1.FormattingEnabled = True
        Me.C1.Location = New System.Drawing.Point(154, 12)
        Me.C1.Margin = New System.Windows.Forms.Padding(4)
        Me.C1.Name = "C1"
        Me.C1.Size = New System.Drawing.Size(133, 27)
        Me.C1.TabIndex = 2
        Me.ToolTip1.SetToolTip(Me.C1, "Image Resolution")
        '
        'C2
        '
        Me.C2.FormattingEnabled = True
        Me.C2.Location = New System.Drawing.Point(154, 47)
        Me.C2.Margin = New System.Windows.Forms.Padding(4)
        Me.C2.Name = "C2"
        Me.C2.Size = New System.Drawing.Size(133, 27)
        Me.C2.TabIndex = 4
        Me.ToolTip1.SetToolTip(Me.C2, "Image Split Parts")
        '
        'C
        '
        Me.C.Location = New System.Drawing.Point(154, 83)
        Me.C.Margin = New System.Windows.Forms.Padding(4)
        Me.C.Name = "C"
        Me.C.Size = New System.Drawing.Size(133, 26)
        Me.C.TabIndex = 6
        Me.ToolTip1.SetToolTip(Me.C, "Quality %")
        Me.C.Value = New Decimal(New Integer() {40, 0, 0, 0})
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.BackColor = System.Drawing.Color.Transparent
        Me.CheckBox1.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        Me.CheckBox1.ForeColor = System.Drawing.Color.Black
        Me.CheckBox1.Location = New System.Drawing.Point(154, 117)
        Me.CheckBox1.Margin = New System.Windows.Forms.Padding(4)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(93, 23)
        Me.CheckBox1.TabIndex = 7
        Me.CheckBox1.Text = "Show Line"
        Me.ToolTip1.SetToolTip(Me.CheckBox1, "Show Active Areas")
        Me.CheckBox1.UseVisualStyleBackColor = False
        '
        'Button1
        '
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.Location = New System.Drawing.Point(17, 150)
        Me.Button1.Margin = New System.Windows.Forms.Padding(4)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(270, 28)
        Me.Button1.TabIndex = 8
        Me.Button1.Text = "Start"
        Me.ToolTip1.SetToolTip(Me.Button1, "Start / Stop Stream")
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Timer1
        '
        Me.Timer1.Enabled = True
        Me.Timer1.Interval = 500
        '
        'setpanel
        '
        Me.setpanel.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.setpanel.Controls.Add(Me.Label3)
        Me.setpanel.Controls.Add(Me.Label2)
        Me.setpanel.Controls.Add(Me.Label1)
        Me.setpanel.Controls.Add(Me.CheckBox1)
        Me.setpanel.Controls.Add(Me.C)
        Me.setpanel.Controls.Add(Me.C1)
        Me.setpanel.Controls.Add(Me.C2)
        Me.setpanel.Controls.Add(Me.Button1)
        Me.setpanel.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        Me.setpanel.Location = New System.Drawing.Point(544, 63)
        Me.setpanel.Name = "setpanel"
        Me.setpanel.Size = New System.Drawing.Size(306, 192)
        Me.setpanel.TabIndex = 10
        Me.setpanel.Visible = False
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(13, 85)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(60, 19)
        Me.Label3.TabIndex = 14
        Me.Label3.Text = "Quality :"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(13, 50)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(101, 19)
        Me.Label2.TabIndex = 13
        Me.Label2.Text = "Image Tokens :"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(13, 15)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(122, 19)
        Me.Label1.TabIndex = 12
        Me.Label1.Text = "Image Resolution :"
        '
        'Button3
        '
        Me.Button3.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.Button3.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button3.Font = New System.Drawing.Font("Segoe UI", 7.25!)
        Me.Button3.Location = New System.Drawing.Point(436, 59)
        Me.Button3.Margin = New System.Windows.Forms.Padding(4)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(84, 25)
        Me.Button3.TabIndex = 10
        Me.Button3.Text = "Hide"
        Me.ToolTip1.SetToolTip(Me.Button3, "Hide Panel")
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.Button2.BackColor = System.Drawing.Color.Transparent
        Me.Button2.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button2.Font = New System.Drawing.Font("Segoe UI", 7.25!)
        Me.Button2.Location = New System.Drawing.Point(436, 5)
        Me.Button2.Margin = New System.Windows.Forms.Padding(4)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(84, 25)
        Me.Button2.TabIndex = 11
        Me.Button2.Text = "Show"
        Me.ToolTip1.SetToolTip(Me.Button2, "Show Panel")
        Me.Button2.UseVisualStyleBackColor = False
        Me.Button2.Visible = False
        '
        'paneltop
        '
        Me.paneltop.Anchor = System.Windows.Forms.AnchorStyles.Top
        Me.paneltop.Controls.Add(Me.Button6)
        Me.paneltop.Controls.Add(Me.Button5)
        Me.paneltop.Controls.Add(Me.Button4)
        Me.paneltop.Location = New System.Drawing.Point(275, 0)
        Me.paneltop.Name = "paneltop"
        Me.paneltop.Size = New System.Drawing.Size(404, 57)
        Me.paneltop.TabIndex = 12
        '
        'Button6
        '
        Me.Button6.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button6.ForeColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Button6.Image = CType(resources.GetObject("Button6.Image"), System.Drawing.Image)
        Me.Button6.Location = New System.Drawing.Point(269, 6)
        Me.Button6.Name = "Button6"
        Me.Button6.Size = New System.Drawing.Size(122, 43)
        Me.Button6.TabIndex = 2
        Me.Button6.Text = "Settings"
        Me.Button6.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.Button6.UseVisualStyleBackColor = True
        '
        'Button5
        '
        Me.Button5.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button5.ForeColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Button5.Image = CType(resources.GetObject("Button5.Image"), System.Drawing.Image)
        Me.Button5.Location = New System.Drawing.Point(141, 6)
        Me.Button5.Name = "Button5"
        Me.Button5.Size = New System.Drawing.Size(122, 43)
        Me.Button5.TabIndex = 1
        Me.Button5.Text = "Control"
        Me.Button5.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Button4
        '
        Me.Button4.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button4.ForeColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Button4.Image = CType(resources.GetObject("Button4.Image"), System.Drawing.Image)
        Me.Button4.Location = New System.Drawing.Point(13, 6)
        Me.Button4.Name = "Button4"
        Me.Button4.Size = New System.Drawing.Size(122, 43)
        Me.Button4.TabIndex = 0
        Me.Button4.Text = "Snapshot"
        Me.Button4.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText
        Me.Button4.UseVisualStyleBackColor = True
        '
        'Cap
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(940, 653)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.paneltop)
        Me.Controls.Add(Me.setpanel)
        Me.Controls.Add(Me.P1)
        Me.Margin = New System.Windows.Forms.Padding(4)
        Me.Name = "Cap"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Remote Desktop"
        CType(Me.P1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.C, System.ComponentModel.ISupportInitialize).EndInit()
        Me.setpanel.ResumeLayout(False)
        Me.setpanel.PerformLayout()
        Me.paneltop.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents P1 As System.Windows.Forms.PictureBox
    Friend WithEvents C1 As System.Windows.Forms.ComboBox
    Friend WithEvents C2 As System.Windows.Forms.ComboBox
    Friend WithEvents C As System.Windows.Forms.NumericUpDown
    Friend WithEvents CheckBox1 As System.Windows.Forms.CheckBox
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents setpanel As Panel
    Friend WithEvents Button3 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents Label3 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Label1 As Label
    Friend WithEvents paneltop As Panel
    Friend WithEvents Button6 As Button
    Friend WithEvents Button5 As Button
    Friend WithEvents Button4 As Button
End Class
