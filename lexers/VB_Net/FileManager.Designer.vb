<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FileManager
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FileManager))
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.Button1 = New System.Windows.Forms.Button()
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.BackToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MoveToToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DesktopToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.StartupToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DocumentsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TempToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.DownloadToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.UploadToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.ExecuteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RenameToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DeleteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.AddToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RefreshToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImageList2 = New System.Windows.Forms.ImageList(Me.components)
        Me.ContextMenuStrip2 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CancelToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.BrushToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DotNetBarTabcontrol1 = New RT101V001.DotNetBarTabcontrol()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.ListView1 = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader3 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ComboBox1 = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.ListView2 = New System.Windows.Forms.ListView()
        Me.ColumnHeader4 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader5 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader6 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.ColumnHeader7 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.StatusStrip1.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.ContextMenuStrip2.SuspendLayout()
        Me.DotNetBarTabcontrol1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'StatusStrip1
        '
        Me.StatusStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 573)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(1072, 25)
        Me.StatusStrip1.TabIndex = 1
        Me.StatusStrip1.Text = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        Me.ToolStripStatusLabel1.Size = New System.Drawing.Size(171, 20)
        Me.ToolStripStatusLabel1.Text = "Status: Loading drivers ..."
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.Cursor = System.Windows.Forms.Cursors.Hand
        Me.Button1.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.Button1.Image = CType(resources.GetObject("Button1.Image"), System.Drawing.Image)
        Me.Button1.Location = New System.Drawing.Point(866, 12)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(32, 27)
        Me.Button1.TabIndex = 42
        Me.ToolTip1.SetToolTip(Me.Button1, "Refresh")
        Me.Button1.UseVisualStyleBackColor = True
        '
        'ImageList1
        '
        Me.ImageList1.ImageStream = CType(resources.GetObject("ImageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList1.Images.SetKeyName(0, "if_folder_5362.png")
        Me.ImageList1.Images.SetKeyName(1, "if_application_4970.png")
        Me.ImageList1.Images.SetKeyName(2, "photos.png")
        Me.ImageList1.Images.SetKeyName(3, "free-vector-text-file-icon_101919_Text_File_Icon.png")
        Me.ImageList1.Images.SetKeyName(4, "dll_file_37106.jpg")
        Me.ImageList1.Images.SetKeyName(5, "winrar_icon_by_stumpy666davies-d5pve48.png")
        Me.ImageList1.Images.SetKeyName(6, "19.png")
        Me.ImageList1.Images.SetKeyName(7, "descargar-Mp3.jpg")
        Me.ImageList1.Images.SetKeyName(8, "870441_windows_512x512.png")
        Me.ImageList1.Images.SetKeyName(9, "2000px-MS_word_DOC_icon.svg.png")
        Me.ImageList1.Images.SetKeyName(10, "ini-2.png")
        Me.ImageList1.Images.SetKeyName(11, "pdf_icon_svg_by_qubodup-d9n1mhy.png")
        Me.ImageList1.Images.SetKeyName(12, "iso-4.png")
        Me.ImageList1.Images.SetKeyName(13, "kisspng-vbscript-scripting-language-visual-basic-microsoft-analyst-5acc162b6da676" &
        ".9666584115233244594491.jpg")
        Me.ImageList1.Images.SetKeyName(14, "vcredist140.png")
        Me.ImageList1.Images.SetKeyName(15, "if_file_370083.png")
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BackToolStripMenuItem, Me.MoveToToolStripMenuItem, Me.ToolStripSeparator1, Me.DownloadToolStripMenuItem, Me.UploadToolStripMenuItem, Me.ToolStripSeparator3, Me.ExecuteToolStripMenuItem, Me.RenameToolStripMenuItem, Me.DeleteToolStripMenuItem, Me.ToolStripSeparator4, Me.AddToolStripMenuItem, Me.RefreshToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(181, 256)
        '
        'BackToolStripMenuItem
        '
        Me.BackToolStripMenuItem.Image = CType(resources.GetObject("BackToolStripMenuItem.Image"), System.Drawing.Image)
        Me.BackToolStripMenuItem.Name = "BackToolStripMenuItem"
        Me.BackToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.BackToolStripMenuItem.Text = "&Back"
        '
        'MoveToToolStripMenuItem
        '
        Me.MoveToToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DesktopToolStripMenuItem, Me.StartupToolStripMenuItem, Me.DocumentsToolStripMenuItem, Me.TempToolStripMenuItem})
        Me.MoveToToolStripMenuItem.Image = CType(resources.GetObject("MoveToToolStripMenuItem.Image"), System.Drawing.Image)
        Me.MoveToToolStripMenuItem.Name = "MoveToToolStripMenuItem"
        Me.MoveToToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.MoveToToolStripMenuItem.Text = "Move To"
        '
        'DesktopToolStripMenuItem
        '
        Me.DesktopToolStripMenuItem.Image = CType(resources.GetObject("DesktopToolStripMenuItem.Image"), System.Drawing.Image)
        Me.DesktopToolStripMenuItem.Name = "DesktopToolStripMenuItem"
        Me.DesktopToolStripMenuItem.Size = New System.Drawing.Size(159, 26)
        Me.DesktopToolStripMenuItem.Text = "Desktop"
        '
        'StartupToolStripMenuItem
        '
        Me.StartupToolStripMenuItem.Image = CType(resources.GetObject("StartupToolStripMenuItem.Image"), System.Drawing.Image)
        Me.StartupToolStripMenuItem.Name = "StartupToolStripMenuItem"
        Me.StartupToolStripMenuItem.Size = New System.Drawing.Size(159, 26)
        Me.StartupToolStripMenuItem.Text = "Startup"
        '
        'DocumentsToolStripMenuItem
        '
        Me.DocumentsToolStripMenuItem.Image = CType(resources.GetObject("DocumentsToolStripMenuItem.Image"), System.Drawing.Image)
        Me.DocumentsToolStripMenuItem.Name = "DocumentsToolStripMenuItem"
        Me.DocumentsToolStripMenuItem.Size = New System.Drawing.Size(159, 26)
        Me.DocumentsToolStripMenuItem.Text = "Documents"
        '
        'TempToolStripMenuItem
        '
        Me.TempToolStripMenuItem.Image = CType(resources.GetObject("TempToolStripMenuItem.Image"), System.Drawing.Image)
        Me.TempToolStripMenuItem.Name = "TempToolStripMenuItem"
        Me.TempToolStripMenuItem.Size = New System.Drawing.Size(159, 26)
        Me.TempToolStripMenuItem.Text = "Temp"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(177, 6)
        '
        'DownloadToolStripMenuItem
        '
        Me.DownloadToolStripMenuItem.Font = New System.Drawing.Font("Segoe UI", 9.0!, System.Drawing.FontStyle.Bold)
        Me.DownloadToolStripMenuItem.Image = CType(resources.GetObject("DownloadToolStripMenuItem.Image"), System.Drawing.Image)
        Me.DownloadToolStripMenuItem.Name = "DownloadToolStripMenuItem"
        Me.DownloadToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.DownloadToolStripMenuItem.Text = "Download"
        '
        'UploadToolStripMenuItem
        '
        Me.UploadToolStripMenuItem.Image = CType(resources.GetObject("UploadToolStripMenuItem.Image"), System.Drawing.Image)
        Me.UploadToolStripMenuItem.Name = "UploadToolStripMenuItem"
        Me.UploadToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.UploadToolStripMenuItem.Text = "Upload"
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        Me.ToolStripSeparator3.Size = New System.Drawing.Size(177, 6)
        '
        'ExecuteToolStripMenuItem
        '
        Me.ExecuteToolStripMenuItem.Image = CType(resources.GetObject("ExecuteToolStripMenuItem.Image"), System.Drawing.Image)
        Me.ExecuteToolStripMenuItem.Name = "ExecuteToolStripMenuItem"
        Me.ExecuteToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.ExecuteToolStripMenuItem.Text = "Execute"
        '
        'RenameToolStripMenuItem
        '
        Me.RenameToolStripMenuItem.Image = CType(resources.GetObject("RenameToolStripMenuItem.Image"), System.Drawing.Image)
        Me.RenameToolStripMenuItem.Name = "RenameToolStripMenuItem"
        Me.RenameToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.RenameToolStripMenuItem.Text = "Rename"
        '
        'DeleteToolStripMenuItem
        '
        Me.DeleteToolStripMenuItem.Image = CType(resources.GetObject("DeleteToolStripMenuItem.Image"), System.Drawing.Image)
        Me.DeleteToolStripMenuItem.Name = "DeleteToolStripMenuItem"
        Me.DeleteToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.DeleteToolStripMenuItem.Text = "Delete"
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        Me.ToolStripSeparator4.Size = New System.Drawing.Size(177, 6)
        '
        'AddToolStripMenuItem
        '
        Me.AddToolStripMenuItem.Image = CType(resources.GetObject("AddToolStripMenuItem.Image"), System.Drawing.Image)
        Me.AddToolStripMenuItem.Name = "AddToolStripMenuItem"
        Me.AddToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.AddToolStripMenuItem.Text = "Add to Startup"
        '
        'RefreshToolStripMenuItem
        '
        Me.RefreshToolStripMenuItem.Image = CType(resources.GetObject("RefreshToolStripMenuItem.Image"), System.Drawing.Image)
        Me.RefreshToolStripMenuItem.Name = "RefreshToolStripMenuItem"
        Me.RefreshToolStripMenuItem.Size = New System.Drawing.Size(180, 26)
        Me.RefreshToolStripMenuItem.Text = "Refresh"
        '
        'ImageList2
        '
        Me.ImageList2.ImageStream = CType(resources.GetObject("ImageList2.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList2.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList2.Images.SetKeyName(0, "if_arrow_down_8395.png")
        Me.ImageList2.Images.SetKeyName(1, "if_accept_4940.png")
        Me.ImageList2.Images.SetKeyName(2, "if_exclamation_5332.png")
        Me.ImageList2.Images.SetKeyName(3, "if_arrow_down_83952.png")
        '
        'ContextMenuStrip2
        '
        Me.ContextMenuStrip2.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CancelToolStripMenuItem, Me.ToolStripSeparator2, Me.BrushToolStripMenuItem})
        Me.ContextMenuStrip2.Name = "ContextMenuStrip2"
        Me.ContextMenuStrip2.Size = New System.Drawing.Size(127, 62)
        '
        'CancelToolStripMenuItem
        '
        Me.CancelToolStripMenuItem.Image = CType(resources.GetObject("CancelToolStripMenuItem.Image"), System.Drawing.Image)
        Me.CancelToolStripMenuItem.Name = "CancelToolStripMenuItem"
        Me.CancelToolStripMenuItem.Size = New System.Drawing.Size(126, 26)
        Me.CancelToolStripMenuItem.Text = "Cancel"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(123, 6)
        '
        'BrushToolStripMenuItem
        '
        Me.BrushToolStripMenuItem.Image = CType(resources.GetObject("BrushToolStripMenuItem.Image"), System.Drawing.Image)
        Me.BrushToolStripMenuItem.Name = "BrushToolStripMenuItem"
        Me.BrushToolStripMenuItem.Size = New System.Drawing.Size(126, 26)
        Me.BrushToolStripMenuItem.Text = "Clear"
        '
        'DotNetBarTabcontrol1
        '
        Me.DotNetBarTabcontrol1.Alignment = System.Windows.Forms.TabAlignment.Left
        Me.DotNetBarTabcontrol1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage1)
        Me.DotNetBarTabcontrol1.Controls.Add(Me.TabPage2)
        Me.DotNetBarTabcontrol1.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        Me.DotNetBarTabcontrol1.ItemSize = New System.Drawing.Size(44, 136)
        Me.DotNetBarTabcontrol1.Location = New System.Drawing.Point(0, 0)
        Me.DotNetBarTabcontrol1.Multiline = True
        Me.DotNetBarTabcontrol1.Name = "DotNetBarTabcontrol1"
        Me.DotNetBarTabcontrol1.SelectedIndex = 0
        Me.DotNetBarTabcontrol1.Size = New System.Drawing.Size(1072, 573)
        Me.DotNetBarTabcontrol1.SizeMode = System.Windows.Forms.TabSizeMode.Fixed
        Me.DotNetBarTabcontrol1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.BackColor = System.Drawing.Color.White
        Me.TabPage1.Controls.Add(Me.ListView1)
        Me.TabPage1.Controls.Add(Me.Button1)
        Me.TabPage1.Controls.Add(Me.TextBox1)
        Me.TabPage1.Controls.Add(Me.Label1)
        Me.TabPage1.Controls.Add(Me.ComboBox1)
        Me.TabPage1.Controls.Add(Me.Label10)
        Me.TabPage1.Location = New System.Drawing.Point(140, 4)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(928, 565)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "File Explorer"
        '
        'ListView1
        '
        Me.ListView1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ListView1.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1, Me.ColumnHeader3, Me.ColumnHeader2})
        Me.ListView1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.ListView1.GridLines = True
        Me.ListView1.Location = New System.Drawing.Point(10, 45)
        Me.ListView1.MultiSelect = False
        Me.ListView1.Name = "ListView1"
        Me.ListView1.Size = New System.Drawing.Size(888, 514)
        Me.ListView1.SmallImageList = Me.ImageList1
        Me.ListView1.TabIndex = 43
        Me.ListView1.UseCompatibleStateImageBehavior = False
        Me.ListView1.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Text = "Name"
        Me.ColumnHeader1.Width = 360
        '
        'ColumnHeader3
        '
        Me.ColumnHeader3.Text = "Size"
        Me.ColumnHeader3.Width = 168
        '
        'ColumnHeader2
        '
        Me.ColumnHeader2.Text = "File"
        Me.ColumnHeader2.Width = 125
        '
        'TextBox1
        '
        Me.TextBox1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox1.Location = New System.Drawing.Point(486, 12)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ReadOnly = True
        Me.TextBox1.Size = New System.Drawing.Size(374, 26)
        Me.TextBox1.TabIndex = 41
        Me.TextBox1.Text = "\"
        '
        'Label1
        '
        Me.Label1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(381, 15)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(99, 19)
        Me.Label1.TabIndex = 40
        Me.Label1.Text = "Remote Path : "
        '
        'ComboBox1
        '
        Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox1.FlatStyle = System.Windows.Forms.FlatStyle.System
        Me.ComboBox1.FormattingEnabled = True
        Me.ComboBox1.Location = New System.Drawing.Point(64, 12)
        Me.ComboBox1.Name = "ComboBox1"
        Me.ComboBox1.Size = New System.Drawing.Size(289, 27)
        Me.ComboBox1.TabIndex = 39
        '
        'Label10
        '
        Me.Label10.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label10.AutoSize = True
        Me.Label10.Location = New System.Drawing.Point(6, 15)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(52, 19)
        Me.Label10.TabIndex = 38
        Me.Label10.Text = "Drive : "
        '
        'TabPage2
        '
        Me.TabPage2.BackColor = System.Drawing.Color.White
        Me.TabPage2.Controls.Add(Me.Button2)
        Me.TabPage2.Controls.Add(Me.ListView2)
        Me.TabPage2.Location = New System.Drawing.Point(140, 4)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(928, 565)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Transfers"
        '
        'Button2
        '
        Me.Button2.FlatStyle = System.Windows.Forms.FlatStyle.System
        Me.Button2.Location = New System.Drawing.Point(6, 8)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(170, 30)
        Me.Button2.TabIndex = 1
        Me.Button2.Text = "&Open Download Folder"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'ListView2
        '
        Me.ListView2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ListView2.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader4, Me.ColumnHeader5, Me.ColumnHeader6, Me.ColumnHeader7})
        Me.ListView2.ContextMenuStrip = Me.ContextMenuStrip2
        Me.ListView2.GridLines = True
        Me.ListView2.Location = New System.Drawing.Point(6, 44)
        Me.ListView2.Name = "ListView2"
        Me.ListView2.Size = New System.Drawing.Size(888, 514)
        Me.ListView2.TabIndex = 0
        Me.ListView2.UseCompatibleStateImageBehavior = False
        Me.ListView2.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader4
        '
        Me.ColumnHeader4.Text = "ID"
        Me.ColumnHeader4.Width = 128
        '
        'ColumnHeader5
        '
        Me.ColumnHeader5.Text = "Transfer Type"
        Me.ColumnHeader5.Width = 100
        '
        'ColumnHeader6
        '
        Me.ColumnHeader6.Text = "Status"
        Me.ColumnHeader6.Width = 173
        '
        'ColumnHeader7
        '
        Me.ColumnHeader7.Text = "Filename"
        Me.ColumnHeader7.Width = 289
        '
        'FileManager
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1072, 598)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.DotNetBarTabcontrol1)
        Me.Name = "FileManager"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "File Manager ["
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.ContextMenuStrip2.ResumeLayout(False)
        Me.DotNetBarTabcontrol1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents DotNetBarTabcontrol1 As DotNetBarTabcontrol
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As ToolStripStatusLabel
    Friend WithEvents ListView1 As ListView
    Friend WithEvents ColumnHeader1 As ColumnHeader
    Friend WithEvents ColumnHeader2 As ColumnHeader
    Friend WithEvents ColumnHeader3 As ColumnHeader
    Friend WithEvents Button1 As Button
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents ComboBox1 As ComboBox
    Friend WithEvents Label10 As Label
    Friend WithEvents Button2 As Button
    Friend WithEvents ListView2 As ListView
    Friend WithEvents ColumnHeader4 As ColumnHeader
    Friend WithEvents ColumnHeader5 As ColumnHeader
    Friend WithEvents ColumnHeader6 As ColumnHeader
    Friend WithEvents ColumnHeader7 As ColumnHeader
    Friend WithEvents ImageList1 As ImageList
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents DownloadToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents UploadToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ImageList2 As ImageList
    Friend WithEvents BackToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ContextMenuStrip2 As ContextMenuStrip
    Friend WithEvents BrushToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents CancelToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator2 As ToolStripSeparator
    Friend WithEvents ToolStripSeparator3 As ToolStripSeparator
    Friend WithEvents DeleteToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ExecuteToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents RenameToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator4 As ToolStripSeparator
    Friend WithEvents AddToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents RefreshToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents MoveToToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DesktopToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents StartupToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DocumentsToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents TempToolStripMenuItem As ToolStripMenuItem
End Class
