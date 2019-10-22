namespace FSharp.IntelliSense.Gui
{
  partial class MainForm
  {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
      this.pictureBox1 = new System.Windows.Forms.PictureBox();
      this.tabPage4 = new System.Windows.Forms.TabPage();
      this.webHelp = new System.Windows.Forms.WebBrowser();
      this.tabPage3 = new System.Windows.Forms.TabPage();
      this.txtHtml = new System.Windows.Forms.TextBox();
      this.tabPage2 = new System.Windows.Forms.TabPage();
      this.label6 = new System.Windows.Forms.Label();
      this.webPreview = new System.Windows.Forms.WebBrowser();
      this.txtErrors = new System.Windows.Forms.TextBox();
      this.tabPage1 = new System.Windows.Forms.TabPage();
      this.checkLineNumbers = new System.Windows.Forms.CheckBox();
      this.label7 = new System.Windows.Forms.Label();
      this.comboStandard = new System.Windows.Forms.ComboBox();
      this.label5 = new System.Windows.Forms.Label();
      this.txtPrefix = new System.Windows.Forms.TextBox();
      this.txtFile = new System.Windows.Forms.TextBox();
      this.txtSettings = new System.Windows.Forms.TextBox();
      this.txtSymbols = new System.Windows.Forms.TextBox();
      this.txtSource = new System.Windows.Forms.TextBox();
      this.label3 = new System.Windows.Forms.Label();
      this.btnCancel = new System.Windows.Forms.Button();
      this.label1 = new System.Windows.Forms.Label();
      this.btnParse = new System.Windows.Forms.Button();
      this.label2 = new System.Windows.Forms.Label();
      this.btnLoad = new System.Windows.Forms.Button();
      this.tabControl = new System.Windows.Forms.TabControl();
      this.label4 = new System.Windows.Forms.Label();
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
      this.tabPage4.SuspendLayout();
      this.tabPage3.SuspendLayout();
      this.tabPage2.SuspendLayout();
      this.tabPage1.SuspendLayout();
      this.tabControl.SuspendLayout();
      this.SuspendLayout();
      // 
      // openFileDialog
      // 
      this.openFileDialog.Filter = "F# files|*.fs;*.fsx";
      // 
      // pictureBox1
      // 
      this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
      this.pictureBox1.Location = new System.Drawing.Point(17, 9);
      this.pictureBox1.Name = "pictureBox1";
      this.pictureBox1.Size = new System.Drawing.Size(215, 49);
      this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
      this.pictureBox1.TabIndex = 13;
      this.pictureBox1.TabStop = false;
      // 
      // tabPage4
      // 
      this.tabPage4.Controls.Add(this.webHelp);
      this.tabPage4.Location = new System.Drawing.Point(4, 22);
      this.tabPage4.Name = "tabPage4";
      this.tabPage4.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage4.Size = new System.Drawing.Size(721, 418);
      this.tabPage4.TabIndex = 3;
      this.tabPage4.Text = "Help";
      this.tabPage4.UseVisualStyleBackColor = true;
      // 
      // webHelp
      // 
      this.webHelp.Dock = System.Windows.Forms.DockStyle.Fill;
      this.webHelp.Location = new System.Drawing.Point(3, 3);
      this.webHelp.MinimumSize = new System.Drawing.Size(20, 20);
      this.webHelp.Name = "webHelp";
      this.webHelp.Size = new System.Drawing.Size(611, 410);
      this.webHelp.TabIndex = 5;
      // 
      // tabPage3
      // 
      this.tabPage3.Controls.Add(this.txtHtml);
      this.tabPage3.Location = new System.Drawing.Point(4, 22);
      this.tabPage3.Name = "tabPage3";
      this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage3.Size = new System.Drawing.Size(721, 418);
      this.tabPage3.TabIndex = 2;
      this.tabPage3.Text = "Generated code";
      this.tabPage3.UseVisualStyleBackColor = true;
      // 
      // txtHtml
      // 
      this.txtHtml.Dock = System.Windows.Forms.DockStyle.Fill;
      this.txtHtml.Font = new System.Drawing.Font("Consolas", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
      this.txtHtml.Location = new System.Drawing.Point(3, 3);
      this.txtHtml.Multiline = true;
      this.txtHtml.Name = "txtHtml";
      this.txtHtml.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.txtHtml.Size = new System.Drawing.Size(611, 343);
      this.txtHtml.TabIndex = 1;
      this.txtHtml.WordWrap = false;
      // 
      // tabPage2
      // 
      this.tabPage2.Controls.Add(this.label6);
      this.tabPage2.Controls.Add(this.webPreview);
      this.tabPage2.Controls.Add(this.txtErrors);
      this.tabPage2.Location = new System.Drawing.Point(4, 22);
      this.tabPage2.Name = "tabPage2";
      this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage2.Size = new System.Drawing.Size(721, 418);
      this.tabPage2.TabIndex = 1;
      this.tabPage2.Text = "Preview and errors";
      this.tabPage2.UseVisualStyleBackColor = true;
      // 
      // label6
      // 
      this.label6.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.label6.AutoSize = true;
      this.label6.Location = new System.Drawing.Point(0, 299);
      this.label6.Name = "label6";
      this.label6.Size = new System.Drawing.Size(94, 13);
      this.label6.TabIndex = 5;
      this.label6.Text = "Compilation Errors:";
      // 
      // webPreview
      // 
      this.webPreview.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.webPreview.Location = new System.Drawing.Point(3, 3);
      this.webPreview.MinimumSize = new System.Drawing.Size(20, 20);
      this.webPreview.Name = "webPreview";
      this.webPreview.Size = new System.Drawing.Size(715, 293);
      this.webPreview.TabIndex = 4;
      // 
      // txtErrors
      // 
      this.txtErrors.Dock = System.Windows.Forms.DockStyle.Bottom;
      this.txtErrors.Font = new System.Drawing.Font("Consolas", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
      this.txtErrors.ForeColor = System.Drawing.Color.DarkRed;
      this.txtErrors.Location = new System.Drawing.Point(3, 316);
      this.txtErrors.Multiline = true;
      this.txtErrors.Name = "txtErrors";
      this.txtErrors.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.txtErrors.Size = new System.Drawing.Size(715, 99);
      this.txtErrors.TabIndex = 3;
      this.txtErrors.WordWrap = false;
      // 
      // tabPage1
      // 
      this.tabPage1.Controls.Add(this.checkLineNumbers);
      this.tabPage1.Controls.Add(this.label7);
      this.tabPage1.Controls.Add(this.comboStandard);
      this.tabPage1.Controls.Add(this.label5);
      this.tabPage1.Controls.Add(this.txtPrefix);
      this.tabPage1.Controls.Add(this.txtFile);
      this.tabPage1.Controls.Add(this.txtSettings);
      this.tabPage1.Controls.Add(this.txtSymbols);
      this.tabPage1.Controls.Add(this.txtSource);
      this.tabPage1.Controls.Add(this.label3);
      this.tabPage1.Controls.Add(this.btnCancel);
      this.tabPage1.Controls.Add(this.label1);
      this.tabPage1.Controls.Add(this.btnParse);
      this.tabPage1.Controls.Add(this.label2);
      this.tabPage1.Controls.Add(this.btnLoad);
      this.tabPage1.Location = new System.Drawing.Point(4, 22);
      this.tabPage1.Name = "tabPage1";
      this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
      this.tabPage1.Size = new System.Drawing.Size(721, 418);
      this.tabPage1.TabIndex = 0;
      this.tabPage1.Text = "Source";
      this.tabPage1.UseVisualStyleBackColor = true;
      // 
      // checkLineNumbers
      // 
      this.checkLineNumbers.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.checkLineNumbers.AutoSize = true;
      this.checkLineNumbers.Checked = true;
      this.checkLineNumbers.CheckState = System.Windows.Forms.CheckState.Checked;
      this.checkLineNumbers.Location = new System.Drawing.Point(9, 386);
      this.checkLineNumbers.Name = "checkLineNumbers";
      this.checkLineNumbers.Size = new System.Drawing.Size(107, 17);
      this.checkLineNumbers.TabIndex = 15;
      this.checkLineNumbers.Text = "Add line numbers";
      this.checkLineNumbers.UseVisualStyleBackColor = true;
      // 
      // label7
      // 
      this.label7.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.label7.AutoSize = true;
      this.label7.Location = new System.Drawing.Point(486, 332);
      this.label7.Name = "label7";
      this.label7.Size = new System.Drawing.Size(53, 13);
      this.label7.TabIndex = 14;
      this.label7.Text = "Standard:";
      // 
      // comboStandard
      // 
      this.comboStandard.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.comboStandard.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
      this.comboStandard.FormattingEnabled = true;
      this.comboStandard.Location = new System.Drawing.Point(544, 328);
      this.comboStandard.Name = "comboStandard";
      this.comboStandard.Size = new System.Drawing.Size(168, 21);
      this.comboStandard.TabIndex = 13;
      // 
      // label5
      // 
      this.label5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.label5.AutoSize = true;
      this.label5.Location = new System.Drawing.Point(486, 358);
      this.label5.Name = "label5";
      this.label5.Size = new System.Drawing.Size(36, 13);
      this.label5.TabIndex = 12;
      this.label5.Text = "Prefix:";
      // 
      // txtPrefix
      // 
      this.txtPrefix.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.txtPrefix.Location = new System.Drawing.Point(544, 355);
      this.txtPrefix.Name = "txtPrefix";
      this.txtPrefix.Size = new System.Drawing.Size(168, 20);
      this.txtPrefix.TabIndex = 11;
      this.txtPrefix.Text = "fstips";
      // 
      // txtFile
      // 
      this.txtFile.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtFile.Location = new System.Drawing.Point(67, 8);
      this.txtFile.Name = "txtFile";
      this.txtFile.Size = new System.Drawing.Size(564, 20);
      this.txtFile.TabIndex = 10;
      // 
      // txtSettings
      // 
      this.txtSettings.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtSettings.Location = new System.Drawing.Point(103, 329);
      this.txtSettings.Name = "txtSettings";
      this.txtSettings.Size = new System.Drawing.Size(367, 20);
      this.txtSettings.TabIndex = 8;
      // 
      // txtSymbols
      // 
      this.txtSymbols.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtSymbols.Location = new System.Drawing.Point(102, 355);
      this.txtSymbols.Name = "txtSymbols";
      this.txtSymbols.Size = new System.Drawing.Size(368, 20);
      this.txtSymbols.TabIndex = 2;
      // 
      // txtSource
      // 
      this.txtSource.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                  | System.Windows.Forms.AnchorStyles.Left)
                  | System.Windows.Forms.AnchorStyles.Right)));
      this.txtSource.Font = new System.Drawing.Font("Consolas", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
      this.txtSource.Location = new System.Drawing.Point(8, 37);
      this.txtSource.Multiline = true;
      this.txtSource.Name = "txtSource";
      this.txtSource.ScrollBars = System.Windows.Forms.ScrollBars.Both;
      this.txtSource.Size = new System.Drawing.Size(704, 286);
      this.txtSource.TabIndex = 0;
      this.txtSource.Text = resources.GetString("txtSource.Text");
      this.txtSource.WordWrap = false;
      // 
      // label3
      // 
      this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.label3.AutoSize = true;
      this.label3.Location = new System.Drawing.Point(10, 332);
      this.label3.Name = "label3";
      this.label3.Size = new System.Drawing.Size(76, 13);
      this.label3.TabIndex = 9;
      this.label3.Text = "Command line:";
      // 
      // btnCancel
      // 
      this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.btnCancel.Location = new System.Drawing.Point(556, 382);
      this.btnCancel.Name = "btnCancel";
      this.btnCancel.Size = new System.Drawing.Size(75, 23);
      this.btnCancel.TabIndex = 6;
      this.btnCancel.Text = "Cancel";
      this.btnCancel.UseVisualStyleBackColor = true;
      // 
      // label1
      // 
      this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
      this.label1.AutoSize = true;
      this.label1.Location = new System.Drawing.Point(9, 358);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(87, 13);
      this.label1.TabIndex = 5;
      this.label1.Text = "Defined symbols:";
      // 
      // btnParse
      // 
      this.btnParse.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
      this.btnParse.Location = new System.Drawing.Point(637, 382);
      this.btnParse.Name = "btnParse";
      this.btnParse.Size = new System.Drawing.Size(75, 23);
      this.btnParse.TabIndex = 4;
      this.btnParse.Text = "Parse";
      this.btnParse.UseVisualStyleBackColor = true;
      // 
      // label2
      // 
      this.label2.AutoSize = true;
      this.label2.Location = new System.Drawing.Point(6, 11);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(55, 13);
      this.label2.TabIndex = 6;
      this.label2.Text = "File name:";
      // 
      // btnLoad
      // 
      this.btnLoad.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.btnLoad.Location = new System.Drawing.Point(637, 6);
      this.btnLoad.Name = "btnLoad";
      this.btnLoad.Size = new System.Drawing.Size(75, 23);
      this.btnLoad.TabIndex = 3;
      this.btnLoad.Text = "Load";
      this.btnLoad.UseVisualStyleBackColor = true;
      // 
      // tabControl
      // 
      this.tabControl.Controls.Add(this.tabPage1);
      this.tabControl.Controls.Add(this.tabPage2);
      this.tabControl.Controls.Add(this.tabPage3);
      this.tabControl.Controls.Add(this.tabPage4);
      this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
      this.tabControl.Location = new System.Drawing.Point(0, 67);
      this.tabControl.Name = "tabControl";
      this.tabControl.SelectedIndex = 0;
      this.tabControl.Size = new System.Drawing.Size(729, 444);
      this.tabControl.TabIndex = 12;
      // 
      // label4
      // 
      this.label4.Dock = System.Windows.Forms.DockStyle.Top;
      this.label4.Font = new System.Drawing.Font("Calibri", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(238)));
      this.label4.ForeColor = System.Drawing.Color.SteelBlue;
      this.label4.Location = new System.Drawing.Point(0, 0);
      this.label4.Name = "label4";
      this.label4.Size = new System.Drawing.Size(729, 67);
      this.label4.TabIndex = 11;
      this.label4.Text = "  F# Web Snippets";
      this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(234)))), ((int)(((byte)(234)))), ((int)(((byte)(234)))));
      this.ClientSize = new System.Drawing.Size(729, 511);
      this.Controls.Add(this.pictureBox1);
      this.Controls.Add(this.tabControl);
      this.Controls.Add(this.label4);
      this.Name = "MainForm";
      this.Text = "F# Web Snippets";
      ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
      this.tabPage4.ResumeLayout(false);
      this.tabPage3.ResumeLayout(false);
      this.tabPage3.PerformLayout();
      this.tabPage2.ResumeLayout(false);
      this.tabPage2.PerformLayout();
      this.tabPage1.ResumeLayout(false);
      this.tabPage1.PerformLayout();
      this.tabControl.ResumeLayout(false);
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    public System.Windows.Forms.OpenFileDialog openFileDialog;
    private System.Windows.Forms.PictureBox pictureBox1;
    private System.Windows.Forms.TabPage tabPage4;
    public System.Windows.Forms.WebBrowser webHelp;
    private System.Windows.Forms.TabPage tabPage3;
    public System.Windows.Forms.TextBox txtHtml;
    private System.Windows.Forms.TabPage tabPage2;
    private System.Windows.Forms.Label label6;
    public System.Windows.Forms.WebBrowser webPreview;
    public System.Windows.Forms.TextBox txtErrors;
    private System.Windows.Forms.TabPage tabPage1;
    public System.Windows.Forms.CheckBox checkLineNumbers;
    private System.Windows.Forms.Label label7;
    public System.Windows.Forms.ComboBox comboStandard;
    private System.Windows.Forms.Label label5;
    public System.Windows.Forms.TextBox txtPrefix;
    public System.Windows.Forms.TextBox txtFile;
    public System.Windows.Forms.TextBox txtSettings;
    public System.Windows.Forms.TextBox txtSymbols;
    public System.Windows.Forms.TextBox txtSource;
    private System.Windows.Forms.Label label3;
    public System.Windows.Forms.Button btnCancel;
    private System.Windows.Forms.Label label1;
    public System.Windows.Forms.Button btnParse;
    private System.Windows.Forms.Label label2;
    public System.Windows.Forms.Button btnLoad;
    public System.Windows.Forms.TabControl tabControl;
    private System.Windows.Forms.Label label4;
  }
}

