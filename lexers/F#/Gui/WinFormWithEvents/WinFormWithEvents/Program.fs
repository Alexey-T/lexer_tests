#light

open System
open System.Windows.Forms

let frm = new Form()
frm.Text <- "A Simple Win Form example"
frm.Height <- 120
frm.Width <- 350

let lblName = new Label()
lblName.Text <- "Name: "
lblName.Width <- 40
lblName.Left <- 10
lblName.Top <- 10

let txtName = new TextBox()
txtName.Left <- 60
txtName.Top <- 10
txtName.Width <- 200

let mb _ _ = ignore(MessageBox.Show(txtName.Text, "You typed"))
let btnHandler = new EventHandler(mb)

let btnSubmit = new Button()
btnSubmit.Text <- "Submit"
btnSubmit.Left <- 60
btnSubmit.Top <- 50
btnSubmit.Click.AddHandler(btnHandler)

frm.Controls.Add(lblName)
frm.Controls.Add(txtName)
frm.Controls.Add(btnSubmit)

[<STAThread>]
do Application.Run(frm)