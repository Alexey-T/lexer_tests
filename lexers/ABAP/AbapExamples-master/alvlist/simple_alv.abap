"/""
" Abap Examples
" Bryan Abrams - Ctac NL - 17/01/2014
"
" A simple demonstration of using the object CL_SALV_TABLE which creates an easy to use
" table in your reports. The table includes the standard functionality of being able to
" edit the data, give the table pretty colors, sort, and the ability to add custom
" functionality. This is just a simple example of creating a table, formatting columns,
" and then displaying it.
"
"/
report z_bryan_alv_example.

"/""
" This is the structure of how the data is gonna look in the table. Simple, elementary
" data types, no reason to gunk it up with all the different data elements.
"/
types: begin of line_structure,
        id type i,
        date type datum,
        time type uzeit,
        message type string,
       end of line_structure.

perform run_application.

"/""
" Global variables are BAD (Another tutorial).
"/
form run_application.

  data test_data type standard table of line_structure.
  data test_alv type ref to cl_salv_table.

  perform create_test_data tables test_data.

  try.
    perform create_alv_table tables test_data changing test_alv.
    perform set_alv_functions                 changing test_alv.
    perform format_columns                    changing test_alv.
    test_alv->display( ).
  catch cx_salv_msg.
    write: / 'ALV error'.
  endtry.

endform.

"/""
" The constructor of CL_SALV_TABLE is private. The only way to be able to create
" and use an instance of an ALV list in your report is to create the table via
" a static method CL_SALV_TABLE.
"/
form create_alv_table tables table_data
                      changing alv_table type ref to cl_salv_table raising cx_salv_msg.
    cl_salv_table=>factory( importing r_salv_table = alv_table
                             changing t_table      = table_data[] ).
endform.

"/""
" There is a standard set of functions for things like sorting, filtering, etc already
" avaliable with the ALV list. If you need custom events or buttons, those can be
" added as well.
"/
form set_alv_functions changing alv_table type ref to cl_salv_table raising cx_salv_msg.

  " I want to make sure the table actually exists before assigning it functions
  if ( alv_table is not bound ).
    return.
  endif.

  data functions type ref to cl_salv_functions_list.
  functions = alv_table->get_functions( ).
  functions->set_all( ).

endform.

"/""
" This method isn't really that important to the ALV list. I just fill the table with
" some random data. Really you could replace this with data from a selection, or BAPI
" or whatever. The factory will handle the structure of the ALV table based on the
" structure of the table.
"/
form create_test_data tables line_table. "line_structure.

  data line type line_structure.
  data index type i value 0.

  do 10 times.
    line-id = index.
    line-date = sy-datum.
    line-time = sy-uzeit.
    line-message = 'Index Line: ' && space && line-id.
    index = index + 1.
    append line to line_table[].
  enddo.

endform.

"/""
" Normally if the structure of the table includes a data element with a text description,
" (The short, medium, and long text) than the column will automatically assume the title
" of the column should match the data element. If you don't use data elements, or don't want
" to use the text from the data element, you can format the column yourself to say
" what you want.
"/
form format_columns changing alv_table type ref to cl_salv_table raising cx_salv_msg.

  " If I try to use the reference alv_table and it wasn't created for some reason
  " than the program will dump.
  if ( alv_table is not bound ).
    return.
  endif.

  " CL_SALV_COLUMNS_TABLE is the collection of columns IN TOTAL in your ALV list
  " CL_SALV_COLUMN_TABLE  is an individual column by itself
  data alv_columns type ref to cl_salv_columns_table.
  data alv_column type ref to cl_salv_column.

  " The reference for alv_columns should be equal to the reference in the ALV table object.
  alv_columns = alv_table->get_columns( ).

  " And as such now that you have the reference to all of the columns, you can begin
  " getting the individual columns to format them in specific ways.
  " Just a note: elementary types like integers, floats, packed, strings, characters
  " will have empty column names.
  try.
    " The short, medium, and long text are shown depending on the physical width
    " of the column on your screen. You select a column to modify by supplying
    " the instance method of CL_SALV_COLUMNS_TABLE with the ID of the field
    " in the structure of the data. It has to be in CAPS.
    alv_column = alv_columns->get_column( columnname = 'ID' ).
    alv_column->set_short_text( 'ID #' ).
    alv_column->set_medium_text( 'ID Num').
    alv_column->set_long_text( 'ID Number' ).
  catch cx_salv_not_found.
    " If this exception occured it means the column name you supplied
    " wasn't found in the ALV table. Just remember the name is all caps.
  endtry.

  " And we repeat the same for the message column.
  try.
    alv_column = alv_columns->get_column( columnname = 'MESSAGE' ).
    alv_column->set_short_text( 'Msg' ).
    alv_column->set_medium_text( 'Message').
    alv_column->set_long_text( 'Message' ).
  catch cx_salv_not_found.
  endtry.

endform.