report.
" table_data has to refer to a generic type "data" because it doesn't actually
" exist yet. 
data table_data type ref to data.
perform initialize_table using 5 5 changing table_data.
break-point. " check table_data in debugger

" Init table creates a dynamic ABAP with a set height and width.
" data * ptrToData = initialize_table( int width, int height )
*&---------------------------------------------------------------------*
form initialize_table using width type i height type i changing board type ref to data.
  if ( width < 1 or height < 1 ). return. endif. " The table has to be at least so big
  " We have a dynamic row which is the structure
  " of dynamic_table after creating it via cl_alv_table_create=>...
  data: dynamic_table type ref to data.
  data: dynamic_row   type ref to data.
  data: col_index type i value 1.
  data: columns type lvc_t_fcat.
  data: column  like line of columns.
  field-symbols <table> type any table.
  field-symbols <row> type any.
  " We're going to create the number of columns in this structure
  " based on the input the function.
  do width times.
    column-fieldname = '_' && col_index.
    column-datatype  = cl_abap_structdescr=>typekind_char.
    column-inttype   = cl_abap_structdescr=>typekind_char.
    column-intlen    = 1.
    column-decimals  = 0.
    col_index = col_index + 1.
    append column to columns.
  enddo.
  " This is a standard SAP provided utility class which helps make this process easier.
  cl_alv_table_create=>create_dynamic_table(
    exporting it_fieldcatalog = columns
    importing ep_table = dynamic_table
  ).
  " The data has to be accessed via a field symbol.
  assign dynamic_table->* to <table>.
  create data dynamic_row like line of <table>.
  " For however tall the table is going to be I've appended it row for it.
  assign dynamic_row->* to <row>.
  do height times.
    insert <row> into table <table>.
  enddo.
endform.
