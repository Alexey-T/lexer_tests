<html>
 <head><title>Enter to database</title></head>
 <body>
 <table>
 <%@ page import="java.util.*" %>
 <%@ page import="javax.sql.*;" %>
 <%

 java.sql.Connection con;
 java.sql.Statement s;
 java.sql.ResultSet rs;
 java.sql.PreparedStatement pst;

 con=null;
 s=null;
 pst=null;
 rs=null;

// Remember to change the next line with your own environment
 String url=
 "jdbc:jtds:sqlserver://nameofyourdatabaseserver.or.ipaddress/yourdatabasename";
 String id= "username";
 String pass = "password";
 try{

 Class.forName("net.sourceforge.jtds.jdbc.Driver");
 con = java.sql.DriverManager.getConnection(url, id, pass);

 }catch(ClassNotFoundException cnfex){
 cnfex.printStackTrace();

 }
 String sql = "select top 10 * from tbl_sys_user";
 try{
 s = con.createStatement();
 rs = s.executeQuery(sql);
 %>

 <%
 while( rs.next() ){
 %><tr>
 <td><%= rs.getString("cust_id") %></td>
 <td><%= rs.getString("rdate") %></td>
 <td><%= rs.getString("email") %></td>
 </tr>
 <%
 }
 %>

 <%

 }
 catch(Exception e){e.printStackTrace();}
 finally{
 if(rs!=null) rs.close();
 if(s!=null) s.close();
 if(con!=null) con.close();
 }

 %>

 </body>
 </html>
