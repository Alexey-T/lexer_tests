 <!DOCTYPE html>
<html>
<body>
<%
dim h
h=hour(now())

response.write("<p>" & now())
response.write("</p>")
If h<12 then
   response.write("Good Morning!")
else
   response.write("Good day!")
end if
%>
</body>
</html>
