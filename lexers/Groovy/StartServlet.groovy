//http://localhost:8080/gs/StartServlet.groovy
 
//headers : headers.host 
//params : params.myParam 
//session : session?.myParam 
//request : request.remoteHost 
//response : response.contentType=’text/xml’ 
//context : context.myParam 
//application : application.myParam 
//out response.writer  
//sout response.outputStream  

html.html {
    header {
        title 'Groovy First Servlet - V1.0'
    }
    body {
		center {
		    h1 'This is the response from the GroovyServlet .....'
		}
		hr { }
		table {
		    tr {
				td 'Request:'
				td (params.p1 ? params.p1 : 'not parameter "p1" available!')
		    }
		}
    }
}
