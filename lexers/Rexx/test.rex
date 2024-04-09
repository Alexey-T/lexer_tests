/* Get domain controller name */
GetDC: procedure
	NETGETDCNAME = 510  --Comment
	DomainName = value( "DOMAIN", , "OS2ENVIRONMENT" )
	parse value NetMisc( NETGETDCNAME, DomainName, "" ) with myRc dcName
	if myRc <> "0" then do
		say "NetMisc NETGETDCNAME error: "||myRc
		exit 9
	end
return dcName

/* Get requester type */
RequesterType:
	NETSERVICE = 170
	MyRc = NetEnumerate( NETSERVICE, "serviceInfo", "" )
	if MyRc <> 0 then do
		say "NetEnumerate NETSERVICE error: "||MyRc
		exit 9
	end
	if serviceInfo.0 = 0 then do
		req_type = "UNKNOWN"
	end
	else do
		req_type = ""
		do i = 1 to serviceInfo.0
			if serviceInfo.i = "SERVER" then req_type = "SERVER"
		end
	end
return req_type

--Class test here
::class Test
::method Init
return
::method Done
return
