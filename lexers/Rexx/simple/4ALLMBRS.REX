/* Execute a command For All group Members */


/* Command line parsing */
parse arg group commandline
if left( group, 1 ) = '"' then parse arg '"'group'"' commandline
if pos( "?", group ) > 0 then call Syntax
if commandline = ""  then call Syntax


/* Initialize RexxUtil */
if RxFuncQuery( "SysLoadFuncs" ) <> 0 then do
	call RxFuncAdd "SysLoadFuncs", "RexxUtil", "SysLoadFuncs"
	call SysLoadFuncs
end


/* Initialize LAN Server RexxUtil */
if RxFuncQuery( "LoadLsRxutFuncs" ) <> 0 then do
	call RxFuncAdd "LoadLsRxutFuncs", "LSRxUt", "LoadLsRxutFuncs"
	call LoadLsRxutFuncs
end


/* Get the requester type (Server, Peer or Requester)         */
/* If it is not a peer server, get the domain controller name */
ReqType = RequesterType()
if ReqType = "PEER" then do
	SrvName = ""
end
else do
	SrvName = GetDC()
end

/* List all users from the specified group */
NETGROUPUSERS = 340
myRc = NetGetInfo( NETGROUPUSERS, "groupInfo", SrvName, group )
if myRc <> "0" then do
	say "NetGetInfo NETGROUPUSERS "||myRc
	EXIT 1
end


/* Execute specified command for each user from specified group */
do i=1 to groupInfo.0
	fullcommand = commandline
	/* Substitute user ID for "#" */
	do until upos = 0
		upos = pos( "#", fullcommand )
		if upos > 0 then do
			fullcommand = substr( fullcommand, 1, upos - 1 )||,
			              groupInfo.i||,
			              substr( fullcommand, upos + 1 )
		end
	end
	/* Execute command */
	address CMD "@CMD /C "||fullcommand
end
EXIT 0


/* Get domain controller name */
GetDC: procedure
	NETGETDCNAME = 510
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
		if req_type = "" then do i = 1 to serviceInfo.0
			if serviceInfo.i = "PEER" then req_type = "PEER"
		end
		if req_type = "" then do i = 1 to serviceInfo.0
			if serviceInfo.i = "REQUESTER" then req_type = "REQUESTER"
		end
	end
return req_type


/* Help message */
Syntax: procedure
	say
	say "4AllMembers,  Version 1.01 for OS/2"
	say "Executes a command once for every member of a specified group"
	say "Written by Rob van der Woude"
	say "http://www.robvanderwoude.com"
	say
	say "Usage:  4ALLMEMBERS  <group>  <command>  [ <parameters> ]"
	say
	say '"#" characters in <parameters> will be substituted'
	say "by the user ID when <command> is executed"
	say
	say "Set the environment variable DOMAIN=<logon_domain>"
	say "when using this Rexx script in a LAN Server environment"
	EXIT 1
return
