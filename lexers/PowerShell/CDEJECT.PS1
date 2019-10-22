param( [string] $help )

if ( $help -eq "" ) {
	$colCDROMs = ( new-object -COM WMPlayer.OCX.7 ).cdromCollection
	for ( $i = 0; $i -lt $colCDROMs.count ; $i++ ) { $colCDROMs.Item( $i ).eject( ) }
}
else {
	write-host
	write-host "CDEject.ps1, Version 1.00"
	write-host "Eject all CDROMs"
	write-host
	write-host "Needs Windows Media Player installed"
	write-host
	write-host "Based on VBScript code posted by Christoph Fricke on"
	write-host "the microsoft.public.de.german.scripting.wsh newsgroup"
	write-host
	write-host "Written by Rob van der Woude"
	write-host "http://www.robvanderwoude.com"
	write-host
}
