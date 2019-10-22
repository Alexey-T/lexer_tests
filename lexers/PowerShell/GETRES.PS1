# WMI query to list all properties and values of the Win32_DisplayConfiguration class
# This PowerShell script was generated using the WMI Code Generator, Version 1.30
# http://www.robvanderwoude.com/updates/wmigen.html

param( [string]$strComputer = "." )

$colItems = get-wmiobject -class "Win32_DisplayConfiguration" -namespace "root\CIMV2" -computername $strComputer

foreach ($objItem in $colItems) {
	write-host "Device Name                    :" $objItem.DeviceName
	write-host "Pels Width                     :" $objItem.PelsWidth
	write-host "Pels Height                    :" $objItem.PelsHeight
	write-host "Bits Per Pel                   :" $objItem.BitsPerPel
	write-host "Display Frequency              :" $objItem.DisplayFrequency
	write-host
}
