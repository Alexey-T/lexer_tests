# WMI query to list all properties and values of the Win32_BIOS class
# This PowerShell script was generated using the WMI Code Generator, Version 1.30
# http://www.robvanderwoude.com/updates/wmigen.html

param( [string]$strComputer = "." )

$colItems = get-wmiobject -class "Win32_BIOS" -namespace "root\CIMV2" -computername $strComputer

foreach ($objItem in $colItems) {
	write-host "Name                           :" $objItem.Name
	write-host "Version                        :" $objItem.Version
	write-host "Manufacturer                   :" $objItem.Manufacturer
	write-host "SMBIOSBIOS Version             :" $objItem.SMBIOSBIOSVersion
	write-host
}
