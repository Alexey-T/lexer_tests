function Verb-Noun
{
   [CmdletBinding()] # attribute not highlighted
   
   param
   (
      # not highlighted types
      [int32] $i32,
      [int64] $i64,
      [Parameter(ValueFromPipeline = $true)] # attribute not highlighted
      [hashtable] $ht, 
     
      [switch] $sw, # probably the "switch" is highlighted as keyword not as type
     
      [SupportsWildcards()] # attribute not highlighted
      # ok highlighted types
      [string] $str,
      [int] $ii,
      [bool] $b
   )
   
   # typecasting [...]
   # like in the param() section: [typename]
   # maybe highlighting the typename ...
   $arrlst = [System.Collections.ArrayList] @()
   
   # number specials (the SI standard KiB MiB ...)
   # <number><kb | mb | gb | tb | pb>
   $n = 1kb * 1mb
   $nn = 1024 * 1048576 # $nn equals $n
   
   # builtin variables not highlighted as special variables (it would be nice)
   # some exmaples
   $h = $host ; $e = $error ; $o = $ofs ; $oe = $outputencoding   
   $bt = $true ; $bf = $false ; $n = $null 
   
   # variable names can contain scope reference in the form of $scope:varname
   # the scope can be: global local script private
   # or an integer which is >=0
   $var1 = $global:error
   $var2 = $script:somevarname
   $var3 = $1:parentscopevar
   
   # not highlighted operators
   $s = "{0}" -f $var1
   if (-not ($a -or $b) -and ($c -or $d))
   {
      $aa = $bb -split ','
   }
}
 -eq, -ne, -gt, -lt, -le, -ge ! @()
       1.2L   
PS> 1kb
1024

PS> 1.30Dmb
1363148.80

PS> 0x10Gb
17179869184

PS> 1.4e23tb
1.5393162788864E+35

PS> 0x12Lpb
20266198323167232	
                    
100
100u
100D
100l
100uL
100us
100uy
100y
1e2
1.e2
0x1e2
0x1e2L
0x1e2D
482D
482gb
482ngb
0x1e2lgb
0b1011011
0xFFFFFFFF
-0xFFFFFFFF
0xFFFFFFFFu

[byte]
[sbyte]
[Int16]
[short]
[UInt16]
[ushort]
[Int32]
[int]
[UInt32]
[uint]
[Int64]
[long]
[UInt64]
[ulong]
[bigint]
[single]
[float]
[double]
[decimal]
		 
$$
$?
$^
$_
$args
$ConsoleFileName
$Error
$Event
$EventArgs
$EventSubscriber
$ExecutionContext
$false
$foreach
$HOME
$Host
$input
$IsCoreCLR
$IsLinux
$IsMacOS
$IsWindows
$LastExitCode
$Matches
$MyInvocation
$NestedPromptLevel
$null
$PID
$PROFILE
$PSBoundParameters
$PSCmdlet
$PSCommandPath
$PSCulture
$PSDebugContext
$PSHOME
$PSItem
$PSScriptRoot
$PSSenderInfo
$PSUICulture
$PSVersionTable
$PWD
$Sender
$ShellId
$StackTrace
$switch
$this
$true
