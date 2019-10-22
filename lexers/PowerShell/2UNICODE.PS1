# Convert any text file to Unicode

param( [string] $infile = $(throw "Please specify a filename.") )

$outfile = "$infile.unicode"

get-content -Path $infile | out-file $outfile -encoding unicode
