# Convert any text file to UTF-8

param( [string] $infile = $(throw "Please specify a filename.") )

$outfile = "$infile.utf8"

get-content -Path $infile | out-file $outfile -encoding utf8
