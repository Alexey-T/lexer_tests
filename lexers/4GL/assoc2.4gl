define lv_rate ASSOCIATE CHAR(3) WITH ARRAY[2000] OF RECORD
		lv_name CHAR(20),
		lv_rates float
end record
define lv_name ASSOCIATE CHAR(3) WITH ARRAY[2000] OF CHAR(20)

#
# Data correct on 19/04/2002
#
main
DEFINE i FLOAT
define cur char(3)
# Use USD as base
	LET lv_rate<<"USD">>.lv_name="US Dollars"
	LET lv_rate<<"USD">>.lv_rates=1.0

	LET lv_rate<<"EUR">>.lv_name="Euro"
	LET lv_rate<<"EUR">>.lv_rates=0.888477

	LET lv_rate<<"GBP">>.lv_name="UK Pounds"
	LET lv_rate<<"GBP">>.lv_rates=1.44621

	LET lv_rate<<"JPY">>.lv_name="Japan Yen"
	LET lv_rate<<"JPY">>.lv_rates=0.00766891

	LET lv_rate<<"NZD">>.lv_name="New Zealand Dollars"
	LET lv_rate<<"NZD">>.lv_rates=0.447180

	LET lv_rate<<"PTE">>.lv_name="Portugal Escudos"
	LET lv_rate<<"PTE">>.lv_rates=0.00443157

LET i=5
clear screen

CALL banner(" ",1)
CALL banner("Currency Converter",2)
CALL banner(" ",3)

options prompt line last
while true
PROMPT "Enter short code : " for cur
DISPLAY "" at 5,1
DISPLAY" if I have ",i using "#####&&.&&", " ",cur," (", lv_rate<<cur>>.lv_name clipped,") thats about ",lv_rate<<cur>>.lv_rates*i," US Dollars" AT 5,1 attribute(red)
end while
sleep 2

end main

FUNCTION banner(s,n)
DEFINE s CHAR(80)
DEFINE n INTEGER
DEFINE l integer
DEFINE new_string CHAR(80)
LET new_string=" "
LET l=(80-length(s))/2
LET new_string[l,80]=s
DISPLAY new_string at n,1 attribute(reverse)
END FUNCTION
