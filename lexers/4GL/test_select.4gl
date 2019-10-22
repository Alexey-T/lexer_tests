database maindb
main
	define syst record like systables.*




	select * into syst.* from systables where tabid=1
	display syst.tabname



	declare c1 cursor for 
		select * into syst.* from systables where tabid=2
	
	open c1
	fetch c1 
	close c1
	display syst.tabname


	declare c2 cursor for 
		select * into syst.* from systables where tabid=3
	
	foreach c2 
		display syst.tabname
	end foreach





	declare c3 cursor for 
		select *  from systables where tabid=4
	
	open c3
	fetch c3  into syst.*
	close c3
	display syst.tabname


	declare c4 cursor for 
		select *  from systables where tabid=5
	
	foreach c4  into syst.*
		display syst.tabname
	end foreach


sleep 5
end main
