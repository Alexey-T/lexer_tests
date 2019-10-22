;; http://spot.colorado.edu/~sitelic/samplecode/idl/
;;;;;;;;;;;;;   sample program, procedure, subroutine
;;;;;;;;;;;;;
;;;;;;;;;;;;;   Programming is case-insensitive *BUT* filenames aren't!
;;;;;;;;;;;;;	Functions are called by assignment, e.g.,  a = f(x,y).
;;;;;;;;;;;;;	Procedures are called by name, comma, parameters...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; a function ;;;;;;;;;;;;

FUNCTION F, x, y
return, sin(0.1 * x) * cos(0.1 * y)
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; a subroutine / subprogram ;;;;;;;

PRO INFO, a		; array A

	sz = size(A)
	dim = sz[0]	; dimension of array a
	IF dim eq 0 THEN print, "Is a single number,", a
	IF dim eq 1 THEN BEGIN
		print, 'Is a 1-dimensional array of length',sz[1]
		m = min([sz[1],3])
		print, ' starting with',A[0:m]
		ENDIF
	IF dim gt 1 THEN BEGIN
		print, 'Is an array of dimension', dim
		dims = sz[1:dim]
		print, 'size:', dims
		if dim le 6 then print, 'corner values:'
		if dim eq 2 then print, a[0:1,0:1]
		if dim eq 3 then print, a[0:1,0:1,0]
		if dim eq 4 then print, a[0:1,0:1,0,0]
		if dim eq 5 then print, a[0:1,0:1,0,0,0]
		ENDIF
	print
END				; of pro INFO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; the main program, "SUBS" ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO SUBS

print, 'This program is named SUBS'
print, 'Using function "F":'

print, 'f(1,2) equals', f(1,2)
z = f(1.1,2.2)

x = findgen(64) # (fltarr(64)+1)	; outer product
y = transpose(x)
z = f(x,y)
surface, z, title='Function F(x,y)', xtitle='X', $
	ytitle='This is Y', charsize=3

print

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

print, 'Using program/procedure "INFO":'

print, 'mylist = findgen(6)'
mylist = findgen(6)
info, mylist

print, 'mylist = randomu(seed, 5, 5)'
mylist = randomu(seed, 5, 5)
INFO, mylist

print, 'hyparray = findgen(20,20,20,20)'
hyparray = findgen(20,20,20,20)
iNfO, hyparray

END
