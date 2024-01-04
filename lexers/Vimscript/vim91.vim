
class TextPosition
   var lnum: number
   var col: number

   def new(lnum: number, col: number)
	  this.lnum = lnum
	  this.col = col
   enddef

   def SetLnum(lnum: number)
	  this.lnum = lnum
   enddef

   def SetCol(col: number)
	  this.col = col
   enddef

   def SetPosition(lnum: number, col: number)
	  this.lnum = lnum
	  this.col = col
   enddef
 endclass
 
 
var pos = TextPosition.new(1, 1)
public var lnum: number
public var col: number


def GetLnum(): number
   return this._lnum
enddef

def GetCol() number
   return this._col
enddef
	

def GetLnum(): number
   if this._lnum > this._lineCount
	  return this._lineCount
   endif
   return this._lnum
enddef



class SomeClass
	def _Foo(): number
	  return 10
	enddef
	def Bar(): number
	  return this._Foo()
	enddef
endclass
    

class OtherThing
	static var _sum: number	          # only class can read and write
	public static var result: number  # anybody can read and write
	def Bar(): number
	  return this._Foo()
	enddef
endclass


class A
	final v1 = [1, 2]		# final object variable
	public final v2 = {x: 1}	# final object variable
	static final v3 = 'abc'		# final class variable
	public static final v4 = 0z10	# final class variable
endclass
