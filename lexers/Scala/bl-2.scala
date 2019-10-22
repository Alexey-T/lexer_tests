trait Writer {
def write(str: String)
}
object Terminal extends Writer {
def write(str: String) { System.out.println(str) }
}