package groovyx.twitter
/**
 * Created by IntelliJ IDEA.
 * User: alleon
 * Date: May 6, 2008
 * Time: 11:49:43 AM
 * To change this template use File | Settings | File Templates.
 */
class QueryString {
    Map params = [:]

    /**
     * This constructor allows to pass in a Map
     */
    QueryString(Map params) {
        if (params) {
            this.params.putAll params
        }
    }

    /**
     * this methods allows to add name/value pairs
     */
    void add(String name, Object value){
        params.put(name, value)
    }

    /**
     * this method returns a well-formed QueryString
     */
    String toString(){
        def list=[]
        params.each{name, value ->
            list << "$name=" + URLEncoder.encode(value.toString())
        }
        return list.join("&")
    } 
}