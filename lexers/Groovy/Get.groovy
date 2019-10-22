package groovyx.twitter
/**
 * Created by IntelliJ IDEA.
 * User: alleon
 * Date: May 6, 2008
 * Time: 11:47:47 AM
 * To change this template use File | Settings | File Templates.
 */
class Get implements HttpMethod {
    String url
    QueryString queryString = new QueryString()
    URLConnection connection
    String text

    /**
     *
     */
    String getText() {
        def thisUrl = new URL(this.toString())
        connection = thisUrl.openConnection()

        if (connection.responseCode == 200) {
            return connection.content.text
        } else {
            throw new Exception /*TwitterException*/(this.toString()+": ${connection.responseMessage} [${connection.responseCode}]")
        }
    }

    /**
     *
     */
    String toString() {
        return url + "?" + queryString.toString()
    }
}