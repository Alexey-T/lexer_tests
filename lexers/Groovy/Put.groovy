package groovyx.twitter
/**
 * Created by IntelliJ IDEA.
 * User: alleon
 * Date: May 6, 2008
 * Time: 12:38:49 PM
 * To change this template use File | Settings | File Templates.
 */
class Put implements HttpMethod {
    String url
    String body
    String contentType = "application/xml"
    URLConnection connection
    String text

    /**
     *
     */
    String getText() {
        def thisUrl = new URL(url)
        connection = thisUrl.openConnection()
        connection.setRequestMethod("PUT")
        connection.setRequestProperty("Content-Type", contentType)
        connection.doOutput = true

        Writer writer = new OutputStreamWriter(connection.outputStream)
        writer.write(body)
        writer.flush()
        writer.close()

        connection.connect()

        return connection.content.text
    }

    /**
     *
     */
    String toString() {
        return "PUT:\n${contentType}\n${url}\n${body}"
    }
}