package groovyx.twitter

class Twitter {

    def authenticatedUser = null
    XmlSlurper slurper = new XmlSlurper()

    Twitter(name, password) {
        Authenticator.setDefault( [getPasswordAuthentication : { return new PasswordAuthentication(name, password as char[]) } ] as Authenticator)
        authenticatedUser = getUser(name)
    }

    boolean logoff() {
        try {
            def connection = new URL("http://twitter.com/account/end_session").openConnection()
            connection.connect()
            authenticatedUser = null
            return true
        } catch (Exception e) {
            prinln e
            return false
        }
    }

    def getFriends() {
        return getFriends(authenticatedUser)
    }

    def getFriends(user) {
        def friends = []
        def page = 1
        def list = slurper.parse(new URL("http://twitter.com/statuses/friends/${user.id}.xml").openStream())
        while (list.length) {
            list.user.collect(friends) {it}
            page++
            try {
                list = slurper.parse(new URL("http://twitter.com/statuses/friends/${user.id}.xml&page=$page").openStream())
            } catch (Exception e) {
                break
            }
        }
        return friends
    }

    def getFollowers(){
        return getFollowers(authenticatedUser)
    }

    def getFollowers(user){
        def followers = []
        return followers
    }

    def getFriendsTimeline() {
        getFriendsTimeline(authenticatedUser)
    }

    def getFriendsTimeline(friend) {
        def timeline = slurper.parse(new URL("http://twitter.com/statuses/friends_timeline/${friend.screen_name}.xml").openStream()).status.collect{it}
        return timeline
    }

    def getTweets() {
      return getTweets(authenticatedUser)
    }

    def getTweets(friend) {
        def tweets = slurper.parse(
                new URL("http://twitter.com/statuses/user_timeline/${friend.screen_name}.xml").openStream()
            ).status.collect{it}
        return tweets
    }

    def postUpdate(String message){
        if (message.length() > 140) throw new Exception("Message too long")
        def tweet ="<status>$message</status>"
        def connection = new URL("http://twitter.com/statuses/update.xml").openConnection()
        connection.requestMethod = "POST"
        connection.setRequestProperty("Content-Type", "application/xml")
        connection.doOutput = true
        Writer writer = new OutputStreamWriter(connection.outputStream)
        writer.write(tweet)
        writer.flush()
        writer.close()
        connection.connect()

        def result = slurper.parseText(connection.content.text)
        assert result.truncated == false
    }

    def getUser(String name){
        if (!checkCredentials()) throw new Exception("Not authenticated")
        def connection
        if (name.contains('@')){
            connection = new URL("http://twitter.com/users/show.xml?email=${name}").openConnection()
        } else {
            connection = new URL("http://twitter.com/users/show/${name}.xml").openConnection()
        }
        connection.connect()
        def user = slurper.parseText(connection.content.text)
        return user
    }

    def checkCredentials() {
        def url = new URL("http://twitter.com/account/verify_credentials.xml")
        try {
            def connection = url.openConnection()
            connection.connect()
            def result = slurper.parseText(connection.content.text)
            return result as boolean
        } catch (Exception e) {
            return false
        }
    }

}
