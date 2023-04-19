# An example of using Janet's extensible module system to import files from
# URL. To try this, run `janet -l ./examples/urlloader.janet` from the command
# line, and then at the REPL type:
#
# (import https://raw.githubusercontent.com/janet-lang/janet/master/examples/colors.janet :as c)
#
# This will import a file using curl. You can then try:
#
# (print (c/color :green "Hello!"))
#
# This is a bit of a toy example (it just shells out to curl), but it is very
# powerful and will work well in many cases.

(defn- load-url
  [url args]
  (def p (os/spawn ["curl" url "-s"] :p {:out :pipe}))
  (def res (dofile (p :out) :source url ;args))
  (:wait p)
  res)

(defn- check-http-url
  [path]
  (if (or (string/has-prefix? "http://" path)
          (string/has-prefix? "https://" path))
    path))

# Add the module loader and path tuple to right places
(array/push module/paths [check-http-url :janet-http])
(put module/loaders :janet-http load-url)
