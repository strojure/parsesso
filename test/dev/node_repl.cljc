(ns dev.node-repl
  "ClojureScript Node REPL."
  (:require [cljs.repl :as repl]
            [cljs.repl.node :as node]))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- start
  []
  (-> (node/repl-env)
      (repl/repl :quit-prompt (fn []
                                (repl/repl-title)
                                (repl/repl-quit-prompt)))))

(def -main
  "Main entry point."
  start)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(comment
  (start)
  :cljs/quit
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
