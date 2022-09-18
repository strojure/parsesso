(defproject com.github.strojure/parsesso "0.0.0-SNAPSHOT"
  :description "Parser combinators library for Clojure(Script)."
  :url "https://github.com/strojure/parsesso"
  :license {:name "The MIT License" :url "http://opensource.org/licenses/MIT"}

  :dependencies []

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.11.1"]
                                       [org.clojure/clojurescript "1.11.60"]]}
             :dev,,,,, {:dependencies [;; clojurescript repl deps
                                       [com.google.guava/guava "31.1-jre"]
                                       ;; inspiration libs
                                       [org.blancas/kern "1.1.0"]
                                       [rm-hull/jasentaa "0.2.5"]
                                       [the/parsatron "0.0.8"]]}}

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo" :sign-releases false}]])
