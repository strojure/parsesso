(defproject com.github.strojure/parsesso "1.2.0+287"
  :description "Parser combinators library for Clojure(Script)."
  :url "https://github.com/strojure/parsesso"
  :license {:name "The Unlicense" :url "https://unlicense.org"}

  :dependencies []

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.11.1"]
                                       [org.clojure/clojurescript "1.11.60"]]}
             :dev,,,,, {:dependencies [;; clojurescript tests
                                       [com.google.guava/guava "31.1-jre"]
                                       [olical/cljs-test-runner "3.8.0"]
                                       ;; inspiration libs
                                       [org.blancas/kern "1.1.0"]
                                       [rm-hull/jasentaa "0.2.5"]
                                       [the/parsatron "0.0.8"]]
                        :source-paths ["doc"]}}

  :aliases {"cljs-test" ["run" "-m" "cljs-test-runner.main"]}

  :clean-targets ["target" "cljs-test-runner-out"]

  :deploy-repositories [["clojars" {:url "https://clojars.org/repo" :sign-releases false}]])
