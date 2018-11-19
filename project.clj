(defproject dvlopt/htm
            "0.0.0-alpha0"

  :description  "Implementation of Hierarchical Temporal Memory"
  :url          "https://github.com/dvlopt/htm"
  :license      {:name "Eclipse Public License"
                 :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[dvlopt/void         "0.0.1"]
                 [org.clojure/clojure "1.9.0"]]
  :profiles     {:dev     {:source-paths ["dev"]
                           :main         user
                           :dependencies [[criterium              "0.4.4"]
                                          [org.clojure/test.check "0.10.0-alpha2"]]
                           :plugins      [[lein-codox      "0.10.3"]
                                          [venantius/ultra "0.5.2"]]
                           :codox        {:output-path  "doc/auto"
                                          :source-paths ["src"]}
                           :global-vars  {*warn-on-reflection* true}}
                 :uberjar {:aot :all}})
