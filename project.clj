(defproject clj-monopoly "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot clj-monopoly.core
  :target-path "target/%s"
  :plugins [[lein-bikeshed "0.5.2"]
            [lein-kibit "0.1.7"]]
  :profiles {:uberjar {:aot :all}})
