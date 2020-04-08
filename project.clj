(defproject sausage "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.boofcv/boofcv-core "0.35"]
                 [cljfx "1.6.7"]
                 [org.openjfx/javafx-swing "13"]
                 [org.clojure/data.csv "0.1.4"]
                 [thi.ng/geom "1.0.0-RC4"]
                 [com.github.afester.javafx/FranzXaver "0.1"]]
  :main sausage.core
  :profiles {:uberjar {:aot :all}})
