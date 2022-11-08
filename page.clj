;; bb page.clj
(require '[clojure.edn :as edn]
         '[clojure.java.browse :as browse]
         '[clojure.java.io :as io]
         '[cognitect.transit :as transit]
         '[org.httpkit.server :as srv]
         '[hiccup.core :as hp])

(import 'java.io.ByteArrayOutputStream)

(def port 8083)

(defn home-routes [{:keys [:request-method :uri] :as req}]
  (case [request-method uri]
    [:get "/"] {:body (slurp "talktwo.html")
                :status 200}
    [:get "/core.cljs"] {:body (slurp"core.cljs")
                         :status 200}
    [:get "/talktwo.css"] {:body (slurp"talktwo.css")
                         :status 200}))

(defn core-http-server []
  (srv/run-server home-routes {:port port}))

(let [url (str "http://localhost:" port "/")]
  (core-http-server)
  (println "serving" url)
  ;; (browse/browse-url url)
  @(promise))