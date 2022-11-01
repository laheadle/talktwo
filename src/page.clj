;; bb page.clj
(require '[clojure.edn :as edn]
         '[clojure.java.browse :as browse]
         '[clojure.java.io :as io]
         '[cognitect.transit :as transit]
         '[org.httpkit.server :as srv]
         '[hiccup.core :as hp])

(import 'java.io.ByteArrayOutputStream)

(def port 8083)

(defn html [cljs-file]
  (hp/html
    [:html
     [:head
      [:meta {:charset "UTF-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:link {:rel "shortcut icon" :href "data:,"}]
      [:link {:rel "apple-touch-icon" :href "data:,"}]
      [:script {:crossorigin nil :src "https://unpkg.com/react@17/umd/react.production.min.js"}]
      [:script {:crossorigin nil :src "https://unpkg.com/react-dom@17/umd/react-dom.production.min.js"}]
      [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.js" :type "application/javascript"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js" :type "application/javascript"}]
      [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/jsdiff/4.0.2/diff.min.js" :type "application/javascript"}]
      [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.reagent.js" :type "application/javascript"}]
      [:script {:src "https://cdn.jsdelivr.net/gh/borkdude/scittle@0.0.1/js/scittle.cljs-ajax.js" :type "application/javascript"}]
      [:title "Guestbook"]]
     [:body
      [:div {:id "content"}]
      [:script {:type "application/x-scittle" :src cljs-file}]]]))


(defn home-page [_request cljs-file]
  (html cljs-file))

(defn home-routes [{:keys [:request-method :uri] :as req}]
  (case [request-method uri]
    [:get "/"] {:body (home-page req "core.cljs")
                :status 200}
    [:get "/core.cljs"] {:body (slurp"core.cljs")
                             :status 200}))

(defn core-http-server []
  (srv/run-server home-routes {:port port}))

(let [url (str "http://localhost:" port "/")]
  (core-http-server)
  (println "serving" url)
  ;; (browse/browse-url url)
  @(promise))