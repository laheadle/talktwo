(ns talktwo.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [ajax.core :refer [GET POST]]
            [clojure.string :as string]
            ;; [cljs.pprint :refer [pprint]]
            ))



(defn init []
  {:state {:form-fields :not-focused
           :step :initiator-first}
   :elements {}
   :current-step 0
   :steps [{:name "a"
            :header "b"
            :body "bc"}]
   :errors {:name "I failed"
            :header "I failed"
            :body "I failed"}})


;; this is not really executable

(def states
  #{[:and :dialog
     #{[:or :form-fields
        #{:not-focused :name-focused :header-focused :body-focused}]
       [:or :step
        #{:initiator-first
          :responder-first
          :initiator-second
          :responder-second}]}]})

(def focused-states #{:name-focused :header-focused :body-focused})

(defn read-url []
  (let [search (.. js/window -location -search)
        compressed (when (and search (> (. search -length) 0))
                      (. search substring 3))
        decompressed (and compressed (. js/LZString decompressFromEncodedURIComponent compressed))]
    (prn (if decompressed
           (read-string decompressed)
           "no url"))))

(defn write-url [state]
  (str (.. js/window -location -protocol)
       "//"
       (.. js/window -location -host)
       ;; "/"
       (.. js/window -location -pathname)
       "?q="
       ;; (. js/window encodeURI (pr-str state))
       (. js/LZString compressToEncodedURIComponent (pr-str state))))

(defn error [world path]
  [:div.error  
   (get-in @world [:errors path])])

(defn in-state [world outer inner]
  (let [current-state (get-in @world [:state outer])]
    (cond (#{:focused} inner) (focused-states current-state)
          :else (= current-state inner))))

(defn set-state [old outer inner]
  (. js/console log (str [:to outer inner]))
  (assoc-in old [:state outer] inner))

(defn focus-name! [world event]
  (when (in-state world :form-fields :not-focused)
    (swap! world set-state :form-fields :name-focused)))

(defn set-element [old outer inner]
  (assoc-in old [:elements outer] inner))

(defn focus-header! [world event]
  (. js/console log "abct")
  (when (in-state world :form-fields :not-focused)
    (swap! world set-state :form-fields :header-focused)))

(defn focus-body! [world event]
  (when (in-state world :form-fields :not-focused)
    (swap! world set-state :form-fields :body-focused)))

(defn blur! [world]
  (when (in-state world :form-fields :focused)
    (swap! world set-state :form-fields :not-focused)))

(defn set-input [world key event]
  (assoc-in world [:steps (:current-step world) key] (-> event .-target .-value)))

(defn get-input [world key]
  (get-in world [:steps (:current-step world) key]))

(defn body [world]
  [:div
   (error world :body)
   [:textarea {:on-focus #(focus-body! world %)
               :on-blur #(blur! world)
               :value (get-input @world :body)
               :on-change #(swap! world set-input :body %)}]])

(defn header [world]
  [:div
   (error world :header)
   [:input {:on-focus #(focus-header! world %)
            :on-blur #(blur! world)
            :value (get-input @world :header)
            :on-change #(swap! world set-input :header %)}]])

(defn name [world]
  [:div
   (error world :name)
   [:input {:on-focus #(focus-name! world %)
            :on-blur #(blur! world)
            :value (get-input @world :name)
            :on-change #(swap! world set-input :name %)}]])

(defn form [world]
  [:form
   {:on-submit (fn [e]
                 (. e preventDefault)
                 (.. js/navigator -clipboard (writeText "that really worked")))}
   [name world]
   [header world]
   [body world]
   [:button {:type :submit} "nextt"]])

(defn home []
  (let [world (r/atom (init))]
    (fn []
      [:div.dialog
       [:div.form
        [form world]]])))

(prn (:a {:a 1} 2))

(dom/render [home] (.getElementById js/document "content"))


