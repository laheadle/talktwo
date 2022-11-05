(ns talktwo.core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [clojure.string :as string]
            ;; [cljs.pprint :refer [pprint]]
            ))

(defn init [{:keys [steps self]
             :or {self :starter
                  steps []}}]
  {:self self
   :steps steps
   :situation (if (#{:audience} self)
                :done
                (if (< (count steps) 3)
                  (count steps)
                  [3 self (if (= (get steps 0)
                                 (get steps 2))
                            :steady
                            :changed)]))
   :current-step (if (< (count steps) 2)
                   {:name ""
                    :body ""}
                   (get steps 1))
   :state {:form-fields :not-focused}
   :max {:name 80
         :body 600}})


(def focused-states #{:name-focused :body-focused})

(defn read-url []
  (let [search (.. js/window -location -search)
        compressed (when (and search (> (. search -length) 0))
                     (. search substring 3))
        decompressed (and compressed (. js/LZString decompressFromEncodedURIComponent compressed))
        result (when decompressed
                 (read-string decompressed))]
    (prn (str "url " result))
    result))

(defn create-URL [state]
  (prn (str "encoding: " state))
  (str (.. js/window -location -protocol)
       "//"
       (.. js/window -location -host)
       ;; "/"
       (.. js/window -location -pathname)
       "?q="
       ;; (. js/window encodeURI (pr-str state))
       (. js/LZString compressToEncodedURIComponent (pr-str state))))

(defn in-state [world outer inner]
  (let [current-state (get-in world [:state outer])]
    (cond (#{:focused} inner) (focused-states current-state)
          :else (= current-state inner))))

(defn set-state [old outer inner]
  (. js/console log (str [:to outer inner]))
  (assoc-in old [:state outer] inner))

(defn focus-name! [world event]
  (when (in-state @world :form-fields :not-focused)
    (swap! world set-state :form-fields :name-focused)))

(defn focus-body! [world event]
  (when (in-state @world :form-fields :not-focused)
    (swap! world set-state :form-fields :body-focused)))

(defn set-input [world key event]
  (prn (assoc-in world [:current-step key] (-> event .-target .-value)))
  (assoc-in world [:current-step key] (-> event .-target .-value)))

(defn blur! [world]
  (when (in-state @world :form-fields :focused)
    (swap! world set-state :form-fields :not-focused)))

(defn get-input [world key]
  ;; (prn (get-in world [:current-step key]))
  (get-in world [:current-step key]))

(defn empty? [str]
  (or (not str)
      (= (. str -length) 0)))

(defn pending-item [text]
  [:div.pending-item text])

(defn pending-items [world]
  (cond-> []
    (empty? (get-input world :name))
    (conj "Give yourself a name")
    (empty? (get-input world :body))
    (conj "Give yourself a body")))

(defn pending [world]
  [:ol.pending
   (map
    (fn [item]
      [:li.pending-item {:key (hash item)} item])
    (pending-items world))])

(defn remaining [world key]
  (let [input-value (get-input world key)
        label (case key
                :body "Body"
                :name "What do we call you?")
        focused (in-state world :form-fields (case key
                                               :body :body-focused
                                               :name :name-focused))
        max (get-in world [:max key])]
    [:div
     [:span label]
     (when focused
       [:span (str " (Max Characters Remaining: "
                   (- max
                      (. input-value -length))
                   ")")])]))

(defn name-text [text]
  [:span.name text])

(defn pretty [world step label-header]
  (let [name (get-in world [:steps step :name])]
    [:div.pretty
     (label-header name)
     [:div.quoted
      [:div (get-in world [:steps step :body])]]]))

(defn get-changes [world previous-step step key]
  (prn (str "get-changes: "
            [world previous-step step key
             (get-in world [:steps previous-step key])
             (get-in world [:steps step key])]))
  (. js/Diff diffWords
     (get-in world [:steps previous-step key])
     (get-in world [:steps step key])))

(defn render-changes [changes key]
  [:div (map-indexed
         (fn [i change]
           [:span (merge {:key (str key i)}
                         (cond
                           (. change -added) {:class "added"}
                           (. change -removed) {:class "removed"}
                           :else {}))
            (. change -value)])
         changes)])

(defn diff [world previous-step step]
  (let [name-changes (get-changes world previous-step step :name)
        body-changes (get-changes world previous-step step :body)]
    [:div
     [render-changes name-changes :name]
     [render-changes body-changes :body]]))

(defn body! [world]
  [:div
   [remaining @world :body]
   [:textarea {:on-focus #(focus-body! world %)
               :on-blur #(blur! world)
               :value (get-input @world :body)
               :on-change #(swap! world set-input :body %)}]])

(defn name! [world]
  [:div
   [remaining @world :name]
   [:input {:on-focus #(focus-name! world %)
            :on-blur #(blur! world)
            :value (get-input @world :name)
            :on-change #(swap! world set-input :name %)}]])

(defn partner-is-steady [world]
  (= (get (:situation world) 2)
     :steady))

(defn self-is-steady [world]
  (= (:current-step world) (second (:steps world))))

(defn next-URL [world]
  "Transform the current world into the next turn state"
  (if (and (partner-is-steady world)
           (self-is-steady world))
    ;; ok, we are finished.
    ;; produce the final pair of steps: [starter finisher]
    {:self :audience
     :steps
     (if (= :starter (:self world))
       [(:current-step world) (first (:steps world))]
       [(first (:steps world)) (:current-step world)])}
    ;; Invite the partner to continue revising
    {:self (if (= :starter (:self world))
             :finisher
             :starter)
     :steps  (->> (concat [(:current-step world)]
                          (:steps world))
                  (take 3)
                  vec)}))

(defn submit []
  [:button#input_button {:type :submit} "Next"])

(defn form! [world]
  [:form
   {:on-submit (fn [e]
                 (. e preventDefault)
                 (.. js/navigator -clipboard
                     (writeText (create-URL (next-URL @world)))))}
   [name! world]
   [body! world]
   [pending @world]
   [submit]])

(defn done [world]
  [:div.done
   (pretty @world 0  #(vector :div [name-text %]))
   (pretty @world 1 #(vector :div [name-text %]))])


(defn preview [world] "preview")

(defn home []
  (let [world (r/atom (init (read-url)))]
    (prn (str "initializing: " @world))
    (fn []
      (case (:situation @world)
        0
        [:div.dialog
         "This is Talktwo, a dialog maker. Whip up a first draft, for your partner's eyes only."
         [form! world]]
        1
        [:div.dialog
         "This is Talktwo, a dialog maker, and you're in a dialog. To
         keep the ball rolling, whip up a rough response to the
         starter's first draft. You will have plenty of chances to
         revise it, share it, or dump it."
         [pretty @world 0
          #(vector :div "Here is what the starter, " [name-text %] ", wrote")]
         [form! world]
         [preview @world]]
        2
        [:div.dialog
         "Make your revisions, taking into account the finisher's rough response."
         [pretty @world 0 
          #(vector :div [name-text %])]
         [form! world]
         [preview @world]] 
        [3 :finisher :changed]
        [:div.dialog*
         "The starter made changes. You should probably make some
         too. Keep revising the dialog until you want to stop."
         [diff @world 2 0] ;; from to
         [form! world]
         [preview @world]]
        [3 :starter :changed]
        [:div.dialog
         "The finisher made changes. You should probably make some
         too. Keep revising the dialog until you want to stop."
         [diff @world 2 0] ;; from to
         [form! world]
         [preview @world]]
        [3 :finisher :steady]
        [:div.dialog
         "The starter is holding steady with no changes. If you do the same, the dialog is complete."
         [diff @world 2 0]
         [form! world]
         [preview @world]]
        [3 :starter :steady]
        [:div.dialog
         "The finisher is holding steady with no changes. If you do the same, the dialog is complete."
         [diff @world 2 0]
         [form! world]
         [preview @world]]
        :done
        [:div.dialog
         "This is Talktwo, a dialog maker. Here is a dialog. You can share the url."
         [done world]]))))

(dom/render [home] (.getElementById js/document "content"))


