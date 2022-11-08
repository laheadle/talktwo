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
   :situation (if (< (count steps) 3)
                (count steps)
                [3 self (if (= (get steps 0)
                               (get steps 2))
                          :steady
                          :changed)])
   :current-step (if (< (count steps) 2)
                   {:name ""
                    :body ""}
                   (get steps 1))
   :state {:dialog-state
           (cond
             (= [self steps] [:starter []])
             :about
             (#{:audience} self)
             :viewing
             :else
             :not-focused)}
   :max {:name 80
         :body 600}})


(def focused-states #{:name-focused :body-focused})
(def revising-states (clojure.set/union focused-states #{:not-focused}))

(defn empty? [str]
  (or (not str)
      (= str "")))

(defn read-url []
  (let [payload (.. js/window -location -hash)
        ;; _ (prn "pay" payload)
        compressed (when (and payload (> (. payload -length) 0))
                     (. payload substring 3))
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
       "#"
       "00"
       ;; (. js/window encodeURI (pr-str state))
       (. js/LZString compressToEncodedURIComponent (pr-str state))))

(defn in-state [world outer inner]
  (let [current-state (get-in world [:state outer])]
    (cond (#{:focused} inner) (focused-states current-state)
          (#{:revising} inner) (revising-states current-state)
          :else (= current-state inner))))

(defn set-state [old outer inner]
  (. js/console log (str [:to outer inner]))
  (assoc-in old [:state outer] inner))

(defn set-state! [world outer inner]
  (swap! world set-state outer inner))

(defn focus-name! [world event]
  (when (in-state @world :dialog-state :not-focused)
    (swap! world set-state :dialog-state :name-focused)))

(defn focus-body! [world event]
  (when (in-state @world :dialog-state :not-focused)
    (swap! world set-state :dialog-state :body-focused)))

(defn set-input [world key event]
  (prn (assoc-in world [:current-step key] (-> event .-target .-value)))
  (assoc-in world [:current-step key] (-> event .-target .-value)))

(defn blur! [world]
  (when (in-state @world :dialog-state :focused)
    (swap! world set-state :dialog-state :not-focused)))

(defn get-input [world key]
  (prn (get-in world [:current-step key])
       [:current-step key])
  (get-in world [:current-step key]))

(defn pending-item [text]
  [:div.pending-item text])

(defn too-long? [world key]
  (< (get-in world [:max key])
     (.-length (get-input world key))))

(defn pending-items [world]
  (cond-> []
    (empty? (get-input world :name))
    (conj "Give yourself a name")
    (empty? (get-input world :body))
    (conj "Give yourself a body")
    (too-long? world :name)
    (conj "Shrink your name - it is too big")
    (too-long? world :body)
    (conj "Shrink your body - it is too big")))

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
        focused (in-state world :dialog-state (case key
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

(defn body-text [body]
  [:div.body body])

(defn get--or-placeholder [world step key placeholder]
  (let [it (get-in world [:steps step key])
        it (if (empty? it)
             placeholder
             it)]
    it))

(defn get-name-or-placeholder [world step]
  (get--or-placeholder world step :name "(no name yet)"))

(defn get-body-or-placeholder [world step]
  (get--or-placeholder world step :body "(no body yet)"))

(defn pretty [world step label-header]
  (let [name (get-name-or-placeholder world step)
        body (get-body-or-placeholder world step)]
    [:div.pretty
     (label-header name)
     [body-text body]]))

(defn get-changes [world previous-step step key]
  (prn (str "get-changes: "
            [world previous-step step key
             (get-in world [:steps previous-step key])
             (get-in world [:steps step key])]))
  (. js/Diff diffSentences
     (get-in world [:steps previous-step key])
     (get-in world [:steps step key])))

(defn revision-changes [changes key]
  (map-indexed
   (fn [i change]
     [:span.word (merge {:key (str key i)}
                        (cond
                          (. change -added) {:class "added"}
                          (. change -removed) {:class "removed"}
                          :else {}))
      (. change -value)])
   changes))

(defn diff [world previous-step step]
  (let [name-changes (get-changes world previous-step step :name)
        body-changes (get-changes world previous-step step :body)]
    [:div
     [name-text (revision-changes name-changes :name)]
     [body-text (revision-changes body-changes :body)]]))

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

(defn finalize-world [world]
  "Produce the final pair of steps: [starter finisher]"
  {:self :audience
   :steps
   (if (= :starter (:self world))
     [(:current-step world) (first (:steps world))]
     [(first (:steps world)) (:current-step world)])})

(defn dialog-is-complete [world]
  (and (partner-is-steady world)
       (self-is-steady world)))

(defn next-world [world]
  "Transform the current world into the next turn state"
  (if (dialog-is-complete world)
    ;; ok, we are finished.
    (finalize-world world)
    ;; Invite the partner to continue revising
    {:self (if (= :starter (:self world))
             :finisher
             :starter)
     :steps  (->> (concat [(:current-step world)]
                          (:steps world))
                  (take 3)
                  vec)}))

(defn submit [text disable]
  [:button#input_button {:type :submit
                         :disabled disable} text])

(defn form! [world]
  [:form
   {:on-submit (fn [e]
                 (. e preventDefault)
                 (.. js/navigator -clipboard
                     (writeText (create-URL (next-world @world))))
                 (swap! world set-state :dialog-state :previewing))}
   [name! world]
   [body! world]
   [pending @world]
   [submit "Preview" (boolean (< 0
                                 (count (pending-items @world))))]])

(defn dialog-header [world starter finisher]
  (let [finisher-name (get-name-or-placeholder world finisher)
        starter-name (get-name-or-placeholder world starter)]
    [:div.dialog-header
     [:div.dialog-header-top
      "An Imaginary Dialog Between"]
     [:div.dialog-header-starter
      [name-text starter-name]]
     [:div.dialog-header-and
      "and"]
     [:div.dialog-header-finisher
      [name-text finisher-name]]]))

(defn dialog-body [world]
  [:div.dialog-body
   [dialog-header world 0 1]
   [pretty world 0  #(vector :div [name-text %])]
   [pretty world 1 #(vector :div [name-text %])]])

(defn preview! [world]
  [:div.app-body
   [dialog-body (finalize-world @world)]
   [:div.current-turn-status
    (if (self-is-steady @world)
      (if (dialog-is-complete @world)
        "You are ready to complete the dialogue."
        "You are holding steady with no changes, so your partner can
  complete the dialog.")
      "You have made changes, so you will get another turn after your
  partner's.")]
   [:div.call-to-send
    (if (dialog-is-complete @world)
      "Is the dialog complete? A link to it is copied to your
  clipboard. Send it to your partner!"
      "Are you done with your turn? A link to your partner's next turn is copied to your
  clipboard. Send it to your partner! Tip: send it over email or social media.")]
   [:div.see-next-turn
    "(Click to see "
    [:a {:target "_blank"
         :href (create-URL (next-world @world))}
     (if (dialog-is-complete @world)
       "the final dialog"
       "your partner's next turn")] ".)"]
   [:form
    {:on-submit (fn [e]
                  (. e preventDefault)
                  (swap! world set-state :dialog-state :not-focused))}
    [submit "Hide Preview" false]]])

(defn top-of-dialog [text]
  [:div.top-of-dialog text])

(defn revise! [world]
  (prn (:situation @world) "(:situation @world)")
  (case (:situation @world)
    0
    [:div.app-body
     [top-of-dialog "Whip up a first draft of the starter's paragraph, for your partner's eyes only."]
     [form! world]]
    1
    [:div.app-body
     [top-of-dialog [:span "Heads up! You're in a dialog started by "
                     [name-text (get-in @world [:steps 0 :name])]
                     ". To keep the ball rolling, whip up a rough
         response. You will have plenty of chances to revise it, share
         it, or dump it."]]
     [pretty @world 0
      #(vector :div "Here is what the starter, " [name-text %] ", wrote:")]
     [form! world]]
    2
    [:div.app-body
     [top-of-dialog "Make your revisions, taking into account the finisher's rough response."]
     [pretty @world 0 
      #(vector :div [name-text %])]
     [form! world]] 
    [3 :finisher :changed]
    [:div.app-body
     [top-of-dialog "The starter made changes. You should probably make some
         too. Keep revising the dialog until you want to stop."]
     [diff @world 2 0] ;; from to
     [form! world]]
    [3 :starter :changed]
    [:div.app-body
     [top-of-dialog "The finisher made changes. You should probably make some
         too. Keep revising the dialog until you want to stop."]
     [diff @world 2 0] ;; from to
     [form! world]]
    [3 :finisher :steady]
    [:div.app-body
     [top-of-dialog "The starter is holding steady with no changes. If you do the same, the dialog is complete."]
     [diff @world 2 0]
     [form! world]]
    [3 :starter :steady]
    [:div.app-body
     [top-of-dialog "The finisher is holding steady with no changes. If you do the same, the dialog is complete."]
     [diff @world 2 0]
     [form! world]]))

(defn logo! [world]
  [:a {:on-click (fn [e]
                   (set-state! world
                    :previous-dialog-state
                    (get-in @world [:state :dialog-state]))
                   (set-state! world :dialog-state :about))
       :style {:display "inline-block" :border "4px solid pink"
               :margin-right 5}}
   [:span {:style {:display "inline-block" :border "4px solid aqua"}}
    [:span.logo-text {:style {:display "inline-block" :padding "4px 6px"}} "T / t"]]])

(defn slogan! [world] [:span.slogan "Talktwo"])

(defn app! [world screen]
  [:div.app
   [:div.app-header
    [logo! world]
    [slogan! world]]
   screen])

(defn about! [world]
  [:div.about
   [:h1 "Talktwo: A Game of Infinite Dialog"]
   [:p "Talktwo is a game for two players that is dialog all the way down. The object of the game is to create a dialog through dialog."]
   [:p "By playing Talktwo, players create a two-paragraph dialog between two characters, a starter and a finisher. The starter's paragraph is shown first, and the finisher's paragraph is a response. They can share the dialog, or keep it to themselves."]
   [:h3 "Playing the game"]
   [:p " One player,who plays the starter, takes the first turn (and the third,
   and the fifth...), and builds up the first paragraph.  The other
   player, the finisher, takes the second turn (and the fourth, and
   the sixth...) and builds up the second paragraph."]
   [:p "During each turn, one player modifies their character's name or paragraph. When both players are happy with the dialog, they both win, and get a link they can use to share their dialog with anyone they choose. If the dialog stops before both players are happy with it, nobody wins."]
   (let [next-state (get-in @world [:state
                                    :previous-dialog-state])
         next-state-label (if next-state "Go Back to Game"
                              "Play New Game")
         next-state (or next-state :not-focused)]
     [:form {:on-submit
             (fn [e] (. e preventDefault)
               (set-state! world :dialog-state
                           next-state))}
      [submit next-state-label false]])])

(defn home []
  (let [world (r/atom (init (read-url)))]
    (prn (str "initializing: " @world))
    (fn [] (prn @world "@world")
      (prn "(in-state @world :dialog-state :revising)" (in-state @world :dialog-state :revising))
      [app! world
       (cond
         (in-state @world :dialog-state :revising) (revise! world)
         (in-state @world :dialog-state :about) (about! world)
         (in-state @world :dialog-state :previewing) (preview! world)
         (in-state @world :dialog-state :viewing)
         [:div.app-body
          [dialog-body @world]])])))

(dom/render [home] (.getElementById js/document "content"))


