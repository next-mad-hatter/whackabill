(ns whackabill.core
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            ["external-svg-loader" :as svg]))


(defn vec-remove [v i]
  (into (subvec v 0 i) (subvec v (inc i))))


(defn vec-move [v i j]
  (let [x (get v i)
        v (vec-remove v i)]
    (vec (concat (subvec v 0 j) [x] (subvec v j)))))


(defui errors-view [{:keys [errors]}]
  (when (seq errors)
    ($ :div.border-2.border-orange-600.rounded-lg
       ($ :h3.text-orange-600.p-3
          "Oh nein, Datensalat am Eingang!")
       (.log js/console "Errors" (clj->js errors)))))


(defn use-persistent-state
  [store-key initial-value]
  (let [[value set-value!] (uix/use-state initial-value)]
    (uix/use-effect #(when (js/localStorage.hasOwnProperty store-key)
                       (->> (js/localStorage.getItem store-key)
                            edn/read-string
                            set-value!))
                    [store-key])
    (uix/use-effect #(->> value
                          pr-str
                          (js/localStorage.setItem store-key))
                    [value store-key])
    [value set-value!]))


(defn rnd []
  (->
   (Math/random)
   (* 100)
   (Math/floor)))

(defui target-btn [{:keys [pos set-pos! score set-score!]}]
  (let [{:keys [x y]} pos]
    ($ :button.relative.export.border-2.border-lime-500.text-lime-500.rounded-full.p-3.hover:enabled:text-cyan-900.hover:enabled:bg-lime-600
       {:style {:top  (str x "%")
                :left (str y "%")}
        :on-click (fn [_]
                    (let [x' (rnd)
                          y' (rnd)]
                      (-> score inc set-score!)
                      (set-pos! {:x x' :y y'})
                      (.log js/console (str "From" x y))
                      (.log js/console (str "To" x' y'))))
        :disabled false}
       ($ :p (str score)))))


(defui app []
  (let [[pos set-pos!] (uix/use-state {:x 50 :y 50})
        [score set-score!] (uix/use-state 0)]
    ($ :div.h-screen.w-screen
       ($ target-btn {:pos        pos
                      :set-pos!   set-pos!
                      :score      score
                      :set-score! set-score!}))))


(defonce root (uix.dom/create-root (js/document.getElementById "root")))
(uix.dom/render-root ($ app) root)

