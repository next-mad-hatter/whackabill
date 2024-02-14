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


(defn parse-input! [file set-data! set-errors!]
  (ppp/parse file #js{:header         true
                      :skipEmptyLines true
                      :complete       #(let [r (js->clj %)]
                                         (-> r (get "data") set-data!)
                                         (-> r (get "errors") set-errors!))}))


(defui file-uploader [{:keys [set-data! set-errors!]}]
  (let [[label set-label!] (uix/use-state "Man gebe mir Daten (csv mit Kopf)")]
    ($ ddf/FileUploader
       {:name          "file"
        :multiple      false
        :handle-change #(do (set-label! (or (.-name %) "Datei ohne Namen"))
                            (parse-input! % set-data! set-errors!))}
       ($ :div.border-2.border-dashed.rounded-lg.hover:bg-cyan-800
          ($ :p.p-3 label)))))


(defui errors-view [{:keys [errors]}]
  (when (seq errors)
    ($ :div.border-2.border-orange-600.rounded-lg
       ($ :h3.text-orange-600.p-3
          "Oh nein, Datensalat am Eingang!")
       (.log js/console "Errors" (clj->js errors)))))


(defn filter-data [columns data]
  (->> data
       (map #(select-keys % columns))
       (map #(filter
              (fn [[_ v]] (-> v str/trim not-empty))
              %))
       (map #(into {} %))
       (remove empty?)))


(defui save-btn [{:keys [val disabled]}]
  ($ :button.export.border-2.border-lime-500.text-lime-500.rounded-lg.p-2.disabled:border-stone-500.disabled:text-stone-500.hover:enabled:text-cyan-900.hover:enabled:bg-lime-600
     {:on-click #(fs/saveAs (js/Blob. [val]) "results.csv")
      :disabled disabled}
     "Speichern"))


(defui draggable-columns [{:keys [columns]}]
  ($ dnd/Droppable
     {:droppable-id "droppable-move"}
     (fn [x _]
       ($ :div
          (merge (js->clj (.-droppableProps x))
                 {:ref (.-innerRef x)})
          (map-indexed (fn [idx col]
                         ($ dnd/Draggable {:key col :draggable-id col :index idx}
                            (fn [x y]
                              ($ :div.th.csv-column
                                 (merge (js->clj (.-draggableProps x))
                                        (js->clj (.-dragHandleProps x))
                                        {:ref   (.-innerRef x)
                                         :key   col
                                         :class (when (.-isDragging y) "dragged")
                                         :style (.. x -draggableProps -style)})
                                 ($ :div col))))) columns)
          ($ :div.placeholder (.-placeholder x))))))


(defn apply-dnd-action! [columns set-columns! drag]
  (when (.. drag -destination)
    (-> (case (.. drag -destination -droppableId)
          "droppable-move"   (let [i (.. drag -source -index)
                                   j (.. drag -destination -index)]
                               (vec-move columns i j))
          "droppable-delete" (->> (.. drag -source -index)
                                  (vec-remove columns)))
        set-columns!)))


(defui col-input [{:keys [editing set-editing! input-ref columns set-columns! available-columns]}]
  ($ :div
     {:class-name (when (not editing) "hidden")}
     ($ :span.blink ">" )
     (let [valid-columns                  (clj->js  (set/difference
                                                     (set available-columns)
                                                     (set columns)))
           [suggestions set-suggestions!] (uix/use-state [])
           [input-value set-input-value!] (uix/use-state "")
           query->suggestions             (fn [q]
                                            (let [q  (-> q str/lower-case str/trim)
                                                  cs (filter #(-> %
                                                                  str/lower-case
                                                                  (str/starts-with? q))
                                                             valid-columns)]
                                              (->> cs
                                                   (take 42)
                                                   vec)))]
       ($ pra/AutoComplete
             {:complete-method (fn [^js e] (-> (.-query e)
                                               query->suggestions
                                               set-suggestions!))
              :suggestions     (clj->js suggestions)
              :ref             input-ref
              :type            "text"
              :id              "new-column"
              :auto-focus      true
              :value           input-value
              :on-change       (fn [^js e]
                                 (set-input-value! (.. e -target -value)))
              :on-key-down     (fn [^js e]
                                 (case (.-key e)
                                   "Tab"    (do (.preventDefault e)
                                                (-> input-value
                                                    query->suggestions
                                                    set-suggestions!)
                                                (when (seq suggestions)
                                                  (set-input-value! (first suggestions))))
                                   "Escape" (do (set-input-value! "")
                                                (set-suggestions! [])
                                                (set-editing! false))
                                   "Enter"  (let [new-val (str/trim input-value)]
                                              (when (and (seq new-val)
                                                         (not (some #{new-val} columns)))
                                                (set-columns! (conj columns new-val)))
                                              (set-input-value! "")
                                              (set-suggestions! [])
                                              (set-editing! false))
                                   :nothing))}))))


(defui columns-view [{:keys [columns set-columns! available-columns]}]
  (let [[editing set-editing!] (uix/use-state false)]
    ($ dnd/DragDropContext
       {:on-drag-end (partial apply-dnd-action! columns set-columns!)}
       ($ :div.tr
          ($ draggable-columns {:columns columns})
          (let [input-ref (uix/use-ref)]
            ($ :div.actions
               ($ col-input {:input-ref         input-ref
                             :editing           editing
                             :set-editing!      set-editing!
                             :columns           columns
                             :set-columns!      set-columns!
                             :available-columns available-columns})

               ($ :div
                  {:class-name (when editing "hidden")}
                  ($ :button.add.border-2.rounded-lg
                     {:title    "Neue Datenspalte"
                      :on-click (fn []
                                  (set-editing! true)
                                  ;; FIXME: use effect?
                                  #_(.focus @input-ref)
                                  (run!
                                   #(js/setTimeout (fn [] (.focus @input-ref)) %)
                                   [10 100]))}
                     ($ :svg {:data-src "images/list-add.svg"})))))
          ($ :div
             {:class-name (if editing "hidden" "actions")}
             ($ dnd/Droppable
                {:droppable-id "droppable-delete"}
                (fn [x y]
                  ($ :button
                     (merge (js->clj (.-droppableProps x))
                            {:title      "Datenspalte löschen -- hierher ziehen"
                             :ref        (.-innerRef x)
                             :class-name (str/join " " ["delete border-2 rounded-lg border-dashed"
                                                        (if (.-isDraggingOver y) "dragged-over" "")])})
                     ($ :span
                        ($ :svg {:data-src "images/user-trash.svg"})
                        ($ :div.trash-placeholder (.-placeholder x)))))))))))


(defn pad-vals [columns data]
  (letfn [(pad-str [s]
            (let [s ((fnil str/trim "") s)]
              (if (seq s) s ($ :span (clj->js {:dangerouslySetInnerHTML {:__html "&nbsp;"}})))))
          (col-vals [m]
            (reduce (fn [acc col] (assoc acc col (-> m (get col) pad-str))) {} columns))]
    (map col-vals data)))


(defui data-view [{:keys [data errors columns set-columns!]}]
  (let [filtered-data     (filter-data columns data)
        display-data      (pad-vals columns filtered-data)
        available-columns (->> data (mapcat keys) (map str/trim) (remove empty?) distinct vec)]
    (when (and (seq data) (empty? errors))
      ($ :div
         ($ :div.p-2.border-2.rounded-lg
            ($ prt/DataTable {:value      (clj->js display-data)
                              :class-name "rtable--flip"
                              :paginator  true
                              :rows       6
                              ;; :rows-per-page-options (clj->js [5 10 50])
                              :header     ($ columns-view {:columns           columns
                                                           :set-columns!      set-columns!
                                                           :available-columns available-columns})}
               (for [c columns] ($ prc/Column {:key c :field c :header c}))))
         ($ save-btn
            {:val      (when (seq columns)
                         (ppp/unparse (clj->js filtered-data)
                                      (clj->js {:delimiter ";" :columns columns})))
             :disabled (empty? filtered-data)})))))


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


(defui app []
  (let [[data set-data!]       (uix/use-state [])
        [errors set-errors!]   (uix/use-state [])
        [columns set-columns!] (use-persistent-state
                                "buchspalter/columns" ["Datum Beleg"
                                                       "Datum Buchung"
                                                       "Betrag EUR"
                                                       "Betrag Original"
                                                       "Währung"
                                                       "Händler"
                                                       "Nr"
                                                       "Beleg"
                                                       "Anmerkung"])]
    ($ :.flex.flex-col.justify-center.items-start
       ($ :p "Hello Everyone"))))


(defonce root (uix.dom/create-root (js/document.getElementById "root")))
(uix.dom/render-root ($ app) root)

