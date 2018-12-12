(ns function-documentation.main
  (:require [reagent.core :as reagent :refer [atom]]
            [function-documentation.view :as view]
            [function-documentation.slurp :include-macros true :refer [slurp]]))


(enable-console-print!)

(def documentation-data-clojure (as-> (slurp "src/function_documentation/documentation-data.json") $
                            (.parse js/JSON $)
                            (js->clj $ :keywordize-keys true)))

(def documentation-data-raw (slurp "src/function_documentation/documentation-data.json"))

;; File path should be found in documentation-data
(def test-file (slurp "src/function_documentation/sherlock-state.cljc"))

(defonce state-atom (atom {:file test-file
                           :documentation-data-raw documentation-data-raw
                           :documentation-data-clojure documentation-data-clojure}))

(defn render!
  []
  (println "render")
  (reagent/render-component [view/documentation-view {:state    (deref state-atom)
                                                      :trigger-event (fn [{event :event data :data :as params}]
                                                                       (println event data))}]
                            (. js/document (getElementById "app"))))

(render!)

(add-watch state-atom :engine (fn [_ _ _ _]
                                (deref state-atom)
                                (render!)))


