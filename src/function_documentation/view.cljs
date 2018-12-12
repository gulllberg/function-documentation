(ns function-documentation.view
  (:require [function-documentation.core :as core]))

(defn documentation-view [{state :state}]
  [:div
   (map-indexed (fn [index function]
                  [:div {:key index}
                   [:h3 (str (:name function) " - ")
                    [:span {:style {:color (if (= (:status function) "confirmed")
                                             "green"
                                             "red")}}
                     (:status function)]]
                   [:pre [:code (core/get-function-source (:file state) (:name function))]]])
                (get-in state [:documentation-data-clojure :data 0 :functions]))
   ])