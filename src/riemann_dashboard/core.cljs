(ns riemann-dashboard.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [riemann-dashboard.transport :as t]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(defonce app-state (atom {}))

(defn delete-expired-services
  [services]
  (reduce (fn [services service]
            (if (= "expired" (get-in services [service "state"]))
              (dissoc services service)
              services))
          services
          (keys services)))

(defn clear-expired
  [hosts]
  (reduce (fn [hosts host]
            (let [services (delete-expired-services (hosts host))]
              (if (seq services)
                (assoc hosts host services)
                (dissoc hosts host))))
          hosts
          (keys hosts)))

(defn clear-expired!
  []
  (swap! app-state update :hosts clear-expired))

(defn handle-message
  [evt]
  (when-let [{:strs [host service state] :as event} (t/parse-message evt)]
    (swap! app-state assoc-in [:hosts host service] event)))

(def ws (t/connect "ws://localhost:5556/index?query=true&subscribe=true"
                   :on-message handle-message))

(defn state->class
  [state]
  (case state
    "ok" "success"
    "error" "warning"
    "critical" "danger"
    "expired" "info"
    ""))

(defn service-view
  [{:strs [service state metric time] :as event} owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:tr {:class (state->class state)}
        [:th service]
        [:td state]
        [:td (when metric (.toFixed metric 3))]
        [:td time]]))))

(defn host-view
  [[host services] owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div
        [:h1 host]
        [:table.table.table-hover
         [:tbody
          (om/build-all service-view
                        (map val (sort-by key services)))]]]))))

(defn hosts-view
  [data owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div
        (om/build-all host-view (sort-by key (:hosts data)))]))))

(om/root
  hosts-view
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
