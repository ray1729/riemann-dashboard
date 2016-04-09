(ns riemann-dashboard.core
  (:require [om.core :as om :include-macros true]
            [riemann-dashboard.transport :as t]
            [sablono.core :as html :refer-macros [html]]
            [cemerick.url :refer [url]]))

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

(defn reset-websocket!
  [ws-endpoint riemann-query]
  (when-let [ws (:ws @app-state)]
    (.close ws))
  (let [ws-url (assoc (url (str "ws://" ws-endpoint "/index"))
                      :query {:query riemann-query :subscribe true})
        new-state {:ws-endpoint ws-endpoint
                   :riemann-query riemann-query
                   :hosts {}
                   :ws (t/connect ws-url :on-message handle-message)}]
    (reset! app-state new-state)))

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
  [hosts owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div.row
        (om/build-all host-view (sort-by key hosts))]))))

(defn handle-change
  [owner k]
  (fn [e]
    (om/set-state! owner k (.. e -target -value))))

(defn query-form
  [data owner]
  (reify
    om/IInitState
    (init-state [this]
      {:ws-endpoint (:ws-endpoint data)
       :riemann-query (:riemann-query data)})
    om/IRenderState
    (render-state [this state]
      (html
       [:div.row
        [:form.form-horizontal
         [:div.form-group
          [:label.col-sm-4.control-label {:for "websocket-host"} "Websocket Endpoint"]
          [:div.col-sm-6
           [:input#websocket-host.form-control
            {:type "text"
             :size 80
             :value (:ws-endpoint state)
             :on-change (handle-change owner :ws-endpoint)}]]]
         [:div.form-group
          [:label.col-sm-4.control-label {:for "riemann-query"} "Query"]
          [:div.col-sm-6
           [:textarea#riemann-query.form-control
            {:rows 2
             :cols 80
             :value (:riemann-query state)
             :on-change (handle-change owner :riemann-query)}]]]
         [:div.form-group
          [:div.col-sm-offset-4.col-sm-6
           [:button.btn.btn-default
            {:type "submit"
             :on-click (fn [e]
                         (reset-websocket! (:ws-endpoint state)
                                           (:riemann-query state))
                         (.preventDefault e))}
            "Go"]]]]]))))

(defn app-view
  [data owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div.container
        (om/build query-form data)
        (om/build hosts-view (:hosts data))]))))

(reset-websocket! "127.0.0.1:5556" "true")

(om/root
  app-view
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
