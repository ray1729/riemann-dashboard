(ns riemann-dashboard.transport
  (:require [clojure.browser.net :as net]
            [clojure.browser.event :as event]
            [cognitect.transit :as transit]))

(def json-reader (transit/reader :json))

(defn parse-message
  [evt]
  (when-let [message (.-message evt)]
    (transit/read json-reader message)))

(defn connect
  [ws-url & {:keys [on-opened on-message on-closed on-error]
             :or {on-opened  (fn [evt] (.info js/console "Opened Websocket connection"))
                  on-message (fn [evt] (.info js/console "Received message" evt))
                  on-closed  (fn [evt] (.info js/console "Closed Websocket connection"))
                  on-error   (fn [evt] (.error js/console "Websocket error" evt))}}]
  (let [conn (net/websocket-connection true)]
    (event/listen conn :opened  on-opened)
    (event/listen conn :message on-message)
    (event/listen conn :closed  on-closed)
    (event/listen conn :error   on-error)
    (net/connect conn ws-url)
    conn))
