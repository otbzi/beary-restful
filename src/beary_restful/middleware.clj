(ns beary-restful.middleware
  (:use [beary-restufl.core])
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :as log]))

(defn- json-request?
  [req]
  (if-let [#^String type (:content-type req)]
    (not (empty? (re-find #"^application/(vnd.+)?json" type)))))

(defn wrap-request-logging
  "request logging"
  [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [start (System/currentTimeMillis)
          resp (handler req)
          finish (System/currentTimeMillis)]
      (log/info (name request-method) (:status resp)
            (if-let [qs (:query-string req)]
              (str uri "?" qs) uri)
            (str (- finish start) "ms"))
      resp)))

;;json params middleware
(defn wrap-json-params [handler]
  (fn [req]
    (if-let [body (and (json-request? req) (:body req))]
      (let [bstr (slurp body)]
        (if (not-empty bstr)
          (let [json-params (json/read-str bstr :key-fn keyword)
                req* (assoc req
                       :json-params json-params
                       :params (merge (:params req) json-params))]
            (handler req*))
          (handler req)))
      (handler req))))

(defn wrap-error-handling
  "wrap error"
  [handler]
  (fn [req]
    (try
      (or (handler req)
          (fail 404 failure-code "resource not found."))
      (catch Exception e
        (let [error (.getMessage e)]
          (log/error e "Handle request failed")
          (fail 500 internal-error-code error))))))
