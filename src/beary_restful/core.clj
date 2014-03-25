(ns beary-restful.core
  (:use [environ.core])
  (:require [clojure.data.json :as json])

(defonce ^:const success-code 0)
(defonce ^:const unauthorized-code 1)
(defonce ^:const internal-error-code 2)
(defonce ^:const failure-code 3)

;; FIXME: better config
(defonce in-production (Boolean/valueOf ^String (env :beary-production "true")))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json;charset=utf-8"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"}
   :body (json/write-str data)})

(defn success
  "Render a success json response with result."
  ([& [result]]
     (json-response {:code success-code :result result})))

(defn fail
  "Render a failure json response with reason."
  ([reason]
     (fail failure-code reason))
  ([code reason]
     (fail 500 code reason))
  ([status code reason]
     (json-response {:code code :error reason} status)))

(defmacro defhandler0
  [name args & body]
  `(defn ~name [req#]
     (let [{:keys ~args :or {~'req req#}} (:params req#)
           ~'uid (linedesigner-api.utils/->int (:uid req#))]
       ~@body)))

(defmacro def-signin-handler0 [name args & body]
  "Define signined handler."
  (let [args-set (apply hash-set (map keyword args))]
    `(defn ~name [req#]
       (if (or (not-empty (:uid req#)) (not in-production))
         (do
           (let [{:keys ~args :or {~'req req#}} (:params req#)
                 ~'uid (linedesigner-api.utils/->int (:uid req#))]
             (do ~@body)))
         (fail 403 unauthorized-code "User doesn't sign in.")))))

(defmacro check-params
  [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (if (next (next clauses))
              (fail (second clauses))
              (second clauses))
            (throw (IllegalArgumentException.
                    "check-params requires an even number of forms")))
          (cons 'linedesigner-api.handler.helper/check-params (next (next clauses))))))

(defn has-pre? [fdecl]
  (and (map? (first fdecl)) (:pre (first fdecl))))

(def create-handler (fn create-handler [&form &env signin? name & fdecl]
                      (let [m (if (string? (first fdecl))
                                {:doc (first fdecl)}
                                {})
                            fdecl (if (string? (first fdecl))
                                    (next fdecl)
                                    fdecl)
                            m (if (map? (first fdecl))
                                (conj m (first fdecl))
                                m)
                            m (conj (if (meta name) (meta name) {}) m)
                            fdecl (if (map? (first fdecl))
                                    (next fdecl)
                                    fdecl)
                            args (if (vector? (first fdecl))
                                   (first fdecl)
                                   [])
                            fdecl (if (vector? (first fdecl))
                                    (next fdecl)
                                    fdecl)
                            pre (when (has-pre? fdecl)
                                  (:pre (first fdecl)))
                            fdecl (if (has-pre? fdecl)
                                    (next fdecl)
                                    fdecl)
                            handler (if signin?
                                      `linedesigner-api.handler.helper/def-signin-handler0
                                      `linedesigner-api.handler.helper/defhandler0)]
                        (if pre
                          (list handler (with-meta name m) args (concat (cons `linedesigner-api.handler.helper/check-params
                                                                              pre)
                                                                        (list :else (cons 'do fdecl))))
                          (list* handler (with-meta name m) args fdecl)))))

(.setMacro (var create-handler))

(defmacro defhandler
  [name & fdecl]
  `(linedesigner-api.handler.helper/create-handler false ~name ~@fdecl))

(defmacro def-signin-handler
  [name & fdecl]
  `(linedesigner-api.handler.helper/create-handler true ~name ~@fdecl))
