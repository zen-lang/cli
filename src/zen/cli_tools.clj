(ns zen.cli-tools
  (:require [core :as c]
            [zen.core :as zen]))

(def ztx (zen/new-context {:unsafe true}))

(defmulti command (fn [command-name _args] command-name))

(comment

  (def project-namespaces (c/collect-all-project-namespaces {}))

  (zen/read-ns ztx 'zen.cli-tools)

  )

(defn get-config
  []
  (let [project-namespaces (c/collect-all-project-namespaces {})
        _ (doseq [ns project-namespaces]
            (zen/read-ns ztx (symbol ns)))]))

(defn parse
  []
  "parsed")

(defn cli-exec [config & args]
  (let [{:keys [command-name command-args]} (apply parse args)]
    (command command-name)))


(defn cli-repl [config])
