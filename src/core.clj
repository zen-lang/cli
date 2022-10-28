(ns core
  (:gen-class)
  (:require [zen.package]
            [zen.changes]
            [zen.core]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string]
            [clojure.edn]
            [clojure.stacktrace]
            [clojure.java.shell]))


(defn str->edn [x]
  (clojure.edn/read-string (str x)))


(defn split-args-by-space [args-str]
  (map pr-str (clojure.edn/read-string (str \[ args-str \]))))


(defn apply-with-opts [f args opts]
  (apply f (conj (vec args) opts)))


(defn get-pwd [& [{:keys [pwd] :as opts}]]
  (or (some-> pwd (clojure.string/replace #"/+$" ""))
      (zen.package/pwd :silent true)))


(defn get-return-fn [& [opts]]
  (or (:return-fn opts) clojure.pprint/pprint))


(defn get-read-fn [& [opts]]
  (or (:read-fn opts) read-line))


(defn get-prompt-fn [& [opts]]
  (or (:prompt-fn opts)
      #(do (print "zen> ")
           (flush))))


(defn command-not-found-err-message [cmd-name available-commands]
  {:status :error
   :code :command-not-found
   :message (str "Command " cmd-name " not found. Available commands: " (clojure.string/join ", " available-commands))})


(defmacro exception->error-result [& body]
  `(try
     ~@body
     (catch Exception e#
       {:status    :error
        :code      :exception
        :message   (.getMessage e#)
        :exception (Throwable->map e#)})))


(defn repl [commands & [opts]]
  (let [prompt-fn (get-prompt-fn opts)
        read-fn   (get-read-fn opts)
        return-fn (get-return-fn opts)

        opts (update opts :stop-repl-atom #(or % (atom false)))]
    (while (not @(:stop-repl-atom opts))
      (return-fn
        (exception->error-result
          (prompt-fn)
          (let [line              (read-fn)
                [cmd-name rest-s] (clojure.string/split line #" " 2)
                args              (split-args-by-space rest-s)]
            (if-let [cmd-fn (get commands cmd-name)]
              (apply-with-opts cmd-fn args opts)
              (command-not-found-err-message cmd-name (keys commands)))))))))


(defn cmd-unsafe [commands cmd-name args & [opts]]
  (if-let [cmd-fn (get commands cmd-name)]
    (apply-with-opts cmd-fn args opts)
    (command-not-found-err-message cmd-name (keys commands))))


(defn cmd [& args]
  (exception->error-result
    (apply cmd-unsafe args)))

(def commands
  {"echo"       (fn [& args] args)})

(defn -main [& [cmd-name & args]]
  (if (some? cmd-name)
    ((get-return-fn) (cmd commands cmd-name args))
    (repl commands))
  (System/exit 0))
