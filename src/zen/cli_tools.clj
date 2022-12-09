(ns zen.cli-tools
  (:require [clojure.edn]
            [clojure.string]
            [zen.core]
            [clojure.java.io]
            [clojure.java.shell :as shell]))


(defn command-dispatch [command-name & _args]
  command-name)


(defmulti command #'command-dispatch :default ::not-found)


(defmethod command ::not-found [command-name _command-args] #_"TODO: return help"
  {::status :error
   ::code ::implementation-missing
   ::result {:message (str "Command '" command-name " implementation is missing")}})


(defn coerce-args-style-dispatch [command-args-def _command-args]
  (:args-style command-args-def :positional))


(defmulti coerce-args-style #'coerce-args-style-dispatch)


(defmethod coerce-args-style :named [_command-args-def command-args]
  (clojure.edn/read-string (str "{" (clojure.string/join " " command-args) "}")))


(defmethod coerce-args-style :positional [_command-args-def command-args]
  (clojure.edn/read-string (str "[" (clojure.string/join " " command-args) "]")))

(defn handle-command
  [ztx command-sym command-args]
  (if-let [command-def (zen.core/get-symbol ztx command-sym)]
    (let [coerced-args      (coerce-args-style command-def command-args)
          args-validate-res (zen.core/validate-schema ztx
                                                      (:args command-def)
                                                      coerced-args)]
      (if (empty? (:errors args-validate-res))
        (let [command-res (try (command command-sym coerced-args)
                               (catch Exception e
                                 #::{:result {:exception e}
                                     :status :error
                                     :code   ::exception}))]
          (if (::status command-res)
            command-res
            #::{:result command-res
                :status :ok}))
        #::{:status :error
            :code   ::invalid-args
            :result {:message           "invalid args"
                     :validation-result args-validate-res}}))
    #::{:status :error
        :code   ::undefined-command
        :result {:message "undefined command"}}))


(defn extract-commands-params [[command-name & command-args]]
  {:command-name command-name
   :command-args command-args})


(defn cli-exec [ztx config-sym args]
  (let [config (zen.core/get-symbol ztx config-sym)
        commands (:commands config)

        {:keys [command-name command-args]} (extract-commands-params args)

        command-entry (get commands (keyword command-name))

        command-sym        (:command command-entry)
        nested-config-sym  (:config command-entry)]

    (cond
      (= "help" command-name)
      #::{:status :ok
          :result commands}

      (some? nested-config-sym)
      (cli-exec ztx nested-config-sym command-args)

      (some? command-sym)
      (handle-command ztx command-sym command-args)

      :else
      #::{:status :error
          :code ::unknown-command
          :result {:message "unknown command"}})))


(defn cli-repl [config])


(defn str->edn [x]
  (clojure.edn/read-string (str x)))


(defn split-args-by-space [args-str]
  (map pr-str (clojure.edn/read-string (str \[ args-str \]))))


(defn apply-with-opts [f args opts]
  (f (vec args) #_opts))


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
   :message (str "Command " cmd-name " not found. Available commands: "
                 (clojure.string/join ", " available-commands))})


(defmacro exception->error-result [& body]
  `(try
     ~@body
     (catch Exception e#
       {:status    :error
        :code      :exception
        :message   (.getMessage e#)
        :exception (Throwable->map e#)})))


(defn repl [ztx config-sym & [opts]]
  (let [prompt-fn (get-prompt-fn opts)
        read-fn   (get-read-fn opts)
        return-fn (get-return-fn opts)
        config    (zen.core/get-symbol ztx config-sym)
        commands  (:commands config)

        opts (update opts :stop-repl-atom #(or % (atom false)))]
    (while (not @(:stop-repl-atom opts))
      (return-fn
        (exception->error-result
          (prompt-fn)
          (let [line (read-fn)
                args (split-args-by-space line)]
            (cli-exec ztx config-sym args)))))))


(defn cmd-unsafe [commands cmd-name args & [opts]]
  (if-let [cmd-fn (get commands cmd-name)]
    (apply-with-opts cmd-fn args opts)
    (command-not-found-err-message cmd-name (keys commands))))


(defn cmd [& args]
  (exception->error-result
    (apply cmd-unsafe args)))

(def commands
  {"echo"       (fn [& args] args)})


(defn get-pwd [& [{:keys [pwd] :as opts}]]
  (let [sh-fn shell/sh
        pwd (clojure.string/trim-newline (:out (sh-fn "pwd")))]
    (or (some-> pwd (clojure.string/replace #"/+$" ""))
        (pwd :silent true))))

(defn collect-all-project-namespaces [opts]
  (let [pwd (get-pwd opts)
        zrc (str pwd "/zrc")
        relativize #(subs % (count zrc))
        zrc-edns (->> zrc
                      clojure.java.io/file
                      file-seq
                      (filter #(clojure.string/ends-with? % ".edn"))
                      (map #(relativize (.getAbsolutePath %)))
                      (remove clojure.string/blank?)
                      (map #(subs % 1)))
        namespaces (map #(-> %
                             (clojure.string/replace ".edn" "")
                             (clojure.string/replace \/ \.)
                             symbol)
                        zrc-edns)]
    namespaces))


(defn cli-main [ztx config-sym [cmd-name :as args]]
  (if (seq cmd-name)
    (prn (cli-exec ztx config-sym args))
    (repl ztx config-sym)))
