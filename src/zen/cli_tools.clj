(ns zen.cli-tools
  (:require [clojure.edn]
            [clojure.string]
            [zen.core]))


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
