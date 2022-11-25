(ns core
  (:require [zen.core]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string]
            [clojure.edn]
            [zen.cli-tools :as ct]
            [clojure.stacktrace]
            [clojure.java.shell :as shell]
            [clojure.java.shell])
  (:gen-class))


(defn str->edn [x]
  (clojure.edn/read-string (str x)))


(defn split-args-by-space [args-str]
  (map pr-str (clojure.edn/read-string (str \[ args-str \]))))


(defn apply-with-opts [f args opts]
  (apply f (conj (vec args) opts)))


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
        config (zen.core/get-symbol ztx config-sym)
        commands (:commands config)

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

(defn cli-main [ztx config-sym cmd-name args]
  (if (do (seq cmd-name)
          (prn cmd-name))
    (fn [] (ct/cli-exec ztx config-sym args))
    (repl ztx config-sym))
  (prn "Exited."))

(defn -main [& [cmd-name & args]]
  (let [ztx (zen.core/new-context)]
    (zen.core/load-ns ztx
     '{:ns 'my-cli
       :import #{'zen.cli-tools}

       'identity
       {:zen/tags #{'zen.cli-tools/command}
        :zen/desc "returns its arg"
        :args-style :named
        :args {:type 'zen/map
               :require #{:value}
               :keys {:value {:type 'zen/string
                              :zen/desc "value that will be returned by this fn"}}}}

       '+
       {:zen/tags #{'zen.cli-tools/command}
        :zen/desc "calculates sum of passed arguments"
        :args-style :positional
        :args {:type 'zen/vector
               :zen/desc "numbers that will be summed together"
               :every {:type 'zen/number}}}

       'no-implementation
       {:zen/tags #{'zen.cli-tools/command}
        :zen/desc "no implementation should be defined. Needed for implementation missing error handling"
        :args {:type 'zen/vector
               :maxItems 0}}

       'throw-exception
       {:zen/tags #{'zen.cli-tools/command}
        :zen/desc "Throws an exception. Needed for testing exception handling"
        :args {:type 'zen/vector
               :maxItems 0}}

       'my-config
       {:zen/tags #{'zen.cli-tools/config}
        :commands {:identity  {:command 'identity}
                   :+         {:command '+}
                   :undefined {:command 'undefined}
                   :no-impl   {:command 'no-implementation}
                   :fail      {:command 'throw-exception}}}})
    (cli-main ztx 'my-cli/my-config cmd-name args)) )


(comment

  (collect-all-project-namespaces {})

  (get-pwd {})

  (let [sh-fn shell/sh]
    (clojure.string/trim-newline (:out (sh-fn "pwd"))))

  )
