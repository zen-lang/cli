(ns zen.cli-tools-test
  (:require [clojure.test :as t]
            [zen.cli-tools :as sut]
            [zen.core]
            [matcho.core :as matcho]))


(t/deftest cli-tools-schema-test
  (def ztx (zen.core/new-context))

  (zen.core/load-ns
    ztx
    '{:ns my-cli
      :import #{zen.cli-tools}

      identity
      {:zen/tags #{zen.cli-tools/command}
       :zen/desc "returns its arg"
       :args-style :named
       :args {:type zen/map
              :require #{:value}
              :keys {:value {:type zen/string
                             :zen/desc "value that will be returned by this fn"}}}}

      +
      {:zen/tags #{zen.cli-tools/command}
       :zen/desc "calculates sum of passed arguments"
       :args-style :positional
       :args {:type zen/vector
              :zen/desc "numbers that will be summed together"
              :every {:type zen/number}}}

      no-implementation
      {:zen/tags #{zen.cli-tools/command}
       :zen/desc "no implementation should be defined. Needed for implementation missing error handling"
       :args {:type zen/vector
              :maxItems 0}}

      throw-exception
      {:zen/tags #{zen.cli-tools/command}
       :zen/desc "Throws an exception. Needed for testing exception handling"
       :args {:type zen/vector
              :maxItems 0}}

      math-config
      {:zen/tags #{zen.cli-tools/config}
       :commands {:+ {:command +}}}

      my-config
      {:zen/tags #{zen.cli-tools/config}
       :commands {:identity  {:command identity}
                  :undefined {:command undefined}
                  :no-impl   {:command no-implementation}
                  :fail      {:command throw-exception}
                  :math      {:config math-config}}}})

  (matcho/match (zen.core/errors ztx)
                [{:path [:commands :undefined :command]}
                 {:unresolved-symbol 'undefined
                  :ns 'my-cli}
                 nil])

  (defmethod sut/command 'my-cli/identity [_ {:keys [value]} _opts]
    value)

  (defmethod sut/command 'my-cli/+ [_ args _opts]
    (apply + args))

  (defmethod sut/command 'my-cli/throw-exception [_ args _opts]
    (throw (Exception. "some exception during command evaluation")))

  (t/testing "success cmd exec"
    (t/testing "named args"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["identity" ":value" "\"foo\""])
                    {::sut/result "foo"
                     ::sut/status :ok}))

    (t/testing "positional args and also subcommand"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["math" "+" "4" "2" "2"])
                    {::sut/result 8
                     ::sut/status :ok})))

  (t/testing "error handling"
    (t/testing "command is not mentioned in :config"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["?" "4" "2" "2"])
                    {::sut/code ::sut/unknown-command
                     ::sut/status :error}))

    (t/testing "command is not defined in zen, but mentioned in :config"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["undefined" "4" "2" "2"])
                    {::sut/code ::sut/undefined-command
                     ::sut/status :error}))

    (t/testing "command args validation failed"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["identity" "4" "2"])
                    {::sut/code ::sut/invalid-args
                     ::sut/status :error}))

    (t/testing "command implementation is missing in clj runtime"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["no-impl"])
                    {::sut/code ::sut/implementation-missing
                     ::sut/status :error}))

    (t/testing "exception occured during command execution"
      (matcho/match (sut/cli-exec ztx 'my-cli/my-config ["fail"])
                    {::sut/code ::sut/exception
                     ::sut/status :error}))))
