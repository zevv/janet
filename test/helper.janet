# Helper code for running tests

(var num-tests-passed 0)
(var num-tests-run 0)
(var suite-name 0)
(var start-time 0)
(def feat-list @[])

(def is-verbose (os/getenv "VERBOSE"))

(defn assert
  "Override's the default assert with some nice error handling."
  [x &opt e]
  (default e (let [frame (last (debug/stack (fiber/current)))]
               (string/format "assert error at %s line %d"
                              (frame :source) (frame :source-line))))
  (++ num-tests-run)
  (when x (++ num-tests-passed))
  (def str (string e))
  (if x
    (when is-verbose (eprintf "\e[32m✔\e[0m %s: %v" (describe e) x))
    (eprintf "\e[31m✘\e[0m %s: %v" (describe e) x))
  x)

(defmacro assert-error
  [msg & forms]
  (def errsym (keyword (gensym)))
  ~(assert (= ,errsym (try (do ,;forms) ([_] ,errsym))) ,msg))

(defn check-compile-error
  [form]
  (def result (compile form))
  (assert (table? result) (string/format "expected compilation error for %j, but compiled without error" form)))

(defmacro assert-no-error
  [msg & forms]
  (def errsym (keyword (gensym)))
  ~(assert (not= ,errsym (try (do ,;forms) ([_] ,errsym))) ,msg))

(defn start-suite [&opt x]
  (default x (dyn :current-file))
  (set suite-name
       (cond
         (number? x) (string x)
         (string? x) (string/slice x
                                   (length "test/suite-")
                                   (- (inc (length ".janet"))))
         (string x)))
  (set start-time (os/clock))
  (eprint "Starting suite " suite-name "..."))

(defn feature-gate
  [& feats]
  (array/concat feat-list feats))

(defn skip?
  []
  (var result nil)
  (each feat feat-list
    (unless (dyn feat)
      (set result true)
      (break)))
  result)

(defn end-suite []
  (def delta (- (os/clock) start-time))
  (if (skip?)
    (eprintf "Skipping suite %s." suite-name)
    (do
      (eprinf "Finished suite %s in %.3f seconds - " suite-name delta)
      (eprint num-tests-passed " of " num-tests-run " tests passed.")
      (if (not= num-tests-passed num-tests-run) (os/exit 1)))))
