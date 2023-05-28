# test *debug* flags

(import ./helper :prefix "" :exit true)
(start-suite 15)

(assert (deep= (in (disasm (defn a [] (def x 10) x)) :symbolmap)
               @[[0 3 0 'a] [1 3 1 'x]])
        "symbolslots when *debug* is true")

(defn a [arg]
  (def x 10)
  (do
    (def y 20)
    (def z 30)
    (+ x y z)))
(def symbolslots (in (disasm a) :symbolslots))
(def f (asm (disasm a)))
(assert (deep= (in (disasm f) :symbolslots)
               symbolslots)
        "symbolslots survive disasm/asm")

(comment
  (setdyn *debug* true)
  (setdyn :pretty-format "%.40M")
  (def f (fn [x] (fn [y] (+ x y))))
  (assert (deep= (map last (in (disasm (f 10)) :symbolmap))
                 @['x 'y])
          "symbolslots upvalues"))

(assert (deep= (in (disasm (defn a [arg]
                             (def x 10)
                             (do
                               (def y 20)
                               (def z 30)
                               (+ x y z)))) :symbolmap)
               @[[0 7 0 'arg]
                 [0 7 1 'a]
                 [1 7 2 'x]
                 [2 7 3 'y]
                 [3 7 4 'z]])
        "arg & inner symbolslots")

# buffer/push-at
(assert (deep= @"abc456" (buffer/push-at @"abc123" 3 "456")) "buffer/push-at 1")
(assert (deep= @"abc456789" (buffer/push-at @"abc123" 3 "456789")) "buffer/push-at 2")
(assert (deep= @"abc423" (buffer/push-at @"abc123" 3 "4")) "buffer/push-at 3")

(string/format "%p" string/format)
(string/format "%j" @{:a 1 :b 2})
(string/format "%q" (tuple ;(range 0 161)))

(string/format "%q" (table/setproto @{:a 1 :b 2} @{:c 3 :d 4 :_name "Hello"}))
(string/format "%q" (struct/with-proto {:a 1 :b 2} :c 3 :d 4 :_name "Hello"))

(string/format "%t" 123)
(string/format "%v" 123)
(string/format "%V" 123)
(string/format "%g" 123)

(defn- =approx [a b]
  (< (math/abs (- a b)) 0.0001))

(assert (=approx (math/acos 0.3) 1.2661036727795) "acos")
(assert (=approx (math/asin 0.3) 0.3046926540154) "asin")
(assert (=approx (math/atan 3) 1.2490457723983) "atan")
(assert (=approx (math/cos 3) -0.98999249660045) "cos")
(assert (=approx (math/cosh 3) 10.067661995777) "cosh")
(assert (=approx (math/acosh 3) 1.7627471740391) "acosh")
(assert (=approx (math/sin 3) 0.14112000805987) "sin")
(assert (=approx (math/sinh 3) 10.01787492741) "sinh")
(assert (=approx (math/asinh 3) 1.8184464592321) "asinh")
(assert (=approx (math/tan 3) -0.14254654307428) "tan")
(assert (=approx (math/tanh 3) 0.99505475368673) "tanh")
(assert (=approx (math/atanh 0.3) 0.30951960420311) "atanh")
(assert (=approx (math/exp 3) 20.085536923188) "exp")
(assert (=approx (math/exp2 3) 8) "exp2")
(assert (=approx (math/expm1 3) 19.085536923188) "expm1")
(assert (=approx (math/log 3) 1.0986122886681) "log")
(assert (=approx (math/log10 3) 0.47712125471966) "log10")
(assert (=approx (math/log2 3) 1.5849625007212) "log2")
(assert (=approx (math/sqrt 3) 1.7320508075689) "sqrt")
(assert (=approx (math/cbrt 3) 1.4422495703074) "cbrt")
(assert (=approx (math/ceil 3.1) 4) "ceil")
(assert (=approx (math/floor 3.1) 3) "floor")
(assert (=approx (math/trunc 3.7) 3) "trunc")
(assert (=approx (math/round 3.7) 4) "round")
(assert (=approx (math/log1p 3) 1.3862943611199) "log1p")
(assert (=approx (math/erf 3) 0.999977909503) "erf")
(assert (=approx (math/erfc 3) 0.000022090496) "erfc")
(assert (=approx (math/atan2 3 4) 0.64350110879328) "atan2")
(assert (=approx (math/pow 3 4) 81) "pow")
(assert (=approx (math/hypot 3 4) 5) "hypot")

(def b (buffer/new 32))
(buffer/push b @"BBBBB")
(buffer/fill b 64)
(buffer/push-byte b 65)
(buffer/popn b 4)
(buffer/bit @"U" 3)
(buffer/trim @"123")
(buffer/push-at @"111" 1 64)
(buffer/push-at @"111" 1 @"AAA")
(protect (buffer/push-word @"" 3.1415))
(protect (buffer/push-at @"111" -1 64))
(def b @"aaa")
(buffer/push b b)

(def l (ev/rwlock))
(ev/acquire-wlock l)
(ev/release-wlock l)
(ev/acquire-rlock l)
(ev/release-rlock l)
(def l (ev/lock))

(array/pop @[])
(array/peek @[])
(array/fill @[1 1] 2)
(array/ensure @[1 1] 6 2)
(def a @[1 2 3])
(array/concat a @[1 2] 3 a)
(array/insert a 1 @[1 2])
(array/insert a -1 @[1 2])
(protect (array/insert a 32 @[1 2]))
(protect (array/remove @[1 2] 3))
(protect (array/remove @[1 2] 1 -1))


(def a @[1])
(array/pop a)
(array/trim a)

(defn- f [x &named a] (+ x 1))
(def da (disasm f))
(each k [:arity :min-arity :max-arity :bytecode :source :name :vararg 
         :structarg :slotcount :constants :sourcemap :environments :defs]
  (assert (deep= (disasm f k) (da k)) (string "disasm " k)))

(protect (asm {}))
(protect (asm { :bytecode @[]}))





(end-suite)
