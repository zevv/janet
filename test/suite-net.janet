# Copyright (c) 2023 Calvin Rose & contributors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

(import ./helper :prefix "" :exit true)
(start-suite)


# net/address and net/address-unpack
(assert (=
         (net/address-unpack (net/address "127.0.0.1" "http"))
         ["127.0.0.1" 80]))

(assert (=
          (net/address-unpack (first (net/address "127.0.0.1" "http" :stream true)))
          ["127.0.0.1" 80]))


(assert-error
  "expected socket type as :stream or :datagram, got :nonsense"
  (net/address "127.0.0.1" "http" :nonsense))

# Basic listen/connect/shutdown sequence
(def s1 (net/listen "127.0.0.1" "1801" :stream))
(assert (= (net/localname s1) ["127.0.0.1" 1801]))

(def s2 (net/connect "127.0.0.1" "1801" :stream "127.0.0.1" "1802"))
(assert (= (net/localname s2) ["127.0.0.1" 1802]))

(def s3 (net/connect "127.0.0.1" "1801" :stream))
(assert (not= (net/localname s3) ["127.0.0.1" 1802]))

(assert (not (first (protect
  (net/connect "127.0.0.1" "1801" :stream "127.0.0.1" "1802"))))) # already bound

# for code coverage
(net/flush s2)
(net/shutdown s2)

(defn- run-server [host port]
  (net/server host port
    (fn [c]
      (net/write c "123")
      (ev/sleep 0.1)
      (net/write c "456")
      (net/close c))))

# write/read/chunk

(def server (run-server "0.0.0.0" "1901"))

(def s (net/connect "127.0.0.1" "1901"))
(assert (deep= (net/chunk s 6) @"123456"))
(net/close s)

(def s (net/connect "127.0.0.1" "1901"))
(assert (deep= (net/read s 6) @"123"))
(net/close s)

(net/close server)

# unix sockets

(def has-unix-sockets (first (protect (net/address :unix "/tmp/sock"))))

(if has-unix-sockets (do
  (assert (= (net/address-unpack (net/address :unix "/tmp/sock"))
            ["/tmp/sock"] ))

  (os/rm "/tmp/sock")
  (def server (run-server :unix "/tmp/sock"))
  (def s (net/connect :unix "/tmp/sock"))
  (assert (deep= (net/read s 3) @"123"))
  (net/close server)))


# sendto/recvfrom

(def s1 (net/listen "127.0.0.1" "1903" :datagram))
(def s2 (net/listen "127.0.0.1" "1904" :datagram))
(net/send-to s1 (net/address "127.0.0.1" "1904" :datagram) "123")
(assert (= (net/address-unpack (net/recv-from s2 1024 @"")) ["127.0.0.1" 1903]))

# Listen errors
(assert (not (first (protect (net/listen "123.123.123.123" "1")))))
(assert (not (first (protect (net/listen :unix "/illegal-path")))))


(end-suite)
