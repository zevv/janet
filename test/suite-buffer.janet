# Copyright (c) 2023 Calvin Rose
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

# Buffer blitting
# 16ebb1118
(def b (buffer/new-filled 100))
(buffer/bit-set b 100)
(buffer/bit-clear b 100)
(assert (zero? (sum b)) "buffer bit set and clear")
(assert (= false (buffer/bit b 101)) "bit get false")
(buffer/bit-toggle b 101)
(assert (= true (buffer/bit b 101)) "bit get true")
(assert (= 32 (sum b)) "buffer bit set and clear")
(assert-error "invalid bit index 1000" (buffer/bit-toggle b 1000))

(def b2 @"hello world")

(buffer/blit b2 "joyto ")
(assert (= (string b2) "joyto world") "buffer/blit 1")

(buffer/blit b2 "joyto" 6)
(assert (= (string b2) "joyto joyto") "buffer/blit 2")

(buffer/blit b2 "abcdefg" 5 6)
(assert (= (string b2) "joytogjoyto") "buffer/blit 3")

# buffer/push

(assert (deep= (buffer/push @"AA" @"BB") @"AABB") "buffer/push buffer")
(assert (deep= (buffer/push @"AA" 66 66) @"AABB") "buffer/push int")
(def b @"AA")
(assert (deep= (buffer/push b b) @"AAAA") "buffer/push buffer self")

# buffer/push-byte
(assert (deep= (buffer/push-byte @"AA" 66) @"AAB") "buffer/push-byte")
(assert-error "bad slot #1, expected 32 bit signed integer" (buffer/push-byte @"AA" :flap))

# Buffer push word
# e755f9830
(def b3 @"")
(buffer/push-word b3 0xFF 0x11)
(assert (= 8 (length b3)) "buffer/push-word 1")
(assert (= "\xFF\0\0\0\x11\0\0\0" (string b3)) "buffer/push-word 2")
(buffer/clear b3)
(buffer/push-word b3 0xFFFFFFFF 0x1100)
(assert (= 8 (length b3)) "buffer/push-word 3")
(assert (= "\xFF\xFF\xFF\xFF\0\x11\0\0" (string b3)) "buffer/push-word 4")
(assert-error "cannot convert 0.5 to machine word" (buffer/push-word @"" 0.5))

# Buffer push string
# 175925207
(def b4 (buffer/new-filled 10 0))
(buffer/push-string b4 b4)
(assert (= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" (string b4))
        "buffer/push-buffer 1")
(def b5 @"123")
(buffer/push-string b5 "456" @"789")
(assert (= "123456789" (string b5)) "buffer/push-buffer 2")

# some tests for buffer/format
# 029394d
(assert (= (string (buffer/format @"" "pi = %6.3f" math/pi)) "pi =  3.142")
        "%6.3f")
(assert (= (string (buffer/format @"" "pi = %+6.3f" math/pi)) "pi = +3.142")
        "%6.3f")
(assert (= (string (buffer/format @"" "pi = %40.20g" math/pi))
           "pi =                     3.141592653589793116") "%6.3f")

(assert (= (string (buffer/format @"" "🐼 = %6.3f" math/pi)) "🐼 =  3.142")
        "UTF-8")
(assert (= (string (buffer/format @"" "π = %.8g" math/pi)) "π = 3.1415927")
        "π")
(assert (= (string (buffer/format @"" "\xCF\x80 = %.8g" math/pi))
           "\xCF\x80 = 3.1415927") "\xCF\x80")

# Regression #301
# a3d4ecddb
(def b (buffer/new-filled 128 0x78))
(assert (= 38 (length (buffer/blit @"" b -1 90))) "buffer/blit 1")

(def a @"abcdefghijklm")
(assert (deep= @"abcde" (buffer/blit @"" a -1 0 5)) "buffer/blit 2")
(assert (deep= @"bcde" (buffer/blit @"" a -1 1 5)) "buffer/blit 3")
(assert (deep= @"cde" (buffer/blit @"" a -1 2 5)) "buffer/blit 4")
(assert (deep= @"de" (buffer/blit @"" a -1 3 5)) "buffer/blit 5")

# buffer/push-at
# c55d93512
# TODO: issue 1168
(assert (deep= @"abc456" (buffer/push-at @"abc123" 3 "456"))
        "buffer/push-at 1")
(assert (deep= @"abc456789" (buffer/push-at @"abc123" 3 "456789"))
        "buffer/push-at 2")
(assert (deep= @"abc423" (buffer/push-at @"abc123" 3 "4"))
        "buffer/push-at 3")
(assert (deep=  (buffer/push-at @"AAA" 1 66) @"ABA"))
(assert (deep=  (buffer/push-at @"AAA" 1 @"B") @"ABA"))
(assert-error "index out of range [0, 3)" (buffer/push-at @"AAA" 5 66))

# buffer/slice
(assert (deep= (buffer/slice @"12345" 2) @"345") "buffer/slice 1")
(assert (deep= (buffer/slice @"12345" 0 -3) @"123") "buffer/slice 0 -3")
(assert-error "start index 9 out of range [-6,5]" (buffer/slice @"12345" 9))
(assert-error "buffer/slice end out of range" (buffer/slice @"12345" 1 8))

# buffer/popn
(assert (deep= (buffer/popn @"1234" 2) @"12") "buffer/popn 1")
(assert (deep= (buffer/popn @"1234" 5) @"") "buffer/popn 2")
(assert-error "buffer/popn negative" (buffer/popn @"1234" -1))

# buffer/fill
(assert (deep= (buffer/fill @"AA" 66) @"BB"))

# code coverage
(buffer/trim @"123")

#buffer/new
(assert (deep= (buffer/new 16) @"") "buffer/new 1")
(end-suite)

