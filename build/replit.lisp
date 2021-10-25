(ql:quickload '("face-mask-ce-verifier-web" "swank-tunnel"))

(fmcv-web:start :web-interface "0.0.0.0")
(swank-tunnel:start)
