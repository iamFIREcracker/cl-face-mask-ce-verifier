(ql:quickload '("face-mask-ce-verifier-web" "swank-tunnel"))

(fmcv:start :web-interface "0.0.0.0")
(swank-tunnel:start :dont-ngrok t)
