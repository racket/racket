dyn-server: dynamic.rkt
	ulimit -n 10000
	racket -t dynamic.rkt

stat-server: static.rkt
	ulimit -n 10000
	racket -t static.rkt

dyn-httperf:
	httperf --hog --num-conns 1000 --num-calls 1000 --burst-length 20 --port 8000 --rate 1000 --uri=/pong

stat-httperf:
	httperf --hog --num-conns 1000 --num-calls 1000 --burst-length 20 --port 8001 --rate 1000 --uri=/FiringGeometry.png
