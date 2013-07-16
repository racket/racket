dyn-server: dynamic.rkt
	ulimit -n 10000
	racket -t dynamic.rkt

static.txt: dynamic.rkt
	wget http://localhost:8000 -O static.txt

stat-server: static.txt static.rkt
	ulimit -n 10000
	racket -t static.rkt

dyn-httperf:
	httperf --hog --server localhost --port=8000 --rate=6000 --num-conn=5 --num-calls=1000

stat-httperf:
	httperf --hog --server localhost --port=8000 --rate=6000 --num-conn=5 --num-calls=1000
