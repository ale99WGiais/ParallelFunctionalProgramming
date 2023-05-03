.PHONY: data clean plot all

all: plot

plot: exercise_1_1.pdf exercise_1_3.pdf

two_%_i32s:
	futhark dataset -b --i32-bounds=-10000:10000 \
	                -g [$*]i32 -g [$*]i32 > $@

exercise_1_1.pdf: benchmark
	python3 plot.py exercise_1_1 $(SIZES)

exercise_1_3.pdf: benchmark
	python3 plot.py exercise_1_3 $(SIZES)

benchmark: exercise_1_1-opencl.json exercise_1_1-c.json exercise_1_3-opencl.json exercise_1_3-c.json

SIZES = 100 1000 10000 100000 1000000 5000000 10000000

exercise_1_1-opencl.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=multicore \
	              --json exercise_1_1-opencl.json \
	              exercise_1_1.fut

exercise_1_1-c.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=c \
	              --json exercise_1_1-c.json \
	              exercise_1_1.fut

exercise_1_3-opencl.json: $(SIZES:%=two_%_i32s) exercise_1_3.fut
	futhark bench --backend=multicore exercise_1_3.fut --json exercise_1_3-opencl.json

exercise_1_3-c.json: $(SIZES:%=two_%_i32s) exercise_1_3.fut
	futhark bench --backend=c exercise_1_3.fut --json exercise_1_3-c.json

clean:
	rm -f two_*_i32s *.json *.pdf *.bin *.c


