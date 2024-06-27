#!/usr/bin/env sh

numberplate() {
	echo $(shuf -er -n2 {A..Z} | paste -sd "")$(shuf -er -n2 {0..9} | paste -sd "") $(shuf -er -n3 {A..Z} | paste -sd "")
}

invalid_numberplate() {
	echo $(shuf -er -n4 {A..Z} {0..9} | paste -sd "") $(shuf -er -n3 {A..Z} {0..9} | paste -sd "")
}

echo -n "" > $(dirname $0)/input.csv
for _ in {1..100}
do
	np=$(numberplate)
	echo "$(shuf -er -n1 "$(invalid_numberplate)" "$np" "$np" "$np"),$(echo "$(date +%N) % (50 * 3600) + 30 * 3600" | bc)" >> input.csv
done
