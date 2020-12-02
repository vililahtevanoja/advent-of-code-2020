package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const inputFile = "input.txt"

func main() {
	data, _ := ioutil.ReadFile(inputFile)

	lines := strings.Split(string(data), "\n")
	ns := []int{}
	for _, line := range lines {
		n, _ := strconv.Atoi(line)
		ns = append(ns, n)
	}
	for i, n1 := range ns {
		for _, n2 := range ns[i:] {
			if n1+n2 == 2020 {
				fmt.Printf("%d + %d = %d\n", n1, n2, n1+n2)
				fmt.Printf("%d * %d = %d\n", n1, n2, n1*n2)
			}
		}
	}
}
