package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

// global vars
var array []byte
var pointer int
var program []byte
var bracketLookup map[int]int

// report error and exit
func die(err string) {
	fmt.Printf("Error: %s\n", err)
	os.Exit(1)
}

func bracketMatch(pos, open int) int {
	for i := pos; i < len(program); i++ {
		switch program[i] {
		case '[':
			i = bracketMatch(i+1, i)
		case ']':
			if open == -1 {
				die("unmatched ]")
			}
			bracketLookup[open] = i
			bracketLookup[i] = open
			return i
		}
	}
	if open != -1 {
		die("unmatched [")
	}
	return 0
}

func interpret() {
	for i := 0; i < len(program); i++ {
		switch program[i] {
		case '>':
			pointer++
			if pointer == len(array) {
				// double array size
				array = append(array, make([]byte, len(array))...)
			}
		case '<':
			pointer--
			if pointer < 0 {
				die("out of bounds")
			}
		case '+':
			array[pointer]++
		case '-':
			array[pointer]--
		case '.':
			fmt.Printf("%c", array[pointer])
		case ',':
			fmt.Scanf("%c", &array[pointer])
		case '[':
			if array[pointer] == 0 {
				i = bracketLookup[i]
			}
		case ']':
			if array[pointer] != 0 {
				i = bracketLookup[i]
			}
		}
	}
	fmt.Printf("\n")
}

func main() {
	// read stdin into program string
	program, _ = ioutil.ReadAll(os.Stdin)
	// initialize array
	array = make([]byte, 512)
	// check brackets and create lookup map
	bracketLookup = make(map[int]int)
	_ = bracketMatch(0, -1)
	// interpret
	interpret()
}
