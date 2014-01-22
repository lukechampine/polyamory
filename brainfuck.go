package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

// global vars
var array []byte
var pointer int
var program string
var bracketLookup map[int]int

// simple LIFO stack, used for bracketMatch
type Node struct {
	value int
	next  *Node
}

type Stack struct {
	top  *Node
	size int
}

func (s *Stack) Push(value int) {
	s.top = &Node{value, s.top}
	s.size++
}

func (s *Stack) Pop() int {
	value := s.top.value
	s.top = s.top.next
	s.size--
	return value
}

// report error and exit
func die(err string) {
	fmt.Printf("Error: %s\n", err)
	os.Exit(0)
}

func bracketMatch() {
	bracketLookup = make(map[int]int)
	stack := new(Stack)
	for i, c := range program {
		switch c {
		case '[':
			stack.Push(i)
		case ']':
			if stack.size == 0 {
				die("unmatched ]")
			}
			j := stack.Pop()
			bracketLookup[i] = j
			bracketLookup[j] = i

		}
	}
	if stack.size != 0 {
		die("unmatched [")
	}
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
	in, _ := ioutil.ReadAll(os.Stdin)
	program = string(in)
	// initialize array
	array = make([]byte, 512)
	// check brackets and create lookup map
	bracketMatch()
	// interpret
	interpret()
}
