package main

import (
	"math/rand"
	"time"
)

func hunter(graph []Node, n int, numOfTraps int, trapMadePrinter chan<- int) {

	s := rand.NewSource(time.Now().UnixNano() + 300)
	r := rand.New(s)

	var randNode int
	for i := 0; i < numOfTraps; i++ {
		time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
		randNode = r.Intn(n)
		trapMadePrinter <- randNode     //i want firstly pass info about that to avoid situation that
		graph[randNode].trapped <- true //packet will be trapped before "trap was made" is printed
	}

}
