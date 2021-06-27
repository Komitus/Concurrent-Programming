package main

import (
	"math/rand"
	"time"
)

func sender(entry chan<- Packet, k int) {

	s := rand.NewSource(time.Now().UnixNano() + 300)
	r := rand.New(s)
	for i := 0; i < k; i++ {
		packet := Packet{packetId: i, visitedNodes: make([]*Node, 0)}
		entry <- packet
		time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
	}

}
