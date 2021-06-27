package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Packet struct {
	packetId     int
	visitedNodes []*Node
	ttl          int
}

type Node struct {
	id              int
	nextChannels    []*chan Packet //array of pointers to channels of Packets
	recChannel      chan Packet
	packetsReceived []*Packet
	trapped         chan bool
}

func GenerateGraph(n, d, b int) []Node {

	retNodes := make([]Node, n)
	retNodes[0] = Node{id: 0, nextChannels: make([]*chan Packet, 0),
		recChannel: make(chan Packet), packetsReceived: make([]*Packet, 0), trapped: make(chan bool)}

	for c := 1; c < n; c++ {
		retNodes[c] = Node{id: c, nextChannels: make([]*chan Packet, 0),
			recChannel: make(chan Packet), packetsReceived: make([]*Packet, 0), trapped: make(chan bool)}
		retNodes[c-1].nextChannels = append(retNodes[c].nextChannels, &retNodes[c].recChannel) //make path through graph
	}
	receiverChannel := make(chan Packet)
	retNodes[n-1].nextChannels = append(retNodes[n-1].nextChannels, &receiverChannel)

	//generating d shortucuts
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	var src, dest int
	//first is src node, next is dest
	shortCuts := make([][]int, n) //first index is source Node, second destination

	//last cannot have shortcuts
	for i := range shortCuts {
		shortCuts[i] = make([]int, 0)
	}

	var repeated bool = false
	c1 := 0

	//directed without cycles
	for c1 < d {
		repeated = false
		src = r1.Intn(n - 2)
		dest = src + 2 + r1.Intn(n-src-2)

		for j := 0; j < len(shortCuts[src]); j++ {
			if shortCuts[src][j] == dest {
				repeated = true
				break
			}
		}
		if !repeated {
			//fmt.Println("added: ", src, " ", dest)
			retNodes[src].nextChannels = append(retNodes[src].nextChannels,
				&retNodes[dest].recChannel)
			shortCuts[src] = append(shortCuts[src], dest)
			c1++
		}

	}

	//fmt.Println("Shortcuts allowing cycles: ")
	//shortcuts which allows cycles

	//last cannot have shortcuts

	c2 := 0
	for c2 < b {
		repeated = false
		src = 1 + r1.Intn(n-1) // from 1 to n-1
		dest = r1.Intn(src)    //from 0 to src-1

		for j := 0; j < len(shortCuts[src]); j++ {
			if shortCuts[src][j] == dest {
				repeated = true
				break
			}
		}
		if !repeated {
			//fmt.Println("added: ", src, " ", dest)
			retNodes[src].nextChannels = append(retNodes[src].nextChannels,
				&retNodes[dest].recChannel)
			shortCuts[src] = append(shortCuts[src], dest)
			c2++
		}

	}

	//printing graph

	fmt.Println("Printing Graph:\u001b[38;5;137m")
	for i := 0; i < n; i++ {
		fmt.Println("Node", i, ":")
		fmt.Print("    --> ", i+1, ", ")
		for j := 0; j < len(shortCuts[i]); j++ {
			fmt.Print(shortCuts[i][j], ", ")
		}
		fmt.Println()
	}
	fmt.Println("\u001b[0m")

	return retNodes
}
