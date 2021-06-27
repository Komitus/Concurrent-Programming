package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Packet struct {
	packetId     int
	visitedNodes []*Node
}

type Node struct {
	id              int
	nextChannels    []*chan Packet //array of pointers to channels of Packets
	recChannel      chan Packet
	packetsReceived []*Packet
}

func GenerateGraph(n, d int) []Node {

	retNodes := make([]Node, n)
	retNodes[0] = Node{id: 0, nextChannels: make([]*chan Packet, 0),
		recChannel: make(chan Packet), packetsReceived: make([]*Packet, 0)}

	for c := 1; c < n; c++ {
		retNodes[c] = Node{id: c, nextChannels: make([]*chan Packet, 0),
			recChannel: make(chan Packet), packetsReceived: make([]*Packet, 0)}
		retNodes[c-1].nextChannels = append(retNodes[c].nextChannels, &retNodes[c].recChannel) //make path through graph
	}
	receiverChannel := make(chan Packet)
	retNodes[n-1].nextChannels = append(retNodes[n-1].nextChannels, &receiverChannel)

	//generating d shortucuts
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	var src, dest int
	//first is src node, next is dest
	shortCuts := make([][]int, n-1) //first index is source Node, second destination
	//last cannot have shortcuts
	for i := range shortCuts {
		shortCuts[i] = make([]int, 0)
	}

	var repeated bool = false
	c := 0
	for c < d {
		repeated = false
		src = r1.Intn(n - 2)
		dest = r1.Intn(n)
		//fmt.Println("src: ", src, " dest: ", dest)
		if src+1 < dest {

			for j := 0; j < len(shortCuts[src]); j++ {
				if shortCuts[src][j] == dest {
					//fmt.Println("c:", c)
					repeated = true
					break
				}
			}
			if !repeated {
				//fmt.Println("added: ", src, " ", dest)
				retNodes[src].nextChannels = append(retNodes[src].nextChannels,
					&retNodes[dest].recChannel)
				shortCuts[src] = append(shortCuts[src], dest)
				c++
			}
		}
	}
	//printing graph

	fmt.Println("\u001b[38;5;137mPrinting Graph:")
	for i := 0; i < n-1; i++ {
		fmt.Println("Node", i, ":")
		fmt.Print("    --> ", i+1, ", ")
		for j := 0; j < len(shortCuts[i]); j++ {
			fmt.Print(shortCuts[i][j], ", ")
		}
		fmt.Println()
	}
	fmt.Println("Node", n-1, ":")
	fmt.Println("\u001b[0m")

	return retNodes
}
