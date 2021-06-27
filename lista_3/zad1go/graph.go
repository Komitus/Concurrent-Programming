package main

import (
	"fmt"
	"math/rand"
	"time"
)

type PacketData struct {
	node_id uint16
	cost    uint16
}
type Packet struct {
	came_from uint16
	offer     []PacketData
}

type Routing_cell struct {
	nextHop uint16
	cost    uint16
	changed bool
}

type Node struct {
	id            uint16
	neighbours    map[uint16]chan Packet
	rec_channel   chan Packet
	routing_table []Routing_cell
}

func Abs(x int16) int16 {
	if x < 0 {
		return -x
	}
	return x
}

func generateGraph(n, d uint16) *[]Node {

	retNodes := make([]Node, n)
	retNodes[0] = Node{id: 0, neighbours: make(map[uint16](chan Packet), 0), rec_channel: make(chan Packet),
		routing_table: make([]Routing_cell, n)}

	for c := uint16(1); c < n; c++ {
		retNodes[c] = Node{id: c, neighbours: make(map[uint16](chan Packet), 0), rec_channel: make(chan Packet),
			routing_table: make([]Routing_cell, n)}
		retNodes[c].neighbours[c-1] = retNodes[c-1].rec_channel
		retNodes[c-1].neighbours[c] = retNodes[c].rec_channel
	}

	//generating d shortucuts
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 := rand.New(s1)
	var src, dest uint16
	//first is src node, next is dest

	var repeated bool = false
	c1 := uint16(0)

	//shortcuts
	//List 3 require undirected graph
	//so 1 shortcuts is in both directions
	for c1 < d {
		repeated = false
		src = uint16(r1.Intn(int(n)))
		dest = uint16(r1.Intn(int(n)))
		if src == dest {
			continue
		}
		for k := range retNodes[src].neighbours {
			if k == dest {
				repeated = true
				break
			}
		}
		if !repeated {
			retNodes[src].neighbours[dest] = retNodes[dest].rec_channel
			retNodes[dest].neighbours[src] = retNodes[src].rec_channel
			c1++
		}
	}

	for i := uint16(0); i < n; i++ {
		for j := uint16(0); j < n; j++ {

			if i == j {
				continue
			} else if _, ok := retNodes[i].neighbours[j]; ok {
				retNodes[i].routing_table[j].cost = 1
				retNodes[i].routing_table[j].nextHop = j
				retNodes[i].routing_table[j].changed = true
			} else {
				retNodes[i].routing_table[j].cost = uint16(Abs(int16(i) - int16(j)))
				if i < j {
					retNodes[i].routing_table[j].nextHop = i + 1
				} else {
					retNodes[i].routing_table[j].nextHop = i - 1
				}
				retNodes[i].routing_table[j].changed = true
			}
		}
	}

	//printing graph

	fmt.Println("Printing Graph:\u001b[38;5;137m")
	for i := uint16(0); i < n; i++ {
		fmt.Println("Node", i, ":")
		fmt.Print("    --> ")
		for k := range retNodes[i].neighbours {
			fmt.Print(k, ", ")
		}
		fmt.Println()
	}
	fmt.Println("\u001b[0m")

	return &retNodes
}
