package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func main() {
	//int 16 bcs we have routing table for every node so
	// 10000*10000 - ints only for routing table
	var n, d uint16
	input_n, _ := strconv.Atoi(os.Args[1])
	input_d, _ := strconv.Atoi(os.Args[2])
	n = uint16(input_n)
	d = uint16(input_d)
	fmt.Println("Shortucts are in both directions ex. 3->1 and 1->3")
	printer_channel := make(chan string)
	timeout := make(chan bool)
	graph := *generateGraph(n, d)
	for i := uint16(0); i < n; i++ {
		go node_routine(&graph[i], printer_channel)
	}
	go printer(printer_channel, timeout, &graph)

	<-timeout
}

func printer(printer_channel <-chan string, timeout chan<- bool, graph *[]Node) {

	last_time := time.Now().UnixNano()
	curr_time := time.Now().UnixNano()

	for {
		select {
		case message := <-printer_channel:
			fmt.Println(message)
			last_time = time.Now().UnixNano()
		default:
			curr_time = time.Now().UnixNano()
			if int64(curr_time)-int64(last_time) > int64(3*time.Second) {

				fmt.Println()
				fmt.Println("#####################################################################")
				fmt.Println("################ Printing routing table for every node ##############")
				fmt.Println("#####################################################################")
				fmt.Println("Print look like this")
				fmt.Println("Node i : [ node_0{nextHop, cost, changed} ... node_n-1{nextHop, cost, changed} ]")
				fmt.Println()
				//there will be true values on changed if i == j bcs on initializing i put true there
				n := len(*graph)
				for i := 0; i < n; i++ {
					fmt.Print("Node ", i, ":  [ ")
					for j := 0; j < len((*graph)[i].routing_table); j++ {
						if (*graph)[(i+1)%n].routing_table[j].changed == true {
							fmt.Println("Changed == true in node ", i, "at rt[", j, "]")
							os.Exit(1)
						} else if j == i {
							fmt.Print(strings.Repeat("#", len(fmt.Sprint((*graph)[(i+1)%n].routing_table[j]))), " ")
						} else {
							fmt.Print((*graph)[i].routing_table[j], " ")
						}
					}
					fmt.Println("]")
				}
				timeout <- true
				return
			}
		}
	}
}
