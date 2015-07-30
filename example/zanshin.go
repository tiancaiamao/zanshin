package main

import (
	"encoding/binary"
	"fmt"
	"github.com/tiancaiamao/zanshin"
	"io"
	"net"
)

func main() {
	vm := zanshin.New()

	l, err := net.Listen("tcp", ":8293")
	if err != nil {
		panic(err)
	}

	conn, err := l.Accept()
	if err != nil {
		panic(err)
	}
	l.Close()
	defer conn.Close()

	var buf [4096]byte
	for {
		_, err := io.ReadFull(conn, buf[:4])
		if err != nil {
			panic(err)
		}

		size := binary.LittleEndian.Uint32(buf[:])
		if size > 4096 {
			panic("to big packet")
		}

		_, err = io.ReadFull(conn, buf[:size])
		if err != nil {
			panic("read data error")
		}
		fmt.Println("read bytecode:", buf[:size])

		err = vm.Run(buf[:size])
		if err != nil {
			fmt.Println("vm exec error:%s\n", err)
		} else {
			fmt.Println(">%#v\n", vm.Value())
		}
	}
}
