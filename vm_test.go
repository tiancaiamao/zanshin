package zanshin

import (
	"testing"
)

func TestAll(t *testing.T) {
	testcase := [][]byte{
		[]byte{84, 27, 0, 20},           // (define a 3)
		[]byte{82, 34, 83, 35, 104, 20}, // (+ 1 2)
	}

	vm := New()
	for _, test := range testcase {
		err := vm.Run(test)
		if err != nil {
			t.Fail()
		}
	}
}
