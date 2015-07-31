package zanshin

import (
	"bytes"
	"testing"
)

type TestCase struct {
	input  string
	code   []byte
	result Value
	error  bool
}

var testcase = []TestCase{
	TestCase{
		input:  "(define a 3)",
		code:   []byte{84, 27, 0, 20},
		result: valueVOID,
		error:  false,
	},
	TestCase{
		input:  "(+ 1 2)",
		code:   []byte{82, 34, 83, 35, 104, 20},
		result: 3,
		error:  false,
	},
	TestCase{
		input: "(lambda (x) x)",
		code:  []byte{40, 2, 30, 4, 72, 32, 1, 43, 20},
		result: &Procedure{
			code: []byte{40, 2, 30, 4, 72, 32, 1, 43, 20},
			pos:  4,
			env:  nil,
		},
		error: false,
	},
	TestCase{
		input:  "((lambda (x) x) 1)",
		code:   []byte{82, 34, 51, 60, 32, 1, 20},
		result: 1,
		error:  false,
	},
}

func TestAll(t *testing.T) {
	vm := New()
	for _, test := range testcase {
		err := vm.Run(test.code)
		if err != nil && !test.error {
			t.Errorf("%s failed: should not get a error, but it get error: %s\n", test.input, err)
		} else if vm.Value() != test.result {
			p1, ok1 := vm.Value().(*Procedure)
			p2, ok2 := test.result.(*Procedure)
			if ok1 && ok2 && procedureEqual(p1, p2) {
			} else {
				t.Errorf("%s failed: expect %#v, but get %#v\n", test.input, test.result, vm.Value)
			}
		}
	}
}

func procedureEqual(p1, p2 *Procedure) bool {
	return p1.pos == p2.pos && bytes.Equal(p1.code, p2.code)
}
