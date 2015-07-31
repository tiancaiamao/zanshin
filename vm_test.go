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
	TestCase{
		input:  "(if #t 1 #f)",
		code:   []byte{10, 31, 3, 82, 30, 1, 11, 20},
		result: 1,
		error:  false,
	},
	TestCase{
		input:  "(begin #t #f)",
		code:   []byte{10, 11, 20},
		result: false,
		error:  false,
	},
	TestCase{
		input:  "(begin (define f (lambda () 3)) (f))",
		code:   []byte{40, 2, 30, 5, 75, 0, 32, 84, 43, 27, 0, 8, 0, 34, 50, 39, 45, 20},
		result: 3,
		error:  false,
	},
	TestCase{
		input:  "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))",
		code:   []byte{40, 2, 30, 32, 71, 32, 1, 34, 81, 35, 106, 31, 3, 82, 30, 19, 1, 34, 8, 0, 34, 1, 34, 82, 35, 105, 34, 51, 60, 39, 37, 45, 38, 35, 109, 43, 27, 0, 8, 0, 34, 79, 5, 34, 51, 60, 39, 45, 20},
		result: 120,
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
