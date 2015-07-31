package zanshin

import (
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
}

func TestAll(t *testing.T) {
	vm := New()
	for _, test := range testcase {
		err := vm.Run(test.code)
		if err != nil && !test.error {
			t.Errorf("%s failed: should not get a error, but it get error: %s\n", test.input, err)
		} else if vm.Value() != test.result {
			t.Errorf("%s failed: expect %#v, but get %#v\n", test.input, test.result, vm.Value)
		}
	}
}
