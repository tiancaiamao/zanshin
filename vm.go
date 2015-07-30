package zanshin

import (
	"errors"
)

var (
	valueVOID Value = &struct{}{}
)

type Value interface{}

type stack struct {
	data []Value
	idx  int
}

func (s *stack) Push(v Value) {
	if s.idx == len(s.data) {
		tmp := make([]Value, s.idx+1000)
		copy(tmp, s.data)
	}

	s.data[s.idx] = v
	s.idx++
}

func (s *stack) Pop() Value {
	s.idx--
	return s.data[s.idx]
}

type Cons struct {
	car Value
	cdr Value
}

func valueCar(v Value) Value {
	return v.(*Cons).car
}

func valueCdr(v Value) Value {
	return v.(*Cons).cdr
}

func setCar(v1 Value, v2 Value) {
	v1.(*Cons).car = v2
}

func setCdr(v1 Value, v2 Value) {
	v1.(*Cons).cdr = v2
}

func valuePairP(v Value) bool {
	_, ok := v.(*Cons)
	return ok
}

type Symbol string

func valueSymbolP(v Value) bool {
	_, ok := v.(Symbol)
	return ok
}

type Primitive int

// TODO
func valuePrimitive(i byte, j byte) Primitive {
	return Primitive(i)
}

type Procedure struct{}

func valueProcedureP(v Value) bool {
	_, ok := v.(*Procedure)
	return ok
}

type Closure struct {
	code []byte
	pos  int
	env  Value
}

func valueMakeClosure(code []byte, pos int, env Value) Value {
	return &Closure{
		code: code,
		pos:  pos,
		env:  env,
	}
}

type VM struct {
	env  Value
	val  Value
	arg1 Value
	arg2 Value
	fun  Value

	global []Value

	stack *stack
	code  []byte
	pc    int
}

func New() *VM {
	return &VM{
		stack: new(stack),
	}
}

func (vm *VM) Run(code []byte) error {
	vm.code = code

	for {
		opcode := vm.code[vm.pc]
		if opcode == 20 {
			break
		}

		instruct := instructions[opcode]
		err := instruct(vm)
		if err != nil {
			return err
		}
	}

	return nil
}

func (vm *VM) Value() Value {
	return vm.val
}

func _CONSTANT(vm *VM) error {
	switch vm.code[vm.pc] {
	case 10:
		vm.val = true
		vm.pc += 1
	case 11:
		vm.val = false
		vm.pc += 1

	case 12:
		vm.val = nil
		vm.pc += 1
	case 80:
		vm.val = -1
		vm.pc += 1
	case 81:
		vm.val = 0
		vm.pc += 1
	case 82:
		vm.val = 1
		vm.pc += 1
	case 83:
		vm.val = 2
		vm.pc += 1
	case 84:
		vm.val = 3
		vm.pc += 1
	case 79:
		vm.val = int(vm.code[vm.pc+1])
		vm.pc += 1
	default:
		return errors.New("常量错误")
	}
	return nil
}

func _PUSH_VALUE(vm *VM) error {
	vm.stack.Push(vm.val)
	vm.pc++
	return nil
}

func _POP_FUNCTION(vm *VM) error {
	vm.val = vm.stack.Pop()
	vm.pc++
	return nil
}

func _PRESERVE_ENV(vm *VM) error {
	vm.stack.Push(vm.env)
	vm.pc++
	return nil
}

func _RESTORE_ENV(vm *VM) error {
	env := vm.stack.Pop()
	vm.env = env
	return nil
}

func _FINISH(vm *VM) error {
	return nil
}

func _CREATE_CLOSURE(vm *VM) error {
	offset := int(vm.code[vm.pc+1])
	vm.val = valueMakeClosure(vm.code, vm.pc+offset+2, vm.env)
	vm.pc += 2
	return nil
}

func _FUNCTION_INVOKE(vm *VM) error {
	fun := vm.fun

	if clo, ok := fun.(*Closure); ok {
		vm.stack.Push(vm.code)
		vm.stack.Push(vm.pc)

		vm.code = clo.code
		vm.pc = clo.pos
		vm.env = clo.env
	} else if primitiveIdx, ok := fun.(byte); ok {
		switch primitiveIdx {
		case 10:
		case 13:
		// cons
		case 14:
		// car
		case 15:
		// cdr
		case 16:
		// pair?
		case 17:
		// symbol?
		case 18:
		// eq?
		case 19:
		default:
			break
			// TODO
		}
	} else {
		return errors.New("没有这个primitive额")
	}
	return nil
}

func _INVOKE0(vm *VM) error {
	switch vm.code[vm.pc] {
	case 88:
	//(read)
	case 89:
	// (newline)
	default:
		return errors.New("没有这个啦")
	}
	return nil
}

func _INVOKE1(vm *VM) error {
	switch vm.code[vm.pc] {
	case 90:
		vm.val = valueCar(vm.val)
		break
	case 91:
		vm.val = valueCdr(vm.val)
		break
	case 92:
		vm.val = valuePairP(vm.val)
	case 93:
		vm.val = valueSymbolP(vm.val)
	case 94:
		// TODO
		// vm.val = sexp_display(NULL, vm.val, NULL);
		break
	case 95:
		vm.val = valueProcedureP(vm.val)
	case 96:
		if vm.val == nil {
			vm.val = true
		} else {
			vm.val = false
		}
	case 97:
		// TODO continuation
		break
	case 98:
		// TODO
		//			vm.val = (vm.val == SEXP_EOF) ? true : false;
		break
	default:
		return errors.New("invoke1调用出错了")
	}
	vm.pc++
	return nil
}

func _INVOKE2(vm *VM) error {
	switch vm.code[vm.pc] {
	case 100: // cons
		vm.val = &Cons{vm.arg1, vm.val}
		break
	case 101: // eq?
		if vm.val == vm.arg1 {
			vm.val = true
		} else {
			vm.val = false
		}
	case 102: // set-car!
		setCar(vm.arg1, vm.val)
	case 103: // set-cdr!
		setCdr(vm.arg1, vm.val)
	case 104: // +
		v1, ok1 := vm.arg1.(int)
		v2, ok2 := vm.val.(int)
		if !ok1 || !ok2 {
			return errors.New("参数不是int不能加")
		}
		vm.val = v1 + v2
	case 105: // -
		v1, ok1 := vm.arg1.(int)
		v2, ok2 := vm.val.(int)
		if !ok1 || !ok2 {
			return errors.New("参数不是int不能加")
		}
		vm.val = v1 - v2
	case 106: // =
		vm.val = vm.val == vm.arg1
	case 107: // >
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 > v2 {
			vm.val = true
		} else {
			vm.val = false
		}
	case 108: // <
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 < v2 {
			vm.val = true
		} else {
			vm.val = false
		}
	case 109: // *
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 > v2 {
			vm.val = true
		} else {
			vm.val = false
		}

		vm.val = v1 * v2
	case 110: // <=
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 <= v2 {
			vm.val = true
		} else {
			vm.val = false
		}
	case 111: // >=
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 >= v2 {
			vm.val = true
		} else {
			vm.val = false
		}
	case 112: // %
		v1, ok1 := vm.val.(int)
		v2, ok2 := vm.arg1.(int)
		if !ok1 || !ok2 {
			return errors.New("参数类型断言错误")
		}
		if v1 >= v2 {
			vm.val = true
		} else {
			vm.val = false
		}
		vm.val = v1 % v2
	default:
		return errors.New("invoke2未定义的primitive")
	}
	return nil
}

func _RETURN(vm *VM) error {
	vm.pc = vm.stack.Pop().(int)
	vm.code = vm.stack.Pop().([]byte)
	return nil
}

func activation_frame_argument(env Value, i int) Value {
	vec := valueCar(env)
	return vec.([]Value)[i]
}

func _SHALLOW_ARGUMENT_REF(vm *VM) error {
	code := vm.code[vm.pc]

	if code >= byte(1) && code < byte(5) {
		vm.val = activation_frame_argument(vm.env, int(code)-1)
		vm.pc++
	} else if code == 5 {
		vm.val = activation_frame_argument(vm.env, int(vm.code[code+1]))
		vm.pc += 2
	}
	return nil
}

func _SET_SHALLOW_ARGUMENT(vm *VM) error {
	code := vm.code[vm.pc]

	if code >= 21 && code < 25 {
		vec := valueCar(vm.env).([]Value)
		vec[code-21] = vm.val
		vm.val = valueVOID
		vm.pc++
	} else if code == 25 {
		vec := valueCar(vm.env).([]Value)
		vec[vm.code[vm.pc+1]] = vm.val
		vm.val = valueVOID
		vm.pc += 2
	}
	return nil
}

func _DEEP_ARGUMENT_REF(vm *VM) error {
	i := int(vm.code[vm.pc+1])
	j := int(vm.code[vm.pc+2])
	vm.val = deepFetch(vm.env, i, j)
	vm.pc += 3
	return nil
}

// TODO
func deepFetch(env Value, i int, j int) Value {
	return nil
}

// TODO
func deepUpdate(env Value, i int, j int, v Value) {

}

func _SET_DEEP_ARGUMENT(vm *VM) error {
	i := int(vm.code[vm.pc+1])
	j := int(vm.code[vm.pc+2])
	deepUpdate(vm.env, i, j, vm.val)
	vm.val = valueVOID
	vm.pc += 3
	return nil
}

func _PREDEFINED(vm *VM) error {
	op := vm.code[vm.pc]

	if op >= 10 && op < 19 {
		vm.val = valuePrimitive(op, 0)
		vm.pc++
	} else if op == 19 {
		vm.val = valuePrimitive(op, vm.code[vm.pc+1])
		vm.pc += 2
	}
	return nil
}

func _CHECKED_GLOBAL_REF(vm *VM) error {
	i := vm.code[vm.pc+1]
	vm.val = vm.global[i]
	if vm.val == valueVOID {
		return errors.New("Uninitialized global variable")
	}
	vm.pc += 2
	return nil
}

func _GLOBAL_REF(vm *VM) error {
	i := vm.code[vm.pc+1]
	vm.val = vm.global[i]
	vm.pc += 2
	return nil
}

func _SET_GLOBAL(vm *VM) error {
	i := vm.code[vm.pc+1]
	vm.global[i] = vm.val
	vm.val = valueVOID
	vm.pc += 2
	return nil
}

func _GOTO(vm *VM) error {
	var offset2 int
	offset1 := int(vm.code[vm.pc+1])
	op := vm.code[vm.pc]

	if op == 30 {
		vm.pc += offset1
		vm.pc += 2
	} else if op == 28 {
		offset2 = int(vm.code[vm.pc+2])
		vm.pc += (offset1 + offset2*255)
		vm.pc += 3
	}
	return nil
}

func valueNotP(val Value) bool {
	b, ok := val.(bool)
	if ok && b == false {
		return true
	}
	return false
}

func _JUMP_FALSE(vm *VM) error {
	op := vm.code[vm.pc]

	if op == 31 {
		if valueNotP(vm.val) {
			offset := int(vm.code[vm.pc+1])
			vm.pc += offset
		}
		vm.pc += 2
	} else if op == 29 {
		if valueNotP(vm.val) {
			offset1 := int(vm.code[vm.pc+1])
			offset2 := int(vm.code[vm.pc+2])
			vm.pc += (offset1 + offset2*256)
		}
		vm.pc += 3
	}
	return nil
}

func _ALLOCATE_DOTTED_FRAME(vm *VM) error {
	arity := vm.code[vm.pc+1]
	tmp := make([]Value, arity)
	tmp[arity-1] = valueVOID
	vm.val = tmp
	vm.pc += 2
	return nil
}

func _ALLOCATE_FRAME(vm *VM) error {
	pc := vm.code[vm.pc]

	if pc >= 50 && pc < 55 {
		vm.val = make([]Value, pc-50)
		vm.pc++
	} else if pc == 55 {
		vm.val = make([]Value, vm.code[pc+1])
		vm.pc += 2
	}
	return nil
}

func set_activation_frame_argument(vec Value, pos int, v Value) {
	vec.([]Value)[pos] = v
}

func _POP_FRAME(vm *VM) error {
	pc := int(vm.code[vm.pc])
	top := vm.stack.Pop()

	if pc >= 60 && pc < 64 {
		set_activation_frame_argument(vm.val, pc-60, top)
		vm.pc++
	} else if pc == 64 {
		set_activation_frame_argument(vm.val, int(vm.code[pc+1]), top)
		vm.pc += 2
	}
	return nil
}

func _POP_CONS_FRAME(vm *VM) error {
	arity := int(vm.code[vm.pc+1])
	var tmp Value

	v := vm.stack.Pop()
	tmp = &Cons{v, vm.val.([]Value)[arity]}
	set_activation_frame_argument(vm.val, arity, tmp)
	vm.pc += 2
	return nil
}

func _EXTEND_ENV(vm *VM) error {
	vm.env = &Cons{vm.env, vm.val}
	vm.pc++
	return nil
}

func _POP_ARG1(vm *VM) error {
	vm.arg1 = vm.stack.Pop()
	vm.pc++
	return nil
}

func _POP_ARG2(vm *VM) error {
	vm.arg2 = vm.stack.Pop()
	vm.pc++
	return nil
}

func _PACK_FRAME(vm *VM) error {
	arity := int(vm.code[vm.pc+1])
	var tmp Value
	for i := arity; i < len(vm.val.([]Value)); i++ {
		tmp = &Cons{vm.val.([]Value)[i], tmp}
	}
	set_activation_frame_argument(vm.val, arity, tmp)
	vm.pc += 2
	return nil
}

func _UNLINK_ENV(vm *VM) error {
	vm.env = valueCdr(vm.env)
	vm.pc++
	return nil
}

func _ARITYEQ(vm *VM) error {
	op := int(vm.code[vm.pc])
	if op >= 71 && op < 75 {
		if len(vm.val.([]Value)) != op-70 {
			return errors.New("函数接受参数不一样")
		}
		vm.pc++
	} else if op == 75 {
		if len(vm.val.([]Value)) != int(vm.code[vm.pc+1]) {
			return errors.New("函数接受参数不一样")
		}
		vm.pc += 2
	}
	return errors.New("arityq不对")
}

var (
	instructions [256]func(*VM) error
)

func init() {
	instructions[1] = _SHALLOW_ARGUMENT_REF
	instructions[2] = _SHALLOW_ARGUMENT_REF
	instructions[3] = _SHALLOW_ARGUMENT_REF
	instructions[4] = _SHALLOW_ARGUMENT_REF
	instructions[5] = _SHALLOW_ARGUMENT_REF
	instructions[6] = _DEEP_ARGUMENT_REF
	instructions[7] = _GLOBAL_REF
	instructions[8] = _CHECKED_GLOBAL_REF
	//
	instructions[10] = _CONSTANT
	instructions[11] = _CONSTANT
	instructions[12] = _CONSTANT
	instructions[13] = _PREDEFINED
	instructions[14] = _PREDEFINED
	instructions[15] = _PREDEFINED
	instructions[16] = _PREDEFINED
	instructions[17] = _PREDEFINED
	instructions[18] = _PREDEFINED
	instructions[19] = _PREDEFINED
	instructions[20] = _FINISH
	instructions[21] = _SET_SHALLOW_ARGUMENT
	instructions[22] = _SET_SHALLOW_ARGUMENT
	instructions[23] = _SET_SHALLOW_ARGUMENT
	instructions[24] = _SET_SHALLOW_ARGUMENT
	instructions[25] = _SET_SHALLOW_ARGUMENT
	instructions[26] = _SET_DEEP_ARGUMENT
	instructions[27] = _SET_GLOBAL
	instructions[28] = _GOTO
	instructions[29] = _JUMP_FALSE
	instructions[30] = _GOTO
	instructions[31] = _JUMP_FALSE
	instructions[32] = _EXTEND_ENV
	instructions[33] = _UNLINK_ENV
	instructions[34] = _PUSH_VALUE
	instructions[35] = _POP_ARG1
	instructions[36] = _POP_ARG2
	instructions[37] = _PRESERVE_ENV
	instructions[38] = _RESTORE_ENV
	instructions[39] = _POP_FUNCTION
	instructions[40] = _CREATE_CLOSURE
	//
	instructions[43] = _RETURN
	instructions[44] = _PACK_FRAME
	instructions[45] = _FUNCTION_INVOKE
	//
	instructions[47] = _POP_CONS_FRAME
	//
	//
	instructions[50] = _ALLOCATE_FRAME
	instructions[51] = _ALLOCATE_FRAME
	instructions[52] = _ALLOCATE_FRAME
	instructions[53] = _ALLOCATE_FRAME
	instructions[54] = _ALLOCATE_FRAME
	instructions[55] = _ALLOCATE_FRAME
	instructions[56] = _ALLOCATE_DOTTED_FRAME
	//
	instructions[60] = _POP_FRAME
	instructions[61] = _POP_FRAME
	instructions[62] = _POP_FRAME
	instructions[63] = _POP_FRAME
	instructions[64] = _POP_FRAME
	//
	instructions[71] = _ARITYEQ
	instructions[72] = _ARITYEQ
	instructions[73] = _ARITYEQ
	instructions[74] = _ARITYEQ
	instructions[75] = _ARITYEQ
	//
	instructions[79] = _CONSTANT
	instructions[80] = _CONSTANT
	instructions[81] = _CONSTANT
	instructions[82] = _CONSTANT
	instructions[83] = _CONSTANT
	instructions[84] = _CONSTANT
	//
	instructions[89] = _INVOKE0
	instructions[89] = _INVOKE0
	instructions[90] = _INVOKE1
	instructions[91] = _INVOKE1
	instructions[92] = _INVOKE1
	instructions[93] = _INVOKE1
	instructions[94] = _INVOKE1
	instructions[95] = _INVOKE1
	instructions[96] = _INVOKE1
	instructions[97] = _INVOKE1
	instructions[98] = _INVOKE1
	//
	instructions[100] = _INVOKE2
	instructions[101] = _INVOKE2
	instructions[102] = _INVOKE2
	instructions[103] = _INVOKE2
	instructions[104] = _INVOKE2
	instructions[105] = _INVOKE2
	instructions[106] = _INVOKE2
	instructions[107] = _INVOKE2
	instructions[108] = _INVOKE2
	instructions[109] = _INVOKE2
	instructions[110] = _INVOKE2
	instructions[111] = _INVOKE2
	instructions[112] = _INVOKE2
}
