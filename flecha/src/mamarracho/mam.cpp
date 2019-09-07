#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <map>
#include <cassert>
#include <cstring>

using namespace std;

// Helper functions

typedef signed long long int i64;
typedef unsigned long long int u64;

void fail(const string& msg) {
	cerr << "*** Mamarracho ***" << endl;
	cerr << msg << endl;
	exit(1);
}

void usage(const string& program) {
	cerr << "Usage:" << endl;
	cerr << "  " << program;
	cerr << " input.mam        Run program." << endl;
	cerr << "  " << program;
	cerr << " -p input.mam     Print program." << endl;
	exit(1);
}

string int_to_string(int n) {
	ostringstream s;
	s << n;
	return s.str();
}

string i64_to_string(i64 n) {
	ostringstream s;
	s << n;
	return s.str();
}

string u64_to_string(u64 n) {
	ostringstream s;
	s << n;
	return s.str();
}

string char_to_string(char c) {
	char s[2];
	s[0] = c;
	s[1] = '\0';
	return string(s);
}

// Instructions

typedef string Reg;
typedef string Label;

typedef int Opcode;
const Opcode OP_label = 1;
const Opcode OP_mov_reg = 2;
const Opcode OP_mov_int = 3;
const Opcode OP_mov_label = 4;
const Opcode OP_alloc = 5;
const Opcode OP_load = 6;
const Opcode OP_store = 7;
const Opcode OP_print_char = 8;
const Opcode OP_print = 9;
const Opcode OP_jump = 10;
const Opcode OP_jump_eq = 11;
const Opcode OP_jump_lt = 12;
const Opcode OP_add = 13;
const Opcode OP_sub = 14;
const Opcode OP_mul = 15;
const Opcode OP_div = 16;
const Opcode OP_mod = 17;
const Opcode OP_call = 18;
const Opcode OP_icall = 19;
const Opcode OP_return = 20;

const Opcode OPCODE_MIN = OP_label;
const Opcode OPCODE_MAX = OP_return;

string opcode_name(Opcode op) {
	switch (op) {
		case OP_label: return "label";
		case OP_mov_reg: return "mov_reg";
		case OP_mov_int: return "mov_int";
		case OP_mov_label: return "mov_label";
		case OP_alloc: return "alloc";
		case OP_load: return "load";
		case OP_store: return "store";
		case OP_print_char: return "print_char";
		case OP_print: return "print";
		case OP_jump: return "jump";
		case OP_jump_eq: return "jump_eq";
		case OP_jump_lt: return "jump_lt";
		case OP_add: return "add";
		case OP_sub: return "sub";
		case OP_mul: return "mul";
		case OP_div: return "div";
		case OP_mod: return "mod";
		case OP_call: return "call";
		case OP_icall: return "icall";
		case OP_return: return "return";
		default:
			fail("Opcode name not implemented for " +
				int_to_string(op));
			return "";
	}
}

string opcode_paramTypes(Opcode op) {
	switch (op) {
		case OP_label: return "l";
		case OP_mov_reg: return "rr";
		case OP_mov_int: return "ri";
		case OP_mov_label: return "rl";
		case OP_alloc: return "ru";
		case OP_load: return "rru";
		case OP_store: return "rur";
		case OP_print_char: return "r";
		case OP_print: return "r";
		case OP_jump: return "l";
		case OP_jump_eq: return "rrl";
		case OP_jump_lt: return "rrl";
		case OP_add: return "rrr";
		case OP_sub: return "rrr";
		case OP_mul: return "rrr";
		case OP_div: return "rrr";
		case OP_mod: return "rrr";
		case OP_call: return "l";
		case OP_icall: return "r";
		case OP_return: return "";
		default:
			fail("Opcode paramTypes not implemented for " +
				opcode_name(op));
			return "";
	}
}

class Instruction {
public:
	Instruction(Opcode op);
	Opcode opcode() const;

	const Reg& regParam(int i) const;
	i64 i64Param(int i) const;
	u64 u64Param(int i) const;
	const Label& labelParam(int i) const;

	int numRegParams() const;
	int numI64Params() const;
	int numU64Params() const;
	int numLabelParams() const;

	void addRegParam(Reg reg);
	void addI64Param(i64 x);
	void addU64Param(u64 x);
	void addLabelParam(Label label);
private:
	Opcode _op;
	vector<Reg> _regParams;
	vector<i64> _i64Params;
	vector<i64> _u64Params;
	vector<Label> _labelParams;
};

Instruction::Instruction(Opcode op) : _op(op) {
}

Opcode Instruction::opcode() const {
	return _op;
}

const Reg& Instruction::regParam(int i) const {
	return _regParams[i];
}

i64 Instruction::i64Param(int i) const {
	return _i64Params[i];
}

u64 Instruction::u64Param(int i) const {
	return _u64Params[i];
}

const Label& Instruction::labelParam(int i) const {
	return _labelParams[i];
}

int Instruction::numRegParams() const {
	return _regParams.size();
}

int Instruction::numI64Params() const {
	return _i64Params.size();
}

int Instruction::numU64Params() const {
	return _u64Params.size();
}

int Instruction::numLabelParams() const {
	return _labelParams.size();
}

void Instruction::addRegParam(Reg reg) {
	_regParams.push_back(reg);
}

void Instruction::addI64Param(i64 x) {
	_i64Params.push_back(x);
}

void Instruction::addU64Param(u64 x) {
	_u64Params.push_back(x);
}

void Instruction::addLabelParam(Label label) {
	_labelParams.push_back(label);
}

ostream& operator<<(ostream& os, const Instruction& instr) {
	if (instr.opcode() == OP_label) {
		os << instr.labelParam(0) << ":";
	} else {
		string paramTypes = opcode_paramTypes(instr.opcode());
		os << opcode_name(instr.opcode()) << "(";
		int pr = 0;
		int pi = 0;
		int pu = 0;
		int pl = 0;
		for (unsigned int i = 0; i < paramTypes.size(); i++) {
			char paramType = paramTypes[i];
			if (i > 0) {
				os << ", ";
			}
			switch (paramType) {
			case 'r':
				os << "$" << instr.regParam(pr++);
				break;
			case 'i':
				os << instr.i64Param(pi++);
				break;
			case 'u':
				os << instr.u64Param(pu++);
				break;
			case 'l':
				os << instr.labelParam(pl++);
				break;
			default:
				assert(false);
			}
		}
		os << ")";
	}
	return os;
}

// Program

class Program {
public:
	Program();
	void addInstruction(Instruction instruction);
	const Instruction& instruction(int i) const;
	u64 size() const;
	u64 labelLocation(const Label& label) const;
	void lint();
private:
	vector<Instruction> _instructions;
	map<Label, u64> _labels;

	void _fail(const string& msg) const;
};

Program::Program() {
}

void Program::addInstruction(Instruction instruction) {
	if (instruction.opcode() == OP_label) {
		Label label = instruction.labelParam(0);
		u64 position = _instructions.size();
		if (_labels.find(label) != _labels.end()) {
			_fail("Repeated label \"" + label + "\".");
		}
		_labels[label] = position;
	}
	_instructions.push_back(instruction);
}

u64 Program::size() const {
	return _instructions.size();
}

const Instruction& Program::instruction(int i) const {
	return _instructions[i];
}

u64 Program::labelLocation(const Label& label) const {
	if (_labels.find(label) == _labels.end()) {
		_fail("Undefined label \"" + label + "\".");
	}
	return _labels.at(label);
}

void Program::lint() {
	// Check that all referenced labels exist
	for (u64 i = 0; i < size(); i++) {
		const Instruction& instr = instruction(i);
		for (int j = 0; j < instr.numLabelParams(); j++) {
			Label label = instr.labelParam(j);
			if (_labels.find(label) == _labels.end()) {
				_fail("Undefined label \"" + label + "\".");
			}
		}
	}
}

void Program::_fail(const string& msg) const {
	fail("Semantic error.\n" + msg);
}

ostream& operator<<(ostream& os, const Program& p) {
	for (u64 i = 0; i < p.size(); i++) {
		os << p.instruction(i) << endl;
	}
	return os;
}

// Reader

class Reader {
public:
	Reader(const string& filename);
	~Reader();
	Program readProgram();
private:
	istream& _is;
	string _filename;
	int _row;
	int _col;

	bool _isPrintable(char c);
	bool _isAlpha(char c);
	bool _isDigit(char c);
	bool _isIdent(char c);
	bool _isWhitespace(char c);
	void _match(char expected, const string& errmsg);
	string _currentChar();
	string _readName();
	string _readDigits();
	string _readIdentifier();
	Reg _readRegister();
	Label _readLabel();
	i64 _readI64();
	u64 _readU64();
	Instruction _readInstruction();
	Instruction _readInstrParams(Opcode op, const string& paramTypes);
	void _skipComment();
	void _skipWhitespaceAndComments();
	bool _eof();
	void _fail(const string& msg);
};

Reader::Reader(const string& filename) :
	_is(*new ifstream(filename.c_str())), _filename(filename), _row(1), _col(1)
{
	if (!_is.good()) {
		fail("File \"" + filename + "\" does not exist."); 
	}
}

Reader::~Reader() {
	delete &_is;
}

Program Reader::readProgram() {
	Program program;
	while (true) {
		_skipWhitespaceAndComments();
		if (!_is.good()) {
			break;
		}
		program.addInstruction(_readInstruction());
	}
	program.lint();
	return program;
}

bool Reader::_isPrintable(char c) {
	return 32 <= c && c <= 127;
}

bool Reader::_isAlpha(char c) {
	return ('A' <= c && c <= 'Z')
	    || ('a' <= c && c <= 'z')
	    || c == '_';
}

bool Reader::_isDigit(char c) {
	return '0' <= c && c <= '9';
}

bool Reader::_isIdent(char c) {
	return _isAlpha(c) || _isDigit(c);
}

bool Reader::_isWhitespace(char c) {
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

void Reader::_match(char expected, const string& errmsg) {
	_skipWhitespaceAndComments();
	if (_is.good() && _is.peek() == expected) {
		_is.get();
		_col++;
	} else {
		_fail(errmsg);
	}
}

string Reader::_currentChar() {
	if (_is.good() && _isPrintable(_is.peek())) {
		return "'" + char_to_string(_is.peek()) + "'";
	} else if (_is.good()) {
		return "chr(" + int_to_string(_is.peek()) + ")";
	} else {
		return "end of file";
	}
}

string Reader::_readName() {
	string id = "";
	while (_is.good() && _isIdent(_is.peek())) {
		id += _is.get();
		_col++;
	}
	return id;
}

string Reader::_readDigits() {
	string id = "";
	while (_is.good() && _isDigit(_is.peek())) {
		id += _is.get();
		_col++;
	}
	return id;
}

string Reader::_readIdentifier() {
	_skipWhitespaceAndComments();
	if (!_is.good() || !_isAlpha(_is.peek())) {
		_fail(
		  "Expected an identifier, "
		  "but found " + _currentChar() + ".\n"
		  "Identifiers should be of the form [_A-Za-z][_A-Za-z0-9]*."
		);
	}
	return _readName();
}

Reg Reader::_readRegister() {
	_skipWhitespaceAndComments();
	if (!_is.good() || (_is.peek() != '$' && _is.peek() != '@')) {
		_fail(
		  "Expected a register, "
		  "but found " + _currentChar() + ".\n"
		  "Registers should be of the form [@$][_A-Za-z0-9]+."
		);
	}
	char sigil = _is.get();
	_col++;
	return char_to_string(sigil) + _readName();
}

Label Reader::_readLabel() {
	return _readIdentifier();
}

i64 Reader::_readI64() {
	_skipWhitespaceAndComments();
	string sign = "";
	if (_is.good() && _is.peek() == '-') {
		sign = "-";
		_is.get();
		_col++;
	}
	if (!_is.good() || !_isDigit(_is.peek())) {
		_fail(
		  "Expected a signed integer, "
		  "but found " + _currentChar() + "."
		);
	}
	string digits = sign + _readDigits();
	i64 x;
	stringstream(digits) >> x;
	if (i64_to_string(x) != digits) {
		_fail(
		  "Invalid signed integer constant \"" + digits + "\".\n"
		  "Signed integers should be in the range -2^63..2^63-1."
		);
	}
	return x;
}

u64 Reader::_readU64() {
	_skipWhitespaceAndComments();
	if (!_is.good() || !_isDigit(_is.peek())) {
		_fail(
		  "Expected an unsigned integer, "
		  "but found " + _currentChar() + "."
		);
	}
	string digits = _readDigits();
	u64 x;
	stringstream(digits) >> x;
	if (u64_to_string(x) != digits) {
		_fail(
		  "Invalid unsigned integer constant \"" + digits + "\".\n"
		  "Unsigned integers should be in the range 0..2^64-1."
		);
	}
	return x;
}

Instruction Reader::_readInstruction() {
	string id = _readIdentifier();
	if (_is.good() && _is.peek() == ':') {
		Instruction instruction(OP_label);
		_is.get();
		_col++;
		instruction.addLabelParam(id);
		return instruction;
	}
	for (Opcode op = OPCODE_MIN; op <= OPCODE_MAX; ++op) {
		if (id == opcode_name(op)) {
			return _readInstrParams(op, opcode_paramTypes(op));
		}
	}
	_fail("Invalid instruction: \"" + id + "\".");
	return Instruction(OPCODE_MIN);
}

Instruction Reader::_readInstrParams(Opcode op, const string& paramTypes) {
	Instruction instruction(op);

	bool with_parens = false;
	_skipWhitespaceAndComments();
	if (_is.good() && _is.peek() == '(') {
		with_parens = true;
		_is.get();
		_col++;
	}
	for (unsigned int i = 0; i < paramTypes.size(); i++) {
		char paramType = paramTypes[i];
		if (i > 0) {
			_match(',',
			  "Expected separator ','.\n"
			  "Note: "
			  + opcode_name(op) + " takes "
			  + int_to_string(paramTypes.size()) + " arguments."
			);
		}
		switch (paramType) {
		case 'r':
			instruction.addRegParam(_readRegister());
			break;
		case 'i':
			instruction.addI64Param(_readI64());
			break;
		case 'u':
			instruction.addU64Param(_readU64());
			break;
		case 'l':
			instruction.addLabelParam(_readLabel());
			break;
		default:
			assert(false);
		}
	}
	if (with_parens) {
		_match(')', "Expected closing ')'.");
	}
	return instruction;
}

void Reader::_skipComment() {
	while (_is.good() && _is.peek() != '\n') {
		_is.get();
		_col++;
	}
}

void Reader::_skipWhitespaceAndComments() {
	while (_is.good()) {
 		if (_is.peek() == '\n') {
			_row++;
			_col = 1;
			_is.get();
		} else if (_isWhitespace(_is.peek())) {
			_is.get();
			_col++;
		} else if (_is.peek() == '%') {
			_skipComment();
		} else {
			break;
		}
	}
}

bool Reader::_eof() {
	_skipWhitespaceAndComments();
	return !_is.good();
}

void Reader::_fail(const string& msg) {
	fail(
	  "Syntax error near \"" + _filename + "\""
	  + " (line " + int_to_string(_row) + ","
	  + " column " + int_to_string(_col) + ").\n" +
	  msg
	);
}

// Virtual machine

typedef enum {
	VInt,
	VPtr,
	VLoc,
} ValueTag;

class Value {
public:
	Value();
	Value(ValueTag tag, i64 vint);

	ValueTag tag() const;
	i64 vi64() const;
	u64 vu64() const;
private:
	ValueTag _tag;
	i64 _vint;
};

Value::Value() : _tag(VInt), _vint(0) {
}

Value::Value(ValueTag tag, i64 vint) : _tag(tag), _vint(vint) {
}

ValueTag Value::tag() const {
	return _tag;
}

i64 Value::vi64() const {
	return _vint;
}

u64 Value::vu64() const {
	return (u64)_vint;
}

ostream& operator<<(ostream& os, Value v) {
	switch (v.tag()) {
	case VInt:
		os << v.vi64();
		break;
	case VPtr:
		os << "VPtr(" << v.vu64() << ")";
		break;
	case VLoc:
		os << "VLoc(" << v.vu64() << ")";
		break;
	}
	return os;
}

typedef vector<Value> Cell;
typedef map<Reg, Value> Env;

class VM {
public:
	VM(const Program& program);
	void run();
	~VM();
private:
	const Program& _program;
	Env* _local_env;
	Env _global_env;
	vector<Cell> _memory;
	vector<u64> _call_stack;
	vector<Env*> _dump;
	u64 _ip;

	Value& _env(const Reg& r);

	void _step();
	void _check_int(const Reg& r, const string& errmsg);
	void _fail(const string& msg);

	void _mov_reg(const Reg& r1, const Reg& r22);
	void _mov_int(const Reg& r, i64 n);
	void _mov_label(const Reg& r, const Label& l);
	void _alloc(const Reg& r, u64 n);
	void _load(const Reg& r1, const Reg& r2, u64 i);
	void _store(const Reg& r1, u64 i, const Reg& r2);
	void _print_char(const Reg& r);
	void _print(const Reg& r);
	void _jump(const Label& l);
	void _jump_eq(const Reg& r1, const Reg& r2, const Label& l);
	void _jump_lt(const Reg& r1, const Reg& r2, const Label& l);
	void _add(const Reg& r1, const Reg& r2, const Reg& r3);
	void _sub(const Reg& r1, const Reg& r2, const Reg& r3);
	void _mul(const Reg& r1, const Reg& r2, const Reg& r3);
	void _div(const Reg& r1, const Reg& r2, const Reg& r3);
	void _mod(const Reg& r1, const Reg& r2, const Reg& r3);
	void _call_loc(u64 loc);
	void _call(const Label& l);
	void _icall(const Reg& r);
	void _return();
};

VM::VM(const Program& program) : _program(program), _ip(0) {
	_local_env = new Env();
}

void VM::run() {
	while (_ip < _program.size()) {
		_step();
	}
}

VM::~VM() {
	for (int i = 0; i < _dump.size(); i++) {
		delete _dump[i];
	}
	delete _local_env;
}

void VM::_step() {
	if (_ip >= _program.size()) {
		_fail("Program terminated.");
	}
	Instruction instr = _program.instruction(_ip++);
	switch (instr.opcode()) {
		case OP_label:
			/* Nop */
			break;
		case OP_mov_reg:
			_mov_reg(instr.regParam(0), instr.regParam(1));
			break;
		case OP_mov_int:
			_mov_int(instr.regParam(0), instr.i64Param(0));
			break;
		case OP_mov_label:
			_mov_label(instr.regParam(0), instr.labelParam(0));
			break;
		case OP_alloc:
			_alloc(instr.regParam(0), instr.u64Param(0));
			break;
		case OP_load:
			_load(instr.regParam(0),
			      instr.regParam(1),
			      instr.u64Param(0));
			break;
		case OP_store:
			_store(instr.regParam(0),
			       instr.u64Param(0),
		               instr.regParam(1));
			break;
		case OP_print_char:
			_print_char(instr.regParam(0));
			break;
		case OP_print:
			_print(instr.regParam(0));
			break;
		case OP_jump:
			_jump(instr.labelParam(0));
			break;
		case OP_jump_eq:
			_jump_eq(instr.regParam(0),
				 instr.regParam(1),
				 instr.labelParam(0));
			break;
		case OP_jump_lt:
			_jump_lt(instr.regParam(0),
				 instr.regParam(1),
				 instr.labelParam(0));
			break;
		case OP_add:
			_add(instr.regParam(0),
			     instr.regParam(1),
			     instr.regParam(2));
			break;
		case OP_sub:
			_sub(instr.regParam(0),
			     instr.regParam(1),
			     instr.regParam(2));
			break;
		case OP_mul:
			_mul(instr.regParam(0),
			     instr.regParam(1),
			     instr.regParam(2));
			break;
		case OP_div:
			_div(instr.regParam(0),
			     instr.regParam(1),
			     instr.regParam(2));
			break;
		case OP_mod:
			_mod(instr.regParam(0),
			     instr.regParam(1),
			     instr.regParam(2));
			break;
		case OP_call:
			_call(instr.labelParam(0));
			break;
		case OP_icall:
			_icall(instr.regParam(0));
			break;
		case OP_return:
			_return();
			break;
		default:
			_fail("Opcode \"" +
			      opcode_name(instr.opcode()) +
			      "\" not implemented.\n");
	}
}

Value& VM::_env(const Reg& r) {
	assert(r[0] == '@' || r[0] == '$');
	if (r[0] == '@') {
		return _global_env[r];
	} else if (r[0] == '$') {
		return (*_local_env)[r];
	}
}

void VM::_mov_reg(const Reg& r1, const Reg& r2) {
	_env(r1) = _env(r2);
}

void VM::_mov_int(const Reg& r, i64 n) {
	_env(r) = Value(VInt, n);
}

void VM::_mov_label(const Reg& r, const Label& l) {
	_env(r) = Value(VLoc, _program.labelLocation(l));
}

void VM::_alloc(const Reg& r, u64 n) {
	u64 p = _memory.size();
	_env(r) = Value(VPtr, p);
	_memory.push_back(Cell(n, Value()));
}

void VM::_load(const Reg& r1, const Reg& r2, u64 i) {
	Value vr2 = _env(r2);
	if (vr2.tag() != VPtr) {
		_fail("load: Value is not a pointer.");
	}
	Cell& cell = _memory[vr2.vu64()];
	if (i >= cell.size()) {
		_fail("load: Index out of bounds.");
	}
	_env(r1) = cell[i];
}

void VM::_store(const Reg& r1, u64 i, const Reg& r2) {
	Value vr1 = _env(r1);
	if (vr1.tag() != VPtr) {
		_fail("store: Value is not a pointer.");
	}
	Cell& cell = _memory[vr1.vu64()];
	if (i >= cell.size()) {
		_fail("store: Index out of bounds.");
	}
	cell[i] = _env(r2);
}

void VM::_print_char(const Reg& r) {
	Value v = _env(r);
	cout << ((unsigned char)v.vi64());
}

void VM::_print(const Reg& r) {
	cout << _env(r);
}

void VM::_jump(const Label& l) {
	_ip = _program.labelLocation(l);
}

void VM::_jump_eq(const Reg& r1, const Reg& r2, const Label& l) {
	_check_int(r1, "jump_eq: First argument is not an integer.");
	_check_int(r2, "jump_eq: Second argument is not an integer.");
	if (_env(r1).vi64() == _env(r2).vi64()) {
		_jump(l);
	}
}

void VM::_jump_lt(const Reg& r1, const Reg& r2, const Label& l) {
	_check_int(r1, "jump_lt: First argument is not an integer.");
	_check_int(r2, "jump_lt: Second argument is not an integer.");
	if (_env(r1).vi64() < _env(r2).vi64()) {
		_jump(l);
	}
}

void VM::_add(const Reg& r1, const Reg& r2, const Reg& r3) {
	_check_int(r2, "add: Second argument is not an integer.");
	_check_int(r3, "add: Third argument is not an integer.");
	i64 a = _env(r2).vi64();
	i64 b = _env(r3).vi64();
	_env(r1) = Value(VInt, a + b);
}

void VM::_sub(const Reg& r1, const Reg& r2, const Reg& r3) {
	_check_int(r2, "sub: Second argument is not an integer.");
	_check_int(r3, "sub: Third argument is not an integer.");
	i64 a = _env(r2).vi64();
	i64 b = _env(r3).vi64();
	_env(r1) = Value(VInt, a - b);
}

void VM::_mul(const Reg& r1, const Reg& r2, const Reg& r3) {
	_check_int(r2, "mul: Second argument is not an integer.");
	_check_int(r3, "mul: Third argument is not an integer.");
	i64 a = _env(r2).vi64();
	i64 b = _env(r3).vi64();
	_env(r1) = Value(VInt, a * b);
}

void VM::_div(const Reg& r1, const Reg& r2, const Reg& r3) {
	_check_int(r2, "div: Second argument is not an integer.");
	_check_int(r3, "div: Third argument is not an integer.");
	i64 a = _env(r2).vi64();
	i64 b = _env(r3).vi64();
	if (b == 0) {
		_fail("div: Zero division.");
	}
	_env(r1) = Value(VInt, a / b);
}

void VM::_mod(const Reg& r1, const Reg& r2, const Reg& r3) {
	_check_int(r2, "mod: Second argument is not an integer.");
	_check_int(r3, "mod: Third argument is not an integer.");
	i64 a = _env(r2).vi64();
	i64 b = _env(r3).vi64();
	if (b == 0) {
		_fail("mod: Zero division.");
	}
	_env(r1) = Value(VInt, a % b);
}

void VM::_call_loc(u64 loc) {
	_call_stack.push_back(_ip);
	_dump.push_back(_local_env);
	_local_env = new Env();
	_ip = loc;
}

void VM::_call(const Label& l) {
	_call_loc(_program.labelLocation(l));
}

void VM::_icall(const Reg& r) {
	Value v = _env(r);
	if (v.tag() != VLoc) {
		_fail("icall: Value is not a location.");
	}
	_call_loc(v.vu64());
}

void VM::_return() {
	if (_call_stack.size() == 0 || _dump.size() == 0) {
		_fail("return: Empty stack.");
	}
	_ip = _call_stack.back();
	delete _local_env;
	_local_env = _dump.back();
	_call_stack.pop_back();
	_dump.pop_back();
}

void VM::_check_int(const Reg& r, const string& errmsg) {
	Value v = _env(r);
	if (v.tag() != VInt) {
		_fail(errmsg);
	}
}

void VM::_fail(const string& msg) {
	fail("Runtime error.\n" + msg);
}

// Main

int main(int argc, char** argv) {
	bool option_print_program = false;
	string filename = "";

	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-p")) {
			option_print_program = true;
		} else {
			filename = argv[i];
		}
	}

	if (filename == "") {
		usage(argv[0]);
	}

	Reader reader(filename);
	Program program = reader.readProgram();

	if (option_print_program) {
		cout << program;
		exit(0);
	} else {
		// Run program
		VM vm(program);
		vm.run();
	}

	return 0;
}

