use hare::ast;
use hare::lex;
use hare::parse;
use hare::types;
use io;
use strings;

type signed = (int | i8 | i16 | i32 | i64);
// equivalent to:
// type signed = (i64 | (i32 | (i16 | (i8 | int | int | int))));
type unsigned = (uint | u8 | u16 | u32 | u64);
type integer = (...unsigned | ...signed);
type floating = (f32 | f64);
type numeric = (...integer | ...floating);

type numeric_repr = struct {
	id: u32,
	union {
		_int: int,
		_i8: i8,
		_i16: i16,
		_i32: i32,
		_i64: i64,
		// ...
	},
};

export fn main() void = {
	const input = bufio::fixed(strings::toutf8("int"), io::mode::READ);
	const lexer = lex::init(&input, "<string>");
	const _type = parse::_type(&lexer)!;
	defer ast::type_finish(&_type);
	const store = types::store(types::x86_64, null, null);
	defer types::store_free(store);
	const itype = types::lookup(store, &_type) as const *types::_type;

	const obj: numeric = 1337;
	const ptr = &obj: *numeric_repr;
	assert(ptr.id == itype.id);
	assert(ptr._int == 1337);
};