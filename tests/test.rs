use diesel_auto_type::auto_type;

use diesel::{dsl, prelude::*};

diesel::table! {
	abc {
		id -> Integer,
	}
}

#[auto_type]
fn inline_rendering() -> _ {
	dsl::not(abc::id.eq::<i32>(1))
}

#[auto_type(type_alias)]
fn type_aliased() -> _ {
	dsl::not(abc::id.eq(1i32))
}

#[auto_type(type_case = "PascalCase")]
fn type_aliased_pascal() -> dsl::not<dsl::Eq<abc::id, i32>> {
	dsl::not(abc::id.eq(1))
}

#[auto_type(type_name = "tyPeAliasEdCustom")]
fn type_aliased_custom() -> _ {
	dsl::not(abc::id.eq(1i32))
}

#[test]
fn test_auto_type() {
	let _ = abc::table.select(inline_rendering());

	let ta: type_aliased = type_aliased();
	let _ = abc::table.select(ta);

	let ta: TypeAliasedPascal = type_aliased_pascal();
	let _ = abc::table.select(ta);

	let ta: tyPeAliasEdCustom = type_aliased_custom();
	let _ = abc::table.select(ta);
}
