use diesel_auto_type::auto_type;

use diesel::{dsl, prelude::*};

diesel::table! {
	abc {
		id -> Integer,
	}
}

#[auto_type]
fn automatically_typed_function() -> _ {
	dsl::not(abc::id.eq::<i32>(1))
}

#[test]
fn test_auto_type() {
	let _ = abc::table.select(automatically_typed_function());
}
