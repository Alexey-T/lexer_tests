#pike 7.7

//! Pike 7.6 compatibility.

protected array(array(int|string)) _describe_program(program p)
{
  return map(predef::_describe_program(p),
	     lambda(array(mixed) symbol_info) {
	       // Remove the value_type entry (index 2).
	       return symbol_info[..1]+symbol_info[3..];
	     });
}

object master()
{
  return __REAL_VERSION__::master()->get_compat_master(7, 6);
}

protected Mapping.ShadowedMapping compat_all_constants =
  Mapping.ShadowedMapping(predef::all_constants(),
			  ([
			    "all_constants": all_constants,
			    "_describe_program": _describe_program,
			    "sprintf": sprintf_76,
			    "array_sscanf": array_sscanf_76,
			    "master": master,
#if constant(System.RegGetValue_76)
			    "RegGetValue": System.RegGetValue_76,
			    "RegGetKeyNames": System.RegGetKeyNames_76,
			    "RegGetValues": System.RegGetValues_76,
#endif
			  ]), 1);

mapping(string:mixed) all_constants()
{
  // Intentional lie in the return type.
  mixed x = compat_all_constants;
  return x;
}

protected void create()
{
}
