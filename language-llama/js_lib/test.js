var core = require('./llama_core');
def_str = core._default.cast(core.TConst('Str'));
console.log(core.show(def_str));

just_2 = core.Just(2);

console.log(just_2.repr());

mklist = core.str_or_array_to_list;

console.log(mklist([1,2,3]).repr());

