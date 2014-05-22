var Zero = {};
var Join = {};

function add_instance(trait, func_name, type, func) {
  ...
}

function get_instance(trait, func_name, type) {
  ...
}

function mid(type) {
  var join = get_instance(Join, 'join', type);
  var zero = get_instance(Zero, 'zero', type);
  return join(zero);
}

function mconcat(type) {
  var join = get_instance(Join, 'join', type);
  var zero = get_instance(Zero, 'zero', type);
  return foldl(join)(zero);
};

add_instance(Zero, 'zero', TConst('Str'), '');
add_instance(Join, 'join', TConst('Str'), strcat);

function println(type) {
  var show = get_instance(Show, 'show', type);
  return _r_comp(show)(Sys.get('puts'))
}

println(TConst('Str'))(mconcat(TConst('Str'))(build_list(['hello', ', ',mid(TConst('Str'))('world!')])));

add_instance(Show, 'show', TApply(TConst('List'))(TVar('a')), function(list) { return function(type) {
  var show = get_instance(Show, 'show', type);
  var join = get_instance(Join, 'join', TApply(TConst('List'))(TVar('a')));
  return join((join('[')(_apply(join_by(','))(lmap(show)(list)))))(']');
}})

println(TApply(TConst('List'))(TConst('Str')))(build_list(['hello', 'world!']))

// Trying the 'types hold their instances' idea...?

Type.prototype.get_instance = function(trait, func_name) {
  ...
}

Type.prototype.add_instance = function(trait, func_name, func) {
  ...
}

// Nah, we still probably want to have functions, because we're going
// to be looking up by the types. But values can hold on to their types.

function println(a) {
  var show = get_instance(Show, 'show', a.type);
  return _r_comp(show)(Sys.get('puts'))
}

add_instance(Functor, 'map', TConst('List'), lmap)

// then how do we choose which to run at runtime?
// or better, what does the function `map` refer to?
var print_functor = map(println)

function print_functor(f) {
  var map = get_instance(Functor, 'map', f.type);
  return map(println)(f);
}

// So this restricts us from being point-free... Although if there's
// always a straightforward translation from point-free to point-full
// and we only need to do this specifically with trait functions, it
// should be OK. And more specifically, this should only be an issue
// with higher-kinded types.

print_functor(['hello', 'there', 'yo']);

// foo m_n = m_n.bind (n => lift $ join 3 n) ! map println

function foo (m_n) {
  var bind = get_instance(Monad, 'bind', m_n.type);
  var map = get_instance(Functor, 'map', m_n.type);
  var lift = get_instance(Applicative, 'lift', m_n.type);
  return _r_comp(bind(m_n)(function (n) {
    var join = get_instance(Join, 'join', n.type);
    return _apply(lift)(join(3)(n));
  }))(map(println));
};

// So that's all good that I could write that, but could I generate it?
// Reflection has the limitation of only working on argument types, not
// return types (doesn't it? Maybe not?)

// To see if that's possible, let's look at JUST that inner function:
// n => lift $ join 3 n
// Or more generally:
// foo n = lift $ join 3 n
// Or even:
// foo = lift <~ join 3
// How would we write this, using reflection? Is it possible?

var foo = _comp(lift)(join(3))

// however, on its own lift is not defined. In fact, is `join`? For
// a curried function?

function foo(lift) {
  return _comp(lift)(join(3));
}

// That would work... but yeah that's not reflection any more. Argh blarg.
// At compile time we can detect that the expression (lift <~ join 3) contains
// two trait functions. Let's assume any trait function can be rewritten to
// use reflection:

// foo = lift
// becomes

function foo(type) {
  var lift = get_instance(Applicative, 'lift', type);
  return lift;
}

// But that's not really accurate. Really the issue is that any scope the expression
// `foo 3` is used in, should receive its own version of `lift`.
// map print (foo 3); map (+1) (foo 3)
// Hmmmmmmmmmm. Blargh argh zargh.
