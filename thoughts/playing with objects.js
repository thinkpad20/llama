/*
object Gender = { Male; Female }
object Person = Person (name: Str) (age: Int) (gender: Gender);
object Manager extends Person =
  Manager name age gender (dept: Str) extends Person name age gender;

foo (m: Manager) = case m of
    Manager name Male dept => "male manager named #[name], managing #[dept]"
  | Person name _ => "female manager named #[name]";

liz = Manager "Liz" Female 25 "Sales"
jim = Manager "Jim" Male 31 "Supplies"

assert foo liz == "female manager named Liz"
assert foo jim == "male manager named Jim, managing Supplies"


* Create schema for all of these types
*
*/

__schemas__ = {
  Gender: {
    __class__: "Gender",
    __constructors__: [{
        __cname__: 'Male'
      }, {
        __cname__: 'Female'
      }
    ],
  },

  Person: {
    __class__: "Person",
    __constructors__: [{
      __cname__: "Person",
      __attrs__: {
        __order__: ["name", "age", "gender"],
        name: StrSchema,
        age: IntSchema,
        gender: GenderSchema
      }
    }],
  }
}

CreateObject = function (className, cName, args...) {
  var schema = __schemas__[className];
  if (!schema) {
    throw new Error();
  }
  var template = {
    __class__: className,
    __cname__: cName
  }
  if (_.has(schema, '__parent__')) {
    template.__parent__ = CreateObject(schema.__parent__.)
  }

}

Person = function (name) {
  return function (age) {
    return function (gender) {
      return CreateObject('Person', 'Person', name, age, gender);
    };
  };
};

Manager = function (name) {
  return function (age) {
    return function (gender) {
      return function (dept) {
        return CreateObject('Manager', 'Manager', name, age, gender, dept);
      };
    };
  };
};

Female = function () {
  return CreateObject('Gender', 'Female');
}

Male = function () {
  return CreateObject('Gender', 'Male');
}

liz = CreateObject('Manager', 'Liz', Female(), 25, 'Sales')

liz = {
  __class__: "Manager",
  __cname__: "Manager",
  __attrs__: {
    dept: {
      __class__: "Str",
      __value__: "Sales"
    }
  }
  __parent__: {
    __class__: "Person",
    __cname__: "Person"
    __attrs__: {
      __order__: ["name", "gender", "age"],
      name: {
        __class__: "Str",
        __value__: "Liz"
      },
      gender: {
        __class__: "Gender",
        __cname__: "Female"
      },
      age: {
        __class__: "Int",
        __value__: 25
      }
    }
  }
}
