use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::{
    functions::{LoxCallable, LoxFunction},
    runtime_errors::RuntimeErrorOrReturnValue,
    values::Value,
    Locals, RuntimeError,
};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    pub name: String,
    pub locals: Locals,
    pub methods: HashMap<String, LoxFunction>,
    pub superclass: Option<Box<LoxClass>>,
}

impl LoxClass {
    pub fn find_method(&self, name: &str) -> Option<LoxFunction> {
        if let Some(method) = self.methods.get(name) {
            return Some(method.clone());
        }
        if let Some(superclass) = &self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl LoxCallable for LoxClass {
    fn call(&self, arguments: Vec<Value>) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        let instance = LoxInstance::new(self.clone());
        if let Some(initializer) = instance.class.find_method("init") {
            initializer.bind(instance.clone(), true).call(arguments)?;
        }
        Ok(Rc::new(RefCell::new(Value::Instance(instance))))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    pub class: LoxClass,
    fields: Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        if let Some(v) = self.fields.borrow().get(name) {
            return Ok(v.clone());
        }
        if let Some(f) = self.class.find_method(name) {
            // Note has the same state as self!
            let instance_to_bind = self.clone();
            let is_initializer = name == "init";
            let bound_f = f.bind(instance_to_bind, is_initializer);
            return Ok(Rc::new(RefCell::new(Value::Function(bound_f))));
        }
        Err(RuntimeError {
            message: format!("Undefined property '{name}'."),
        })
    }

    pub fn set(&mut self, name: String, value: Rc<RefCell<Value>>) {
        self.fields.borrow_mut().insert(name, value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} instance", self.class.name))
    }
}
