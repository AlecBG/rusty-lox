use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use super::{
    functions::NativeFunction,
    runtime_errors::{RuntimeError, RuntimeErrorOrReturnValue},
    values::Value,
};

pub fn construct_global_values() -> HashMap<String, Rc<RefCell<Value>>> {
    let clock = Value::NativeFunction(NativeFunction::Clock);
    HashMap::from([("clock".to_string(), Rc::new(RefCell::new(clock)))])
}

pub trait Environment: Debug + EnvironmentClone {
    fn push(&mut self);
    fn pop(&mut self);
    fn define(&mut self, name: String, value: Value);
    fn assign(&self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue>;
    fn assign_at(
        &self,
        name: String,
        value: Value,
        distance: &usize,
    ) -> Result<(), RuntimeErrorOrReturnValue>;
    fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue>;
    fn get_at(
        &self,
        distance: &usize,
        name: &str,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue>;
    fn get_depth(&self) -> usize;

    fn debug(&self);
}

/// This is using a trick found https://stackoverflow.com/questions/30353462/how-to-clone-a-struct-storing-a-boxed-trait-object
/// The fundamental problem is I want the Environment to be cloneable, but traits are not allowed to be
/// subtraits of Clone and be object safe as Clone is not object safe (it returns Self), so we introduce some indirection
/// with the trait EnvironmentClone
pub trait EnvironmentClone {
    fn clone_box(&self) -> Box<dyn Environment>;
}

impl<T> EnvironmentClone for T
where
    T: 'static + Environment + Clone,
{
    fn clone_box(&self) -> Box<dyn Environment> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Environment> {
    fn clone(&self) -> Box<dyn Environment> {
        self.clone_box()
    }
}

type RefCellEnvironmentStack = Vec<Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>>;

/// In this environment we get closures like in say javascript or python.
/// For example
/// ```lox
/// var x = 1;
/// fun f() {
///     print x;
/// }
/// x = x + 1;
/// f();
/// ```
/// This code will print 2.
#[derive(Clone, Debug, PartialEq)]
pub struct RefCellEnvironment {
    values: RefCellEnvironmentStack,
    pos: usize,
}

impl RefCellEnvironment {
    pub fn new() -> Self {
        let mut env = Self {
            values: vec![Rc::new(RefCell::new(construct_global_values()))],
            pos: 0,
        };
        env.push();
        env
    }
}

impl Environment for RefCellEnvironment {
    fn push(&mut self) {
        self.values.push(Rc::new(RefCell::new(HashMap::new())));
        self.pos += 1;
    }

    fn pop(&mut self) {
        self.pos = self.values.len() - 1;
        if self.pos == 0 {
            return;
        }
        self.values.pop();
        self.pos -= 1;
    }

    fn define(&mut self, name: String, value: Value) {
        self.values[self.pos]
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn assign(&self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue> {
        let mut pos = self.pos;
        loop {
            match self.assign_at(name.clone(), value.clone(), &pos) {
                Ok(()) => return Ok(()),
                Err(_) => {
                    if pos == 0 {
                        break;
                    }
                    pos -= 1;
                }
            };
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
    }

    fn assign_at(
        &self,
        name: String,
        value: Value,
        distance: &usize,
    ) -> Result<(), RuntimeErrorOrReturnValue> {
        let values = self
            .values
            .get(*distance)
            .ok_or::<RuntimeErrorOrReturnValue>(
                RuntimeError {
                    message: "Location in stack not found.".to_string(),
                }
                .into(),
            )?;
        if values.borrow().contains_key(&name) {
            values
                .borrow_mut()
                .insert(name, Rc::new(RefCell::new(value)));
            Ok(())
        } else {
            Err(RuntimeError {
                message: format!("Undefined variable '{name}'."),
            }
            .into())
        }
    }

    fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        for values in self.values.iter().rev() {
            if let Some(v) = values.borrow().get(name) {
                return Ok(v.clone());
            }
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
    }

    fn get_at(
        &self,
        distance: &usize,
        name: &str,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        let values = self
            .values
            .get(*distance)
            .ok_or::<RuntimeErrorOrReturnValue>(
                RuntimeError {
                    message: "Location in stack not found.".to_string(),
                }
                .into(),
            )?;
        if let Some(v) = values.borrow().get(name) {
            return Ok(v.clone());
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
    }

    fn get_depth(&self) -> usize {
        self.values.len()
    }

    fn debug(&self) {
        for (i, vals_ptr) in self.values.iter().enumerate() {
            println!("Depth {i}");
            let vals = &*vals_ptr.borrow();
            for (k, v) in vals {
                println!("{k}: {}", v.borrow().get_type());
            }
        }
    }
}

impl Default for RefCellEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct ClosureEnvironment {
    enclosed_environment: Box<dyn Environment>,
    enclosed_environment_depth: usize,
    values: RefCellEnvironmentStack,
    pos: usize,
}

impl ClosureEnvironment {
    pub fn new(enclosed_environment: Box<dyn Environment>) -> Self {
        let depth = enclosed_environment.get_depth();
        Self {
            enclosed_environment,
            enclosed_environment_depth: depth,
            values: vec![],
            pos: depth - 1,
        }
    }
}

impl Environment for ClosureEnvironment {
    fn push(&mut self) {
        self.values.push(Rc::new(RefCell::new(HashMap::new())));
        self.pos += 1;
    }

    fn pop(&mut self) {
        self.pos = self.values.len() - 1;
        if self.pos == self.enclosed_environment_depth {
            panic!("Tried to pop a closure environment into its enclosed environment!");
        }
        self.values.pop();
        self.pos -= 1;
    }

    fn define(&mut self, name: String, value: Value) {
        self.values[self.pos - self.enclosed_environment_depth]
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn assign(&self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue> {
        let mut pos = self.pos;
        loop {
            match self.assign_at(name.clone(), value.clone(), &pos) {
                Ok(()) => return Ok(()),
                Err(_) => {
                    if pos == 0 {
                        break;
                    }
                    pos -= 1;
                }
            };
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
    }

    fn assign_at(
        &self,
        name: String,
        value: Value,
        distance: &usize,
    ) -> Result<(), RuntimeErrorOrReturnValue> {
        if distance < &self.enclosed_environment_depth {
            return self.enclosed_environment.assign_at(name, value, distance);
        }
        let d = distance - self.enclosed_environment_depth;
        let values = self.values.get(d).ok_or::<RuntimeErrorOrReturnValue>(
            RuntimeError {
                message: "Location in stack not found.".to_string(),
            }
            .into(),
        )?;
        if values.borrow().contains_key(&name) {
            values
                .borrow_mut()
                .insert(name, Rc::new(RefCell::new(value)));
            Ok(())
        } else {
            Err(RuntimeError {
                message: format!("Undefined variable '{name}'."),
            }
            .into())
        }
    }

    fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        for values in self.values.iter().rev() {
            if let Some(v) = values.borrow().get(name) {
                return Ok(v.clone());
            }
        }
        self.enclosed_environment.get(name)
    }

    fn get_at(
        &self,
        distance: &usize,
        name: &str,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        if distance < &self.enclosed_environment_depth {
            return self.enclosed_environment.get_at(distance, name);
        }
        let d = distance - self.enclosed_environment_depth;
        let values = self.values.get(d).ok_or::<RuntimeErrorOrReturnValue>(
            RuntimeError {
                message: "Location in stack not found.".to_string(),
            }
            .into(),
        )?;
        if let Some(v) = values.borrow().get(name) {
            return Ok(v.clone());
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
    }

    fn get_depth(&self) -> usize {
        self.values.len() + self.enclosed_environment_depth
    }

    fn debug(&self) {
        self.enclosed_environment.debug();
        println!("----");
        for (i, vals_ptr) in self.values.iter().enumerate() {
            println!("Depth {}", i + self.enclosed_environment_depth);
            let vals = &*vals_ptr.borrow();
            for (k, v) in vals {
                println!("{k}: {}", v.borrow().get_type());
            }
        }
    }
}
