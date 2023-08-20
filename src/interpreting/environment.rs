use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    rc::Rc,
};

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
    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue>;
    fn assign_at(
        &mut self,
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

/// In this environment we get closures like in say javascript or python.
/// For example
/// ```
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
    values: Vec<Rc<RefCell<HashMap<String, Rc<RefCell<Value>>>>>>,
    pos: usize,
}

impl RefCellEnvironment {
    pub fn new() -> Self {
        Self {
            values: vec![Rc::new(RefCell::new(construct_global_values()))],
            pos: 0,
        }
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

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue> {
        let pos = self.pos;
        if self.values[self.pos].borrow().contains_key(&name) {
            self.values[self.pos]
                .borrow_mut()
                .insert(name, Rc::new(RefCell::new(value)));
            Ok(())
        } else {
            if self.pos == 0 {
                return Err(RuntimeError {
                    message: format!("Undefined variable '{name}'."),
                }
                .into());
            }
            self.pos -= 1;
            self.assign(name, value)?;
            self.pos = pos;
            Ok(())
        }
    }

    fn assign_at(
        &mut self,
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
        values
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
        Ok(())
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

/// In this environment one does not have persistent state in closures.
/// The environment of a function is a frozen copy of what the environment was
/// when the function was defined. For example
/// ```
/// var x = 1;
/// fun f() {
///     print x;
/// }
/// x = x + 1;
/// f();
/// ```
/// This code will print 1.
#[derive(Clone, Debug, PartialEq)]
pub struct SingleCopyEnvironment {
    values: Vec<HashMap<String, Rc<RefCell<Value>>>>,
    pos: usize,
}

impl SingleCopyEnvironment {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            values: vec![construct_global_values()],
            pos: 0,
        }
    }
}

impl Environment for SingleCopyEnvironment {
    fn push(&mut self) {
        self.values.push(HashMap::new());
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
        self.values[self.pos].insert(name, Rc::new(RefCell::new(value)));
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue> {
        let pos = self.pos;
        if let Entry::Occupied(mut e) = self.values[self.pos].entry(name.clone()) {
            e.insert(Rc::new(RefCell::new(value)));
            Ok(())
        } else {
            if self.pos == 0 {
                return Err(RuntimeError {
                    message: format!("Undefined variable '{name}'."),
                }
                .into());
            }
            self.pos -= 1;
            self.assign(name, value)?;
            self.pos = pos;
            Ok(())
        }
    }

    fn assign_at(
        &mut self,
        name: String,
        value: Value,
        distance: &usize,
    ) -> Result<(), RuntimeErrorOrReturnValue> {
        let values = self
            .values
            .get_mut(*distance)
            .ok_or::<RuntimeErrorOrReturnValue>(
                RuntimeError {
                    message: "Location in stack not found.".to_string(),
                }
                .into(),
            )?;
        values.insert(name, Rc::new(RefCell::new(value)));
        Ok(())
    }

    fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        for values in self.values.iter().rev() {
            if let Some(v) = values.get(name) {
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
        if let Some(v) = values.get(name) {
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
        for (i, vals) in self.values.iter().enumerate() {
            println!("Depth {i}");
            for (k, v) in vals {
                println!("{k}: {}", v.borrow().get_type());
            }
        }
    }
}
