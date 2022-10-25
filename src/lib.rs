use std::collections::{BTreeMap, HashMap};

type InternalReference = u128;
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct CommandReference {
    inner: InternalReference,
}
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ArgumentReference {
    inner: InternalReference,
}
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct FlagReference {
    inner: InternalReference,
}


impl CommandReference {
    pub fn from(inner: InternalReference) -> Self {
        Self { inner }
    }
}
impl ArgumentReference {
    pub fn from(inner: InternalReference) -> Self {
        Self { inner }
    }
}
impl FlagReference {
    pub fn from(inner: InternalReference) -> Self {
        Self { inner }
    }
}

fn generate_uuid(i: &mut u128) -> InternalReference {
    *i += 1;
    *i
}

/// Represents an "argument", an input you give to a command to change its behavior.
struct Argument {
    /// The string used to invoke the argument, often prefixed with `--`
    invoker: String,
    /// A shorter version of `invoker`, often one character long and prefixed with `-`
    short_invoker: Option<String>,
    /// A potential n value of when this argument should appear if it has no invoker
    n_value: Option<u8>,
    /// The name of what you put into the argument, used in usage generation
    input: String,
}

/// Represents a "flag", an argument that doesn't take any input.
struct Flag {
    /// The string used to invoke the flag, often prefixed with `--`
    invoker: String,
    /// A shorter version of `invoker`, often one character long and prefixed with `-`
    short_invoker: Option<String>,
    /// A description of what the flag does, used in usage generation
    description: String,
}

/// Represents a "command", something that you tell the program to do.
struct Command {
    /// The string used to invoke the command.
    invoker: String,
    /// A shorter version of `invoker`, useful if this command is commonly used and/or long
    short_invoker: Option<String>,
    /// A description of what the command does, used in usage generation
    description: String,
    /// Arguments required for this command
    required_arguments: Vec<ArgumentReference>,
    /// Is the invoker the default invoker?
    is_default: bool,
}

pub enum Invoker<'a> {
    /// Single word
    Word(&'a str),
    /// Word prefixed with `-`
    Dash(&'a str),
    /// Word prefixed with `--`
    DoubleDash(&'a str),
    /// Both a shorter word prefixed with `-` and a longer word prefixed with `--`
    DashAndDoubleDash(&'a str, &'a str),
    /// A word and a shorter word
    WordAndShortWord(&'a str, &'a str),
    /// Does not have an invoking string, but rather is based on the position of the argument
    NWithoutInvoker(u8),
    /// Invoke by default, currently only for use with commands
    Default,
}

impl<'a> Invoker<'a> {
    pub fn to_full_and_short(&self) -> (String, Option<String>, Option<u8>) {
        match self {
            Invoker::Word(word) => (word.to_string(), None, None),
            Invoker::Dash(word) => (format!("-{}", word), None, None),
            Invoker::DoubleDash(word) => (format!("--{}", word), None, None),
            Invoker::DashAndDoubleDash(short, long) => (format!("--{}", long), Some(format!("-{}", short)), None),
            Invoker::WordAndShortWord(word, short) => (word.to_string(), Some(short.to_string()), None),
            Invoker::NWithoutInvoker(n) => ("".to_string(), None, Some(*n)),
            Invoker::Default => ("".to_string(), None, None),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FromInputError {
    CommandNotFound,
    ArgumentNotFound,
    ArgumentNotValidInLocation,
}

#[derive(Default)]
pub struct CommandInterface {
    name: String,
    description: String,
    commands: BTreeMap<CommandReference, Command>,
    arguments: BTreeMap<ArgumentReference, Argument>,
    flags: BTreeMap<FlagReference, Flag>,
    /// Hashmap of potential ways to activate a command to the command UUID
    command_invokers: HashMap<String, CommandReference>,
    /// Hashmap of potential ways to invoke an argument to the argument UUID
    argument_invokers: HashMap<String, ArgumentReference>,
    /// Hashmap of potential ways to invoke a flag to the flag UUID
    flag_invokers: HashMap<String, FlagReference>,
    /// Hashmap of index to arguments that use an NWithoutInvoker invoker
    n_arguments: HashMap<u8, ArgumentReference>,

    /// Is construction complete?
    ready: bool,

    /// Internal usage
    int: u128,
}

#[derive(Debug)]
pub struct CommandInput {
    pub command: CommandReference,
    pub arguments: HashMap<ArgumentReference, String>,
    pub inputs: Vec<String>,
    pub flags: Vec<FlagReference>,
}

impl CommandInterface {
    pub fn new(name: &str, description: &str) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            commands: BTreeMap::new(),
            arguments: BTreeMap::new(),
            flags: BTreeMap::new(),
            command_invokers: HashMap::new(),
            argument_invokers: HashMap::new(),
            flag_invokers: HashMap::new(),
            n_arguments: HashMap::new(),
            ready: false,
            int: 0,
        }
    }
    /// Add a command to the interface.
    pub fn add_command(
        &mut self,
        invocation: Invoker,
        arguments: Vec<ArgumentReference>,
        description: &str,
    ) -> CommandReference {
        assert!(!self.ready, "cannot add commands after construction is complete");
        let uuid = CommandReference::from(generate_uuid(&mut self.int));
        let (invoker, short_invoker, n) = invocation.to_full_and_short();
        self.commands.insert(uuid, Command {
            invoker: invoker.clone(),
            short_invoker: short_invoker.clone(),
            description: description.to_string(),
            required_arguments: arguments,
            is_default: matches!(invocation, Invoker::Default),
        });
        self.command_invokers.insert(invoker, uuid);
        if let Some(short_invoker) = short_invoker {
            self.command_invokers.insert(short_invoker, uuid);
        }
        uuid
    }
    /// Add an argument to the interface.
    pub fn add_argument(
        &mut self,
        invocation: Invoker,
        input: &str,
    ) -> ArgumentReference {
        assert!(!self.ready, "cannot add arguments after construction is complete");
        let uuid = ArgumentReference::from(generate_uuid(&mut self.int));
        let (invoker, short_invoker, n) = invocation.to_full_and_short();
        self.arguments.insert(uuid, Argument {
            invoker: invoker.clone(),
            short_invoker: short_invoker.clone(),
            n_value: n,
            input: input.to_string(),
        });
        self.argument_invokers.insert(invoker, uuid);
        if let Some(short_invoker) = short_invoker {
            self.argument_invokers.insert(short_invoker, uuid);
        }
        uuid
    }
    /// Add a flag to the interface.
    pub fn add_flag(
        &mut self,
        invocation: Invoker,
        description: &str,
    ) -> FlagReference {
        assert!(!self.ready, "cannot add flags after construction is complete");
        let uuid = FlagReference::from(generate_uuid(&mut self.int));
        let (invoker, short_invoker, n) = invocation.to_full_and_short();
        self.flags.insert(uuid, Flag {
            invoker: invoker.clone(),
            short_invoker: short_invoker.clone(),
            description: description.to_string(),
        });
        self.flag_invokers.insert(invoker, uuid);
        if let Some(short_invoker) = short_invoker {
            self.flag_invokers.insert(short_invoker, uuid);
        }
        uuid
    }

    /// Finish construction of the interface.
    pub fn finalise(&mut self) {
        assert!(!self.ready, "cannot finalise twice");
        self.ready = true;
    }

    /// Internal function
    fn get_command_with_args_from_string_vec(
        &self,
        input: Vec<String>
    ) -> Result<CommandInput, FromInputError> {
        let mut command = self.command_invokers.get(&input[0]);
        if command.is_none() {
            // do we have a default command?
            let default_command = self.commands.iter().find(|(_, command)| command.is_default);
            if default_command.is_none() {
                return Err(FromInputError::CommandNotFound);
            }
            command = Some(&default_command.unwrap().0);
        }
        let command = command.unwrap();
        let mut args = Vec::new();
        let mut active_flags = Vec::new();
        let mut args_not_prefixed = Vec::new();
        let mut i = 1;
        // find all flags
        while i < input.len() {
            if let Some(flag) = self.flag_invokers.get(&input[i]) {
                active_flags.push(*flag);
            } else {
                args_not_prefixed.push(input[i].clone());
            }
            i += 1;
        }
        // remove flags from input
        let input: Vec<String> = input.into_iter().filter(|s| !self.flag_invokers.contains_key(s)).collect();
        i = 1;
        // find all arguments
        while i < input[1..].len() {
            // if arg contains a =, remove and split into arg and value
            // otherwise, take arg as input[i] and value as input[i+1] (fail if input[i+1] doesn't exist)
            if input[i].contains('=') {
                let mut split = input[i].split('=');
                let arg = split.next().unwrap();
                let value = split.next().unwrap();
                let arg = self.argument_invokers.get(arg).ok_or(FromInputError::ArgumentNotFound)?;
                args.push((*arg, value.to_string()));
                // remove this value from args_not_prefixed
                args_not_prefixed.retain(|s| s != value);
                i += 1;
            } else if i < input.len() - 1 && !input[i+1].starts_with('-') && self.argument_invokers.contains_key(&input[i]) {
                let arg = self.argument_invokers.get(&input[i]).ok_or(FromInputError::ArgumentNotFound)?;
                args.push((*arg, input[i+1].to_string()));
                // remove these values from args_not_prefixed
                args_not_prefixed.retain(|s| s != &input[i] && s != &input[i+1]);
                i += 2;
            } else {
                i += 1;
            }
        }

        // find max nwithoutinvoker value
        let mut max_n = 0;
        for arg in self.commands[command].required_arguments.iter() {
            if let Some(n) = self.arguments[arg].n_value {
                if n > max_n {
                    max_n = n;
                }
            }
        }

        // assert that all required arguments are present
        let command_inner = self.commands.get(command).unwrap();
        for required_arg in command_inner.required_arguments.iter() {
            if !args.iter().any(|(arg, _)| arg == required_arg) &&
                args_not_prefixed.len() - 1 < max_n as usize {
                return Err(FromInputError::ArgumentNotFound);
            }
        }
        // assert that no arguments are present twice
        args.sort();
        args.dedup();

        Ok(CommandInput {
            command: *command,
            arguments: args.into_iter().collect(),
            inputs: args_not_prefixed,
            flags: active_flags,
        })
    }

    pub fn get_specific_command_usage(&self, command: CommandReference) -> String {
        let command_inner = self.commands.get(&command).unwrap();
        let mut usage = format!("{} ", command_inner.invoker);
        for arg in command_inner.required_arguments.iter() {
            let arg_inner = self.arguments.get(arg).unwrap();
            // if short invoker is present, format as
            // -s, --long -> description
            // otherwise, format as
            // --long -> description
            if let Some(short_invoker) = arg_inner.short_invoker.as_ref() {
                usage.push_str(&format!("   -{} <{}>, --{}[=]<{}>", short_invoker, arg_inner.input, arg_inner.invoker, arg_inner.input));
            } else {
                usage.push_str(&format!("   --{}[=]<{}>", arg_inner.invoker, arg_inner.input));
            }
        }
        usage
    }

    pub fn get_full_usage(&self) -> String {
        let mut usage = String::new();
        for command in self.commands.keys() {
            usage.push_str(&format!("{}\n", self.get_specific_command_usage(*command)));
        }
        usage
    }

    pub fn get_simple_usage(&self) -> String {
        let mut usage = String::new();
        for command in self.commands.keys() {
            let command_inner = self.commands.get(command).unwrap();
            if command_inner.is_default {
                // skip
                continue;
            }
            let required_inputs = {
                let mut out = String::new();
                // for each required argument, if it's an NWithoutInvoker, add it to the final string
                for arg in command_inner.required_arguments.iter() {
                    let arg_inner = self.arguments.get(arg).unwrap();
                    if arg_inner.n_value.is_some() {
                        out.push_str(&format!(" <{}>", arg_inner.input));
                    }
                }
                out
            };
            if let Some(short_invoker) = &command_inner.short_invoker {
                usage.push_str(&format!("   {}, {}{} - {}\n", command_inner.invoker, short_invoker, required_inputs, command_inner.description));
            } else {
                usage.push_str(&format!("   {}{} - {}\n", command_inner.invoker, required_inputs, command_inner.description));
            }
        }
        usage
    }

    pub fn get_all_argument_usage(&self) -> String {
        let mut usage = String::new();
        for flag in self.flags.keys() {
            let flag_inner = self.flags.get(flag).unwrap();
            // if short invoker is present, format as
            // -s, --long -> description
            // otherwise, format as
            // --long -> description
            if let Some(short_invoker) = flag_inner.short_invoker.as_ref() {
                usage.push_str(&format!("   {}, {} -> {}\n", short_invoker, flag_inner.invoker, flag_inner.description));
            } else {
                usage.push_str(&format!("   {} -> {}\n", flag_inner.invoker, flag_inner.description));
            }
            usage.push('\n');
        }
        for argument in self.arguments.keys() {
            let argument_inner = self.arguments.get(argument).unwrap();
            // if short invoker is present, format as
            // -s, --long -> description
            // otherwise, format as
            // --long -> description
            if let Some(short_invoker) = argument_inner.short_invoker.as_ref() {
                usage.push_str(&format!("   {} <{}>, {}[=]<{}>", short_invoker, argument_inner.input, argument_inner.invoker, argument_inner.input));
                usage.push('\n');
            } else if argument_inner.n_value.is_none() {
                usage.push_str(&format!("   {}[=]<{}>", argument_inner.invoker, argument_inner.input));
                usage.push('\n');
            }
        }
        usage
    }

    pub fn print_help(&self) {
        println!("{}", self.name);
        println!("{}", self.description);
        println!();
        println!("usage: {} [command] [arguments]", self.name);
        println!("commands:");
        println!("{}", self.get_simple_usage());
        println!("arguments:");
        println!("{}", self.get_all_argument_usage());
    }

    pub fn go(&self) -> Result<CommandInput, FromInputError> {
        let input = std::env::args().collect();
        self.get_command_with_args_from_string_vec(input)
    }

    pub fn go_and_print_usage_on_failure(
        &self
    ) -> Result<CommandInput, FromInputError> {
        let input = std::env::args().collect();
        match self.get_command_with_args_from_string_vec(input) {
            Ok(result) => Ok(result),
            Err(err) => {
                self.print_help();
                Err(err)
            }
        }
    }

    pub fn go_fake_args_for_testing(
        &self,
        input: Vec<String>
    ) -> Result<CommandInput, FromInputError> {
        self.get_command_with_args_from_string_vec(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(1 + 1, 2);
    }

    #[test]
    fn try_to_get_fake_arguments() {
        let mut interface = CommandInterface::new("test", "command for testing");
        let arg = interface.add_argument(Invoker::Word("arg"), "nothing");
        let arg2 = interface.add_argument(Invoker::DashAndDoubleDash("a", "argument"), "nothing");
        let n_arg = interface.add_argument(Invoker::NWithoutInvoker(0), "file");
        let n_arg2 = interface.add_argument(Invoker::NWithoutInvoker(1), "output");
        let command = interface.add_command(Invoker::Word("do"), vec![arg, n_arg, n_arg2], "a command to do things");
        let command2 = interface.add_command(Invoker::Word("yeah"), vec![arg2], "a command to do other! things");
        let def_command = interface.add_command(Invoker::Default, vec![arg, arg2], "the default command");
        let flag = interface.add_flag(Invoker::DashAndDoubleDash("f", "flag"), "enables the flag");
        interface.finalise();
        //interface.print_help();
        let input = interface.go_fake_args_for_testing(vec!["do".to_string(), "filename".to_string(), "arg".to_string(), "val".to_string(), "-f".to_string(), "output".to_string()]).unwrap();
        let input2 = interface.go_fake_args_for_testing(vec!["filename".to_string(), "arg".to_string(), "val".to_string(), "-f".to_string(), "output".to_string(), "extra".to_string()]).unwrap();

        //println!("{:?}", input);
        //println!("{:?}", input2);

        assert_eq!(command, input.command);
        assert_eq!(input.arguments.len(), 1);
        assert_eq!(def_command, input2.command);
    }
}
