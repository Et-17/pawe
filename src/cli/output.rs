use std::fmt::Display;

use crate::cli::TreeNode;
use crate::cli::arg_parser::EvolutionOutputArgs;
use crate::evolution::Rule;
use crate::phonemes::Phoneme;

pub fn display_label<T: Display>(text: T, args: &EvolutionOutputArgs) {
    if args.no_stages || args.no_labels || args.csv {
        return;
    }

    println!("{}", text);
}

pub fn display_evolution_label<T: Display>(start: &T, end: &T, args: &EvolutionOutputArgs) {
    if args.show_changes {
        display_label(format_args!("Evolving from {start} to {end}"), args);
    } else {
        display_label(format_args!("{end}"), args);
    }
}

pub fn display_word(word: &[Phoneme]) {
    for character in word {
        print!("{}", character);
    }
}

pub fn display_word_stage(word: &[Phoneme], first: bool, args: &EvolutionOutputArgs) {
    if args.no_stages {
        return;
    }

    if !args.no_labels && !args.csv {
        print!("    ");
    }

    if args.csv && !first {
        print!(", ");
    }

    display_word(word);

    if !args.csv {
        println!();
    }
}

fn display_rule(rule: &Rule, args: &EvolutionOutputArgs) {
    if !args.show_rules && !args.csv {
        return;
    }

    if !args.no_labels {
        print!("    ");
    }

    println!("// {}", rule);
}

pub fn display_application(
    word: &[Phoneme],
    changed: bool,
    rule: &Rule,
    args: &EvolutionOutputArgs,
) {
    if !changed && !args.show_all_rules {
        return;
    }

    display_rule(rule, args);

    if args.show_changes {
        display_word_stage(word, false, args);
    }
}

pub fn display_lang_result(word: &[Phoneme], args: &EvolutionOutputArgs) {
    if args.show_changes {
        return;
    }

    display_word_stage(word, false, args);
}

pub fn display_final_result(word: &[Phoneme], args: &EvolutionOutputArgs) {
    if args.no_stages {
        display_word(word);
    }

    if args.no_stages || args.csv {
        println!();
    }
}

pub fn display_start<T: Display>(word: &[Phoneme], lang: &T, args: &EvolutionOutputArgs) {
    display_label(lang, args);
    display_word_stage(word, true, args);
}

pub fn display_tree(tree: &TreeNode) {
    print!("{}: ", tree.language);
    display_word(&tree.word);
    println!();

    display_tree_node_children(&tree.children, &String::new());
}

fn gen_tree_indent(last: bool) -> &'static str {
    if last { "    " } else { "│   " }
}

fn display_tree_node(tree: &TreeNode, prior: &impl Display, last: bool) {
    print!("{prior}");

    if last {
        print!("└───");
    } else {
        print!("├───");
    }

    print!("{}: ", tree.language);
    display_word(&tree.word);
    println!();

    let new_prior = format!("{prior}{}", gen_tree_indent(last));

    display_tree_node_children(&tree.children, &new_prior);
}

fn display_tree_node_children(children: &[TreeNode], prior: &impl Display) {
    let Some((last_child, body_children)) = children.split_last() else {
        return;
    };

    for child in body_children {
        display_tree_node(child, prior, false);
    }
    display_tree_node(last_child, prior, true);
}
