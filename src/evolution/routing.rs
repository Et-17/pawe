use std::collections::{HashMap, VecDeque};

use crate::config::{Label, LabelEncoding};
use crate::evolution::Rule;

use super::errors::{EResult, EvolutionErrorType::*};

pub type Route = Vec<Label>;
type Languages = LabelEncoding;
type Evolutions = HashMap<Label, HashMap<Label, Vec<Rule>>>;

pub fn find_route(
    start_name: String,
    end_name: String,
    languages: &Languages,
    evolutions: &Evolutions,
) -> EResult<Route> {
    let start = languages
        .encode(&start_name)
        .ok_or_else(|| UndefinedLanguage(start_name).by("router"))?;
    let end = languages
        .encode(&end_name)
        .ok_or_else(|| UndefinedLanguage(end_name).by("router"))?;

    search(&start, &end, evolutions)
}

fn search(start: &Label, end: &Label, evolutions: &Evolutions) -> EResult<Route> {
    let mut frontier = VecDeque::new();
    let mut visited = HashMap::new();
    let mut last = None;
    frontier.push_back(start);

    loop {
        let Some(next_lang) = frontier.pop_front() else {
            return Err(NoConnection(start.clone(), end.clone()).by("router"));
        };

        if visited.contains_key(next_lang) {
            continue;
        }
        visited.insert(next_lang, last);
        last = Some(next_lang);

        if next_lang == end {
            break;
        }

        if let Some(descendants) = evolutions.get(next_lang) {
            frontier.extend(descendants.iter().map(|(desc, _)| desc));
        }
    }

    unwind_search_map(&visited, start, end)
}

fn unwind_search_map(
    map: &HashMap<&Label, Option<&Label>>,
    start: &Label,
    end: &Label,
) -> EResult<Route> {
    let mut route = vec![end];

    while let Some(last_lang) = route.last() {
        match map.get(last_lang) {
            Some(Some(connection)) => route.push(connection),
            _ => break,
        }
    }

    if route.first().is_none_or(|&r_end| r_end != end) {
        Err(NoConnection(start.clone(), end.clone()).by("router"))
    } else {
        route.reverse();
        Ok(route.into_iter().cloned().collect())
    }
}
