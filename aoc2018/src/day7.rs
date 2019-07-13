use std::collections::{HashMap, HashSet};

fn read_requirements(lines: Vec<String>) -> HashMap<char, Vec<char>> {
    let mut requirements:HashMap<char, Vec<char>> = HashMap::new();

    for line in lines {
        let parts:Vec<&str> = line.split(' ').collect();

        let before = parts[1].chars().next().unwrap();
        requirements.entry(before).or_insert(vec![]);

        let after = parts[7].chars().next().unwrap();
        let required = requirements.entry(after).or_insert(vec![]);
        required.push(before);
    }

    requirements
}

fn part_1(requirements: &HashMap<char, Vec<char>>) -> String {
    let mut done:Vec<char> = vec![];

    while done.len() < requirements.len() {
        for ch in "ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars() {
            if let Some(required) = requirements.get(&ch) {
                if ! done.contains(&ch) && required.iter().all(|c| done.contains(c)) {
                    done.push(ch);
                    break
                }
            }
        }
    }

    done.iter().collect::<String>()
}

const N_WORKERS:usize = 5;
const TASK_COST:u32 = 60;

#[derive(PartialEq, Eq)]
struct Task {
    ready_at: u32,
    ch: char,
}

fn remove_finished_tasks(time: u32, tasks:Vec<Task>, done:&mut HashSet<char>) -> Vec<Task> {
    let mut new_tasks = vec![];

    for task in tasks {
        if time >= task.ready_at {
            done.insert(task.ch);
        } else {
            new_tasks.push(task);
        }
    }

    new_tasks
}

fn task_duration(ch: char) -> u32 {
    TASK_COST + (ch as u8 - 'A' as u8) as u32 + 1
}

fn part_2(requirements: &HashMap<char, Vec<char>>) -> u32 {
    let mut tasks:Vec<Task> = vec![];
    let mut started_on:HashSet<char> = HashSet::new();
    let mut done:HashSet<char> = HashSet::new();

    let mut time = 0;
    while done.len() < requirements.len() {
        tasks = remove_finished_tasks(time, tasks, &mut done);

        let mut run_again = true;
        while run_again && N_WORKERS > tasks.len() {
            run_again = false;

            for ch in "ABCDEFGHIJKLMNOPQRSTUVWXYZ".chars() {
                if let Some(required) = requirements.get(&ch) {
                    if ! started_on.contains(&ch) && required.iter().all(|c| done.contains(c)) {
                        tasks.push( Task { ready_at: time + task_duration(ch), ch: ch });
                        started_on.insert(ch);

                        run_again = true;
                        break;
                    }
                }
            }
        }

        time += 1;
    }

    time - 1
}

pub fn solve(lines: Vec<String>) {
    let requirements = read_requirements(lines);

    println!("Day 7");
    println!("Part 1: {}", part_1(&requirements));
    println!("Part 2: {}", part_2(&requirements));
}
