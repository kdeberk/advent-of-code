
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

#[derive(Eq, PartialEq)]
enum Observation {
    Start { minute: u32, guard_id: u32 },
    Sleep { minute: u32 },
    Awake { minute: u32 },
}

#[derive(Clone, Eq, PartialEq)]
struct Sleep {
    start: u32,
    end: u32,
}

#[derive(Clone, Eq, PartialEq)]
struct Guard {
    id: u32,
    sleeps: Vec<Sleep>,
}

impl Guard {
    fn new(guard_id: u32) -> Self {
        Self { id: guard_id, sleeps: vec![] }
    }

    fn minutes_asleep(&self) -> u32 {
        let mut count = 0;

        for Sleep { start, end } in self.sleeps.iter() {
            count += end - start
        }

        count
    }

    fn minute_most_often_asleep(&self) -> u32 {
        let mut minutes = [0u32; 60];

        for Sleep { start, end } in self.sleeps.iter() {
            for i in *start..*end {
                minutes[i as usize] += 1;
            }
        }

        let mut minute = 0;
        for i in 1..60 {
            if minutes[i] > minutes[minute] {
                minute = i;
            }
        }

        minute as u32
    }

    fn n_times_asleep_at_minute(&self, minute: u32) -> u32 {
        let mut count = 0;
        for Sleep { start, end } in self.sleeps.iter() {
            if *start <= minute && minute < *end {
                count += 1;
            }
        }
        count
    }
}

fn read_observations(filename: &str) -> Vec<Observation> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    let mut lines:Vec<String> = reader.lines().map (|l| l.unwrap()).collect();
    lines.sort();

    lines.into_iter().map (|line| {
        let parts:Vec<&str> = line.split([' ', '[', ']', '#', ':', '-'].as_ref()).filter(|part| 0 < part.trim().len()).collect();
        let minute:u32 = parts[4].parse().unwrap();

        if line.contains("begins shift") {
            let guard_id:u32 = parts[6].parse().unwrap();

            Observation::Start { minute, guard_id }
        } else if line.contains("falls asleep") {
            Observation::Sleep { minute }
        } else if line.contains("wakes up") {
            Observation::Awake { minute }
        } else {
            panic!("Action not known")
        }
    }).collect()
}

fn read_guard_shifts(observations: Vec<Observation>) -> Vec<Guard> {
    let mut guards:HashMap<u32, Guard> = HashMap::new();
    let mut current_guard_id:Option<u32> = None;
    let mut current_sleep_start:Option<u32> = None;

    for observation in observations.iter() {
        match observation {
            Observation::Start { minute: _minute, guard_id } => {
                if let Some(sleep_start) = current_sleep_start {
                    if let Some(mut guard) = guards.get_mut(&current_guard_id.unwrap()) {
                        guard.sleeps.push(Sleep { start: sleep_start, end: 60 });
                    } else {
                        panic!("Who was sleeping?");
                    }
                }

                guards.entry(*guard_id).or_insert(Guard::new(*guard_id));
                current_guard_id = Some(*guard_id);
                current_sleep_start = None;
            },
            Observation::Sleep { minute } => {
                current_sleep_start = Some(*minute);
            },
            Observation::Awake { minute } => {
                if let Some(sleep_start) = current_sleep_start {
                    if let Some(mut guard) = guards.get_mut(&current_guard_id.unwrap()) {
                        guard.sleeps.push(Sleep { start: sleep_start, end: *minute });
                        current_sleep_start = None;
                    } else {
                        panic!("Who was sleeping?")
                    }
                } else {
                    panic!("Not sleeping?")
                }
            }
        }
    }

    guards.into_iter().map (|(_k, v)| v).collect::<Vec<Guard>>()
}

fn part_1(guards: &Vec<Guard>) {
    let mut most_asleep_guard = &guards[0];

    for guard in guards[1..].iter() {
        if guard.minutes_asleep() > most_asleep_guard.minutes_asleep() {
            most_asleep_guard = guard;
        }
    }

    println!("{}", most_asleep_guard.id * most_asleep_guard.minute_most_often_asleep());
}

fn part_2(guards: &Vec<Guard>) {
    let mut most_asleep_guard = &guards[0];

    for guard in guards[1..].iter() {
        if guard.n_times_asleep_at_minute(guard.minute_most_often_asleep()) >
            most_asleep_guard.n_times_asleep_at_minute(most_asleep_guard.minute_most_often_asleep())
        {
            most_asleep_guard = guard;
        }
    }

    println!("{}", most_asleep_guard.id * most_asleep_guard.minute_most_often_asleep());
}

fn main() {
    let guards = read_guard_shifts(read_observations("input"));

    part_1(&guards);
    part_2(&guards);
}
